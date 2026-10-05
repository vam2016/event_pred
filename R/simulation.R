# Design-stage simulation. Canonical times are continuous elapsed days.
simulation_parameter_model <- function(method, v, unit = "months") {
  if (method == "exponential" && identical(v$exp_input, "survival")) {
    if (length(v$survival_time)!=1 || !is.finite(v$survival_time) || v$survival_time<=0 ||
        length(v$survival_prob)!=1 || !is.finite(v$survival_prob) || v$survival_prob<=0 || v$survival_prob>=1)
      stop("指数固定时点需为正数，生存率需在(0,1)。")
    v$exp_rate <- -log(v$survival_prob)/v$survival_time; v$exp_input <- "rate"
  }
  if (method == "pwe" && identical(v$pwe_input, "survival")) {
    cuts <- parse_unit_numbers(v$parameter_cuts); s <- parse_unit_numbers(v$pwe_survivals)
    if (!length(cuts) || length(s)!=length(cuts) || any(s<=0 | s>1) || any(diff(c(1,s))>0) ||
        any(cuts<=0) || is.unsorted(cuts,strictly=TRUE) || length(v$pwe_last_rate)!=1 ||
        !is.finite(v$pwe_last_rate) || v$pwe_last_rate<0)
      stop("PWE需正递增切点、同数量非递增生存率(0,1]及非负末段风险率。")
    rates <- c(-diff(log(c(1,s)))/diff(c(0,cuts)),v$pwe_last_rate)
    v$parameter_rates <- paste(format(rates,digits=17),collapse=",")
  }
  parameter_input_model(method,v,unit)
}

validate_simulation_config <- function(cfg) {
  integer_scalar <- function(x,lo,hi) is.numeric(x)&&length(x)==1&&is.finite(x)&&x==floor(x)&&x>=lo&&x<=hi
  if (!integer_scalar(cfg$n,1,5000)) stop("总样本量需为1–5000整数。")
  if (!integer_scalar(cfg$reps,1,200)) stop("重复试验数需为1–200整数。")
  if (!integer_scalar(cfg$seed,0,.Machine$integer.max)) stop("随机种子超出整数范围。")
  if (!is.numeric(cfg$max_day)||length(cfg$max_day)!=1||!is.finite(cfg$max_day)||cfg$max_day<=0||cfg$max_day>3650)
    stop("最大研究窗口需在(0,3650]日。")
  if (!cfg$cut_mode %in% c("fixed","target")) stop("未知截点规则。")
  if (cfg$cut_mode=="fixed" && (!is.numeric(cfg$cuts)||!length(cfg$cuts)||length(cfg$cuts)>20||
      any(!is.finite(cfg$cuts))||any(cfg$cuts<=0)||is.unsorted(cfg$cuts,strictly=TRUE)||max(cfg$cuts)>cfg$max_day))
    stop("DCO需1–20个正递增时间，且不超过最大研究窗口。")
  if (!integer_scalar(cfg$target,1,100000)) stop("D*需为正整数。")
  if (!cfg$enroll_mode %in% c("constant","piecewise","schedule")) stop("未知入组方式。")
  if (cfg$enroll_mode=="constant" && (length(cfg$enroll_rate)!=1||!is.finite(cfg$enroll_rate)||cfg$enroll_rate<=0)) stop("恒定入组率需为正数。")
  if (cfg$enroll_mode=="piecewise") parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts)
  if (cfg$enroll_mode=="schedule"&&(!is.numeric(cfg$entry_days)||length(cfg$entry_days)!=cfg$n||
      any(!is.finite(cfg$entry_days))||any(cfg$entry_days<0)||is.unsorted(cfg$entry_days))) stop("固定入组日程需提供N个非负且非递减研究时间。")
  if (length(cfg$groups)<1||length(cfg$groups)>6) stop("组数需为1–6。")
  labels <- vapply(cfg$groups,function(g) g$name,character(1))
  if (anyNA(labels)||any(!nzchar(trimws(labels)))||anyDuplicated(labels)) stop("组名需非空且唯一。")
  for (g in cfg$groups) {
    if (length(g$weight)!=1||!is.finite(g$weight)||g$weight<=0||length(g$dropout_rate)!=1||!is.finite(g$dropout_rate)||g$dropout_rate<0) stop("分配权重需为正、脱落率需非负。")
    if (!g$model$id %in% parameter_catalog()$id) stop("模拟分布不在参数模型范围内。")
  }
  time_factor(cfg$display_unit)
  if (length(cfg$origin)!=1||is.na(as.Date(cfg$origin))) stop("研究起点日期无效。")
  if (length(cfg$paramcd)!=1||!cfg$paramcd %in% c("PFS","OS")) stop("请选择PFS或OS终点。")
  if (!is.numeric(cfg$fixed_times)||!length(cfg$fixed_times)||length(cfg$fixed_times)>20||any(!is.finite(cfg$fixed_times))||any(cfg$fixed_times<=0)||is.unsorted(cfg$fixed_times,strictly=TRUE)) stop("固定随访时点需1–20个正递增值。")
  if (cfg$n*cfg$reps*(if(cfg$cut_mode=="fixed")length(cfg$cuts) else 1) > 1e6) stop("受试者×重复数×截点数不能超过100万，请减少配置。")
  invisible(TRUE)
}

simulate_survival_truth <- function(cfg, sim_id=1L) {
  entry <- switch(cfg$enroll_mode,
    constant=cumsum(rexp(cfg$n,cfg$enroll_rate)),
    piecewise=inverse_cumhaz(parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts),cumsum(rexp(cfg$n))),
    schedule=cfg$entry_days)
  allocation <- sample.int(length(cfg$groups),cfg$n,replace=TRUE,prob=vapply(cfg$groups,function(g)g$weight,numeric(1)))
  event <- dropout <- rep(Inf,cfg$n)
  for (j in seq_along(cfg$groups)) {
    ix <- which(allocation==j); if(!length(ix)) next
    g <- cfg$groups[[j]]
    event[ix] <- if(is.null(g$hazard_profile))sample_conditional(g$model,rep(0,length(ix)),g$hazard_multiplier %||% 1) else nph_inverse(g$model,-log(runif(length(ix))),g$hazard_profile$cuts,g$hazard_profile$hr)
    if (g$dropout_rate>0) dropout[ix] <- rexp(length(ix),g$dropout_rate)
  }
  data.frame(SIMID=sim_id,USUBJID=sprintf("SIM%03d-%05d",sim_id,seq_len(cfg$n)),
    group=vapply(cfg$groups,function(g)g$name,character(1))[allocation],
    entry_day=entry,event_time_day=event,dropout_time_day=dropout,
    event_day=entry+event,dropout_day=entry+dropout,stringsAsFactors=FALSE)
}

# Analysis receives observed records only. Subjects not yet entered are absent.
survival_at_cut <- function(truth, dco, cut_id=1L) {
  x <- truth[is.finite(truth$entry_day)&truth$entry_day<dco,,drop=FALSE]
  admin <- dco-x$entry_day
  # Calendar-time membership avoids subtractive roundoff dropping an event at exact DCO.
  status <- ifelse(x$event_time_day<=x$dropout_time_day & x$event_day<=dco,"event",
    ifelse(x$dropout_time_day<x$event_time_day & x$dropout_day<=dco,"dropout","active"))
  time <- ifelse(status=="event",x$event_time_day,ifelse(status=="dropout",x$dropout_time_day,admin))
  data.frame(SIMID=x$SIMID,CUTID=rep(cut_id,nrow(x)),DCO_DAY=rep(dco,nrow(x)),USUBJID=x$USUBJID,
    group=x$group,entry_day=x$entry_day,time_day=time,obs_day=x$entry_day+time,
    event=as.integer(status=="event"),status=status,stringsAsFactors=FALSE)
}

analyze_simulated_survival <- function(observed, cfg, sim_id, cut_id, dco) {
  summary_rows <- fixed_rows <- curve_rows <- list()
  labels <- c("总体",vapply(cfg$groups,function(g)g$name,character(1)))
  for (j in seq_along(labels)) {
    x <- if(j==1) observed else observed[observed$group==labels[j],,drop=FALSE]
    scope <- if(j==1)"overall" else "group"
    median <- lower <- upper <- NA_real_
    fixed <- data.frame(time_day=cfg$fixed_times,survival=NA_real_,lower=NA_real_,upper=NA_real_,n_risk=0L,status="无入组者")
    if(nrow(x)) {
      fit <- survival::survfit(survival::Surv(time_day,event)~1,data=x,conf.type="log-log")
      q <- quantile(fit,probs=.5,conf.int=TRUE)
      median <- unname(q$quantile[1]); lower <- unname(q$lower[1]); upper <- unname(q$upper[1])
      sm <- summary(fit,times=cfg$fixed_times,extend=TRUE)
      supported <- cfg$fixed_times<=max(x$time_day)
      fixed$survival[supported] <- sm$surv[supported]; fixed$lower[supported] <- sm$lower[supported]; fixed$upper[supported] <- sm$upper[supported]
      fixed$n_risk <- vapply(cfg$fixed_times,function(t)sum(x$time_day>=t),integer(1))
      fixed$status <- ifelse(supported,"可估计","超出观察范围")
      curve_rows[[j]] <- data.frame(SIMID=sim_id,CUTID=cut_id,DCO_DAY=dco,scope=scope,group=labels[j],
        time_day=c(0,fit$time),survival=c(1,fit$surv),lower=c(1,fit$lower),upper=c(1,fit$upper),
        n_risk=c(nrow(x),fit$n.risk),n_event=c(0,fit$n.event),n_censor=c(0,fit$n.censor))
    }
    summary_rows[[j]] <- data.frame(SIMID=sim_id,CUTID=cut_id,DCO_DAY=dco,scope=scope,group=labels[j],n=nrow(x),
      events=sum(x$event),dropouts=sum(x$status=="dropout"),administrative=sum(x$status=="active"),
      median_day=median,lower_day=lower,upper_day=upper,median_status=if(!nrow(x))"无入组者" else if(is.na(median))"NR" else "可估计")
    fixed_rows[[j]] <- cbind(data.frame(SIMID=sim_id,CUTID=cut_id,DCO_DAY=dco,scope=scope,group=labels[j]),fixed)
  }
  list(summary=do.call(rbind,summary_rows),fixed=do.call(rbind,fixed_rows),curves=do.call(rbind,curve_rows))
}

run_survival_simulation <- function(cfg,progress=function(value,detail)NULL) {
  validate_simulation_config(cfg); set.seed(cfg$seed)
  truth_rows <- obs_rows <- summary_rows <- fixed_rows <- curve_rows <- cut_rows <- list()
  for(b in seq_len(cfg$reps)) {
    truth <- simulate_survival_truth(cfg,b); truth_rows[[b]] <- truth
    usable <- sort(truth$event_day[is.finite(truth$event_day)&truth$event_time_day<=truth$dropout_time_day&truth$event_day<=cfg$max_day])
    hit <- length(usable)>=cfg$target
    target_day <- if(hit)usable[cfg$target] else NA_real_
    cuts <- if(cfg$cut_mode=="fixed") cfg$cuts else if(hit)target_day else cfg$max_day
    for(k in seq_along(cuts)) {
      dco <- cuts[k]; observed <- survival_at_cut(truth,dco,k)
      a <- analyze_simulated_survival(observed,cfg,b,k,dco)
      obs_rows[[length(obs_rows)+1L]] <- observed; summary_rows[[length(summary_rows)+1L]] <- a$summary
      fixed_rows[[length(fixed_rows)+1L]] <- a$fixed; curve_rows[[length(curve_rows)+1L]] <- a$curves
      cut_rows[[length(cut_rows)+1L]] <- data.frame(SIMID=b,CUTID=k,DCO_DAY=dco,n=nrow(observed),events=sum(observed$event),
        target_reached=sum(observed$event)>=cfg$target,target_day=if(hit && target_day<=dco)target_day else NA_real_,
        analysis_status=if(cfg$cut_mode=="target"&&!hit)"窗口内未达标，按窗口末分析" else "已按设定截点分析")
    }
    progress(b/cfg$reps,paste("试验",b,"/",cfg$reps))
  }
  bind <- function(x) {y<-do.call(rbind,x);if(is.null(y))data.frame() else {rownames(y)<-NULL;y}}
  list(config=cfg,truth=bind(truth_rows),observed=bind(obs_rows),summary=bind(summary_rows),fixed=bind(fixed_rows),
    curves=bind(curve_rows),cuts=bind(cut_rows),created_at=format(Sys.time(),tz="UTC",usetz=TRUE),version="0.30.0")
}

# Date-level export preserves zero-day records and never changes event status.
# The prediction importer requires positive elapsed time; incompatible exports
# carry an explicit report instead of silently dropping or shifting patients.
simulation_adtte <- function(observed,cfg) {
  origin <- as.Date(cfg$origin); f <- time_factor(cfg$display_unit); n <- nrow(observed)
  start <- origin+floor(observed$entry_day); adt <- origin+floor(observed$obs_day)
  data.frame(STUDYID=rep("SIMULATED",n),SIMID=observed$SIMID,CUTID=observed$CUTID,DCO_DAY=observed$DCO_DAY,
    USUBJID=observed$USUBJID,PARAMCD=rep(cfg$paramcd,n),STARTDT=start,ADT=adt,
    AVAL=(as.numeric(adt-start)+1)/f,AVALU=rep(switch(cfg$display_unit,days="DAYS",weeks="WEEKS",months="MONTHS"),n),
    CNSR=ifelse(observed$status=="event",0L,ifelse(observed$status=="dropout",2L,1L)),
    TRTP=observed$group,TRTA=observed$group,EVNTDESC=ifelse(observed$status=="event","EVENT",ifelse(observed$status=="dropout","PERMANENT FOLLOW-UP EXIT","CUTOFF")),ANL01FL=rep("Y",n))
}
simulation_export_issues <- function(adtte) {
  ix <- as.Date(adtte$ADT)==as.Date(adtte$STARTDT)
  data.frame(SIMID=adtte$SIMID[ix],CUTID=adtte$CUTID[ix],USUBJID=adtte$USUBJID[ix],issue=rep("同日起止；现有事件预测入口不接受。请保留连续时间分析数据。",sum(ix)))
}
simulation_result_overview <- function(result) {
  s <- result$summary
  groups <- split(seq_len(nrow(s)),interaction(s$CUTID,s$scope,s$group,drop=TRUE))
  do.call(rbind,lapply(groups,function(ix) {
    x <- s[ix,,drop=FALSE]; estimable <- is.finite(x$median_day)
    q <- if(any(estimable))quantile_with_inf(x$median_day[estimable]) else rep(NA_real_,3)
    data.frame(CUTID=x$CUTID[1],scope=x$scope[1],group=x$group[1],trials=nrow(x),
      mean_n=mean(x$n),mean_events=mean(x$events),median_estimable=mean(estimable),
      conditional_lower_day=q[1],conditional_median_day=q[2],conditional_upper_day=q[3])
  }))
}
