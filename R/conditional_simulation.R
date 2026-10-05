# IA -> Final prediction conditions on the ORIGINAL individual histories.
# Known event/exit times are fixed; only active and future subjects are sampled.
conditional_truth <- function(data, model, cfg, sim_id=1L) {
  n<-nrow(data);event<-drop<-rep(Inf,n)
  event[data$status=="event"]<-data$time[data$status=="event"]
  drop[data$status=="dropout"]<-data$time[data$status=="dropout"]
  a<-which(data$status=="active")
  if(length(a)) {
    event[a]<-sample_conditional(model,data$time[a],cfg$multiplier)
    drop[a]<-data$time[a]+if(cfg$dropout_rate==0)Inf else rexp(length(a),cfg$dropout_rate)
  }
  entry<-data$entry;ids<-as.character(data$id);source<-rep("IA记录",n)
  if(cfg$future_n>0) {
    en<-cfg$cut+if(cfg$enroll_mode=="piecewise")inverse_cumhaz(parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts),cumsum(rexp(cfg$future_n))) else cumsum(rexp(cfg$future_n,cfg$enroll_rate))
    event<-c(event,sample_conditional(model,rep(0,cfg$future_n),cfg$multiplier))
    drop<-c(drop,if(cfg$dropout_rate==0)rep(Inf,cfg$future_n) else rexp(cfg$future_n,cfg$dropout_rate))
    prefix<-paste0("FUTURE::",cfg$name,"::");new_ids<-paste0(prefix,seq_len(cfg$future_n));while(any(new_ids %in% c(ids,cfg$reserved_ids))) {prefix<-paste0("_",prefix);new_ids<-paste0(prefix,seq_len(cfg$future_n))}
    entry<-c(entry,en);ids<-c(ids,new_ids);source<-c(source,rep("未来入组",cfg$future_n))
  }
  data.frame(SIMID=sim_id,USUBJID=ids,group=cfg$name,entry_day=entry,event_time_day=event,dropout_time_day=drop,
    event_day=entry+event,dropout_day=entry+drop,source=source,stringsAsFactors=FALSE)
}
conditional_baseline <- function(data,cfg) {
  o<-data.frame(SIMID=0L,CUTID=0L,DCO_DAY=cfg$cut,USUBJID=data$id,group=data$group,entry_day=data$entry,time_day=data$time,obs_day=data$obs_day,event=data$event,status=data$status)
  c(list(observed=o),analyze_simulated_survival(o,cfg,0,0,cfg$cut))
}
validate_conditional_config <- function(data,cfg) {
  scalar<-function(x)is.numeric(x)&&length(x)==1&&is.finite(x)
  if(!scalar(cfg$reps)||cfg$reps<1||cfg$reps>500||cfg$reps!=floor(cfg$reps))stop("重复模拟数需1–500整数。")
  if(!scalar(cfg$seed)||cfg$seed<0||cfg$seed>.Machine$integer.max||cfg$seed!=floor(cfg$seed))stop("随机种子超出整数范围。")
  if(!scalar(cfg$max_day)||cfg$max_day<=cfg$cut||cfg$max_day-cfg$cut>3650)stop("Final窗口需在IA之后，且最长延伸3650日。")
  if(!cfg$cut_mode %in% c("fixed","target"))stop("未知Final截点规则。")
  if(cfg$cut_mode=="fixed"&&(!length(cfg$cuts)||length(cfg$cuts)>20||any(!is.finite(cfg$cuts))||any(cfg$cuts<=cfg$cut)||is.unsorted(cfg$cuts,strictly=TRUE)))stop("Final DCO需1–20个严格递增且晚于IA的研究时间。")
  if(cfg$cut_mode=="target"&&(!scalar(cfg$target)||cfg$target<1||cfg$target!=floor(cfg$target)))stop("D*需为正整数，表示历史与未来的总累计事件数。")
  if(!length(cfg$fixed_times)||length(cfg$fixed_times)>20||any(!is.finite(cfg$fixed_times))||any(cfg$fixed_times<=0)||is.unsorted(cfg$fixed_times,strictly=TRUE))stop("固定随访时点需1–20个正递增时间。")
  if(!length(cfg$q_values)||length(cfg$q_values)>10||any(!is.finite(cfg$q_values))||any(cfg$q_values<=0)||anyDuplicated(cfg$q_values))stop("q情景需1–10个不重复的正数。")
  if(!cfg$model_mode %in% c("fit","manual")||!cfg$uncertainty %in% c("plugin","bootstrap","gamma","bayes_weibull"))stop("未知模型处理方式。")
  if(cfg$model_mode=="manual"&&cfg$uncertainty!="plugin")stop("指定分布使用固定参数；参数不确定性需选择从IA数据拟合。")
  labs<-vapply(cfg$groups,`[[`,character(1),"name")
  if(length(labs)<1||length(labs)>6||anyDuplicated(labs)||!setequal(unique(data$group),labs))stop("IA数据组别与各组设置需一一对应。")
  for(g in cfg$groups) {
    for(id in c("future_n","enroll_rate","dropout_rate","multiplier"))if(!scalar(g[[id]]))stop(paste("各组参数需有限单值：",id))
    if(g$future_n<0||g$future_n>1000||g$future_n!=floor(g$future_n)||g$enroll_rate<0||g$dropout_rate<0||g$multiplier<=0)stop("未来人数需0–1000整数，率需非负，q需正数。")
    if(!g$enroll_mode %in% c("constant","piecewise"))stop("未知入组方式。")
    if(g$future_n>0&&g$enroll_mode=="constant"&&g$enroll_rate==0&&cfg$process_uncertainty!="gamma")stop("继续入组时恒定入组率需为正。")
    if(g$future_n>0&&g$enroll_mode=="piecewise")parameter_model("pwe",list(rates=g$enroll_rates),g$enroll_cuts)
    if(cfg$process_uncertainty=="gamma"&&g$enroll_mode!="constant")stop("过程Gamma后验仅支持恒定入组过程。")
    if(cfg$model_mode=="fit"&&!g$fit_method %in% model_catalog()$id)stop("未知拟合模型。")
    if(cfg$uncertainty=="gamma"&&!g$fit_method %in% c("exponential","pwe"))stop("事件Gamma后验仅支持指数或PWE。")
    if(cfg$uncertainty=="bayes_weibull"&&g$fit_method!="weibull")stop("MCMC后验需各组使用Weibull。")
  }
  prior_ids<-c(if(cfg$uncertainty=="gamma")c("prior_shape","prior_rate"),if(cfg$process_uncertainty=="gamma")c("enroll_prior_shape","enroll_prior_rate","drop_prior_shape","drop_prior_rate"))
  for(id in prior_ids)if(!scalar(cfg[[id]])||cfg[[id]]<=0)stop("Gamma先验参数需为正数。")
  if((nrow(data)+sum(vapply(cfg$groups,`[[`,numeric(1),"future_n")))*cfg$reps*length(cfg$q_values)*(if(cfg$cut_mode=="fixed")length(cfg$cuts) else 1)>1e6)stop("受试者×模拟×情景×截点不能超过100万。")
  time_factor(cfg$display_unit);if(is.na(as.Date(cfg$origin)))stop("研究起点日期无效。")
  invisible(TRUE)
}
simulate_conditional_batch <- function(data,cfg,draws,processes,sim_id,state) {
  rows<-obs<-summ<-fixed<-curves<-cuts<-list()
    for(q in cfg$q_values) {
      assign(".Random.seed",state,envir=.GlobalEnv)
      truth<-do.call(rbind,lapply(seq_along(cfg$groups),function(j){gc<-processes[[j]];gc$multiplier<-gc$multiplier*q;conditional_truth(data[data$group==gc$name,,drop=FALSE],draws[[j]],gc,sim_id)}))
      truth$scenario_q<-q;rows[[length(rows)+1]]<-truth
      dates<-sort(truth$event_day[is.finite(truth$event_day)&truth$event_time_day<=truth$dropout_time_day&truth$event_day<=cfg$max_day])
      hit<-length(dates)>=cfg$target;td<-if(hit)max(cfg$cut,dates[cfg$target]) else NA_real_
      dc<-if(cfg$cut_mode=="fixed")cfg$cuts else if(hit)td else cfg$max_day
      for(k in seq_along(dc)) {
        o<-survival_at_cut(truth,dc[k],k);a<-analyze_simulated_survival(o,cfg,sim_id,k,dc[k])
        addq<-function(x){x$scenario_q<-rep(q,nrow(x));x}
        obs[[length(obs)+1]]<-addq(o);summ[[length(summ)+1]]<-addq(a$summary);fixed[[length(fixed)+1]]<-addq(a$fixed);curves[[length(curves)+1]]<-addq(a$curves)
        cuts[[length(cuts)+1]]<-data.frame(SIMID=sim_id,CUTID=k,scenario_q=q,DCO_DAY=dc[k],n=nrow(o),events=sum(o$event),target_reached=hit&&td<=dc[k],target_day=if(hit&&td<=dc[k])td else NA_real_,analysis_status=if(cfg$cut_mode=="target"&&!hit)"窗口内未达标，保留窗口末分析" else if(cfg$cut_mode=="target"&&td==cfg$cut)"IA已达标" else "按设定截点分析")
      }
    }
  list(rows=rows,obs=obs,summ=summ,fixed=fixed,curves=curves,cuts=cuts)
}
run_conditional_simulation <- function(data,cfg,progress=function(value,detail)NULL,retain_failures=FALSE) {
  cfg<-complete_config(cfg)
  data<-validate_data(data,cfg$cut,require_events=cfg$model_mode=="fit",gap_mode="strict")
  # No unobserved IA interval is silently imputed into the baseline KM.
  if(any(abs(data$obs_day-data$entry-data$time)>1e-6))stop("条件模拟需使用不含首日偏移的经过时间。")
  validate_conditional_config(data,cfg);cfg$reserved_ids<-data$id;set.seed(cfg$seed)
  baseline<-conditional_baseline(data,cfg);models<-pps<-list()
  for(j in seq_along(cfg$groups)) {
    g<-cfg$groups[[j]];dd<-data[data$group==g$name,,drop=FALSE]
    if(cfg$model_mode=="fit")dd<-validate_data(dd,cfg$cut,require_events=TRUE,gap_mode="strict")
    gc<-modifyList(cfg,g)
    m<-if(cfg$model_mode=="manual")g$model else fit_model(dd,g$fit_method,g$cuts,g$tail_rate)
    if(cfg$uncertainty=="bayes_weibull") {
      progress(0,paste(g$name,"Weibull MCMC"));m<-fit_bayesian_weibull(dd,gc)
      if(!m$posterior$passed)stop(paste(g$name,"MCMC诊断未通过，请增加迭代或调整先验。"))
    }
    models[[j]]<-m;pps[j]<-list(process_posterior(dd,gc))
  }
  rows<-list();obs<-summ<-fixed<-curves<-cuts<-list();fail<-list()
  for(b in seq_len(cfg$reps)) {
    draws<-processes<-list();error<-NULL
    for(j in seq_along(cfg$groups)) {
      g<-cfg$groups[[j]];dd<-data[data$group==g$name,,drop=FALSE]
      attempt<-tryCatch({
        m<-models[[j]]
        if(cfg$uncertainty=="bootstrap")m<-fit_model(dd[sample.int(nrow(dd),replace=TRUE),,drop=FALSE],g$fit_method,g$cuts,g$tail_rate)
        if(cfg$uncertainty=="gamma")m<-gamma_posterior_draw(m,cfg$prior_shape,cfg$prior_rate)
        if(cfg$uncertainty=="bayes_weibull")m<-posterior_model_draw(m)
        list(model=m,process=process_draw(modifyList(cfg,g),pps[[j]]))
      },error=function(e)e)
      if(inherits(attempt,"error")){error<-paste(g$name,conditionMessage(attempt));break}
      draws[[j]]<-attempt$model;processes[[j]]<-attempt$process
    }
    if(!is.null(error)){fail[[length(fail)+1]]<-data.frame(SIMID=b,reason=error);next}
    # Common random numbers and model/process draws across q scenarios.
    batch<-tryCatch(simulate_conditional_batch(data,cfg,draws,processes,b,.Random.seed),error=function(e)e)
    if(inherits(batch,"error")){fail[[length(fail)+1]]<-data.frame(SIMID=b,reason=paste("轨迹生成：",conditionMessage(batch)));next}
    rows<-c(rows,batch$rows);obs<-c(obs,batch$obs);summ<-c(summ,batch$summ);fixed<-c(fixed,batch$fixed);curves<-c(curves,batch$curves);cuts<-c(cuts,batch$cuts)
    progress(b/cfg$reps,paste("条件模拟",b,"/",cfg$reps))
  }
  bind<-function(x){z<-do.call(rbind,x);if(is.null(z))data.frame() else {rownames(z)<-NULL;z}}
  failures<-bind(fail)
  if(!retain_failures&&length(unique(unlist(lapply(summ,`[[`,"SIMID"))))<.9*cfg$reps)stop(paste("成功模拟少于90%，请检查模型或参数。",paste(head(failures$reason,3),collapse="；")))
  if(!length(summ)&&retain_failures)return(list(config=cfg,baseline=baseline,models=models,process_posteriors=pps,truth=data.frame(),observed=data.frame(),summary=data.frame(),fixed=data.frame(),curves=data.frame(),cuts=data.frame(),failures=failures,created_at=format(Sys.time(),tz="UTC",usetz=TRUE),version="0.30.0"))
  s<-bind(summ);ix<-match(paste(s$scope,s$group),paste(baseline$summary$scope,baseline$summary$group))
  s$IA_median_day<-baseline$summary$median_day[ix];s$median_change_day<-s$median_day-s$IA_median_day
  f<-bind(fixed);ix<-match(paste(f$scope,f$group,f$time_day),paste(baseline$fixed$scope,baseline$fixed$group,baseline$fixed$time_day))
  f$IA_survival<-baseline$fixed$survival[ix];f$survival_change<-f$survival-f$IA_survival
  list(config=cfg,baseline=baseline,models=models,process_posteriors=pps,truth=bind(rows),observed=bind(obs),summary=s,fixed=f,curves=bind(curves),cuts=bind(cuts),failures=failures,
    created_at=format(Sys.time(),tz="UTC",usetz=TRUE),version="0.30.0")
}
conditional_overview <- function(r) {
  s<-r$summary;parts<-split(seq_len(nrow(s)),interaction(s$scenario_q,s$CUTID,s$scope,s$group,drop=TRUE))
  do.call(rbind,lapply(parts,function(ix){x<-s[ix,,drop=FALSE];ok<-is.finite(x$median_day);ia<-x$IA_median_day[1];both<-ok&is.finite(ia);increased<-both&x$median_day>ia
    q<-if(any(ok))quantile_with_inf(x$median_day[ok]) else rep(NA_real_,3)
    change<-if(any(both))quantile_with_inf(x$median_change_day[both]) else rep(NA_real_,3)
    data.frame(scenario_q=x$scenario_q[1],CUTID=x$CUTID[1],scope=x$scope[1],group=x$group[1],requested=r$config$reps,successful=nrow(x),IA_median_day=ia,
      median_estimable=mean(ok),conditional_lower_day=q[1],conditional_median_day=q[2],conditional_upper_day=q[3],change_lower_day=change[1],change_median_day=change[2],change_upper_day=change[3],
      joint_increase_probability=if(is.finite(ia))mean(increased) else NA_real_,
      failure_bound_lower=if(is.finite(ia))sum(increased)/r$config$reps else NA_real_,failure_bound_upper=if(is.finite(ia))(sum(increased)+r$config$reps-nrow(x))/r$config$reps else NA_real_,conditional_increase_probability=if(any(both))mean(increased[both]) else NA_real_,
      conditional_increase_mcse=if(any(both)){p<-mean(increased[both]);sqrt(p*(1-p)/sum(both))} else NA_real_)
  }))
}
conditional_fixed_overview <- function(r) {
  s<-r$fixed;parts<-split(seq_len(nrow(s)),interaction(s$scenario_q,s$CUTID,s$scope,s$group,s$time_day,drop=TRUE))
  do.call(rbind,lapply(parts,function(ix){x<-s[ix,,drop=FALSE];ok<-is.finite(x$survival);both<-ok&is.finite(x$IA_survival);q<-if(any(ok))quantile_with_inf(x$survival[ok]) else rep(NA_real_,3);d<-if(any(both))quantile_with_inf(x$survival_change[both]) else rep(NA_real_,3)
    data.frame(scenario_q=x$scenario_q[1],CUTID=x$CUTID[1],scope=x$scope[1],group=x$group[1],time_day=x$time_day[1],IA_survival=x$IA_survival[1],estimable=mean(ok),lower=q[1],median=q[2],upper=q[3],change_lower=d[1],change_median=d[2],change_upper=d[3])
  }))
}
