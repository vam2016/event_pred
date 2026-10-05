if(!exists("conditional_has_history",mode="function"))source("R/ia_history.R",local=TRUE)
# Patient-level conditional prediction. The original IA history is never resampled.
conditional_is_prediction <- function(cfg) identical(cfg$purpose %||% "survival","rejection")
conditional_prediction_plan <- function(cfg) {
  z<-list(effect_mode="ph",primary="logrank",cut_mode="target",gs_timing=cfg$prediction_timing,
    gs_spending=cfg$prediction_spending,gs_futility=cfg$prediction_futility,gs_futility_z=cfg$prediction_futility_z,
    sided=cfg$prediction_sided,alpha=cfg$prediction_alpha,miss_policy="no_reject",estimate_hr=FALSE,
    target_values=cfg$target,n_values=1,hr_values=1,reps=1L,
    treatment_fraction=cfg$prediction_allocation,gs_beta=cfg$prediction_beta,gs_beta_spending=cfg$prediction_beta_spending,gs_beta_gamma=cfg$prediction_beta_gamma,gs_design_hr=cfg$prediction_design_hr)
  validate_sequential_config(z)
  p<-gs_plan(z,cfg$target)
  if(!is.null(cfg$prediction_plan)) {
    saved<-cfg$prediction_plan
    if(!all(names(p) %in% names(saved))||!isTRUE(all.equal(saved[,names(p),drop=FALSE],p,tolerance=1e-8,check.attributes=FALSE)))stop("保存边界的事件目标、比例或停止界值与原配置不符。")
    p<-saved
  }
  p
}
validate_conditional_prediction <- function(data,cfg) {
  scalar<-function(x,lo,hi)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi
  labs<-vapply(cfg$groups,`[[`,character(1),"name")
  if(length(labs)!=2||!setequal(labs,unique(data$group)))stop("拒绝概率需要已知组别的两组IA数据；合并数据不能恢复治疗比较。")
  if(length(cfg$prediction_control)!=1||!cfg$prediction_control %in% labs)stop("请选择Control组；另一组作为Treatment。")
  if(!cfg$prediction_design %in% c("fixed","sequential")||!cfg$prediction_sided %in% c("benefit","two")||!scalar(cfg$prediction_alpha,.0001,.2))stop("请选择单次Final或组序贯及单/双侧；alpha需0.0001–0.2。")
  if(cfg$cut_mode=="fixed"&&length(cfg$cuts)!=1)stop("拒绝概率每次使用一个预定Final DCO；不能在多个DCO中选择最小p。")
  if(cfg$cut_mode=="target"&&cfg$target<=sum(data$event)&&!(conditional_has_history(cfg)&&cfg$target==sum(data$event)))stop("未来目标D*需大于IA已观察事件数。")
  if(cfg$prediction_design=="sequential") {
    if(cfg$cut_mode!="target")stop("组序贯条件预测仅支持事件驱动截点。")
    p<-conditional_prediction_plan(cfg)
    if(!conditional_has_history(cfg)&&sum(data$event)!=p$target_events[1])stop(paste0("本版组序贯从首次IA继续，IA事件数须等于预设首目标",p$target_events[1],"；当前为",sum(data$event),"。请使用原计划的D*和信息比例。"))
  }
  if(length(cfg$prediction_miss)!=1||!cfg$prediction_miss %in% c("no_reject","analyze")||(cfg$prediction_design=="sequential"&&cfg$prediction_miss!="no_reject"))stop("组序贯窗口未达下一目标时只关闭试验；单次Final需预设窗口规则。")
  invisible(TRUE)
}
conditional_prediction_logrank <- function(o,cfg) {
  x<-o;x$group<-ifelse(x$group==cfg$prediction_control,"Control","Treatment")
  a<-analyze_study_trial(x,list(primary="logrank",estimate_hr=FALSE,sided=cfg$prediction_sided,alpha=cfg$prediction_alpha),NA_real_)
  list(valid=a$logrank_valid,w=if(a$logrank_valid)-a$logrank_z else NA_real_,p=a$logrank_p,note=a$logrank_note,
    variance=if(a$logrank_valid)study_logrank(x$time_day,x$event,as.integer(x$group=="Treatment"))$variance else NA_real_)
}
conditional_prediction_ia <- function(data,cfg) {
  o<-data.frame(USUBJID=data$id,group=data$group,entry_day=data$entry,time_day=data$time,obs_day=data$obs_day,event=data$event,status=data$status,DCO_DAY=cfg$cut)
  a<-conditional_prediction_logrank(o,cfg)
  if(cfg$prediction_design=="sequential"&&!a$valid)stop("所选IA的log-rank无法计算，请检查两组风险集和事件信息。")
  j<-cfg$prediction_look %||% 1L
  action<-if(cfg$prediction_design=="sequential")gs_action(a$w,a$valid,j,cfg$prediction_plan,cfg$prediction_sided) else "not_tested"
  data.frame(DCO_DAY=cfg$cut,events=sum(data$event),n=nrow(data),control=cfg$prediction_control,treatment=setdiff(unique(data$group),cfg$prediction_control),benefit_z=a$w,score_variance=a$variance,nominal_p=a$p,valid=a$valid,action=action)
}
conditional_prediction_path <- function(truth,cfg,b,q,ia) {
  sequential<-cfg$prediction_design=="sequential";plan<-cfg$prediction_plan;path<-list();selected<-NULL
  record<-function(o,j,target,hit,performed,a,action) data.frame(SIMID=b,scenario_q=q,look=j,information_fraction=if(sequential)plan$information_fraction[j] else 1,alpha_spent=if(sequential)plan$alpha_spent[j] else cfg$prediction_alpha,DCO_DAY=o$DCO_DAY[1],target_events=target,events=sum(o$event),n=nrow(o),target_reached=hit,performed=performed,valid=if(performed)a$valid else NA,benefit_z=if(performed)a$w else NA_real_,score_variance=if(performed)a$variance else NA_real_,nominal_p=if(performed)a$p else NA_real_,upper_z=if(sequential)plan$upper_z[j] else if(cfg$prediction_sided=="two")qnorm(1-cfg$prediction_alpha/2) else qnorm(1-cfg$prediction_alpha),lower_z=if(sequential)plan$lower_efficacy_z[j] else if(cfg$prediction_sided=="two")-qnorm(1-cfg$prediction_alpha/2) else -Inf,futility_z=if(sequential)plan$futility_z[j] else -Inf,action=action,note=if(performed)a$note else "")
  if(sequential) {
    o<-survival_at_cut(truth,cfg$cut,1);a<-conditional_prediction_logrank(o,cfg)
    if(conditional_has_history(cfg)) {
      h<-cfg$prediction_history_path;h$SIMID<-b;h$scenario_q<-q
      order<-names(record(o,1,plan$target_events[1],TRUE,TRUE,a,ia$action));path[[1]]<-h[,order,drop=FALSE]
    } else path[[1]]<-record(o,1,plan$target_events[1],TRUE,TRUE,a,ia$action)
    if(ia$action!="continue")selected<-o
  }
  if(is.null(selected)) {
    dates<-sort(truth$event_day[is.finite(truth$event_day)&truth$event_time_day<=truth$dropout_time_day&truth$event_day<=cfg$max_day])
    looks<-if(sequential)seq.int((cfg$prediction_look %||% 1L)+1L,nrow(plan)) else 1L
    for(j in looks) {
      target<-if(sequential)plan$target_events[j] else if(cfg$cut_mode=="target")cfg$target else NA_real_
      hit<-if(cfg$cut_mode=="target")length(dates)>=target else NA
      day<-if(cfg$cut_mode=="fixed")cfg$cuts[1] else if(hit)dates[target] else cfg$max_day
      o<-survival_at_cut(truth,day,j);performed<-cfg$cut_mode=="fixed"||isTRUE(hit)||cfg$prediction_miss=="analyze"
      a<-if(performed)conditional_prediction_logrank(o,cfg) else list(valid=FALSE,w=NA_real_,p=NA_real_,variance=NA_real_,note="")
      action<-if(!performed)"window_unreached" else if(!a$valid)"invalid" else if(sequential)gs_action(a$w,TRUE,j,plan,cfg$prediction_sided) else if(a$p<=cfg$prediction_alpha)if(a$w>=0)"efficacy_benefit" else "efficacy_reverse" else "final_no_reject"
      path[[length(path)+1]]<-record(o,j,target,hit,performed,a,action);selected<-o
      if(action!="continue")break
    }
  }
  p<-do.call(rbind,path);if(conditional_has_history(cfg))p$phase<-ifelse(p$look<=cfg$prediction_look,"observed_history","simulated_future");last<-tail(p,1);valid<-last$action!="invalid";reject<-if(valid)startsWith(last$action,"efficacy") else NA
  row<-data.frame(SIMID=b,scenario_q=q,generated=TRUE,decision_valid=valid,decision_reject=reject,stop_look=last$look,stop_reason=last$action,DCO_DAY=last$DCO_DAY,events=last$events,n=last$n,analysis_count=sum(p$performed),future_analysis_count=sum(p$performed & if(sequential)p$look>(cfg$prediction_look %||% 1L) else TRUE),target_reached=if(cfg$cut_mode=="target")last$events>=cfg$target else NA,note=last$note)
  selected$CUTID<-1L;selected$scenario_q<-q
  list(row=row,looks=p,observed=selected)
}
run_conditional_prediction <- function(data,cfg,progress=function(value,detail)NULL) {
  cfg<-complete_config(cfg);cfg$purpose<-"rejection"
  if(!conditional_has_history(cfg))cfg$prediction_look<-NULL
  if(conditional_has_history(cfg)) {
    cfg$prediction_history_path<-conditional_history_path(cfg$prediction_history,cfg)
    cfg$prediction_look<-nrow(cfg$prediction_history_path)
    last<-cfg$prediction_history[[cfg$prediction_look]]
    if(!isTRUE(all.equal(cfg$cut,last$cut)))stop("当前IA时间与所选历史快照不一致。")
    cols<-c("id","entry","time","obs_day","status","event","group")
    if(!all(cols %in% names(data)))stop("当前IA缺少规范化的历史字段。")
    sortdata<-function(d){z<-d[order(d$id),cols,drop=FALSE];rownames(z)<-NULL;z}
    if(!isTRUE(all.equal(sortdata(data),sortdata(last$data),tolerance=1e-12,check.attributes=FALSE)))stop("当前患者资料不是所选历史IA的观察快照。")
    # Already terminal paths need no future model, rate, prior, horizon or repetitions.
    if(tail(cfg$prediction_history_path$action,1)!="continue") {
      cfg$model_mode<-"manual";cfg$uncertainty<-"plugin";cfg$process_uncertainty<-"fixed";cfg$reps<-1L;cfg$q_values<-1;cfg$seed<-0L;cfg$max_day<-cfg$cut+1
      cfg$groups<-lapply(sort(unique(data$group)),function(lab)list(name=lab,future_n=0,enroll_mode="constant",enroll_rate=0,enroll_cuts=numeric(),enroll_rates=numeric(),dropout_rate=0,multiplier=1,model=NULL))
    }
  }
  data<-validate_data(data,cfg$cut,require_events=cfg$model_mode=="fit",gap_mode="strict")
  if(any(abs(data$obs_day-data$entry-data$time)>1e-6))stop("条件拒绝预测使用不含首日偏移的经过时间；请先按ADTTE日期与AVAL偏移标准化。")
  cfg$fixed_times<-1 # Not an input for the rejection-probability task.
  validate_conditional_config(data,cfg);validate_conditional_prediction(data,cfg)
  if(cfg$prediction_design=="sequential")cfg$prediction_plan<-conditional_prediction_plan(cfg)
  ia<-conditional_prediction_ia(data,cfg)
  # Reuse the validated model/posterior/process engine, with one internal horizon cut.
  # No future analysis reads the internal horizon KM or any latent values as observed data.
  engine<-cfg;engine$purpose<-"survival";engine$cut_mode<-"fixed";engine$cuts<-cfg$max_day;engine$target<-1;engine$reps<-cfg$reps
  if(cfg$prediction_design=="sequential"&&ia$action!="continue") {
    tr<-data.frame(USUBJID=data$id,group=data$group,entry_day=data$entry,event_time_day=ifelse(data$status=="event",data$time,Inf),dropout_time_day=ifelse(data$status=="dropout",data$time,Inf))
    tr$event_day<-tr$entry_day+tr$event_time_day;tr$dropout_day<-tr$entry_day+tr$dropout_time_day
    copies<-lapply(cfg$q_values,function(q)do.call(rbind,lapply(seq_len(cfg$reps),function(b){x<-tr;x$SIMID<-b;x$scenario_q<-q;x})))
    r<-list(baseline=conditional_baseline(data,cfg),models=list(),process_posteriors=list(),truth=do.call(rbind,copies),failures=data.frame(),created_at=format(Sys.time(),tz="UTC",usetz=TRUE))
  } else r<-run_conditional_simulation(data,engine,progress,retain_failures=TRUE)
  decisions<-looks<-observed<-list()
  for(q in cfg$q_values)for(b in seq_len(cfg$reps)) {
    tr<-r$truth[r$truth$SIMID==b&r$truth$scenario_q==q,,drop=FALSE]
    attempt<-if(nrow(tr))tryCatch(conditional_prediction_path(tr,cfg,b,q,ia),error=function(e)e) else simpleError(r$failures$reason[match(b,r$failures$SIMID)] %||% "生成失败")
    if(inherits(attempt,"error")) {
      decisions[[length(decisions)+1]]<-data.frame(SIMID=b,scenario_q=q,generated=FALSE,decision_valid=FALSE,decision_reject=NA,stop_look=NA_integer_,stop_reason="generation_failure",DCO_DAY=NA_real_,events=NA_integer_,n=NA_integer_,analysis_count=0L,future_analysis_count=0L,target_reached=NA,note=conditionMessage(attempt))
    } else {decisions[[length(decisions)+1]]<-attempt$row;looks[[length(looks)+1]]<-attempt$looks;observed[[length(observed)+1]]<-attempt$observed}
  }
  bind<-function(x){d<-do.call(rbind,x);if(is.null(d))data.frame() else {rownames(d)<-NULL;d}}
  r$config<-cfg;r$ia_decision<-ia;if(conditional_has_history(cfg))r$history_path<-cfg$prediction_history_path;r$decisions<-bind(decisions);r$looks<-bind(looks);r$observed<-bind(observed)
  if(!nrow(r$observed)){r$observed<-r$baseline$observed[0,,drop=FALSE];r$observed$scenario_q<-numeric()}
  # The prediction result exposes stopping records only; latent truth and horizon estimates stay internal.
  r$truth<-NULL;r$summary<-r$fixed<-r$curves<-data.frame();r$cuts<-r$decisions[,c("SIMID","scenario_q","DCO_DAY","events","target_reached"),drop=FALSE];r$cuts$CUTID<-1L
  packages<-c("R","survival",if(cfg$prediction_design=="sequential")"rpact",if(cfg$uncertainty=="bayes_weibull")"posterior")
  r$dependencies<-data.frame(package=packages,version=vapply(packages,function(p)if(p=="R")as.character(getRversion()) else as.character(packageVersion(p)),character(1)))
  r$version<-"0.30.0";r
}
conditional_prediction_overview <- function(r) {
  do.call(rbind,lapply(split(r$decisions,r$decisions$scenario_q),function(x){n<-nrow(x);v<-x$decision_valid;nv<-sum(v);nr<-sum(x$decision_reject,na.rm=TRUE);p<-if(nv)nr/nv else NA_real_;decided<-r$config$prediction_design=="sequential"&&r$ia_decision$action!="continue";ci<-if(decided)c(p,p) else study_wilson(nr,nv)
    data.frame(scenario_q=x$scenario_q[1],requested=n,valid=nv,invalid=n-nv,rejection_conditional=p,mcse=if(nv)sqrt(p*(1-p)/nv) else NA_real_,wilson_lower=ci[1],wilson_upper=ci[2],failure_bound_lower=nr/n,failure_bound_upper=(nr+n-nv)/n,
      efficacy_benefit=sum(x$stop_reason=="efficacy_benefit")/n,efficacy_reverse=sum(x$stop_reason=="efficacy_reverse")/n,futility=sum(x$stop_reason=="futility")/n,window_unreached=sum(x$stop_reason=="window_unreached")/n,
      target_probability=if(r$config$cut_mode=="target"&&any(x$generated))mean(x$target_reached[x$generated]) else NA_real_,mean_future_analyses=if(any(x$generated))mean(x$future_analysis_count[x$generated]) else NA_real_,mean_events=if(any(x$generated))mean(x$events[x$generated]) else NA_real_,mean_stop_day=if(any(x$generated))mean(x$DCO_DAY[x$generated]) else NA_real_)
  }))
}
conditional_prediction_script <- function(r) {
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  d<-r$baseline$observed;data<-data.frame(id=d$USUBJID,entry=d$entry_day,time=d$time_day,obs_day=d$obs_day,status=d$status,event=d$event,group=d$group)
  c('# 在相同版本event_pred源码根目录运行；IA原始观察历史及边界快照保留。',
    'for(f in c("units","models","inputs","forecast","bayes","simulation","conditional_simulation","nph","sequential","study","conditional_prediction","ia_history"))source(paste0("R/",f,".R"))',
    paste0('cfg <- ',dump(r$config)),paste0('data <- ',dump(data)),
    'r <- run_conditional_prediction(data,cfg)',
    if(conditional_has_history(r$config))'write.csv(r$history_path,"prediction_history.csv",row.names=FALSE)',
    'write.csv(r$decisions,"prediction_trials.csv",row.names=FALSE);write.csv(r$looks,"prediction_looks.csv",row.names=FALSE);write.csv(conditional_prediction_overview(r),"prediction_summary.csv",row.names=FALSE)')
}
