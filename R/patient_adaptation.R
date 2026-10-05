# Disjoint patient cohorts: stage 1 is frozen at IA; stage 2 uses new patients only.
patient_adaptive_plan <- function(cfg) {
  a<-cfg;a$purpose<-"interim";a$z1<-0
  adaptive_plan(a)
}
patient_adaptive_validate <- function(cfg,data=NULL) {
  patient_adaptive_plan(cfg)
  scalar<-function(x,lo,hi,int=FALSE)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi&&(!int||x==floor(x))
  if(!cfg$purpose %in% c("patient_interim","patient_simulation"))stop("请选择患者IA重估或患者完整设计评价。")
  time_factor(cfg$display_unit)
  if(cfg$purpose=="patient_interim") {
    data<-validate_data(data,cfg$cut,require_events=FALSE,gap_mode="strict")
    if(!"group" %in% names(data)||length(unique(data$group))!=2||anyNA(data$group)||any(!nzchar(data$group)))stop("患者IA需要已知组别的两组资料。")
    if(length(cfg$control)!=1||!cfg$control %in% data$group)stop("明确指定Control组。")
    if(sum(data$event)!=cfg$d1)stop(paste0("当前IA事件数须等于原计划D1=",cfg$d1,"；当前为",sum(data$event),"。"))
    if(any(abs(data$obs_day-data$entry-data$time)>1e-6))stop("IA记录需使用不含首日偏移的连续经过时间。")
    return(invisible(data))
  }
  for(k in c("n1","n2"))if(!scalar(cfg[[k]],10,5000,TRUE))stop("每阶段最多入组人数需为10–5000整数。")
  if(cfg$d1>cfg$n1||cfg$d_max-cfg$d1>cfg$n2)stop("阶段事件上限不能超过对应队列人数上限。")
  for(k in c("window1","window2"))if(!scalar(cfg[[k]],.001,3650))stop("各阶段最大窗口需在(0,3650]日。")
  if(!cfg$enrollment %in% c("batch","constant"))stop("选择阶段开始一次入组或恒定Poisson入组。")
  if(cfg$enrollment=="constant")for(k in c("rate1","rate2"))if(!scalar(cfg[[k]],.Machine$double.eps,1e6))stop("两阶段入组率须为正。")
  if(length(cfg$dropout_rates)!=2||any(!is.finite(cfg$dropout_rates)|cfg$dropout_rates<0))stop("Control和Treatment独立脱落风险率须非负。")
  if(!cfg$control_model$id %in% parameter_catalog()$id)stop("选择Control生存分布。")
  if(!scalar(cfg$hr_true,.01,5))stop("模拟真实HR须为0.01–5。")
  if(!scalar(cfg$reps,20,10000,TRUE)||!scalar(cfg$seed,0,.Machine$integer.max,TRUE))stop("重复数需20–10000整数，种子需0至2147483647整数。")
  if((cfg$n1+cfg$n2)*cfg$reps*length(unique(c(1,cfg$hr_true)))>2e7)stop("本次患者生成量超过2000万，请减少人数或重复数。")
  invisible(TRUE)
}
patient_adaptive_score <- function(observed,control="Control") {
  labs<-unique(observed$group)
  if(nrow(observed)<2||length(labs)!=2||!control %in% labs||sum(observed$event)<1)return(list(valid=FALSE,z=NA_real_,variance=NA_real_,note="两组风险集或事件不足"))
  lr<-study_logrank(observed$time_day,observed$event,as.integer(observed$group!=control))
  valid<-is.finite(lr$variance)&&lr$variance>0
  list(valid=valid,z=if(valid)-lr$score/sqrt(lr$variance) else NA_real_,variance=lr$variance,note=if(valid)"" else "log-rank方差无效")
}
patient_adaptive_interim <- function(data,cfg) {
  data<-patient_adaptive_validate(cfg,data)
  o<-data.frame(USUBJID=data$id,group=data$group,entry_day=data$entry,time_day=data$time,obs_day=data$obs_day,event=data$event,status=data$status,DCO_DAY=cfg$cut,cohort=1L)
  s<-patient_adaptive_score(o,cfg$control);if(!s$valid)stop(s$note)
  c<-cfg;c$purpose<-"interim";c$z1<-0
  d<-adaptive_decision(s$z,c);names(d)[names(d)=="final_events"]<-"combined_target_events"
  list(config=cfg,plan=patient_adaptive_plan(cfg),decision=d,ia=data.frame(n=nrow(data),events=sum(data$event),control=cfg$control,benefit_z=s$z,variance=s$variance,one_sided_p=pnorm(s$z,lower.tail=FALSE)),
    data=data,observed=o,version="0.30.0")
}
patient_adaptive_truth <- function(cfg,stage,hr,b) {
  n<-if(stage==1)cfg$n1 else cfg$n2
  z<-list(n=n,enroll_mode=if(cfg$enrollment=="batch")"schedule" else "constant",entry_days=rep(0,n),enroll_rate=if(stage==1)cfg$rate1 else cfg$rate2,
    groups=list(list(name="Control",weight=1-cfg$p,dropout_rate=cfg$dropout_rates[1],model=cfg$control_model,hazard_multiplier=1),
      list(name="Treatment",weight=cfg$p,dropout_rate=cfg$dropout_rates[2],model=cfg$control_model,hazard_multiplier=hr)))
  t<-simulate_survival_truth(z,b);t$USUBJID<-paste0("C",stage,"-",t$USUBJID);t
}
patient_adaptive_cut <- function(truth,target,window,stage) {
  dates<-sort(truth$event_day[truth$event_time_day<=truth$dropout_time_day&is.finite(truth$event_day)&truth$event_day<=window])
  hit<-length(dates)>=target;day<-if(hit)dates[target] else window
  o<-survival_at_cut(truth,day,stage);o$cohort<-rep(stage,nrow(o))
  list(hit=hit,day=day,observed=o)
}
patient_adaptive_trial <- function(cfg,hr,b,keep=FALSE) {
  a<-patient_adaptive_plan(cfg)
  # Replicate seeds remain stable when B is changed; scenarios and cohorts are distinct.
  set.seed(study_replicate_seed(cfg$seed,match(hr,unique(c(1,cfg$hr_true))),b))
  first<-patient_adaptive_cut(patient_adaptive_truth(cfg,1,hr,b),cfg$d1,cfg$window1,1L)
  s1<-if(first$hit)patient_adaptive_score(first$observed) else list(valid=FALSE,z=NA_real_,variance=NA_real_)
  d2<-a$d2_min;rule_reason<-"stage1_not_analyzed";cp<-NA_real_
  if(first$hit&&s1$valid) {
    c<-cfg;c$purpose<-"interim";c$z1<-0
    decision<-adaptive_decision(s1$z,c);d2<-decision$selected_d2;cp<-decision$cp_selected;rule_reason<-decision$reason
  }
  second_truth<-if(first$hit&&s1$valid)patient_adaptive_truth(cfg,2,hr,b) else NULL
  samples<-list()
  rows<-lapply(c("adaptive","original"),function(design) {
    target<-if(design=="adaptive")d2 else a$d2_min
    second<-if(!is.null(second_truth))patient_adaptive_cut(second_truth,target,cfg$window2,2L) else NULL
    s2<-if(!is.null(second)&&second$hit)patient_adaptive_score(second$observed) else list(valid=FALSE,z=NA_real_,variance=NA_real_)
    action<-if(!first$hit)"stage1_window_unreached" else if(!s1$valid)"stage1_invalid" else if(!second$hit)"stage2_window_unreached" else if(!s2$valid)"stage2_invalid" else "tested"
    valid<-!action %in% c("stage1_invalid","stage2_invalid")
    combined<-if(action=="tested")a$w1*s1$z+a$w2*s2$z else NA_real_
    reject<-if(!valid)NA else if(action=="tested")combined>=a$critical else FALSE
    if(keep) {
      o<-first$observed
      if(!is.null(second)) {
        v<-second$observed
        for(k in c("entry_day","obs_day","DCO_DAY"))v[[k]]<-v[[k]]+first$day
        o<-rbind(o,v)
      }
      o$design<-design;o$true_hr<-hr;samples[[design]]<<-o
    }
    data.frame(SIMID=b,true_hr=hr,design=design,replicate_seed=study_replicate_seed(cfg$seed,match(hr,unique(c(1,cfg$hr_true))),b),
      decision_valid=valid,reject=reject,action=action,z1=s1$z,variance1=s1$variance,z2=s2$z,variance2=s2$variance,combination_z=combined,
      selected_d2=target,combined_target_events=cfg$d1+target,cp_selected=if(design=="adaptive")cp else NA_real_,rule_reason=rule_reason,
      stage1_hit=first$hit,stage2_hit=if(is.null(second))NA else second$hit,
      n1_observed=nrow(first$observed),n2_observed=if(is.null(second))0L else nrow(second$observed),
      events1=sum(first$observed$event),events2=if(is.null(second))0L else sum(second$observed$event),
      ia_day=first$day,final_day=first$day+if(is.null(second))0 else second$day,note="")
  })
  list(rows=do.call(rbind,rows),observed=if(keep)do.call(rbind,samples) else NULL)
}
patient_adaptive_summary <- function(rows,cfg) {
  keys<-expand.grid(true_hr=unique(c(1,cfg$hr_true)),design=c("adaptive","original"),stringsAsFactors=FALSE)
  do.call(rbind,lapply(seq_len(nrow(keys)),function(i){k<-keys[i,];x<-rows[rows$true_hr==k$true_hr&rows$design==k$design,,drop=FALSE]
    n<-nrow(x);valid<-sum(x$decision_valid);reject<-sum(x$reject,na.rm=TRUE);p<-if(valid)reject/valid else NA_real_;z<-qnorm(.975)
    center<-(p+z^2/(2*valid))/(1+z^2/valid);half<-z*sqrt(p*(1-p)/valid+z^2/(4*valid^2))/(1+z^2/valid)
    data.frame(true_hr=k$true_hr,hypothesis=if(k$true_hr==1)"H0" else "HR scenario",design=k$design,requested=cfg$reps,completed=n,valid=valid,invalid=n-valid,pending=cfg$reps-n,
      rejections=reject,rejection_rate=p,mcse=if(valid)sqrt(p*(1-p)/valid) else NA_real_,wilson_low=center-half,wilson_high=center+half,
      rejection_lower=reject/cfg$reps,rejection_upper=(reject+cfg$reps-valid)/cfg$reps,
      stage1_unreached=sum(x$action=="stage1_window_unreached"),stage2_unreached=sum(x$action=="stage2_window_unreached"),
      mean_target_events=if(n)mean(x$combined_target_events) else NA_real_,mean_observed_events=if(n)mean(x$events1+x$events2) else NA_real_,
      mean_n=if(n)mean(x$n1_observed+x$n2_observed) else NA_real_,mean_final_day=if(n)mean(x$final_day) else NA_real_)
  }))
}
run_patient_adaptive <- function(cfg,progress=function(state)NULL,should_cancel=function()FALSE) {
  cfg$purpose<-"patient_simulation";patient_adaptive_validate(cfg)
  had<-exists(".Random.seed",.GlobalEnv,inherits=FALSE);if(had)old<-get(".Random.seed",.GlobalEnv)
  on.exit(if(had)assign(".Random.seed",old,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
  rows<-list();observed<-list();status<-"complete";hrs<-unique(c(1,cfg$hr_true));total<-length(hrs)*cfg$reps;done<-0L
  state<-function() {
    r<-do.call(rbind,rows);if(is.null(r))r<-data.frame(SIMID=integer(),true_hr=numeric(),design=character(),replicate_seed=integer(),decision_valid=logical(),reject=logical(),action=character(),z1=numeric(),variance1=numeric(),z2=numeric(),variance2=numeric(),combination_z=numeric(),selected_d2=numeric(),combined_target_events=numeric(),cp_selected=numeric(),rule_reason=character(),stage1_hit=logical(),stage2_hit=logical(),n1_observed=integer(),n2_observed=integer(),events1=integer(),events2=integer(),ia_day=numeric(),final_day=numeric(),note=character())
    list(config=cfg,plan=patient_adaptive_plan(cfg),trials=r,summary=patient_adaptive_summary(r,cfg),observed=do.call(rbind,observed),completed=done,total=total,status=status,version="0.30.0")
  }
  for(b in seq_len(cfg$reps)) {
    if(should_cancel()){status<-"cancelled";break}
    for(hr in hrs) {
      r<-tryCatch(patient_adaptive_trial(cfg,hr,b,b==1L),error=function(e){
        # Retain an explicit invalid row for each requested paired design.
        x<-data.frame(SIMID=b,true_hr=hr,design=c("adaptive","original"),replicate_seed=study_replicate_seed(cfg$seed,match(hr,hrs),b),decision_valid=FALSE,reject=NA,action="generation_failed",z1=NA_real_,variance1=NA_real_,z2=NA_real_,variance2=NA_real_,combination_z=NA_real_,selected_d2=NA_real_,combined_target_events=NA_real_,cp_selected=NA_real_,rule_reason="generation_failed",stage1_hit=NA,stage2_hit=NA,n1_observed=NA_integer_,n2_observed=NA_integer_,events1=NA_integer_,events2=NA_integer_,ia_day=NA_real_,final_day=NA_real_,note=conditionMessage(e));list(rows=x,observed=NULL)
      })
      rows[[length(rows)+1L]]<-r$rows;if(!is.null(r$observed))observed[[length(observed)+1L]]<-r$observed;done<-done+1L
    }
    if(b%%25L==0L||b==cfg$reps)progress(state())
  }
  state()
}
patient_adaptive_reproduction <- function(result) {
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  paste0('# Run in the event_pred source directory. Disjoint patient cohorts.\n',
    'for(f in c("units","models","forecast","inputs","simulation","study","adaptation","patient_adaptation"))source(paste0("R/",f,".R"))\ncfg <- ',dump(result$config),
    if(result$config$purpose=="patient_interim")paste0('\ndata <- ',dump(result$data),'\nr <- patient_adaptive_interim(data,cfg)\nwrite.csv(r$decision,"patient_adaptive_decision.csv",row.names=FALSE)\n') else
      paste0('\nr <- run_patient_adaptive(cfg,should_cancel=local({i<-0L;function(){i<<-i+1L;i>',result$completed/length(unique(c(1,result$config$hr_true))),'L}}))\nwrite.csv(r$trials,"patient_adaptive_trials.csv",row.names=FALSE)\nwrite.csv(r$summary,"patient_adaptive_summary.csv",row.names=FALSE)\n'))
}
