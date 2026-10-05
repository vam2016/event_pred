# Conditional prediction after a real IA. Only an independent NEW cohort is generated.
patient_prediction_is_extended <- function(cfg) {
  (!is.null(cfg$process_uncertainty)&&cfg$process_uncertainty!="fixed")||!is.null(cfg$scenarios)
}
patient_prediction_validate <- function(data,cfg) {
  if(patient_prediction_is_extended(cfg))return(patient_prediction_extended_validate(data,cfg))
  c<-cfg;c$purpose<-"patient_interim";d<-patient_adaptive_validate(c,data)
  scalar<-function(x,lo,hi,int=FALSE)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi&&(!int||x==floor(x))
  if(!identical(cfg$purpose,"patient_prediction"))stop("请选择实际IA重估后的条件预测。")
  if(!scalar(cfg$n2,10,5000,TRUE)||cfg$n2<cfg$d_max-cfg$d1)stop("新队列人数需10–5000整数且不少于Dmax−D1。")
  if(!scalar(cfg$window2,.001,3650))stop("IA后最大窗口需在(0,3650]日。")
  if(!cfg$enrollment %in% c("batch","constant","piecewise"))stop("选择新队列一次、恒定或分段Poisson入组。")
  if(cfg$enrollment=="constant"&&!scalar(cfg$rate2,.Machine$double.eps,1e6))stop("新队列总体入组率须为正。")
  if(cfg$enrollment=="piecewise")parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts)
  if(length(cfg$dropout_rates)!=2||any(!is.finite(cfg$dropout_rates)|cfg$dropout_rates<0))stop("两组独立脱落风险率需非负。")
  if(!scalar(cfg$reps,20,10000,TRUE)||!scalar(cfg$seed,0,.Machine$integer.max,TRUE))stop("模拟数需20–10000整数，种子需0至2147483647整数。")
  if(cfg$n2*cfg$reps>2e7)stop("新患者×模拟数不能超过2000万。")
  if(!cfg$model_mode %in% c("manual","fit")||!cfg$uncertainty %in% c("plugin","bootstrap","gamma","bayes_weibull"))stop("未知事件模型处理方式。")
  if(cfg$model_mode=="manual") {
    if(cfg$uncertainty!="plugin"||length(cfg$manual_models)!=2)stop("手动输入需要两组固定参数模型。")
    for(m in cfg$manual_models)if(!m$id %in% parameter_catalog()$id)stop("未知手动生存分布。")
  } else {
    if(!cfg$fit_method %in% model_catalog()$id)stop("未知拟合模型。")
    for(lab in unique(d$group))validate_data(d[d$group==lab,,drop=FALSE],cfg$cut,require_events=TRUE,gap_mode="strict")
    if(cfg$uncertainty=="gamma") {
      if(!cfg$fit_method %in% c("exponential","pwe"))stop("Gamma后验只支持指数或PWE。")
      if(!scalar(cfg$prior_shape,.Machine$double.eps,1e8)||!scalar(cfg$prior_rate,.Machine$double.eps,1e12))stop("Gamma先验shape和rate须为正。")
    }
    if(cfg$uncertainty=="bayes_weibull"&&cfg$fit_method!="weibull")stop("Weibull MCMC须选择Weibull拟合模型。")
  }
  if(!is.null(cfg$origin)&&(length(cfg$origin)!=1||is.na(as.Date(cfg$origin))))stop("研究起点日期无效。")
  invisible(d)
}
patient_prediction_prepare <- function(data,cfg) {
  labs<-c(cfg$control,setdiff(unique(data$group),cfg$control));models<-list()
  for(j in 1:2) {
    dd<-data[data$group==labs[j],,drop=FALSE]
    m<-if(cfg$model_mode=="manual")cfg$manual_models[[j]] else fit_model(dd,cfg$fit_method,cfg$cuts %||% numeric(),cfg$tail_rate %||% .002)
    if(cfg$uncertainty=="bayes_weibull") {
      # This seed does not depend on B or on later per-replicate draws.
      set.seed(study_replicate_seed(cfg$seed,90L,j));m<-fit_bayesian_weibull(dd,cfg)
      if(!isTRUE(m$posterior$passed))stop(paste(labs[j],"MCMC诊断未通过：需要Rhat≤1.01及bulk/tail ESS≥400。"))
    }
    models[[labs[j]]]<-m
  }
  models
}
patient_prediction_draw <- function(data,cfg,models) {
  lapply(names(models),function(lab) {
    m<-models[[lab]];dd<-data[data$group==lab,,drop=FALSE]
    if(cfg$uncertainty=="bootstrap")m<-fit_model(dd[sample.int(nrow(dd),nrow(dd),replace=TRUE),,drop=FALSE],cfg$fit_method,cfg$cuts %||% numeric(),cfg$tail_rate %||% .002)
    if(cfg$uncertainty=="gamma")m<-gamma_posterior_draw(m,cfg$prior_shape,cfg$prior_rate)
    if(cfg$uncertainty=="bayes_weibull")m<-posterior_model_draw(m)
    m
  })
}
patient_prediction_truth <- function(cfg,draws,labs,ids,b) {
  z<-list(n=cfg$n2,enroll_mode=switch(cfg$enrollment,batch="schedule",constant="constant",piecewise="piecewise"),entry_days=rep(0,cfg$n2),
    enroll_rate=cfg$rate2,enroll_cuts=cfg$enroll_cuts,enroll_rates=cfg$enroll_rates,
    groups=lapply(1:2,function(j)list(name=labs[j],weight=c(1-cfg$p,cfg$p)[j],dropout_rate=cfg$dropout_rates[j],model=draws[[j]],hazard_multiplier=1)))
  t<-simulate_survival_truth(z,b);pre<-"NEW::";newids<-paste0(pre,t$USUBJID)
  while(any(newids %in% ids)){pre<-paste0("_",pre);newids<-paste0(pre,t$USUBJID)}
  t$USUBJID<-newids;t
}
patient_prediction_empty <- function() {
  data.frame(SIMID=integer(),design=character(),replicate_seed=integer(),generation_valid=logical(),decision_valid=logical(),reject=logical(),action=character(),
    z1=numeric(),variance1=numeric(),z2=numeric(),variance2=numeric(),combination_z=numeric(),selected_d2=integer(),combined_target_events=integer(),
    stage2_hit=logical(),n1_observed=integer(),n2_observed=integer(),events1=integer(),events2=integer(),dropouts2=integer(),
    ia_day=numeric(),target_day=numeric(),target_wait_day=numeric(),final_day=numeric(),wait_day=numeric(),note=character())
}
patient_prediction_trial <- function(data,cfg,models,ia,b,keep=FALSE) {
  set.seed(study_replicate_seed(cfg$seed,91L,b))
  # One parameter draw per group per trial, shared by all its new patients.
  draws<-patient_prediction_draw(data,cfg,models)
  truth<-patient_prediction_truth(cfg,draws,names(models),data$id,b)
  z<-patient_prediction_analyze(truth,data,cfg,ia,b,keep);z$draws<-draws;z
}
patient_prediction_analyze <- function(truth,data,cfg,ia,b,keep=FALSE) {
  samples<-list();a<-ia$plan
  rows<-lapply(c("adaptive","original"),function(design) {
    target<-if(design=="adaptive")ia$decision$selected_d2 else a$d2_min
    cut<-patient_adaptive_cut(truth,target,cfg$window2,2L)
    s<-if(cut$hit)patient_adaptive_score(cut$observed,cfg$control) else list(valid=FALSE,z=NA_real_,variance=NA_real_)
    action<-if(!cut$hit)"stage2_window_unreached" else if(!s$valid)"stage2_invalid" else "tested"
    valid<-action!="stage2_invalid";zz<-if(action=="tested")a$w1*ia$ia$benefit_z+a$w2*s$z else NA_real_
    if(keep) {
      o<-ia$observed;o$SIMID<-rep(b,nrow(o));o$CUTID<-rep(1L,nrow(o))
      v<-cut$observed;for(id in c("entry_day","obs_day","DCO_DAY"))v[[id]]<-v[[id]]+cfg$cut
      v<-v[,names(o),drop=FALSE];o<-rbind(o,v);o$design<-design;samples[[design]]<<-o
    }
    data.frame(SIMID=b,design=design,replicate_seed=study_replicate_seed(cfg$seed,91L,b),generation_valid=TRUE,decision_valid=valid,
      reject=if(!valid)NA else if(action=="tested")zz>=a$critical else FALSE,action=action,
      z1=ia$ia$benefit_z,variance1=ia$ia$variance,z2=s$z,variance2=s$variance,combination_z=zz,selected_d2=target,combined_target_events=cfg$d1+target,
      stage2_hit=cut$hit,n1_observed=nrow(data),n2_observed=nrow(cut$observed),events1=cfg$d1,events2=sum(cut$observed$event),dropouts2=sum(cut$observed$status=="dropout"),
      ia_day=cfg$cut,target_day=if(cut$hit)cfg$cut+cut$day else Inf,target_wait_day=if(cut$hit)cut$day else Inf,final_day=cfg$cut+cut$day,wait_day=cut$day,note="")
  })
  list(rows=do.call(rbind,rows),observed=if(keep)do.call(rbind,samples) else NULL)
}
patient_prediction_summary <- function(rows,cfg,ia) {
  probabilities<-resources<-list()
  for(design in c("adaptive","original")) {
    x<-rows[rows$design==design,,drop=FALSE];n<-nrow(x);nv<-sum(x$decision_valid);ng<-sum(x$generation_valid)
    nr<-sum(x$reject,na.rm=TRUE);nh<-sum(x$stage2_hit,na.rm=TRUE);p<-if(nv)nr/nv else NA_real_;z<-qnorm(.975)
    center<-(p+z^2/(2*nv))/(1+z^2/nv);half<-z*sqrt(p*(1-p)/nv+z^2/(4*nv^2))/(1+z^2/nv)
    probabilities[[design]]<-data.frame(design=design,selected_d2=if(design=="adaptive")ia$decision$selected_d2 else ia$plan$d2_min,
      requested=cfg$reps,completed=n,valid=nv,invalid=n-nv,pending=cfg$reps-n,rejections=nr,rejection_rate=p,mcse=if(nv)sqrt(p*(1-p)/nv) else NA_real_,
      wilson_low=center-half,wilson_high=center+half,rejection_lower=nr/cfg$reps,rejection_upper=(nr+cfg$reps-nv)/cfg$reps,
      generated=ng,target_hits=nh,target_hit_rate=if(ng)nh/ng else NA_real_,target_lower=nh/cfg$reps,target_upper=(nh+cfg$reps-ng)/cfg$reps,
      stage2_unreached=sum(x$action=="stage2_window_unreached"))
    y<-x[x$generation_valid,,drop=FALSE];hit<-y[y$stage2_hit,,drop=FALSE];missing<-cfg$reps-ng
    q<-function(v)if(length(v))quantile_with_inf(v) else rep(NA_real_,3)
    hq<-q(hit$target_wait_day);gq<-q(y$target_wait_day);fq<-q(y$wait_day)
    # Full-request quantile bounds: unknown replicates can lie at IA or beyond the window.
    low<-q(c(y$target_wait_day,rep(0,missing)));up<-q(c(y$target_wait_day,rep(Inf,missing)))
    avg<-function(id)if(ng)mean(y[[id]]) else NA_real_
    resources[[design]]<-data.frame(design=design,generated=ng,target_hits=nh,
      hit_wait_lower_day=hq[1],hit_wait_median_day=hq[2],hit_wait_upper_day=hq[3],
      generated_wait_lower_day=gq[1],generated_wait_median_day=gq[2],generated_wait_upper_day=gq[3],
      all_wait_median_lower_day=low[2],all_wait_median_upper_day=up[2],
      termination_lower_day=fq[1],termination_median_day=fq[2],termination_upper_day=fq[3],
      mean_new_n=avg("n2_observed"),mean_new_events=avg("events2"),mean_new_dropouts=avg("dropouts2"),frozen_ia_n=nrow(ia$data),frozen_ia_events=cfg$d1)
    if(!is.null(cfg$origin)) {
      date<-function(v)if(!is.finite(v))if(is.na(v))NA_character_ else "窗口内未达" else as.character(as.Date(cfg$origin)+floor(cfg$cut+v))
      resources[[design]]$hit_median_date<-date(hq[2]);resources[[design]]$generated_median_date<-date(gq[2]);resources[[design]]$termination_median_date<-date(fq[2])
    }
  }
  ad<-rows[rows$design=="adaptive",,drop=FALSE];org<-rows[rows$design=="original",,drop=FALSE];ok<-ad$decision_valid&org$decision_valid
  d<-as.integer(ad$reject[ok])-as.integer(org$reject[ok]);nv<-length(d)
  paired<-data.frame(requested=cfg$reps,paired_valid=nv,mean_rejection_difference=if(nv)mean(d) else NA_real_,
    paired_mcse=if(nv>1)sd(d)/sqrt(nv) else NA_real_,difference_lower=(sum(d)-(cfg$reps-nv))/cfg$reps,difference_upper=(sum(d)+cfg$reps-nv)/cfg$reps)
  list(summary=do.call(rbind,probabilities),resources=do.call(rbind,resources),paired=paired)
}
run_patient_prediction <- function(data,cfg,progress=function(state)NULL,should_cancel=function()FALSE) {
  if(patient_prediction_is_extended(cfg))return(run_patient_prediction_extended(data,cfg,progress,should_cancel))
  data<-patient_prediction_validate(data,cfg);c<-cfg;c$purpose<-"patient_interim";ia<-patient_adaptive_interim(data,c)
  had<-exists(".Random.seed",.GlobalEnv,inherits=FALSE);if(had)old<-get(".Random.seed",.GlobalEnv)
  on.exit(if(had)assign(".Random.seed",old,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
  models<-list();rows<-list();observed<-NULL;done<-0L;status<-"complete"
  state<-function() {
    trials<-if(length(rows))do.call(rbind,rows) else patient_prediction_empty();s<-patient_prediction_summary(trials,cfg,ia)
    c(list(config=cfg,data=data,plan=ia$plan,ia=ia$ia,decision=ia$decision,ia_observed=ia$observed,models=models,
      trials=trials,observed=observed,completed=done,total=cfg$reps,status=status,version="0.30.0"),s)
  }
  if(should_cancel()){status<-"cancelled";return(state())}
  models<-patient_prediction_prepare(data,cfg)
  for(b in seq_len(cfg$reps)) {
    if(should_cancel()){status<-"cancelled";break}
    attempt<-tryCatch(patient_prediction_trial(data,cfg,models,ia,b,keep=is.null(observed)),error=function(e)e)
    if(inherits(attempt,"error")) {
      # Preserve frozen IA and target even if model drawing or generation failed.
      x<-patient_prediction_empty();x[1:2,]<-NA;x$SIMID<-b;x$design<-c("adaptive","original");x$replicate_seed<-study_replicate_seed(cfg$seed,91L,b)
      x$generation_valid<-FALSE;x$decision_valid<-FALSE;x$reject<-NA;x$action<-"generation_failed";x$note<-conditionMessage(attempt)
      x$z1<-ia$ia$benefit_z;x$variance1<-ia$ia$variance;x$selected_d2<-c(ia$decision$selected_d2,ia$plan$d2_min);x$combined_target_events<-cfg$d1+x$selected_d2
      x$n1_observed<-nrow(data);x$events1<-cfg$d1;x$ia_day<-cfg$cut;rows[[b]]<-x
    } else {rows[[b]]<-attempt$rows;if(is.null(observed))observed<-attempt$observed}
    done<-b;if(b%%25L==0L||b==cfg$reps)progress(state())
  }
  state()
}
patient_prediction_sample <- function(r,b,scenario=NULL) {
  if(isTRUE(r$extended))return(patient_prediction_extended_sample(r,b,scenario %||% "输入基准"))
  if(length(b)!=1||!is.finite(b)||b!=floor(b)||!b %in% r$trials$SIMID[r$trials$generation_valid])stop("选择已成功生成的模拟轮次。")
  had<-exists(".Random.seed",.GlobalEnv,inherits=FALSE);if(had)old<-get(".Random.seed",.GlobalEnv)
  on.exit(if(had)assign(".Random.seed",old,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
  ia<-list(plan=r$plan,ia=r$ia,decision=r$decision,observed=r$ia_observed)
  patient_prediction_trial(r$data,r$config,r$models,ia,as.integer(b),TRUE)
}
patient_prediction_model_table <- function(r) {
  out<-lapply(names(r$models),function(lab) {
    m<-r$models[[lab]];p<-unit_model_parameters(m,r$config$display_unit)
    data.frame(group=lab,model=m$label,parameters=paste(names(p),format(p,digits=7),sep="=",collapse="; "),
      event_uncertainty=r$config$uncertainty,
      posterior_parameters=if(r$config$uncertainty=="gamma")paste0("shape=",paste(format(m$events+r$config$prior_shape,digits=7),collapse=","),"; rate（受试者·",time_label(r$config$display_unit),"）=",paste(format((m$exposure+r$config$prior_rate)/time_factor(r$config$display_unit),digits=7),collapse=",")) else "",
      warning=paste(m$warning,collapse="; "))
  });if(length(out))do.call(rbind,out) else data.frame()
}
patient_prediction_diagnostics <- function(r) {
  out<-lapply(names(r$models),function(lab){m<-r$models[[lab]];d<-m$posterior$diagnostics;if(is.null(d))return(NULL);d$group<-lab;d$passed<-m$posterior$passed;d})
  x<-do.call(rbind,out);if(is.null(x))data.frame() else x
}
patient_prediction_reproduction <- function(r) {
  if(isTRUE(r$extended))return(patient_prediction_extended_reproduction(r))
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  paste0('# Run in event_pred. Stage 1 is frozen; stage 2 uses independent new patients only.\n',
    'for(f in c("units","models","forecast","inputs","bayes","simulation","study","adaptation","patient_adaptation","patient_adaptive_prediction","patient_prediction_process"))source(paste0("R/",f,".R"))\n',
    'cfg <- ',dump(r$config),'\ndata <- ',dump(r$data),
    '\nr <- run_patient_prediction(data,cfg,should_cancel=local({i<- -1L;function(){i<<-i+1L;i>',r$completed,'L}}))\n',
    'write.csv(r$trials,"patient_prediction_trials.csv",row.names=FALSE)\nwrite.csv(r$summary,"patient_prediction_summary.csv",row.names=FALSE)\nwrite.csv(r$resources,"patient_prediction_resources.csv",row.names=FALSE)\n')
}
