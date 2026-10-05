# Future-process posterior and paired sensitivity scenarios. IA is never resampled.
patient_prediction_parse_scenarios <- function(text) {
  x<-tryCatch(read.csv(text=text,check.names=FALSE,stringsAsFactors=FALSE,strip.white=TRUE),error=function(e)stop("情景表须为CSV，每行一个名称与五个倍率。"))
  expected<-c("情景","Control事件倍率","Treatment事件倍率","入组倍率","Control脱落倍率","Treatment脱落倍率")
  if(!identical(names(x),expected))stop(paste0("情景表表头须为：",paste(expected,collapse=",")))
  names(x)<-c("name","event_control_q","event_treatment_q","enroll_q","dropout_control_q","dropout_treatment_q");x
}
patient_prediction_scenarios <- function(cfg) {
  base<-data.frame(name="输入基准",event_control_q=1,event_treatment_q=1,enroll_q=1,dropout_control_q=1,dropout_treatment_q=1,stringsAsFactors=FALSE)
  x<-cfg$scenarios
  if(!is.null(x)) {
    if(!is.data.frame(x)||!identical(names(x),names(base))||nrow(x)<1||nrow(x)>11)stop("比较表需1–11个情景，另自动保留输入基准。")
    x$name<-trimws(as.character(x$name))
    if(anyNA(x$name)||any(!nzchar(x$name))||any(nchar(x$name)>48)||anyDuplicated(x$name)||"输入基准" %in% x$name)stop("情景名称需非空、唯一且不超过48字；输入基准为保留名称。")
    for(id in setdiff(names(x),"name")) {
      lo<-if(grepl("dropout",id))0 else .01
      if(!is.numeric(x[[id]])||any(!is.finite(x[[id]])|x[[id]]<lo|x[[id]]>10))stop("事件和入组倍率需0.01–10，脱落倍率需0–10。")
    }
    if(cfg$enrollment=="batch"&&any(x$enroll_q!=1))stop("一次入组情景的入组倍率须为1。")
    base<-rbind(base,x)
  }
  rownames(base)<-NULL;base
}
patient_prediction_process_posterior <- function(data,cfg) {
  mode<-cfg$process_uncertainty %||% "fixed"
  if(mode=="fixed")return(NULL)
  labs<-c(cfg$control,setdiff(unique(data$group),cfg$control));c<-cfg;c$process_uncertainty<-"gamma"
  if(mode=="gamma_dropout") {c$recruit_start<-0;c$recruit_end<-cfg$cut;c$enroll_prior_shape<-1;c$enroll_prior_rate<-1}
  drops<-lapply(labs,function(lab)process_posterior(data[data$group==lab,,drop=FALSE],c)$dropout);names(drops)<-labs
  list(enroll=if(mode=="gamma")process_posterior(data,c)$enroll else NULL,dropout=drops)
}
patient_prediction_extended_validate <- function(data,cfg) {
  # Validate data before calculating its exposure or count sufficient statistics.
  c<-cfg;c$purpose<-"patient_interim";d<-patient_adaptive_validate(c,data)
  mode<-cfg$process_uncertainty %||% "fixed"
  scalar<-function(x)is.numeric(x)&&length(x)==1&&is.finite(x)
  if(!mode %in% c("fixed","gamma_dropout","gamma"))stop("请选择输入固定过程、脱落Gamma或入组与脱落Gamma。")
  if(mode!="fixed")for(id in c("drop_prior_shape","drop_prior_rate"))if(!scalar(cfg[[id]])||cfg[[id]]<=0)stop("脱落Gamma先验shape和rate须为正。")
  if(mode=="gamma") {
    if(cfg$enrollment!="constant")stop("联合过程后验只支持未来恒定Poisson入组。")
    if(!isTRUE(cfg$recruit_complete))stop("估计入组后验前，需确认历史窗口内入组记录完整且期间采用恒定Poisson入组。")
    for(id in c("enroll_prior_shape","enroll_prior_rate"))if(!scalar(cfg[[id]])||cfg[[id]]<=0)stop("入组Gamma先验shape和rate须为正。")
    if(!scalar(cfg$recruit_start)||!scalar(cfg$recruit_end)||cfg$recruit_start<0||cfg$recruit_end<=cfg$recruit_start||cfg$recruit_end>cfg$cut)stop("历史入组窗口需满足0≤起点<终点≤IA。")
  }
  p<-patient_prediction_process_posterior(d,cfg)
  c<-cfg;c$process_uncertainty<-"fixed";c$scenarios<-NULL
  # Substitute posterior means only for validating unused input-rate fields.
  if(!is.null(p))c$dropout_rates<-vapply(p$dropout,function(z)unname(z["shape"]/z["rate"]),numeric(1))
  if(!is.null(p$enroll))c$rate2<-unname(p$enroll["shape"]/p$enroll["rate"])
  d<-patient_prediction_validate(d,c);s<-patient_prediction_scenarios(cfg)
  if(cfg$n2*cfg$reps*nrow(s)>2e7)stop("新患者×模拟数×情景数不能超过2000万。")
  invisible(d)
}
patient_prediction_process_draw <- function(cfg,posterior) {
  z<-list(enroll_rate=if(cfg$enrollment=="constant")cfg$rate2 else NA_real_,dropout_rates=cfg$dropout_rates)
  if(!is.null(posterior)) {
    z$dropout_rates<-vapply(posterior$dropout,function(p)rgamma(1,p["shape"],p["rate"]),numeric(1))
    if(!is.null(posterior$enroll))z$enroll_rate<-rgamma(1,posterior$enroll["shape"],posterior$enroll["rate"])
    if(any(!is.finite(z$dropout_rates)|z$dropout_rates<=0)||(!is.null(posterior$enroll)&&(!is.finite(z$enroll_rate)||z$enroll_rate<=0)))stop("过程Gamma抽样出现非正或非有限率；该轮保留为失败，请复核先验。")
  }
  z
}
patient_prediction_latent <- function(cfg,process) {
  # Draw every component once. Zero-rate scenarios never change later streams.
  n<-cfg$n2;arrival<-if(cfg$enrollment=="batch")NULL else rexp(n)
  arm<-sample.int(2,n,replace=TRUE,prob=c(1-cfg$p,cfg$p));event_u<-drop_exp<-numeric(n)
  for(j in 1:2) {
    ix<-which(arm==j);event_u[ix]<-runif(length(ix))
    drop_exp[ix]<-if(process$dropout_rates[j]>0)rexp(length(ix)) else Inf
  }
  list(arrival=arrival,arm=arm,event_u=event_u,drop_exp=drop_exp)
}
patient_prediction_scenario_truth <- function(cfg,draws,process,latent,s,labs,ids,b) {
  entry<-switch(cfg$enrollment,batch=rep(0,cfg$n2),
    constant=cumsum(latent$arrival/(process$enroll_rate*s$enroll_q)),
    piecewise=inverse_cumhaz(parameter_model("pwe",list(rates=cfg$enroll_rates*s$enroll_q),cfg$enroll_cuts),cumsum(latent$arrival)))
  event<-drop<-rep(Inf,cfg$n2)
  for(j in 1:2) {
    ix<-which(latent$arm==j)
    if(!length(ix))next
    event[ix]<-sample_conditional(draws[[j]],rep(0,length(ix)),c(s$event_control_q,s$event_treatment_q)[j],u=latent$event_u[ix])
    rate<-process$dropout_rates[j]*c(s$dropout_control_q,s$dropout_treatment_q)[j]
    if(rate>0)drop[ix]<-latent$drop_exp[ix]/rate
  }
  pre<-"NEW::";uid<-paste0(pre,sprintf("SIM%03d-%05d",b,seq_len(cfg$n2)))
  while(any(uid %in% ids)){pre<-paste0("_",pre);uid<-paste0(pre,sprintf("SIM%03d-%05d",b,seq_len(cfg$n2)))}
  data.frame(SIMID=b,USUBJID=uid,group=labs[latent$arm],entry_day=entry,event_time_day=event,dropout_time_day=drop,event_day=entry+event,dropout_day=entry+drop,stringsAsFactors=FALSE)
}
patient_prediction_extended_batch <- function(data,cfg,models,posterior,ia,b,keep=FALSE) {
  set.seed(study_replicate_seed(cfg$seed,91L,b));draws<-patient_prediction_draw(data,cfg,models)
  process<-patient_prediction_process_draw(cfg,posterior);latent<-patient_prediction_latent(cfg,process);scenarios<-patient_prediction_scenarios(cfg)
  parts<-lapply(seq_len(nrow(scenarios)),function(j) {
    s<-scenarios[j,,drop=FALSE]
    tryCatch({t<-patient_prediction_scenario_truth(cfg,draws,process,latent,s,names(models),data$id,b);a<-patient_prediction_analyze(t,data,cfg,ia,b,keep)
      a$rows$SCENARIO<-s$name;if(!is.null(a$observed))a$observed$SCENARIO<-s$name;a
    },error=function(e)list(rows=patient_prediction_extended_failure(cfg,ia,b,s$name,conditionMessage(e)),observed=NULL))
  })
  list(rows=do.call(rbind,lapply(parts,`[[`,"rows")),observed=do.call(rbind,lapply(parts,`[[`,"observed")),
    process_draw=data.frame(SIMID=b,enroll_rate_per_day=process$enroll_rate,dropout_control_rate_per_day=unname(process$dropout_rates[1]),dropout_treatment_rate_per_day=unname(process$dropout_rates[2])),draws=draws,latent=latent)
}
patient_prediction_extended_failure <- function(cfg,ia,b,scenario,message) {
  x<-patient_prediction_empty();x[1:2,]<-NA;x$SIMID<-b;x$design<-c("adaptive","original");x$replicate_seed<-study_replicate_seed(cfg$seed,91L,b)
  x$generation_valid<-FALSE;x$decision_valid<-FALSE;x$reject<-NA;x$action<-"generation_failed";x$note<-message
  x$z1<-ia$ia$benefit_z;x$variance1<-ia$ia$variance;x$selected_d2<-c(ia$decision$selected_d2,ia$plan$d2_min);x$combined_target_events<-cfg$d1+x$selected_d2
  x$n1_observed<-nrow(ia$data);x$events1<-cfg$d1;x$ia_day<-cfg$cut;x$SCENARIO<-scenario;x
}
patient_prediction_scenario_contrasts <- function(rows,cfg) {
  s<-patient_prediction_scenarios(cfg);out<-list()
  for(scenario in setdiff(s$name,"输入基准"))for(design in c("adaptive","original")) {
    base<-rows[rows$SCENARIO=="输入基准"&rows$design==design,,drop=FALSE]
    x<-rows[rows$SCENARIO==scenario&rows$design==design,,drop=FALSE];base<-base[match(x$SIMID,base$SIMID),,drop=FALSE]
    ok<-x$decision_valid&base$decision_valid;delta<-as.integer(x$reject[ok])-as.integer(base$reject[ok]);nv<-length(delta)
    g<-x$generation_valid&base$generation_valid;hit<-x$stage2_hit[g]&base$stage2_hit[g]
    wait<-x$target_wait_day[g][hit]-base$target_wait_day[g][hit];close<-x$wait_day[g]-base$wait_day[g]
    dh<-as.integer(x$stage2_hit[g])-as.integer(base$stage2_hit[g]);ng<-sum(g)
    mean_safe<-function(v)if(length(v))mean(v) else NA_real_;se<-function(v)if(length(v)>1)sd(v)/sqrt(length(v)) else NA_real_
    out[[length(out)+1]]<-data.frame(SCENARIO=scenario,reference="输入基准",design=design,requested=cfg$reps,paired_valid=nv,
      mean_rejection_difference=mean_safe(delta),paired_mcse=se(delta),difference_lower=(sum(delta)-(cfg$reps-nv))/cfg$reps,difference_upper=(sum(delta)+cfg$reps-nv)/cfg$reps,
      paired_generated=ng,hit_difference=mean_safe(dh),hit_difference_mcse=se(dh),hit_difference_lower=(sum(dh)-(cfg$reps-ng))/cfg$reps,hit_difference_upper=(sum(dh)+cfg$reps-ng)/cfg$reps,
      both_hit=sum(hit),both_hit_wait_difference_day=mean_safe(wait),both_hit_wait_mcse_day=se(wait),termination_difference_day=mean_safe(close),termination_difference_mcse_day=se(close))
  }
  x<-do.call(rbind,out);if(is.null(x))data.frame() else x
}
patient_prediction_extended_summary <- function(rows,cfg,ia) {
  scenarios<-patient_prediction_scenarios(cfg);parts<-lapply(scenarios$name,function(name){
    z<-patient_prediction_summary(rows[rows$SCENARIO==name,,drop=FALSE],cfg,ia)
    lapply(z,function(x){x$SCENARIO<-name;x})
  })
  result<-lapply(c("summary","resources","paired"),function(id)do.call(rbind,lapply(parts,`[[`,id)));names(result)<-c("summary","resources","paired")
  c(result,list(scenario_contrasts=patient_prediction_scenario_contrasts(rows,cfg)))
}
run_patient_prediction_extended <- function(data,cfg,progress=function(state)NULL,should_cancel=function()FALSE) {
  data<-patient_prediction_extended_validate(data,cfg);c<-cfg;c$purpose<-"patient_interim";ia<-patient_adaptive_interim(data,c)
  had<-exists(".Random.seed",.GlobalEnv,inherits=FALSE);if(had)old<-get(".Random.seed",.GlobalEnv)
  on.exit(if(had)assign(".Random.seed",old,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
  scenarios<-patient_prediction_scenarios(cfg);posterior<-patient_prediction_process_posterior(data,cfg)
  models<-list();rows<-list();observed<-NULL;process_draws<-list();done<-0L;status<-"complete"
  state<-function() {
    trials<-if(length(rows))do.call(rbind,rows) else {x<-patient_prediction_empty();x$SCENARIO<-character();x}
    dd<-if(length(process_draws))do.call(rbind,process_draws) else data.frame(SIMID=integer(),enroll_rate_per_day=numeric(),dropout_control_rate_per_day=numeric(),dropout_treatment_rate_per_day=numeric())
    c(list(config=cfg,data=data,plan=ia$plan,ia=ia$ia,decision=ia$decision,ia_observed=ia$observed,models=models,process_posteriors=posterior,process_draws=dd,scenarios=scenarios,
      trials=trials,observed=observed,completed=done*nrow(scenarios),completed_batches=done,total=cfg$reps*nrow(scenarios),status=status,extended=TRUE,version="0.30.0"),patient_prediction_extended_summary(trials,cfg,ia))
  }
  if(should_cancel()){status<-"cancelled";return(state())}
  models<-patient_prediction_prepare(data,cfg)
  for(b in seq_len(cfg$reps)) {
    if(should_cancel()){status<-"cancelled";break}
    z<-tryCatch(patient_prediction_extended_batch(data,cfg,models,posterior,ia,b,keep=is.null(observed)),error=function(e)e)
    if(inherits(z,"error")) {
      rows[[b]]<-do.call(rbind,lapply(scenarios$name,function(s)patient_prediction_extended_failure(cfg,ia,b,s,conditionMessage(z))))
      process_draws[[b]]<-data.frame(SIMID=b,enroll_rate_per_day=NA_real_,dropout_control_rate_per_day=NA_real_,dropout_treatment_rate_per_day=NA_real_)
    } else {rows[[b]]<-z$rows;process_draws[[b]]<-z$process_draw;if(is.null(observed)&&!is.null(z$observed))observed<-z$observed}
    done<-b;if(b%%25L==0L||b==cfg$reps)progress(state())
  }
  state()
}
patient_prediction_extended_sample <- function(r,b,scenario="输入基准") {
  if(length(b)!=1||!is.finite(b)||b!=floor(b)||length(scenario)!=1||!scenario %in% r$scenarios$name||!b %in% r$trials$SIMID[r$trials$SCENARIO==scenario&r$trials$generation_valid])stop("选择该情景中已成功生成的轮次。")
  had<-exists(".Random.seed",.GlobalEnv,inherits=FALSE);if(had)old<-get(".Random.seed",.GlobalEnv)
  on.exit(if(had)assign(".Random.seed",old,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
  ia<-list(plan=r$plan,ia=r$ia,decision=r$decision,observed=r$ia_observed,data=r$data)
  z<-patient_prediction_extended_batch(r$data,r$config,r$models,r$process_posteriors,ia,as.integer(b),TRUE)
  z$rows<-z$rows[z$rows$SCENARIO==scenario,,drop=FALSE];z$observed<-z$observed[z$observed$SCENARIO==scenario,,drop=FALSE];z
}
patient_prediction_process_table <- function(r) {
  p<-r$process_posteriors;if(is.null(p))return(data.frame())
  f<-time_factor(r$config$display_unit);u<-time_label(r$config$display_unit)
  row<-function(kind,group,z,count,exposure,units) {
    q<-qgamma(c(.025,.5,.975),z["shape"],z["rate"])*f
    data.frame(process=kind,group=group,historical_count=count,historical_exposure=exposure/f,exposure_unit=units,
      posterior_shape=unname(z["shape"]),posterior_rate=unname(z["rate"])/f,rate_unit=paste0(if(kind=="入组")"人/" else "每",u),
      posterior_mean=unname(z["shape"]/z["rate"])*f,posterior_lower=q[1],posterior_median=q[2],posterior_upper=q[3])
  }
  out<-list()
  if(!is.null(p$enroll))out[[1]]<-row("入组","总体",p$enroll,sum(r$data$entry>=r$config$recruit_start&r$data$entry<=r$config$recruit_end),r$config$recruit_end-r$config$recruit_start,u)
  for(lab in names(p$dropout)) {d<-r$data[r$data$group==lab,,drop=FALSE];out[[length(out)+1]]<-row("永久脱落",lab,p$dropout[[lab]],sum(d$status=="dropout"),sum(d$time),paste0("受试者·",u))}
  do.call(rbind,out)
}
patient_prediction_process_export <- function(r) {
  x<-r$process_draws;u<-r$config$display_unit;f<-time_factor(u)
  for(id in names(x)[grepl("_per_day$",names(x))])x[[sub("_per_day$",paste0("_per_",u),id)]]<-x[[id]]*f
  x$time_unit<-rep(u,nrow(x));x$days_per_unit<-rep(f,nrow(x));x
}
patient_prediction_extended_reproduction <- function(r) {
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  paste0('# Run in event_pred. Fixed actual IA; all future scenarios share each replicate.\n',
    'for(f in c("units","models","forecast","inputs","bayes","simulation","study","adaptation","patient_adaptation","patient_adaptive_prediction","patient_prediction_process"))source(paste0("R/",f,".R"))\n',
    'cfg <- ',dump(r$config),'\ndata <- ',dump(r$data),
    '\nr <- run_patient_prediction(data,cfg,should_cancel=local({i<- -1L;function(){i<<-i+1L;i>',r$completed_batches,'L}}))\n',
    'write.csv(r$trials,"patient_prediction_trials.csv",row.names=FALSE)\nwrite.csv(r$summary,"patient_prediction_summary.csv",row.names=FALSE)\nwrite.csv(r$resources,"patient_prediction_resources.csv",row.names=FALSE)\n',
    'write.csv(r$scenario_contrasts,"patient_prediction_scenario_contrasts.csv",row.names=FALSE)\nwrite.csv(r$paired,"patient_prediction_paired.csv",row.names=FALSE)\nwrite.csv(patient_prediction_process_export(r),"patient_prediction_process_draws.csv",row.names=FALSE)\n')
}
