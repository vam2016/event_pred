# Replay actual Chrome downloads from V2-102, with original synthetic XPT inputs.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e));attach(e)
downloads<-Sys.getenv("LOG_MODELS_DOWNLOADS");fixtures<-Sys.getenv("SAS_CHECK_FIXTURES")
if(!nzchar(downloads)||!nzchar(fixtures))stop("Set LOG_MODELS_DOWNLOADS and SAS_CHECK_FIXTURES")
checks<-character();check<-function(ok,label){if(!isTRUE(ok))stop(paste("FAILED:",label));checks<<-c(checks,label);cat("PASS:",label,"\n")}
near<-function(x,y)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-10))
restore<-function(spec){id<-spec$method;list(id=id,label=model_catalog()$label[match(id,model_catalog()$id)],params=unlist(spec$params),cuts=unlist(spec$cuts),warning=character(),aic=NA_real_,input_basis=spec$input_basis)}
for(name in c("lognormal","loglogistic","grouped","fitted","fitted-grouped","target")) {
 saved<-jsonlite::fromJSON(file.path(downloads,paste0(name,"-config.json")));cfg<-saved$config;override<-NULL
 if(cfg$input_mode=="adtte") {
  m<-cfg$adtte_mapping;data<-normalize_adtte(read_adtte(file.path(fixtures,"synthetic-v8.xpt")),m$paramcd,cfg$origin,cfg$cut,m$offset,unlist(m$dropout_codes),
   flag=m$flag,flag_value=m$flag_value,date_encoding=m$date_encoding,gap_mode=cfg$gap_mode,aval_unit=m$aval_unit,group_column=m$group_column)
 }else if(cfg$analysis_mode=="grouped") {
  cohorts<-list();override<-list()
  for(i in seq_along(cfg$groups)) {
   key<-names(cfg$groups)[i];g<-cfg$groups[[key]];p<-g$cohort_parameters;dd<-parameter_cohort(cfg$cut,p$active_n,p$known_n,p$duration,p$age_mode,as.integer((cfg$seed+i)%%.Machine$integer.max));dd$id<-paste(key,dd$id,sep="_");dd$group<-rep(g$name,nrow(dd));cohorts[[key]]<-dd
   model<-restore(g$parameter_model);override[[key]]<-setNames(list(model),model$id)
  }
  data<-do.call(rbind,cohorts)
 }else {
  p<-cfg$cohort_parameters;data<-parameter_cohort(cfg$cut,p$active_n,p$known_n,p$duration,p$age_mode,cfg$seed);model<-restore(cfg$parameter_model);override<-setNames(list(model),model$id)
 }
 r<-run_forecast(data,cfg,models_override=override)
 if(name=="target") {
  csv<-read.csv(file.path(downloads,"target-milestones.csv"))
  check(all(vapply(c("reached","lower_day","median_day","upper_day","simulations"),function(id)near(r$summary[[id]],csv[[id]]),logical(1))),"actual log-logistic target download replays including unreached mass and denominator")
 }else {
  csv<-read.csv(file.path(downloads,paste0(name,"-curves.csv")))
  check(identical(r$curves$method,csv$method)&&all(vapply(c("day","mean","lower","median","upper"),function(id)near(r$curves[[id]],csv[[id]]),logical(1))),paste(name,"actual downloaded full prediction curves replay"))
 }
 check(r$data_summary$n==saved$data_summary$n&&r$data_summary$events==saved$data_summary$events,paste(name,"downloaded row and event counts replay"))
 if(name=="grouped") {
  gcsv<-read.csv(file.path(downloads,"grouped-group-curves.csv"));check(near(r$group_curves$mean,gcsv$mean)&&near(r$group_curves$lower,gcsv$lower)&&near(r$group_curves$upper,gcsv$upper),"actual individual group curves replay from distinct distributions")
  mats<-r$group_counts[[1]];check(identical(r$counts[[1]],Reduce(`+`,mats)),"mixed-distribution joint counts equal pathwise group sum")
 }
 if(name %in% c("lognormal","loglogistic")) {
  eq<-read.csv(file.path(downloads,paste0(name,"-parameters.csv")))
  check(any(grepl("sigma",eq$parameter))&&any(grepl("mu",eq$parameter)),paste(name,"downloaded equivalent parameter table includes mu and sigma"))
  if(name=="loglogistic")check(any(is.infinite(eq$value)),"actual heavy-tail equivalent-parameter CSV retains infinite mean")
 }
}
# Run the downloaded script, not a hand-copied simulation configuration.
script_env<-new.env(parent=globalenv());sys.source(file.path(downloads,"sim-reproduce.R"),envir=script_env)
actual<-read.csv("simulation_summary.csv");csv<-read.csv(file.path(downloads,"sim-summary.csv"));unlink("simulation_summary.csv")
check(identical(actual$events,csv$events)&&identical(actual$n,csv$n)&&near(actual$median_day,csv$median_day)&&near(actual$lower_day,csv$lower_day)&&near(actual$upper_day,csv$upper_day),"actual downloaded log-time simulation script reproduces all fixed-DCO summaries")
truth<-read.csv(file.path(downloads,"sim-truth.csv"));obs<-read.csv(file.path(downloads,"sim-observed.csv"))
check(near(script_env$res$truth$event_time_day,truth$event_time_day)&&near(script_env$res$truth$dropout_time_day,truth$dropout_time_day),"actual downloaded script reproduces separate latent event and exit clocks")
s<-script_env$res$observed;selected<-s[s$SIMID==1&s$CUTID==1,]
check(near(selected$time_day,obs$time_day)&&identical(selected$event,obs$event)&&!"event_time_day"%in%names(obs),"actual downloaded observations replay and exclude latent times")
ad<-read.csv(file.path(downloads,"sim-adtte.csv"));check(all(ad$AVAL==as.numeric(as.Date(ad$ADT)-as.Date(ad$STARTDT))+1)&&all(ad$CNSR==ifelse(obs$status=="event",0,ifelse(obs$status=="dropout",2,1))),"actual log-time simulation ADTTE preserves date offset and censor codes")
jsonlite::write_json(list(status="passed",passed=length(checks),checks=checks,R=R.version.string,scope="actual browser-download replay, no new random configuration or real patient data"),"validation/v2_log_models_replay_results.json",auto_unbox=TRUE,pretty=TRUE)
