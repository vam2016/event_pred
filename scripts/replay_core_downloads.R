# Replay the files actually downloaded from the browser, not a synthetic config copy.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e));attach(e)
downloads<-Sys.getenv("CORE_CHECK_OUTPUT",unset="../../work/core-check-downloads")
checks<-character();check<-function(ok,label){if(!isTRUE(ok))stop(paste("FAILED",label));checks<<-c(checks,label);cat("PASS",label,"\n")}
near<-function(x,y) isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-10))
restore_model<-function(spec){id<-spec$method;list(id=id,label=parameter_catalog()$label[match(id,parameter_catalog()$id)],params=unlist(spec$params),cuts=unlist(spec$cuts),warning=character(),aic=NA_real_,input_basis=spec$input_basis)}
for(name in c("count","target")) {
 saved<-jsonlite::fromJSON(file.path(downloads,paste0(name,"-config.json")));cfg<-saved$config
 p<-cfg$cohort_parameters;data<-parameter_cohort(cfg$cut,p$active_n,p$known_n,p$duration,p$age_mode,cfg$seed)
 model<-restore_model(cfg$parameter_model);r<-run_forecast(data,cfg,models_override=setNames(list(model),model$id))
 csv<-read.csv(file.path(downloads,if(name=="count")"count-curves.csv" else "target-milestones.csv"))
 if(name=="count")check(near(r$curves$day,csv$day)&&near(r$curves$mean,csv$mean)&&near(r$curves$median,csv$median)&&near(r$curves$lower,csv$lower)&&near(r$curves$upper,csv$upper),"actual event curve CSV replays from downloaded config")
 else check(near(r$summary$reached,csv$reached)&&near(r$summary$median_day,csv$median_day)&&near(r$summary$lower_day,csv$lower_day)&&near(r$summary$upper_day,csv$upper_day),"actual target CSV replays with unreached mass retained")
}
saved<-jsonlite::fromJSON(file.path(downloads,"fitted-config.json"));cfg<-saved$config;m<-cfg$adtte_mapping
raw<-read.csv(file.path(downloads,"synthetic-adtte.csv"));data<-normalize_adtte(raw,m$paramcd,cfg$origin,cfg$cut,m$offset,m$dropout_codes,flag=m$flag %||% "",flag_value=m$flag_value,date_encoding=m$date_encoding,gap_mode="strict",aval_unit=m$aval_unit)
r<-run_forecast(data,cfg);fcsv<-read.csv(file.path(downloads,"fitted-curves.csv"));check(near(r$curves$mean,fcsv$mean)&&near(r$curves$lower,fcsv$lower)&&near(r$curves$upper,fcsv$upper),"actual data-fitted prediction CSV replays exactly");check(r$data_summary$n==saved$data_summary$n&&r$data_summary$events==saved$data_summary$events,"actual uploaded ADTTE replays with declared date mapping")
saved<-jsonlite::fromJSON(file.path(downloads,"fitted-group-config.json"));cfg<-saved$config;m<-cfg$adtte_mapping
data<-normalize_adtte(raw,m$paramcd,cfg$origin,cfg$cut,m$offset,m$dropout_codes,flag=m$flag %||% "",flag_value=m$flag_value,date_encoding=m$date_encoding,gap_mode="strict",aval_unit=m$aval_unit,group_column=m$group_column)
r<-run_forecast(data,cfg);fcsv<-read.csv(file.path(downloads,"fitted-group-curves.csv"))
check(near(r$curves$mean,fcsv$mean)&&near(r$curves$lower,fcsv$lower)&&near(r$curves$upper,fcsv$upper),"actual known-group ADTTE prediction replays from treatment mapping")
# The browser's downloaded R script runs in the same source root and writes a raw-day summary.
source(file.path(downloads,"sim-reproduce.R"),local=new.env(parent=globalenv()))
actual<-read.csv("simulation_summary.csv");csv<-read.csv(file.path(downloads,"sim-summary.csv"))
check(identical(actual$events,csv$events)&&identical(actual$n,csv$n)&&near(actual$median_day,csv$median_day),"actual downloaded simulation R script reproduces exported statistics")
unlink("simulation_summary.csv")
observed<-read.csv(file.path(downloads,"sim-observed.csv"));truth<-read.csv(file.path(downloads,"sim-truth.csv"))
check(!"event_time_day"%in%names(observed)&&"event_time_day"%in%names(truth),"downloaded observation/truth files remain separate")
jsonlite::write_json(list(version=e$core_version,status="passed",passed=length(checks),checks=checks),"validation/core_replay_results.json",auto_unbox=TRUE,pretty=TRUE)
