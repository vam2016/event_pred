# V2-201: regenerate actual browser forecasts from original XPT and frozen config.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e));attach(e)
downloads<-Sys.getenv("BOOTSTRAP_DOWNLOADS");fixtures<-Sys.getenv("SAS_CHECK_FIXTURES")
if(!nzchar(downloads)||!nzchar(fixtures))stop("Set BOOTSTRAP_DOWNLOADS and SAS_CHECK_FIXTURES")
checks<-character();check<-function(ok,label){if(!isTRUE(ok))stop(paste("FAILED:",label));checks<<-c(checks,label);cat("PASS:",label,"\n")}
near<-function(x,y)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-10))
for(name in c("pooled","grouped","target","partial")){
 saved<-jsonlite::fromJSON(file.path(downloads,paste0(name,"-config.json")));cfg<-saved$config;m<-cfg$adtte_mapping
 data<-normalize_adtte(read_adtte(file.path(fixtures,if(name=="partial")"partial.csv" else "synthetic-v8.xpt")),m$paramcd,cfg$origin,cfg$cut,m$offset,unlist(m$dropout_codes),flag=m$flag,flag_value=m$flag_value,date_encoding=m$date_encoding,gap_mode=cfg$gap_mode,aval_unit=m$aval_unit,group_column=m$group_column)
 r<-run_forecast(data,cfg)
 if(name=="target"){
  csv<-read.csv(file.path(downloads,"target-milestones.csv"))
  check(all(vapply(c("reached","reached_mcse","lower_day","median_day","upper_day","requested","simulations","failed"),function(id)near(r$summary[[id]],csv[[id]]),logical(1))),"actual bootstrap target dates, probabilities and denominator replay from original data")
  check(all(is.infinite(r$milestones[[1]]))&&all(r$summary$reached==0),"actual unreachable bootstrap targets retain effective misses")
 }else{
  csv<-read.csv(file.path(downloads,paste0(name,"-curves.csv")))
  check(identical(csv$method,r$curves$method)&&all(vapply(c("day","mean","lower","median","upper"),function(id)near(r$curves[[id]],csv[[id]]),logical(1))),paste(name,"actual three-model bootstrap curves and intervals replay"))
 }
 diag<-read.csv(file.path(downloads,paste0(name,"-diagnostics.csv")))
 check(all(vapply(c("requested","simulations","failed","fit_failed","simulation_failed"),function(id)near(r$diagnostics[[id]],diag[[id]])&&near(r$diagnostics[[id]],saved$diagnostics[[id]]),logical(1))),paste(name,"actual CSV and JSON diagnostics agree with replayed request, valid and failure counts"))
 failure<-read.csv(file.path(downloads,paste0(name,"-failures.csv")))
 check(nrow(failure)==nrow(r$simulation_failures)&&all(vapply(names(failure),function(id)identical(as.character(failure[[id]]),as.character(r$simulation_failures[[id]])),logical(1))),paste(name,"actual failure CSV replays attempt, group, stage and reason"))
 check(r$data_summary$n==saved$data_summary$n&&r$data_summary$events==saved$data_summary$events&&all(vapply(r$counts,function(mat)all(mat[,1]==saved$data_summary$events),logical(1))),paste(name,"bootstrap replay keeps original selected patients and known-event baseline"))
 if(name=="pooled"){
  readout<-read.csv(file.path(downloads,"pooled-readout.csv"),check.names=FALSE)
  ref<-core_forecast_readout(r,121)
  check(near(readout$累计均值,ref$累计均值)&&near(readout$请求次数,ref$请求次数)&&near(readout$成功模拟次数,ref$成功模拟次数)&&near(readout$失败次数,ref$失败次数),"actual linked time-readout CSV replays counts and requested/effective denominator")
 }
 if(name=="partial"){
  check(r$diagnostics$simulations==99&&r$diagnostics$fit_failed==1&&nrow(r$simulation_failures)==1&&r$simulation_failures$stage=="fit","actual nonempty failure record replays its fit phase, reason and effective denominator")
  readout<-read.csv(file.path(downloads,"partial-readout.csv"),check.names=FALSE)
  check(readout$请求次数==100&&readout$成功模拟次数==99&&readout$失败次数==1,"actual partial-failure readout reports 100 requested, 99 effective and one failed attempt")
 }
 if(name=="grouped"){
  csv<-read.csv(file.path(downloads,"grouped-group-curves.csv"))
  check(near(csv$mean,r$group_curves$mean)&&near(csv$lower,r$group_curves$lower)&&near(csv$upper,r$group_curves$upper),"actual within-group bootstrap curves replay")
  check(all(vapply(names(r$counts),function(id)identical(r$counts[[id]],Reduce(`+`,r$group_counts[[id]])),logical(1))),"all three grouped bootstrap models conserve pathwise joint counts")
 }
}
for(name in c("frozen","unsupported-retained","sparse-retained")){
 filename<-if(name=="frozen")"frozen-config.json" else paste0(name,".json")
 saved<-jsonlite::fromJSON(file.path(downloads,filename));original<-jsonlite::fromJSON(file.path(downloads,"pooled-config.json"))
 check(identical(saved$config,original$config)&&identical(saved$diagnostics,original$diagnostics),paste(name,"actual downloaded configuration and diagnostics remain the last successful result"))
}
jsonlite::write_json(list(status="passed",passed=length(checks),checks=checks,R=R.version.string,scope="actual downloaded three-model bootstrap forecasts and frozen diagnostics, original synthetic XPT, no generated replacement configuration"),"validation/v2_bootstrap_replay_results.json",auto_unbox=TRUE,pretty=TRUE)
cat("BOOTSTRAP_REPLAY_CHECKS_PASSED",length(checks),"\n")
