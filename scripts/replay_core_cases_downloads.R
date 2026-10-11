# Execute the actual browser-downloaded example scripts in isolated directories.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e))
out<-Sys.getenv("CASES_DOWNLOADS");if(!nzchar(out)||!dir.exists(out))stop("Set CASES_DOWNLOADS")
checks<-character();check<-function(ok,label){if(!isTRUE(ok))stop(label);checks<<-c(checks,label);cat("PASS:",label,"\n")}
near<-function(x,y)isTRUE(all.equal(x,y,tolerance=1e-9,check.attributes=FALSE))
read_export<-function(file,ref)read.csv(file,check.names=FALSE,colClasses=vapply(ref,function(z)if(is.character(z))"character" else if(is.logical(z))"logical" else "numeric",character(1)))
for(id in names(e$core_case_catalog())) {
  x<-jsonlite::read_json(file.path(out,paste0(id,".json")),simplifyVector=TRUE)
  r<-e$core_run_case(x);tables<-e$core_case_tables(r)
  check(identical(x$id,id)&&near(tables,e$core_case_tables(e$core_run_case(e$core_case_spec(id)))),paste(id,"actual downloaded JSON agrees with versioned source example"))
  check(identical(readLines(file.path(out,paste0(id,".R")),warn=FALSE),e$core_case_replay_script()),paste(id,"actual R download equals declared replay script"))
  tmp<-tempfile("core-case-replay-");dir.create(tmp)
  for(p in c("app_core.R","R","docs","www"))stopifnot(file.symlink(normalizePath(p),file.path(tmp,p)))
  file.copy(file.path(out,paste0(id,".json")),file.path(tmp,"survcast_case.json"))
  file.copy(file.path(out,paste0(id,".R")),file.path(tmp,"survcast_case_replay.R"))
  old<-getwd();setwd(tmp);log<-tryCatch(system2(file.path(R.home("bin"),"Rscript"),"survcast_case_replay.R",stdout=TRUE,stderr=TRUE),finally=setwd(old))
  check(is.null(attr(log,"status")),paste(id,"actual downloaded script completes in source-linked temporary folder"))
  for(name in names(tables)) {
    ref<-tables[[name]];csv<-read_export(file.path(tmp,paste0("survcast_case_",name,".csv")),ref)
    rownames(ref)<-NULL;rownames(csv)<-NULL
    check(near(ref,csv),paste(id,name,"actual script output reproduces all exported table values"))
  }
  if(isTRUE(x$config$input_mode %in% "adtte")) {
    d<-e$normalize_adtte(e$read_adtte(file.path(out,paste0(id,".csv"))),"OS",x$config$origin,x$config$cut,offset=1,dropout_codes=2)
    check(identical(r$forecast$counts,e$run_forecast(d,x$config)$counts),paste(id,"actual CSV upload mapping reproduces all prediction paths"))
  }
  unlink(tmp,recursive=TRUE)
}
jsonlite::write_json(list(status="passed",passed=length(checks),checks=checks,tolerance="1e-9 for CSV; exact forecast count paths; synthetic actual downloads only"),"validation/v2_core_cases_replay_results.json",auto_unbox=TRUE,pretty=TRUE)
cat("CORE_CASES_REPLAY_CHECKS_PASSED",length(checks),"\n")
