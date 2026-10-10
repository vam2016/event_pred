# Re-read each original synthetic file using the configuration actually downloaded by Chrome.
e <- new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e));attach(e)
directory <- Sys.getenv("SAS_CHECK_FIXTURES"); downloads <- Sys.getenv("SAS_BROWSER_DOWNLOADS")
if(!nzchar(directory) || !nzchar(downloads)) stop("Set SAS_CHECK_FIXTURES and SAS_BROWSER_DOWNLOADS.")
cases <- c(csv="synthetic.csv",v5="synthetic-v5.xpt",v8="synthetic-v8.xpt",sas="synthetic.sas7bdat",
  weeks="synthetic-weeks.xpt",grouped="synthetic.sas7bdat",target="synthetic.sas7bdat",numeric="synthetic-numeric.xpt")
checks <- character()
check <- function(ok,label) {if(!isTRUE(ok))stop(paste("FAILED:",label));checks<<-c(checks,label);cat("PASS:",label,"\n")}
near <- function(x,y) isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-10))
for(name in names(cases)) {
  saved <- jsonlite::fromJSON(file.path(downloads,paste0(name,"-config.json")))
  cfg <- saved$config; m <- cfg$adtte_mapping
  data <- normalize_adtte(read_adtte(file.path(directory,cases[[name]])),m$paramcd,cfg$origin,cfg$cut,m$offset,
    unlist(m$dropout_codes),flag=m$flag,flag_value=m$flag_value,date_encoding=m$date_encoding,
    gap_mode=cfg$gap_mode,aval_unit=m$aval_unit,group_column=m$group_column)
  r <- run_forecast(data,cfg)
  if(name=="target") {
    actual <- read.csv(file.path(downloads,"target-milestones.csv"))
    check(all(vapply(c("reached","lower_day","median_day","upper_day"),function(id)near(r$summary[[id]],actual[[id]]),logical(1))),"target actual downloaded milestone dates and probabilities replay with unreached mass retained")
  } else {
    actual <- read.csv(file.path(downloads,paste0(name,"-curves.csv")))
    check(all(vapply(c("day","mean","median","lower","upper"),function(id)near(r$curves[[id]],actual[[id]]),logical(1))),paste(name,"actual downloaded curves replay from source file and frozen configuration"))
  }
  check(r$data_summary$n==saved$data_summary$n && r$data_summary$events==saved$data_summary$events,paste(name,"actual downloaded row/event counts replay"))
}
jsonlite::write_json(list(status="passed",scope="replay of actual browser-downloaded SAS/CSV configuration and curves with original synthetic files",
  passed=length(checks),checks=checks,R=R.version.string),"validation/v2_sas_replay_results.json",auto_unbox=TRUE,pretty=TRUE)
