# V2-101: format integration and input contract checks, not new model validation.
source("R/models.R"); source("R/forecast.R"); source("R/inputs.R"); source("R/bayes.R"); source("R/groups.R")
source("scripts/create_sas_fixtures.R")
checks <- list()
check <- function(ok,label) {
  if (!isTRUE(ok)) stop(paste("FAILED:",label))
  checks[[length(checks)+1L]] <<- list(label=label,status="passed")
  cat("PASS:",label,"\n")
}
fails <- function(expr,pattern) {
  err <- tryCatch({force(expr);NULL},error=identity)
  inherits(err,"error") && grepl(pattern,conditionMessage(err),fixed=TRUE)
}
near <- function(x,y) isTRUE(all.equal(x,y,tolerance=1e-10,check.attributes=FALSE))
directory <- Sys.getenv("SAS_CHECK_FIXTURES",unset=tempfile("survcast-sas-"))
raw <- create_sas_fixtures(directory)
norm <- function(x,encoding="iso",unit="days") normalize_adtte(x,"OS","2025-01-01",100,1,2,
  flag="ANL01FL",flag_value="Y",date_encoding=encoding,aval_unit=unit,group_column="TRTP")
expected <- data.frame(id=sprintf("SYN%03d",0:59),entry=0,
  time=ifelse(0:59<36,10+(0:59)*2,ifelse(0:59<42,90,100)),
  obs_day=ifelse(0:59<36,10+(0:59)*2,ifelse(0:59<42,90,100)),
  status=ifelse(0:59<36,"event",ifelse(0:59<42,"dropout","active")),
  day_offset=0,group=ifelse((0:59)%%2,"B","A"),stringsAsFactors=FALSE)
base <- norm(raw)
check(near(base[names(expected)],expected),"normalization matches independently declared patient dates, statuses and stored groups")
cfg <- list(task="count",input_mode="adtte",analysis_mode="pooled",cut=100,horizon=80,sims=50,
  target=45,seed=8173,origin="2025-01-01",methods=c("exponential","weibull","pwe"),cuts=c(30,60),
  future_n=0,enroll_rate=0,dropout_rate=0,lag=0,multiplier=1,prior_shape=.5,prior_rate=50,tail_rate=.002,
  uncertainty="plugin",clock="occurred",ensemble=FALSE)
reference <- run_forecast(base,cfg)
for (filename in c("synthetic.csv","synthetic-v5.xpt","synthetic-v8.xpt","synthetic.sas7bdat")) {
  imported <- read_adtte(file.path(directory,filename),toupper(filename))
  check(nrow(imported)==180 && identical(attr(imported,"adtte_source")$format,tolower(tools::file_ext(filename))),paste(filename,"reads uppercase extension and records decoder"))
  data <- norm(imported)
  check(near(data,base),paste(filename,"retains selected endpoint, flag, date, censor code and treatment"))
  result <- run_forecast(data,cfg)
  check(near(result$curves,reference$curves) && identical(result$counts,reference$counts),paste(filename,"reproduces all three model predictions and trajectories"))
  gc <- cfg; gc$analysis_mode <- "grouped"; gc$methods <- "exponential"
  gc$groups <- list(A=list(name="A",future_n=0,enroll_rate=0,dropout_rate=0),B=list(name="B",future_n=0,enroll_rate=0,dropout_rate=0))
  gr <- run_forecast(data,gc); br <- run_forecast(base,gc)
  check(near(gr$curves,br$curves) && identical(gr$counts,br$counts),paste(filename,"reproduces known-group prediction"))
}
numeric_dates <- read_adtte(file.path(directory,"synthetic-numeric.xpt"))
check(fails(norm(numeric_dates),"数值 SAS 日期请选择 SAS 编码"),"unformatted numeric SAS dates require explicit date encoding")
check(near(norm(numeric_dates,"sas"),base),"explicit numeric SAS dates agree with typed dates")
check(near(norm(read_adtte(file.path(directory,"synthetic-weeks.xpt")),unit="weeks"),base),"file week units agree with day data independently of display units")
check(fails(norm(read_adtte(file.path(directory,"synthetic-weeks.xpt"))),"AVALU"),"incorrect file units are rejected")
labelled <- raw; labelled$CNSR <- haven::labelled(labelled$CNSR,c(EVENT=0,ACTIVE=1,EXIT=2))
labelled$TRTP <- haven::labelled(ifelse(raw$TRTP=="A",1,2),c(Drug=1,Control=2))
ld <- norm(labelled)
check(identical(ld$status,base$status) && identical(ld$group,ifelse(base$group=="A","1","2")),"SAS value labels never recode censor values or substitute group labels")
tagged <- raw; tagged$CNSR <- haven::tagged_na("a")
check(fails(norm(tagged),"必填变量不能缺失"),"SAS tagged missing censor values are rejected")
check(fails(normalize_adtte(raw,"OS","2025-01-01",100),"每个 USUBJID"),"unfiltered multiple analysis rows are rejected")
bad <- raw; bad$TRTP[1] <- NA
check(fails(norm(bad),"组别不能缺失"),"missing selected treatment values are rejected")
bad <- raw; bad$AVAL[1] <- bad$AVAL[1]+1
check(fails(norm(bad),"AVAL 与"),"incorrect date offset cannot silently alter elapsed follow-up")
check(fails(adtte_date(as.Date(NA)),"有效整日"),"typed missing dates are rejected")
check(fails(adtte_date(structure(1.5,class="Date")),"有效整日"),"fractional typed dates are rejected")
check(fails(adtte_date(as.POSIXct("2025-01-01 12:00:00",tz="UTC")),"不自动截去"),"datetime fractions are not silently truncated")
check(identical(adtte_date(as.POSIXct("2025-01-01",tz="UTC")),as.Date("2025-01-01")),"UTC midnight dates preserve calendar day")
check(fails(adtte_date(24000.5,"sas"),"时间部分"),"numeric SAS datetime fractions are rejected")
check(fails(adtte_date("2025-02-30"),"无效日期"),"invalid ISO calendar dates are rejected")
check(fails(read_adtte("unused","file.xlsx"),"支持 CSV"),"unsupported format reports supported extensions")
writeLines("USUBJID,USUBJID\nA,B",file.path(directory,"duplicate.csv"))
check(fails(read_adtte(file.path(directory,"duplicate.csv")),"变量名必须唯一"),"duplicate CSV column names are rejected rather than repaired")
writeLines("invalid",file.path(directory,"corrupt.xpt"))
check(fails(read_adtte(file.path(directory,"corrupt.xpt")),"读取 ADTTE（XPT）失败"),"damaged XPT reports format-specific import failure")
missing_env <- new.env(parent=globalenv());sys.source("R/inputs.R",envir=missing_env)
missing_env$requireNamespace <- function(package,...) if(package=="haven") FALSE else base::requireNamespace(package,...)
check(fails(missing_env$read_adtte("unused","file.xpt"),"--with-sas"),"missing optional haven explains installation without blocking CSV")
check(nrow(missing_env$read_adtte(file.path(directory,"synthetic.csv")))==180,"CSV remains usable without optional haven")
output <- Sys.getenv("SAS_CHECK_OUTPUT",unset="validation/v2_sas_input_results.json")
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(list(status="passed",scope="synthetic ADTTE input and format equivalence; no independent SAS producer or new statistical method validation",
  checks=checks,passed=length(checks),R=R.version.string,haven=as.character(utils::packageVersion("haven"))),output,auto_unbox=TRUE,pretty=TRUE)
cat("SAS_INPUT_CHECKS_PASSED",length(checks),"\n")
