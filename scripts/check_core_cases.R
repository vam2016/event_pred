# V2-401: frozen display contracts and same-source downloadable case replay.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e));attach(e)
checks<-character();check<-function(ok,label){if(!isTRUE(ok))stop(paste("FAILED:",label));checks<<-c(checks,label);cat("PASS:",label,"\n")}
near<-function(x,y)isTRUE(all.equal(x,y,tolerance=1e-10,check.attributes=FALSE))
runs<-list()
for(id in names(core_case_catalog())) {
  x<-core_case_spec(id);r<-core_run_case(x);runs[[id]]<-r
  file<-tempfile();jsonlite::write_json(x,file,auto_unbox=TRUE,pretty=TRUE,digits=NA,null="null",dataframe="rows")
  y<-jsonlite::read_json(file,simplifyVector=TRUE);rr<-core_run_case(y);unlink(file)
  check(near(core_case_tables(r),core_case_tables(rr)),paste(id,"JSON round trip reproduces every exported result table"))
  if(!is.null(r$forecast))check(identical(r$forecast$counts,rr$forecast$counts),paste(id,"JSON round trip reproduces individual Monte Carlo paths"))
  else check(near(r$simulation$truth,rr$simulation$truth),"event example JSON round trip reproduces continuous truth within 1e-10")
  if(isTRUE(x$config$input_mode %in% "adtte")) {
    d<-normalize_adtte(core_case_adtte(x),"OS",x$config$origin,x$config$cut,offset=1,dropout_codes=2)
    check(identical(d$id,x$data$id)&&identical(d$time,x$data$time)&&identical(d$status,x$data$status),paste(id,"ADTTE inclusive-day mapping preserves the exact case records"))
    rp<-run_forecast(d,x$config)
    check(identical(r$forecast$counts,rp$counts),paste(id,"downloadable ADTTE predicts exactly the same patient paths"))
  }
}
check(identical(as.integer(table(core_case_spec("bootstrap")$data$status)),c(18L,6L,36L)),"case has independently counted 18 active, 6 exits and 36 events")
check(all(runs$bootstrap$forecast$diagnostics$requested==100)&&all(with(runs$bootstrap$forecast$diagnostics,requested==simulations+failed)),"bootstrap case conserves attempts across all original three models")
x<-runs$log_time$forecast
expected<-10*(1-plnorm(100,log(50),.8,lower.tail=FALSE)/plnorm(20,log(50),.8,lower.tail=FALSE))
v<-x$counts[[1]][,121];check(abs(mean(v)-expected)<4*sd(v)/sqrt(length(v)),"log-time case agrees with independent conditional Log-normal expectation within four MCSE")
b<-runs$validation$backtest;s<-runs$validation$scenario
check(all(b$summary$observed_events==36)&&all(is.infinite(b$summary$actual_target_day)),"historical case truth has 36 events and never reaches target45")
set.seed(91);before<-.Random.seed;saved<-serialize(list(b,s),NULL)
pb<-plotly::plotly_build(core_backtest_plot(b));ps<-plotly::plotly_build(core_scenario_plot(s))
check(identical(before,.Random.seed)&&identical(saved,serialize(list(b,s),NULL)),"auxiliary charts neither consume RNG nor mutate frozen data")
check(length(pb$x$data)==8&&sum(vapply(pb$x$data,function(z)identical(z$fill,"tonexty"),logical(1)))==2,"two historical cuts display two saved predictive bands plus prediction and actual lines")
for(k in 1:2) {
  d<-b$curves[b$curves$backtest_cut==c(40,60)[k],]
  check(near(pb$x$data[[4*k-1]]$x,d$day)&&near(pb$x$data[[4*k-1]]$y,d$median)&&near(pb$x$data[[4*k]]$y,d$actual),paste("backtest cut",k,"plotted coordinates equal the saved curve table"))
}
check(near(ps$x$data[[1]]$y,core_scenario_plot_data(s)$reached)&&is.null(ps$x$data[[1]]$error_y$array)&&!isTRUE(ps$x$data[[1]]$error_y$visible),"target scenario plots saved probabilities without turning MCSE into a confidence interval")
check(identical(ps$x$layout$yaxis$range,c(0,1))&&all(grepl("请求 / 有效 / 失败",ps$x$data[[1]]$text)),"probability chart uses 0–1 axis and exposes denominators in hover text")
s$config$task<-"count";pc<-plotly::plotly_build(core_scenario_plot(s));d<-core_scenario_plot_data(s)
check(near(pc$x$data[[1]]$y,d$median_events_end)&&near(pc$x$data[[1]]$error_y$array,d$upper_events_end-d$median_events_end)&&near(pc$x$data[[1]]$error_y$arrayminus,d$median_events_end-d$lower_events_end),"count scenario points and asymmetric whiskers equal saved predictive quantiles")
check(near(d$multiplier,c(.75,1,1.25)),"scenario display sorts risk factors while retaining automatic baseline")
partial<-s;partial$summary<-partial$summary[partial$summary$multiplier!=.75,];pp<-plotly::plotly_build(core_scenario_plot(partial))
check(length(pp$x$data[[1]]$x)==2&&!any(pp$x$data[[1]]$x==.75),"missing alternative does not fabricate a zero-valued scenario")
empty<-plotly::plotly_build(core_scenario_plot(NULL));check(length(empty$x$layout$annotations)==1,"empty plot instructs user to run analysis")
check(identical(readLines("docs/CORE_CASES.md",warn=FALSE),core_case_markdown()),"versioned case guide matches shared source definitions exactly")
manual<-paste(readLines("docs/CORE_HANDBOOK.md",warn=FALSE),collapse="\n")
check(all(vapply(core_case_catalog(),function(x)grepl(x,manual,fixed=TRUE),logical(1)))&&grepl('core-section-9',manual,fixed=TRUE),"online and offline manual includes four current examples and ninth anchor")
bad<-core_case_spec("log_time");bad$version<-"1.0";check(inherits(tryCatch(core_run_case(bad),error=identity),"error"),"replay rejects mismatched case version")
check(nrow(runs$event_simulation$simulation$cuts)==5&&simulation_target_overview(runs$event_simulation$simulation)$trials==5,"event example retains all requested trials in the denominator")
jsonlite::write_json(list(status="passed",passed=length(checks),checks=checks,R=R.version.string,scope="synthetic teaching cases; frozen plot coordinates; no coverage or clinical validity claim"),"validation/v2_core_cases_results.json",pretty=TRUE,auto_unbox=TRUE,digits=NA)
cat("CORE_CASES_CHECKS_PASSED",length(checks),"\n")
