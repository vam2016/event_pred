# V2-201 patient case resampling, independent references, and failure accounting.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e));attach(e)
checks<-character();measurements<-list()
check<-function(ok,label){if(!isTRUE(ok))stop(paste("FAILED:",label));checks<<-c(checks,label);cat("PASS:",label,"\n")}
near<-function(x,y,tol=1e-9)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tol))
error_of<-function(expr)tryCatch({force(expr);NULL},error=identity)
cfg_for<-function(method="exponential",sims=50)list(task="count",cut=100,horizon=80,sims=sims,target=10000,future_n=0,enroll_rate=0,dropout_rate=0,lag=0,multiplier=1,seed=6412,prior_shape=.5,prior_rate=50,tail_rate=.002,uncertainty="bootstrap",clock="occurred",methods=method,cuts=c(20,50),ensemble=FALSE,input_mode="adtte",origin="2025-01-01",display_unit="days",engine_unit="days")
# Deterministic right-censored patient records: event, permanent exit, current risk set.
d<-data.frame(id=paste0("P",1:60),entry=0,time=c(seq(10,80,length.out=36),rep(70,6),rep(100,18)),status=c(rep("event",36),rep("dropout",6),rep("active",18)),marker=1:60)
d<-validate_data(d,100)
for(id in e$core_bootstrap_models)check(is.null(error_of(e$validate_core_uncertainty(cfg_for(id)))),paste(id,"core permits patient bootstrap on ADTTE"))
check(inherits(error_of(e$validate_core_uncertainty(cfg_for("lognormal"))),"error")&&inherits(error_of(e$validate_core_uncertainty(cfg_for("loglogistic"))),"error"),"core keeps log-time bootstrap outside the current validated scope")
c<-cfg_for();c$input_mode<-"parameters";check(inherits(error_of(e$validate_core_uncertainty(c)),"error"),"given-parameter inputs do not invent an empirical bootstrap population")
# Verify the resampled full rows and their likelihood with an external sample-index reference.
original_fit<-e$fit_model; captured<-NULL
e$fit_model<-function(data,method,cuts,tail_rate){captured<<-data;original_fit(data,method,cuts,tail_rate)}
set.seed(238);ix<-sample.int(nrow(d),nrow(d),replace=TRUE)
set.seed(238);m<-e$bootstrap_patient_model(d,"exponential",c(20,50),.002)
check(identical(captured,d[ix,,drop=FALSE])&&anyDuplicated(captured$id)>0,"resampling retains complete patient time, event, exit and metadata records with replacement")
check(near(m$params,sum(d$event[ix])/sum(d$time[ix])),"bootstrap exponential refit agrees with independent events/exposure likelihood")
set.seed(238);pm<-e$bootstrap_patient_model(d,"pwe",c(20,50),.002)
exposure<-vapply(1:3,function(j){bounds<-c(0,20,50,Inf);sum(pmax(0,pmin(d$time[ix],bounds[j+1])-bounds[j]))},numeric(1))
events<-vapply(1:3,function(j)sum(d$event[ix]==1&d$time[ix]>c(0,20,50)[j]&d$time[ix]<=c(20,50,Inf)[j]),numeric(1))
check(near(pm$params,events/exposure),"bootstrap PWE refit agrees with independently counted segment events and exposure")
set.seed(238);wm<-e$bootstrap_patient_model(d,"weibull",c(20,50),.002);wd<-d[ix,]
nll<-function(p){k<-exp(p[1]);eta<-exp(p[2]);-sum(wd$event*(log(k)-k*log(eta)+(k-1)*log(wd$time))-(wd$time/eta)^k)}
opt<-optim(log(c(1.3,100)),nll,method="BFGS",control=list(reltol=1e-12))
check(opt$convergence==0&&near(c(1/wm$params[2],exp(wm$params[1])),exp(opt$par),2e-5),"bootstrap Weibull refit agrees with independent censored likelihood optimization")
e$fit_model<-original_fit
# Exact random-sequence reference for exponential: original risk set remains fixed.
cfg<-cfg_for();set.seed(cfg$seed);mat<-matrix(NA_real_,cfg$sims,121);grid<-seq(100,180,length.out=121)
for(b in 1:cfg$sims){ix<-sample.int(nrow(d),nrow(d),replace=TRUE);rate<-sum(d$event[ix])/sum(d$time[ix]);future<-100-log(runif(sum(d$status=="active")))/rate;mat[b,]<-findInterval(grid,sort(c(d$obs_day[d$event==1],future)))}
r<-e$run_forecast(d,cfg)
check(identical(r$counts$exponential,mat),"complete bootstrap forecast matches independently refitted exponential paths on original current patients")
check(all(r$counts$exponential[,1]==36)&&all(r$counts$exponential<=54),"bootstrap keeps known events and permanent exits fixed, rather than predicting resampled duplicates")
check(r$diagnostics$requested==50&&r$diagnostics$simulations==50&&r$diagnostics$failed==0&&nrow(r$simulation_failures)==0,"zero failures produce explicit requested/effective counts and an empty typed failure table")
for(id in c("exponential","weibull","pwe")){
 cfg<-cfg_for(id);r1<-e$run_forecast(d,cfg);r2<-e$run_forecast(d,cfg)
 check(identical(r1$counts,r2$counts)&&identical(r1$summary,r2$summary)&&identical(r1$simulation_failures,r2$simulation_failures),paste(id,"seed reproduces bootstrap counts, target summary and failures"))
 cfg$seed<-cfg$seed+1;r3<-e$run_forecast(d,cfg);check(!identical(r1$counts,r3$counts),paste(id,"a different seed changes bootstrap trajectories"))
}
# Exhaust all 5^5 bootstrap samples to obtain the conditional predictive expectation.
small<-validate_data(data.frame(id=LETTERS[1:5],entry=0,time=c(10,20,30,40,100),status=c(rep("event",4),"active")),100)
indices<-as.matrix(expand.grid(rep(list(1:5),5)));event_n<-rowSums(matrix(small$event[indices],nrow(indices)));time_sum<-rowSums(matrix(small$time[indices],nrow(indices)));valid<-event_n>=2
prob<-mean(1-exp(-80*event_n[valid]/time_sum[valid]));failure_prob<-mean(!valid)
cfg<-cfg_for(sims=2000);cfg$task<-"target";cfg$target<-5;cfg$seed<-24019;r<-e$run_forecast(small,cfg)
end<-r$counts[[1]][,121];se<-sd(end)/sqrt(length(end));tolerance<-4*se+1e-6
measurements$enumerated_bootstrap<-list(samples=nrow(indices),valid_samples=sum(valid),failure_probability=failure_prob,expected_mean=4+prob,observed_mean=mean(end),mcse=se,tolerance=tolerance,requested=2000,effective=nrow(r$counts[[1]]),failed=r$diagnostics$failed)
check(abs(mean(end)-(4+prob))<=tolerance,"predictive bootstrap mean agrees with exhaustive sample enumeration within four MCSE")
check(abs(r$diagnostics$failed-2000*failure_prob)<=4*sqrt(2000*failure_prob*(1-failure_prob)),"actual low-event fit failures agree with independently enumerated resample probability")
check(r$summary$reached==mean(end>=5)&&near(r$summary$reached_mcse,sqrt(r$summary$reached*(1-r$summary$reached)/length(end))),"target probability and MCSE use effective paths including valid unreached paths")
check(r$summary$requested==2000&&r$summary$failed==r$diagnostics$failed&&all(r$simulation_failures$stage=="fit"),"target summary distinguishes failed fits from valid misses and requested denominator")
# Controlled errors test failure policy boundaries and stage attribution.
original_bootstrap<-e$bootstrap_patient_model;original_trial<-e$simulate_trial
calls<-0L;e$bootstrap_patient_model<-function(...){calls<<-calls+1L;if(calls<=5)stop("controlled refit failure");original_bootstrap(...)}
cfg<-cfg_for();r<-e$run_forecast(d,cfg)
check(r$diagnostics$simulations==45&&r$diagnostics$fit_failed==5&&r$diagnostics$simulation_failed==0&&nrow(r$simulation_failures)==5,"exactly ten percent refit failures are retained and excluded without replacement draws")
check(identical(r$simulation_failures$replicate,1:5)&&all(r$simulation_failures$reason=="controlled refit failure")&&all(r$counts[[1]][,1]==36),"failure records preserve attempt numbers, reasons and original cohort baseline")
calls<-0L;e$bootstrap_patient_model<-function(...){calls<<-calls+1L;if(calls<=6)stop("controlled excess failure");original_bootstrap(...)}
err<-error_of(e$run_forecast(d,cfg));check(inherits(err,"error")&&grepl("44 / 请求 50",conditionMessage(err))&&grepl("controlled excess failure",conditionMessage(err)),"more than ten percent failure stops with valid/requested counts and cause")
e$bootstrap_patient_model<-original_bootstrap;calls<-0L;e$simulate_trial<-function(...){calls<<-calls+1L;if(calls<=2)stop("controlled future path failure");original_trial(...)}
r<-e$run_forecast(d,cfg);check(r$diagnostics$simulation_failed==2&&r$diagnostics$fit_failed==0&&all(r$simulation_failures$stage=="simulation"),"future simulation failures are distinct from bootstrap refit failures")
e$simulate_trial<-original_trial
# Within-group case resampling; a failed group invalidates that joint trial in all groups.
g1<-d;g1$group<-"A";g2<-d;g2$id<-paste0("B",g2$id);g2$group<-"B";data<-rbind(g1,g2)
cfg<-cfg_for();cfg$analysis_mode<-"grouped";cfg$common_methods<-"exponential";cfg$groups<-list(g1=list(name="A",future_n=0,enroll_rate=0,dropout_rate=0,multiplier=1,lag=0,cuts=c(20,50),tail_rate=.002,fit_method=""),g2=list(name="B",future_n=0,enroll_rate=0,dropout_rate=0,multiplier=1,lag=0,cuts=c(20,50),tail_rate=.002,fit_method=""))
seen<-character();e$bootstrap_patient_model<-function(data,...){check_group<-unique(data$group);if(length(check_group)!=1)stop("cross-group resample");seen<<-c(seen,check_group);original_bootstrap(data,...)}
r<-e$run_forecast(data,cfg)
check(length(seen)==100&&sum(seen=="A")==50&&sum(seen=="B")==50,"each group is independently resampled at its original patient count without mixing patients")
check(identical(r$counts[[1]],Reduce(`+`,r$group_counts[[1]]))&&all(r$counts[[1]][,1]==72),"grouped bootstrap jointly sums complete paths with both original known-event baselines")
calls<-0L;e$bootstrap_patient_model<-function(data,...){if(unique(data$group)=="B"){calls<<-calls+1L;if(calls<=5)stop("controlled group B refit failure")};original_bootstrap(data,...)}
r<-e$run_forecast(data,cfg)
check(nrow(r$counts[[1]])==45&&all(vapply(r$group_counts[[1]],nrow,integer(1))==45)&&identical(r$counts[[1]],Reduce(`+`,r$group_counts[[1]])),"one-group failures exclude the entire joint replicate from every group and total")
check(all(r$simulation_failures$group=="B")&&all(r$simulation_failures$stage=="fit")&&r$diagnostics$fit_failed==5,"group-specific failure reason and phase survive joint exclusion")
cfg$task<-"target";cfg$target<-10000;calls<-0L;r<-e$run_forecast(data,cfg)
check(r$summary$reached==0&&r$summary$simulations==45&&all(is.infinite(r$milestones[[1]]))&&sum(core_target_mass(r)$frequency)==45,"grouped target bins retain all valid unreachable paths without counting failed fits as misses")
e$bootstrap_patient_model<-original_bootstrap
check(grepl("近似包含",forecast_interval_note(cfg))&&grepl("条件于有效轮次",forecast_interval_note(cfg)),"bootstrap result explanation states parameter uncertainty and conditional failure scope")
cfg$uncertainty<-"plugin";check(grepl("未包含参数估计不确定性",forecast_interval_note(cfg)),"fixed fitted-parameter explanation retains its original scope")
jsonlite::write_json(list(status="passed",version=e$core_version,R=R.version.string,passed=length(checks),checks=checks,measurements=measurements,tolerances="deterministic 1e-9; independent Weibull optimization 2e-5; enumerated bootstrap predictive mean and fit-failure count four MCSE; no coverage-rate claim"),"validation/v2_bootstrap_results.json",pretty=TRUE,auto_unbox=TRUE,digits=NA)
cat("BOOTSTRAP_CHECKS_PASSED",length(checks),"\n")
