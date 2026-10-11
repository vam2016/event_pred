# V2-302: independent historical reconstruction and scenario-error references.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e));attach(e)
checks<-character();measurements<-list();check<-function(ok,label){if(!isTRUE(ok))stop(paste("FAILED:",label));checks<<-c(checks,label);cat("PASS:",label,"\n")}
near<-function(x,y,tol=1e-10)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tol));error_of<-function(expr)tryCatch({force(expr);NULL},error=identity)
cfg<-list(task="count",cut=100,horizon=80,sims=100,target=50,future_n=999,enroll_rate=9,dropout_rate=0,lag=0,multiplier=1,seed=6401,prior_shape=.5,prior_rate=50,tail_rate=.002,uncertainty="plugin",clock="occurred",methods="exponential",cuts=c(20,50),ensemble=FALSE,input_mode="adtte",origin="2025-01-01",display_unit="days",engine_unit="days")
d<-data.frame(id=paste0("P",1:60),entry=0,time=c(seq(10,80,length.out=36),rep(70,6),rep(100,18)),status=c(rep("event",36),rep("dropout",6),rep("active",18)))
d<-validate_data(d,100);late<-data.frame(id="future",entry=70,time=20,status="event");truth<-validate_data(rbind(d[,c("id","entry","time","status")],late),100)
plan<-data.frame(cut=c(60,40),future_n=c(4,0),enroll_rate=c(.2,0));r<-e$backtest_forecast(truth,cfg,c(40,60),100,plan=plan)
check(identical(r$plan$cut,c(40,60))&&identical(r$plan$future_n,c(0,4)),"historical plans match cut keys rather than final remaining count or input row order")
check(nrow(r$snapshots[['40']])==60&&!('future'%in%r$snapshots[['60']]$id),"future entrants are excluded from both training snapshots")
s<-r$snapshots[['40']];ix<-d$obs_day>40
check(all(s$status[ix]=="active")&&all(s$event[ix]==0)&&all(s$time[ix]==40)&&all(s$obs_day<=40),"future event and exit observations become active at the historical cut")
check(sum(s$event)==sum(d$obs_day<=40&d$event==1),"historical observed events retain only events known by the cut")
check(near(r$forecasts[['40']]$models$exponential$params,sum(s$event)/sum(s$time)),"historical exponential refit matches independently counted snapshot likelihood")
check(near(r$configurations[['40']]$horizon,60)&&r$configurations[['60']]$horizon==40&&r$configurations[['40']]$future_n==0&&r$configurations[['60']]$future_n==4,"each frozen historical horizon stops at truth cut and uses its own plan")
# Changing only future outcomes cannot change an earlier fitted forecast.
alternative<-truth;at<-alternative$obs_day>60;alternative$status[at]<-"active";alternative$event[at]<-0;alternative$time[at]<-100-alternative$entry[at];alternative$obs_day[at]<-100
r2<-e$backtest_forecast(alternative,cfg,c(40,60),100,plan=plan)
check(identical(r$forecasts[['40']]$counts,r2$forecasts[['40']]$counts)&&identical(r$forecasts[['60']]$counts,r2$forecasts[['60']]$counts),"altering only post-cut truth leaves refits and all forecast paths unchanged")
actual<-sum(truth$event==1&truth$obs_day<=100);check(all(r$summary$observed_events==actual)&&all(r$summary$error==r$summary$predicted_events-actual),"event error is forecast median minus independently counted final-window truth")
check(all(r$summary$count_covered==(r$summary$count_lower<=actual&r$summary$count_upper>=actual))&&all(r$summary$count_interval_width==r$summary$count_upper-r$summary$count_lower),"event interval inclusion and width match independent endpoint comparisons")
check(all(r$diagnostics$simulations+r$diagnostics$failed==r$diagnostics$requested)&&nrow(r$simulation_failures)==0,"historical diagnostics conserve requested and effective attempts")
check(!any(grepl('target|brier|probability',names(e$core_validation_summary(r,'backtest')))),"event-count backtest omits dummy-target scoring fields")
cfg$task<-"target";cfg$target<-25;r<-e$backtest_forecast(truth,cfg,c(40,60),100,plan=plan)
hit<-sort(truth$obs_day[truth$event==1])[25]
check(all(r$summary$actual_target_day==hit)&&all(r$summary$brier==(r$summary$reached_probability-1)^2),"target truth and Brier use independently sorted event order and window attainment")
cfg$target<-10000;r<-e$backtest_forecast(truth,cfg,60,100,plan=plan[1,])
check(all(is.infinite(r$summary$actual_target_day))&&all(is.na(r$summary$target_error))&&all(r$summary$brier==r$summary$reached_probability^2),"unreached target stays infinite with undefined timing error and correct Brier")
check(!any(grepl('observed_events|predicted_events|count_',names(e$core_validation_summary(r,'backtest')))),"target backtest exposes probability and window-truncated date scoring separately")
for(p in list(NULL,data.frame(cut=40,future_n=0,enroll_rate=0),data.frame(cut=c(40,60),future_n=c(0,-1),enroll_rate=c(0,.2))))check(inherits(error_of(e$backtest_forecast(truth,cfg,c(40,60),100,plan=p)),"error"),"missing, incomplete or invalid historical plan is rejected")
check(inherits(error_of(e$backtest_forecast(truth,cfg,c(40,40),100,plan=plan)),"error")&&inherits(error_of(e$backtest_forecast(truth,cfg,100,100,plan=plan[1,])),"error"),"duplicate and future historical cuts are rejected")
other<-cfg;other$input_mode<-"parameters";check(inherits(error_of(e$backtest_forecast(truth,other,40,100,plan=plan[2,])),"error"),"parameter input cannot invent historical individual records")
# Scenario references use independent seeds and explicit arithmetic, never seed-based pairing.
cfg$cut<-100;cfg$future_n<-0;cfg$enroll_rate<-0;cfg$target<-45
z<-e$core_scenarios(d,cfg,c(.75,1.25));base<-z$forecasts[['1']]
check(identical(z$values,c(1,.75,1.25))&&length(unique(vapply(z$configurations,`[[`,integer(1),'seed')))==3&&z$pairing=='independent_seeds',"scenario baseline is retained and seeds differ without a pairing claim")
for(key in names(z$forecasts)) {
 rr<-z$forecasts[[key]];row<-z$summary[z$summary$multiplier==as.numeric(key),];v<-rr$counts[[1]][,121];bv<-base$counts[[1]][,121]
 check(near(row$mean_events_end,mean(v))&&near(row$count_mcse,sd(v)/sqrt(length(v))),paste(key,"mean and its MCSE match sample moments"))
 check(near(row$count_difference,mean(v)-mean(bv))&&near(row$count_difference_mcse,if(key=='1')0 else sqrt(var(v)/length(v)+var(bv)/length(bv))),paste(key,"count difference uses the independent variance sum"))
 bp<-mean(bv>=cfg$target);p<-mean(v>=cfg$target)
 check(near(row$reached,p)&&near(row$probability_difference,p-bp)&&near(row$probability_difference_mcse,if(key=='1')0 else sqrt(p*(1-p)/length(v)+bp*(1-bp)/length(bv))),paste(key,"probability difference uses effective counts and independent binomial MCSE"))
}
check(identical(base$counts,e$run_forecast(d,cfg)$counts),"scenario baseline replays the original frozen seed exactly")
z2<-e$core_scenarios(d,cfg,c(.75,1.25));check(identical(z$summary,z2$summary)&&identical(z$diagnostics,z2$diagnostics),"frozen scenario configuration reproduces summaries and attempt counts")
for(v in list(c(1,1),numeric(),0,6,seq(.5,1.5,length.out=6)))check(inherits(error_of(e$core_scenarios(d,cfg,v)),"error"),"duplicate, empty, out-of-range or oversized scenario list is rejected")
check(all(e$core_validation_seed(.Machine$integer.max,0:5)>=0)&&all(e$core_validation_seed(.Machine$integer.max,0:5)<=.Machine$integer.max)&&e$core_validation_seed(.Machine$integer.max,0)==.Machine$integer.max,"scenario seeds remain valid near the integer limit")
# Compare an exponential risk multiplier with an analytic conditional Binomial mean.
cfg$task<-'count';cfg$sims<-2000;z<-e$core_scenarios(d,cfg,c(.5,2));rate<-sum(d$event)/sum(d$time)
for(v in z$values){row<-z$summary[z$summary$multiplier==v,];expected<-36+18*(1-exp(-rate*v*80));check(abs(row$mean_events_end-expected)<=4*row$count_mcse+1e-6,paste(v,'risk multiplier agrees with an independent exponential/Binomial expectation'));measurements[[as.character(v)]]<-list(expected=expected,observed=row$mean_events_end,mcse=row$count_mcse,tolerance=4*row$count_mcse+1e-6)}
# Group plans and multipliers preserve the original group identities.
a<-d;a$group<-'A';b<-d;b$id<-paste0('B',b$id);b$group<-'B';dd<-rbind(a,b);cfg$sims<-50;cfg$analysis_mode<-'grouped';cfg$common_methods<-'exponential';cfg$groups<-list(g1=list(name='A',future_n=0,enroll_rate=0,dropout_rate=0,multiplier=1,lag=0,cuts=c(20,50),tail_rate=.002,fit_method=''),g2=list(name='B',future_n=0,enroll_rate=0,dropout_rate=0,multiplier=1,lag=0,cuts=c(20,50),tail_rate=.002,fit_method=''))
gp<-data.frame(group=c('B','A'),cut=60,future_n=c(4,2),enroll_rate=c(.2,.1));r<-e$backtest_forecast(dd,cfg,60,100,plan=gp)
check(r$configurations[['60']]$groups$g1$future_n==2&&r$configurations[['60']]$groups$g2$future_n==4&&r$summary$planned_future_n==6,"group historical plan matches named groups and conserves combined planned recruitment")
gp$group<-c('A','A');check(inherits(error_of(e$backtest_forecast(dd,cfg,60,100,plan=gp)),"error"),"duplicate group plans do not silently fill the missing group")
z<-e$core_scenarios(dd,cfg,.5);check(z$configurations[['0.5']]$groups$g1$multiplier==.5&&z$configurations[['0.5']]$groups$g2$multiplier==.5&&identical(z$forecasts[['0.5']]$counts[[1]],Reduce(`+`,z$forecasts[['0.5']]$group_counts[[1]])),"two-group scenario changes both risks and combines synchronized group counts")
# Bootstrap refit failures remain failed attempts, with per-analysis keys.
cfg$analysis_mode<-'pooled';cfg$groups<-NULL;cfg$uncertainty<-'bootstrap';original<-e$bootstrap_patient_model;calls<-0L
e$bootstrap_patient_model<-function(...){calls<<-calls+1L;if(calls%%50%in%1:5)stop('controlled refit failure');original(...)}
z<-e$core_scenarios(d,cfg,.75);check(all(z$diagnostics$requested==50)&&all(z$diagnostics$simulations==45)&&all(z$diagnostics$fit_failed==5)&&nrow(z$simulation_failures)==10&&all(z$simulation_failures$multiplier%in%c(1,.75)),"scenario bootstrap retains failures by factor and uses effective denominator without replacement")
calls<-0L;r<-e$backtest_forecast(d,cfg,60,100,plan=data.frame(cut=60,future_n=0,enroll_rate=0));check(r$summary$simulations==45&&r$summary$failed==5&&nrow(r$simulation_failures)==5&&all(r$simulation_failures$cut==60),"historical bootstrap failures carry cut and stage into frozen diagnostics")
e$bootstrap_patient_model<-original
# All five fixed-fit core models are usable in scenarios and snapshot refits.
cfg$uncertainty<-'plugin';cfg$methods<-e$core_models;z<-e$core_scenarios(d,cfg,1.25);r<-e$backtest_forecast(d,cfg,60,100,plan=data.frame(cut=60,future_n=0,enroll_rate=0))
check(length(unique(z$summary$method))==5&&length(unique(r$summary$method))==5,"five core models support fixed-fit scenarios and historical refits")
cfg$display_unit<-'weeks';r<-e$backtest_forecast(d,cfg,60,100,plan=data.frame(cut=60,future_n=0,enroll_rate=0));out<-e$core_validation_export(r,'backtest')
check(all(abs(out$cut_weeks-60/7)<1e-10)&&all(out$cut==60)&&all(out$days_per_unit==7),"CSV retains canonical continuous days and frozen selected-unit values")
check(!is.null(e$core_validation_table(r,'backtest')),"localized table handles frozen time units and unreachable dates")
plain<-d[,c('id','entry','time','status')]
normalized<-e$backtest_forecast(plain,cfg,60,100,plan=data.frame(cut=60,future_n=0,enroll_rate=0))
check(identical(normalized$summary,r$summary),"historical API normalizes raw records before truth scoring and snapshot reconstruction")
pp<-r;pp$plan$enroll_rate<-.2;px<-e$core_validation_plan_export(pp)
check(all(px$enroll_rate_per_day==.2)&&all(abs(px$enroll_rate_per_weeks-1.4)<1e-10)&&!('enroll_rate'%in%names(px)),"historical plan CSV explicitly distinguishes daily and frozen weekly enrollment rates")
# Boundary seeds and scenario-level failures preserve completed alternatives.
boundary<-cfg;boundary$seed<-.Machine$integer.max;boundary$methods<-'exponential'
br<-e$backtest_forecast(d,boundary,c(40,60),100,plan=data.frame(cut=c(40,60),future_n=0,enroll_rate=0))
check(all(vapply(br$configurations,`[[`,integer(1),'seed')>=0),"historical multi-cut seeds wrap safely at integer limit")
original_run<-e$run_forecast;e$run_forecast<-function(data,cfg,...){if(cfg$multiplier<1)stop('controlled scenario failure');original_run(data,cfg,...)}
z<-e$core_scenarios(d,cfg,c(.75,1.25))
check(nrow(z$failures)==1&&z$failures$multiplier==.75&&z$failures$method=='all'&&length(z$forecasts)==2,"whole alternative failure remains explicit and does not erase baseline or later success")
check(!any(z$summary$multiplier==.75)&&length(z$configurations)==3,"failed scenario configuration is retained without fabricating zero probability or effective paths")
e$run_forecast<-function(...)stop('controlled baseline failure')
check(inherits(error_of(e$core_scenarios(d,cfg,.75)),"error"),"baseline failure stops scenario batch explicitly")
e$run_forecast<-original_run
jsonlite::write_json(list(status='passed',version=e$core_version,R=R.version.string,passed=length(checks),checks=checks,measurements=measurements,tolerances='deterministic 1e-10; analytic exponential mean four empirical MCSE plus 1e-6; no historical coverage-rate claim'), 'validation/v2_validation_results.json',pretty=TRUE,auto_unbox=TRUE,digits=NA)
cat('VALIDATION_CHECKS_PASSED',length(checks),'\n')
