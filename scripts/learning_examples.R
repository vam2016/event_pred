# Run from the SurvCast source root. Educational examples, not clinical inputs.
source("R/units.R");source("R/models.R");source("R/forecast.R")
source("R/inputs.R");source("R/bayes.R");source("R/groups.R");source("R/simulation.R")
f<-time_factor("months")
base <- list(task="count",input_mode="parameters",analysis_mode="pooled",origin="2025-01-01",display_unit="months",engine_unit="days",
 cut=12*f,horizon=12*f,sims=1000,seed=20261006,target=1,methods="exponential",clock="occurred",ensemble=FALSE,uncertainty="plugin",process_uncertainty="fixed",
 future_n=0,enroll_rate=0,dropout_rate=0,lag=0,multiplier=1,cuts=numeric(),prior_shape=.5,prior_rate=50,tail_rate=.002)
cohort<-parameter_cohort(base$cut,100,40,6*f,"fixed",base$seed)
exp_model<-parameter_model("exponential",list(median=12*f))
r<-run_forecast(cohort,base,models_override=list(exponential=exp_model))
last<-r$counts$exponential[,121]
bench<-list(analytic_mean=90,analytic_sd=5,analytic_quantiles=40+qbinom(c(.025,.5,.975),100,.5),simulated_mean=mean(last),simulated_sd=sd(last),simulated_quantiles=quantile_with_inf(last))
closed<-function(lambda,mu,u)lambda/(lambda+mu)*(-expm1(-(lambda+mu)*u))
lambda<-log(2)/12;mu<-.005;h<-12;r_enroll<-10
cap_density<-function(e)r_enroll*ppois(59,r_enroll*e)
expected_future<-integrate(function(e)cap_density(e)*closed(lambda,mu,h-e),0,h,rel.tol=1e-10)$value
process<-list(lambda_month=lambda,dropout_mu_month=mu,active_prob_12=closed(lambda,mu,12),active_expected_100=100*closed(lambda,mu,12),future_expected_capped_60=expected_future,uncapped_future_expected=r_enroll*lambda/(lambda+mu)*(h-(-expm1(-(lambda+mu)*h))/(lambda+mu)))
progress_cfg<-base;progress_cfg$future_n<-60;progress_cfg$enroll_rate<-10/f;progress_cfg$dropout_rate<-.005/f
progress_res<-run_forecast(cohort,progress_cfg,models_override=list(exponential=exp_model))
process$simulated_total_mean<-mean(progress_res$counts$exponential[,121]);process$analytic_total_mean<-40+process$active_expected_100+expected_future
cfg_target<-base;cfg_target$task<-"target";cfg_target$target<-150
unreach<-run_forecast(cohort,cfg_target,models_override=list(exponential=exp_model))
wb<-parameter_model("weibull",list(median=12,shape=2));u<-.4;age<-8
wbtime<-sample_conditional(wb,age,u=u)
weibull<-list(median=12,shape=2,eta=12/sqrt(log(2)),age=age,u=u,cumhaz_age=model_cumhaz(wb,age),target_cumhaz=model_cumhaz(wb,age)-log(u),total_time=unname(wbtime),remaining_time=unname(wbtime-age),remaining_event_prob_6=1-model_survival(wb,age+6)/model_survival(wb,age))
pwe<-parameter_model("pwe",list(rates=c(.03,.06,.09,.06)),c(3,6,12))
pw<-list(survival_at_cuts=model_survival(pwe,c(3,6,12)),median=unname(inverse_cumhaz(pwe,log(2))),cumhaz_age8=model_cumhaz(pwe,8),total_time_u04=unname(sample_conditional(pwe,8,u=.4)))
raw<-data.frame(USUBJID=paste0("P00",1:4),PARAMCD="OS",STARTDT=c("2025-01-01","2025-01-10","2025-02-01","2025-02-10"),ADT=c("2025-02-15","2025-03-15","2025-04-01","2025-03-05"),AVAL=c(46,65,60,24),AVALU="DAYS",CNSR=c(0,0,1,2),TRTP=c("A","B","A","B"),ANL01FL="Y")
d<-normalize_adtte(raw,"OS","2025-01-01",90,1,2,aval_unit="days")
fit<-fit_model(d,"exponential")
data_example<-list(continuous_times=d$time,exposure=sum(d$time),events=sum(d$event),lambda_per_day=unname(fit$params[1]),median_days=unname(log(2)/fit$params[1]))
simcfg<-list(n=60,reps=20,seed=20261006,origin="2025-01-01",paramcd="OS",display_unit="months",engine_unit="days",enroll_mode="constant",enroll_rate=10/f,enroll_cuts=numeric(),enroll_rates=numeric(),cut_mode="fixed",cuts=c(12,24,36)*f,max_day=36*f,target=1,fixed_times=c(6,12,60)*f,
 groups=list(list(name="A",weight=1,dropout_rate=.005/f,model=parameter_model("weibull",list(median=12*f,shape=1.2))),list(name="B",weight=1,dropout_rate=.005/f,model=parameter_model("weibull",list(median=18*f,shape=1.2)))))
sim<-run_survival_simulation(simcfg)
assets<-Sys.getenv("LEARNING_ASSETS","docs/learning-assets");dir.create(assets,showWarnings=FALSE)
write.csv(raw,file.path(assets,"adtte-learning-example.csv"),row.names=FALSE)
write.csv(unit_export(progress_res$curves,"months","day"),file.path(assets,"prediction-example-curves.csv"),row.names=FALSE)
write.csv(unit_export(simulation_result_overview(sim),"months",c("conditional_lower_day","conditional_median_day","conditional_upper_day")),file.path(assets,"simulation-example-overview.csv"),row.names=FALSE)
jsonlite::write_json(list(benchmark=bench,process=process,weibull=weibull,pwe=pw,adtte=data_example,unreachable=list(target=150,capacity=140,reached=unreach$summary$reached,median_day="Inf"),R=R.version.string,seed=base$seed),file.path(assets,"example-results.json"),pretty=TRUE,auto_unbox=TRUE,digits=12)
# Check the worked-example identities; this is a teaching-artifact check.
stopifnot(abs(log(2)/(12*f) - log(2)/12/f)<1e-12,
 abs(model_survival(exp_model,12*f)-.5)<1e-12,
 abs(weibull$total_time-(12/sqrt(log(2)))*sqrt((8/(12/sqrt(log(2))))^2-log(.4)))<1e-10,
 abs(data_example$lambda_per_day-2/191)<1e-12,
 unreach$summary$reached==0,
 all(r$counts$exponential>=40),all(r$counts$exponential<=140),
 all(sim$summary$n==sim$summary$events+sim$summary$dropouts+sim$summary$administrative))
# Figure data are exported separately; a plotting runtime is not needed here.
write.csv(sim$curves[sim$curves$SIMID==1 & sim$curves$CUTID==2 & sim$curves$scope=="group",],file.path(assets,"simulation-example-km.csv"),row.names=FALSE)
z<-table(last)
write.csv(data.frame(count=as.numeric(names(z)),probability=as.numeric(z)/sum(z)),file.path(assets,"prediction-example-mass.csv"),row.names=FALSE)
cat(jsonlite::toJSON(list(benchmark=bench,process=process,weibull=weibull,pwe=pw,adtte=data_example),pretty=TRUE,auto_unbox=TRUE,digits=8),"\n")
