# V2-102: independent log-time distribution/likelihood and forecast references.
e <- new.env(parent=globalenv()); invisible(sys.source("app_core.R",envir=e)); attach(e)
checks <- character(); measurements <- list()
check <- function(ok,label) {if(!isTRUE(ok))stop(paste("FAILED:",label)); checks<<-c(checks,label);cat("PASS:",label,"\n")}
near <- function(x,y,tol=1e-9) isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tol))
fails <- function(expr) inherits(tryCatch({force(expr);NULL},error=function(e)e),"error")
softplus <- function(x) pmax(x,0)+log1p(exp(-abs(x)))
# Gaussian/logistic formulas, independent of workbench distribution functions.
logsurv <- function(id,t,mu,sigma) {
 z <- (log(t)-mu)/sigma
 if(id=="lognormal") pnorm(z,lower.tail=FALSE,log.p=TRUE) else -softplus(z)
}
logdensity <- function(id,t,mu,sigma) {
 z <- (log(t)-mu)/sigma
 if(id=="lognormal") -.5*z^2-log(t*sigma)-.5*log(2*pi) else z-log(t*sigma)-2*softplus(z)
}
quantile_surv <- function(id,logs,mu,sigma) {
 if(id=="lognormal") exp(mu+sigma*qnorm(logs,lower.tail=FALSE,log.p=TRUE)) else exp(mu+sigma*(-logs+log1p(-exp(logs))))
}
check(all(c("lognormal","loglogistic") %in% e$core_models),"core entry opens both log-time models")
for(id in c("lognormal","loglogistic")) {
 mu <- log(180);sigma <- .65; m <- parameter_model(id,list(median=180,scale=sigma))
 t <- c(0,.01,20,180,500,5000)
 check(near(model_survival(m,t),exp(logsurv(id,t,mu,sigma))),paste(id,"survival agrees with Gaussian/logistic definition including zero age"))
 check(near(model_cumhaz(m,t),-logsurv(id,t,mu,sigma)),paste(id,"cumulative hazard agrees with independent log survival"))
 z <- c(0,.0001,log(2),2,12)
 check(near(inverse_cumhaz(m,z),quantile_surv(id,-z,mu,sigma)),paste(id,"inverse cumulative hazard matches reference quantiles"))
 a <- c(0,30,180,1200);u<-c(.99,.8,.3,.0001)
 ref <- quantile_surv(id,logsurv(id,a,mu,sigma)+log(u),mu,sigma)
 check(near(sample_conditional(m,a,u=u),ref)&&all(ref>=a),paste(id,"conditional event age preserves existing follow-up"))
 tiny <- parameter_model(id,list(median=1,scale=.1));age<-exp(80)
 check(model_survival(tiny,age)==0 && is.finite(model_cumhaz(tiny,age)) &&
       near(log(sample_conditional(tiny,age,u=.2)),log(quantile_surv(id,logsurv(id,age,0,.1)+log(.2),0,.1)),1e-10),paste(id,"log-tail conditioning remains finite after survival underflows"))
 for(unit in c("days","weeks","months")) {
  v<-group_input_defaults(unit);v$median<-180/time_factor(unit);v$scale<-sigma
  median_form<-parameter_input_model(id,v,unit);v$log_input<-"mu";v$log_mu<-log(v$median)
  mu_form<-parameter_input_model(id,v,unit)
  display<-unit_model_parameters(mu_form,unit)
  check(near(median_form$params,m$params)&&near(mu_form$params,m$params)&&near(display,c(v$log_mu,sigma)),paste(id,unit,"median/mu forms and display parameters represent the same distribution"))
 }
 vals<-list(median=180/30.4375,log_mu=log(180/30.4375),scale=sigma,g1_log_mu=1.3,g2_log_mu=2.7)
 converted<-convert_unit_inputs(vals,"months","days");back<-convert_unit_inputs(converted,"days","months")
 check(near(converted$log_mu,log(180))&&near(converted$g1_log_mu,1.3+log(30.4375))&&converted$scale==sigma&&near(unlist(back),unlist(vals)),paste(id,"pooled/grouped log locations shift with units while sigma stays fixed"))
 eq<-model_equivalents(m,"days");mean_ref<-if(id=="lognormal")180*exp(sigma^2/2) else 180*pi*sigma/sin(pi*sigma)
 check(near(eq$value[eq$parameter=="中位时间 m"],180)&&near(eq$value[eq$parameter=="均值"],mean_ref),paste(id,"equivalent parameters give independently calculated median and mean"))
 check(all(vapply(c(0,-1,Inf,NA_real_),function(s)fails(parameter_model(id,list(median=180,scale=s))),logical(1))),paste(id,"nonpositive or nonfinite sigma fails"))
 check(fails(parameter_input_model(id,list(log_input="mu",log_mu=Inf,scale=.8)))&&fails(parameter_input_model(id,list(log_input="mu",log_mu=1000,scale=.8))),paste(id,"invalid or unrepresentable mu fails visibly"))
 # Optimize the right-censored likelihood in mu/log(sigma), not survreg.
 set.seed(if(id=="lognormal")723 else 724);n<-700
 latent<-if(id=="lognormal")exp(mu+sigma*rnorm(n)) else {u0<-runif(n);exp(mu+sigma*log(u0/(1-u0)))}
 censor<-runif(n,80,380);d<-data.frame(time=pmin(latent,censor),event=as.integer(latent<=censor))
 fitted<-fit_model(d,id)
 nll<-function(par) {ls<-logsurv(id,d$time,par[1],exp(par[2]));ld<-logdensity(id,d$time,par[1],exp(par[2]));-sum(ifelse(d$event==1,ld,ls))}
 opt<-optim(c(log(160),log(.8)),nll,method="BFGS",control=list(reltol=1e-12),hessian=TRUE)
 reference<-c(opt$par[1],exp(opt$par[2]));ll<- -opt$value
 measurements[[paste0(id,"_fit")]]<-list(events=sum(d$event),survreg=as.list(fitted$params),independent=as.list(reference),loglik=fitted$loglik,reference_loglik=ll)
 check(opt$convergence==0&&min(eigen(opt$hessian,symmetric=TRUE)$values)>0&&near(fitted$params,reference,2e-5),paste(id,"AFT fit agrees with independent censored-likelihood optimization"))
 check(near(fitted$loglik,ll,1e-8)&&near(fitted$aic,4-2*ll,1e-8),paste(id,"log likelihood and two-parameter AIC agree with reference"))
 check(fails(fit_model(data.frame(time=c(10,20,30),event=c(1,0,0)),id)),paste(id,"fewer than two events rejects fit"))
 check(fails(fit_model(data.frame(time=rep(20,12),event=rep(1,12)),id)),paste(id,"degenerate tied-event fit is rejected rather than used for prediction"))
 # Current age 80, 120 active, five known events, independent exponential exits.
 cfg<-list(task="count",cut=100,horizon=120,sims=2000,target=10000,future_n=0,enroll_rate=0,
  dropout_rate=.003,lag=0,multiplier=1,seed=1762,prior_shape=.5,prior_rate=50,tail_rate=.002,
  uncertainty="plugin",clock="occurred",methods=id,cuts=numeric(),ensemble=FALSE,input_mode="parameters",
  origin="2025-01-01",display_unit="days",engine_unit="days")
 cohort<-parameter_cohort(100,120,5,80,"fixed",1)
 probability<-integrate(function(v)exp(logdensity(id,80+v,mu,sigma)-logsurv(id,80,mu,sigma)-.003*v),0,120,rel.tol=1e-10)$value
 r<-run_forecast(cohort,cfg,models_override=setNames(list(m),id));end<-r$counts[[1]][,121];expected<-5+120*probability;mcse<-sd(end)/sqrt(length(end))
 measurements[[paste0(id,"_count")]]<-list(expected=expected,observed=mean(end),mcse=mcse,tolerance=4*mcse+1e-6)
 check(abs(mean(end)-expected)<=4*mcse+1e-6,paste(id,"finite-window count with permanent exits agrees with independent integral within four MCSE"))
 check(all(r$counts[[1]][,1]==5)&&all(apply(r$counts[[1]],1,function(x)all(diff(x)>=0))),paste(id,"count trajectories retain known events and increase monotonically"))
 cfg$task<-"target";cfg$sims<-50;cfg$target<-126
 r<-run_forecast(cohort,cfg,models_override=setNames(list(m),id))
 check(r$summary$reached==0&&all(is.infinite(r$milestones[[1]]))&&r$summary$simulations==50,paste(id,"unreachable target retains all paths and infinite target dates"))
}
heavy<-parameter_model("loglogistic",list(median=180,scale=1.2));eq<-model_equivalents(heavy,"days")
check(is.infinite(eq$value[eq$parameter=="均值"])&&near(inverse_cumhaz(heavy,log(2)),180)&&is.finite(model_cumhaz(heavy,1e6)),"heavy-tail log-logistic has infinite mean but finite median and window probabilities")
# Independent generation check via probability integral transform and binomial MCSE.
models<-list(parameter_model("lognormal",list(median=180,scale=.65)),heavy)
scfg<-list(n=3000,reps=1,seed=520,origin="2025-01-01",paramcd="OS",display_unit="days",
 groups=list(list(name="A",weight=1,dropout_rate=.003,model=models[[1]]),list(name="B",weight=1,dropout_rate=.001,model=models[[2]])),
 enroll_mode="constant",enroll_rate=20,enroll_cuts=numeric(),enroll_rates=numeric(),entry_days=numeric(),cut_mode="fixed",cuts=c(100,300),target=1,max_day=300,fixed_times=c(30,60))
sr<-run_survival_simulation(scfg)
for(j in 1:2) {
 x<-sr$truth[sr$truth$group==c("A","B")[j],];p<-exp(logsurv(models[[j]]$id,x$event_time_day,log(180),models[[j]]$params[2]));bins<-tabulate(pmin(10L,1L+floor(p*10)),10);tol<-4*sqrt(nrow(x)*.1*.9)
 check(all(abs(bins-nrow(x)/10)<=tol),paste(models[[j]]$id,"generated latent times pass independent survival-PIT decile checks"))
}
check(all(sr$observed$time_day<=sr$observed$DCO_DAY-sr$observed$entry_day+1e-8)&&!any(c("event_time_day","dropout_time_day")%in%names(sr$observed)),"log-time simulation applies each fixed DCO and keeps latent truth out of observation data")
jsonlite::write_json(list(status="passed",version=e$core_version,R=R.version.string,survival=as.character(packageVersion("survival")),
 passed=length(checks),checks=checks,measurements=measurements,tolerances="deterministic 1e-9; independent fitted parameters 2e-5, likelihood/AIC 1e-8; forecast means and PIT deciles 4 MCSE; no bootstrap validation"),
 "validation/v2_log_models_results.json",auto_unbox=TRUE,pretty=TRUE,digits=NA)
cat("LOG_MODELS_CHECKS_PASSED",length(checks),"\n")
