# Focused release checks. Run from the repository root; no external data.
e <- new.env(parent=globalenv()); invisible(sys.source("app_core.R",envir=e))
attach(e)
checks <- list(); measurements <- list()
check <- function(ok,label) {
  if(!isTRUE(ok))stop(paste("FAILED:",label))
  checks[[length(checks)+1L]] <<- list(label=label,status="passed")
  cat(sprintf("PASS %02d: %s\n",length(checks),label))
}
near <- function(x,y,tol=1e-8) isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tol))
fails <- function(expr) inherits(tryCatch({force(expr);NULL},error=function(e)e),"error")
check(inherits(e$ui,"shiny.tag.list"),"core UI constructs with all help definitions")
check(!exists("register_joint_server",envir=e,inherits=FALSE),"core entry does not load joint research servers")
for(path in c("app_core.R",list.files("R",pattern="^core_.*[.]R$",full.names=TRUE)))check(!inherits(try(parse(path),silent=TRUE),"try-error"),paste("R parses",path))

# Unit equivalence and independently defined likelihoods.
v <- group_input_defaults("months"); v$design_model<-"exponential";v$exp_input<-"median";v$median<-12
m <- parameter_input_model("exponential",v,"months")
check(near(m$params,log(2)/(12*30.4375)),"exponential median/rate equivalence in continuous days")
v$exp_input<-"rate";v$exp_rate<-log(2)/12
check(near(parameter_input_model("exponential",v,"months")$params,m$params),"equivalent exponential forms produce identical distribution")
v$weibull_input<-"median";v$shape<-1.7;v$median<-18
w <- parameter_input_model("weibull",v,"months")
check(near(exp(w$params[1]),18*30.4375/log(2)^(1/1.7))&&near(1/w$params[2],1.7),"Weibull median/scale and AFT shape conversion")
v$weibull_input<-"eta";v$eta<-18/log(2)^(1/1.7)
check(near(parameter_input_model("weibull",v,"months")$params,w$params),"equivalent Weibull forms produce identical distribution")
v$drop_input<-"probability";v$drop_prob<-.05;v$drop_period<-3
check(near(dropout_input_rate(v),-log(.95)/3),"exit probability/rate conversion is an independent latent clock")
x <- list(median=12,eta=15,shape=1.7,enroll_rate=15,dropout_rate=.008,parameter_cuts="3,6",parameter_rates=".03,.06,.02")
for(unit in c("days","weeks")) {
 z<-convert_unit_inputs(convert_unit_inputs(x,"months",unit),unit,"months")
 check(near(z$median,x$median)&&near(z$eta,x$eta)&&near(z$enroll_rate,x$enroll_rate)&&near(z$dropout_rate,x$dropout_rate)&&near(parse_unit_numbers(z$parameter_rates),parse_unit_numbers(x$parameter_rates)),paste("unit round trip",unit))
}
d <- data.frame(id=paste0("P",1:6),entry=0,time=c(2,3,6,10,10,10),status=c("event","event","dropout","active","active","active"))
d <- validate_data(d,10)
check(near(fit_model(d,"exponential")$params,2/41),"exponential MLE counts events and all observed exposure")
pd <- data.frame(id=paste0("T",1:5),entry=0,time=c(3,6,9,12,15),status=c("event","event","event","active","active"))
pd <- validate_data(pd,15,require_events=FALSE,gap_mode="impute")
pf <- fit_model(pd,"pwe",c(3,6))
check(near(pf$params,c(1/15,1/12,1/18)),"PWE independent exposure and events at cut boundaries")
check(fails(fit_model(pd,"pwe",c(3,30))),"PWE rejects unobserved exposure segments")
zero_tail <- fit_model(pd,"pwe",c(3,9))
check(tail(zero_tail$params,1)==0&&length(zero_tail$warning)>0,"PWE zero-event exposed tail retains zero rate and warning")
set.seed(112);t<-rweibull(500,1.65,250);censor<-runif(500,100,450);wd<-data.frame(time=pmin(t,censor),event=as.integer(t<=censor))
wf<-fit_model(wd,"weibull")
nll<-function(z){k<-exp(z[1]);eta<-exp(z[2]);-sum(wd$event*(log(k)-k*log(eta)+(k-1)*log(wd$time))-(wd$time/eta)^k)}
opt<-optim(log(c(1.4,230)),nll,method="BFGS",control=list(reltol=1e-12))
check(opt$convergence==0&&near(c(1/wf$params[2],exp(wf$params[1])),exp(opt$par),2e-5),"Weibull fitted parameters agree with independent likelihood optimization")

# Conditional inversion: reference formulas do not call model_cumhaz/inverse_cumhaz.
u<-c(.999,.8,.5,.01);a<-c(1,50,120,300)
wm<-parameter_model("weibull",list(median=180,shape=1.8));eta<-180/log(2)^(1/1.8)
ref<-eta*((a/eta)^1.8-log(u))^(1/1.8)
check(near(sample_conditional(wm,a,u=u),ref),"Weibull conditioning uses existing risk age")
pm<-parameter_model("pwe",list(rates=c(0,.006,0)),c(50,100))
check(near(model_cumhaz(pm,c(0,50,75,100,500)),c(0,0,.15,.3,.3)),"PWE independent cumulative hazard includes zero segments")
check(near(inverse_cumhaz(pm,c(.06,.3)),c(60,100))&&is.infinite(inverse_cumhaz(pm,.301)),"PWE inversion handles boundary and finite cumulative hazard")
check(fails(sample_conditional(parameter_model("weibull",list(median=0,shape=1)),1)),"invalid model parameters rejected")

basecfg <- function(cut=100,h=120) list(task="count",cut=cut,horizon=h,sims=2000,target=100000,
 future_n=0,enroll_rate=0,dropout_rate=.003,lag=0,multiplier=1,seed=282,prior_shape=.5,prior_rate=50,
 tail_rate=.002,uncertainty="plugin",clock="occurred",methods="exponential",cuts=numeric(),ensemble=FALSE,
 input_mode="parameters",origin="2025-01-01",display_unit="days",engine_unit="days")
cohort<-parameter_cohort(100,150,5,80,"fixed",1)
mc_check <- function(res,expected,label) {
 values<-res$counts[[1]][,ncol(res$counts[[1]])];se<-sd(values)/sqrt(length(values));tolerance<-4*se+1e-6
 measurements[[label]] <<- list(expected=expected,observed=mean(values),mcse=se,tolerance=tolerance)
 check(abs(mean(values)-expected)<=tolerance,label)
}
expo<-parameter_model("exponential",list(median=150));cfg<-basecfg()
p<-log(2)/150/(log(2)/150+.003)*(1-exp(-(log(2)/150+.003)*120))
r<-run_forecast(cohort,cfg,models_override=list(exponential=expo));mc_check(r,5+150*p,"exponential event count with independent permanent exits")
check(all(r$counts[[1]][,1]==5)&&all(apply(r$counts[[1]],1,function(z)all(diff(z)>=0))),"known events fixed and each trajectory is monotone")
check(all(!is.finite(r$milestones[[1]]))&&r$summary$reached==0&&is.infinite(r$summary$median_day),"unreachable targets retain all simulations and infinite quantiles")
c2<-cfg;c2$target<-4;c2$task<-"target";ar<-run_forecast(cohort,c2,models_override=list(exponential=expo))
check(ar$summary$reached==1&&all(ar$milestones[[1]]<=100),"already reached targets use observed event history")
check(identical(r$counts,run_forecast(cohort,cfg,models_override=list(exponential=expo))$counts),"fixed seed reproduces forecast trajectories")
for(method in c("weibull","pwe")) {
 cc<-cfg;cc$methods<-method
 if(method=="weibull") {
  model<-wm;hfun<-function(t)1.8/eta*(t/eta)^.8;sfun<-function(t)exp(-(t/eta)^1.8)
  pr<-integrate(function(v)hfun(80+v)*sfun(80+v)/sfun(80)*exp(-.003*v),0,120,rel.tol=1e-10)$value
 } else {
  model<-parameter_model("pwe",list(rates=c(.002,.008,.004)),c(60,130))
  hf<-function(t).002*pmin(t,60)+.008*pmax(0,pmin(t,130)-60)+.004*pmax(0,t-130)
  density<-function(v)ifelse(80+v<=130,.008,.004)*exp(-hf(80+v)+hf(80)-.003*v)
  pr<-integrate(density,0,50)$value+integrate(density,50,120)$value
 }
 rr<-run_forecast(cohort,cc,models_override=setNames(list(model),method));mc_check(rr,5+150*pr,paste(method,"age-conditioned competing-exit event expectation"))
}
# Cap N: arrival i has Gamma(i,1) cumulative-intensity density, not uniform entry.
empty<-parameter_cohort(0,0,0,0,"fixed",1)
for(mode in c("constant","piecewise")) {
 cc<-basecfg(0,500);cc$future_n<-12;cc$enroll_rate<-.035;cc$enroll_mode<-mode
 if(mode=="piecewise"){cc$enroll_cuts<-c(100,250);cc$enroll_rates<-c(.06,.015,.04)}
 arrival<-function(s)if(mode=="constant").035*s else .06*pmin(s,100)+.015*pmax(0,pmin(s,250)-100)+.04*pmax(0,s-250)
 rate<-function(s)if(mode=="constant")rep(.035,length(s)) else ifelse(s<=100,.06,ifelse(s<=250,.015,.04))
 expected<-sum(vapply(1:12,function(i) {
  fn<-function(s)rate(s)*dgamma(arrival(s),shape=i,rate=1)*(log(2)/150)/(log(2)/150+.003)*(1-exp(-(log(2)/150+.003)*(500-s)))
  sum(vapply(list(c(0,100),c(100,250),c(250,500)),function(bounds)integrate(fn,bounds[1],bounds[2],rel.tol=1e-9)$value,numeric(1)))
 },numeric(1)))
 rr<-run_forecast(empty,cc,models_override=list(exponential=expo));mc_check(rr,expected,paste(mode,"capped future enrollment independent arrival integral"))
}
# Two groups; changing a withdrawn patient's event assumptions does not generate future events.
gd<-cohort;gd$group<-"A";gd2<-cohort;gd2$id<-paste0("B",gd2$id);gd2$group<-"B";gd<-rbind(gd,gd2)
gcfg<-cfg;gcfg$analysis_mode<-"grouped";gcfg$groups<-list(g1=list(name="A",future_n=0,enroll_rate=0,dropout_rate=.003,multiplier=1,lag=0,cuts=numeric(),tail_rate=.002,method="exponential"),g2=list(name="B",future_n=0,enroll_rate=0,dropout_rate=.003,multiplier=1,lag=0,cuts=numeric(),tail_rate=.002,method="weibull"));gcfg$methods<-c("exponential","weibull")
gr<-run_forecast(gd,gcfg,models_override=list(g1=list(exponential=expo),g2=list(weibull=wm)))
check(identical(gr$counts[[1]],gr$group_counts[[1]][[1]]+gr$group_counts[[1]][[2]]),"grouped counts equal paired per-trajectory sum")
check(all(gr$counts[[1]][,1]==10),"grouped starting events include both groups")
withdrawn<-data.frame(id=paste0("D",1:30),entry=0,time=40,status="dropout",obs_day=40)
rr<-run_forecast(rbind(cohort,withdrawn),cfg,models_override=list(exponential=expo))
check(identical(rr$counts,r$counts),"permanently withdrawn patients add no future events")

# ADTTE date and censoring definitions are checked independently of model fitting.
original_trial <- e$simulate_trial
e$simulate_trial <- function(...) list(occurred=c(5,15,15,35),reported=c(5,15,15,35))
tc <- basecfg(10,20);tc$task<-"target";tc$target<-3;tc$sims<-50
td<-data.frame(id=c("known","active"),entry=0,time=c(5,10),status=c("event","active"))
tr<-run_forecast(td,tc,models_override=list(exponential=expo))
check(all(tr$milestones[[1]]==15)&&tr$summary$reached==1,"target is the D*-th event date, including tied events")
tc$target<-4;tr<-run_forecast(td,tc,models_override=list(exponential=expo))
check(all(is.infinite(tr$milestones[[1]]))&&tr$summary$reached==0,"events after prediction window do not imply target attainment")
e$simulate_trial <- original_trial
tc<-cfg;tc$task<-"target";tc$target<-50
tr<-run_forecast(cohort,tc,models_override=list(exponential=expo))
check(near(tr$summary$reached_mcse,sqrt(tr$summary$reached*(1-tr$summary$reached)/tc$sims)),"target probability MCSE uses all successful simulation paths")

# ADTTE date and censoring definitions are checked independently of model fitting.
raw<-data.frame(USUBJID=c("E1","E2","A1","D1"),PARAMCD="PFS",STARTDT="2025-01-01",ADT=c("2025-01-06","2025-01-08","2025-01-11","2025-01-05"),AVAL=c(6,8,11,5),CNSR=c(0,0,1,2),TRTP=c("A","B","A","B"))
nd<-normalize_adtte(raw,"PFS","2025-01-01",10,1,2,group_column="TRTP")
check(identical(nd$time,c(5,7,10,4))&&identical(nd$status,c("event","event","active","dropout")),"ADTTE offset validates AVAL but does not add a continuous exposure day")
for(unit in c("weeks","months")) {x<-raw;x$AVAL<-x$AVAL/time_factor(unit);check(near(normalize_adtte(x,"PFS","2025-01-01",10,1,2,aval_unit=unit)$time,nd$time),paste("ADTTE AVAL units",unit))}
x<-raw;x$AVAL[1]<-x$AVAL[1]+1;check(fails(normalize_adtte(x,"PFS","2025-01-01",10,1,2)),"inconsistent AVAL rejected")
x<-raw;x$ADT[3]<-"2025-01-10";x$AVAL[3]<-10;check(fails(normalize_adtte(x,"PFS","2025-01-01",10,1,2)),"unconfirmed follow-up gap rejected")
x<-rbind(raw,raw[1,]);check(fails(normalize_adtte(x,"PFS","2025-01-01",10,1,2)),"duplicate analysis records rejected")

# Survival observation construction has hand-set event/exit/admin clocks.
truth<-data.frame(SIMID=1L,USUBJID=c("1","2","3","4"),group="A",entry_day=c(0,2,8,15),event_time_day=c(5,12,5,2),dropout_time_day=c(20,4,20,20));truth$event_day<-truth$entry_day+truth$event_time_day;truth$dropout_day<-truth$entry_day+truth$dropout_time_day
obs<-survival_at_cut(truth,10,1)
check(nrow(obs)==3&&near(obs$time_day,c(5,4,2))&&identical(obs$status,c("event","dropout","active")),"simulation cut uses event/exit/admin minimum and excludes future entry")
scfg<-list(n=120,reps=2,seed=192,origin="2025-01-01",paramcd="PFS",display_unit="days",groups=list(list(name="A",weight=1,dropout_rate=.003,model=expo),list(name="B",weight=1,dropout_rate=0,model=wm)),enroll_mode="constant",enroll_rate=.7,enroll_cuts=numeric(),enroll_rates=numeric(),entry_days=numeric(),cut_mode="fixed",cuts=c(100,300),target=1,max_day=300,fixed_times=c(30,60))
sr<-run_survival_simulation(scfg);sr2<-run_survival_simulation(scfg)
check(identical(sr$truth,sr2$truth)&&identical(sr$summary,sr2$summary),"simulation seed reproduces truth and analyses")
check(all(sr$observed$time_day<=sr$observed$DCO_DAY-sr$observed$entry_day+1e-8),"observed simulation never extends beyond cutoff")
check(all(sr$observed$event==as.integer(sr$observed$status=="event")),"simulation event indicator matches observed status")
adt<-simulation_adtte(sr$observed,scfg)
check(all(adt$AVAL==as.numeric(adt$ADT-adt$STARTDT)+1)&&all(adt$CNSR==ifelse(sr$observed$status=="event",0,ifelse(sr$observed$status=="dropout",2,1))),"simulation ADTTE preserves declared date offset and censor codes")
check(!any(c("event_time_day","dropout_time_day")%in%names(sr$observed)),"observation outputs exclude latent truth clocks")

# Freeze actual environment used; simulation limits/tolerances precede assessment.
packages<-c("shiny","bslib","commonmark","survival","ggplot2","plotly","DT","jsonlite","scales")
versions<-setNames(vapply(packages,function(p)as.character(packageVersion(p)),character(1)),packages)
jsonlite::write_json(list(version=e$core_version,R=R.version.string,packages=as.list(versions),checks=checks,measurements=measurements,passed=length(checks),tolerance_rule="analytical means: 4 empirical Monte Carlo SE + 1e-6; deterministic default 1e-8; independent Weibull optimization 2e-5",status="passed"),Sys.getenv("CORE_NUMERIC_OUTPUT",unset="validation/core_check_results.json"),auto_unbox=TRUE,pretty=TRUE,digits=NA)
cat("CORE_CHECKS_PASSED",length(checks),"\n")
