# V2-301 observable-event stopping, independent fixtures, KM, and denominators.
e<-new.env(parent=globalenv());invisible(sys.source("app_core.R",envir=e));attach(e)
checks<-character();measurements<-list()
check<-function(ok,label){if(!isTRUE(ok))stop(paste("FAILED:",label));checks<<-c(checks,label);cat("PASS:",label,"\n")}
near<-function(x,y,tol=1e-9)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=tol))
error_of<-function(expr)tryCatch({force(expr);NULL},error=identity)
cfg_for<-function(model=e$parameter_model("exponential",list(median=log(2)/.05)),reps=3)list(n=20,reps=reps,seed=7301,origin="2025-01-01",paramcd="OS",display_unit="days",engine_unit="days",groups=list(list(name="A",weight=1,dropout_rate=0,model=model)),enroll_mode="constant",enroll_rate=2,enroll_cuts=numeric(),enroll_rates=numeric(),entry_days=numeric(),cut_mode="target",cuts=numeric(),target=10,max_day=20,fixed_times=c(4,6,9,40))
truth_for<-function(entry,event,dropout=rep(Inf,length(entry)),groups=rep("A",length(entry)),sim=1L)data.frame(SIMID=sim,USUBJID=paste0("P",seq_along(entry)),group=groups,entry_day=entry,event_time_day=event,dropout_time_day=dropout,event_day=entry+event,dropout_day=entry+dropout)
truth<-truth_for(c(0,1,2,0,10,20,0),c(5,9,8,20,1,5,10),c(Inf,Inf,Inf,4,Inf,Inf,10),c("A","A","B","B","A","B","A"))
c<-cfg_for();c$n<-7;c$target<-2;c$max_day<-10;c$groups[[2]]<-list(name="B",weight=1,dropout_rate=0,model=c$groups[[1]]$model)
s<-e$simulation_cut_schedule(truth,c)
check(s$DCO_DAY==10&&s$target_day==10&&s$cut_reason=="event_target","hand-declared second observable event stops at day ten")
obs<-e$survival_at_cut(truth,10)
check(nrow(obs)==5&&sum(obs$event)==4&&identical(obs$USUBJID,c("P1","P2","P3","P4","P7")),"all tied DCO events retained; entry exactly at DCO and later entries excluded")
check(obs$status[obs$USUBJID=="P4"]=="dropout"&&obs$status[obs$USUBJID=="P7"]=="event","exit-before-event excludes latent event; exact event/exit tie counts as event")
c$target<-4;check(e$simulation_cut_schedule(truth,c)$DCO_DAY==10,"target exactly attained at window endpoint counts as reached")
c$target<-5;s<-e$simulation_cut_schedule(truth,c);check(s$DCO_DAY==10&&is.na(s$target_day)&&s$cut_reason=="window_end","first event beyond window cannot fabricate attainment or target date")
c$target<-1;check(e$simulation_cut_schedule(truth,c)$DCO_DAY==5,"first-event target analyzes immediately at the earliest observable event")
floating<-truth_for(.1,.2);dco<-floating$event_day
check(e$survival_at_cut(floating,dco)$event==1&&e$simulation_cut_schedule(floating,within(c,{max_day<-dco}))$DCO_DAY==dco,"calendar membership retains exact DCO event despite subtractive roundoff")
a<-e$analyze_simulated_survival(obs,c,1L,1L,10);f<-a$fixed[a$fixed$scope=="overall",]
check(near(f$survival[1:3],c(1,.75,.25))&&is.na(f$survival[4]),"KM fixed values match independent hand product of risk-set fractions without unsupported extrapolation")
check(identical(f$n_risk,c(5L,3L,2L,0L)),"independent risk-set counts retain event subjects immediately before each follow-up time")
check(sum(a$summary$events[a$summary$scope=="group"])==4&&a$summary$events[a$summary$scope=="overall"]==4,"group observed event counts sum to overall count at the same joint DCO")
# Controlled trials: ties reached, events impossible, and nobody enrolled.
original<-e$simulate_survival_truth;e$simulate_survival_truth<-function(cfg,sim_id){x<-truth
 if(sim_id==2){x$event_time_day<-Inf;x$event_day<-Inf}
 if(sim_id==3){x$entry_day<-11;x$event_day<-11+x$event_time_day;x$dropout_day<-11+x$dropout_time_day}
 x$SIMID<-sim_id;x$USUBJID<-paste0(sim_id,"-",x$USUBJID);x}
c$target<-2;r<-e$run_survival_simulation(c);o<-e$simulation_target_overview(r)
check(nrow(r$cuts)==3&&identical(r$cuts$target_reached,c(TRUE,FALSE,FALSE))&&all(r$cuts$DCO_DAY==10),"all requested trials receive actual DCO, including zero-event and no-entry misses")
check(o$requested==3&&o$trials==3&&o$reached==1&&o$unreached==2&&near(o$reached_probability,1/3)&&near(o$reached_mcse,sqrt(2/27)),"attainment proportion and binomial MCSE use all trials rather than only reached trials")
check(all(is.na(r$cuts$target_day[2:3]))&&o$conditional_target_median_day==10&&o$actual_dco_median_day==10,"missing hit dates remain distinct from real window-end analysis dates")
check(all(r$summary$n[r$summary$SIMID==3]==0)&&all(r$summary$median_status[r$summary$SIMID==3]=="无入组者")&&all(r$fixed$status[r$fixed$SIMID==3]=="无入组者"),"no-entry trial keeps all group summaries and unsupported fixed-time states")
check(!any(c("event_time_day","event_day","dropout_time_day","dropout_day")%in%names(r$observed)),"analysis observation data excludes latent truth clocks")
check(all(r$observed$obs_day<=r$observed$DCO_DAY)&&all(r$observed$entry_day<r$observed$DCO_DAY),"observation data uses only positive follow-up before each actual DCO")
e$simulate_survival_truth<-original
# Date precision is separate from stopping precision, including same-day rows.
tiny<-truth_for(c(0,.25),c(.5,1));v<-e$survival_at_cut(tiny,.5);adt<-e$simulation_adtte(v,c)
check(nrow(adt)==2&&all(adt$ADT==adt$STARTDT)&&all(adt$AVAL==1)&&identical(adt$CNSR,c(0L,1L)),"date-floor export preserves same-day rows, original event status and inclusive-day offset")
check(nrow(e$simulation_export_issues(adt))==2&&near(v$time_day,c(.5,.25)),"same-day export warning does not shift or remove continuous-time observations")
# All five core distributions, both accepted enrollment processes, exact replay.
models<-list(exponential=e$parameter_model("exponential",list(median=log(2)/.05)),weibull=e$parameter_model("weibull",list(median=20*log(2)^(1/1.3),shape=1.3)),pwe=e$parameter_model("pwe",list(rates=c(.02,.08)),10),lognormal=e$parameter_model("lognormal",list(median=15,scale=.7)),loglogistic=e$parameter_model("loglogistic",list(median=15,scale=.7)))
for(id in names(models)){
 c<-cfg_for(models[[id]],5);c$target<-4;c$groups[[2]]<-list(name="B",weight=2,dropout_rate=.03,model=models$exponential)
 if(id%in%c("pwe","loglogistic")){c$enroll_mode<-"piecewise";c$enroll_cuts<-5;c$enroll_rates<-c(1,3)}
 x<-e$run_survival_simulation(c);y<-e$run_survival_simulation(c)
 check(identical(x$truth,y$truth)&&identical(x$cuts,y$cuts)&&identical(x$summary,y$summary),paste(id,"seed exactly reproduces target cuts, truth and KM analyses"))
 expected<-vapply(1:5,function(b){t<-x$truth[x$truth$SIMID==b,];events<-sort((t$entry_day+t$event_time_day)[t$event_time_day<=t$dropout_time_day&t$entry_day+t$event_time_day<=20]);if(length(events)>=4)events[4] else 20},numeric(1))
 check(near(x$cuts$DCO_DAY,expected)&&all(x$cuts$target_reached==(x$cuts$events>=4)),paste(id,"actual DCO agrees with independent observable-event order statistic"))
 fixed<-c;fixed$cut_mode<-"fixed";fixed$cuts<-c(5,20);z<-e$run_survival_simulation(fixed)
 check(identical(x$truth,z$truth)&&all(z$cuts$cut_reason=="fixed_dco")&&is.null(e$simulation_target_overview(z)),paste(id,"fixed DCO keeps identical latent trajectories and does not invent target summary"))
}
c<-cfg_for(e$parameter_model("pwe",list(rates=c(0,0)),5));c$target<-1;x<-e$run_survival_simulation(c);o<-e$simulation_target_overview(x)
check(all(x$cuts$events==0)&&all(!x$cuts$target_reached)&&o$reached_probability==0&&all(is.na(o[,12:14])),"zero event hazard retains all misses and undefined conditional hit quantiles")
c$enroll_mode<-"piecewise";c$enroll_cuts<-5;c$enroll_rates<-c(0,0);x<-e$run_survival_simulation(c)
check(nrow(x$observed)==0&&nrow(x$summary)==6&&nrow(x$curves)==0&&all(x$cuts$n==0),"zero enrollment intensity keeps every empty trial without producing latent observations")
check(nrow(e$core_sim_filter(x$curves,1,1))==0,"empty event-driven curves pass the core display filter safely")
c<-cfg_for();c$target<-100000;x<-e$run_survival_simulation(c)
check(all(!x$cuts$target_reached)&&all(x$cuts$DCO_DAY==20),"unreachable target above planned population is a retained window-end scenario")
# Independent analytic benchmark: simultaneous planned entries, exp event clock.
c<-cfg_for(reps=200);c$enroll_mode<-"schedule";c$entry_days<-rep(0,20);c$fixed_times<-c(5,10,40)
x<-e$run_survival_simulation(c);o<-e$simulation_target_overview(x)
p<-pbinom(9,20,1-exp(-.05*20),lower.tail=FALSE);prob_tol<-4*sqrt(p*(1-p)/200)+1/200
expected_dco<-integrate(function(t)pbinom(9,20,1-exp(-.05*t)),0,20,rel.tol=1e-10)$value
se<-sd(x$cuts$DCO_DAY)/sqrt(200);dco_tol<-4*se+1e-6
measurements$analytic_exponential<-list(n=20,target=10,window_day=20,rate=.05,trials=200,seed=7301,expected_probability=p,observed_probability=o$reached_probability,probability_tolerance=prob_tol,expected_actual_dco=expected_dco,observed_actual_dco=mean(x$cuts$DCO_DAY),dco_mcse=se,dco_tolerance=dco_tol)
check(abs(o$reached_probability-p)<=prob_tol,"attainment proportion agrees with independent binomial probability within predeclared four MCSE")
check(abs(mean(x$cuts$DCO_DAY)-expected_dco)<=dco_tol,"mean actual DCO agrees with integrated stopped order-statistic expectation within four MCSE")
unit<-c;unit$display_unit<-"weeks";u<-e$run_survival_simulation(unit)
check(identical(x$cuts,u$cuts)&&identical(x$truth,u$truth)&&identical(x$summary,u$summary),"display unit does not alter canonical event-driven simulation or RNG")
for(v in list(0,1.5,NA_real_,100001)){bad<-c;bad$target<-v;check(inherits(error_of(e$validate_simulation_config(bad)),"error"),paste("invalid target rejected",v))}
bad<-c;bad$max_day<-3651;check(inherits(error_of(e$validate_simulation_config(bad)),"error"),"event-driven maximum window obeys engine limit")
jsonlite::write_json(list(status="passed",passed=length(checks),checks=checks,measurements=measurements,R=R.version.string,scope="synthetic fixtures; independent KM/order-statistic/binomial references; not stopping-rule coverage or power"),"validation/v2_event_simulation_results.json",auto_unbox=TRUE,pretty=TRUE,digits=NA)
cat("EVENT_SIMULATION_CHECKS_PASSED",length(checks),"\n")
