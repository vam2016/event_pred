local({
 source('R/simulation.R');source('R/conditional_simulation.R')
 x<-validate_data(demo_data(n=120),540);x$group<-rep(c('A','B'),60)
 m<-parameter_model('exponential',list(median=200))
 g<-function(name)list(name=name,model=m,fit_method='exponential',cuts=numeric(),tail_rate=.002,future_n=10,enroll_mode='constant',enroll_rate=.5,enroll_cuts=numeric(),enroll_rates=numeric(),dropout_rate=.001,multiplier=1)
 cc<-complete_config(list(cut=540,origin='2025-01-01',paramcd='OS',source='test',display_unit='days',groups=list(g('A'),g('B')),model_mode='manual',uncertainty='plugin',cut_mode='fixed',cuts=c(700,900),target=1,max_day=900,fixed_times=c(100,200,1000),reps=30,seed=81,q_values=c(.75,1,1.25),prior_shape=.5,prior_rate=50))
 r<-run_conditional_simulation(x,cc)
 check(identical(r$observed,run_conditional_simulation(x,cc)$observed),'IA conditional simulation is reproducible')
 check(nrow(r$summary)==30*3*2*3,'all simulations, scenarios, cuts, overall and group analyses retained')
 check(!any(c('event_time_day','dropout_time_day','event_day') %in% names(r$observed)),'Final KM analysis excludes unobserved trajectory fields')
 for(status in c('event','dropout')) {
  known<-x[x$status==status,];y<-r$observed[r$observed$USUBJID %in% known$id,];ix<-match(y$USUBJID,known$id)
  check(all(y$status==status)&&near(y$time_day,known$time[ix])&&near(y$obs_day,known$obs_day[ix]),paste('all q/cut paths preserve IA',status,'time and status'))
 }
 a<-r$truth[r$truth$source=='IA记录'&r$truth$USUBJID %in% x$id[x$status=='active'],]
 ix<-match(a$USUBJID,x$id);check(all(a$event_time_day>x$time[ix])&&all(a$dropout_time_day>x$time[ix]),'active IA patients are conditioned beyond their original observed ages')
 y<-r$truth[r$truth$source=='未来入组',];check(all(y$entry_day>540)&&nrow(y)==30*3*20,'future patients enter after IA with specified group maxima')
 lo<-r$truth[r$truth$scenario_q==.75,];hi<-r$truth[r$truth$scenario_q==1.25,]
 check(identical(lo$USUBJID,hi$USUBJID)&&near(lo$entry_day,hi$entry_day)&&near(lo$dropout_time_day,hi$dropout_time_day)&&all(hi$event_time_day<=lo$event_time_day),'q scenarios share randomness and only modify future event hazards')
 early<-r$observed[r$observed$CUTID==1,];late<-r$observed[r$observed$CUTID==2,];i<-match(paste(early$SIMID,early$scenario_q,early$USUBJID),paste(late$SIMID,late$scenario_q,late$USUBJID))
 check(all(late$time_day[i]>=early$time_day)&&all(late$event[i][early$event==1]==1),'multiple Final cuts share coherent patient trajectories')
 check(all(is.na(r$fixed$survival[r$fixed$time_day==1000])),'Final fixed survival outside observation support is undefined')
 ov<-conditional_overview(r);check(all(ov$joint_increase_probability<=ov$conditional_increase_probability,na.rm=TRUE),'joint increase probability is bounded by estimability-conditional probability')
 check(all(r$summary$median_change_day==r$summary$median_day-r$summary$IA_median_day,na.rm=TRUE),'median changes use captured observed IA baseline')
 fx<-conditional_fixed_overview(r);check(all(fx$estimable>=0&fx$estimable<=1)&&all(is.na(fx$IA_survival[fx$time_day==1000])),'fixed survival overview records estimability and undefined IA extrapolation')
 # Independent conditional exponential probability for a fixed IA risk set.
 aa<-data.frame(id=paste0('c',1:100),entry=0,time=100,obs_day=100,status='active',group='A');gg<-g('A');gg$future_n<-0;gg$dropout_rate<-0
 ca<-cc;ca$cut<-100;ca$groups<-list(gg);ca$cuts<-200;ca$max_day<-200;ca$q_values<-1;ca$reps<-100;ca$fixed_times<-50
 rr<-run_conditional_simulation(aa,ca);p<-1-exp(-log(2)*100/200);n<-10000
 check(abs(sum(rr$observed$event)-n*p)<4*sqrt(n*p*(1-p)),'conditional event count matches independent exponential residual probability')
 check(all(is.na(rr$summary$IA_median_day))&&all(is.na(conditional_overview(rr)$conditional_increase_probability)),'IA NR leaves median changes and increase probabilities undefined')
 # Exact event-driven target and unreachable-window retention.
 ct<-cc;ct$cut_mode<-'target';ct$target<-100;ct$q_values<-1;ct$reps<-5
 rt<-run_conditional_simulation(x,ct)
 for(b in unique(rt$cuts$SIMID)) {
  tr<-rt$truth[rt$truth$SIMID==b,];dates<-sort(tr$event_day[is.finite(tr$event_day)&tr$event_time_day<=tr$dropout_time_day&tr$event_day<=ct$max_day]);z<-rt$cuts[rt$cuts$SIMID==b,]
  check(near(z$DCO_DAY,if(length(dates)>=ct$target)max(ct$cut,dates[ct$target]) else ct$max_day),'conditional target cut equals independent observable event order statistic')
 }
 ct$target<-999;nr<-run_conditional_simulation(x,ct);check(nrow(nr$cuts)==5&&all(!nr$cuts$target_reached)&&all(nr$cuts$DCO_DAY==ct$max_day),'unreachable Final target retains all requested trials at maximum DCO')
 ct$target<-1;at<-run_conditional_simulation(x,ct);check(all(at$cuts$DCO_DAY==ct$cut)&&all(at$cuts$analysis_status=='IA已达标'),'target already reached at IA uses observed IA cut')
 fit<-cc;fit$model_mode<-'fit';fit$q_values<-1;fit$reps<-10
 for(mode in c('plugin','bootstrap','gamma')) {
  fit$uncertainty<-mode;z<-run_conditional_simulation(x,fit)
  check(nrow(z$summary)>=.9*10*2*3&&identical(z$baseline$observed,r$baseline$observed),paste(mode,'uses original IA risk set and baseline'))
 }
 fit$uncertainty<-'plugin';fit$process_uncertainty<-'gamma';fit$recruit_start<-0;fit$recruit_end<-540
 pr<-run_conditional_simulation(x,fit)
 check(near(pr$process_posteriors[[1]]$enroll['shape'],1+sum(x$group=='A'))&&near(pr$process_posteriors[[1]]$dropout['shape'],.5+sum(x$group=='A'&x$status=='dropout')),'conditional process posteriors update group-specific enrollment and dropout counts')
 # MCMC continuation draws a joint parameter row after diagnostic gating.
 mc<-fit;mc$process_uncertainty<-'fixed';mc$uncertainty<-'bayes_weibull';mc$groups<-list(g('A'));mc$groups[[1]]$fit_method<-'weibull';mc$reps<-2;mc$mcmc_draws<-4000
 md<-x;md$group<-'A';mr<-run_conditional_simulation(md,mc)
 check(mr$models[[1]]$posterior$passed&&nrow(mr$summary)==8,'Weibull MCMC conditional simulation passes diagnostics and retains original history')
 small<-data.frame(id=letters[1:8],entry=0,time=c(10,15,20,25,30,30,30,30),obs_day=c(10,15,20,25,30,30,30,30),status=c(rep('event',4),rep('active',4)),group='A')
 sc<-cc;sc$cut<-30;sc$cuts<-c(50,70);sc$max_day<-70;sc$groups<-list(g('A'));sc$groups[[1]]$future_n<-0;sc$model_mode<-'fit';sc$uncertainty<-'bootstrap';sc$q_values<-1;sc$reps<-100;sc$seed<-26
 sr<-run_conditional_simulation(small,sc);sv<-conditional_overview(sr)
 check(nrow(sr$failures)>0&&length(unique(sr$summary$SIMID))>=90,'low-event bootstrap failures are recorded separately from NR and target misses')
 check(near(sv$failure_bound_upper-sv$failure_bound_lower,rep(nrow(sr$failures)/100,nrow(sv))),'failure probability bounds account for every failed requested replicate')
 reserved<-x;reserved$id[1]<-'FUTURE::A::1';cr<-run_conditional_simulation(reserved,cc)
 tr<-cr$truth[cr$truth$SIMID==1&cr$truth$scenario_q==1,];check(!anyDuplicated(tr$USUBJID),'future IDs cannot collide with existing IA IDs')
 # Reject unsupported data/parameter combinations without modifying histories.
 bad<-x;bad$obs_day[bad$status=='active']<-bad$obs_day[bad$status=='active']-1;bad$time[bad$status=='active']<-bad$time[bad$status=='active']-1
 check(fails(run_conditional_simulation(bad,cc)),'IA gaps require confirmed data rather than silent historical imputation')
 badc<-cc;badc$cuts<-c(500,700);check(fails(run_conditional_simulation(x,badc)),'Final DCO cannot precede or equal IA in fixed-DCO mode')
 badc<-cc;badc$q_values<-c(1,0);check(fails(run_conditional_simulation(x,badc)),'nonpositive future q rejected')
 badc<-cc;badc$model_mode<-'fit';badc$uncertainty<-'gamma';badc$groups[[1]]$fit_method<-'weibull';check(fails(run_conditional_simulation(x,badc)),'Gamma posterior rejects unsupported fitted distribution')
 # Hidden irrelevant parameters must never be parsed.
 check(!fails(parameter_input_model('exponential',list(exp_input='median',median=12,parameter_rates='invalid',parameter_cuts='invalid'))),'exponential ignores irrelevant hidden PWE text')
 shiny::testServer(e$server,{
  session$setInputs(task='conditional',ci_source='demo',ci_unit='months',ci_grouped='pooled',ci_model_mode='fit',ci_uncertainty='plugin',ci_process='fixed',ci_cut_mode='fixed',ci_cuts='24,30',ci_fixed='6,12,18',ci_reps=3,ci_seed=31,ci_scenarios=FALSE,c1_fit_method='exponential',c1_future_n=0,c1_dropout_rate=0,c1_drop_input='rate',c1_multiplier=1)
  session$setInputs(ci_run=1,ci_trial='1',ci_view_cut='1',ci_view_q='1')
  check(grepl('合成示例',output$ci_status$html,fixed=TRUE)&&grepl('data',output$ci_km,fixed=TRUE),'Shiny IA conditional run and KM overlay complete')
  check(nzchar(output$ci_overview)&&nzchar(output$ci_fixed_overview)&&nzchar(output$ci_baseline_summary),'Shiny conditional summaries and captured IA baseline render')
 })
})
