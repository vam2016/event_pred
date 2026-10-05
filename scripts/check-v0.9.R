# Independent fixed-design study checks. No internet or external data required.
local({
  source('R/study.R',local=TRUE);source('R/study_server.R',local=TRUE)
  group_input_defaults<-e$group_input_defaults;simulation_defaults<-e$simulation_defaults
  base<-list(goal='grid',n_values=100,hr_values=c(1,.7),reps=20,seed=42,treatment_fraction=.5,alpha=.05,primary='logrank',sided='two',estimate_hr=TRUE,miss_policy='analyze',cut_mode='fixed',dco_values=c(300,900),enroll_mode='constant',enroll_rate=1,dropout_rates=c(0,0),control_model=parameter_model('exponential',list(median=200)),display_unit='days',origin='2025-01-01',paramcd='OS',reference_power=.8)
  # Exact hand risk-set calculation with event/event/censor ties.
  t<-c(1,2,2,3,4,4);d<-c(1,1,0,1,1,0);arm<-c(0,1,0,1,0,1)
  refU<- -3/6+(1-3/5)+(1-2/3)-1/2
  refV<- .25+3*2/25+2/9+.25
  lr<-study_logrank(t,d,arm)
  check(near(lr$score,refU)&&near(lr$variance,refV),'log-rank tied risk sets match independent hypergeometric hand calculation')
  x<-data.frame(time_day=t,event=d,group=ifelse(arm,'Treatment','Control'),arm=arm)
  official<-survival::survdiff(survival::Surv(time_day,event)~arm,data=x,rho=0)
  aa<-analyze_study_trial(x,base,1)
  check(near(aa$logrank_z^2,official$chisq)&&near(aa$logrank_p,official$pvalue),'standard log-rank agrees with official survival implementation on ties')
  set.seed(183);x<-data.frame(time_day=pmin(rexp(400),1.8),group=rep(c('Control','Treatment'),200));x$event<-as.integer(x$time_day<1.8);x$arm<-as.integer(x$group=='Treatment')
  oo<-survival::survdiff(survival::Surv(time_day,event)~arm,data=x);aa<-analyze_study_trial(x,base,1)
  cf<-survival::coxph(survival::Surv(time_day,event)~arm,data=x,ties='efron',control=survival::coxph.control(iter.max=50,timefix=FALSE))
  check(near(aa$logrank_z^2,oo$chisq),'continuous log-rank agrees with official software')
  check(near(aa$beta,coef(cf))&&near(aa$se_beta,sqrt(vcov(cf))), 'Cox estimate and SE agree with separate official fit')
  single<-base;single$sided<-'benefit';as<-analyze_study_trial(x,single,1)
  check(near(as$logrank_p,pnorm(as$logrank_z))&&near(as$cox_p,pnorm(as$beta/as$se_beta)),'one-sided benefit tests use Treatment-negative direction')
  x$group<-ifelse(x$group=='Treatment','Control','Treatment');swap<-analyze_study_trial(x,single,1)
  check(near(as$logrank_p+swap$logrank_p,1)&&near(as$cox_p+swap$cox_p,1),'swapping labels reverses signed one-sided alternatives')
  sep<-data.frame(group=rep(c('Control','Treatment'),each=15),time_day=c(1:15,rep(20,15)),event=c(rep(1,15),rep(0,15)))
  ss<-analyze_study_trial(sep,base,.7)
  check(ss$logrank_valid&&!ss$cox_valid&&nzchar(ss$cox_note),'Cox infinite estimate is flagged while log-rank can remain valid')
  zero<-sep;zero$event<-0;zz<-analyze_study_trial(zero,base,1)
  check(!zz$logrank_valid&&!zz$cox_valid&&is.na(zz$logrank_reject),'no-event trial gives invalid analysis rather than invented p-value')
  close<-study_logrank(c(1,1+1e-10,2,3),rep(1,4),c(0,1,0,1));exact<-study_logrank(c(1,1,2,3),rep(1,4),c(0,1,0,1))
  check(!near(close$score,exact$score),'near-neighbor continuous event times are not silently tied')
  check(fails(study_logrank(c(1,2),c(1,1),c(0,2))),'invalid arm coding rejected')
  # PH inversion for all Control distributions, including defective tails.
  for(id in parameter_catalog()$id) {
    model<-parameter_input_model(id,group_input_defaults('days'),'days');u<-c(.95,.8,.6);tt<-sample_conditional(model,rep(0,3),multiplier=.7,u=u)
    check(near(model_survival(model,tt)^.7,u),'PH inverse matches survival-power identity for finite tested quantiles')
  }
  w<-parameter_model('weibull',list(median=12,shape=1.2));med<-sample_conditional(w,0,multiplier=.7,u=.5)
  check(near(med,12*.7^(-1/1.2)),'Weibull Treatment median uses HR power rather than median divided by HR')
  cr<-parameter_model('cure_weibull',list(median=12,shape=1.2,cure=.3));check(near(model_survival(cr,Inf)^.7,.3^.7),'PH transformation changes cure mass by survival power')
  # Reproducibility and shared patients across cutoffs, including regenerable samples.
  r<-run_design_study(base);r2<-run_design_study(base)
  check(identical(r$rows,r2$rows),'independent design studies reproduce every analysis row')
  check(nrow(r$rows)==80&&all(r$rows$generated)&&all(r$rows$decision_valid),'H0/H1 fixed-cut matrix retains every planned analysis')
  a<-r$rows[r$rows$SCENARIO==1,];b<-r$rows[r$rows$SCENARIO==2,]
  check(identical(a$replicate_seed,b$replicate_seed)&&all(b$events>=a$events),'same N/HR multiple DCO analyses share seeds and monotone events')
  check(!any(r$rows$replicate_seed[r$rows$BASEID==1] %in% r$rows$replicate_seed[r$rows$BASEID==2]),'different HR bases use independent repeat seeds')
  sample<-study_sample(r,1,2);row<-r$rows[r$rows$SCENARIO==1&r$rows$SIMID==2,]
  check(sum(sample$observed$event)==row$events&&!any(grepl('event_time_day|dropout_time_day',names(sample$observed))),'regenerated study sample matches summary and excludes latent times')
  check(nrow(sample$truth)==row$n&&all(sample$truth$SCENARIO==1)&&all(sample$observed$SCENARIO==1),'sample truth and observations preserve scenario keys with planned patient count')
  pp<-study_paired_comparisons(r);delta<-as.integer(b$decision_reject)-as.integer(a$decision_reject)
  check(near(pp$conditional_difference[1],mean(delta))&&near(pp$paired_mcse[1],sd(delta)/sqrt(length(delta))),'paired cutoff comparison uses empirical covariance of rejection indicators')
  boundary<-data.frame(SIMID=1,USUBJID=c('e','d','a'),group='Control',entry_day=10.1,event_time_day=c(.1,Inf,Inf),dropout_time_day=c(Inf,.1,Inf),event_day=c(10.2,Inf,Inf),dropout_day=c(Inf,10.2,Inf))
  ob<-survival_at_cut(boundary,10.2)
  check(identical(ob$status,c('event','dropout','active'))&&near(ob$time_day[1:2],c(.1,.1)),'exact calendar cutoff retains event and dropout despite subtractive floating-point roundoff')
  # Event-driven cutoff independently calculated from latent observable order statistics.
  target<-base;target$cut_mode<-'target';target$target_values<-c(40,999);target$max_day<-500
  rt<-run_design_study(target)
  for(b in 1:3) {
    cell<-rt$scenarios[1,];set.seed(study_replicate_seed(target$seed,cell$BASEID,b));truth<-simulate_survival_truth(study_trial_config(target,cell),b)
    times<-sort(truth$event_day[truth$event_time_day<=truth$dropout_time_day&truth$event_day<=target$max_day]);cut<-if(length(times)>=40)times[40] else 500
    z<-rt$rows[rt$rows$SCENARIO==1&rt$rows$SIMID==b,]
    check(near(z$DCO_DAY,cut),'event-driven DCO equals independently sorted observable event time')
  }
  hit<-rt$rows[rt$rows$target_reached,,drop=FALSE];check(all(hit$events==hit$cut_value),'every reached continuous event-driven cut observes exactly D* events')
  miss<-rt$rows[rt$rows$cut_value==999,];check(all(!miss$target_reached)&&all(miss$DCO_DAY==500),'unreachable event targets retain every trial at maximum window')
  target$miss_policy<-'no_reject';nr<-run_design_study(target);miss<-nr$rows[nr$rows$cut_value==999,]
  check(all(miss$decision_valid)&all(!miss$decision_reject),'predefined missed-target nonrejection rule forms valid decisions')
  nc<-base;nc$control_model<-parameter_model('pwe',list(rates=0));allzero<-run_design_study(nc)
  check(all(allzero$overview$invalid==20)&&all(allzero$overview$rejection_lower==0)&&all(allzero$overview$rejection_upper==1)&&all(is.na(allzero$overview$rejection_conditional)), 'all-invalid trials retain failure denominator and full probability bounds')
  ww<-study_wilson(0,20);check(ww[2]>0&&ww[1]>=-1e-15,'zero rejections retain nonzero-width Wilson interval')
  # Cancel at a whole paired replicate boundary; preserve completed keys.
  done<-0L;cc<-run_design_study(base,progress=function(s)done<<-s$completed,should_cancel=function()done>=6,batch_size=1)
  check(cc$status=='cancelled'&&cc$completed==6&&nrow(cc$rows)==6&&all(cc$overview$requested==20),'cooperative cancellation saves completed full paired replicates with original requested counts')
  empty<-run_design_study(base,should_cancel=function()TRUE)
  check(empty$status=='cancelled'&&nrow(empty$rows)==0&&all(c('SCENARIO','SIMID') %in% names(empty$rows)),'cancellation before first trial preserves empty result schema')
  rr<-tempfile(fileext='.R');writeLines(study_reproduction_script(cc),rr);ee<-new.env(parent=environment());sys.source(rr,envir=ee)
  check(identical(cc$rows,ee$res$rows)&&near(cc$overview$rejection_conditional,ee$res$overview$rejection_conditional),'exported R script reproduces saved rows of a cancelled run')
  big_replay<-base;big_replay$reps<-1000;done<-0L;short<-run_design_study(big_replay,progress=function(s)done<<-s$completed,should_cancel=function()done>0,batch_size=1)
  writeLines(study_reproduction_script(short),rr);sys.source(rr,envir=ee)
  check(identical(short$rows,ee$res$rows)&&ee$replay_cfg$reps==20&&all(ee$res$overview$requested==1000),'cancelled replay restricts repeat prefix while preserving original requested counts and exact rows')
  check(any(grepl('Cox恢复与覆盖',study_report(short),fixed=TRUE))&&any(grepl('失败原因计数',study_report(short),fixed=TRUE)),'partial Markdown report retains parameter recovery and failure-denominator details')
  unlink(c(rr,'study_trials.csv','study_summary.csv','study_contrasts.csv','study_looks.csv','study_stopping.csv'))
  # Independent PH normal approximation reference and a prespecified Monte Carlo matrix.
  check(near(study_ph_reference(1,180)$approximate_rejection,.05)&&near(study_ph_reference(.7,180)$approximate_required_events,246.787104,tol=1e-7),'PH information reference matches H0 alpha and independent Schoenfeld event calculation')
  check(is.infinite(study_ph_reference(1,180)$approximate_required_events)&&is.infinite(study_ph_reference(1.2,180,sided='benefit')$approximate_required_events),'no finite required events for null or opposite one-sided effect')
  cal<-base;cal$n_values<-300;cal$reps<-1000;cal$dco_values<-2000;cal$enroll_mode<-'schedule';cal$entry_days<-rep(0,300);cal$control_model<-parameter_model('exponential',list(median=100));cal$seed<-9031
  mc<-run_design_study(cal);ov<-mc$overview;ref<-study_ph_reference(ov$true_hr,ov$mean_events)
  # Tolerance fixed in advance: 4 Monte Carlo SE + 0.02 finite-sample allowance for the normal approximation.
  check(all(abs(ov$rejection_conditional-ref$approximate_rejection)<4*sqrt(ref$approximate_rejection*(1-ref$approximate_rejection)/cal$reps)+.02),'H0/H1 1000-trial rejection matrix agrees with independent PH approximation within preset tolerance')
  check(all(abs(ov$coverage-.95)<4*sqrt(.95*.05/ov$cox_valid))&&all(abs(ov$beta_bias)<.04),'PH Cox coverage and logHR recovery satisfy preset 4-MCSE / bias checks')
  check(all(ov$generation_failures==0)&all(ov$cox_valid==cal$reps),'calibration matrix preserves every generated and estimated trial')
  write.csv(ov,'validation/study_calibration_summary.csv',row.names=FALSE);write.csv(mc$rows,'validation/study_calibration_raw.csv',row.names=FALSE)
  jsonlite::write_json(list(config=cal,tolerance='4 MCSE + .02 approximation; coverage 4 MCSE; logHR bias < .04',dependencies=c(R=as.character(getRversion()),survival=as.character(packageVersion('survival')))),'validation/study_calibration_config.json',pretty=TRUE,auto_unbox=TRUE,digits=NA)
  # Parameter limits and modes.
  for(change in list(list(n_values=c(100,100)),list(hr_values=0),list(reps=19),list(treatment_fraction=1),list(alpha=0),list(dco_values=3651),list(n_values=seq(10,5000,by=10)))) {
    bad<-modifyList(base,change);check(fails(validate_study_config(bad)),'invalid study limits/configuration rejected')
  }
  # A real isolated worker completes one run; separate worker is cooperatively cancelled.
  j<-study_start_job(base);on.exit({if(j$process$is_alive())j$process$kill_tree();unlink(j$dir,recursive=TRUE)},add=TRUE)
  j$process$wait(timeout=15000);state<-study_read_job(j)
  check(isTRUE(state$finished)&&identical(state$result$rows,r$rows),'background R worker reproduces synchronous core results')
  big<-base;big$reps<-1000;j2<-study_start_job(big);on.exit({if(j2$process$is_alive())j2$process$kill_tree();unlink(j2$dir,recursive=TRUE)},add=TRUE)
  deadline<-Sys.time()+10
  repeat {s<-study_read_job(j2);if(!is.null(s$state$rows)||s$finished||Sys.time()>deadline)break;Sys.sleep(.05)}
  study_cancel_job(j2);j2$process$wait(timeout=15000);s<-study_read_job(j2)
  check(s$finished&&s$result$status=='cancelled'&&s$result$completed<s$result$total,'background cancellation publishes explicitly partial final result')
  check(j$dir!=j2$dir&&!file.exists(file.path(j$dir,'cancel.flag')),'separate sessions have isolated job state and cancellation files')
  check(!j$process$is_alive()&&!j2$process$is_alive(),'completed and cancelled local workers exit without orphan processes')
  # Mode-specific configuration ignores hidden irrelevant fields.
  vals<-list(st_goal='type1',st_unit='months',st_origin='2025-01-01',st_endpoint='PFS',st_n=100,st_hr_grid='invalid',st_allocation=.5,st_enroll_mode='constant',st_enroll_rate=15,st_enroll_cuts='invalid',st_cut_mode='fixed',st_dco=36,st_target_grid='invalid',st_primary='logrank',st_sided='two',st_alpha=.05,st_estimate=TRUE,st_precision='mcse',st_epsilon=.01,st_seed=31)
  inp<-study_input_config(vals);check(identical(inp$hr_values,1)&&inp$reps==2500&&near(inp$dco_values,36*30.4375),'type-I entry ignores hidden grids and computes planned precision B')
  vals$st_goal<-'power';vals$st_hr<-.7;vals$st_h0<-TRUE;vals$st_power<-.8;inp<-study_input_config(vals)
  check(near(inp$hr_values,c(.7,1)),'power entry adds explicit optional H0 benchmark')
  shiny::testServer(function(input,output,session){handles<-e$register_study_server(input,output,session)}, {
    vals<-list(st_goal='type1',st_unit='months',st_origin='2025-01-01',st_endpoint='PFS',st_n=100,st_allocation=.5,st_enroll_mode='constant',st_enroll_rate=15,st_cut_mode='fixed',st_dco=36,st_primary='logrank',st_sided='two',st_alpha=.05,st_estimate=FALSE,st_precision='fixed',st_reps=5000,st_seed=1)
    do.call(session$setInputs,vals);session$setInputs(st_run=1)
    running<-handles$job();check(!is.null(running)&&running$process$is_alive(),'Shiny session launches independent background worker')
    session$setInputs(st_n=120,st_alpha=.1)
    check(handles$job()$config$n_values==100&&handles$job()$config$alpha==.05,'editing inputs during background run preserves submitted configuration snapshot')
    session$close()
    check(!running$process$is_alive()&&!dir.exists(running$dir),'closing Shiny session stops its active worker and cleans its private files')
  })
})
