# D2a: prespecified group-sequential PH/log-rank and canonical conditional power.
local({
  e<-new.env();sys.source('app.R',envir=e)
  group_input_defaults<-e$group_input_defaults;simulation_defaults<-e$simulation_defaults;simulation_parameter_model<-e$simulation_parameter_model
  source('R/sequential.R',local=TRUE);source('R/study.R',local=TRUE);source('R/study_server.R',local=TRUE)
  z<-list(design_mode='sequential',goal='power',n_values=300,hr_values=c(1,.7),effect_mode='ph',reps=20,seed=71,treatment_fraction=.5,control_model=parameter_model('exponential',list(median=100)),dropout_rates=c(0,0),display_unit='days',origin='2025-01-01',paramcd='OS',enroll_mode='schedule',entry_days=rep(0,300),cut_mode='target',target_values=180,max_day=1000,miss_policy='no_reject',primary='logrank',sided='benefit',alpha=.025,estimate_hr=FALSE,gs_timing=c(.5,1),gs_spending='asOF',gs_futility='none',gs_futility_z=numeric())
  set.seed(11);before<-.Random.seed;p<-gs_plan(z,180)
  check(identical(before,.Random.seed),'boundary construction preserves the patient-generation random state')
  # rpact asOF uses total A(t)=2*sided*Phi(-z[1-alpha/(2*sided)]/sqrt(t)).
  spent<-2*pnorm(-qnorm(1-z$alpha/2)/sqrt(p$information_fraction))
  check(max(abs(p$alpha_spent-spent))<1e-7&&near(p$upper_z[1],qnorm(1-spent[1])),'OBF-type spending and first boundary match independent analytic formula')
  q<-z;q$gs_spending<-'asP';q$gs_timing<-c(.3,.7,1);pp<-gs_plan(q,181)
  check(max(abs(pp$alpha_spent-q$alpha*log(1+(exp(1)-1)*pp$information_fraction)))<1e-7,'Pocock-type cumulative alpha matches independent spending formula after event rounding')
  check(identical(gs_event_targets(100,c(.28,1)),c(28,100)),'event rounding removes machine precision overshoot at exact integer targets')
  check(identical(pp$target_events,c(55,127,181))&&near(pp$information_fraction,pp$target_events/181),'event targets round upward and actual target ratios determine the saved boundaries')
  null_rejection<-function(plan,sided,delta=0) {t<-plan$information_fraction;cov<-outer(t,t,function(x,y)sqrt(pmin(x,y)/pmax(x,y)));1-gs_probability(if(sided=='two')plan$lower_efficacy_z else rep(-Inf,length(t)),plan$upper_z,delta*sqrt(t),cov)}
  for(sided in c('benefit','two'))for(spending in c('asOF','asP')) {
    q<-z;q$sided<-sided;q$alpha<-if(sided=='two').05 else .025;q$gs_spending<-spending;q$gs_timing<-c(.3,.7,1);b<-gs_plan(q,180)
    check(abs(null_rejection(b,sided)-q$alpha)<2e-6,paste(spending,sided,'joint normal H0 crossing probability agrees with total alpha'))
  }
  q<-z;q$gs_timing<-c(.3,.7,1);q$gs_futility<-'z';q$gs_futility_z<-c(0,.2);p3<-gs_plan(q,180)
  cp<-gs_conditional_power(p,1,1,.7);mu<-sqrt(.5)+(-log(.7)*sqrt(180*.25))*.5
  check(near(cp$full,pnorm((mu-p$upper_z[2])/sqrt(.5)))&&near(cp$full,cp$final_only),'one remaining look CP agrees exactly with independent conditional normal tail')
  respect<-gs_conditional_power(p3,1,1,.7);ignore<-gs_conditional_power(p3,1,1,.7,respect_futility=FALSE)
  check(respect$full<=ignore$full+1e-8&&ignore$full>=ignore$final_only-1e-8,'complete-path CP respects optional futility and differs from final-only rejection')
  check(gs_conditional_power(p3,1,-1,.7)$full==0&&gs_conditional_power(p3,1,10,.7)$full==1,'already stopped interim has decided status and no invented final-only CP')
  two<-z;two$sided<-'two';two$alpha<-.05;two$gs_timing<-c(.3,.7,1);tp<-gs_plan(two,180)
  ca<-gs_conditional_power(tp,1,1,.7,sided='two');cb<-gs_conditional_power(tp,1,-1,1/.7,sided='two')
  check(near(ca$full,cb$full,tol=1e-7),'two-sided CP preserves group/direction reversal including both efficacy tails')
  # Independent Brownian-increment Monte Carlo check of future full-path probability.
  B<-100000;set.seed(721);t0<-p3$information_fraction[1];tt<-p3$information_fraction[-1];dt<-diff(c(t0,tt));delta<--log(.7)*sqrt(180*.25)
  inc<-matrix(rnorm(B*length(dt)),B)*rep(sqrt(dt),each=B)+rep(delta*dt,each=B)
  score<-t(apply(inc,1,cumsum))+sqrt(t0);ww<-sweep(score,2,sqrt(tt),'/');active<-rep(TRUE,B);reject<-rep(FALSE,B)
  for(j in seq_along(tt)){hit<-active&ww[,j]>=p3$upper_z[j+1];reject[hit]<-TRUE;active[hit]<-FALSE;if(j<length(tt))active[active&ww[,j]<=p3$futility_z[j+1]]<-FALSE}
  pr<-mean(reject)
  check(abs(pr-respect$full)<4*sqrt(pr*(1-pr)/B),'full-path CP agrees with independent 100000 Brownian-increment paths within 4 MCSE')
  set.seed(80);before<-.Random.seed;gs_conditional_power(p3,1,1,.7)
  check(identical(before,.Random.seed),'deterministic CP does not advance the simulation random stream')
  for(change in list(list(gs_timing=c(.5,.5,1)),list(gs_timing=c(.05,1)),list(gs_timing=c(.5,.9)),list(primary='rmst'),list(effect_mode='delayed'),list(cut_mode='fixed'),list(miss_policy='analyze'),list(estimate_hr=TRUE),list(gs_futility='z',gs_futility_z=c(0,0)),list(gs_futility='z',gs_futility_z=5))) {
    bad<-modifyList(z,change);check(fails(validate_study_config(bad))||fails(gs_prepare(bad)),'unsupported sequential timing/method/stop boundary rejected')
  }
  r<-run_design_study(z)
  check(nrow(r$rows)==40&&all(r$rows$decision_valid)&&all(r$rows$generated),'sequential study denominator counts independent trials rather than repeated looks')
  check(all(r$looks$events[r$looks$performed]==r$looks$target_events[r$looks$performed]),'each completed sequential analysis contains exactly its prespecified event count')
  check(all(vapply(split(r$looks,paste(r$looks$SCENARIO,r$looks$SIMID)),function(x)all(head(x$action,-1)=='continue')&&tail(x$action,1)!='continue',logical(1))),'no analysis after the first efficacy/futility/final stopping action')
  check(all(r$rows$decision_reject==startsWith(r$rows$gs_stop_reason,'efficacy')),'trial rejection is determined by boundary crossing rather than last nominal p')
  sm<-study_sample(r,1,1)
  check(sm$observed$DCO_DAY[1]==r$rows$DCO_DAY[1]&&sum(sm$observed$event)==r$rows$events[1],'selected sample observation data is censored at actual stopping time')
  unreachable<-z;unreachable$target_values<-10000;ur<-run_design_study(unreachable)
  check(all(ur$rows$decision_valid)&all(!ur$rows$decision_reject)&all(!ur$looks$performed)&all(ur$rows$gs_stop_reason=='window_unreached'),'unreached first look closes the window without an extra significance test')
  # Common patient draws compare non-binding futility to ignoring it.
  fut<-z;fut$gs_futility<-'z';fut$gs_futility_z<-0;fr<-run_design_study(fut)
  check(all(!fr$rows$decision_reject|r$rows$decision_reject)&&all(fr$rows$DCO_DAY<=r$rows$DCO_DAY),'executed non-binding futility is a pathwise subset of efficacy without futility')
  check(all(!fr$config$estimate_hr)&&all(fr$overview$cox_valid==0),'sequential decision does not label ordinary stopped-sample Wald intervals as adjusted coverage')
  n<-0L;part<-run_design_study(z,progress=function(s)n<<-s$completed,should_cancel=function()n>0,batch_size=2)
  tmp<-tempfile(fileext='.R');writeLines(study_reproduction_script(part),tmp);re<-new.env(parent=environment());sys.source(tmp,envir=re)
  check(identical(re$res$rows,part$rows)&&identical(re$res$looks,part$looks)&&all(re$res$overview$requested==20),'cancelled sequential replay preserves exact saved trials, look traces and requested denominator')
  unlink(c(tmp,'study_trials.csv','study_summary.csv','study_contrasts.csv','study_looks.csv','study_stopping.csv'))
  job<-study_start_job(z);on.exit({if(job$process$is_alive())job$process$kill_tree();unlink(job$dir,recursive=TRUE)},add=TRUE);job$process$wait(15000);bg<-study_read_job(job)$result
  check(identical(bg$rows,r$rows)&&identical(bg$looks,r$looks),'private background sequential worker agrees with synchronous trial and look records')
  # Hidden NPH/method/fixed-cut fields do not constrain this workflow.
  values<-list(st_design_mode='sequential',st_goal='type1',st_effect='delayed',st_hr_grid='invalid',st_origin='2025-01-01',st_endpoint='OS',st_n=100,st_allocation=.5,st_enroll_mode='constant',st_enroll_rate=15,st_cut_mode='fixed',st_dco=-1,st_target=60,st_max=36,st_miss='analyze',st_primary='rmst',st_tau=-1,st_sided='two',st_alpha=.05,st_estimate=TRUE,st_precision='fixed',st_reps=20,st_seed=31,st_gs_timing='0.5,1',st_gs_spending='asOF',st_gs_futility='z',st_gs_futility_z='invalid')
  inp<-study_input_config(values)
  check(inp$primary=='logrank'&&inp$effect_mode=='ph'&&inp$cut_mode=='target'&&inp$miss_policy=='no_reject'&&!inp$estimate_hr&&inp$gs_futility=='none','sequential entry ignores hidden incompatible methods and two-sided futility inputs')
  # Prespecified trial-level PH validation matrix against independent canonical probabilities.
  raw<-list();overview<-list();configurations<-list()
  for(kind in c('of_one','p_two','of_futility')) {
    cal<-z;cal$reps<-1000;cal$seed<-switch(kind,of_one=8111,p_two=8112,of_futility=8113)
    if(kind=='p_two'){cal$sided<-'two';cal$alpha<-.05;cal$gs_spending<-'asP';cal$gs_timing<-c(.3,.7,1)}
    if(kind=='of_futility'){cal$gs_futility<-'z';cal$gs_futility_z<-0}
    res<-run_design_study(cal);ov<-res$overview;plan<-res$config$gs_plans[['180']]
    h0<-ov[ov$hypothesis=='H0',]
    if(kind!='of_futility')check(abs(h0$rejection_conditional-cal$alpha)<4*sqrt(cal$alpha*(1-cal$alpha)/1000),paste(kind,'1000-trial H0 error satisfies prespecified 4-MCSE nominal bound')) else check(h0$rejection_conditional<=cal$alpha+4*sqrt(cal$alpha*(1-cal$alpha)/1000),'non-binding futility H0 rejection stays below alpha plus prespecified MC precision')
    check(all(ov$invalid==0)&&all(ov$generation_failures==0),paste(kind,'calibration retains every generated trial decision'))
    if(kind!='of_futility'){alt<-ov[ov$hypothesis=='H1',];expected<-null_rejection(plan,cal$sided,-log(.7)*sqrt(180*.25));check(abs(alt$rejection_conditional-expected)<4*sqrt(expected*(1-expected)/1000)+.02,paste(kind,'PH trial power agrees with independent canonical normal approximation'))}
    overview[[kind]]<-cbind(design=kind,ov);raw[[kind]]<-cbind(design=kind,res$rows);configurations[[kind]]<-res$config
  }
  write.csv(do.call(rbind,overview),'validation/gs_calibration_summary.csv',row.names=FALSE);write.csv(do.call(rbind,raw),'validation/gs_calibration_raw.csv',row.names=FALSE)
  jsonlite::write_json(list(configurations=configurations,tolerance='H0 4 MCSE; H1 4 MCSE + .02 canonical approximation; futility only upper error bound',dependencies=study_dependencies(z)),'validation/gs_calibration_config.json',auto_unbox=TRUE,pretty=TRUE,digits=NA)
})
