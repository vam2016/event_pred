# D1: NPH generation and prespecified fixed-design comparative methods.
local({
  source('R/nph.R',local=TRUE);source('R/study.R',local=TRUE);source('R/study_server.R',local=TRUE)
  group_input_defaults<-e$group_input_defaults;simulation_defaults<-e$simulation_defaults
  m<-parameter_model('exponential',list(median=100));lambda<-log(2)/100
  tt<-c(0,20,50,80,200);h<-lambda*(pmin(tt,50)+.5*pmax(tt-50,0))
  check(near(nph_cumhaz(m,tt,50,c(1,.5)),h),'delayed-effect cumulative hazard matches independent exponential piecewise integral')
  check(near(nph_inverse(m,h,50,c(1,.5)),tt),'NPH inverse matches exact delayed-effect event ages including boundary')
  for(id in parameter_catalog()$id) {
    model<-parameter_input_model(id,group_input_defaults('days'),'days');cuts<-c(50,200);hr<-c(1.3,.8,.5);t<-c(10,50,100,200,300)
    hh<-nph_cumhaz(model,t,cuts,hr);check(near(nph_inverse(model,hh,cuts,hr),t,tol=1e-7),paste(id,'NPH cumulative hazard and inverse round-trip'))
    check(near(nph_cumhaz(model,t,cuts,rep(.7,3)),.7*model_cumhaz(model,t)),paste(id,'constant piecewise HR reduces to PH'))
  }
  defective<-parameter_model('pwe',list(rates=c(.01,0)),100)
  check(near(nph_cumhaz(defective,Inf,50,c(1,.5)),.75)&&is.infinite(nph_inverse(defective,1,50,c(1,.5))),'zero baseline tail retains finite transformed hazard and infinite event mass')
  check(fails(validate_hr_profile(c(10,10),c(1,.8,.5)))&&fails(validate_hr_profile(10,c(1,0))),'malformed HR profile and nonpositive HR rejected')
  # Independent event-time FH weights and risk-set variance.
  time<-c(1,2,2,3,4,4);event<-c(1,1,0,1,1,0);arm<-c(0,1,0,1,0,1)
  pre<-c(1,5/6,2/3,4/9);u<-c(-.5,.4,1/3,-.5);v<-c(.25,.24,2/9,.25)
  fh<-study_fh(time,event,arm,0,1)
  check(near(fh$score,sum((1-pre)*u))&&near(fh$variance,sum((1-pre)^2*v)),'late-weighted FH score/variance match independent hand calculation with ties')
  lr<-study_logrank(time,event,arm);f0<-study_fh(time,event,arm,0,0)
  check(near(c(f0$score,f0$variance),c(lr$score,lr$variance)),'FH(0,0) reduces exactly to standard log-rank')
  check(near(study_fh(time,event,1-arm,0,1)$score,-fh$score),'weighted log-rank benefit direction reverses on group swap')
  # KM functionals, independent Greenwood hand reference and software references.
  km<-study_km_functional(c(1,2,3,4),c(1,1,0,1),3,'rmst')
  check(near(km$estimate,2.25)&&near(km$variance,1.25^2/12+.5^2/6),'KM RMST area and Greenwood variance match hand integral')
  check(fails(study_km_functional(c(1,2),c(1,0),3)),'positive KM tail beyond observation support is never extrapolated')
  zero<-study_km_functional(c(1,2),c(1,1),5)
  check(near(zero$estimate,1.5)&&near(zero$variance,.125),'KM tail at zero permits further tau without positive-survival extrapolation')
  set.seed(10);t<-pmin(rexp(200,.01),rexp(200,.004),200);d<-as.integer(t<200) # Explicit separate censoring reference below.
  eventt<-rexp(200,.01);cens<-pmin(rexp(200,.004),200);t<-pmin(eventt,cens);d<-as.integer(eventt<=cens);arm<-rep(0:1,each=100)
  obs<-data.frame(time_day=t,event=d,group=ifelse(arm,'Treatment','Control'))
  cfg<-list(primary='rmst',analysis_tau=100,sided='two',alpha=.05)
  ours<-study_contrast(obs,cfg);ref<-survRM2::rmst2(t,d,arm,tau=100)$unadjusted.result[1,]
  check(near(c(ours$difference,ours$lower,ours$upper,ours$p),unname(ref),tol=1e-8),'RMST difference/CI/p agree with independently installed survRM2')
  sf<-survival::survfit(survival::Surv(t[arm==0],d[arm==0])~1,timefix=FALSE)
  rr<-summary(sf,times=80);ss<-study_km_functional(t[arm==0],d[arm==0],80,'survival')
  check(near(ss$estimate,rr$surv)&&near(ss$variance,rr$std.err^2),'fixed-time KM survival and Greenwood variance agree with official survival')
  one<-cfg;one$sided<-'benefit';oo<-study_contrast(obs,one)
  check(near(oo$p,pnorm(oo$z,lower.tail=FALSE)),'RMST positive Treatment−Control contrast uses upper-tail benefit test')
  # End-to-end NPH, null labels, honest Cox summaries and reproducible background/export.
  base<-list(goal='grid',effect_mode='delayed',hr_cuts=50,hr_profile=c(1,.5),include_h0=TRUE,n_values=100,hr_values=numeric(),reps=20,seed=351,treatment_fraction=.5,control_model=m,dropout_rates=c(0,0),display_unit='days',origin='2025-01-01',paramcd='OS',enroll_mode='schedule',entry_days=rep(0,100),cut_mode='fixed',dco_values=c(300,500),miss_policy='analyze',primary='rmst',analysis_tau=100,sided='two',alpha=.05,estimate_hr=TRUE,reference_power=.8)
  sc<-study_scenarios(base);exact_delta<-(1-exp(-lambda*50))/lambda+exp(-lambda*50)*(1-exp(-lambda*.5*50))/(lambda*.5)-(1-exp(-lambda*100))/lambda
  check(near(sc$true_effect[sc$profile_id==1],rep(exact_delta,2))&&all(sc$hypothesis[sc$profile_id==0]=='H0'),'NPH RMST true contrast agrees with piecewise exponential analytic areas')
  b<-base;b$analysis_tau<-30
  check(all(study_scenarios(b)$hypothesis=='H0'),'RMST before delayed benefit is correctly labeled null even though later curves differ')
  b$primary<-'survival';check(all(study_scenarios(b)$hypothesis=='H0'),'fixed-time pre-benefit null is based on target survival difference')
  r<-run_design_study(base);a<-r$rows[r$rows$BASEID==1,]
  check(nrow(r$rows)==80&&all(r$rows$generated)&&all(r$rows$decision_valid),'NPH H0/H1 multiple-cut matrix retains planned valid RMST decisions')
  check(all(is.na(a$true_hr))&&all(is.na(a$covered))&&all(is.na(r$overview$beta_bias[r$overview$effect_label!='HR(t)=1'])),'NPH Cox does not fabricate a constant true HR bias or coverage')
  check(all(is.finite(study_contrast_overview(r)$true_effect)),'RMST recovery retains actual estimand truth rather than Cox HR')
  b<-base;b$primary<-'fh';b$fh_rho<-0;b$fh_gamma<-1;ff<-run_design_study(b)
  check(all(ff$rows$decision_valid)&&all(ff$rows$fh_valid),'FH primary decision integrates with repeated NPH studies')
  b$primary<-'survival';ss<-run_design_study(b);check(all(ss$rows$decision_valid),'fixed-time primary decision integrates with repeated NPH studies')
  b<-base;b$cut_mode<-'target';b$target_values<-c(30,999);b$max_day<-500;b$miss_policy<-'no_reject';et<-run_design_study(b)
  check(all(et$rows$events[et$rows$target_reached]==30)&&all(!et$rows$decision_reject[!et$rows$target_reached]),'NPH target cutoff and missed-target decision preserve all repeats')
  j<-study_start_job(base);on.exit({if(j$process$is_alive())j$process$kill_tree();unlink(j$dir,recursive=TRUE)},add=TRUE);j$process$wait(15000)
  check(identical(study_read_job(j)$result$rows,r$rows),'background NPH worker agrees with synchronous simulation including contrast truth')
  tmp<-tempfile(fileext='.R');writeLines(study_reproduction_script(r),tmp);env<-new.env(parent=environment());sys.source(tmp,envir=env)
  check(identical(env$res$rows,r$rows)&&near(study_contrast_overview(env$res)$mean_difference,study_contrast_overview(r)$mean_difference),'NPH export script losslessly reproduces every analysis and contrast summary')
  unlink(c(tmp,'study_trials.csv','study_summary.csv','study_contrasts.csv','study_looks.csv','study_stopping.csv'))
  # Hidden PH fields and FH custom weights do not constrain NPH/preset configurations.
  vals<-list(st_goal='power',st_effect='delayed',st_delay=6,st_late_hr=.65,st_hr_grid='invalid',st_h0=TRUE,st_unit='months',st_origin='2025-01-01',st_endpoint='OS',st_n=100,st_allocation=.5,st_enroll_mode='constant',st_enroll_rate=15,st_cut_mode='fixed',st_dco=36,st_primary='fh',st_weight_preset='01',st_fh_rho=-99,st_fh_gamma=-99,st_sided='two',st_alpha=.05,st_estimate=FALSE,st_precision='fixed',st_reps=20,st_seed=31)
  inp<-study_input_config(vals);check(near(inp$hr_cuts,6*30.4375)&&near(inp$hr_profile,c(1,.65))&&inp$fh_gamma==1,'delayed NPH entry ignores hidden PH grids and inactive custom FH values')
  vals$st_effect<-'crossing';vals$st_change<-6;vals$st_early_hr<-.7;vals$st_cross_hr<-.8
  check(fails(study_input_config(vals)),'crossing preset requires HR on opposite sides of one')
  # Prespecified 1000-repeat null/alternative matrix for each new primary test.
  summary<-list();raw<-list()
  cal<-base;cal$n_values<-300;cal$entry_days<-rep(0,300);cal$dco_values<-600;cal$reps<-1000;cal$estimate_hr<-FALSE;cal$hr_profile<-c(1,.65);cal$hr_cuts<-60;cal$analysis_tau<-100
  for(method in c('fh','rmst','survival')) {
    cal$primary<-method;cal$fh_rho<-0;cal$fh_gamma<-1;cal$seed<-switch(method,fh=4501,rmst=4502,survival=4503)
    z<-run_design_study(cal);v<-z$overview;h0<-v[v$hypothesis=='H0',]
    check(all(abs(h0$rejection_conditional-.05)<4*sqrt(.05*.95/1000)),paste(method,'1000-repeat null rejection agrees with nominal alpha within preset 4-MCSE bound'))
    check(all(v$invalid==0)&&all(v$generation_failures==0),paste(method,'calibration retains all generated trials and decisions'))
    if(method!='fh') {
      co<-study_contrast_overview(z)
      check(all(abs(co$coverage-.95)<4*sqrt(.95*.05/co$valid))&&all(abs(co$bias)<4*co$empirical_sd/sqrt(co$valid)),paste(method,'contrast recovery and 95% coverage meet prespecified MC precision bounds'))
    }
    summary[[method]]<-cbind(primary=method,v);raw[[method]]<-cbind(primary=method,z$rows)
    if(method!='fh')write.csv(study_contrast_overview(z),paste0('validation/nph_',method,'_recovery.csv'),row.names=FALSE)
  }
  write.csv(do.call(rbind,summary),'validation/nph_calibration_summary.csv',row.names=FALSE);cols<-unique(unlist(lapply(raw,names)));raw<-lapply(raw,function(d){for(nm in setdiff(cols,names(d)))d[[nm]]<-NA;d[,cols,drop=FALSE]});write.csv(do.call(rbind,raw),'validation/nph_calibration_raw.csv',row.names=FALSE)
  jsonlite::write_json(list(config=cal,seeds=c(fh=4501,rmst=4502,survival=4503),methods=c('fh','rmst','survival'),tolerance='H0 rejection 4 MCSE; coverage 4 MCSE; bias 4 empirical-SD/sqrt(B)',reference=as.character(packageVersion('survRM2'))),'validation/nph_calibration_config.json',auto_unbox=TRUE,pretty=TRUE,digits=NA)
})
