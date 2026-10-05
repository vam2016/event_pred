# Independent references for trajectories, KM, exports and Markdown/TeX.
local({
  source('R/simulation.R')
  m <- parameter_model('exponential',list(median=100))
  cc <- list(n=100,reps=3,seed=24,origin='2025-01-01',paramcd='OS',display_unit='days',
    groups=list(list(name='A',weight=1,dropout_rate=.001,model=m),list(name='B',weight=1,dropout_rate=.002,model=m)),
    enroll_mode='constant',enroll_rate=1,cut_mode='fixed',cuts=c(100,300),target=60,max_day=500,fixed_times=c(25,50,600))
  z <- run_survival_simulation(cc)
  check(identical(z$observed,run_survival_simulation(cc)$observed),'design trials reproduce under fixed seed')
  check(nrow(z$truth)==300 && nrow(z$cuts)==6,'design stores planned patients and each analysis cut')
  check(all(z$observed$time_day>0) && all(z$observed$obs_day<=z$observed$DCO_DAY),'observed patients respect entry and DCO')
  check(!any(c('event_time_day','dropout_time_day','event_day') %in% names(z$observed)),'analysis data excludes unobserved truth')
  for(b in 1:3) {
    early<-subset(z$observed,SIMID==b&CUTID==1);late<-subset(z$observed,SIMID==b&CUTID==2)
    e<-early[early$event==1,];l<-late[match(e$USUBJID,late$USUBJID),]
    check(near(e$time_day,l$time_day)&&all(l$event==1),'later cut preserves all earlier event histories')
    d<-early[early$status=='dropout',];l<-late[match(d$USUBJID,late$USUBJID),]
    check(near(d$time_day,l$time_day)&&all(l$status=='dropout'),'later cut preserves permanent follow-up exits')
  }
  check(all(z$fixed$status[z$fixed$time_day==600]=='超出观察范围')&&all(is.na(z$fixed$survival[z$fixed$time_day==600])),'fixed survival never extends beyond observed support')
  check(all(z$summary$n==z$summary$events+z$summary$dropouts+z$summary$administrative),'observed status counts partition each cohort')
  fixture<-data.frame(SIMID=1,USUBJID=paste0('x',1:4),group='A',entry_day=0,event_time_day=c(2,4,8,10),dropout_time_day=Inf,event_day=c(2,4,8,10),dropout_day=Inf)
  cf<-cc;cf$groups<-cf$groups[1];cf$fixed_times<-c(3,5)
  o<-survival_at_cut(fixture,6)
  check(near(o$time_day,c(2,4,6,6))&&identical(o$event,c(1L,1L,0L,0L)),'independent four-patient DCO construction')
  a<-analyze_simulated_survival(o,cf,1,1,6)
  check(near(a$fixed$survival[a$fixed$scope=='overall'],c(.75,.5)),'KM product limits match hand calculation')
  check(near(a$fixed$n_risk[a$fixed$scope=='overall'],c(3,2)),'risk numbers use patients observed through requested age')
  nd<-cc;nd$enroll_mode<-'schedule';nd$n<-4;nd$entry_days<-rep(0,4);nd$groups<-list(list(name='A',weight=1,dropout_rate=0,model=parameter_model('pwe',list(rates=0))))
  nr<-run_survival_simulation(nd)
  check(all(nr$summary$median_status=='NR')&&all(is.na(nr$summary$median_day)),'zero-risk tail retains NR instead of finite or infinite estimated median')
  check(all(simulation_result_overview(nr)$median_estimable==0)&&all(is.na(simulation_result_overview(nr)$conditional_median_day)),'all-NR overview records zero estimability with undefined conditional median')
  nd$cut_mode<-'target';nd$target<-2;nn<-run_survival_simulation(nd)
  check(all(!nn$cuts$target_reached)&&all(is.na(nn$cuts$target_day))&&all(nn$cuts$DCO_DAY==nd$max_day),'unreached event target retains every trial at window end')
  td<-cc;td$cut_mode<-'target';td$target<-20;tz<-run_survival_simulation(td)
  check(all(tz$cuts$events>=20)&&all(tz$cuts$target_reached),'event-driven analysis reaches configured total target')
  for(b in 1:3) {
    tt<-subset(tz$truth,SIMID==b);dates<-sort(tt$event_day[tt$event_time_day<=tt$dropout_time_day]);dco<-tz$cuts$DCO_DAY[tz$cuts$SIMID==b]
    check(near(dco,dates[20]),'event-driven DCO equals independent event order statistic')
  }
  pe<-cc;pe$enroll_mode<-'piecewise';pe$enroll_cuts<-20;pe$enroll_rates<-c(1,0);pp<-run_survival_simulation(pe)
  check(any(is.infinite(pp$truth$entry_day))&&!any(is.infinite(pp$observed$entry_day)),'zero enrollment tail preserves non-entered plans outside analyses')
  ne<-nd;ne$enroll_mode<-'schedule';ne$entry_days<-rep(1000,4);ne$cut_mode<-'fixed';ne$cuts<-100
  no<-run_survival_simulation(ne);aa<-simulation_adtte(no$observed,ne)
  check(nrow(no$observed)==0&&all(no$summary$n==0)&&nrow(aa)==0,'no-entry cut has valid empty observations and export headers')
  vv<-list(exp_input='survival',survival_time=12,survival_prob=.5)
  ms<-simulation_parameter_model('exponential',vv,'months')
  check(near(ms$params[1],log(2)/(12*30.4375)),'fixed-time exponential calibration matches independent analytic rate')
  vp<-list(pwe_input='survival',parameter_cuts='3,6',pwe_survivals='.8,.6',pwe_last_rate=.05)
  mp<-simulation_parameter_model('pwe',vp,'months')
  check(near(model_survival(mp,c(3,6)*30.4375),c(.8,.6))&&near(tail(mp$params,1),.05/30.4375),'PWE survival-point calibration and tail agree with input')
  vp$pwe_survivals<-'0.6,0.8';check(fails(simulation_parameter_model('pwe',vp)),'increasing PWE survival inputs rejected')
  vv$survival_prob<-1;check(fails(simulation_parameter_model('exponential',vv)),'invalid exponential fixed survival rejected')
  for(field in c('n','reps','target','max_day','fixed_times')) {
    bad<-cc;bad[[field]]<-NA_real_;check(fails(validate_simulation_config(bad)),paste('invalid design field rejected:',field))
  }
  aa<-simulation_adtte(o,cc)
  back<-normalize_adtte(aa,'OS',cc$origin,6,offset=1,dropout_codes=2,aval_unit='days',group_column='TRTP')
  check(identical(back$id,aa$USUBJID)&&near(back$time,as.numeric(aa$ADT-aa$STARTDT)),'ADTTE date export reads back through existing prediction contract')
  check(all(aa$AVAL==as.numeric(aa$ADT-aa$STARTDT)+1),'date export AVAL agrees with dates and offset')
  tiny<-o;tiny$entry_day<-0;tiny$time_day<-c(.1,.2,.3,.4);tiny$obs_day<-tiny$time_day
  tiny_adtte<-simulation_adtte(tiny,cc)
  check(nrow(simulation_export_issues(tiny_adtte))==4&&all(tiny_adtte$STARTDT==tiny_adtte$ADT)&&identical(tiny_adtte$CNSR,c(0L,0L,1L,1L)),'zero-day export preserves original status and identifies incompatibility')
  for(unit in c('days','weeks','months')) {
    cu<-cc;cu$display_unit<-unit;ad<-simulation_adtte(o,cu)
    rd<-normalize_adtte(ad,'OS',cc$origin,6,offset=1,dropout_codes=2,aval_unit=unit)
    check(near(rd$time,back$time),paste('simulation date export unit equivalence:',unit))
  }
  for(method in parameter_catalog()$id) {
    p<-list(median=100,shape=1.2,scale=.8,cure=.3,median2=200,shape2=1.5,mix=.4,rate=.01,rates=c(.01,.02));if(method=='gompertz')p$shape<--.001
    c8<-cc;c8$groups<-list(list(name='A',weight=1,dropout_rate=0,model=parameter_model(method,p,100)));c8$reps<-1
    r8<-run_survival_simulation(c8)
    check(all(r8$observed$time_day>0)&&all(r8$observed$obs_day<=r8$observed$DCO_DAY),paste(method,'complete survival trial generator'))
  }
  source('R/handbook.R')
  text<-'中文$T_i$与$\\lambda$。\n\n$$S(t)=e^{-H(t)}$$\n\n|符号|式子|\n|---|---|\n|$a_i$|$\\sigma$|\n\n`$not_math$`\n\n```r\nx$parameter\n```'
  h<-handbook_markdown_html(text)
  check(grepl('type="math/tex">T_i</script>',h,fixed=TRUE)&&grepl('type="math/tex">\\lambda</script>',h,fixed=TRUE),'adjacent Chinese inline TeX preserves underscores and backslashes')
  check(grepl('mode=display',h,fixed=TRUE)&&grepl('<table>',h,fixed=TRUE)&&grepl('type="math/tex">\\sigma',h,fixed=TRUE),'display and table math survive CommonMark processing')
  check(grepl('<code>$not_math$</code>',h,fixed=TRUE)&&grepl('x$parameter',h,fixed=TRUE),'code dollar signs remain literal code')
  check(!grepl('HANDBOOKMATHTOKEN',handbook_html(),fixed=TRUE)&&grepl('hb-simulation',handbook_standalone_html(),fixed=TRUE),'full handbook and HTML download have resolved formula tokens and simulation chapter')
  empirical<-cc;empirical$n<-5000;empirical$reps<-1;empirical$enroll_mode<-'schedule';empirical$entry_days<-rep(0,5000);empirical$cuts<-150;empirical$groups<-list(list(name='A',weight=2,dropout_rate=0,model=m),list(name='B',weight=1,dropout_rate=0,model=m))
  er<-run_survival_simulation(empirical);p0<-1-exp(-log(2)*150/100)
  check(abs(sum(er$observed$event)-5000*p0)<4*sqrt(5000*p0*(1-p0)),'full generated trial event count agrees with independent exponential probability')
  check(abs(sum(er$truth$group=='A')-5000*2/3)<4*sqrt(5000*2/9),'simple random allocation agrees with configured probability')
  bad<-cc;bad$n<-5000;bad$reps<-200;bad$cuts<-seq(10,200,10)
  check(fails(validate_simulation_config(bad)),'design storage limit rejects oversized patient by trial by cut combinations')
  # Actual session initialization and run path, including independent units.
  shiny::testServer(e$server,{
    session$setInputs(sim_unit='months',sim_groups=1,sim_n=20,sim_reps=2,sim_seed=123,sim_origin='2025-01-01',sim_endpoint='PFS',sim_enroll_mode='constant',sim_enroll_rate=10,sim_enroll_cuts='3,6',sim_enroll_rates='9,18,12',sim_schedule='',sim_cut_mode='fixed',sim_cuts='12,24',sim_target=5,sim_max=36,sim_fixed='6,12',s1_name='A',s1_weight=1,s1_design_model='exponential',s1_exp_input='median',s1_median=12,s1_dropout_rate=0,s1_drop_input='rate')
    session$setInputs(sim_run=1)
    check(grepl('0.30.0',output$sim_status$html,fixed=TRUE),'Shiny design generation succeeds and stores result snapshot')
    session$setInputs(sim_trial='1',sim_view_cut='2');check(grepl('data',output$sim_km,fixed=TRUE),'Shiny KM chart renders selected simulation without module scope errors');check(grepl('行政删失',output$sim_export_note$html,fixed=TRUE),'Shiny chosen trial exposes date export rules')
  })
})
