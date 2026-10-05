local({
 e<-new.env();sys.source('app.R',envir=e)
 for(f in c('conditional_simulation','conditional_prediction','study','conditional_prediction_server'))source(paste0('R/',f,'.R'),local=TRUE)
 x<-validate_data(demo_data(n=120),540);x$group<-rep(c('A','B'),60)
 g<-function(name)list(name=name,model=parameter_model('exponential',list(median=200)),fit_method='exponential',cuts=numeric(),tail_rate=.002,future_n=0,enroll_mode='constant',enroll_rate=.5,enroll_cuts=numeric(),enroll_rates=numeric(),dropout_rate=0,multiplier=1)
 c0<-list(purpose='rejection',prior_shape=.5,prior_rate=50,cut=540,origin='2025-01-01',paramcd='OS',source='test',display_unit='days',groups=list(g('A'),g('B')),model_mode='manual',uncertainty='plugin',cut_mode='fixed',cuts=900,target=1,max_day=900,fixed_times=1,reps=30,seed=81,q_values=c(.75,1,1.25),prediction_design='fixed',prediction_control='A',prediction_sided='two',prediction_alpha=.05,prediction_miss='no_reject')
 r<-run_conditional_prediction(x,c0)
 check(nrow(r$decisions)==90&&all(r$decisions$decision_valid),'conditional prediction retains every requested trial and q scenario')
 check(identical(r$decisions,run_conditional_prediction(x,c0)$decisions)&&identical(r$looks,run_conditional_prediction(x,c0)$looks),'patient-level rejection prediction is reproducible')
 check(is.null(r$truth)&&!nrow(r$summary)&&!nrow(r$fixed),'prediction result omits latent truth and unrelated survival-estimate summaries')
 for(status in c('event','dropout')) {
   known<-x[x$status==status,];y<-r$observed[r$observed$USUBJID %in% known$id,];ix<-match(y$USUBJID,known$id)
   check(all(y$status==status)&&near(y$time_day,known$time[ix]),paste('rejection prediction preserves original IA',status,'histories'))
 }
 a<-r$observed[r$observed$USUBJID %in% x$id[x$status=='active'],];ix<-match(a$USUBJID,x$id)
 check(all(a$time_day>=x$time[ix]),'future observed follow-up never precedes original IA active ages')
 for(b in 1:3) {
   o<-r$observed[r$observed$SIMID==b&r$observed$scenario_q==1,];fit<-survival::survdiff(survival::Surv(time_day,event)~group,data=o,rho=0)
   z<-r$looks[r$looks$SIMID==b&r$looks$scenario_q==1,]
   check(near(z$nominal_p,pchisq(fit$chisq,1,lower.tail=FALSE),tol=1e-6),'predicted final log-rank matches independent survival software')
 }
 reverse<-c0;reverse$prediction_control<-'B';rr<-run_conditional_prediction(x,reverse)
 check(identical(r$decisions$decision_reject,rr$decisions$decision_reject)&&near(r$looks$benefit_z,-rr$looks$benefit_z),'two-sided rejection and sign preserve control/treatment reversal')
 ct<-c0;ct$q_values<-1;ct$cut_mode<-'target';ct$target<-sum(x$event)+20
 rt<-run_conditional_prediction(x,ct)
 check(all(rt$decisions$target_reached)&&all(rt$decisions$events==ct$target),'conditional final event target uses total historical plus future observed events')
 nr<-ct;nr$target<-9999;un<-run_conditional_prediction(x,nr)
 check(all(un$decisions$decision_valid)&&all(!un$decisions$decision_reject)&&all(!un$looks$performed)&&all(un$decisions$DCO_DAY==nr$max_day),'unreachable event target closes without extra test under prespecified no-reject rule')
 nr$prediction_miss<-'analyze';at<-run_conditional_prediction(x,nr)
 check(all(at$looks$performed)&&all(at$decisions$decision_valid),'single Final alternative window rule performs exactly one prespecified test')
 for(mode in c('plugin','gamma','bootstrap')) {
   z<-ct;z$model_mode<-'fit';z$uncertainty<-mode;z$reps<-20;s<-run_conditional_prediction(x,z)
   check(nrow(s$decisions)==20&&identical(s$baseline$observed,r$baseline$observed),paste(mode,'parameter handling retains the original IA risk set and denominator'))
 }
 # A low-event bootstrap can fail in more than 10%; all requested decisions remain visible.
 small<-data.frame(id=paste0('s',1:16),entry=0,time=rep(c(10,15,20,25,30,30,30,30),2),obs_day=rep(c(10,15,20,25,30,30,30,30),2),status=rep(c(rep('event',4),rep('active',4)),2),group=rep(c('A','B'),each=8))
 boot<-c0;boot$cut<-30;boot$cuts<-60;boot$max_day<-60;boot$model_mode<-'fit';boot$uncertainty<-'bootstrap';boot$q_values<-1;boot$reps<-100
 br<-run_conditional_prediction(small,boot);ov<-conditional_prediction_overview(br)
 check(ov$invalid>0&&nrow(br$decisions)==100&&near(ov$failure_bound_upper-ov$failure_bound_lower,ov$invalid/100),'bootstrap failure probability bounds retain every requested patient prediction')
 original_truth<-conditional_truth;conditional_truth<-function(...)stop('injected generation failure')
 emptyc<-c0;emptyc$reps<-3;emptyc$q_values<-1;allfailed<-run_conditional_prediction(x,emptyc);conditional_truth<-original_truth;emptyov<-conditional_prediction_overview(allfailed)
 check(emptyov$valid==0&&emptyov$failure_bound_lower==0&&emptyov$failure_bound_upper==1&&is.na(emptyov$rejection_conditional)&&nrow(allfailed$observed)==0&&all(c('USUBJID','time_day','status') %in% names(allfailed$observed)),'all failed future draws retain full probability bounds and typed empty observation schema')
 seqc<-ct;seqc$target<-2*sum(x$event);seqc$prediction_design<-'sequential';seqc$prediction_timing<-c(.5,.75,1);seqc$prediction_spending<-'asOF';seqc$prediction_futility<-'none';seqc$prediction_futility_z<-numeric()
 sr<-run_conditional_prediction(x,seqc)
 check(all(sr$looks$benefit_z[sr$looks$look==1]==sr$ia_decision$benefit_z)&&all(sr$looks$events[sr$looks$look==1]==sum(x$event)),'all conditional paths start with exactly the actual observed IA statistic')
 check(all(vapply(split(sr$looks,paste(sr$looks$SIMID,sr$looks$scenario_q)),function(d)all(head(d$action,-1)=='continue')&&tail(d$action,1)!='continue',logical(1))),'conditional sequential paths never analyze after stopping')
 check(all(sr$decisions$decision_reject==startsWith(sr$decisions$stop_reason,'efficacy')),'patient prediction uses whole-path boundary crossing rather than terminal nominal p')
 stopc<-seqc;stopc$prediction_sided<-'benefit';stopc$prediction_futility<-'z';w<-sr$ia_decision$benefit_z;stopc$prediction_futility_z<-c(w+.01,-1)
 if(w<gs_plan(list(gs_timing=stopc$prediction_timing,gs_spending='asOF',gs_futility='none',sided='benefit',alpha=.05),stopc$target)$upper_z[1]) {
   st<-run_conditional_prediction(x,stopc)
   check(all(st$decisions$stop_reason=='futility')&&all(st$decisions$future_analysis_count==0)&&!length(st$models)&&conditional_prediction_overview(st)$rejection_conditional==0,'already stopped IA yields decided zero without model fitting or future tests')
 }
 tmp<-tempfile();writeLines(conditional_prediction_script(sr),tmp);re<-new.env(parent=environment());sys.source(tmp,re)
 check(identical(sr$decisions,re$r$decisions)&&identical(sr$looks,re$r$looks),'lossless actual-IA script reproduces all prediction decisions and look records')
 unlink(c(tmp,'prediction_trials.csv','prediction_looks.csv','prediction_summary.csv'))
 for(change in list(list(prediction_control='unknown'),list(cuts=c(800,900)),list(prediction_alpha=0),list(prediction_design='unknown')))check(fails(run_conditional_prediction(x,modifyList(c0,change))),'unsupported prediction group/alpha/multiple-final selection rejected')
 bad<-seqc;bad$prediction_timing<-c(.4,1);check(fails(run_conditional_prediction(x,bad)),'sequential prediction rejects IA events inconsistent with the original first look')
 bad<-x;bad$group<-'pooled';check(fails(run_conditional_prediction(bad,c0)),'pooled IA data cannot produce a two-group treatment rejection probability')
 bad<-x;bad$time<-bad$time+1;bad$day_offset<-1;check(fails(run_conditional_prediction(bad,c0)),'patient prediction rejects nonstandardized first-day offsets even before a decided IA')
 inp<-conditional_prediction_input(list(ci_purpose='rejection',ci_pr_design='fixed',ci_pr_control='A',ci_pr_timing='invalid',ci_pr_futility_z='invalid'),c0,c('A','B'))
 check(inp$prediction_design=='fixed','fixed prediction ignores hidden sequential timing and futility fields')
 # Independently derived conditional exponential count probability.
 aa<-data.frame(id=paste0('a',1:200),entry=0,time=100,obs_day=100,status='active',group=rep(c('A','B'),each=100))
 ac<-c0;ac$cut<-100;ac$cuts<-200;ac$max_day<-200;ac$q_values<-1;ac$reps<-300
 ar<-run_conditional_prediction(aa,ac);p<-1-exp(-log(2)*100/200);N<-200*300
 check(abs(sum(ar$decisions$events)-N*p)<4*sqrt(N*p*(1-p)),'prediction future event count matches independent exponential conditional survival formula')
 # Posterior-predictive mean and variance: shared Gamma hazard induces within-replicate dependence.
 gc<-c0;gc$model_mode<-'fit';gc$uncertainty<-'gamma';gc$q_values<-1;gc$reps<-500
 gr<-run_conditional_prediction(x,gc);h<-gc$max_day-gc$cut;expected<-sum(x$event);variance<-0
 for(name in unique(x$group)) {
   dd<-x[x$group==name,];a<-gc$prior_shape+sum(dd$event);bb<-gc$prior_rate+sum(dd$time);n<-sum(dd$status=='active');L1<-(bb/(bb+h))^a;L2<-(bb/(bb+2*h))^a;p<-1-L1
   expected<-expected+n*p;variance<-variance+n*p*(1-p)+n*(n-1)*(L2-L1^2)
 }
 observed<-mean(gr$decisions$events);mcse<-sqrt(variance/gc$reps)
 check(abs(observed-expected)<4*mcse,'Gamma posterior predictive events match independent integrated mean and shared-parameter variance within 4 MCSE')
 write.csv(data.frame(method=c('fixed exponential','Gamma integrated'),expected=c(200*(1-exp(-log(2)*100/200)),expected),observed=c(mean(ar$decisions$events),observed),mcse=c(sqrt(200*(1-exp(-log(2)*100/200))*exp(-log(2)*100/200)/300),mcse),reps=c(300,500)),'validation/conditional_prediction_calibration.csv',row.names=FALSE)
 shiny::testServer(e$server,{
   session$setInputs(task='conditional',ci_purpose='rejection',ci_source='demo',ci_unit='months',ci_grouped='pooled',ci_model_mode='fit',ci_uncertainty='plugin',ci_process='fixed',ci_cut_mode='fixed',ci_cuts='30',ci_fixed='invalid',ci_reps=3,ci_seed=31,ci_scenarios=FALSE,ci_pr_design='fixed',ci_pr_control='A',ci_pr_sided='two',ci_pr_alpha=.05,c1_fit_method='exponential',c2_fit_method='exponential',c1_future_n=0,c2_future_n=0,c1_dropout_rate=0,c2_dropout_rate=0,c1_drop_input='rate',c2_drop_input='rate',c1_multiplier=1,c2_multiplier=1)
   session$setInputs(ci_run=1,ci_trial='1',ci_view_cut='1',ci_view_q='1')
   check(output$ci_result_purpose=='rejection'&&nzchar(output$ci_prediction_overview)&&nzchar(output$ci_prediction_path),'Shiny two-group IA prediction uses original groups and ignores hidden invalid fixed times')
 })
})
