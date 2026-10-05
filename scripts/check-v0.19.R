local({
 source('R/sequential_inference.R')
 cfg<-function(side='two',spend='asOF',timing=c(.5,1),p=.5,fut='none',z=rep(0,length(timing)-1))list(design_mode='sequential',alpha=if(side=='two').05 else .025,sided=side,gs_spending=spend,gs_futility=fut,gs_futility_z=z,gs_timing=timing,treatment_fraction=p)
 path<-function(c,p,z) {k<-length(z);data.frame(SCENARIO=1L,SIMID=1L,look=seq_len(k),events=p$target_events[seq_len(k)],performed=TRUE,valid=TRUE,target_reached=TRUE,benefit_z=z,information_fraction=p$information_fraction[seq_len(k)],upper_z=p$upper_z[seq_len(k)],lower_efficacy_z=p$lower_efficacy_z[seq_len(k)],futility_z=p$futility_z[seq_len(k)],action=vapply(seq_len(k),function(j)gs_action(z[j],TRUE,j,p,c$sided),character(1)))}
 # Independent conditional-normal integration (no rpact probability API).
 quad<-function(delta,I,l,u,w) {
  mu<-delta*sqrt(I);rho<-sqrt(I[1]/I[2]);sd<-sqrt(1-rho^2)
  above<-integrate(function(x)dnorm(x-mu[1])*pnorm((mu[2]+rho*(x-mu[1])-w)/sd),l[1],u[1],rel.tol=1e-11)$value
  below<-integrate(function(x)dnorm(x-mu[1])*pnorm((w-mu[2]-rho*(x-mu[1]))/sd),l[1],u[1],rel.tol=1e-11)$value
  c(upper=pnorm(mu[1]-u[1])+above,lower=pnorm(l[1]-mu[1])+below)
 }
 for(side in c('two','benefit')) {
  c<-cfg(side,fut=if(side=='two')'none' else 'z');p<-gs_plan(c,100);I<-p$target_events*.25;l<-if(side=='two')p$lower_efficacy_z else p$futility_z;u<-p$upper_z
  for(delta in c(-.4,0,.4))for(w in c(-2.3,.1,2.3))check(near(gs_ordering_tails(delta,I,l,u,w),quad(delta,I,l,u,w),1e-7),paste('signed tails match independent 1D integral',side,delta,w))
  z<-if(side=='two')c(.1,-2.3) else c(.5,2.3);r<-gs_inference(c,p,path(c,p,z),2);f<-r$final
  check(f$available&&is.finite(f$adjusted_p)&&f$root_probability_error<1e-7,paste('terminal inference finite with root residual',side))
  check(near(quad(-log(f$median_unbiased_hr),I,l,u,z[2])[1],.5,1e-7),paste('median HR verified by independent quadrature',side))
  check(near(quad(-log(f$adjusted_hr_lower),I,l,u,z[2])[1],.975,1e-7)&&near(quad(-log(f$adjusted_hr_upper),I,l,u,z[2])[1],.025,1e-7),paste('CI endpoints independently inverted',side))
  check(near(f$upper_tail_null+f$lower_tail_null,1,1e-7),paste('null signed tails exhaust probability',side))
  if(side=='two') {
   o<-gs_inference(c,p,path(c,p,-z),2)$final
   check(near(f$adjusted_p,o$adjusted_p,1e-7)&&near(f$median_unbiased_hr,1/o$median_unbiased_hr,1e-7)&&near(f$adjusted_hr_lower,1/o$adjusted_hr_upper,1e-7),'direction reversal gives reciprocal HR and identical two-sided p')
   er<-gs_inference(c,p,path(c,p,-4),1)$final
   check(er$action=='efficacy_reverse'&&er$median_unbiased_hr>1&&near(er$adjusted_p,2*pnorm(-4),1e-7),'early reverse stop preserves sign rather than using absolute Z')
  } else {
   e<-gs_inference(c,p,path(c,p,-.1),1)$final
   check(e$action=='futility'&&near(e$adjusted_p,pnorm(.1),1e-7)&&near(e$median_unbiased_hr,exp(.1/sqrt(I[1])),1e-7),'first-stage futile stop reduces to directional normal inference')
   old<-c;old$gs_futility_z<-NULL;check(identical(gs_inference(old,p,path(c,p,z),2),r),'old frozen futility exports recover original values from saved plan')
  }
  bad<-p;bad$lower_efficacy_z[1]<-0;check(fails(gs_inference(c,bad,path(c,p,z),2)),paste('tampered reverse boundary rejected',side))
  bad<-p;bad$futility_z[1]<-1;check(fails(gs_inference(c,bad,path(c,p,z),2)),paste('tampered futility boundary rejected',side))
  bad<-path(c,p,z);bad$look[1]<-1.1;check(fails(gs_inference(c,p,bad,2)),paste('fractional look rejected',side))
  rc<-c;rc$future_hr<-100;rc$true_hr<-.01;rc$display_unit<-'weeks';check(identical(gs_inference(rc,p,path(c,p,z),2),r),paste('unobserved truth future HR and display units excluded',side))
  set.seed(191);seed<-.Random.seed;invisible(gs_inference(c,p,path(c,p,z),2));check(identical(seed,.Random.seed),paste('new inference preserves RNG',side))
  tf<-tempfile();writeLines(gs_inference_script(r),tf);env<-new.env(parent=globalenv());sys.source(tf,env);unlink(tf);unlink(c('sequential_repeated_replayed.csv','sequential_final_replayed.csv'));check(identical(env$inference,r),paste('new inference script exactly reproduces',side))
 }
 # Independent multivariate rectangle integration for three and five analyses.
 rect<-function(delta,I,l,u,w) {
  k<-length(I);mu<-delta*sqrt(I);C<-outer(I,I,function(a,b)sqrt(pmin(a,b)/pmax(a,b)))
  # Truncate at 12 marginal SDs: omitted mass <= 2*j*Phi(-12), below 1e-31.
  prob<-function(a,b,j)as.numeric(mvtnorm::pmvnorm(pmax(a,mu[seq_len(j)]-12),pmin(b,mu[seq_len(j)]+12),mean=mu[seq_len(j)],sigma=C[seq_len(j),seq_len(j),drop=FALSE],algorithm=mvtnorm::Miwa(steps=512)))
  high<-low<-0
  for(j in seq_len(k-1)) {high<-high+prob(c(head(l,j-1),u[j]),c(head(u,j-1),Inf),j);low<-low+prob(c(head(l,j-1),-Inf),c(head(u,j-1),l[j]),j)}
  c(upper=high+prob(c(head(l,k-1),w),c(head(u,k-1),Inf),k),lower=low+prob(c(head(l,k-1),-Inf),c(head(u,k-1),w),k))
 }
 for(k in c(3,5))for(side in c('two','benefit')) {
  c<-cfg(side,'asP',seq(1/k,1,length.out=k),.4,if(side=='two')'none' else 'z',rep(-.5,k-1));p<-gs_plan(c,203);I<-p$target_events*.24;l<-if(side=='two')p$lower_efficacy_z else p$futility_z
  for(delta in c(0,.15))check(near(gs_ordering_tails(delta,I,l,p$upper_z,1.7),rect(delta,I,l,p$upper_z,1.7),2e-5),paste('independent Miwa rectangles match',k,side,delta))
 }
 c<-cfg('benefit',fut='z',z=2.7);p<-gs_plan(c,100);r<-gs_inference(c,p,path(c,p,2.4),1)
 check(r$final$action=='futility'&&r$final$adjusted_p<.025&&!r$final$p_agrees_original_decision,'nonbinding futility p does not overwrite original no-rejection decision')
 p2<-gs_plan(c,200);r2<-gs_inference(c,p2,path(c,p2,2.4),1)
 check(near(r$final$adjusted_p,r2$final$adjusted_p,1e-7),'stage-one null p independent of later information')
 c<-cfg('two');p<-gs_plan(c,100);d<-path(c,p,c(.2,-2.3));r<-gs_inference(c,p,d,1);d$benefit_z[2]<-NA;d$valid[2]<-FALSE;check(identical(gs_inference(c,p,d,1),r)&&!r$final$available,'two-sided ongoing prefix excludes all later observations')
 check(fails(gs_ordering_inference(c(10,20),c(-Inf,-Inf),c(3,2),1,NA,'two'))&&fails(gs_ordering_inference(c(10,20),c(-Inf,-Inf),c(3,2),1,.05,'unknown')),'invalid inference alpha and direction rejected')
 check(fails(gs_ordering_tails(0,c(10,10),c(-Inf,-Inf),c(3,2),1))&&fails(gs_ordering_tails(0,10,3,2,1)),'invalid information and interval boundaries rejected')
})
