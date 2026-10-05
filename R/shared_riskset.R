# Conditional event-mark likelihood martingale. Within each EXACT common-entry
# cohort, risk ages agree, so the unknown cohort baseline cancels. No rounding
# of entry ages and no Breslow approximation for tied failures is permitted.
sp_risksets<-function(o,cfg){
 if(!all(c("entry_day","obs_day","event","group","status") %in% names(o)))stop("风险集检验需要入组/精确观察时间/状态/组别。")
 parts<-list()
 for(entry in unique(o$entry_day)){
   x<-o[o$entry_day==entry,,drop=FALSE];events<-which(x$event==1L)
   if(anyDuplicated(x$obs_day[events]))stop("同入组队列存在并列事件，当前精确事件标记检验不作并列近似。")
   for(i in events[order(x$obs_day[events])]){
     t<-x$obs_day[i]
     if(any(x$status=="dropout"&x$obs_day==t))stop("事件与永久退出同刻，需精确先后信息。")
     risk<-x$obs_day>=t;rc<-sum(risk&x$group==cfg$control);rt<-sum(risk&x$group!=cfg$control)
     parts[[length(parts)+1L]]<-data.frame(entry_day=entry,event_day=t,R_CONTROL=rc,R_TREATMENT=rt,X_TREATMENT=as.integer(x$group[i]!=cfg$control))
   }
 }
 d<-batch_bind_columns(parts);if(nrow(d))d<-d[order(d$event_day,d$entry_day),,drop=FALSE]
 d
}
sp_mark_statistics<-function(o,cfg){r<-sp_risksets(o,cfg)
 informative<-if(nrow(r))r$R_CONTROL>0&r$R_TREATMENT>0 else logical()
 list(events=sum(o$event[o$group!=cfg$control]),exposure=NA_real_,risksets=r[informative,,drop=FALSE],all_risksets=r,informative_events=sum(informative))
}
sp_mark_loglike<-function(theta,r){
 if(!nrow(r))return(0)
 # Remove only singleton-group factors: they are identically one at every HR.
 offset<-log(r$R_TREATMENT)-log(r$R_CONTROL)
 z<-theta+offset;softplus<-pmax(z,0)+log1p(exp(-abs(z)))
 sum(r$X_TREATMENT*theta-log(r$R_CONTROL)-softplus)
}
sp_mark_mixture<-function(stat,grid,weights){g<-sp_grid(grid,weights);ll<-vapply(log(g$values),sp_mark_loglike,numeric(1),r=stat$risksets);sp_logsum(log(g$weights)+ll)}
sp_mark_evidence<-function(stat,cfg){
 loge<-sp_mark_mixture(stat,cfg$test_grid,cfg$test_weights)-sp_mark_loglike(0,stat$risksets)
 list(log_e=loge,e_value=if(loge>700)Inf else exp(loge),e_p=min(1,exp(-loge)),conditional_error_bound=min(1,exp(log(cfg$alpha)+loge)),n_treatment_events=stat$events,baseline_exposure=NA_real_,informative_events=stat$informative_events)
}
sp_mark_confidence<-function(stat,cfg){
 r<-stat$risksets;level<-1-cfg$interval_alpha
 empty<-function(mle,lo=0,hi=Inf,is_empty=FALSE)list(hr_mle=mle,cs_lower=lo,cs_upper=hi,cs_empty=is_empty,interval_level=level,confidence_engine="common_entry_cohort_mark_mixture_CS")
 if(!nrow(r))return(empty(NA_real_))
 n<-nrow(r);k<-sum(r$X_TREATMENT);offset<-log(r$R_TREATMENT)-log(r$R_CONTROL)
 score<-function(theta)k-sum(plogis(theta+offset));mle_theta<-if(k==0)-Inf else if(k==n)Inf else uniroot(score,c(-200,200),tol=1e-10)$root
 mle<-exp(mle_theta);center<-if(is.finite(mle_theta))mle_theta else if(k==0)-200 else 200
 mix<-sp_mark_mixture(stat,cfg$ci_grid,cfg$ci_weights);f<-function(theta)mix-sp_mark_loglike(theta,r)-log(1/cfg$interval_alpha)
 if(f(center)>0)return(empty(mle,NA_real_,NA_real_,TRUE))
 lo<-if(f(-200)<=0)0 else exp(uniroot(f,c(-200,center),tol=1e-9)$root)
 hi<-if(f(200)<=0)Inf else exp(uniroot(f,c(center,200),tol=1e-9)$root)
 empty(mle,lo,hi)
}
