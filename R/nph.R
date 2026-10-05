# Piecewise hazard-ratio transformation on individual follow-up time.
study_is_ph <- function(cfg) identical(cfg$effect_mode %||% "ph","ph")
validate_hr_profile <- function(cuts,hr) {
  if(!is.numeric(cuts)||any(!is.finite(cuts)|cuts<=0)||is.unsorted(cuts,strictly=TRUE)||length(cuts)>20||
     !is.numeric(hr)||length(hr)!=length(cuts)+1||any(!is.finite(hr)|hr<.01|hr>100))stop("HR切点需0–20个正递增随访时间；HR数量=切点数+1，值在0.01–100。")
  invisible(TRUE)
}
nph_base_hazard <- function(model,t) {
  out<-model_cumhaz(model,t)
  if(model$id=="pwe"&&any(is.infinite(t)))out[is.infinite(t)]<-if(tail(model$params,1)>0)Inf else sum(head(model$params,-1)*diff(c(0,model$cuts)))
  out
}
nph_cumhaz <- function(model,t,cuts,hr) {
  validate_hr_profile(cuts,hr);starts<-c(0,cuts);ends<-c(cuts,Inf);out<-rep(0,length(t))
  for(j in seq_along(hr)) {
    h0<-nph_base_hazard(model,starts[j]);delta<-nph_base_hazard(model,pmax(starts[j],pmin(t,ends[j])))-h0
    if(is.infinite(h0))delta[]<-0
    out<-out+hr[j]*delta
  }
  out
}
nph_inverse <- function(model,z,cuts,hr) {
  validate_hr_profile(cuts,hr);starts<-c(0,cuts);hc<-nph_base_hazard(model,starts);inc<-diff(hc);inc[is.nan(inc)&is.infinite(head(hc,-1))]<-0
  weighted<-c(0,cumsum(hr[seq_along(cuts)]*inc));j<-findInterval(z,weighted)
  inverse_cumhaz(model,hc[j]+(z-weighted[j])/hr[j])
}
study_profile <- function(cfg,cell) {
  if(study_is_ph(cfg))return(list(cuts=numeric(),hr=cell$hr))
  if(identical(as.integer(cell$profile_id),0L))return(list(cuts=numeric(),hr=1))
  list(cuts=cfg$hr_cuts,hr=cfg$hr_profile)
}
study_effect_label <- function(cfg,cell) {
  if(study_is_ph(cfg))paste0("HR=",cell$hr) else if(cell$profile_id==0)"HR(t)=1" else paste0("HR(t)=",paste(cfg$hr_profile,collapse="→"))
}
study_true_estimand <- function(cfg,cell) {
  pr<-study_profile(cfg,cell);tau<-cfg$analysis_tau
  if(all(pr$hr==1))return(0)
  if(cfg$primary=="survival")return(exp(-nph_cumhaz(cfg$control_model,tau,pr$cuts,pr$hr))-model_survival(cfg$control_model,tau))
  # Split quadrature at both HR and baseline hazard knots.
  knots<-sort(unique(c(0,tau,pr$cuts[pr$cuts<tau],cfg$control_model$cuts[cfg$control_model$cuts<tau])))
  sum(vapply(seq_len(length(knots)-1),function(j)integrate(function(t)exp(-nph_cumhaz(cfg$control_model,t,pr$cuts,pr$hr))-model_survival(cfg$control_model,t),knots[j],knots[j+1],rel.tol=1e-9,abs.tol=1e-9,subdivisions=300)$value,numeric(1)))
}
study_risk_sets <- function(time,event,arm=rep(0,length(time))) {
  if(length(time)!=length(event)||length(time)!=length(arm)||any(!is.finite(time)|time<0)||any(!event %in% c(0,1))||any(!arm %in% c(0,1)))stop("无效的右删失观察记录。")
  times<-sort(unique(time));ix<-match(time,times);k<-length(times)
  n<-tabulate(ix,nbins=k);n1<-tabulate(ix[arm==1],nbins=k);y<-rev(cumsum(rev(n)));y1<-rev(cumsum(rev(n1)))
  d<-tabulate(ix[event==1],nbins=k);d1<-tabulate(ix[event==1&arm==1],nbins=k)
  s<-cumprod(1-d/y);before<-head(c(1,s),-1)
  data.frame(time=times,y=y,y1=y1,y0=y-y1,d=d,d1=d1,s=s,before=before)
}
study_fh <- function(time,event,arm,rho=0,gamma=0) {
  if(any(!is.finite(c(rho,gamma)))||rho<0||gamma<0||rho>5||gamma>5)stop("FH rho、gamma需在0–5。")
  r<-study_risk_sets(time,event,arm);w<-r$before^rho*(1-r$before)^gamma;keep<-r$d>0&r$y>1
  list(score=sum(w*(r$d1-r$d*r$y1/r$y)),variance=sum(w[keep]^2*r$y1[keep]*r$y0[keep]*r$d[keep]*(r$y[keep]-r$d[keep])/(r$y[keep]^2*(r$y[keep]-1))),risk=r,weights=w)
}
study_km_functional <- function(time,event,tau,kind=c("rmst","survival")) {
  kind<-match.arg(kind)
  if(!length(time))stop("该组没有观察记录。")
  if(length(tau)!=1||!is.finite(tau)||tau<=0)stop("预设随访时点tau需为正。")
  r<-study_risk_sets(time,event);last_surv<-tail(r$s,1)
  if(tau>max(time)&&last_surv>0)stop("tau超过该组观察支持且KM未降至0；不外推。")
  q<-r[r$time<=tau,,drop=FALSE];green<-rep(0,nrow(q));ok<-q$y>q$d;green[ok]<-q$d[ok]/(q$y[ok]*(q$y[ok]-q$d[ok]))
  if(kind=="survival") {
    value<-if(nrow(q))tail(q$s,1) else 1;var<-if(value==0)0 else value^2*sum(green)
  } else {
    starts<-c(0,q$time);ends<-c(q$time,tau);areas<-(ends-starts)*c(1,q$s);value<-sum(areas)
    # Area under KM from each event time to tau multiplies its Greenwood increment.
    remaining<-if(nrow(q))rev(cumsum(rev(areas[-1]))) else numeric();var<-sum(remaining^2*green)
  }
  list(estimate=value,variance=var,n=length(time),n_risk_tau=sum(time>=tau),events=sum(event),tail_zero=last_surv==0)
}
study_contrast <- function(observed,cfg) {
  kind<-cfg$primary
  a<-study_km_functional(observed$time_day[observed$group=="Control"],observed$event[observed$group=="Control"],cfg$analysis_tau,kind)
  b<-study_km_functional(observed$time_day[observed$group=="Treatment"],observed$event[observed$group=="Treatment"],cfg$analysis_tau,kind)
  diff<-b$estimate-a$estimate;se<-sqrt(a$variance+b$variance)
  if(!is.finite(se)||se<=0)stop("差值方差为0或无效，Wald检验不可计算。")
  z<-diff/se;p<-if(cfg$sided=="two")2*pnorm(-abs(z)) else pnorm(z,lower.tail=FALSE)
  list(control=a$estimate,treatment=b$estimate,difference=diff,se=se,z=z,p=p,reject=p<=cfg$alpha,lower=diff-qnorm(.975)*se,upper=diff+qnorm(.975)*se,n_risk_control=a$n_risk_tau,n_risk_treatment=b$n_risk_tau)
}
study_extended_analysis <- function(observed,cfg,true_effect=NA_real_) {
  out<-list(fh_valid=FALSE,fh_z=NA_real_,fh_p=NA_real_,fh_reject=NA,fh_note="",contrast_valid=FALSE,contrast_control=NA_real_,contrast_treatment=NA_real_,contrast_estimate=NA_real_,contrast_se=NA_real_,contrast_z=NA_real_,contrast_p=NA_real_,contrast_reject=NA,contrast_lower=NA_real_,contrast_upper=NA_real_,contrast_covered=NA,n_risk_control_tau=NA_integer_,n_risk_treatment_tau=NA_integer_,contrast_note="")
  if(cfg$primary=="fh") {
    tryCatch({a<-study_fh(observed$time_day,observed$event,as.integer(observed$group=="Treatment"),cfg$fh_rho,cfg$fh_gamma)
      if(!is.finite(a$variance)||a$variance<=0)stop("加权log-rank方差为0或无效。")
      z<-a$score/sqrt(a$variance);p<-if(cfg$sided=="two")2*pnorm(-abs(z)) else pnorm(z)
      out$fh_valid<-TRUE;out$fh_z<-z;out$fh_p<-p;out$fh_reject<-p<=cfg$alpha
    },error=function(e)out$fh_note<<-conditionMessage(e))
  }
  if(cfg$primary %in% c("rmst","survival")) {
    tryCatch({a<-study_contrast(observed,cfg);out$contrast_valid<-TRUE;out$contrast_control<-a$control;out$contrast_treatment<-a$treatment;out$contrast_estimate<-a$difference;out$contrast_se<-a$se;out$contrast_z<-a$z;out$contrast_p<-a$p;out$contrast_reject<-a$reject;out$contrast_lower<-a$lower;out$contrast_upper<-a$upper;out$n_risk_control_tau<-a$n_risk_control;out$n_risk_treatment_tau<-a$n_risk_treatment;out$contrast_covered<-a$lower<=true_effect&&true_effect<=a$upper
    },error=function(e)out$contrast_note<<-conditionMessage(e))
  }
  out
}
study_contrast_overview <- function(result) {
  if(!result$config$primary %in% c("rmst","survival")||!nrow(result$rows))return(data.frame())
  do.call(rbind,lapply(split(result$rows,result$rows$SCENARIO),function(x){ok<-x$contrast_valid;d<-x$contrast_estimate[ok];n<-sum(ok);true<-x$true_effect[1];covered<-x$contrast_covered[ok];ci<-study_wilson(sum(covered),n)
    data.frame(SCENARIO=x$SCENARIO[1],true_effect=true,valid=n,invalid=nrow(x)-n,mean_control=if(n)mean(x$contrast_control[ok]) else NA_real_,mean_treatment=if(n)mean(x$contrast_treatment[ok]) else NA_real_,mean_difference=if(n)mean(d) else NA_real_,bias=if(n)mean(d-true) else NA_real_,rmse=if(n)sqrt(mean((d-true)^2)) else NA_real_,empirical_sd=if(n>1)sd(d) else NA_real_,mean_se=if(n)mean(x$contrast_se[ok]) else NA_real_,coverage=if(n)mean(covered) else NA_real_,coverage_mcse=if(n)sqrt(mean(covered)*(1-mean(covered))/n) else NA_real_,coverage_lower=ci[1],coverage_upper=ci[2],mean_risk_control=if(n)mean(x$n_risk_control_tau[ok]) else NA_real_,mean_risk_treatment=if(n)mean(x$n_risk_treatment_tau[ok]) else NA_real_)
  }))
}
study_primary_label <- function(cfg) switch(cfg$primary,logrank="log-rank",cox="Cox Wald",fh=paste0("FH(",cfg$fh_rho,",",cfg$fh_gamma,")"),rmst="RMST差值 Wald",survival="固定时点生存率差 Wald")
