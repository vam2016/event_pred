# Repeated independent design-stage trials under specified two-arm survival truth.
study_scenarios <- function(cfg) {
  cut_values<-if(cfg$cut_mode=="fixed")cfg$dco_values else cfg$target_values
  base<-if(study_is_ph(cfg))expand.grid(n=cfg$n_values,hr=cfg$hr_values,KEEP.OUT.ATTRS=FALSE) else expand.grid(n=cfg$n_values,profile_id=if(isTRUE(cfg$include_h0))c(1L,0L) else 1L,KEEP.OUT.ATTRS=FALSE)
  if(!study_is_ph(cfg))base$hr<-ifelse(base$profile_id==0|all(cfg$hr_profile==1),1,NA_real_)
  base$BASEID<-seq_len(nrow(base))
  rows<-lapply(seq_len(nrow(base)),function(j)cbind(base[rep(j,length(cut_values)),,drop=FALSE],cut_value=cut_values))
  x<-do.call(rbind,rows);rownames(x)<-NULL;x$SCENARIO<-seq_len(nrow(x));x$hypothesis<-ifelse(!is.na(x$hr)&x$hr==1,"H0","H1")
  if(!study_is_ph(cfg))x$effect_label<-vapply(seq_len(nrow(x)),function(j)study_effect_label(cfg,x[j,]),character(1))
  if(cfg$primary %in% c("rmst","survival")) {
    x$true_effect<-vapply(seq_len(nrow(x)),function(j)study_true_estimand(cfg,x[j,]),numeric(1))
    x$hypothesis<-ifelse(abs(x$true_effect)<=1e-10*max(1,if(cfg$primary=="rmst")cfg$analysis_tau else 1),"H0","H1")
  }
  x
}
study_replicate_seed <- function(seed,base_id,replicate) as.integer((as.double(seed)+base_id*100000+replicate)%%2147483646+1)
validate_study_config <- function(cfg) {
  vec<-function(x,lo,hi,integer=FALSE)is.numeric(x)&&length(x)>0&&all(is.finite(x)&x>=lo&x<=hi)&&!anyDuplicated(x)&&(!integer||all(x==floor(x)))
  if(!vec(cfg$n_values,10,5000,TRUE))stop("N候选值需不重复的10–5000整数。")
  if(study_is_ph(cfg)&&!vec(cfg$hr_values,.01,100))stop("HR需不重复的正数（0.01–100），HR=1为H0。")
  if(!vec(cfg$reps,20,10000,TRUE)||length(cfg$reps)!=1)stop("每情景重复数需20–10000整数。")
  if(!vec(cfg$seed,0,.Machine$integer.max,TRUE)||length(cfg$seed)!=1)stop("种子需0至2147483647整数。")
  if(length(cfg$treatment_fraction)!=1||!vec(cfg$treatment_fraction,.01,.99))stop("试验组分配概率需在0.01–0.99。")
  if(length(cfg$alpha)!=1||!vec(cfg$alpha,.0001,.2))stop("检验alpha需在0.0001–0.2。")
  if(!cfg$primary %in% c("logrank","cox","fh","rmst","survival")||!cfg$sided %in% c("two","benefit"))stop("请选择支持的检验与方向。")
  if(!cfg$miss_policy %in% c("analyze","no_reject"))stop("未知窗口末分析规则。")
  if(!cfg$cut_mode %in% c("fixed","target"))stop("未知截点规则。")
  if(cfg$cut_mode=="fixed"&&!vec(cfg$dco_values,.001,3650))stop("固定DCO需不重复的正研究时间，最长3650日。")
  if(cfg$cut_mode=="target"&&(!vec(cfg$target_values,1,100000,TRUE)||length(cfg$max_day)!=1||!vec(cfg$max_day,.001,3650)))stop("D*需正整数，最大研究窗口需在(0,3650]日。")
  if(!cfg$enroll_mode %in% c("constant","piecewise","schedule"))stop("未知入组过程。")
  if(cfg$enroll_mode=="constant"&&(length(cfg$enroll_rate)!=1||!vec(cfg$enroll_rate,.Machine$double.eps,1e6)))stop("恒定入组率需为正数。")
  if(cfg$enroll_mode=="piecewise")parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts)
  if(cfg$enroll_mode=="schedule"&&(length(cfg$entry_days)!=max(cfg$n_values)||any(!is.finite(cfg$entry_days))||any(cfg$entry_days<0)||is.unsorted(cfg$entry_days)))stop("固定日程需提供最大N个非负非递减时间，各N取前N个时间。")
  if(length(cfg$dropout_rates)!=2||any(!is.finite(cfg$dropout_rates)|cfg$dropout_rates<0))stop("两组独立退出风险率需非负。")
  if(!cfg$control_model$id %in% parameter_catalog()$id)stop("未知对照生成分布。")
  time_factor(cfg$display_unit)
  if(length(cfg$origin)!=1||is.na(as.Date(cfg$origin)))stop("研究起点无效。")
  if(!cfg$paramcd %in% c("PFS","OS"))stop("请选择PFS或OS。")
  if(!study_is_ph(cfg))validate_hr_profile(cfg$hr_cuts,cfg$hr_profile)
  if(cfg$primary=="fh"&&(length(cfg$fh_rho)!=1||length(cfg$fh_gamma)!=1||!vec(cfg$fh_rho,0,5)||!vec(cfg$fh_gamma,0,5)))stop("FH权重参数需在0–5。")
  if(cfg$primary %in% c("rmst","survival")&&(length(cfg$analysis_tau)!=1||!vec(cfg$analysis_tau,.001,3650)))stop("tau需为正个体随访时间，最长3650日。")
  if(study_is_sequential(cfg))validate_sequential_config(cfg)
  sc<-study_scenarios(cfg)
  if(nrow(sc)>60||nrow(sc)*cfg$reps>200000||sum(unique(sc[,c("n","BASEID")])$n)*cfg$reps>3e7)stop("最多60情景、20万次截点分析；生成患者×重复数最多3000万。请减少候选值或重复数。")
  invisible(TRUE)
}
study_trial_config <- function(cfg,cell) {
  pr<-study_profile(cfg,cell)
  list(n=cell$n,reps=1,seed=cfg$seed,origin=cfg$origin,paramcd=cfg$paramcd,display_unit=cfg$display_unit,
    groups=list(list(name="Control",weight=1-cfg$treatment_fraction,dropout_rate=cfg$dropout_rates[1],model=cfg$control_model,hazard_multiplier=1),
                list(name="Treatment",weight=cfg$treatment_fraction,dropout_rate=cfg$dropout_rates[2],model=cfg$control_model,hazard_multiplier=if(study_is_ph(cfg))cell$hr else 1,hazard_profile=if(study_is_ph(cfg))NULL else pr)),
    enroll_mode=cfg$enroll_mode,enroll_rate=cfg$enroll_rate,enroll_cuts=cfg$enroll_cuts,enroll_rates=cfg$enroll_rates,
    entry_days=if(cfg$enroll_mode=="schedule")head(cfg$entry_days,cell$n) else numeric())
}
study_analysis_cut <- function(truth,cfg,cell) {
  if(cfg$cut_mode=="fixed")return(list(day=cell$cut_value,hit=NA,hit_day=NA_real_))
  times<-sort(truth$event_day[is.finite(truth$event_day)&truth$event_time_day<=truth$dropout_time_day&truth$event_day<=cfg$max_day])
  hit<-length(times)>=cell$cut_value;day<-if(hit)times[cell$cut_value] else cfg$max_day
  list(day=day,hit=hit,hit_day=if(hit)day else NA_real_)
}
# Risk sets immediately before each exact follow-up time; hypergeometric ties variance.
study_logrank <- function(time,event,arm) {
  if(length(time)!=length(event)||length(time)!=length(arm)||any(!is.finite(time)|time<0)||any(!event %in% c(0,1))||any(!arm %in% c(0,1)))stop("无效的log-rank观察记录。")
  times<-sort(unique(time));ix<-match(time,times);k<-length(times)
  n<-tabulate(ix,nbins=k);n1<-tabulate(ix[arm==1],nbins=k)
  y<-rev(cumsum(rev(n)));y1<-rev(cumsum(rev(n1)));y0<-y-y1
  d<-tabulate(ix[event==1],nbins=k);d1<-tabulate(ix[event==1&arm==1],nbins=k)
  keep<-d>0&y>1
  list(score=sum(d1-d*y1/y),variance=sum(y1[keep]*y0[keep]*d[keep]*(y[keep]-d[keep])/(y[keep]^2*(y[keep]-1))))
}
# Treatment is coded 1; negative Z/beta means benefit (HR < 1).
analyze_study_trial <- function(observed,cfg,true_hr) {
  x<-observed;x$arm<-as.integer(x$group=="Treatment")
  out<-list(logrank_valid=FALSE,logrank_z=NA_real_,logrank_p=NA_real_,logrank_reject=NA,
    cox_valid=FALSE,beta=NA_real_,se_beta=NA_real_,hr_estimate=NA_real_,hr_lower=NA_real_,hr_upper=NA_real_,cox_p=NA_real_,cox_reject=NA,covered=NA,logrank_note="",cox_note="")
  if(nrow(x)<2||length(unique(x$arm))!=2||sum(x$event)==0) {
    out$logrank_note<-"两组风险集或事件信息不足";out$cox_note<-out$logrank_note;return(out)
  }
  lr<-tryCatch(study_logrank(x$time_day,x$event,x$arm),error=function(e)e)
  if(!inherits(lr,"error")&&is.finite(lr$variance)&&lr$variance>0) {
    z<-lr$score/sqrt(lr$variance);p<-if(cfg$sided=="two")pchisq(z^2,1,lower.tail=FALSE) else pnorm(z)
    out$logrank_valid<-TRUE;out$logrank_z<-z;out$logrank_p<-p;out$logrank_reject<-p<=cfg$alpha
  } else out$logrank_note<-if(inherits(lr,"error"))conditionMessage(lr) else "log-rank方差为0或无效"
  if(isTRUE(cfg$estimate_hr)||cfg$primary=="cox") {
    warnings<-character()
    fit<-tryCatch(withCallingHandlers(survival::coxph(survival::Surv(time_day,event)~arm,data=x,ties="efron",control=survival::coxph.control(iter.max=50,timefix=FALSE)),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)e)
    if(inherits(fit,"error"))out$cox_note<-conditionMessage(fit)
    else {
      beta<-unname(coef(fit)[1]);se<-sqrt(unname(vcov(fit)[1,1]))
      ok<-!length(warnings)&&is.finite(beta)&&is.finite(se)&&se>0&&is.finite(exp(beta))&&fit$iter<50
      if(ok) {
        ci<-beta+c(-1,1)*qnorm(.975)*se;z<-beta/se;p<-if(cfg$sided=="two")2*pnorm(-abs(z)) else pnorm(z)
        out$cox_valid<-TRUE;out$beta<-beta;out$se_beta<-se;out$hr_estimate<-exp(beta);out$hr_lower<-exp(ci[1]);out$hr_upper<-exp(ci[2]);out$cox_p<-p;out$cox_reject<-p<=cfg$alpha;out$covered<-if(is.finite(true_hr))ci[1]<=log(true_hr)&&log(true_hr)<=ci[2] else NA
      } else out$cox_note<-paste(c(warnings,"Cox估计/信息矩阵未通过有限性或收敛检查"),collapse="；")
    }
  }
  out
}
study_trial_row <- function(truth,cfg,cell,b) {
  cut<-study_analysis_cut(truth,cfg,cell);o<-survival_at_cut(truth,cut$day,cell$SCENARIO)
  a<-c(analyze_study_trial(o,cfg,cell$hr),study_extended_analysis(o,cfg,cell$true_effect %||% NA_real_))
  key<-if(cfg$primary %in% c("rmst","survival"))"contrast" else cfg$primary
  valid<-a[[paste0(key,"_valid")]];reject<-a[[paste0(key,"_reject")]]
  miss<-cfg$cut_mode=="target"&&!isTRUE(cut$hit)
  decision_valid<-valid;decision_reject<-reject
  if(miss&&cfg$miss_policy=="no_reject"){decision_valid<-TRUE;decision_reject<-FALSE}
  row<-cbind(data.frame(SCENARIO=cell$SCENARIO,BASEID=cell$BASEID,SIMID=b,replicate_seed=study_replicate_seed(cfg$seed,cell$BASEID,b),n=cell$n,true_hr=cell$hr,hypothesis=cell$hypothesis,cut_value=cell$cut_value,
    DCO_DAY=cut$day,n_observed=nrow(o),n_control=sum(o$group=="Control"),n_treatment=sum(o$group=="Treatment"),events=sum(o$event),target_reached=cut$hit,target_day=cut$hit_day,
    generated=TRUE,decision_valid=decision_valid,decision_reject=decision_reject,generation_note=""),as.data.frame(a))
  if(!study_is_ph(cfg))row$effect_label<-cell$effect_label
  if(cfg$primary %in% c("rmst","survival"))row$true_effect<-cell$true_effect
  row
}
study_failed_rows <- function(cells,b,cfg,message) {
  rows<-lapply(seq_len(nrow(cells)),function(j){cell<-cells[j,];empty<-data.frame(group=character(),time_day=numeric(),event=integer());a<-c(analyze_study_trial(empty,cfg,cell$hr),study_extended_analysis(empty,cfg,cell$true_effect %||% NA_real_))
    row<-cbind(data.frame(SCENARIO=cell$SCENARIO,BASEID=cell$BASEID,SIMID=b,replicate_seed=study_replicate_seed(cfg$seed,cell$BASEID,b),n=cell$n,true_hr=cell$hr,hypothesis=cell$hypothesis,cut_value=cell$cut_value,DCO_DAY=NA_real_,n_observed=NA_integer_,n_control=NA_integer_,n_treatment=NA_integer_,events=NA_integer_,target_reached=NA,target_day=NA_real_,generated=FALSE,decision_valid=FALSE,decision_reject=NA,generation_note=message),as.data.frame(a))
    if(!study_is_ph(cfg))row$effect_label<-cell$effect_label
    if(cfg$primary %in% c("rmst","survival"))row$true_effect<-cell$true_effect
    row})
  do.call(rbind,rows)
}
study_wilson <- function(k,n) {
  if(n==0)return(c(NA_real_,NA_real_));z<-qnorm(.975);p<-k/n;den<-1+z^2/n
  pmin(1,pmax(0,c((p+z^2/(2*n)-z*sqrt(p*(1-p)/n+z^2/(4*n^2)))/den,(p+z^2/(2*n)+z*sqrt(p*(1-p)/n+z^2/(4*n^2)))/den)))
}
study_overview <- function(rows,cfg) {
  if(!nrow(rows))return(data.frame())
  overview<-do.call(rbind,lapply(split(rows,rows$SCENARIO),function(x){valid<-x$decision_valid;reject<-x$decision_reject %in% TRUE;k<-sum(reject);n<-nrow(x);v<-sum(valid);invalid<-n-v;p<-if(v)k/v else NA_real_;w<-study_wilson(k,v)
    cv<-x$cox_valid;coverage<-x$covered[cv&!is.na(x$covered)];beta<-x$beta[cv];ci<-study_wilson(sum(coverage),length(coverage))
    data.frame(SCENARIO=x$SCENARIO[1],n=x$n[1],true_hr=x$true_hr[1],hypothesis=x$hypothesis[1],cut_value=x$cut_value[1],requested=cfg$reps,completed=n,decision_valid=v,invalid=invalid,generation_failures=sum(!x$generated),
      rejection_lower=k/n,rejection_upper=(k+invalid)/n,rejection_conditional=p,mcse=if(v)sqrt(p*(1-p)/v) else NA_real_,wilson_lower=w[1],wilson_upper=w[2],
      mean_events=if(any(x$generated))mean(x$events[x$generated]) else NA_real_,mean_dco_day=if(any(x$generated))mean(x$DCO_DAY[x$generated]) else NA_real_,target_reached=if(cfg$cut_mode=="target"&&any(x$generated))mean(x$target_reached[x$generated]) else NA_real_,
      cox_valid=sum(cv),beta_bias=if(any(cv))mean(beta-log(x$true_hr[1])) else NA_real_,beta_rmse=if(any(cv))sqrt(mean((beta-log(x$true_hr[1]))^2)) else NA_real_,beta_empirical_sd=if(sum(cv)>1)sd(beta) else NA_real_,mean_se_beta=if(any(cv))mean(x$se_beta[cv]) else NA_real_,hr_bias=if(any(cv))mean(x$hr_estimate[cv]-x$true_hr[1]) else NA_real_,coverage=if(length(coverage))mean(coverage) else NA_real_,coverage_mcse=if(length(coverage)){p<-mean(coverage);sqrt(p*(1-p)/length(coverage))} else NA_real_,coverage_lower=ci[1],coverage_upper=ci[2])
  }))
  if(!study_is_ph(cfg))overview$effect_label<-vapply(overview$SCENARIO,function(id)rows$effect_label[match(id,rows$SCENARIO)],character(1))
  overview$mean_beta<-vapply(overview$SCENARIO,function(id){x<-rows[rows$SCENARIO==id&rows$cox_valid,];if(nrow(x))mean(x$beta) else NA_real_},numeric(1))
  overview$median_cox_hr<-vapply(overview$SCENARIO,function(id){x<-rows[rows$SCENARIO==id&rows$cox_valid,];if(nrow(x))median(x$hr_estimate) else NA_real_},numeric(1))
  overview
}
# Approximation only: constant PH, large-sample event information p(1-p)D.
study_ph_reference <- function(hr,events,allocation=.5,alpha=.05,sided="two",power=.8) {
  delta<-log(hr)*sqrt(events*allocation*(1-allocation))
  rejection<-if(sided=="two"){z<-qnorm(1-alpha/2);pnorm(-z-delta)+pnorm(delta-z)} else pnorm(qnorm(alpha)-delta)
  needed<-(qnorm(if(sided=="two")1-alpha/2 else 1-alpha)+qnorm(power))^2/(allocation*(1-allocation)*log(hr)^2)
  needed[hr==1 | (sided=="benefit" & hr>1)]<-Inf
  data.frame(hr=hr,events=events,approximate_rejection=rejection,approximate_required_events=needed)
}
run_design_study <- function(cfg,progress=function(state)NULL,should_cancel=function()FALSE,batch_size=10L) {
  validate_study_config(cfg);cfg<-gs_prepare(cfg);sc<-study_scenarios(cfg);base_ids<-unique(sc$BASEID);rows<-list();look_rows<-list();done<-0L;total<-nrow(sc)*cfg$reps;cancelled<-FALSE
  for(id in base_ids) {
    cells<-sc[sc$BASEID==id,,drop=FALSE]
    for(b in seq_len(cfg$reps)) {
      if(should_cancel()){cancelled<-TRUE;break}
      set.seed(study_replicate_seed(cfg$seed,id,b));tc<-study_trial_config(cfg,cells[1,])
      trial<-tryCatch({truth<-simulate_survival_truth(tc,b);if(study_is_sequential(cfg)){a<-lapply(seq_len(nrow(cells)),function(j)gs_trial(truth,cfg,cells[j,],b));look_rows[[length(look_rows)+1]]<-do.call(rbind,lapply(a,`[[`,"looks"));do.call(rbind,lapply(a,`[[`,"row"))} else do.call(rbind,lapply(seq_len(nrow(cells)),function(j)study_trial_row(truth,cfg,cells[j,],b)))},error=function(e)if(study_is_sequential(cfg))gs_failed_rows(cells,b,cfg,conditionMessage(e)) else study_failed_rows(cells,b,cfg,conditionMessage(e)))
      rows[[length(rows)+1]]<-trial;done<-done+nrow(trial)
      if(b%%batch_size==0||b==cfg$reps) {
        d<-do.call(rbind,rows);progress(list(completed=done,total=total,rows=d,overview=study_overview(d,cfg),scenario=sc,status="running",looks=if(length(look_rows))do.call(rbind,look_rows) else gs_empty_looks()))
      }
    }
    if(cancelled)break
  }
  d<-if(length(rows))do.call(rbind,rows) else (if(study_is_sequential(cfg))gs_failed_rows(sc[1,,drop=FALSE],1,cfg,"not run") else study_failed_rows(sc[1,,drop=FALSE],1,cfg,"not run"))[FALSE,,drop=FALSE];rownames(d)<-NULL
  list(config=cfg,scenarios=sc,rows=d,overview=study_overview(d,cfg),completed=done,total=total,status=if(cancelled)"cancelled" else "complete",looks=if(length(look_rows))do.call(rbind,look_rows) else gs_empty_looks(),created_at=format(Sys.time(),tz="UTC",usetz=TRUE),version="0.30.0")
}
study_sample <- function(result,scenario,replicate) {
  cell<-result$scenarios[result$scenarios$SCENARIO==scenario,,drop=FALSE]
  if(nrow(cell)!=1||!any(result$rows$SCENARIO==scenario&result$rows$SIMID==replicate&result$rows$generated))stop("请选择已成功生成的情景与轮次。")
  set.seed(study_replicate_seed(result$config$seed,cell$BASEID,replicate));cfg<-study_trial_config(result$config,cell);truth<-simulate_survival_truth(cfg,replicate)
  cut<-if(study_is_sequential(result$config))list(day=result$rows$DCO_DAY[match(paste(scenario,replicate),paste(result$rows$SCENARIO,result$rows$SIMID))]) else study_analysis_cut(truth,result$config,cell);o<-survival_at_cut(truth,cut$day,scenario)
  o$SCENARIO<-rep(scenario,nrow(o));truth$SCENARIO<-rep(scenario,nrow(truth))
  cfg$fixed_times<-c(180,365);list(observed=o,truth=truth,analysis=analyze_simulated_survival(o,cfg,replicate,scenario,cut$day))
}

# Only cutoffs within the same N/HR base use common random numbers.
study_paired_comparisons <- function(result) {
  if(!nrow(result$rows))return(data.frame())
  out<-list()
  for(base in unique(result$scenarios$BASEID)) {
    cells<-result$scenarios[result$scenarios$BASEID==base,,drop=FALSE]
    if(nrow(cells)<2)next
    for(pair in combn(cells$SCENARIO,2,simplify=FALSE)) {
      a<-result$rows[result$rows$SCENARIO==pair[1],];b<-result$rows[result$rows$SCENARIO==pair[2],]
      x<-merge(a[,c("SIMID","decision_valid","decision_reject")],b[,c("SIMID","decision_valid","decision_reject")],by="SIMID")
      valid<-x$decision_valid.x&x$decision_valid.y;d<-as.integer(x$decision_reject.y[valid])-as.integer(x$decision_reject.x[valid]);n<-length(d)
      out[[length(out)+1]]<-data.frame(BASEID=base,scenario_a=pair[1],scenario_b=pair[2],paired_completed=nrow(x),paired_valid=n,conditional_difference=if(n)mean(d) else NA_real_,paired_mcse=if(n>1)sd(d)/sqrt(n) else NA_real_)
    }
  }
  if(length(out))do.call(rbind,out) else data.frame()
}
