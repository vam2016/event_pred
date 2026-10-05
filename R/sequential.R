if(!exists("gs_design_object",mode="function"))source("R/sequential_beta.R",local=TRUE)
# Prespecified PH/log-rank event-driven group sequential designs.
study_is_sequential <- function(cfg) identical(cfg$design_mode %||% "fixed","sequential")
gs_event_targets <- function(target,timing) {
  x<-target*timing;r<-round(x);close<-abs(x-r)<=16*.Machine$double.eps*pmax(1,abs(x));x[close]<-r[close];ceiling(x)
}
validate_sequential_config <- function(cfg) {
  if(!study_is_ph(cfg)||cfg$primary!="logrank"||cfg$cut_mode!="target")stop("组序贯目前使用两组PH、标准log-rank及事件驱动截点。")
  t<-cfg$gs_timing
  if(!is.numeric(t)||length(t)<2||length(t)>5||any(!is.finite(t)|t<.1|t>1)||is.unsorted(t,strictly=TRUE)||tail(t,1)!=1)stop("计划信息比例需2–5个递增值，首个至少0.1、最后为1。例如0.5,1。")
  if(!cfg$gs_spending %in% c("asOF","asP"))stop("请选择OBF型或Pocock型alpha消耗。")
  if(!cfg$gs_futility %in% c("none","z","beta"))stop("请选择不设、非约束性Z或约束性beta消耗。")
  gs_validate_beta(cfg)
  if(cfg$gs_futility=="z"&&(cfg$sided!="benefit"||length(cfg$gs_futility_z)!=length(t)-1||any(!is.finite(cfg$gs_futility_z)|abs(cfg$gs_futility_z)>5)))stop("非约束性无效性只用于单侧获益，需K−1个−5至5的Z界值。")
  if(cfg$miss_policy!="no_reject")stop("组序贯窗口末未达下一目标时只关闭试验，不追加显著性检验。")
  if(isTRUE(cfg$estimate_hr))stop("组序贯暂不输出停止后未经调整的Cox区间及覆盖率。")
  if(length(cfg$target_values)*length(cfg$n_values)*length(cfg$hr_values)*cfg$reps*length(t)>200000)stop("组序贯计划分析次数最多20万，请减少情景、重复数或分析次数。")
  for(d in cfg$target_values) {
    targets<-gs_event_targets(d,t);if(anyDuplicated(targets))stop("信息比例取整后有重复事件目标，请提高最终D*或分开比例。")
  }
  invisible(TRUE)
}
# Preserve the caller's RNG state even if a dependency changes its internals.
gs_preserve_rng <- function(expr) {
  exists_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(exists_seed)old<-get(".Random.seed",envir=.GlobalEnv)
  on.exit(if(exists_seed)assign(".Random.seed",old,envir=.GlobalEnv) else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv),add=TRUE)
  force(expr)
}
gs_plan <- function(cfg,target) {
  targets<-gs_event_targets(target,cfg$gs_timing);t<-targets/target
  d<-gs_design_object(cfg,t)
  lower<-if(cfg$sided=="two")-d$criticalValues else rep(-Inf,length(t))
  fut<-c(if(gs_is_binding(cfg))as.numeric(d$futilityBounds) else if(cfg$gs_futility=="z")cfg$gs_futility_z else rep(-Inf,length(t)-1),-Inf)
  if(length(fut)!=length(t)||anyNA(fut))stop("原设计未返回每次分析对应的无效界值。")
  if(any(fut>=d$criticalValues))stop("无效性边界需低于同次效力边界。")
  plan<-data.frame(look=seq_along(t),target_events=targets,planned_fraction=cfg$gs_timing,information_fraction=t,upper_z=d$criticalValues,lower_efficacy_z=lower,futility_z=fut,alpha_spent=d$alphaSpent,nominal_stage_p=d$stageLevels)
  if(gs_is_binding(cfg)) {meta<-gs_beta_plan_metadata(cfg,d,target);for(id in names(meta))plan[[id]]<-meta[[id]]}
  plan
}
gs_prepare <- function(cfg) {
  if(!study_is_sequential(cfg))return(cfg)
  validate_sequential_config(cfg)
  if(is.null(cfg$gs_plans))cfg$gs_plans<-setNames(lapply(cfg$target_values,function(d)gs_plan(cfg,d)),as.character(cfg$target_values))
  for(d in cfg$target_values) {
    p<-cfg$gs_plans[[as.character(d)]]
    if(is.null(p)||!isTRUE(all.equal(p$target_events,gs_event_targets(d,cfg$gs_timing)))||!isTRUE(all.equal(p$information_fraction,p$target_events/d)))stop("保存的组序贯事件目标与当前配置不符。")
    if(gs_is_binding(cfg)) {
      expected<-gs_plan(cfg,d)
      if(!all(names(expected) %in% names(p))||!isTRUE(all.equal(p[,names(expected),drop=FALSE],expected,tolerance=1e-8,check.attributes=FALSE)))stop("保存的约束性beta设计与原配置不符。")
    }
  }
  cfg
}
gs_action <- function(w,valid,look,plan,sided) {
  if(!valid||!is.finite(w))return("invalid")
  if(w>=plan$upper_z[look])return("efficacy_benefit")
  if(sided=="two"&&w<=plan$lower_efficacy_z[look])return("efficacy_reverse")
  if(look<nrow(plan)&&w<=plan$futility_z[look])return("futility")
  if(look==nrow(plan))"final_no_reject" else "continue"
}
gs_empty_looks <- function() data.frame(SCENARIO=integer(),BASEID=integer(),SIMID=integer(),look=integer(),target_events=numeric(),information_fraction=numeric(),DCO_DAY=numeric(),events=integer(),n_observed=integer(),target_reached=logical(),performed=logical(),valid=logical(),benefit_z=numeric(),score_variance=numeric(),event_information=numeric(),upper_z=numeric(),lower_efficacy_z=numeric(),futility_z=numeric(),alpha_spent=numeric(),action=character())
gs_trial <- function(truth,cfg,cell,b) {
  plan<-cfg$gs_plans[[as.character(cell$cut_value)]];traces<-list();last<-NULL
  for(j in seq_len(nrow(plan))) {
    tmp<-cell;tmp$cut_value<-plan$target_events[j]
    row<-study_trial_row(truth,cfg,tmp,b)
    o<-survival_at_cut(truth,row$DCO_DAY);lr<-if(isTRUE(row$target_reached))study_logrank(o$time_day,o$event,as.integer(o$group=="Treatment")) else list(variance=NA_real_)
    w<-if(isTRUE(row$target_reached)&&row$logrank_valid)-row$logrank_z else NA_real_
    action<-if(!isTRUE(row$target_reached))"window_unreached" else gs_action(w,row$logrank_valid,j,plan,cfg$sided)
    traces[[j]]<-data.frame(SCENARIO=cell$SCENARIO,BASEID=cell$BASEID,SIMID=b,look=j,target_events=plan$target_events[j],information_fraction=plan$information_fraction[j],DCO_DAY=row$DCO_DAY,events=row$events,n_observed=row$n_observed,target_reached=row$target_reached,performed=isTRUE(row$target_reached),valid=isTRUE(row$target_reached)&&row$logrank_valid,benefit_z=w,score_variance=lr$variance,event_information=row$events*cfg$treatment_fraction*(1-cfg$treatment_fraction),upper_z=plan$upper_z[j],lower_efficacy_z=plan$lower_efficacy_z[j],futility_z=plan$futility_z[j],alpha_spent=plan$alpha_spent[j],action=action)
    if(gs_is_binding(cfg)) {traces[[j]]$binding_futility<-TRUE;traces[[j]]$beta_spent<-plan$beta_spent[j]}
    last<-row
    if(action!="continue")break
  }
  last$cut_value<-cell$cut_value;last$gs_stop_look<-j;last$gs_stop_reason<-action;last$gs_analysis_count<-sum(vapply(traces,function(x)x$performed,logical(1)));last$gs_stop_planned_fraction<-plan$information_fraction[j]
  last$target_reached<-last$events>=cell$cut_value;last$target_day<-if(last$target_reached)last$DCO_DAY else NA_real_
  last$decision_valid<-action!="invalid";last$decision_reject<-if(last$decision_valid)startsWith(action,"efficacy") else NA
  list(row=last,looks=do.call(rbind,traces))
}
gs_failed_rows <- function(cells,b,cfg,message) {
  d<-study_failed_rows(cells,b,cfg,message);d$gs_stop_look<-NA_integer_;d$gs_stop_reason<-"generation_failure";d$gs_analysis_count<-0L;d$gs_stop_planned_fraction<-NA_real_;d
}
gs_overview <- function(result) {
  if(!study_is_sequential(result$config)||!nrow(result$rows))return(data.frame())
  do.call(rbind,lapply(split(result$rows,result$rows$SCENARIO),function(x) {
    n<-nrow(x);done<-x$generated
    data.frame(SCENARIO=x$SCENARIO[1],completed=n,requested=result$config$reps,early_efficacy=sum(startsWith(x$gs_stop_reason,"efficacy")&x$gs_stop_look<length(result$config$gs_timing),na.rm=TRUE)/n,efficacy_benefit=sum(x$gs_stop_reason=="efficacy_benefit")/n,efficacy_reverse=sum(x$gs_stop_reason=="efficacy_reverse")/n,futility=sum(x$gs_stop_reason=="futility")/n,final_no_reject=sum(x$gs_stop_reason=="final_no_reject")/n,window_unreached=sum(x$gs_stop_reason=="window_unreached")/n,invalid=sum(!x$decision_valid),mean_looks=if(any(done))mean(x$gs_analysis_count[done]) else NA_real_,mean_events=if(any(done))mean(x$events[done]) else NA_real_,mean_stop_day=if(any(done))mean(x$DCO_DAY[done]) else NA_real_)
  }))
}
gs_probability <- function(lower,upper,mean,cov) {
  if(any(lower>=upper))return(0)
  sd<-sqrt(diag(cov));if(any(!is.finite(sd)|sd<=0))stop("条件协方差无效。")
  lo<-pmax(-12,pmin(12,(lower-mean)/sd));hi<-pmax(-12,pmin(12,(upper-mean)/sd));if(any(lo>=hi))return(0)
  if(length(mean)==1L)return(as.numeric(pnorm(hi)-pnorm(lo)))
  corr<-cov/outer(sd,sd)
  p<-gs_preserve_rng(mvtnorm::pmvnorm(lower=lo,upper=hi,mean=rep(0,length(mean)),corr=corr,algorithm=mvtnorm::Miwa(steps=256)))
  if(!is.finite(p)||p< -1e-6||p>1+1e-6)stop("条件正态积分未通过概率范围检查。")
  pmin(1,pmax(0,as.numeric(p)))
}
# Conditional canonical increments. No future patient trajectory is used.
gs_conditional_power <- function(plan,look,w,future_hr,allocation=.5,sided="benefit",respect_futility=TRUE) {
  if(length(look)!=1||!look %in% plan$look||length(w)!=1||!is.finite(w)||length(future_hr)!=1||!is.finite(future_hr)||future_hr<.01||future_hr>100)stop("请选择已计算的分析及有限Z，未来HR需0.01–100。")
  if(length(allocation)!=1||!is.finite(allocation)||allocation<=0||allocation>=1)stop("分配概率需在0与1之间。")
  if("binding_futility" %in% names(plan)&&any(plan$binding_futility)&&!isTRUE(respect_futility))stop("约束性无效边界必须执行；不能沿用原效力界计算忽略该规则的条件功效。")
  action<-gs_action(w,TRUE,look,plan,sided)
  if(action!="continue")return(list(full=if(startsWith(action,"efficacy"))1 else 0,final_only=NA_real_,status=action))
  t0<-plan$information_fraction[look];idx<-seq.int(look+1,nrow(plan));t<-plan$information_fraction[idx]
  delta<--log(future_hr)*sqrt(tail(plan$target_events,1)*allocation*(1-allocation))
  mu<-w*sqrt(t0/t)+delta*(t-t0)/sqrt(t)
  covariance<-outer(t,t,function(x,y)(pmin(x,y)-t0)/sqrt(x*y))
  lower<-if(sided=="two")plan$lower_efficacy_z[idx] else if(respect_futility)plan$futility_z[idx] else rep(-Inf,length(idx))
  upper<-plan$upper_z[idx];cp<-0
  for(j in seq_along(idx)) {
    prev<-if(j==1)integer() else seq_len(j-1);use<-seq_len(j)
    cp<-cp+gs_probability(c(lower[prev],upper[j]),c(upper[prev],Inf),mu[use],covariance[use,use,drop=FALSE])
    if(sided=="two")cp<-cp+gs_probability(c(lower[prev],-Inf),c(upper[prev],lower[j]),mu[use],covariance[use,use,drop=FALSE])
  }
  if(cp>1+1e-5)stop("条件功效积分累计超过1，请核对设计。")
  j<-length(t);sd_final<-sqrt(covariance[j,j]);last<-pnorm((mu[j]-upper[j])/sd_final)
  if(sided=="two")last<-last+pnorm((lower[j]-mu[j])/sd_final)
  list(full=pmin(1,pmax(0,cp)),final_only=last,status="continue",future_hr=future_hr,delta=delta,information_fraction=t0)
}
study_dependencies <- function(cfg) {
  packages<-c("R","survival","processx",if(study_is_sequential(cfg))c("rpact","mvtnorm"))
  data.frame(package=packages,version=vapply(packages,function(p)if(p=="R")as.character(getRversion()) else as.character(packageVersion(p)),character(1)))
}
