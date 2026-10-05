# Batch effect profiles and preplanned method comparisons; development draft.
batch_is_ph <- function(cfg) identical(cfg$effect_mode %||% "ph","ph")
batch_fields <- function(cfg) c(batch_scenario_fields,if(!batch_is_ph(cfg))"profile")
batch_parse_profiles <- function(text,unit="months") {
  if(length(text)!=1||is.na(text)||!nzchar(trimws(text)))stop("请填写分段HR定义，每行名称 | 切点 | HR列表。")
  lines<-trimws(strsplit(text,"\n",fixed=TRUE)[[1]]);lines<-lines[nzchar(lines)]
  if(!length(lines)||length(lines)>12)stop("分段HR定义需1–12行。")
  out<-list()
  for(line in lines) {
    fields<-trimws(strsplit(line,"|",fixed=TRUE)[[1]])
    if(length(fields)!=3||!nzchar(fields[1])||fields[1] %in% names(out))stop("每行需唯一名称 | 切点 | HR列表，不能使用名称中的竖线。")
    cuts<-parse_unit_numbers(fields[2])*time_factor(unit);hr<-parse_unit_numbers(fields[3]);validate_hr_profile(cuts,hr)
    out[[fields[1]]]<-list(cuts=cuts,hr=hr)
  }
  out
}
batch_profile_text <- function(profiles,unit="months") {
  nums<-function(x)paste(format(x,digits=16,trim=TRUE),collapse=",")
  paste(vapply(names(profiles),function(n)paste(n,nums(profiles[[n]]$cuts/time_factor(unit)),nums(profiles[[n]]$hr),sep=" | "),character(1)),collapse="\n")
}
batch_method_names <- c(logrank="普通log-rank",cox="Cox Wald",fh="FH加权log-rank",rmst="RMST差值Wald",survival="固定时点生存率差Wald")
batch_methods <- function(cfg) if(ma_is(cfg))"design" else if(ds_is(cfg))"search" else if(fp_is(cfg))"prediction" else if(ob_is(cfg))ob_methods(cfg) else if(ms_is(cfg))"prediction" else if(je_is(cfg))je_methods(cfg) else if(sp_is(cfg))sp_methods(cfg) else if(cb_is(cfg))cb_methods(cfg) else if(hg_is(cfg))hg_methods(cfg) else if(abr_is(cfg))abr_methods(cfg) else if(jr_is(cfg))c("PFS","OS") else if(batch_is_sequential(cfg))c("logrank",if(isTRUE(cfg$fixed_benchmark))"fixed_final") else unique(c(cfg$primary,if(isTRUE(cfg$compare_methods))cfg$extra_methods))
batch_method_label <- function(method,cfg) if(ma_is(cfg))unname(ma_names[method]) else if(ds_is(cfg))unname(ds_names[method]) else if(fp_is(cfg))"柔性/盲态预测" else if(ob_is(cfg))ifelse(method=="release","已知临床积压发布",unname(ob_names[method])) else if(ms_is(cfg))"多状态条件预测" else if(je_is(cfg))unname(je_names[method]) else if(sp_is(cfg))unname(sp_names[method]) else if(cb_is(cfg))unname(cb_names[method]) else if(hg_is(cfg))unname(hg_names[method]) else if(abr_is(cfg))unname(abr_names[method]) else if(jr_is(cfg))unname(jr_policy_names[method]) else ifelse(method=="fixed_final","原D*固定Final参照",ifelse(batch_is_sequential(cfg)&method=="logrank","组序贯log-rank",unname(batch_method_names[method])))
batch_effect <- function(cfg,sc,amplitude) {
  pr<-if(batch_is_ph(cfg))list(cuts=numeric(),hr=1) else cfg$profiles[[as.character(sc$profile)]]
  if(is.null(pr))stop("情景引用未知HR定义。")
  list(cuts=pr$cuts,hr=pr$hr*amplitude)
}
batch_method_truth <- function(tc,cell,method) {
  if(method=="cox")return(list(value=if(study_is_ph(tc))log(cell$hr) else NA_real_,note=if(study_is_ph(tc))"" else "NPH无单一真实Cox HR，偏倚和覆盖不计算"))
  if(!method %in% c("rmst","survival"))return(list(value=NA_real_,note=""))
  c<-tc;c$primary<-method
  tryCatch(list(value=study_true_estimand(c,cell),note=""),error=function(e)list(value=NA_real_,note=paste0("生成分布真值计算失败：",conditionMessage(e))))
}
batch_method_rows <- function(primary_row,observed,tc,cell,sc,cfg,b) {
  methods<-batch_methods(cfg)
  out<-lapply(methods,function(method) {
    c<-tc;c$primary<-method
    generated<-isTRUE(primary_row$generated)
    t<-if(generated)batch_method_truth(tc,cell,method) else list(value=NA_real_,note="该轮生成/分析失败，真值未提供")
    empty<-list(valid=FALSE,reject=NA,p=NA_real_,z=NA_real_,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,covered=NA,note=primary_row$generation_note %||% "")
    a<-if(!generated)empty else tryCatch({
      if(method %in% c("logrank","cox")) {
        if(method=="logrank")list(valid=primary_row$logrank_valid,reject=primary_row$logrank_reject,p=primary_row$logrank_p,z=primary_row$logrank_z,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,covered=NA,note=primary_row$logrank_note)
        else list(valid=primary_row$cox_valid,reject=primary_row$cox_reject,p=primary_row$cox_p,z=primary_row$beta/primary_row$se_beta,estimate=primary_row$beta,se=primary_row$se_beta,lower=log(primary_row$hr_lower),upper=log(primary_row$hr_upper),covered=primary_row$covered,note=primary_row$cox_note)
      } else {
        e<-if(method==cfg$primary)as.list(primary_row) else study_extended_analysis(observed,c,t$value)
        if(method=="fh")list(valid=e$fh_valid,reject=e$fh_reject,p=e$fh_p,z=e$fh_z,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,covered=NA,note=e$fh_note)
        else list(valid=e$contrast_valid,reject=e$contrast_reject,p=e$contrast_p,z=e$contrast_z,estimate=e$contrast_estimate,se=e$contrast_se,lower=e$contrast_lower,upper=e$contrast_upper,covered=e$contrast_covered,note=e$contrast_note)
      }
    },error=function(e){empty$note<-conditionMessage(e);empty})
    performed<-generated&&isTRUE(primary_row$formal_test_performed)
    closed<-generated&&!performed&&cfg$miss_policy=="no_reject"
    known<-if(closed)TRUE else generated&&isTRUE(a$valid)
    decision<-if(closed)FALSE else if(known)isTRUE(a$reject) else NA
    contrast<-method %in% c("rmst","survival")
    global_null<-if(study_is_ph(tc))isTRUE(cell$hr==1) else all(tc$hr_profile==1)
    null_status<-if(!generated)"unknown" else if(contrast)if(!is.finite(t$value))"unknown" else if(abs(t$value)<=1e-10*max(1,if(method=="rmst")tc$analysis_tau else 1))"contrast_null" else "alternative" else if(global_null)"global_survival_null" else "alternative"
    data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=primary_row$replicate_seed,method=method,is_primary=method==cfg$primary,
      method_label=unname(batch_method_names[method]),effect_mode=cfg$effect_mode %||% "ph",profile=if(batch_is_ph(cfg))"PH" else as.character(sc$profile),
      generated=generated,analysis_valid=isTRUE(a$valid),analysis_performed=performed,decision_valid=known,decision_reject=decision,
      p_value=a$p,z=a$z,benefit_z=if(contrast)a$z else -a$z,estimate=a$estimate,se=a$se,lower=a$lower,upper=a$upper,
      true_effect=t$value,covered=if(is.finite(t$value))a$covered else NA,null_status=null_status,
      analysis_tau_day=if(contrast)tc$analysis_tau else NA_real_,fh_rho=if(method=="fh")tc$fh_rho else NA_real_,fh_gamma=if(method=="fh")tc$fh_gamma else NA_real_,
      effect_scale=primary_row$drawn_effect_scale,baseline_time_scale=primary_row$time_scale,
      estimate_unit=if(method=="rmst")"days" else if(method=="survival")"probability" else if(method=="cox")"logHR" else "score_only",
      analysis_note=a$note,truth_note=t$note,stringsAsFactors=FALSE)
  })
  do.call(rbind,out)
}
batch_method_overview <- function(rows,cfg,sc) {
  plan<-expand.grid(SCENARIO=sc$SCENARIO,method=batch_methods(cfg),stringsAsFactors=FALSE,KEEP.OUT.ATTRS=FALSE)
  do.call(rbind,lapply(seq_len(nrow(plan)),function(j) {
    id<-plan$SCENARIO[j];m<-plan$method[j]
    x<-if(nrow(rows))rows[rows$SCENARIO==id&rows$method==m,,drop=FALSE] else data.frame()
    processed<-nrow(x);known<-if(processed)x$decision_valid %in% TRUE else logical();v<-sum(known);k<-if(v)sum(x$decision_reject[known] %in% TRUE) else 0L
    p<-if(v)k/v else NA_real_;w<-study_wilson(k,v)
    ok<-if(processed)x$analysis_valid %in% TRUE else logical();est<-if(processed)ok&is.finite(x$estimate) else logical()
    truth<-if(processed)est&is.finite(x$true_effect) else logical();err<-if(any(truth))x$estimate[truth]-x$true_effect[truth] else numeric()
    cov<-if(processed)ok&!is.na(x$covered) else logical();coverage<-if(any(cov))mean(x$covered[cov]) else NA_real_
    data.frame(SCENARIO=id,label=sc$label[match(id,sc$SCENARIO)],method=m,method_label=batch_method_label(m,cfg),is_primary=m==cfg$primary,
      requested=cfg$reps,processed=processed,not_run=cfg$reps-processed,valid=v,invalid_processed=processed-v,successes=k,
      success_lower_all=k/cfg$reps,success_upper_all=(k+cfg$reps-v)/cfg$reps,success_conditional=p,mcse=if(v)sqrt(p*(1-p)/v) else NA_real_,wilson_lower=w[1],wilson_upper=w[2],
      analysis_valid=sum(ok),estimate_n=sum(est),truth_n=sum(truth),mean_estimate=if(any(est))mean(x$estimate[est]) else NA_real_,
      mean_true_effect=if(any(truth))mean(x$true_effect[truth]) else NA_real_,bias=if(length(err))mean(err) else NA_real_,rmse=if(length(err))sqrt(mean(err^2)) else NA_real_,
      mean_se=if(any(est))mean(x$se[est]) else NA_real_,coverage=coverage,coverage_n=sum(cov),coverage_mcse=if(any(cov))sqrt(coverage*(1-coverage)/sum(cov)) else NA_real_,
      null_rows=if(processed)sum(x$null_status %in% c("contrast_null","global_survival_null")) else 0L,
      estimate_unit=if(m=="rmst")"days" else if(m=="survival")"probability" else if(m=="cox")"logHR" else "score_only")
  }))
}
batch_method_pairs <- function(rows,cfg,sc) {
  extras<-setdiff(batch_methods(cfg),cfg$primary);if(!length(extras))return(data.frame())
  out<-list()
  for(id in sc$SCENARIO)for(m in extras) {
    x<-if(nrow(rows))rows[rows$SCENARIO==id,,drop=FALSE] else data.frame()
    known<-function(method){a<-rep(NA,cfg$reps);if(nrow(x)){d<-x[x$method==method,,drop=FALSE];ok<-d$decision_valid %in% TRUE;a[d$SIMID[ok]]<-d$decision_reject[ok]};a}
    a<-known(cfg$primary);b<-known(m);paired<-!is.na(a)&!is.na(b);d<-as.integer(b[paired])-as.integer(a[paired]);n<-length(d)
    lo<-function(y)ifelse(is.na(y),0,as.integer(y));hi<-function(y)ifelse(is.na(y),1,as.integer(y))
    out[[length(out)+1L]]<-data.frame(SCENARIO=id,label=sc$label[match(id,sc$SCENARIO)],primary_method=cfg$primary,comparison_method=m,requested=cfg$reps,paired_valid=n,
      primary_success_paired=if(n)mean(a[paired]) else NA_real_,comparison_success_paired=if(n)mean(b[paired]) else NA_real_,
      paired_difference=if(n)mean(d) else NA_real_,paired_mcse=if(n>1)sd(d)/sqrt(n) else NA_real_,
      paired_difference_lower_all=mean(lo(b)-hi(a)),paired_difference_upper_all=mean(hi(b)-lo(a)))
  }
  do.call(rbind,out)
}
batch_attach_method_outputs <- function(state) {
  if(ma_is(state$config))return(ma_aggregate(state))
  if(ds_is(state$config))return(ds_aggregate(state))
  if(fp_is(state$config))return(fp_aggregate(state))
  if(ob_is(state$config))return(ob_aggregate(state))
  if(ms_is(state$config))return(ms_aggregate(state))
  if(je_is(state$config))return(je_aggregate(state))
  if(sp_is(state$config))return(sp_aggregate(state))
  if(cb_is(state$config))return(cb_aggregate(state))
  if(hg_is(state$config))return(hg_aggregate(state))
  if(abr_is(state$config))return(abr_aggregate(state))
  if(jr_is(state$config))return(jr_aggregate(state))
  state$method_rows<-state$method_rows %||% data.frame()
  state$method_overview<-batch_method_overview(state$method_rows,state$config,state$scenarios)
  state$method_pairs<-batch_method_pairs(state$method_rows,state$config,state$scenarios);batch_attach_sequential_outputs(state)
}
