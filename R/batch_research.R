# Fixed final PH/NPH study batches and prior-predictive PH assurance.
jr_is <- function(cfg)identical(cfg$research_family,"joint_endpoints")
abr_is <- function(cfg)identical(cfg$research_family,"independent_cohort_adaptation")
cb_is <- function(cfg)identical(cfg$research_family,"actual_ia_conditional")
hg_is <- function(cfg)identical(cfg$research_family,"heterogeneous_survival")
fp_is <- function(cfg)identical(cfg$research_family,"flexible_interval_prediction")
ob_is <- function(cfg)identical(cfg$research_family,"observation_process")
ma_is <- function(cfg)identical(cfg$research_family,"multiarm_independent_design")
ds_is <- function(cfg)identical(cfg$research_family,"finite_design_search")
ms_is <- function(cfg)identical(cfg$research_family,"multistate_conditional")
je_is <- function(cfg)identical(cfg$research_family,"joint_sequential_likelihood")
sp_is <- function(cfg)identical(cfg$research_family,"shared_patient_adaptation")
batch_scenario_fields <- c("label","n","hr","time_scale","enroll_scale","dropout_scale","cut_value")
batch_scenarios <- function(cfg) {
  if(ma_is(cfg))return(ma_scenarios(cfg))
  if(ds_is(cfg))return(ds_scenarios(cfg))
  if(fp_is(cfg))return(fp_scenarios(cfg))
  if(ob_is(cfg))return(ob_scenarios(cfg))
  if(ms_is(cfg))return(ms_scenarios(cfg))
  if(je_is(cfg))return(je_scenarios(cfg))
  if(sp_is(cfg))return(sp_scenarios(cfg))
  if(cb_is(cfg))return(cb_scenarios(cfg))
  if(hg_is(cfg))return(hg_scenarios(cfg))
  if(abr_is(cfg))return(abr_scenarios(cfg))
  if(jr_is(cfg))return(jr_scenarios(cfg))
  fields<-batch_fields(cfg)
  if(cfg$scenario_source=="csv") {
    d<-cfg$scenario_table
    if(!is.data.frame(d)||!identical(sort(names(d)),sort(fields)))stop("情景CSV列名需与当前效应模式一致：PH七列；NPH另加profile列。")
    d<-d[,fields,drop=FALSE]
  } else if(cfg$scenario_source=="grid") {
    axes<-cfg$axes
    if(!identical(sort(names(axes)),sort(setdiff(fields,"label"))))stop("情景轴不完整。")
    if(any(vapply(axes,length,integer(1))<1)||prod(vapply(axes,length,integer(1)))>60)stop("每个情景轴需有值，笛卡尔积最多60情景。")
    d<-expand.grid(axes,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
    d$label<-paste0("S",seq_len(nrow(d)));d<-d[,fields,drop=FALSE]
  } else stop("请选择列表组合或情景CSV。")
  if(!nrow(d)||nrow(d)>60)stop("情景数需1–60。")
  if(anyNA(d$label)||any(!nzchar(trimws(as.character(d$label))))||anyDuplicated(d$label))stop("情景label需非空且唯一。")
  for(k in setdiff(fields,c("label","profile")))if(!is.numeric(d[[k]])||any(!is.finite(d[[k]])))stop(paste0("情景",k,"需为有限数值。"))
  if(any(d$n<10|d$n>5000|d$n!=floor(d$n))||any(d$hr<.01|d$hr>100)||any(d$time_scale<.01|d$time_scale>100)||any(d$enroll_scale<.01|d$enroll_scale>100)||any(d$dropout_scale<0|d$dropout_scale>100))stop("N需10–5000整数；HR/时间倍数/入组倍数0.01–100；退出倍数0–100。")
  if(cfg$cut_rule %in% c("target","event_minimum")) {
    if(any(d$cut_value<1|d$cut_value>100000|d$cut_value!=floor(d$cut_value)))stop("事件目标需1–100000整数。")
    d$cut_value_engine<-d$cut_value
  } else {
    d$cut_value_engine<-d$cut_value*time_factor(if(cfg$scenario_source=="csv")cfg$table_unit else cfg$display_unit)
    if(any(d$cut_value_engine<=0|d$cut_value_engine>3650))stop("DCO或入组后追加随访需在(0,3650]日。")
  }
  if(anyDuplicated(d[,setdiff(fields,"label"),drop=FALSE]))stop("存在参数完全重复的情景，请删除重复行。")
  if(!batch_is_ph(cfg)){
    d$profile<-as.character(d$profile)
    if(anyNA(d$profile)||any(!d$profile %in% names(cfg$profiles)))stop("情景profile需匹配本次分段HR定义名称。")
    for(j in seq_len(nrow(d))){pr<-batch_effect(cfg,d[j,,drop=FALSE],d$hr[j]);validate_hr_profile(pr$cuts,pr$hr)}
  }
  d$cut_unit<-if(cfg$cut_rule %in% c("target","event_minimum"))"events" else if(cfg$scenario_source=="csv")cfg$table_unit else cfg$display_unit
  d$SCENARIO<-seq_len(nrow(d));d
}
validate_batch_config <- function(cfg) {
  if(ma_is(cfg))return(validate_multiarm(cfg))
  if(ds_is(cfg))return(validate_design_search(cfg))
  if(fp_is(cfg))return(validate_flexible_prediction(cfg))
  if(ob_is(cfg))return(validate_observation(cfg))
  if(ms_is(cfg))return(validate_multistate(cfg))
  if(je_is(cfg))return(validate_joint_sequential(cfg))
  if(sp_is(cfg))return(validate_shared_adaptation(cfg))
  if(cb_is(cfg))return(validate_conditional_batch(cfg))
  if(hg_is(cfg))return(validate_heterogeneity(cfg))
  if(abr_is(cfg))return(validate_adaptive_batch(cfg))
  if(jr_is(cfg))return(validate_joint_research(cfg))
  scalar<-function(x,lo,hi)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi
  if(!cfg$mode %in% c("fixed_truth","assurance")||!cfg$cut_rule %in% c("fixed","target","event_minimum","last_entry_followup"))stop("请选择固定真值/assurance及支持的共同截点规则。")
  if(!scalar(cfg$reps,20,10000)||cfg$reps!=floor(cfg$reps)||!scalar(cfg$seed,0,.Machine$integer.max)||cfg$seed!=floor(cfg$seed))stop("每情景重复数20–10000，种子0–2147483647，均为整数。")
  if(!cfg$primary %in% names(batch_method_names)||!cfg$sided %in% c("benefit","two")||!scalar(cfg$alpha,.0001,.2)||!scalar(cfg$treatment_fraction,.01,.99))stop("批量比较限两组的预定一次Final分析，alpha及分配概率需在支持范围。")
  if(!is.null(cfg$effect_mode)&&!cfg$effect_mode %in% c("ph","nph"))stop("未知效应模式。")
  if(!batch_is_ph(cfg)){
    if(cfg$mode!="fixed_truth")stop("NPH批量首批限定固定真值；assurance仍使用PH设计先验。")
    if(!is.list(cfg$profiles)||length(cfg$profiles)<1||length(cfg$profiles)>12||is.null(names(cfg$profiles))||any(!nzchar(names(cfg$profiles)))||anyDuplicated(names(cfg$profiles)))stop("需1–12个唯一命名HR定义。")
    for(pr in cfg$profiles)validate_hr_profile(pr$cuts,pr$hr)
  }
  methods<-batch_methods(cfg)
  if(any(!methods %in% c(names(batch_method_names),if(batch_is_sequential(cfg))"fixed_final")))stop("未知比较方法。")
  if("fh" %in% methods&&(!scalar(cfg$fh_rho,0,5)||!scalar(cfg$fh_gamma,0,5)))stop("FH rho/gamma需在0–5。")
  if(any(methods %in% c("rmst","survival"))&&!scalar(cfg$analysis_tau,.001,3650))stop("tau需在(0,3650]日。")
  if(cfg$mode=="assurance"&&(cfg$sided!="benefit"||!scalar(cfg$prior_loghr_sd,0,2)||!scalar(cfg$prior_logtime_sd,0,1)))stop("assurance首批限定获益方向；logHR先验SD为0–2，log时间倍数先验SD为0–1。")
  if(!cfg$control_model$id %in% parameter_catalog()$id)stop("未知基准Control生成模型。")
  if(!cfg$enroll_mode %in% c("constant","piecewise"))stop("批量入口支持恒定或分段Poisson入组。")
  if(cfg$enroll_mode=="constant"&&!scalar(cfg$enroll_rate,.Machine$double.eps,1e6))stop("基准入组率需正。")
  if(cfg$enroll_mode=="piecewise")parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts)
  if(length(cfg$dropout_rates)!=2||any(!is.finite(cfg$dropout_rates)|cfg$dropout_rates<0))stop("两组独立退出率需非负。")
  if(cfg$cut_rule!="fixed"&&(!scalar(cfg$max_day,.Machine$double.eps,3650)||!cfg$miss_policy %in% c("no_reject","analyze")))stop("非固定规则需最长3650日窗口及预定未达规则处理。")
  if(cfg$cut_rule=="event_minimum"&&(!scalar(cfg$minimum_day,.Machine$double.eps,3650)||cfg$minimum_day>cfg$max_day))stop("最短研究持续时间需为正且不超过最大窗口。")
  if(isTRUE(cfg$filter_enabled)&&(!scalar(cfg$success_threshold,.5,.999)||!cfg$filter_basis %in% c("request_lower","wilson_lower")))stop("候选筛选需预定0.5–0.999阈值及所用概率依据。")
  time_factor(cfg$display_unit)
  if(length(cfg$origin)!=1||is.na(as.Date(cfg$origin))||!cfg$paramcd %in% c("PFS","OS"))stop("研究日期或终点无效。")
  sc<-batch_scenarios(cfg)
  if(batch_is_sequential(cfg)) {
    if(!batch_is_ph(cfg)||cfg$primary!="logrank"||cfg$cut_rule!="target"||cfg$miss_policy!="no_reject"||isTRUE(cfg$estimate_hr)||isTRUE(cfg$compare_methods))stop("组序贯批量限定PH/log-rank/事件目标，窗口关闭不拒绝，不读取Cox或附加方法。")
    if(nrow(sc)*cfg$reps*length(cfg$gs_timing)>200000)stop("组序贯计划分析总数最多20万。")
    for(j in seq_len(nrow(sc))){a<-sc[j,,drop=FALSE];validate_sequential_config(batch_trial_config(cfg,a,a$hr,a$time_scale))}
  }
  if(!scalar(cfg$reference_scenario,1,nrow(sc))||cfg$reference_scenario!=floor(cfg$reference_scenario))stop("参照情景ID不在本次情景表中。")
  if(nrow(sc)*cfg$reps>200000||sum(sc$n)*cfg$reps>3e7||nrow(sc)*cfg$reps*length(methods)>500000)stop("全部情景最多20万轮、患者×轮次最多3000万、方法×轮次最多50万。")
  invisible(TRUE)
}
batch_replicate_seed <- function(seed,scenario,replicate)as.integer((as.double(seed)+as.double(scenario)*1000003+as.double(replicate)*100003)%%2147483646+1)
batch_with_rng <- function(seed,expr) {
  existed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE);if(existed)old<-get(".Random.seed",envir=.GlobalEnv)
  kind<-RNGkind();on.exit({do.call(RNGkind,as.list(kind));if(existed)assign(".Random.seed",old,envir=.GlobalEnv) else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)},add=TRUE)
  RNGkind("Mersenne-Twister","Inversion","Rejection");set.seed(seed);force(expr)
}
batch_trial_config <- function(cfg,sc,hr,scale) {
  rate<-if(cfg$enroll_mode=="constant")cfg$enroll_rate*sc$enroll_scale else NULL
  pr<-batch_effect(cfg,sc,hr)
  c<-list(design_mode="fixed",goal="power",effect_mode=if(batch_is_ph(cfg))"ph" else "nph",n_values=sc$n,hr_values=hr,
    control_model=scale_parameter_model(cfg$control_model,scale),treatment_fraction=cfg$treatment_fraction,
    dropout_rates=cfg$dropout_rates*sc$dropout_scale,enroll_mode=cfg$enroll_mode,enroll_rate=rate,
    enroll_cuts=cfg$enroll_cuts,enroll_rates=cfg$enroll_rates*sc$enroll_scale,entry_days=numeric(),
    display_unit=cfg$display_unit,origin=cfg$origin,paramcd=cfg$paramcd,
    cut_mode="fixed",dco_values=if(cfg$cut_rule=="fixed")sc$cut_value_engine else cfg$max_day,
    max_day=cfg$max_day,miss_policy="analyze",primary=cfg$primary,sided=cfg$sided,alpha=cfg$alpha,
    estimate_hr=isTRUE(cfg$estimate_hr)||"cox" %in% batch_methods(cfg),reps=cfg$reps,seed=cfg$seed)
  if(!batch_is_ph(cfg)){c$hr_cuts<-pr$cuts;c$hr_profile<-pr$hr;c$include_h0<-FALSE}
  if("fh" %in% batch_methods(cfg)){c$fh_rho<-cfg$fh_rho;c$fh_gamma<-cfg$fh_gamma}
  if(any(batch_methods(cfg) %in% c("rmst","survival")))c$analysis_tau<-cfg$analysis_tau
  if(batch_is_sequential(cfg)) {
    c$design_mode<-"sequential";c$cut_mode<-"target";c$target_values<-sc$cut_value_engine;c$dco_values<-NULL;c$primary<-"logrank";c$estimate_hr<-FALSE;c$miss_policy<-"no_reject"
    for(k in c("gs_timing","gs_spending","gs_futility","gs_futility_z","gs_beta","gs_beta_spending","gs_beta_gamma","gs_design_hr"))if(!is.null(cfg[[k]]))c[[k]]<-cfg[[k]]
    c$gs_plans<-if(is.null(cfg$batch_gs_plans))NULL else cfg$batch_gs_plans[as.character(sc$cut_value_engine)]
  }
  c
}
batch_analysis_cut <- function(truth,cfg,sc) {
  if(cfg$cut_rule=="fixed")return(list(day=sc$cut_value_engine,rule_reached=TRUE,target_reached=NA,target_day=NA_real_,last_entry_day=NA_real_,planned_rule_day=sc$cut_value_engine))
  if(cfg$cut_rule=="last_entry_followup") {
    last<-max(truth$entry_day);planned<-last+sc$cut_value_engine
    hit<-is.finite(planned)&&planned<=cfg$max_day
    return(list(day=if(hit)planned else cfg$max_day,rule_reached=hit,target_reached=NA,target_day=NA_real_,last_entry_day=last,planned_rule_day=planned))
  }
  times<-sort(truth$event_day[is.finite(truth$event_day)&truth$event_time_day<=truth$dropout_time_day&truth$event_day<=cfg$max_day])
  hit<-length(times)>=sc$cut_value_engine;target_day<-if(hit)times[sc$cut_value_engine] else NA_real_
  planned<-if(hit)max(target_day,if(cfg$cut_rule=="event_minimum")cfg$minimum_day else 0) else Inf
  reached<-hit&&planned<=cfg$max_day
  list(day=if(reached)planned else cfg$max_day,rule_reached=reached,target_reached=hit,target_day=target_day,last_entry_day=NA_real_,planned_rule_day=planned)
}
batch_trial <- function(cfg,sc,b,keep_sample=FALSE) {
  if(batch_is_sequential(cfg))return(batch_sequential_trial(cfg,sc,b,keep_sample))
  seed<-batch_replicate_seed(cfg$seed,sc$SCENARIO,b)
  batch_with_rng(seed,{
    stage<-"prior";hr<-sc$hr;scale<-sc$time_scale;prior_scale<-1
    empty_cfg<-batch_trial_config(cfg,sc,sc$hr,sc$time_scale);tc<-empty_cfg
    cell<-data.frame(SCENARIO=sc$SCENARIO,BASEID=sc$SCENARIO,n=sc$n,hr=if(batch_is_ph(cfg))sc$hr else NA_real_,profile_id=1L,effect_label=if(batch_is_ph(cfg))paste0("HR=",sc$hr) else as.character(sc$profile),hypothesis="unknown",true_effect=NA_real_,cut_value=if(cfg$cut_rule=="fixed")sc$cut_value_engine else cfg$max_day)
    out<-tryCatch({
      if(cfg$mode=="assurance") {
        hr<-exp(rnorm(1,log(sc$hr),cfg$prior_loghr_sd));prior_scale<-exp(rnorm(1,0,cfg$prior_logtime_sd));scale<-scale*prior_scale
      }
      if(!is.finite(hr)||hr<.01||hr>100||!is.finite(scale)||scale<=0)stop("先验抽样超出引擎支持范围；保留失败，不截断或重抽先验。")
      tc<-batch_trial_config(cfg,sc,hr,scale);validate_study_config(tc)
      cell$hr<-if(batch_is_ph(cfg))hr else NA_real_
      null<-if(batch_is_ph(cfg))hr==1 else all(tc$hr_profile==1)
      cell$hypothesis<-if(cfg$mode=="assurance")"prior_mixture" else if(null)"global_H0" else "global_H1"
      cell$effect_label<-if(batch_is_ph(cfg))paste0("HR=",hr) else paste0(sc$profile,":HR(t)=",paste(tc$hr_profile,collapse="→"))
      truth_effect<-batch_method_truth(tc,cell,cfg$primary);cell$true_effect<-truth_effect$value
      if(cfg$primary %in% c("rmst","survival")&&cfg$mode!="assurance")cell$hypothesis<-if(!is.finite(cell$true_effect))"unknown" else if(abs(cell$true_effect)<=1e-10*max(1,if(cfg$primary=="rmst")tc$analysis_tau else 1))"contrast_H0" else "contrast_H1"
      stage<-"generation";truth<-simulate_survival_truth(study_trial_config(tc,cell),b)
      cut<-batch_analysis_cut(truth,cfg,sc);stage<-"analysis";tc$dco_values<-cut$day;cell$cut_value<-cut$day
      row<-study_trial_row(truth,tc,cell,b)
      row$target_reached<-cut$target_reached;row$target_day<-cut$target_day
      row$rule_reached<-cut$rule_reached;row$planned_rule_day<-cut$planned_rule_day;row$last_entry_day<-cut$last_entry_day
      row$formal_test_performed<-cut$rule_reached||cfg$miss_policy=="analyze"
      if(!cut$rule_reached&&cfg$miss_policy=="no_reject"){row$decision_valid<-TRUE;row$decision_reject<-FALSE}
      row$failure_stage<-"";row$replicate_seed<-seed
      list(row=row,truth=if(keep_sample)truth else NULL,observed=survival_at_cut(truth,cut$day,1L),study_config=tc)
    },error=function(e){
      row<-study_failed_rows(cell,b,empty_cfg,conditionMessage(e));row$true_hr<-if(batch_is_ph(cfg)&&is.finite(hr))hr else NA_real_
      row$rule_reached<-NA;row$planned_rule_day<-NA_real_;row$last_entry_day<-NA_real_;row$formal_test_performed<-NA
      row$failure_stage<-stage;row$replicate_seed<-seed;list(row=row)
    })
    out$row$label<-sc$label;out$row$mode<-cfg$mode;out$row$configured_hr<-sc$hr
    out$row$effect_mode<-cfg$effect_mode %||% "ph";out$row$profile<-if(batch_is_ph(cfg))"PH" else as.character(sc$profile)
    out$row$drawn_effect_scale<-hr;out$row$time_scale<-scale;out$row$prior_time_scale<-prior_scale;out$row$cut_rule<-cfg$cut_rule
    out$row$configured_cut_value<-sc$cut_value
    out$method_rows<-batch_method_rows(out$row,out$observed,tc,cell,sc,cfg,b)
    if(!keep_sample){out$observed<-NULL;out$study_config<-NULL}
    out
  })
}
batch_overview <- function(rows,cfg,sc=batch_scenarios(cfg)) {
  do.call(rbind,lapply(seq_len(nrow(sc)),function(j) {
    cell<-sc[j,,drop=FALSE];x<-if(nrow(rows))rows[rows$SCENARIO==cell$SCENARIO,,drop=FALSE] else data.frame()
    processed<-nrow(x);valid<-if(processed)x$decision_valid %in% TRUE else logical();v<-sum(valid)
    k<-if(processed)sum(x$decision_reject[valid] %in% TRUE) else 0L;unknown<-cfg$reps-v
    p<-if(v)k/v else NA_real_;w<-study_wilson(k,v)
    generated<-if(processed)x$generated %in% TRUE else logical()
    cv<-if(processed)x$cox_valid %in% TRUE else logical();cover<-if(any(cv))x$covered[cv&!is.na(x$covered)] else logical()
    beta_error<-if(any(cv))x$beta[cv]-log(x$true_hr[cv]) else numeric()
    data.frame(SCENARIO=cell$SCENARIO,label=cell$label,mode=cfg$mode,n=cell$n,configured_hr=cell$hr,effect_mode=cfg$effect_mode %||% "ph",profile=if(batch_is_ph(cfg))"PH" else as.character(cell$profile),
      time_scale=cell$time_scale,enroll_scale=cell$enroll_scale,dropout_scale=cell$dropout_scale,configured_cut_value=cell$cut_value,
      requested=cfg$reps,processed=processed,not_run=cfg$reps-processed,valid=v,invalid_processed=processed-v,
      generation_failures=if(processed)sum(!generated) else 0L,successes=k,
      success_lower_all=k/cfg$reps,success_upper_all=(k+unknown)/cfg$reps,
      success_conditional=p,mcse=if(v)sqrt(p*(1-p)/v) else NA_real_,wilson_lower=w[1],wilson_upper=w[2],
      mean_events=if(any(generated))mean(x$events[generated]) else NA_real_,
      mean_dco_day=if(any(generated))mean(x$DCO_DAY[generated]) else NA_real_,
      rule_reached=if(any(generated))mean(x$rule_reached[generated]) else NA_real_,
      cox_valid=sum(cv),beta_bias=if(length(beta_error))mean(beta_error) else NA_real_,
      beta_rmse=if(length(beta_error))sqrt(mean(beta_error^2)) else NA_real_,
      coverage=if(length(cover))mean(cover) else NA_real_,coverage_n=length(cover),
      mean_drawn_hr=if(any(generated))mean(x$true_hr[generated]) else NA_real_)
  }))
}
batch_comparisons <- function(overview,reference) {
  if(!reference %in% overview$SCENARIO)return(data.frame())
  a<-overview[overview$SCENARIO==reference,,drop=FALSE]
  d<-overview[overview$SCENARIO!=reference,,drop=FALSE];if(!nrow(d))return(data.frame())
  data.frame(reference=reference,SCENARIO=d$SCENARIO,label=d$label,
    difference_valid=d$success_conditional-a$success_conditional,
    independent_mcse=sqrt(d$mcse^2+a$mcse^2),
    difference_lower_all=d$success_lower_all-a$success_upper_all,
    difference_upper_all=d$success_upper_all-a$success_lower_all)
}
batch_candidates <- function(overview,cfg) {
  if(!isTRUE(cfg$filter_enabled))return(data.frame())
  d<-overview
  d$criterion_value<-if(cfg$filter_basis=="request_lower")d$success_lower_all else d$wilson_lower
  d$threshold<-cfg$success_threshold
  d$meets_threshold<-ifelse(d$processed<d$requested,NA,d$criterion_value>=d$threshold)
  d$basis<-cfg$filter_basis
  d[,c("SCENARIO","label","n","configured_cut_value","mean_events","mean_dco_day","processed","requested","criterion_value","threshold","meets_threshold","basis"),drop=FALSE]
}
run_batch_research <- function(cfg,progress=function(state)NULL,should_cancel=function()FALSE,replay_keys=NULL,resume_state=NULL,continuation_operation="resume") {
  if(ma_is(cfg))return(run_multiarm(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(ds_is(cfg))return(run_design_search(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(fp_is(cfg))return(run_flexible_prediction(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(ob_is(cfg))return(run_observation(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(ms_is(cfg))return(run_multistate(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(je_is(cfg))return(run_joint_sequential(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(sp_is(cfg))return(run_shared_adaptation(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(cb_is(cfg))return(run_conditional_batch(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(hg_is(cfg))return(run_heterogeneity(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(abr_is(cfg))return(run_adaptive_batch(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  if(jr_is(cfg))return(run_joint_research(cfg,progress,should_cancel,replay_keys,resume_state,continuation_operation))
  validate_batch_config(cfg);cfg<-batch_prepare_sequential(cfg);sc<-batch_scenarios(cfg)
  if(!is.null(resume_state)&&!is.null(replay_keys))stop("续跑与按键重放不能同时使用。")
  if(!is.null(resume_state))batch_validate_continuation(resume_state,cfg,continuation_operation)
  seed_rows<-function(k){d<-resume_state[[k]];if(is.data.frame(d)&&nrow(d))list(d) else list()}
  rows<-seed_rows("rows");method_rows<-seed_rows("method_rows");look_rows<-seed_rows("looks")
  handled<-matrix(FALSE,nrow(sc),cfg$reps)
  if(length(rows))handled[cbind(rows[[1]]$SCENARIO,rows[[1]]$SIMID)]<-TRUE
  initial_completed<-sum(handled);done<-initial_completed;cancelled<-FALSE;next_publish<-Sys.time()
  source_hash<-batch_source_hash();created<-format(Sys.time(),tz="UTC",usetz=TRUE);deps<-batch_dependencies(cfg)
  bind<-function(x){d<-do.call(rbind,x);if(is.null(d))data.frame() else {rownames(d)<-NULL;d}}
  snapshot<-function(status){d<-bind(rows);ov<-batch_overview(d,cfg,sc)
    batch_attach_method_outputs(list(config=cfg,scenarios=sc,rows=d,method_rows=bind(method_rows),looks=bind(look_rows),overview=ov,
      comparisons=batch_comparisons(ov,cfg$reference_scenario),candidates=batch_candidates(ov,cfg),completed=done,total=nrow(sc)*cfg$reps,status=status,
      version=batch_version,review_status="pending_v0.35_review",schema="event_pred.batch_research.v4",created_at=created,dependencies=deps,source_hash=source_hash,
      initial_completed=initial_completed,resumed_from_run_id=resume_state$run_id %||% "",continuation_operation=if(is.null(resume_state))"new" else continuation_operation))}
  for(j in seq_len(nrow(sc))) {
    for(b in seq_len(cfg$reps)) {
      if(handled[j,b])next
      if(!is.null(replay_keys)&&!any(replay_keys$SCENARIO==j&replay_keys$SIMID==b))next
      if(should_cancel()){cancelled<-TRUE;break}
      a<-batch_trial(cfg,sc[j,,drop=FALSE],b);rows[[length(rows)+1L]]<-a$row;method_rows[[length(method_rows)+1L]]<-a$method_rows
      if(!is.null(a$looks)&&nrow(a$looks))look_rows[[length(look_rows)+1L]]<-a$looks
      done<-done+1L;handled[j,b]<-TRUE
      if(done==initial_completed+1L||b==cfg$reps||Sys.time()>=next_publish){progress(snapshot("running"));next_publish<-Sys.time()+2}
    }
    if(cancelled)break
  }
  snapshot(if(cancelled)"cancelled" else if(!is.null(replay_keys)&&done<nrow(sc)*cfg$reps)"partial_replay" else "complete")
}

batch_sample <- function(r,scenario,replicate) {
  if(fp_is(r$config))return(fp_sample(r,scenario,replicate))
  if(ob_is(r$config))return(ob_sample(r,scenario,replicate))
  if(ms_is(r$config))return(ms_sample(r,scenario,replicate))
  if(je_is(r$config))return(je_sample(r,scenario,replicate))
  if(sp_is(r$config))return(sp_sample(r,scenario,replicate))
  if(cb_is(r$config))return(cb_sample(r,scenario,replicate))
  if(hg_is(r$config))return(hg_sample(r,scenario,replicate))
  if(abr_is(r$config))return(abr_sample(r,scenario,replicate))
  if(jr_is(r$config))return(jr_sample(r,scenario,replicate))
  sc<-r$scenarios[r$scenarios$SCENARIO==scenario,,drop=FALSE]
  if(nrow(sc)!=1||!nrow(r$rows)||!any(r$rows$SCENARIO==scenario&r$rows$SIMID==replicate&r$rows$generated))stop("请选择已生成的情景和轮次。")
  out<-batch_trial(r$config,sc,replicate,TRUE)
  if(is.null(out$observed))stop("选定轮次未能重新生成观察数据。")
  out$observed$SCENARIO<-scenario;out$truth$SCENARIO<-scenario;out
}
batch_report <- function(r) {
  if(fp_is(r$config))return(fp_report(r))
  if(ob_is(r$config))return(ob_report(r))
  if(ms_is(r$config))return(ms_report(r))
  if(je_is(r$config))return(je_report(r))
  if(sp_is(r$config))return(sp_report(r))
  if(cb_is(r$config))return(cb_report(r))
  if(hg_is(r$config))return(hg_report(r))
  if(abr_is(r$config))return(abr_report(r))
  if(jr_is(r$config))return(jr_report(r))
  tab<-function(d)capture.output(print(d,row.names=FALSE))
  c("# 批量设计研究 / assurance（开发稿，待复核）","",paste0("版本：",r$version,"；保存时间：",r$created_at,"；任务状态：",r$status),
    paste0("模式：",r$config$mode,"；每情景请求：",r$config$reps,"；处理轮次：",r$completed,"/",r$total),
    "","## 已提交设计","","```json",jsonlite::toJSON(r$config,auto_unbox=TRUE,pretty=TRUE,digits=NA),"```",
    "","## 原情景表","","```text",tab(r$scenarios),"```","","## 结果与分母","","```text",tab(r$overview),"```",
    "","## 相对参照比较","","```text",tab(r$comparisons),"```","","## 分析方法汇总","","```text",tab(r$method_overview),"```","","## 组序贯原计划","","```text",tab(r$gs_plans),"```","","## 停止路径与资源","","```text",tab(r$stops),tab(r$resources),"```","","## 同轮方法配对比较","","```text",tab(r$method_pairs),"```","","## 预定候选筛选","","```text",tab(r$candidates),"```",
    "","固定真值报告给定生成参数下的拒绝概率；assurance每轮抽取一组logHR与基准时间尺度真值，供该轮全部患者共享，再按预定单侧检验计算成功。不是用当前IA后验预测，也不是先验中位参数对应的单一功效。",
    "情景独立生成，不把相同SIMID称为患者配对或公共随机数；差值MCSE按独立情景计算。未知/失败/取消轮次保留全部请求成功概率上下界；有效轮次估计与Wilson区间另列。",
    "候选筛选只比较预定计算阈值，不保证真实功效、唯一最优设计或设计搜索后的错误率。校准输入为指定总体生存率，不是个体资料拟合。",
    "固定真值可PH或分段HR；assurance仍限PH。预定主分析与附加方法比较共享同一轮观察数据，附加结果不重写主决策；未实现联合/择优拒绝的多重性规则。NPH Cox无单一HR真值，不报告其偏倚/覆盖。组序贯批量仅PH/log-rank、事件规则和预设无效停止；原D*固定Final参照共享潜在患者轨迹，仅模拟比较，不改变主决策。停止阶段名义p不是调整p。IA后自适应、联合终点仍未接入；代码/方法/结果/界面/导出及公式显示尚未验证。")
}
batch_reproduction_script <- function(r) {
  if(fp_is(r$config))return(research_replay_script(r,"run_flexible_prediction"))
  if(ob_is(r$config))return(research_replay_script(r,"run_observation"))
  if(ms_is(r$config))return(research_replay_script(r,"run_multistate"))
  if(je_is(r$config))return(research_replay_script(r,"run_joint_sequential"))
  if(sp_is(r$config))return(research_replay_script(r,"run_shared_adaptation"))
  if(cb_is(r$config))return(research_replay_script(r,"run_conditional_batch"))
  if(hg_is(r$config))return(research_replay_script(r,"run_heterogeneity"))
  if(abr_is(r$config))return(abr_reproduction_script(r))
  if(jr_is(r$config))return(jr_reproduction_script(r))
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  keys<-if(nrow(r$rows))r$rows[,c("SCENARIO","SIMID"),drop=FALSE] else data.frame(SCENARIO=integer(),SIMID=integer())
  c("# event_pred v0.30.0 development draft; replay not yet verified.",
    'if (!exists("%||%", mode="function")) `%||%` <- function(x,y) if (is.null(x)) y else x',
    'for (f in c("units","models","forecast","inputs","simulation","nph","sequential","study","parameter_calibration","batch_research","batch_analysis","batch_sequential","batch_workspace","joint_survival","joint_research","joint_marginal","adaptation","patient_adaptation","adaptive_batch")) source(paste0("R/",f,".R"))',
    paste0('if(as.character(getRversion())!="',r$dependencies$R,'")stop("R version differs")'),
    paste0('if(as.character(packageVersion("survival"))!="',r$dependencies$survival,'")stop("survival version differs")'),
    if(!is.null(r$source_hash))paste0('if(batch_source_hash()!="',r$source_hash,'")stop("Simulation source differs")'),
    paste0("cfg <- ",dump(r$config)),
    if(batch_is_sequential(r$config))paste0('if(as.character(packageVersion("rpact"))!="',r$dependencies$rpact,'")stop("rpact version differs")'),
    if(batch_is_sequential(r$config))paste0('if(as.character(packageVersion("mvtnorm"))!="',r$dependencies$mvtnorm,'")stop("mvtnorm version differs")'),paste0("saved_keys <- ",dump(keys)),
    'result <- run_batch_research(cfg,replay_keys=saved_keys)',
    'saveRDS(result,"batch_research_replayed.rds")',
    'write.csv(result$rows,"batch_trials_replayed.csv",row.names=FALSE)',
    'write.csv(result$overview,"batch_overview_replayed.csv",row.names=FALSE)',
    'write.csv(result$comparisons,"batch_comparisons_replayed.csv",row.names=FALSE)',
    'write.csv(result$method_rows,"batch_methods_replayed.csv",row.names=FALSE)',
    'write.csv(result$method_overview,"batch_method_overview_replayed.csv",row.names=FALSE)',
    'write.csv(result$method_pairs,"batch_method_pairs_replayed.csv",row.names=FALSE)',
    'write.csv(result$looks,"batch_looks_replayed.csv",row.names=FALSE)',
    'write.csv(result$stops,"batch_stops_replayed.csv",row.names=FALSE)',
    'write.csv(result$resources,"batch_resources_replayed.csv",row.names=FALSE)')
}
