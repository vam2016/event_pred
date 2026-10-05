# Batch adapters for prespecified event-driven PH/log-rank sequential designs.
batch_is_sequential <- function(cfg) identical(cfg$design_mode %||% "fixed","sequential")
batch_dependencies <- function(cfg) {
  d<-list(R=as.character(getRversion()),survival=as.character(utils::packageVersion("survival")),rng=c("Mersenne-Twister","Inversion","Rejection"))
  if(batch_is_sequential(cfg)||(cb_is(cfg)&&cfg$branch=="existing_patients"&&cfg$payload$engine$prediction_design=="sequential")){d$rpact<-as.character(utils::packageVersion("rpact"));d$mvtnorm<-as.character(utils::packageVersion("mvtnorm"))}
  if((ma_is(cfg)&&cfg$multiplicity %in% c("dunnett","closed_dunnett"))||(ds_is(cfg)&&cfg$base$multiplicity %in% c("dunnett","closed_dunnett")))d$mvtnorm<-as.character(utils::packageVersion("mvtnorm"))
  if(cb_is(cfg)&&identical(cfg$payload$engine$uncertainty,"bayes_weibull"))d$posterior<-as.character(utils::packageVersion("posterior"))
  d
}
batch_prepare_sequential <- function(cfg) {
  if(ma_is(cfg)||ds_is(cfg)||cb_is(cfg)||hg_is(cfg)||sp_is(cfg)||ms_is(cfg)||je_is(cfg)||fp_is(cfg)||ob_is(cfg))return(cfg)
  if(!batch_is_sequential(cfg))return(cfg)
  sc<-batch_scenarios(cfg);plans<-list()
  for(d in unique(sc$cut_value_engine)) {
    a<-sc[match(d,sc$cut_value_engine),,drop=FALSE];tc<-batch_trial_config(cfg,a,a$hr,a$time_scale)
    tc$gs_plans<-if(is.null(cfg$batch_gs_plans))NULL else cfg$batch_gs_plans[as.character(d)]
    tc<-gs_preserve_rng(gs_prepare(tc));saved<-tc$gs_plans[[as.character(d)]];expected<-gs_preserve_rng(gs_plan(tc,d))
    if(!all(names(expected) %in% names(saved))||!isTRUE(all.equal(saved[,names(expected),drop=FALSE],expected,tolerance=1e-8,check.attributes=FALSE)))stop("保存的原组序贯边界与本次配置不符。")
    plans[[as.character(d)]]<-saved
  }
  cfg$batch_gs_plans<-plans;cfg
}
batch_sequential_plans <- function(cfg) {
  if(!batch_is_sequential(cfg))return(data.frame())
  do.call(rbind,lapply(names(cfg$batch_gs_plans),function(k)cbind(final_target=as.numeric(k),cfg$batch_gs_plans[[k]])))
}
batch_sequential_method <- function(row,cfg,sc,b,method="logrank") {
  generated<-isTRUE(row$generated);gs<-method=="logrank";valid<-if(gs)isTRUE(row$decision_valid)&&row$gs_stop_reason!="window_unreached" else isTRUE(row$logrank_valid)
  data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=row$replicate_seed,method=method,is_primary=gs,
    method_label=if(gs)"组序贯log-rank" else "原D*固定Final参照",effect_mode="ph",profile="PH",generated=generated,
    analysis_valid=valid,analysis_performed=isTRUE(row$formal_test_performed),decision_valid=isTRUE(row$decision_valid),decision_reject=if(isTRUE(row$decision_valid))isTRUE(row$decision_reject) else NA,
    p_value=if(gs)NA_real_ else row$logrank_p,stage_nominal_p=if(gs&&isTRUE(row$formal_test_performed)&&row$gs_stop_reason!="window_unreached")row$logrank_p else NA_real_,
    z=if(generated)row$logrank_z else NA_real_,benefit_z=if(generated)-row$logrank_z else NA_real_,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,true_effect=NA_real_,covered=NA,
    null_status=if(!generated)"unknown" else if(is.finite(row$true_hr)&&row$true_hr==1)"global_survival_null" else "alternative",
    analysis_tau_day=NA_real_,fh_rho=NA_real_,fh_gamma=NA_real_,effect_scale=row$drawn_effect_scale,baseline_time_scale=row$time_scale,estimate_unit="score_only",
    analysis_note=if(gs)row$gs_stop_reason else row$logrank_note,truth_note=if(gs)"停止时名义阶段p不是组序贯调整p；本表不提供调整p" else "同轮潜在轨迹上的预定一次Final参照",stringsAsFactors=FALSE)
}
batch_sequential_trial <- function(cfg,sc,b,keep_sample=FALSE) {
  seed<-batch_replicate_seed(cfg$seed,sc$SCENARIO,b)
  batch_with_rng(seed,{
    hr<-sc$hr;scale<-sc$time_scale;prior_scale<-1;stage<-"prior";tc<-batch_trial_config(cfg,sc,hr,scale)
    cell<-data.frame(SCENARIO=sc$SCENARIO,BASEID=sc$SCENARIO,n=sc$n,hr=hr,hypothesis=if(cfg$mode=="assurance")"prior_mixture" else if(hr==1)"H0" else "H1",cut_value=sc$cut_value_engine)
    truth<-NULL;o<-NULL;benchmark<-NULL
    out<-tryCatch({
      if(cfg$mode=="assurance"){hr<-exp(rnorm(1,log(sc$hr),cfg$prior_loghr_sd));prior_scale<-exp(rnorm(1,0,cfg$prior_logtime_sd));scale<-scale*prior_scale}
      if(!is.finite(hr)||hr<.01||hr>100||!is.finite(scale)||scale<=0)stop("先验抽样超出引擎范围；失败保留，不截断或重抽。")
      tc<-batch_trial_config(cfg,sc,hr,scale);validate_study_config(tc);cell$hr<-hr
      stage<-"generation";truth<-simulate_survival_truth(study_trial_config(tc,cell),b)
      stage<-"analysis";a<-gs_trial(truth,tc,cell,b);row<-a$row
      if(row$gs_stop_reason=="window_unreached"){row$logrank_valid<-FALSE;row$logrank_z<-NA_real_;row$logrank_p<-NA_real_;row$logrank_reject<-NA;row$logrank_note<-"窗口未达到下一分析目标，无新增正式检验"}
      row$rule_reached<-row$gs_stop_reason %in% c("efficacy_benefit","efficacy_reverse","futility","final_no_reject")
      row$planned_rule_day<-NA_real_;row$last_entry_day<-NA_real_;row$formal_test_performed<-row$gs_analysis_count>0
      o<-survival_at_cut(truth,row$DCO_DAY,1L)
      if(isTRUE(cfg$fixed_benchmark)) {
        benchmark<-tryCatch({c<-tc;c$design_mode<-"fixed";r<-study_trial_row(truth,c,cell,b);r$formal_test_performed<-isTRUE(r$target_reached);r},error=function(e){r<-study_failed_rows(cell,b,tc,conditionMessage(e));r$formal_test_performed<-FALSE;r})
      }
      list(row=row,looks=a$looks)
    },error=function(e){r<-gs_failed_rows(cell,b,tc,conditionMessage(e));r$rule_reached<-NA;r$planned_rule_day<-NA_real_;r$last_entry_day<-NA_real_;r$formal_test_performed<-NA;list(row=r,looks=gs_empty_looks())})
    decorate<-function(row){row$replicate_seed<-seed;row$true_hr<-if(is.finite(hr))hr else NA_real_;row$label<-sc$label;row$mode<-cfg$mode;row$configured_hr<-sc$hr;row$effect_mode<-"ph";row$profile<-"PH";row$drawn_effect_scale<-hr;row$time_scale<-scale;row$prior_time_scale<-prior_scale;row$cut_rule<-"target";row$configured_cut_value<-sc$cut_value;row$failure_stage<-if(isTRUE(row$generated))"" else stage;row}
    out$row<-decorate(out$row);out$method_rows<-batch_sequential_method(out$row,cfg,sc,b)
    if(isTRUE(cfg$fixed_benchmark)) {
      if(is.null(benchmark)){benchmark<-study_failed_rows(cell,b,tc,out$row$generation_note);benchmark$formal_test_performed<-FALSE}
      benchmark<-decorate(benchmark);out$method_rows<-rbind(out$method_rows,batch_sequential_method(benchmark,cfg,sc,b,"fixed_final"))
      out$benchmark<-benchmark
    }
    if(nrow(out$looks))out$looks$replicate_seed<-seed
    if(keep_sample){out$truth<-truth;out$observed<-o;out$study_config<-tc}
    out
  })
}
batch_stop_outputs <- function(state) {
  cfg<-state$config;sc<-state$scenarios;rows<-state$rows;stops<-list();resources<-list()
  actions<-c("efficacy_benefit","efficacy_reverse","futility","final_no_reject","window_unreached")
  for(id in sc$SCENARIO) {
    x<-if(nrow(rows))rows[rows$SCENARIO==id,,drop=FALSE] else data.frame();n<-nrow(x)
    known<-if(n)x$decision_valid %in% TRUE else logical();v<-sum(known);generated<-if(n)x$generated %in% TRUE else logical()
    for(action in actions)for(look in seq_along(cfg$gs_timing)) {
      k<-if(n)sum(known&x$gs_stop_reason==action&x$gs_stop_look==look,na.rm=TRUE) else 0L
      stops[[length(stops)+1L]]<-data.frame(SCENARIO=id,label=sc$label[match(id,sc$SCENARIO)],look=look,action=action,requested=cfg$reps,processed=n,known_decisions=v,stopped=k,probability_lower_all=k/cfg$reps,probability_upper_all=(k+cfg$reps-v)/cfg$reps,probability_valid=if(v)k/v else NA_real_)
    }
    quant<-function(k,p){a<-if(n)x[[k]][generated] else numeric();a<-a[is.finite(a)];if(length(a))unname(quantile(a,p)) else NA_real_}
    avg<-function(k){a<-if(n)x[[k]][generated] else numeric();a<-a[is.finite(a)];if(length(a))mean(a) else NA_real_}
    resources[[length(resources)+1L]]<-data.frame(SCENARIO=id,label=sc$label[match(id,sc$SCENARIO)],requested=cfg$reps,processed=n,resource_n=sum(generated),mean_observed=avg("n_observed"),mean_events=avg("events"),mean_looks=avg("gs_analysis_count"),mean_stop_day=avg("DCO_DAY"),stop_p05_day=quant("DCO_DAY",.05),stop_median_day=quant("DCO_DAY",.5),stop_p95_day=quant("DCO_DAY",.95),unknown_decisions=cfg$reps-v)
  }
  list(stops=do.call(rbind,stops),resources=do.call(rbind,resources))
}
batch_attach_sequential_outputs <- function(state) {
  state$looks<-state$looks %||% data.frame()
  if(!batch_is_sequential(state$config)){state$gs_plans<-data.frame();state$stops<-data.frame();state$resources<-data.frame();return(state)}
  a<-batch_stop_outputs(state);state$gs_plans<-batch_sequential_plans(state$config);state$stops<-a$stops;state$resources<-a$resources;state
}
batch_saved_cp <- function(state,scenario,replicate,look,future_hr,respect_futility=TRUE) {
  if(!batch_is_sequential(state$config))stop("请选择已保存的组序贯批量结果。")
  a<-state$looks[state$looks$SCENARIO==scenario&state$looks$SIMID==replicate&state$looks$look==look,,drop=FALSE]
  if(nrow(a)!=1||!isTRUE(a$valid)||a$action!="continue")stop("仅提供已有效计算且按原规则继续的IA条件功效。")
  sc<-state$scenarios[state$scenarios$SCENARIO==scenario,,drop=FALSE];plan<-state$config$batch_gs_plans[[as.character(sc$cut_value_engine)]]
  c<-gs_conditional_power(plan,look,a$benefit_z,future_hr,state$config$treatment_fraction,state$config$sided,respect_futility)
  data.frame(version=state$version,SCENARIO=scenario,SIMID=replicate,look=look,observed_benefit_z=a$benefit_z,observed_day=a$DCO_DAY,information_fraction=a$information_fraction,future_hr=future_hr,respect_futility=respect_futility,full_cp=c$full,final_only_cp=c$final_only,status=c$status,interpretation="Canonical PH approximation; diagnostic only; no adaptation decision")
}
