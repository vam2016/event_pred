if(!exists("gs_ordering_tails",mode="function"))source("R/sequential_ordering.R",local=TRUE)
if(!exists("gs_binding_repeated",mode="function"))source("R/sequential_repeated_binding.R",local=TRUE)
# Observed cumulative log-rank paths; event-information PH approximation.
# Inference is computed on demand, never during generation of simulated trials.
gs_inference <- function(cfg,plan,path,look) {
  if(!identical(cfg$design_mode,"sequential"))stop("组序贯推断需要已保存的PH/log-rank组序贯结果。")
  if(length(look)!=1||!is.finite(look)||!look %in% plan$look)stop("请选择已完成且有效的分析。")
  if(!cfg$sided %in% c("benefit","two")||!cfg$gs_spending %in% c("asOF","asP")||!cfg$gs_futility %in% c("none","z","beta")||length(cfg$alpha)!=1||!is.finite(cfg$alpha)||cfg$alpha<.0001||cfg$alpha>.2)stop("原设计方向、alpha消耗或无效规则无效。")
  need<-c("look","target_events","information_fraction","upper_z","lower_efficacy_z","futility_z","alpha_spent")
  if(!all(need %in% names(plan))||nrow(plan)<2||nrow(plan)>5||!identical(as.numeric(plan$look),as.numeric(seq_len(nrow(plan))))||any(!is.finite(plan$target_events)|plan$target_events!=floor(plan$target_events)|plan$target_events<=0)||is.unsorted(plan$target_events,strictly=TRUE))stop("原计划需2–5次连续原分析和递增整数事件目标。")
  if(any(!is.finite(plan$information_fraction)|plan$information_fraction<.1|plan$information_fraction>1)||is.unsorted(plan$information_fraction,strictly=TRUE)||tail(plan$information_fraction,1)!=1)stop("原计划信息比例无效。")
  look<-as.integer(look)
  if(!all(c("look","performed","valid","target_reached","benefit_z","events","action","SCENARIO","SIMID","information_fraction","upper_z","lower_efficacy_z","futility_z") %in% names(path)))stop("逐次分析记录缺少必要字段。")
  # Discard all later records before validating or constructing the dataset.
  d<-path[path$look<=look,,drop=FALSE];d<-d[order(d$look),,drop=FALSE]
  if(nrow(d)!=look||!identical(as.numeric(d$look),as.numeric(seq_len(look)))||anyNA(d[,c("performed","valid","target_reached")])||!all(d$performed&d$valid&d$target_reached)||any(!is.finite(d$benefit_z)))stop("所选次序以前必须是连续、已执行且有效的分析。")
  if(length(unique(d$SCENARIO))!=1||length(unique(d$SIMID))!=1)stop("推断只能使用同一情景、同一试验的观察路径。")
  if(any(d$events!=plan$target_events[seq_len(look)]))stop("当前推断要求各次事件数与原计划事件目标一致；事件并列或计划变更需另行处理。")
  p<-cfg$treatment_fraction
  if(length(p)!=1||!is.finite(p)||p<=0||p>=1)stop("分配概率需在0与1之间。")
  side<-if(cfg$sided=="two")2L else 1L
  design<-gs_design_object(cfg,plan$information_fraction)
  same<-function(x,y)isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-8))
  if(!same(design$criticalValues,plan$upper_z)||!same(plan$information_fraction,plan$target_events/tail(plan$target_events,1))||!same(plan$alpha_spent,design$alphaSpent))stop("重建的原设计边界或信息比例不一致，不能进行调整推断。")
  expected_lower<-if(side==2L)-plan$upper_z else rep(-Inf,nrow(plan))
  if(!same(plan$lower_efficacy_z,expected_lower))stop("原反方向效力边界与检验方向不一致。")
  if(cfg$gs_futility=="none")expected_futility<-rep(-Inf,nrow(plan)) else if(gs_is_binding(cfg)) {
    expected_futility<-c(as.numeric(design$futilityBounds),-Inf)
    metadata<-gs_beta_plan_metadata(cfg,design,tail(plan$target_events,1))
    if(!all(names(metadata) %in% names(plan)))stop("保存的beta计划缺少设计参照字段。")
    for(name in names(metadata)) {
      expected_meta<-rep(metadata[[name]],length.out=nrow(plan))
      if(!isTRUE(all.equal(as.vector(plan[[name]]),as.vector(expected_meta),tolerance=1e-8)))stop("保存的beta计划元数据与原配置不一致。")
    }
  } else {
    if(side==2L)stop("当前双侧阶段排序仅支持不设无效边界的单区间继续规则。")
    # Older v0.17/v0.18 frozen exports stored the original Z values in the plan.
    values<-cfg$gs_futility_z
    if(is.null(values))values<-head(plan$futility_z,-1)
    if(length(values)!=nrow(plan)-1||any(!is.finite(values)|abs(values)>5))stop("原单侧无效Z界值无效。")
    expected_futility<-c(values,-Inf)
  }
  if(!same(plan$futility_z,expected_futility)||any(plan$futility_z>=plan$upper_z))stop("原无效界值与保存配置不一致。")
  ix<-seq_len(look)
  if(!same(d$information_fraction,plan$information_fraction[ix])||!same(d$upper_z,plan$upper_z[ix])||!same(d$lower_efficacy_z,plan$lower_efficacy_z[ix])||!same(d$futility_z,plan$futility_z[ix]))stop("观察记录与保存的原设计边界不一致。")
  expected<-vapply(ix,function(j)gs_action(d$benefit_z[j],TRUE,j,plan,cfg$sided),character(1))
  if(!identical(as.character(d$action),expected)||any(head(expected,-1)!="continue"))stop("观察路径与原停止规则不一致，或包含停止后的分析。")
  binding_repeated<-if(gs_is_binding(cfg))gs_binding_repeated(cfg,plan,d) else NULL
  repeated_available<-TRUE
  repeated_note<-if(!gs_is_binding(cfg))"重复推断按保存的原效力设计反演；每次分别报告，不取历史最小p。单侧alpha0.025与双侧alpha0.05对应双侧95%重复区间。" else "约束性beta重复推断采用完整原计划效力边界形状的重新校准带及前缀交集；单侧p与双侧区间分别校准。它不反演原beta停止决策，原效力/无效界和阶段排序结果保持。新增接口未验证。"
  if(!gs_is_binding(cfg)) {
    dataset<-rpact::getDataset(cumulativeEvents=d$events,cumulativeLogRanks=-d$benefit_z,cumulativeAllocationRatio=rep(p/(1-p),look))
    args<-list(design=design,dataInput=dataset,tolerance=1e-8)
    if(side==1L)args$directionUpper<-FALSE
    stage_args<-args;stage_args$tolerance<-NULL
    stage<-gs_preserve_rng(do.call(rpact::getStageResults,stage_args))
    repeated_p<-gs_preserve_rng(rpact::getRepeatedPValues(stage,tolerance=1e-8))
    repeated_ci<-gs_preserve_rng(do.call(rpact::getRepeatedConfidenceIntervals,args))
  } else {
    repeated_p<-binding_repeated$p
    repeated_ci<-rbind(binding_repeated$hr_lower,binding_repeated$hr_upper)
  }
  level<-if(side==1L)1-2*cfg$alpha else 1-cfg$alpha
  info<-d$events*p*(1-p)
  repeated<-data.frame(SCENARIO=d$SCENARIO,SIMID=d$SIMID,look=d$look,events=d$events,information_fraction=d$information_fraction,benefit_z=d$benefit_z,event_information=info,log_hr_approx=-d$benefit_z/sqrt(info),hr_approx=exp(-d$benefit_z/sqrt(info)),repeated_p=as.numeric(repeated_p[ix]),repeated_hr_lower=as.numeric(repeated_ci[1,ix]),repeated_hr_upper=as.numeric(repeated_ci[2,ix]),interval_level=level,action=d$action)
  if(!gs_is_binding(cfg)&&(any(!is.finite(repeated$repeated_p)|repeated$repeated_p<0|repeated$repeated_p>1)||anyNA(repeated$repeated_hr_lower)||anyNA(repeated$repeated_hr_upper)))stop("重复推断数值计算失败，请核对记录及依赖版本。")
  if(gs_is_binding(cfg)){
    repeated$interval_empty<-binding_repeated$empty;repeated$repeated_engine<-binding_repeated$engine
    repeated$band_scale<-binding_repeated$scale;repeated$band_full_plan_crossing<-binding_repeated$coverage_crossing
    repeated$original_beta_decision<-d$action;repeated$repeated_matches_original_test<-FALSE
  }
  terminal<-tail(expected,1) %in% c("efficacy_benefit","efficacy_reverse","futility","final_no_reject")
  final_available<-terminal
  reason<-if(!terminal)"所选分析当时仍继续试验；阶段排序结果只在实际停止或计划最终分析提供。" else if(gs_is_binding(cfg))"阶段排序按原约束性beta规则实际执行的停止路径计算；效力界和无效界均固定。新增分支待复核，不改变原停止/拒绝决策。" else if(cfg$gs_futility!="none")"阶段排序按原无效Z规则实际执行的停止路径计算；单侧p为获益方向尾概率，区间为双侧。该推断不改变原停止/拒绝决策；忽略无效规则继续试验需另行处理。" else if(side==2L)"双侧有方向阶段排序：先计算获益/反方向尾概率，再取两倍较小值；中位无偏HR和双侧区间通过同一排序反演。" else "使用原效力停止规则的阶段排序；不包含计划变更或窗口关闭。"
  final<-data.frame(SCENARIO=d$SCENARIO[look],SIMID=d$SIMID[look],look=look,events=d$events[look],action=expected[look],available=final_available,adjusted_p=NA_real_,median_unbiased_hr=NA_real_,adjusted_hr_lower=NA_real_,adjusted_hr_upper=NA_real_,interval_level=level,reason=reason)
  if(final_available&&side==1L&&cfg$gs_futility=="none") {
    final_p<-gs_preserve_rng(rpact::getFinalPValue(stage))
    final_ci<-gs_preserve_rng(do.call(rpact::getFinalConfidenceInterval,args))
    if(!identical(as.integer(final_p$finalStage),as.integer(look)))stop("原设计与推断引擎识别的停止次序不一致。")
    final$adjusted_p<-as.numeric(final_p$pFinal);final$median_unbiased_hr<-as.numeric(final_ci$medianUnbiased);final$adjusted_hr_lower<-as.numeric(final_ci$finalConfidenceInterval[1]);final$adjusted_hr_upper<-as.numeric(final_ci$finalConfidenceInterval[2])
    if(anyNA(final[,c("adjusted_p","median_unbiased_hr","adjusted_hr_lower","adjusted_hr_upper")])||final$adjusted_p<0||final$adjusted_p>1)stop("阶段排序推断数值计算失败。")
  }
  final$upper_tail_null<-NA_real_;final$lower_tail_null<-NA_real_;final$interval_tail_alpha<-if(side==2L)cfg$alpha/2 else cfg$alpha
  final$ordering<-"signed_counterclockwise_stagewise";final$root_probability_error<-NA_real_
  final$engine<-if(side==1L&&cfg$gs_futility=="none")"rpact_native_stagewise" else "rpact_boundary_probability_inversion"
  final$p_agrees_original_decision<-NA
  if(final_available) {
    lower<-if(side==2L)plan$lower_efficacy_z[ix] else plan$futility_z[ix]
    if(side==1L&&cfg$gs_futility=="none") {
      tails<-gs_ordering_tails(0,info,lower,plan$upper_z[ix],d$benefit_z[look])
      final$upper_tail_null<-unname(tails['upper']);final$lower_tail_null<-unname(tails['lower'])
    } else {
      a<-gs_ordering_inference(info,lower,plan$upper_z[ix],d$benefit_z[look],cfg$alpha,cfg$sided)
      for(name in intersect(names(a),names(final)))final[[name]]<-a[[name]]
    }
    final$p_agrees_original_decision<-(final$adjusted_p<=cfg$alpha)==startsWith(final$action,"efficacy")
  }
  # Export only the prefix and minimal frozen design configuration.
  config<-cfg[c("design_mode","alpha","sided","gs_spending","gs_futility","treatment_fraction")]
  if(cfg$gs_futility=="z")config$gs_futility_z<-head(plan$futility_z,-1)
  if(gs_is_binding(cfg))for(name in intersect(c("gs_beta","gs_beta_spending","gs_beta_gamma","gs_design_hr","gs_beta_input"),names(cfg)))config[[name]]<-cfg[[name]]
  list(repeated=repeated,repeated_available=repeated_available,repeated_note=repeated_note,review_status=if(gs_is_binding(cfg))"pending_v0.35_review" else "previous_scope",final=final,path=d,plan=plan,config=config,look=as.integer(look),version="0.30.0",rpact_version=as.character(utils::packageVersion("rpact")),information_assumption="D*p*(1-p); PH/log-rank canonical normal approximation")
}
gs_inference_script <- function(r) {
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  c('# event_pred 0.30.0: observed-prefix group sequential inference',
    '# Run from the event_pred source root; requires rpact. No latent patient times are used.',
    'source("R/sequential.R"); source("R/sequential_inference.R")',
    paste0('required_rpact <- "',r$rpact_version,'"'),
    'if (as.character(packageVersion("rpact")) != required_rpact) stop("rpact version differs from the saved calculation")',
    paste0('cfg <- ',dump(r$config)),paste0('plan <- ',dump(r$plan)),paste0('observed_prefix <- ',dump(r$path)),paste0('selected_look <- ',r$look,'L'),
    'inference <- gs_inference(cfg, plan, observed_prefix, selected_look)',
    'write.csv(inference$repeated, "sequential_repeated_replayed.csv", row.names=FALSE)',
    'write.csv(inference$final, "sequential_final_replayed.csv", row.names=FALSE)')
}
