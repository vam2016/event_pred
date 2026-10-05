# Independent, prespecified patient cohorts; fixed inverse-normal combination.
abr_names <- c(original="原计划",bounded="上下限重估",promising="promising-zone重估")
abr_methods <- function(cfg)unique(c(cfg$primary,cfg$compare_rules))
abr_plan_fields <- c("n1","n2","d1","d_plan","d_max","hr_assumed","cp_target","cp_min")
abr_fields <- c("label",abr_plan_fields,"true_hr","time_scale","enroll_scale","dropout_scale")
abr_parse_plans <- function(text) {
  lines<-trimws(strsplit(text,"\n",fixed=TRUE)[[1]]);lines<-lines[nzchar(lines)]
  if(!length(lines)||length(lines)>12)stop("事件计划需1–12行。")
  out<-lapply(lines,function(line){v<-trimws(strsplit(line,"|",fixed=TRUE)[[1]]);if(length(v)!=2||!nzchar(v[1]))stop("每行：名称 | N1,N2,D1,Dplan,Dmax,HRassumed,CPtarget,CPmin。")
    x<-parse_unit_numbers(v[2]);if(length(x)!=8)stop("每个事件计划需8个数值。")
    d<-as.data.frame(as.list(setNames(x,abr_plan_fields)));d$plan<-v[1];d})
  d<-do.call(rbind,out);if(anyDuplicated(d$plan))stop("事件计划名称需唯一。");d
}
abr_scenarios <- function(cfg) {
  if(cfg$scenario_source=="csv"){
    d<-cfg$scenario_table;if(!is.data.frame(d)||!identical(sort(names(d)),sort(abr_fields)))stop(paste("情景CSV列名：",paste(abr_fields,collapse=",")))
    d<-d[,abr_fields,drop=FALSE]
  } else if(cfg$scenario_source=="grid"){
    axes<-cfg$axes;if(!identical(sort(names(axes)),sort(c("true_hr","time_scale","enroll_scale","dropout_scale"))))stop("重估研究的情景轴不完整。")
    plans<-cfg$plans;if(!is.data.frame(plans)||!identical(sort(names(plans)),sort(c("plan",abr_plan_fields))))stop("事件计划列不完整。")
    size<-prod(vapply(axes,length,integer(1)))*nrow(plans)
    if(!size||size>60||any(vapply(axes,length,integer(1))<1))stop("事件计划与全部列表组合需1–60情景。")
    d<-merge(plans,expand.grid(axes,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE),by=NULL,sort=FALSE);d$label<-paste0("S",seq_len(nrow(d)),"_",d$plan);d<-d[,abr_fields,drop=FALSE]
  } else stop("请选择列表组合或情景CSV。")
  if(!nrow(d)||nrow(d)>60||anyNA(d$label)||any(!nzchar(trimws(d$label)))||anyDuplicated(d$label))stop("情景需1–60个非空唯一名称。")
  for(k in setdiff(abr_fields,"label"))if(!is.numeric(d[[k]])||any(!is.finite(d[[k]])))stop(paste0(k,"需为有限数值。"))
  if(any(d$true_hr<.01|d$true_hr>5)||any(d$time_scale<.01|d$time_scale>100)||any(d$enroll_scale<.01|d$enroll_scale>100)||any(d$dropout_scale<0|d$dropout_scale>100))stop("真实HR为0.01–5，时间/入组倍数0.01–100，脱落倍数0–100。")
  if(cfg$enrollment=="batch"&&any(d$enroll_scale!=1))stop("阶段开始一次入组时，入组倍数须为1。")
  if(any(d$cp_min<=0|d$cp_min>=d$cp_target))stop("每行需0 < CPmin < CPtarget，CPmin定义promising规则。")
  if(anyDuplicated(d[,setdiff(abr_fields,"label"),drop=FALSE]))stop("情景参数不能完全重复。")
  d$SCENARIO<-seq_len(nrow(d));rownames(d)<-NULL;d
}
abr_trial_config <- function(cfg,sc) {
  z<-cfg$patient_base
  for(k in abr_plan_fields)z[[k]]<-sc[[k]]
  z$purpose<-"patient_simulation";z$rule<-"promising";z$hr_true<-sc$true_hr;z$reps<-20;z$seed<-cfg$seed
  z$control_model<-scale_parameter_model(z$control_model,sc$time_scale)
  z$dropout_rates<-z$dropout_rates*sc$dropout_scale
  if(z$enrollment=="constant"){z$rate1<-z$rate1*sc$enroll_scale;z$rate2<-z$rate2*sc$enroll_scale}
  z
}
validate_adaptive_batch <- function(cfg) {
  scalar<-function(x,lo,hi,int=FALSE)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi&&(!int||x==floor(x))
  if(!abr_is(cfg)||!identical(cfg$mode,"fixed_truth")||!identical(cfg$design_mode,"fixed")||!identical(cfg$sided,"benefit"))stop("重估批量研究限定固定真值、独立两队列及单侧获益组合检验。")
  if(!scalar(cfg$reps,20,10000,TRUE)||!scalar(cfg$seed,0,.Machine$integer.max,TRUE))stop("重复数20–10000，种子0–2147483647整数。")
  if(length(cfg$primary)!=1||!cfg$primary %in% names(abr_names)||any(!abr_methods(cfg) %in% names(abr_names)))stop("请选择预定主规则和支持的比较规则。")
  time_factor(cfg$display_unit)
  if(length(cfg$origin)!=1||is.na(as.Date(cfg$origin))||!cfg$paramcd %in% c("PFS","OS"))stop("选择有效研究起点和PFS/OS终点。")
  sc<-abr_scenarios(cfg)
  for(j in seq_len(nrow(sc))){c<-abr_trial_config(cfg,sc[j,,drop=FALSE]);patient_adaptive_validate(c)
    if(!identical(c$p,cfg$treatment_fraction)||!identical(c$alpha,cfg$alpha)||!identical(c$enrollment,cfg$enrollment))stop("共同设计参数与患者参数不一致。")}
  if(nrow(sc)*cfg$reps>100000||sum(sc$n1+sc$n2)*cfg$reps>3e7||nrow(sc)*cfg$reps*length(abr_methods(cfg))>500000)stop("最多10万轮、3000万患者×轮次、50万规则×轮次。")
  invisible(TRUE)
}
abr_trial <- function(cfg,sc,b,keep_sample=FALSE) {
  seed<-batch_replicate_seed(cfg$seed,sc$SCENARIO,b)
  batch_with_rng(seed,{
    failed<-function(message){mr<-do.call(rbind,lapply(abr_methods(cfg),function(m)data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,method=m,is_primary=m==cfg$primary,generated=FALSE,decision_valid=FALSE,decision_reject=NA,action="generation_failed",note=message)))
      list(row=data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,true_hr=sc$true_hr,generated=FALSE,decision_valid=FALSE,decision_reject=NA,generation_note=message),method_rows=mr,looks=data.frame())}
    tryCatch({
      c<-abr_trial_config(cfg,sc);a<-patient_adaptive_plan(c)
      truth1<-patient_adaptive_truth(c,1,sc$true_hr,b);first<-patient_adaptive_cut(truth1,c$d1,c$window1,1L)
      s1<-if(first$hit)patient_adaptive_score(first$observed) else list(valid=FALSE,z=NA_real_,variance=NA_real_)
      truth2<-if(first$hit&&s1$valid)patient_adaptive_truth(c,2,sc$true_hr,b) else NULL
      mr<-list();paths<-list();observations<-list()
      for(m in abr_methods(cfg)){
        rule<-c;rule$purpose<-"interim";rule$z1<-0;rule$rule<-if(m=="promising")"promising" else "bounded"
        d<-if(first$hit&&s1$valid)adaptive_decision(s1$z,rule) else NULL
        target<-if(m=="original"||is.null(d))a$d2_min else d$selected_d2
        second<-if(!is.null(truth2))patient_adaptive_cut(truth2,target,c$window2,2L) else NULL
        s2<-if(!is.null(second)&&second$hit)patient_adaptive_score(second$observed) else list(valid=FALSE,z=NA_real_,variance=NA_real_)
        action<-if(!first$hit)"stage1_window_unreached" else if(!s1$valid)"stage1_invalid" else if(!second$hit)"stage2_window_unreached" else if(!s2$valid)"stage2_invalid" else "tested"
        valid<-!action %in% c("stage1_invalid","stage2_invalid");combined<-if(action=="tested")a$w1*s1$z+a$w2*s2$z else NA_real_
        cp<-if(is.null(d))NA_real_ else if(m=="original")d$cp_original else d$cp_selected
        wait2<-if(is.null(second))0 else second$day;final_day<-first$day+wait2
        mr[[m]]<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,method=m,is_primary=m==cfg$primary,generated=TRUE,true_hr=sc$true_hr,null_status=if(sc$true_hr==1)"global_survival_null" else "HR_scenario",
          decision_valid=valid,decision_reject=if(!valid)NA else if(action=="tested")combined>=a$critical else FALSE,action=action,analysis_performed=action=="tested",
          z1=s1$z,variance1=s1$variance,z2=s2$z,variance2=s2$variance,combination_z=combined,combination_p=if(is.finite(combined))pnorm(combined,lower.tail=FALSE) else NA_real_,w1=a$w1,w2=a$w2,critical=a$critical,
          conditional_error=if(is.null(d))NA_real_ else d$conditional_error,cp_original=if(is.null(d))NA_real_ else d$cp_original,cp_max=if(is.null(d))NA_real_ else d$cp_max,cp_selected=cp,required_d2=if(is.null(d)||m=="original")NA_real_ else d$required_d2,
          selected_d2=target,combined_target_events=c$d1+target,increased=if(is.null(d))NA else target>a$d2_min,cp_target_met=if(is.finite(cp))cp>=c$cp_target-1e-12 else NA,
          rule_reason=if(is.null(d))"stage1_not_analyzed" else if(m=="original")"original_plan" else d$reason,
          stage1_hit=first$hit,stage2_hit=if(is.null(second))NA else second$hit,n1_observed=nrow(first$observed),n2_observed=if(is.null(second))0L else nrow(second$observed),events1=sum(first$observed$event),events2=if(is.null(second))0L else sum(second$observed$event),
          ia_day=first$day,stage2_day=if(is.null(second))NA_real_ else second$day,final_day=final_day,final_date=as.character(as.Date(cfg$origin)+floor(final_day)),note="")
        paths[[length(paths)+1L]]<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,method=m,stage=1L,target=c$d1,reached=first$hit,analysis_valid=s1$valid,benefit_z=s1$z,variance=s1$variance,n=nrow(first$observed),events=sum(first$observed$event),stage_day=first$day,research_day=first$day)
        if(!is.null(second))paths[[length(paths)+1L]]<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,method=m,stage=2L,target=target,reached=second$hit,analysis_valid=s2$valid,benefit_z=s2$z,variance=s2$variance,n=nrow(second$observed),events=sum(second$observed$event),stage_day=second$day,research_day=final_day)
        if(keep_sample){o<-first$observed;if(!is.null(second)){v<-second$observed;for(k in c("entry_day","obs_day","DCO_DAY"))v[[k]]<-v[[k]]+first$day;o<-rbind(o,v)};o$method<-m;o$SCENARIO<-sc$SCENARIO;observations[[m]]<-o}
      }
      methods<-do.call(rbind,mr);primary<-methods[methods$is_primary,,drop=FALSE]
      row<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,true_hr=sc$true_hr,generated=TRUE,decision_valid=primary$decision_valid,decision_reject=primary$decision_reject,stage1_hit=first$hit,z1=s1$z,variance1=s1$variance,ia_day=first$day,primary_action=primary$action,selected_d2=primary$selected_d2,final_day=primary$final_day,generation_note="")
      out<-list(row=row,method_rows=methods,looks=do.call(rbind,paths))
      if(keep_sample){truth1$cohort<-1L;truth1$stage_entry_day<-truth1$entry_day;truth1$stage_event_day<-truth1$event_day
        if(!is.null(truth2)){truth2$cohort<-2L;truth2$stage_entry_day<-truth2$entry_day;truth2$stage_event_day<-truth2$event_day;for(k in c("entry_day","event_day","dropout_day"))truth2[[k]]<-truth2[[k]]+first$day}
        out$truth<-batch_bind_columns(list(truth1,truth2));out$observed<-batch_bind_columns(observations);out$study_config<-c}
      out
    },error=function(e)failed(conditionMessage(e)))
  })
}
abr_aggregate <- function(state) {
  cfg<-state$config;sc<-state$scenarios;mr<-state$method_rows;rows<-state$rows
  overview<-list();metrics<-list();resources<-list();actions<-list();pairs<-list()
  for(id in sc$SCENARIO){s<-sc[sc$SCENARIO==id,,drop=FALSE];r<-if(nrow(rows))rows[rows$SCENARIO==id,,drop=FALSE] else rows
    overview[[length(overview)+1L]]<-cbind(s,jr_probability(if(nrow(r))ifelse(r$decision_valid,r$decision_reject,NA) else logical(),cfg$reps))
    for(m in abr_methods(cfg)){
      d<-if(nrow(mr))mr[mr$SCENARIO==id&mr$method==m,,drop=FALSE] else mr
      generated<-if(nrow(d))d$generated %in% TRUE else logical();g<-d[generated,,drop=FALSE]
      values<-list(reject=if(nrow(d))ifelse(d$decision_valid,d$decision_reject,NA) else logical())
      for(k in c("stage1_hit","stage2_hit","increased","cp_target_met"))values[[k]]<-if(k %in% names(d))d[[k]] else rep(NA,nrow(d))
      for(k in names(values))metrics[[length(metrics)+1L]]<-cbind(data.frame(SCENARIO=id,label=s$label,true_hr=s$true_hr,null_status=if(s$true_hr==1)"global_survival_null" else "HR_scenario",method=m,method_label=abr_names[[m]],is_primary=m==cfg$primary,metric=k),jr_probability(values[[k]],cfg$reps))
      avg<-function(k)if(nrow(g)&&k %in% names(g)&&any(is.finite(g[[k]])))mean(g[[k]][is.finite(g[[k]])]) else NA_real_
      quant<-function(k,p)if(nrow(g)&&k %in% names(g)&&any(is.finite(g[[k]])))as.numeric(quantile(g[[k]][is.finite(g[[k]])],p)) else NA_real_
      resources[[length(resources)+1L]]<-data.frame(SCENARIO=id,label=s$label,method=m,requested=cfg$reps,processed=nrow(d),generated=sum(generated),generation_failures=sum(!generated),mean_target_events=avg("combined_target_events"),target_p05=quant("combined_target_events",.05),target_median=quant("combined_target_events",.5),target_p95=quant("combined_target_events",.95),mean_n1=avg("n1_observed"),mean_n2=avg("n2_observed"),mean_events1=avg("events1"),mean_events2=avg("events2"),mean_ia_day=avg("ia_day"),mean_final_day=avg("final_day"),final_p05_day=quant("final_day",.05),final_median_day=quant("final_day",.5),final_p95_day=quant("final_day",.95),note="关闭时间包含窗口未达者；资源仅成功生成轮次。目标含未执行阶段的原定下限，请结合IA路径。")
      for(k in c("action","rule_reason"))for(v in if(k %in% names(d))unique(d[[k]][!is.na(d[[k]])]) else character())actions[[length(actions)+1L]]<-cbind(data.frame(SCENARIO=id,method=m,path_field=k,path_value=v),jr_probability(if(k %in% names(d))d[[k]]==v else logical(),cfg$reps))
    }
    if(nrow(mr))for(m in setdiff(abr_methods(cfg),cfg$primary)){
      d<-mr[mr$SCENARIO==id,,drop=FALSE];fields<-c("SIMID","decision_valid","decision_reject","generated","final_day","combined_target_events","stage2_hit")
      # Missing fields on all-failed snapshots remain unknown, rather than omitted.
      for(k in setdiff(fields,names(d)))d[[k]]<-NA
      z<-merge(d[d$method==cfg$primary,fields],d[d$method==m,fields],by="SIMID",suffixes=c("_primary","_comparison"))
      x<-ifelse(z$decision_valid_primary,z$decision_reject_primary,NA);y<-ifelse(z$decision_valid_comparison,z$decision_reject_comparison,NA)
      good<-!is.na(x)&!is.na(y);delta<-as.integer(y[good])-as.integer(x[good]);lo<-function(v)ifelse(is.na(v),0,as.integer(v));hi<-function(v)ifelse(is.na(v),1,as.integer(v))
      finite_pair<-z$generated_primary %in% TRUE&z$generated_comparison %in% TRUE&is.finite(z$final_day_primary)&is.finite(z$final_day_comparison)
      dt<-z$final_day_comparison[finite_pair]-z$final_day_primary[finite_pair]
      pairs[[length(pairs)+1L]]<-data.frame(SCENARIO=id,primary_rule=cfg$primary,comparison_rule=m,paired_valid=sum(good),paired_difference=if(length(delta))mean(delta) else NA_real_,paired_mcse=if(length(delta)>1)sd(delta)/sqrt(length(delta)) else NA_real_,difference_lower_all=(sum(lo(y)-hi(x))-(cfg$reps-nrow(z)))/cfg$reps,difference_upper_all=(sum(hi(y)-lo(x))+(cfg$reps-nrow(z)))/cfg$reps,resource_pairs=sum(finite_pair),mean_final_difference_day=if(length(dt))mean(dt) else NA_real_,final_difference_mcse_day=if(length(dt)>1)sd(dt)/sqrt(length(dt)) else NA_real_)
    }
  }
  state$overview<-do.call(rbind,overview);state$method_overview<-do.call(rbind,metrics);state$resources<-do.call(rbind,resources);state$stops<-batch_bind_columns(actions);state$method_pairs<-batch_bind_columns(pairs)
  state$comparisons<-data.frame();state$candidates<-data.frame();state$policy_overview<-data.frame();state$gs_plans<-data.frame();state
}
abr_initial <- function(cfg,initial=NULL)abr_aggregate(list(config=cfg,scenarios=abr_scenarios(cfg),rows=initial$rows %||% data.frame(),method_rows=initial$method_rows %||% data.frame(),looks=initial$looks %||% data.frame(),completed=initial$completed %||% 0L,total=nrow(abr_scenarios(cfg))*cfg$reps,status="running",version=batch_version,review_status="pending_v0.35_review",schema="event_pred.batch_research.v4",created_at=format(Sys.time(),tz="UTC",usetz=TRUE),dependencies=batch_dependencies(cfg),source_hash=batch_source_hash()))
run_adaptive_batch <- function(cfg,progress=function(state)NULL,should_cancel=function()FALSE,replay_keys=NULL,resume_state=NULL,continuation_operation="resume") {
  validate_adaptive_batch(cfg);if(!is.null(resume_state)){if(!is.null(replay_keys))stop("重放与续跑不能同时使用。");batch_validate_continuation(resume_state,cfg,continuation_operation)}
  sc<-abr_scenarios(cfg);seed_rows<-function(k){d<-resume_state[[k]];if(is.data.frame(d)&&nrow(d))list(d) else list()}
  rows<-seed_rows("rows");mr<-seed_rows("method_rows");paths<-seed_rows("looks");handled<-matrix(FALSE,nrow(sc),cfg$reps)
  if(length(rows))handled[cbind(rows[[1]]$SCENARIO,rows[[1]]$SIMID)]<-TRUE
  initial_completed<-sum(handled);done<-initial_completed;cancelled<-FALSE;next_publish<-Sys.time();hash<-batch_source_hash();created<-format(Sys.time(),tz="UTC",usetz=TRUE)
  snapshot<-function(status)abr_aggregate(list(config=cfg,scenarios=sc,rows=batch_bind_columns(rows),method_rows=batch_bind_columns(mr),looks=batch_bind_columns(paths),completed=done,total=nrow(sc)*cfg$reps,status=status,version=batch_version,review_status="pending_v0.35_review",schema="event_pred.batch_research.v4",created_at=created,dependencies=batch_dependencies(cfg),source_hash=hash,initial_completed=initial_completed,resumed_from_run_id=resume_state$run_id %||% "",continuation_operation=if(is.null(resume_state))"new" else continuation_operation))
  for(j in seq_len(nrow(sc))){for(b in seq_len(cfg$reps)){
    if(handled[j,b])next
    if(!is.null(replay_keys)&&!any(replay_keys$SCENARIO==j&replay_keys$SIMID==b))next
    if(should_cancel()){cancelled<-TRUE;break}
    a<-abr_trial(cfg,sc[j,,drop=FALSE],b);rows[[length(rows)+1L]]<-a$row;mr[[length(mr)+1L]]<-a$method_rows;paths[[length(paths)+1L]]<-a$looks;done<-done+1L;handled[j,b]<-TRUE
    if(done==initial_completed+1L||b==cfg$reps||Sys.time()>=next_publish){progress(snapshot("running"));next_publish<-Sys.time()+2}
  };if(cancelled)break}
  snapshot(if(cancelled)"cancelled" else if(!is.null(replay_keys)&&done<nrow(sc)*cfg$reps)"partial_replay" else "complete")
}
abr_sample <- function(r,scenario,replicate){sc<-r$scenarios[r$scenarios$SCENARIO==scenario,,drop=FALSE];if(nrow(sc)!=1||!any(r$rows$SCENARIO==scenario&r$rows$SIMID==replicate&r$rows$generated))stop("请选择已生成轮次。");abr_trial(r$config,sc,replicate,TRUE)}
abr_adtte <- function(sample,cfg) {
  o<-sample$observed;date<-function(x)as.character(as.Date(cfg$origin)+floor(x))
  data.frame(SCENARIO=o$SCENARIO,SIMID=o$SIMID,METHOD=o$method,COHORT=o$cohort,USUBJID=o$USUBJID,PARAMCD=cfg$paramcd,TRTP=o$group,STARTDT=date(o$entry_day),ADT=date(o$obs_day),AVAL=floor(o$obs_day)-floor(o$entry_day),AVALU="DAYS",CNSR=ifelse(o$event==1,0L,ifelse(o$status=="dropout",2L,1L)),ANL01FL="Y",ENGINE_ENTRY_DAY=o$entry_day,ENGINE_OBS_DAY=o$obs_day,ENGINE_TIME_DAY=o$time_day)
}
abr_report <- function(r) {
  tab<-function(d)capture.output(print(d,row.names=FALSE))
  c("# 独立队列事件数重估批量研究（开发稿，待复核）","",paste0("研究：",r$run_title %||% "","；版本",r$version,"；处理",r$completed,"/",r$total,"；",r$status),paste0("主规则：",abr_names[[r$config$primary]],"；单侧alpha=",r$config$alpha),
    "两个队列预先划分；阶段1在IA冻结，阶段2仅新患者。各规则共享本轮两队列真值，按各自D2截取；原计划权重固定。窗口未达关闭不拒绝；无效检验和生成失败保持未知。",
    "","## 冻结配置","","```json",as.character(jsonlite::toJSON(batch_config_document(r$config),auto_unbox=TRUE,pretty=TRUE,digits=NA)),"```","","## 情景与主结果","","```text",tab(r$scenarios),tab(r$overview),"```","","## 规则概率与配对差","","```text",tab(r$method_overview),tab(r$method_pairs),"```","","## 路径与资源","","```text",tab(r$stops),tab(r$resources),"```","",
    "HR=1为两组生存零假设；其他行按HR情景命名。未自动补充H0；需在真实HR列表或CSV中明确纳入1。CP为PH信息近似，不能代替患者模拟的条件概率。阶段log-rank及事件触发时点均需有限样本校准。",
    "同轮规则比较没有联合择优拒绝。关闭时间分位数含未达标者，不能称为达标时间分位数；资源分母为成功生成轮次。ADTTE按METHOD及COHORT拆分，各规则记录不能合并后重复分析。",
    "2026-10-05仅开发；代码、方法/数值、结果、页面、重放及公式显示复核安排2026-10-06。共享旧患者后续事件的确认性重估仍待开发。")
}
abr_reproduction_script <- function(r) {
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  c("# Run in event_pred source root; development draft, replay not yet checked.",
    'for(f in c("units","models","forecast","inputs","simulation","nph","sequential","study","parameter_calibration","batch_research","batch_analysis","batch_sequential","batch_workspace","joint_survival","joint_research","joint_marginal","adaptation","patient_adaptation","adaptive_batch"))source(paste0("R/",f,".R"))',
    paste0("cfg <- ",dump(r$config)),paste0("keys <- ",dump(r$rows[,intersect(c("SCENARIO","SIMID"),names(r$rows)),drop=FALSE])),
    paste0('if(batch_version!="',r$version,'")stop("Version differs")'),paste0('if(batch_source_hash()!="',r$source_hash,'")stop("Engine source differs")'),paste0("expected_dependencies <- ",dump(r$dependencies)),
    'if(!identical(batch_dependencies(cfg),expected_dependencies))stop("Dependencies differ")','result <- run_adaptive_batch(cfg,replay_keys=keys)',
    vapply(c("scenarios","rows","method_rows","looks","overview","method_overview","method_pairs","resources","stops"),function(k)paste0('write.csv(result$',k,',"adaptive_batch_',k,'_replayed.csv",row.names=FALSE)'),character(1)))
}

# Deterministic reference from the selected saved plan, without future outcomes.
abr_decision_reference <- function(r,scenario,z1_values) {
  sc<-r$scenarios[r$scenarios$SCENARIO==scenario,,drop=FALSE]
  if(nrow(sc)!=1||!is.numeric(z1_values)||!length(z1_values)||length(z1_values)>100||any(!is.finite(z1_values)|abs(z1_values)>12)||anyDuplicated(z1_values))stop("选择一个情景，IA获益Z列表需1–100个不重复的−12至12有限数。")
  cfg<-abr_trial_config(r$config,sc);a<-patient_adaptive_plan(cfg);cfg$purpose<-"interim";cfg$z1<-0
  tables<-lapply(abr_methods(r$config),function(m){cfg$rule<-if(m=="promising")"promising" else "bounded";d<-adaptive_decision(z1_values,cfg)
    if(m=="original"){d$selected_d2<-rep(a$d2_min,nrow(d));d$final_events<-rep(cfg$d_plan,nrow(d));d$cp_selected<-d$cp_original;d$required_d2<-rep(NA_real_,nrow(d));d$target_reached<-d$cp_selected>=cfg$cp_target-1e-12;d$reason<-rep("original_plan",nrow(d))}
    cbind(SCENARIO=sc$SCENARIO,label=sc$label,method=m,method_label=abr_names[[m]],w1=a$w1,w2=a$w2,critical=a$critical,hr_assumed=cfg$hr_assumed,cp_target=cfg$cp_target,cp_min=cfg$cp_min,d)})
  list(table=do.call(rbind,tables),scenario=sc,config=r$config,run_id=r$run_id,z1_values=z1_values,version=batch_version,review_status="pending_v0.35_review",created_at=format(Sys.time(),tz="UTC",usetz=TRUE),note="预定固定权重、PH事件信息正态近似；未使用模拟真值，也未评价患者级条件拒绝概率或从起点错误率。")
}
abr_validate_paths <- function(state,cfg) {
  mr<-state$method_rows;lk<-state$looks;d<-state$rows
  if(!nrow(d)&&is.data.frame(lk)&&nrow(lk))stop("空重估轮次不能包含阶段路径。")
  if(!nrow(d))return(invisible(TRUE))
  generated<-d[d$generated %in% TRUE,,drop=FALSE]
  if(!is.data.frame(lk))stop("阶段路径需为表。")
  if(nrow(lk)&&(!all(c("SCENARIO","SIMID","method","stage") %in% names(lk))||anyNA(lk[,c("SCENARIO","SIMID","method","stage")])||any(!lk$method %in% abr_methods(cfg))||any(!lk$stage %in% 1:2)||anyDuplicated(lk[,c("SCENARIO","SIMID","method","stage")])))stop("重估阶段路径键无效。")
  key<-function(x)paste(x$SCENARIO,x$SIMID)
  for(m in abr_methods(cfg)){
    first<-if(nrow(lk))lk[lk$method==m&lk$stage==1,,drop=FALSE] else data.frame()
    if(!setequal(key(generated),key(first)))stop("已生成轮次的IA路径不完整。")
    md<-mr[mr$method==m,,drop=FALSE];second_expected<-md[md$generated %in% TRUE&md$action %in% c("tested","stage2_window_unreached","stage2_invalid"),,drop=FALSE]
    second<-if(nrow(lk))lk[lk$method==m&lk$stage==2,,drop=FALSE] else data.frame()
    if(!setequal(key(second_expected),key(second)))stop("新队列路径与已处理动作不符。")
  }
  invisible(TRUE)
}
