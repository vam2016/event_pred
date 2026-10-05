# Shared risk sets under an externally specified Control age hazard. The test is
# a counting-process likelihood mixture, not a difference of log-rank Z scores.
sp_names<-c(original="原计划事件目标",bounded="上下限事件重估",promising="Promising-zone事件重估")
sp_methods<-function(cfg)unique(c(cfg$primary,cfg$compare_methods))
sp_fields<-c("label","n","hr","enroll_scale","dropout_scale")
sp_scenarios<-function(cfg){
  d<-cfg$scenario_table
  if(!is.data.frame(d)||!identical(sort(names(d)),sort(sp_fields))||!nrow(d)||nrow(d)>60)stop("情景表需1–60行：label,n,hr,enroll_scale,dropout_scale。")
  d<-d[,sp_fields,drop=FALSE]
  if(anyNA(d$label)||any(!nzchar(trimws(d$label)))||anyDuplicated(d$label)||anyDuplicated(d[,setdiff(sp_fields,"label"),drop=FALSE]))stop("情景名称唯一且参数不完全重复。")
  for(k in setdiff(sp_fields,"label"))if(!is.numeric(d[[k]])||any(!is.finite(d[[k]])))stop("情景数值需有限。")
  low<-if(cfg$mode=="conditional")0 else 10
  if(any(d$n<low|d$n>5000|d$n!=floor(d$n))||any(d$hr<.01|d$hr>5)||any(d$enroll_scale<.01|d$enroll_scale>100)||any(d$dropout_scale<0|d$dropout_scale>100))stop("设计N10–5000；条件未来N0–5000；HR0.01–5；入组倍数0.01–100，退出倍数0–100。")
  if(cfg$enroll_mode=="batch"&&any(d$enroll_scale!=1))stop("一次入组的入组倍数须1。")
  capacity<-d$n+if(cfg$mode=="conditional")nrow(cfg$payload$data) else 0
  if(any(capacity<cfg$d_max))stop("Final事件上限不能超过总计划患者数。")
  d$SCENARIO<-seq_len(nrow(d));d$null_status<-if(cfg$mode=="conditional")"conditional_given_IA" else ifelse(d$hr>=1,"benefit_composite_null","benefit_alternative");d
}
sp_grid<-function(values,weights,benefit=FALSE){
  if(!is.numeric(values)||!length(values)||length(values)>30||any(!is.finite(values)|values<=0|values>20)||anyDuplicated(values)||length(weights)!=length(values)||any(!is.finite(weights)|weights<=0)||benefit&&any(values>=1))stop("原混合HR网格1–30个不同正数≤20，正权重对应；获益检验网格均须<1。")
  list(values=values,weights=weights/sum(weights))
}
validate_shared_adaptation<-function(cfg){
  scalar<-function(x,lo,hi,int=FALSE)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi&&(!int||x==floor(x))
  if(!cfg$test_basis %in% c("known_hazard","cohort_riskset"))stop("选择已知基准风险或共同入组队列风险集检验。")
  if(cfg$test_basis=="cohort_riskset"&&cfg$enroll_mode!="batch")stop("共同入组队列分支当前生成一次入组；实际IA可含多个精确原入组队列，未来新患者一次入组。")
  if(!sp_is(cfg)||!cfg$mode %in% c("fixed_truth","conditional")||!all(sp_methods(cfg) %in% names(sp_names))||!identical(cfg$sided,"benefit"))stop("共享患者研究采用预定单侧获益混合似然检验。")
  a<-cfg;a$purpose<-"interim";a$z1<-0;a$rule<-"promising";adaptive_validate(a)
  if(!scalar(cfg$cp_min,.0001,.999)||cfg$cp_min>=cfg$cp_target)stop("CPmin需正且低于CPtarget。")
  if(!scalar(cfg$reps,20,10000,TRUE)||!scalar(cfg$seed,0,.Machine$integer.max,TRUE)||!scalar(cfg$interval_alpha,.0001,.2))stop("B20–10000、整数种子；区间错误率0.0001–0.2。")
  if(length(cfg$control)!=1||is.na(cfg$control)||!nzchar(cfg$control)||length(cfg$treatment)!=1||is.na(cfg$treatment)||!nzchar(cfg$treatment)||cfg$treatment==cfg$control)stop("两组标签需非空且不同。")
  if(!cfg$control_model$id %in% parameter_catalog()$id||!cfg$enroll_mode %in% c("batch","constant")||length(cfg$dropout_rates)!=2||any(!is.finite(cfg$dropout_rates)|cfg$dropout_rates<0))stop("指定Control基准风险模型、一次/恒定入组和两组独立退出率。")
  if(cfg$enroll_mode=="constant"&&!scalar(cfg$enroll_rate,.Machine$double.eps,1e6))stop("总体入组率需为正。")
  if(!scalar(cfg$max_day,.001,3650)||is.na(as.Date(cfg$origin))||!cfg$paramcd %in% c("PFS","OS"))stop("最大窗口最长3650日，指定起点与终点。")
  time_factor(cfg$display_unit);sp_grid(cfg$test_grid,cfg$test_weights,TRUE);sp_grid(cfg$ci_grid,cfg$ci_weights)
  if(cfg$mode=="conditional"){
    d<-validate_data(cfg$payload$data,cfg$payload$cut,FALSE,"strict")
    if(!"group" %in% names(d)||length(unique(d$group))!=2||!cfg$control %in% d$group||sum(d$event)!=cfg$d1||any(abs(d$obs_day-d$entry-d$time)>1e-6))stop("冻结IA需两组、指定Control、D1事件及连续经过时间；风险集须确认到IA。")
  }
  sc<-sp_scenarios(cfg);n<-sum(sc$n)+if(cfg$mode=="conditional")nrow(cfg$payload$data)*nrow(sc) else 0
  if(nrow(sc)*cfg$reps>100000||n*cfg$reps>3e7||nrow(sc)*cfg$reps*length(sp_methods(cfg))>500000)stop("最多10万轮/3000万患者轮/50万规则轮。")
  invisible(TRUE)
}
sp_logsum<-function(x){m<-max(x);if(!is.finite(m))return(m);m+log(sum(exp(x-m)))}
sp_statistics<-function(o,cfg){
  if(identical(cfg$test_basis,"cohort_riskset"))return(sp_mark_statistics(o,cfg))
  t<-o[o$group!=cfg$control,,drop=FALSE];h<-model_cumhaz(cfg$control_model,t$time_day)
  if(any(!is.finite(h)|h<0))stop("观察风险年龄的已指定累计风险无效。")
  list(events=sum(t$event),exposure=sum(h))
}
sp_logmixture<-function(n,y,grid,weights){g<-sp_grid(grid,weights);sp_logsum(log(g$weights)+n*log(g$values)-g$values*y)}
sp_evidence<-function(stat,cfg){if(identical(cfg$test_basis,"cohort_riskset"))return(sp_mark_evidence(stat,cfg));n<-stat$events;y<-stat$exposure
  loge<-sp_logmixture(n,y,cfg$test_grid,cfg$test_weights)+y
  list(log_e=loge,e_value=if(loge>700)Inf else exp(loge),e_p=min(1,exp(-loge)),conditional_error_bound=min(1,exp(log(cfg$alpha)+loge)),n_treatment_events=n,baseline_exposure=y)
}
sp_confidence<-function(stat,cfg){
  if(identical(cfg$test_basis,"cohort_riskset"))return(sp_mark_confidence(stat,cfg))
  n<-stat$events;y<-stat$exposure;threshold<-log(1/cfg$interval_alpha)
  mix<-sp_logmixture(n,y,cfg$ci_grid,cfg$ci_weights)
  if(y==0){if(n>0)stop("存在事件但基准累计风险暴露为零。");return(list(hr_mle=NA_real_,cs_lower=0,cs_upper=Inf,cs_empty=FALSE,interval_level=1-cfg$interval_alpha,confidence_engine="known_hazard_mixture_CS"))}
  mle<-n/y
  # Write candidate log E without operator-precedence ambiguity.
  f<-function(r)mix-(if(n==0)0 else n*log(r))+r*y-threshold
  minimum<-if(n==0)mix-threshold else f(mle)
  if(minimum>0)return(list(hr_mle=mle,cs_lower=NA_real_,cs_upper=NA_real_,cs_empty=TRUE,interval_level=1-cfg$interval_alpha,confidence_engine="known_hazard_mixture_CS"))
  upper<-max(1,mle*2+1);for(j in 1:60){if(f(upper)>0)break;upper<-upper*2}
  if(!is.finite(upper)||f(upper)<=0)stop("无法建立混合区间上限求根范围。")
  hi<-uniroot(f,c(max(mle,.Machine$double.xmin),upper),tol=1e-9)$root
  lo<-if(n==0)0 else {left<-mle/2;for(j in 1:100){if(f(left)>0)break;left<-left/2};if(f(left)<=0)stop("无法建立混合区间下限求根范围。");uniroot(f,c(left,mle),tol=1e-9)$root}
  list(hr_mle=mle,cs_lower=lo,cs_upper=hi,cs_empty=FALSE,interval_level=1-cfg$interval_alpha,confidence_engine="known_hazard_mixture_CS")
}
sp_truth_design<-function(cfg,sc,b){
  c<-list(n=sc$n,enroll_mode=if(cfg$enroll_mode=="batch")"schedule" else "constant",entry_days=rep(0,sc$n),enroll_rate=cfg$enroll_rate*sc$enroll_scale,
    groups=list(list(name=cfg$control,weight=1-cfg$p,dropout_rate=cfg$dropout_rates[1]*sc$dropout_scale,model=cfg$control_model,hazard_multiplier=1),list(name=cfg$treatment,weight=cfg$p,dropout_rate=cfg$dropout_rates[2]*sc$dropout_scale,model=cfg$control_model,hazard_multiplier=sc$hr)))
  simulate_survival_truth(c,b)
}
sp_truth_conditional<-function(cfg,sc,b){
  d<-cfg$payload$data;cut<-cfg$payload$cut;n<-nrow(d);ev<-dr<-rep(Inf,n);active<-d$status=="active"
  ev[d$status=="event"]<-d$time[d$status=="event"];dr[d$status=="dropout"]<-d$time[d$status=="dropout"]
  for(g in unique(d$group)){ix<-which(active&d$group==g);if(!length(ix))next;r<-if(g==cfg$control)1 else sc$hr;ev[ix]<-sample_conditional(cfg$control_model,d$time[ix],r);rate<-cfg$dropout_rates[if(g==cfg$control)1 else 2]*sc$dropout_scale;if(rate>0)dr[ix]<-d$time[ix]+rexp(length(ix),rate)}
  existing<-data.frame(SIMID=b,USUBJID=d$id,group=d$group,entry_day=d$entry,event_time_day=ev,dropout_time_day=dr,event_day=d$entry+ev,dropout_day=d$entry+dr)
  if(!sc$n)return(existing)
  nc<-cfg;nc$control<-cfg$control;new<-sp_truth_design(nc,sc,b);labs<-unique(d$group);new$group[new$group==cfg$treatment]<-setdiff(labs,cfg$control)
  new$entry_day<-new$entry_day+cut;new$event_day<-new$entry_day+new$event_time_day;new$dropout_day<-new$entry_day+new$dropout_time_day
  prefix<-"FUTURE::";while(any(paste0(prefix,new$USUBJID) %in% d$id))prefix<-paste0("_",prefix);new$USUBJID<-paste0(prefix,new$USUBJID)
  rbind(existing,new)
}
sp_cut<-function(truth,target,window,stage){patient_adaptive_cut(truth,target,window,stage)}
sp_trial<-function(cfg,sc,b,prepared=NULL,keep_sample=FALSE){
  seed<-batch_replicate_seed(cfg$seed,sc$SCENARIO,b)
  batch_with_rng(seed,{
    failed<-function(message){mr<-do.call(rbind,lapply(sp_methods(cfg),function(m)data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,method=m,is_primary=m==cfg$primary,generated=FALSE,decision_valid=FALSE,decision_reject=NA,target_reached=NA,increased=NA,action="generation_failed",note=message)));list(row=data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,generated=FALSE,decision_valid=FALSE,decision_reject=NA,generation_note=message),method_rows=mr,looks=data.frame(),draw_rows=data.frame())}
    tryCatch({
      truth<-if(cfg$mode=="conditional")sp_truth_conditional(cfg,sc,b) else sp_truth_design(cfg,sc,b)
      if(cfg$mode=="conditional"){
        d<-cfg$payload$data;first<-list(hit=TRUE,day=cfg$payload$cut,observed=data.frame(SIMID=b,USUBJID=d$id,group=d$group,entry_day=d$entry,time_day=d$time,obs_day=d$obs_day,status=d$status,event=d$event,DCO_DAY=cfg$payload$cut));end<-first$day+cfg$max_day
      }else {first<-sp_cut(truth,cfg$d1,cfg$max_day,1L);end<-cfg$max_day}
      s1<-patient_adaptive_score(first$observed,cfg$control);st1<-sp_statistics(first$observed,cfg);e1<-sp_evidence(st1,cfg)
      rows<-paths<-observed<-risksets<-list()
      for(m in sp_methods(cfg)){
        ready<-first$hit&&(m=="original"||s1$valid);decision<-NULL;target<-cfg$d_plan
        if(ready&&m!="original"){a<-cfg;a$purpose<-"interim";a$rule<-m;a$z1<-s1$z;decision<-adaptive_decision(s1$z,a);target<-cfg$d1+decision$selected_d2}
        last<-if(ready)sp_cut(truth,target,end,2L) else first
        valid<-!first$hit||ready;performed<-ready&&last$hit
        stat<-sp_statistics(last$observed,cfg);ev<-sp_evidence(stat,cfg)
        reject<-if(!valid)NA else performed&&ev$log_e>=log(1/cfg$alpha)
        action<-if(!first$hit)"IA_window_unreached" else if(!ready)"IA_score_invalid" else if(!last$hit)"Final_window_unreached" else if(reject)"efficacy_benefit" else "final_no_reject"
        ci<-if(isTRUE(cfg$estimate_effect)||keep_sample)sp_confidence(stat,cfg) else list(hr_mle=if(is.finite(stat$exposure)&&stat$exposure>0)stat$events/stat$exposure else NA_real_,cs_lower=NA_real_,cs_upper=NA_real_,cs_empty=NA,interval_level=1-cfg$interval_alpha,confidence_engine="not_requested")
        covered<-if(is.finite(ci$cs_lower)&&!is.na(ci$cs_upper))ci$cs_lower<=sc$hr&&ci$cs_upper>=sc$hr else if(isTRUE(ci$cs_empty))FALSE else NA
        methodrow<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,method=m,is_primary=m==cfg$primary,generated=TRUE,decision_valid=valid,decision_reject=reject,target_reached=if(ready)last$hit else NA,increased=if(ready)target>cfg$d_plan else NA,action=action,
          IA_day=first$day,IA_events=sum(first$observed$event),IA_logrank_z=if(s1$valid)s1$z else NA_real_,IA_log_e=e1$log_e,conditional_error_bound=e1$conditional_error_bound,
          original_target=cfg$d_plan,selected_target=if(ready)target else NA,rule_reason=if(m=="original")"original_plan" else if(!ready)"IA_unavailable" else decision$reason,cp_proxy_selected=if(is.null(decision))NA else decision$cp_selected,
          model_scope=if(cfg$test_basis=="cohort_riskset")"unknown_baseline_within_exact_entry_cohorts" else "correct_externally_specified_baseline",
          log_e=ev$log_e,e_value=ev$e_value,e_p=ev$e_p,n_treatment_events=stat$events,baseline_exposure=stat$exposure,future_treatment_events=stat$events-st1$events,future_baseline_exposure=stat$exposure-st1$exposure,
          hr_mle=ci$hr_mle,cs_lower=ci$cs_lower,cs_upper=ci$cs_upper,cs_empty=ci$cs_empty,interval_level=ci$interval_level,confidence_engine=ci$confidence_engine,covered=covered,true_hr=sc$hr,
          n_observed=nrow(last$observed),events=sum(last$observed$event),final_day=last$day,wait_day=last$day-first$day,future_analysis_count=as.integer(performed),null_status=sc$null_status,test_basis=cfg$test_basis,informative_events=stat$informative_events %||% NA_integer_,note="CP仅为规则代理，正式检验使用保存的混合及风险模型/队列结构。")
        rows[[length(rows)+1L]]<-methodrow
        for(k in 1:2){o<-if(k==1)first$observed else last$observed;st<-if(k==1)st1 else stat;ee<-if(k==1)e1 else ev;paths[[length(paths)+1L]]<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,method=m,stage=k,phase=if(k==1&&cfg$mode=="conditional")"observed_IA" else "simulated",DCO_DAY=if(k==1)first$day else last$day,events=sum(o$event),performed=if(k==1)FALSE else performed,target=if(k==1)cfg$d1 else if(ready)target else NA,n_treatment_events=st$events,baseline_exposure=st$exposure,log_e=ee$log_e,conditional_error_bound=e1$conditional_error_bound,action=if(k==1)if(first$hit)"planning_IA_no_test" else "IA_window_unreached" else action)}
        if(keep_sample){o<-last$observed;o$METHOD<-rep(m,nrow(o));o$SCENARIO<-rep(sc$SCENARIO,nrow(o));observed[[length(observed)+1L]]<-o;if(!is.null(stat$all_risksets)&&nrow(stat$all_risksets)){rs<-stat$all_risksets;rs$SCENARIO<-sc$SCENARIO;rs$SIMID<-b;rs$METHOD<-m;risksets[[length(risksets)+1L]]<-rs}}
      }
      mr<-do.call(rbind,rows);pr<-mr[mr$is_primary,,drop=FALSE]
      out<-list(row=data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,generated=TRUE,decision_valid=pr$decision_valid,decision_reject=pr$decision_reject,generation_note=""),method_rows=mr,looks=do.call(rbind,paths),draw_rows=data.frame())
      if(keep_sample){out$observed<-batch_bind_columns(observed);truth$SCENARIO<-sc$SCENARIO;out$truth<-truth;out$ia_observed<-first$observed;out$risksets<-batch_bind_columns(risksets)};out
    },error=function(e)failed(conditionMessage(e)))
  })
}
sp_aggregate<-function(state){
  state<-research_aggregate(state,sp_methods(state$config),sp_names,c("decision_reject","target_reached","increased"));mr<-state$method_rows;rec<-list()
  for(s in state$scenarios$SCENARIO)for(m in sp_methods(state$config)){x<-if(nrow(mr))mr[mr$SCENARIO==s&mr$method==m,,drop=FALSE] else mr;v<-if("covered" %in% names(x))!is.na(x$covered) else logical();est<-if("hr_mle" %in% names(x))x$hr_mle else numeric();rec[[length(rec)+1L]]<-data.frame(SCENARIO=s,method=m,interval_n=sum(v),coverage=if(any(v))mean(x$covered[v]) else NA_real_,finite_mle_n=sum(is.finite(est)),mean_finite_hr_mle=if(any(is.finite(est)))mean(est[is.finite(est)]) else NA_real_,note=if(state$config$mode=="conditional")"给定IA的条件覆盖，不是从起点CS覆盖保证。" else paste0(state$config$test_basis,"分支的混合置信序列；新增实现未验证。"))}
  state$recovery<-do.call(rbind,rec);state
}
sp_initial<-function(cfg,initial=NULL)research_initial(cfg,sp_scenarios(cfg),sp_aggregate,initial)
run_shared_adaptation<-function(cfg,progress=function(state)NULL,should_cancel=function()FALSE,replay_keys=NULL,resume_state=NULL,continuation_operation="resume"){validate_shared_adaptation(cfg);research_family_run(cfg,sp_scenarios(cfg),sp_trial,sp_aggregate,progress,should_cancel,replay_keys,resume_state,continuation_operation)}
sp_sample<-function(r,scenario,replicate){sc<-r$scenarios[r$scenarios$SCENARIO==scenario,,drop=FALSE];if(nrow(sc)!=1||!any(r$rows$SCENARIO==scenario&r$rows$SIMID==replicate&r$rows$generated))stop("选择已生成轮次。");sp_trial(r$config,sc,replicate,keep_sample=TRUE)}
sp_report<-function(r){tab<-function(d)capture.output(print(d,row.names=FALSE));c("# 共享患者事件重估与适应后推断（开发稿）","",paste0("v",r$version,"；",r$config$mode,"；处理",r$completed,"/",r$total),"旧患者持续随访；已知风险分支使用Treatment计数/累计风险暴露，队列风险集分支使用共同入组队列内事件标记似然。IA普通log-rank只用于事件目标规则CP代理；不相减累计Z，不假设旧患者两个阶段独立。","原混合HR网格/权重及检验分支须在本次数据以前预定；已知风险分支的基准风险也须预定；条件错误是Markov上界，不是原固定Final log-rank的条件错误。CI为另一个预定正HR混合的置信序列，不能称为Cox调整区间。","","## 参数与完整分母","```text",tab(r$scenarios),tab(r$overview),tab(r$method_overview),"```","","## 配对、资源与效应","```text",tab(r$method_pairs),tab(r$resources),tab(r$recovery),"```","","检验分支见test_basis：已知风险模型要求基准风险正确；队列风险集分支在精确同入组队列内消去基准风险。均要求恒定PH、观察过滤完整及独立退出等假设。条件研究拒绝/覆盖以冻结IA为条件，不是类型一类错误。普通非分层年龄Cox的共享患者重估和拟合基准风险替代仍未实现；本轮代码和重放未验证。")}

sp_run_observed<-function(data,cfg){
 d<-validate_data(data,cfg$payload$cut,FALSE,"strict")
 if(!"group" %in% names(d)||length(unique(d$group))!=2||length(cfg$control)!=1||!cfg$control %in% d$group||any(abs(d$obs_day-d$entry-d$time)>1e-6))stop("观察推断需要规范两组IA风险集及Control。")
 if(length(cfg$alpha)!=1||!is.finite(cfg$alpha)||cfg$alpha<.0001||cfg$alpha>.2||length(cfg$interval_alpha)!=1||!is.finite(cfg$interval_alpha)||cfg$interval_alpha<.0001||cfg$interval_alpha>.2)stop("alpha/区间错误率需0.0001–0.2。")
 o<-data.frame(group=d$group,time_day=d$time,event=d$event,entry_day=d$entry,obs_day=d$obs_day,status=d$status);stat<-sp_statistics(o,cfg);e<-sp_evidence(stat,cfg);ci<-sp_confidence(stat,cfg)
 table<-as.data.frame(c(list(n=nrow(d),events=sum(d$event),IA_day=cfg$payload$cut),e,ci,list(e_reject=e$log_e>=log(1/cfg$alpha))),stringsAsFactors=FALSE)
 list(schema="event_pred.shared_observed.v1",version=batch_version,config=cfg,observed=d,statistics=stat,table=table,created_at=format(Sys.time(),tz="UTC",usetz=TRUE),review_status="pending_review",note=paste0("检验分支：",cfg$test_basis,"；原预定混合，精确观察时钟；不是普通Cox区间或log-rank名义p。"))
}

sp_validate_saved<-function(state,cfg){
 d<-state$method_rows;l<-state$looks
 if(!nrow(d))return(invisible(TRUE))
 if(anyDuplicated(d[,c("SCENARIO","SIMID","method"),drop=FALSE]))stop("共享患者结果的规则键重复。")
 generated<-state$rows[state$rows$generated,,drop=FALSE]
 for(i in seq_len(nrow(generated)))for(m in sp_methods(cfg)){
   x<-l[l$SCENARIO==generated$SCENARIO[i]&l$SIMID==generated$SIMID[i]&l$method==m,,drop=FALSE]
   if(!identical(as.integer(x$stage),1:2))stop("共享患者续跑缺少IA/Final路径。")
 }
 invisible(TRUE)
}

sp_observed_bundle<-function(r)list(schema="event_pred.shared_observed.v1",version=r$version,review_status=r$review_status,created_at=r$created_at,exact_result=batch_encode_tree(r),note="Typed result tree preserves infinite interval bounds and exact doubles; contains observed patient records and is not a batch configuration import.")
