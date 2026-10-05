# Actual IA is frozen; only future outcomes, parameters and process rates are drawn.
cb_fields <- c("label","control_event_q","treatment_event_q","control_enroll_q","treatment_enroll_q","control_dropout_q","treatment_dropout_q")
cb_names <- c(logrank="原设计log-rank",adaptive="已选定新队列D2",original="原计划新队列D2")
cb_methods <- function(cfg)if(cfg$branch=="new_cohort")unique(c(cfg$primary,setdiff(c("adaptive","original"),cfg$primary))) else "logrank"
cb_plain <- function(x) {
  if(inherits(x,"Date")||inherits(x,"POSIXt"))return(as.character(x))
  if(is.data.frame(x)){d<-lapply(x,cb_plain);return(as.data.frame(d,stringsAsFactors=FALSE,check.names=FALSE))}
  if(is.list(x))return(lapply(x,cb_plain));x
}
cb_freeze <- function(data,engine,branch) {
  if(!branch %in% c("existing_patients","new_cohort"))stop("未知条件研究类型。")
  cols<-c("id","entry","time","obs_day","status","event","group");if(!all(cols %in% names(data)))stop("IA规范数据缺少个体、时间、状态或组别列。")
  data<-cb_plain(data[,cols,drop=FALSE]);engine<-cb_plain(engine)
  engine$q_values<-1;engine$scenarios<-NULL;engine$reps<-1L
  if(branch=="existing_patients"&&!conditional_is_prediction(engine))stop("选择IA未来拒绝概率或历史IA拒绝预测后再冻结。生存估计变化批量不属于本入口。")
  if(branch=="new_cohort"&&!identical(engine$purpose,"patient_prediction"))stop("选择重估后的新队列条件预测。")
  list(data=data,engine=engine,branch=branch,frozen_at=format(Sys.time(),tz="UTC",usetz=TRUE))
}
cb_scenarios <- function(cfg) {
  d<-cfg$scenario_table
  if(!is.data.frame(d)||!identical(sort(names(d)),sort(cb_fields))||nrow(d)>59)stop(paste("附加情景CSV严格列名：",paste(cb_fields,collapse=","),"；最多59行。"))
  d<-d[,cb_fields,drop=FALSE];base<-as.data.frame(as.list(setNames(c("输入基准",rep(1,6)),cb_fields)),stringsAsFactors=FALSE)
  for(k in setdiff(cb_fields,"label")){base[[k]]<-1;if(!nrow(d))d[[k]]<-numeric();if(!is.numeric(d[[k]])||any(!is.finite(d[[k]])))stop(paste0(k,"需有限数值。"));lo<-if(grepl("dropout",k))0 else .01;if(any(d[[k]]<lo|d[[k]]>10))stop("事件/入组倍数0.01–10，脱落倍数0–10。")}
  if(anyNA(d$label)||any(!nzchar(trimws(d$label)))||anyDuplicated(d$label)||"输入基准" %in% d$label)stop("情景名称须非空唯一，输入基准为保留名称。")
  d<-rbind(base,d);if(anyDuplicated(d[,setdiff(cb_fields,"label"),drop=FALSE]))stop("情景不能与基准或其他情景完全重复。")
  if(cfg$branch=="new_cohort"&&(any(d$control_enroll_q!=d$treatment_enroll_q)||(cfg$payload$engine$enrollment=="batch"&&any(d$control_enroll_q!=1))))stop("新队列采用总体入组过程，两组入组倍数须相等；一次入组时须为1。")
  d$SCENARIO<-seq_len(nrow(d));rownames(d)<-NULL;d
}
cb_engine <- function(cfg) {
  d<-cfg$payload$data;c<-cfg$payload$engine;c$reps<-1L;c$seed<-cfg$seed;c$display_unit<-cfg$display_unit
  if(cfg$branch=="new_cohort"){
    c$purpose<-"patient_prediction";c$reps<-20L;patient_prediction_validate(d,c);c$reps<-1L
    interim<-c;interim$purpose<-"patient_interim";ia<-patient_adaptive_interim(d,interim)
    return(list(data=d,config=c,ia=ia,terminal=FALSE))
  }
  c<-complete_config(c);c$purpose<-"rejection";c$q_values<-1;c$fixed_times<-1
  if(conditional_has_history(c)){
    c$prediction_history_path<-conditional_history_path(c$prediction_history,c);c$prediction_look<-nrow(c$prediction_history_path);last<-c$prediction_history[[c$prediction_look]]
    cols<-c("id","entry","time","obs_day","status","event","group");sortdata<-function(x){x<-x[order(x$id),cols,drop=FALSE];rownames(x)<-NULL;x}
    if(!isTRUE(all.equal(c$cut,last$cut))||!isTRUE(all.equal(sortdata(d),sortdata(last$data),tolerance=1e-12,check.attributes=FALSE)))stop("当前IA必须是所选历史快照，不能换成后续数据。")
  } else c$prediction_look<-NULL
  terminal<-conditional_has_history(c)&&tail(c$prediction_history_path$action,1)!="continue"
  if(terminal){c$model_mode<-"manual";c$uncertainty<-"plugin";c$process_uncertainty<-"fixed";c$max_day<-c$cut+1;c$groups<-lapply(sort(unique(d$group)),function(lab)list(name=lab,future_n=0,enroll_mode="constant",enroll_rate=0,enroll_cuts=numeric(),enroll_rates=numeric(),dropout_rate=0,multiplier=1,model=NULL))}
  d<-validate_data(d,c$cut,require_events=c$model_mode=="fit",gap_mode="strict")
  if(any(abs(d$obs_day-d$entry-d$time)>1e-6))stop("IA需连续经过时间，不能包含首日加项。")
  validate_conditional_config(d,c);validate_conditional_prediction(d,c)
  if(c$prediction_design=="sequential")c$prediction_plan<-conditional_prediction_plan(c)
  ia<-conditional_prediction_ia(d,c);terminal<-c$prediction_design=="sequential"&&ia$action!="continue"
  list(data=d,config=c,ia=ia,terminal=terminal)
}
validate_conditional_batch <- function(cfg) {
  scalar<-function(x,lo,hi)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi&&x==floor(x)
  if(!cb_is(cfg)||!cfg$branch %in% c("existing_patients","new_cohort")||!identical(cfg$mode,"conditional"))stop("未知实际IA条件研究家族。")
  if(!scalar(cfg$reps,20,10000)||!scalar(cfg$seed,0,.Machine$integer.max)||!cfg$primary %in% (if(cfg$branch=="new_cohort")c("adaptive","original") else "logrank"))stop("B为20–10000、种子0–2147483647整数，预定主方法须匹配条件研究。")
  e<-cb_engine(cfg);sc<-cb_scenarios(cfg);n<-if(cfg$branch=="new_cohort")e$config$n2 else nrow(e$data)+sum(vapply(e$config$groups,`[[`,numeric(1),"future_n"))
  if(nrow(sc)*cfg$reps>100000||n*nrow(sc)*cfg$reps>2e7)stop("条件研究最多10万轮、2000万患者×轮次；冻结IA也计入规模。")
  invisible(TRUE)
}
cb_prepare <- function(state) {
  if(!is.null(state$prepared))return(state)
  cfg<-state$config;e<-cb_engine(cfg)
  e$models<-e$posteriors<-list()
  if(!e$terminal)batch_with_rng(batch_replicate_seed(cfg$seed,997L,1L),{
    c<-e$config;d<-e$data
    if(cfg$branch=="new_cohort"){e$models<-patient_prediction_prepare(d,c);e$posteriors<-patient_prediction_process_posterior(d,c)}
    else for(j in seq_along(c$groups)){
      g<-c$groups[[j]];dd<-d[d$group==g$name,,drop=FALSE];gc<-modifyList(c,g)
      m<-if(c$model_mode=="manual")g$model else fit_model(dd,g$fit_method,g$cuts,g$tail_rate)
      if(c$uncertainty=="bayes_weibull"){m<-fit_bayesian_weibull(dd,gc);if(!isTRUE(m$posterior$passed))stop(paste(g$name,"MCMC诊断未通过。"))}
      e$models[[j]]<-m;e$posteriors[j]<-list(process_posterior(dd,gc))
    }
  })
  e$prepared_seed<-batch_replicate_seed(cfg$seed,997L,1L);e$payload_hash<-batch_config_hash(cfg$payload);state$prepared<-e;state
}
cb_existing_truth <- function(data,draws,processes,cfg,sc,b) {
  labs<-vapply(cfg$groups,`[[`,character(1),"name");control<-cfg$prediction_control;parts<-list()
  for(j in seq_along(labs)){
    lab<-labs[j];d<-data[data$group==lab,,drop=FALSE];g<-processes[[j]];arm<-if(lab==control)"control" else "treatment";eq<-sc[[paste0(arm,"_event_q")]];dq<-sc[[paste0(arm,"_dropout_q")]];aq<-sc[[paste0(arm,"_enroll_q")]]
    n<-nrow(d);active<-which(d$status=="active");nn<-length(active)+g$future_n
    # Uniforms, unit-rate exits and arrivals are drawn even when a scenario sets a rate to zero.
    u<-runif(nn);de<-rexp(nn);arrival<-rexp(g$future_n);event<-drop<-rep(Inf,n)
    event[d$status=="event"]<-d$time[d$status=="event"];drop[d$status=="dropout"]<-d$time[d$status=="dropout"]
    rate<-g$dropout_rate*dq;haz<-g$multiplier*eq
    if(length(active)){ix<-seq_along(active);event[active]<-sample_conditional(draws[[j]],d$time[active],haz,u[ix]);drop[active]<-d$time[active]+if(rate==0)Inf else de[ix]/rate}
    entry<-d$entry;ids<-d$id;source<-rep("IA记录",n)
    if(g$future_n>0){ix<-length(active)+seq_len(g$future_n);en<-cfg$cut+if(g$enroll_mode=="piecewise")inverse_cumhaz(parameter_model("pwe",list(rates=g$enroll_rates*aq),g$enroll_cuts),cumsum(arrival)) else cumsum(arrival/(g$enroll_rate*aq))
      event<-c(event,sample_conditional(draws[[j]],rep(0,g$future_n),haz,u[ix]));drop<-c(drop,if(rate==0)rep(Inf,g$future_n) else de[ix]/rate)
      prefix<-paste0("FUTURE::",lab,"::");new<-paste0(prefix,seq_len(g$future_n));while(any(new %in% data$id)){prefix<-paste0("_",prefix);new<-paste0(prefix,seq_len(g$future_n))};entry<-c(entry,en);ids<-c(ids,new);source<-c(source,rep("未来入组",g$future_n))}
    parts[[j]]<-data.frame(SIMID=b,USUBJID=ids,group=lab,entry_day=entry,event_time_day=event,dropout_time_day=drop,event_day=entry+event,dropout_day=entry+drop,source=source)
  };do.call(rbind,parts)
}
cb_trial <- function(cfg,sc,b,prepared,keep_sample=FALSE) {
  seed<-batch_replicate_seed(cfg$seed,sc$SCENARIO,b);latent_seed<-batch_replicate_seed(cfg$seed,1L,b);stage<-"draw"
  batch_with_rng(latent_seed,{
    failure<-function(message){mr<-do.call(rbind,lapply(cb_methods(cfg),function(m)data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,latent_seed=latent_seed,method=m,is_primary=m==cfg$primary,generated=FALSE,decision_valid=FALSE,decision_reject=NA,target_reached=NA,action="generation_failed",failure_stage=stage,note=message)))
      list(row=data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,latent_seed=latent_seed,label=sc$label,generated=FALSE,decision_valid=FALSE,decision_reject=NA,generation_note=message),method_rows=mr,looks=data.frame(),draw_rows=data.frame())}
    tryCatch({
      c<-prepared$config;d<-prepared$data;draw_rows<-list();truth<-NULL
      if(cfg$branch=="new_cohort"){
        draws<-patient_prediction_draw(d,c,prepared$models);process<-patient_prediction_process_draw(c,prepared$posteriors)
        latent<-list(arrival=if(c$enrollment=="batch")NULL else rexp(c$n2),arm=sample.int(2,c$n2,replace=TRUE,prob=c(1-c$p,c$p)),event_u=runif(c$n2),drop_exp=rexp(c$n2))
        ps<-data.frame(event_control_q=sc$control_event_q,event_treatment_q=sc$treatment_event_q,enroll_q=sc$control_enroll_q,dropout_control_q=sc$control_dropout_q,dropout_treatment_q=sc$treatment_dropout_q)
        stage<-"trajectory";truth<-patient_prediction_scenario_truth(c,draws,process,latent,ps,names(prepared$models),d$id,b)
        stage<-"analysis";z<-patient_prediction_analyze(truth,d,c,prepared$ia,b,keep_sample);mr<-z$rows;mr$method<-mr$design;mr$generated<-mr$generation_valid;mr$decision_reject<-mr$reject;mr$target_reached<-mr$stage2_hit;mr$n_observed<-mr$n1_observed+mr$n2_observed;mr$events<-mr$events1+mr$events2;mr$future_analysis_count<-as.integer(mr$action=="tested")
        looks<-mr[,c("SIMID","method","action","z1","z2","variance1","variance2","selected_d2","stage2_hit","final_day"),drop=FALSE];observed<-z$observed
        draw_rows[[1]]<-data.frame(SIMID=b,group="overall",parameter="enroll_rate_per_day",value=process$enroll_rate)
        for(j in 1:2)draw_rows[[length(draw_rows)+1L]]<-data.frame(SIMID=b,group=names(prepared$models)[j],parameter="dropout_rate_per_day",value=process$dropout_rates[j])
      } else {
        if(prepared$terminal){truth<-data.frame(SIMID=b,USUBJID=d$id,group=d$group,entry_day=d$entry,event_time_day=ifelse(d$status=="event",d$time,Inf),dropout_time_day=ifelse(d$status=="dropout",d$time,Inf));truth$event_day<-truth$entry_day+truth$event_time_day;truth$dropout_day<-truth$entry_day+truth$dropout_time_day;draws<-list()}
        else{draws<-processes<-list();for(j in seq_along(c$groups)){
          g<-c$groups[[j]];dd<-d[d$group==g$name,,drop=FALSE];m<-prepared$models[[j]]
          if(c$uncertainty=="bootstrap")m<-fit_model(dd[sample.int(nrow(dd),nrow(dd),replace=TRUE),,drop=FALSE],g$fit_method,g$cuts,g$tail_rate)
          if(c$uncertainty=="gamma")m<-gamma_posterior_draw(m,c$prior_shape,c$prior_rate)
          if(c$uncertainty=="bayes_weibull")m<-posterior_model_draw(m)
          draws[[j]]<-m;processes[[j]]<-process_draw(modifyList(c,g),prepared$posteriors[[j]])
          for(k in c("enroll_rate","dropout_rate"))draw_rows[[length(draw_rows)+1L]]<-data.frame(SIMID=b,group=g$name,parameter=paste0(k,"_per_day"),value=processes[[j]][[k]])
        };stage<-"trajectory";truth<-cb_existing_truth(d,draws,processes,c,sc,b)}
        stage<-"analysis";z<-conditional_prediction_path(truth,c,b,1,prepared$ia);mr<-z$row;mr$method<-"logrank";mr$action<-mr$stop_reason;mr$final_day<-mr$DCO_DAY;mr$wait_day<-mr$DCO_DAY-c$cut;mr$n_observed<-mr$n;looks<-z$looks;looks$method<-"logrank";observed<-z$observed
      }
      for(j in seq_along(draws))for(k in seq_along(draws[[j]]$params))draw_rows[[length(draw_rows)+1L]]<-data.frame(SIMID=b,group=if(cfg$branch=="new_cohort")names(prepared$models)[j] else c$groups[[j]]$name,parameter=paste0(draws[[j]]$id,"_param_",k),value=draws[[j]]$params[k])
      mr$SCENARIO<-sc$SCENARIO;mr$replicate_seed<-seed;mr$latent_seed<-latent_seed;mr$is_primary<-mr$method==cfg$primary;mr$failure_stage<-"";mr$label<-sc$label
      looks$SCENARIO<-rep(sc$SCENARIO,nrow(looks));looks$replicate_seed<-rep(seed,nrow(looks));dr<-batch_bind_columns(draw_rows);if(nrow(dr)){dr$SCENARIO<-sc$SCENARIO;dr$replicate_seed<-seed;dr$latent_seed<-latent_seed}
      p<-mr[mr$is_primary,,drop=FALSE];row<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,latent_seed=latent_seed,label=sc$label,generated=p$generated,decision_valid=p$decision_valid,decision_reject=p$decision_reject,action=p$action,final_day=p$final_day,generation_note="")
      out<-list(row=row,method_rows=mr,looks=looks,draw_rows=dr)
      if(keep_sample){if(cfg$branch=="new_cohort"){truth$stage_entry_day<-truth$entry_day;truth$stage_event_day<-truth$event_day;for(k in c("entry_day","event_day","dropout_day"))truth[[k]]<-truth[[k]]+c$cut};observed$SCENARIO<-rep(sc$SCENARIO,nrow(observed));truth$SCENARIO<-rep(sc$SCENARIO,nrow(truth));out$observed<-observed;out$truth<-truth;out$study_config<-c}
      out
    },error=function(e)failure(conditionMessage(e)))
  })
}
research_scenario_pairs <- function(state,methods) {
  d<-state$method_rows;cfg<-state$config;out<-list();if(!nrow(d))return(data.frame())
  for(m in methods)for(id in setdiff(state$scenarios$SCENARIO,1L)){
    fields<-c("SIMID","decision_valid","decision_reject");x<-d[d$SCENARIO==1&d$method==m,fields,drop=FALSE];y<-d[d$SCENARIO==id&d$method==m,fields,drop=FALSE];z<-merge(x,y,by="SIMID",all=TRUE,suffixes=c("_reference","_scenario"))
    a<-ifelse(z$decision_valid_reference,z$decision_reject_reference,NA);b<-ifelse(z$decision_valid_scenario,z$decision_reject_scenario,NA);good<-!is.na(a)&!is.na(b);delta<-as.integer(b[good])-as.integer(a[good]);lo<-function(v)ifelse(is.na(v),0,as.integer(v));hi<-function(v)ifelse(is.na(v),1,as.integer(v))
    out[[length(out)+1L]]<-data.frame(SCENARIO=id,reference=1L,method=m,paired_valid=sum(good),paired_difference=if(length(delta))mean(delta) else NA_real_,paired_mcse=if(length(delta)>1)sd(delta)/sqrt(length(delta)) else NA_real_,difference_lower_all=(sum(lo(b)-hi(a))-(cfg$reps-nrow(z)))/cfg$reps,difference_upper_all=(sum(hi(b)-lo(a))+(cfg$reps-nrow(z)))/cfg$reps)
  };batch_bind_columns(out)
}
cb_aggregate <- function(state){metrics<-if(state$config$branch=="existing_patients"&&state$config$payload$engine$cut_mode=="fixed")"decision_reject" else c("decision_reject","target_reached");state<-research_aggregate(state,cb_methods(state$config),cb_names,metric_fields=metrics);state$scenario_pairs<-research_scenario_pairs(state,cb_methods(state$config));state$model_overview<-cb_model_overview(state);state$ia_overview<-if(is.null(state$prepared))data.frame() else if(state$config$branch=="new_cohort")state$prepared$ia$ia else state$prepared$ia;state}
cb_initial <- function(cfg,initial=NULL)research_initial(cfg,cb_scenarios(cfg),cb_aggregate,initial)
run_conditional_batch <- function(cfg,progress=function(state)NULL,should_cancel=function()FALSE,replay_keys=NULL,resume_state=NULL,continuation_operation="resume") {validate_conditional_batch(cfg);research_family_run(cfg,cb_scenarios(cfg),cb_trial,cb_aggregate,progress,should_cancel,replay_keys,resume_state,continuation_operation,cb_prepare)}
cb_sample <- function(r,scenario,replicate){sc<-r$scenarios[r$scenarios$SCENARIO==scenario,,drop=FALSE];if(nrow(sc)!=1||!any(r$rows$SCENARIO==scenario&r$rows$SIMID==replicate&r$rows$generated))stop("请选择已生成轮次。");if(is.null(r$prepared))stop("保存快照尚未完成模型准备。");cb_trial(r$config,sc,replicate,r$prepared,TRUE)}
cb_report <- function(r) {
  tab<-function(d)capture.output(print(d,row.names=FALSE))
  c("# 实际IA条件预测批量研究（待复核）","",paste0("类型",r$config$branch,"；研究",r$run_title %||% "","；v",r$version,"；处理",r$completed,"/",r$total,"；",r$status),"原IA和历史路径冻结；仅抽取未来参数/过程/结局。不是从起点功效或Ⅰ类错误。情景不改变原检验计划或已选定D2。",
    "配置JSON、结果RDS与R脚本包含IA个体记录，应按本机研究资料保存。新队列只使用新患者，既有患者分支保留旧风险集续推，二者检验构造不同。",
    "","## 情景与完整请求分母","","```text",tab(r$scenarios),tab(r$overview),tab(r$method_overview),"```","","## 同轮比较与资源","","```text",tab(r$method_pairs),tab(r$scenario_pairs),tab(r$resources),tab(r$stops),"```","",
    "潜在随机数与参数抽样按SIMID在情景间共享；replicate_seed为持久键种子，latent_seed为共同生成种子。Bootstrap只重拟合事件模型，不重抽原IA风险集。历史已停止时不模拟未来或拟合模型。",
    "取消/失败保留请求分母；源码与依赖一致才能续跑。参数后验与样本冻结在准备快照中；追加沿用该准备，B不改变拟合种子。所有新增代码/结果/显示/重放均未验证。")
}
cb_validate_saved <- function(state,cfg) {
  if(!is.null(state$prepared)&&!identical(state$prepared$payload_hash,batch_config_hash(cfg$payload)))stop("准备模型与冻结IA/参数不一致，不能续跑。")
  d<-state$rows;if(!nrow(d))return(invisible(TRUE))
  if(!is.numeric(d$latent_seed)||anyNA(d$latent_seed)||any(d$latent_seed!=vapply(d$SIMID,function(b)batch_replicate_seed(cfg$seed,1L,b),numeric(1))))stop("条件研究共同随机数种子不符。")
  lk<-state$looks;if(nrow(lk)&&(!all(c("SCENARIO","SIMID","method") %in% names(lk))||any(!lk$method %in% cb_methods(cfg))))stop("条件研究路径方法不符。")
  if(nrow(lk)&&all(c("look","method") %in% names(lk))&&anyDuplicated(lk[,c("SCENARIO","SIMID","method","look")]))stop("条件研究存在重复分析路径。")
  for(m in cb_methods(cfg)){
    generated<-d[d$generated %in% TRUE,,drop=FALSE];path<-if(nrow(lk))lk[lk$method==m,,drop=FALSE] else data.frame();key<-function(x)paste(x$SCENARIO,x$SIMID)
    if(!setequal(key(generated),key(path)))stop("已生成条件轮次的分析路径缺失。")
  }
  dr<-state$draw_rows;if(nrow(dr)&&(!all(c("SCENARIO","SIMID") %in% names(dr))||!all(paste(dr$SCENARIO,dr$SIMID) %in% paste(d$SCENARIO,d$SIMID))))stop("抽样参数包含未处理轮次。")
  invisible(TRUE)
}
cb_model_overview <- function(r) {
  e<-r$prepared;if(is.null(e)||!length(e$models))return(data.frame())
  labs<-if(r$config$branch=="new_cohort")names(e$models) else vapply(e$config$groups,`[[`,character(1),"name")
  do.call(rbind,lapply(seq_along(e$models),function(j){m<-e$models[[j]];data.frame(group=labs[j],model=m$id,ia_n=sum(e$data$group==labs[j]),ia_events=sum(e$data$event[e$data$group==labs[j]]),uncertainty=e$config$uncertainty,process_uncertainty=e$config$process_uncertainty %||% "fixed",prepared_seed=e$prepared_seed,mcmc_passed=if(is.null(m$posterior))NA else isTRUE(m$posterior$passed),parameter_vector=paste(format(m$params,digits=8,trim=TRUE),collapse=","),note="准备快照参数；每轮参数另见draw_rows。MCMC诊断状态不代表方法验证。" )}))
}
