# Full-filtration counting likelihoods: specified PFS intensity and state-invariant death.
je_is <- function(cfg)identical(cfg$research_family,"joint_sequential_likelihood")
je_names <- c(original="原事件计划",adaptive="预定联合事件目标重估")
je_methods <- function(cfg)unique(c(cfg$primary,cfg$compare_methods %||% character()))
je_scenarios <- function(cfg){d<-cfg$scenario_table;required<-c("label","n","q01","qdeath","enroll_scale","dropout_scale");if(!is.data.frame(d)||!identical(sort(names(d)),sort(required))||nrow(d)<1||nrow(d)>30||anyNA(d)||anyDuplicated(d$label)||any(!nzchar(d$label)))stop("情景CSV严格六列label,n,q01,qdeath,enroll_scale,dropout_scale；1–30唯一情景。");for(k in setdiff(required,"label"))if(!is.numeric(d[[k]])||any(!is.finite(d[[k]])))stop("情景参数需有限数值。");if(any(d$n<10|d$n>5000|d$n!=floor(d$n))||any(d$q01<.01|d$q01>10|d$qdeath<.01|d$qdeath>10|d$enroll_scale<.01|d$enroll_scale>10|d$dropout_scale<0|d$dropout_scale>10))stop("N10–5000整数；风险/入组倍数0.01–10，脱落倍数0–10。");d$SCENARIO<-seq_len(nrow(d));d}
je_model_scale <- function(m,q){if(m$id %in% c("exponential","pwe"))m$params<-m$params*q else if(m$id=="weibull")m$params[1]<-m$params[1]-m$params[2]*log(q) else stop("转移仅指数/Weibull/PWE。");m}
je_models <- function(cfg,q01=1,qdeath=1){c<-list("01"=cfg$progression_model,"02"=cfg$death_model,"12"=cfg$death_model);t<-c;t[["01"]]<-je_model_scale(c[["01"]],q01);t[["02"]]<-t[["12"]]<-je_model_scale(cfg$death_model,qdeath);list(Control=c,Treatment=t)}
je_logsum <- function(x){m<-max(x);if(is.infinite(m))m else m+log(sum(exp(x-m)))}
je_loge <- function(N,Y,cfg)je_logsum(log(cfg$mixture_weights)+N*log(cfg$mixture_hr)-(cfg$mixture_hr-1)*Y)
je_statistics <- function(d,cfg){ints<-ms_intervals(d,"forward");t<-ints[ints$GROUP=="Treatment",];p<-t[t$transition=="01",];o<-d[d$GROUP=="Treatment",];Yp<-if(nrow(p))sum(model_cumhaz(cfg$progression_model,p$stop)-model_cumhaz(cfg$progression_model,p$start)+model_cumhaz(cfg$death_model,p$stop)-model_cumhaz(cfg$death_model,p$start)) else 0
  Np<-sum(o$STATE==2|is.finite(o$PROG));No<-sum(o$STATE==2);Yo<-sum(o$TIME)*cfg$death_model$params[1]
  data.frame(PARAMCD=c("PFS","OS"),events=c(Np,No),null_compensator=c(Yp,Yo),log_e=c(je_loge(Np,Yp,cfg),je_loge(No,Yo,cfg)))
}
je_snapshot_at <- function(truth,day){s<-joint_state_records(truth,day,1L,"forward")$states;if(!nrow(s))return(data.frame(USUBJID=character(),GROUP=character(),ENTRY=numeric(),TIME=numeric(),STATE=integer(),EXIT=character(),PROG=numeric()))
  out<-lapply(seq_len(nrow(s)),function(i){a<-s[i,];x<-truth[truth$USUBJID==a$USUBJID,];last<-a$last_observed_day;dead<-x$death_day<=last;drop<-!dead&&x$dropout_day<=day;prog<-if(x$progression_day<=last)x$progression_time_day else NA_real_;data.frame(USUBJID=a$USUBJID,GROUP=x$group,ENTRY=x$entry_day,TIME=last-x$entry_day,STATE=if(dead)2L else if(is.finite(prog))1L else 0L,EXIT=if(dead)"event" else if(drop)"dropout" else "active",PROG=prog)});do.call(rbind,out)
}
je_prediction_cfg <- function(cfg,sc,cut,new_n){list(cut=cut,clock="forward",new_n=new_n,enroll_rate=cfg$enroll_rate*sc$enroll_scale,group_weights=c(Control=1-cfg$treatment_fraction,Treatment=cfg$treatment_fraction),dropout_rates=cfg$dropout_rates*sc$dropout_scale,max_day=cfg$max_day)}
je_event_day <- function(truth,endpoint,target,min_day,max_day){t<-joint_endpoint_truth(truth,endpoint);v<-sort(t$event_day[is.finite(t$event_day)&t$event_time_day<=t$dropout_time_day]);day<-if(length(v)>=target)v[target] else Inf;list(day=if(day<=max_day)max(min_day,day) else max_day,reached=day<=max_day)}
je_decision <- function(logmax,cfg){p<-pmin(1,exp(-logmax));names(p)<-c("PFS","OS");jr_policy_decisions(p,cfg$policy,cfg)}
je_predictive_cp <- function(d,cfg,sc,logmax){pc<-je_prediction_cfg(cfg,sc,cfg$pilot_day,0);pc$cut<-cfg$pilot_day;pc$new_n<-max(0,sc$n-nrow(d));models<-je_models(cfg,cfg$planning_q01,cfg$planning_qdeath);vals<-rep(NA,cfg$cp_reps)
  for(j in seq_len(cfg$cp_reps)){a<-tryCatch({tr<-ms_continue(d,models,pc,j);c<-je_event_day(tr,cfg$monitor_endpoint,cfg$dplan,cfg$pilot_day,cfg$max_day);s<-je_statistics(je_snapshot_at(tr,c$day),cfg);je_decision(pmax(logmax,s$log_e),cfg)$success},error=function(e)NA);vals[j]<-a}
  V<-sum(!is.na(vals));list(value=if(V)mean(vals[!is.na(vals)]) else NA_real_,valid=V,requested=cfg$cp_reps)
}
validate_joint_sequential <- function(cfg){sc<-je_scenarios(cfg)
  if(!je_is(cfg)||!cfg$purpose %in% c("design","conditional","observed")||!isTRUE(cfg$predeclared))stop("联合似然检验需确认零假设模型与混合在查看本研究结局前预定。")
  if(!ms_valid_model(cfg$progression_model)||cfg$death_model$id!="exponential"||cfg$death_model$params[1]<=0||any(!is.finite(cfg$death_model$params))||any(!is.finite(cfg$progression_model$params)))stop("进展使用指数/Weibull/PWE；02与12必须同一正指数死亡风险，预先指定且已知。")
  if(!cfg$policy %in% c("co_primary","bonferroni","holm","fixed_sequence")||!cfg$success_goal %in% c("any","both","PFS","OS")||(cfg$policy=="co_primary"&&cfg$success_goal!="both")||cfg$pfs_alpha_weight<=0||cfg$pfs_alpha_weight>=1||!cfg$first_endpoint %in% c("PFS","OS")||cfg$alpha<=0||cfg$alpha>.2)stop("联合规则、成功定义、alpha或权重错误。")
  if(!length(cfg$mixture_hr)||length(cfg$mixture_hr)>50||any(!is.finite(cfg$mixture_hr)|cfg$mixture_hr<=0|cfg$mixture_hr>=1)||length(cfg$mixture_weights)!=length(cfg$mixture_hr)||any(!is.finite(cfg$mixture_weights)|cfg$mixture_weights<=0)||abs(sum(cfg$mixture_weights)-1)>1e-8)stop("1–50个预定获益倍数0<r<1，权重正且和1。")
  if(any(!je_methods(cfg) %in% names(je_names))||!cfg$monitor_endpoint %in% c("PFS","OS")||!cfg$adapt_rule %in% c("bounded","promising"))stop("选择事件规则和监控终点。")
  if(cfg$purpose!="design"){ms_snapshot(cfg$data,cfg$cut);if(!all(cfg$data$GROUP %in% c("Control","Treatment")))stop("实际IA组名必须Control/Treatment。");if(any(sc$n<nrow(cfg$data)))stop("计划N不得少于冻结IA人数。")}
  if(cfg$purpose!="observed"){
    if(cfg$reps<20||cfg$reps>10000||cfg$reps!=floor(cfg$reps)||cfg$dplan<1||cfg$dplan!=floor(cfg$dplan)||cfg$dmax<cfg$dplan||cfg$dmax!=floor(cfg$dmax)||cfg$dmax>100000||cfg$max_day<=cfg$cut||cfg$max_day>3650||cfg$enroll_rate<=0||cfg$treatment_fraction<=0||cfg$treatment_fraction>=1||any(cfg$dropout_rates<0))stop("B20–10000，事件目标正整数，上限≥原目标；窗口IA之后≤3650日，正入组率，分配概率(0,1)。")
    if(length(cfg$look_fractions)<1||length(cfg$look_fractions)>10||any(cfg$look_fractions<=0|cfg$look_fractions>1)||is.unsorted(cfg$look_fractions,strictly=TRUE)||tail(cfg$look_fractions,1)!=1)stop("未来分析目标比例正递增、最多10次、末次1。")
    if(cfg$purpose=="design"&&(cfg$pilot_target<1||cfg$pilot_target>=cfg$dplan||cfg$pilot_target!=floor(cfg$pilot_target)))stop("设计研究IA事件目标正整数且小于原Final。");if(cfg$purpose=="conditional"&&cfg$dplan<=sum(if(cfg$monitor_endpoint=="PFS")cfg$data$STATE==2|is.finite(cfg$data$PROG) else cfg$data$STATE==2))stop("Final须大于冻结IA已知监控事件数。")
    if(cfg$cp_reps<10||cfg$cp_reps>200||cfg$cp_reps!=floor(cfg$cp_reps)||cfg$cp_min<0||cfg$cp_min>=cfg$cp_target||cfg$cp_target>=1||cfg$inflation<1||cfg$inflation>5||!is.finite(cfg$planning_q01)||!is.finite(cfg$planning_qdeath)||cfg$planning_q01<=0||cfg$planning_qdeath<=0)stop("规划预测M10–200，0≤CPmin<CPtarget<1；事件倍数1–5，未来规划转移倍数正。")
    if(nrow(sc)*cfg$reps>20000||sum(sc$n)*cfg$reps>1e7||(length(je_methods(cfg))>1||cfg$primary=="adaptive")&&sum(sc$n)*cfg$reps*cfg$cp_reps>2e7)stop("外层最多2万轮/1000万患者；条件规划总量≤2000万患者。")
  }
  if(!is.finite(cfg$seed)||cfg$seed<0||cfg$seed>.Machine$integer.max||cfg$seed!=floor(cfg$seed))stop("种子需非负整数。")
  invisible(TRUE)
}
je_observed <- function(cfg){s<-je_statistics(cfg$data,cfg);lm<-pmax(0,s$log_e);d<-je_decision(lm,cfg);list(statistics=transform(s,anytime_p=pmin(1,exp(-lm)),claim=as.logical(d$claims)),decision=d,note="此IA当前似然值及后续指定观察序列；未提供历史路径，不能推算未记录的过往最大E。")}
je_trial <- function(cfg,sc,b,prepared=NULL,keep_sample=FALSE)batch_with_rng(batch_replicate_seed(cfg$seed,sc$SCENARIO,b),{
  seed<-batch_replicate_seed(cfg$seed,sc$SCENARIO,b)
  tryCatch({models<-je_models(cfg,sc$q01,sc$qdeath);pc<-je_prediction_cfg(cfg,sc,if(cfg$purpose=="conditional")cfg$cut else 0,if(cfg$purpose=="conditional")sc$n-nrow(cfg$data) else sc$n)
    base<-if(cfg$purpose=="conditional")cfg$data else data.frame();truth<-ms_continue(base,models,pc,b)
    ia<-if(cfg$purpose=="conditional")list(day=cfg$cut,reached=TRUE) else je_event_day(truth,cfg$monitor_endpoint,cfg$pilot_target,0,cfg$max_day)
    frozen<-je_snapshot_at(truth,ia$day);st<-je_statistics(frozen,cfg);lm0<-pmax(0,st$log_e);cfg$pilot_day<-ia$day
    cp<-if("adaptive" %in% je_methods(cfg)&&ia$reached)batch_with_rng(batch_replicate_seed(seed,sc$SCENARIO,997L),je_predictive_cp(frozen,cfg,sc,lm0)) else list(value=NA_real_,valid=0,requested=cfg$cp_reps)
    increase<-is.finite(cp$value)&&cp$value<cfg$cp_target&&(cfg$adapt_rule=="bounded"||cp$value>=cfg$cp_min)
    Dnew<-if(increase)min(cfg$dmax,ceiling(cfg$dplan*cfg$inflation)) else cfg$dplan
    mr<-looks<-observed<-list()
    for(m in je_methods(cfg)){
      D<-if(m=="adaptive")Dnew else cfg$dplan;known<-sum(if(cfg$monitor_endpoint=="PFS")frozen$STATE==2|is.finite(frozen$PROG) else frozen$STATE==2)
      targets<-unique(pmax(known+1L,ceiling(known+(D-known)*cfg$look_fractions)));lm<-lm0;decl<-je_decision(lm,cfg);reason<-if(isTRUE(decl$success))"ia_success" else if(!ia$reached)"ia_window" else "final";day<-ia$day;nlooks<-0L;reached<-ia$reached
      record<-function(k,t,ss,dec,target){data.frame(SCENARIO=sc$SCENARIO,SIMID=b,method=m,look=k,DCO_DAY=t,event_target=target,PARAMCD=c("PFS","OS"),events=ss$events,null_compensator=ss$null_compensator,log_e=ss$log_e,log_running_e=lm,anytime_p=pmin(1,exp(-lm)),claim=as.logical(dec$claims),joint_success=dec$success)}
      looks[[length(looks)+1L]]<-record(0L,day,st,decl,known)
      if(ia$reached&&!isTRUE(decl$success))for(k in seq_along(targets)){
        c<-je_event_day(truth,cfg$monitor_endpoint,targets[k],ia$day,cfg$max_day);day<-c$day;reached<-c$reached;ss<-je_statistics(je_snapshot_at(truth,day),cfg);lm<-pmax(lm,ss$log_e);decl<-je_decision(lm,cfg);nlooks<-nlooks+1L;looks[[length(looks)+1L]]<-record(k,day,ss,decl,targets[k]);if(isTRUE(decl$success)){reason<-"joint_success";break};if(!reached){reason<-"window";break}
      }
      d<-je_snapshot_at(truth,day);nullp<-if(sc$q01==1&&sc$qdeath==1)TRUE else NA; nullo<-if(sc$qdeath==1)TRUE else NA
      if(cfg$purpose=="conditional"){nullp<-nullo<-NA}
      mr[[length(mr)+1L]]<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,method=m,generated=TRUE,decision_valid=!is.na(decl$success),decision_reject=decl$success,target_reached=reached,pfs_claim=decl$claims[1],os_claim=decl$claims[2],any_claim=decl$any,both_claim=decl$both,false_claim=jr_false_claim(decl$claims,c(PFS=nullp,OS=nullo)),final_target=D,closing_day=day,n_observed=nrow(d),observed_events=sum(if(cfg$monitor_endpoint=="PFS")d$STATE==2|is.finite(d$PROG) else d$STATE==2),future_looks=nlooks,stop_reason=reason,planning_cp=cp$value,planning_valid=cp$valid,planning_requested=cp$requested,adapted=m=="adaptive"&&increase,ia_day=ia$day,global_null_future_error_upper=if(cfg$purpose=="conditional")min(1,cfg$alpha*sum(exp(st$log_e)*if(cfg$policy=="bonferroni")c(cfg$pfs_alpha_weight,1-cfg$pfs_alpha_weight) else 1)) else NA_real_)
      if(keep_sample){o<-batch_bind_columns(lapply(c("PFS","OS"),function(e)joint_observed_at_cut(truth,e,day,1L)));o$design<-m;observed[[m]]<-o}
    }
    methods<-batch_bind_columns(mr);methods$final_day<-methods$closing_day;methods$events<-methods$observed_events;methods$action<-methods$stop_reason;methods$wait_day<-methods$closing_day-methods$ia_day;methods$future_analysis_count<-methods$future_looks;p<-methods[methods$method==cfg$primary,];row<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,generated=TRUE,decision_valid=p$decision_valid,decision_reject=p$decision_reject,note="")
    out<-list(row=row,method_rows=methods,looks=batch_bind_columns(looks),draw_rows=data.frame());if(keep_sample){out$truth<-truth;out$observed<-batch_bind_columns(observed);out$frozen_ia<-frozen;out$study_config<-list(origin=cfg$origin,display_unit="days")};out
  },error=function(e){list(row=data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,generated=FALSE,decision_valid=FALSE,decision_reject=NA,note=conditionMessage(e)),method_rows=batch_bind_columns(lapply(je_methods(cfg),function(m)data.frame(SCENARIO=sc$SCENARIO,SIMID=b,method=m,generated=FALSE,decision_valid=FALSE,decision_reject=NA,target_reached=NA))),looks=data.frame(),draw_rows=data.frame())})
})
je_aggregate <- function(state){state<-research_aggregate(state,je_methods(state$config),je_names,c("decision_reject","target_reached","pfs_claim","os_claim","any_claim","both_claim","false_claim"));state$ia_overview<-if(state$config$purpose=="conditional")je_observed(state$config)$statistics else data.frame();state}
run_joint_sequential <- function(cfg,progress=function(state)NULL,should_cancel=function()FALSE,replay_keys=NULL,resume_state=NULL,continuation_operation="resume"){validate_joint_sequential(cfg);if(cfg$purpose=="observed")stop("当前观察推断无需后台批量运行。");research_family_run(cfg,je_scenarios(cfg),je_trial,je_aggregate,progress,should_cancel,replay_keys,resume_state,continuation_operation)}
je_initial <- function(cfg,initial=NULL)research_initial(cfg,je_scenarios(cfg),je_aggregate,initial)
je_sample <- function(r,scenario,replicate){if(!any(r$rows$SCENARIO==scenario&r$rows$SIMID==replicate&r$rows$generated))stop("请选择已生成轮次。");je_trial(r$config,r$scenarios[scenario,,drop=FALSE],replicate,NULL,TRUE)}
je_report <- function(r)c("# 联合终点计数似然序贯研究（开发稿）",paste0("版本",r$version,"；",r$config$purpose,"；处理",r$completed,"/",r$total),"预定零假设：PFS状态0累计风险H01+lambda_death*t；OS在状态0/1中同一已知死亡风险。治疗组使用预定获益混合，Control用于共同患者生成，不用于估计已知风险。",capture.output(print(r$overview,row.names=FALSE)),capture.output(print(r$method_overview,row.names=FALSE)),capture.output(print(r$resources,row.names=FALSE)),"规划CP只比较IA与原Final，有限M预测用于选择事件目标，不保证达到目标CP；正式检验保留原混合及联合多重性规则。任意一般三转移模型OS、拟合基准代入、双侧/无效界/alpha回收和同时区间尚不支持。实际IA条件比例不能解释为无条件功效或FWER。未开展验证。")
