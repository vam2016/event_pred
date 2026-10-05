# Design-stage conditional PH with measured covariates and latent multiplicative frailty.
hg_names <- c(logrank="普通log-rank",stratified_logrank="分层log-rank",adjusted_cox="协变量调整Cox")
hg_methods <- function(cfg)unique(c(cfg$primary,cfg$compare_methods))
hg_fields <- c("label","n","hr_scale","center_sd","subject_variance")
hg_strata <- function(text) {
  d<-read.csv(text=text,stringsAsFactors=FALSE,check.names=FALSE)
  if(!identical(names(d),c("stratum","weight","hazard_q","hr"))||!nrow(d)||nrow(d)>12)stop("分层表需1–12行：stratum,weight,hazard_q,hr。")
  if(anyNA(d$stratum)||any(!nzchar(trimws(d$stratum)))||anyDuplicated(d$stratum))stop("层名称需非空唯一。")
  for(k in c("weight","hazard_q","hr"))if(!is.numeric(d[[k]])||any(!is.finite(d[[k]])|d[[k]]<=0))stop("层权重/基准风险倍数/HR须为正。")
  if(any(d$hazard_q<.01|d$hazard_q>100)||any(d$hr<.01|d$hr>5))stop("层基准风险倍数0.01–100，层HR 0.01–5。");d
}
hg_scenarios <- function(cfg) {
  if(cfg$scenario_source=="csv"){
    d<-cfg$scenario_table;if(!is.data.frame(d)||!identical(sort(names(d)),sort(hg_fields)))stop(paste("CSV列名：",paste(hg_fields,collapse=",")));d<-d[,hg_fields,drop=FALSE]
  } else {
    a<-cfg$axes;if(!identical(sort(names(a)),sort(setdiff(hg_fields,"label")))||any(vapply(a,length,integer(1))<1)||prod(vapply(a,length,integer(1)))>60)stop("N/HR倍数/中心SD/个体方差组合最多60情景。")
    d<-expand.grid(a,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE);d$label<-paste0("S",seq_len(nrow(d)));d<-d[,hg_fields,drop=FALSE]
  }
  if(!nrow(d)||nrow(d)>60||anyNA(d$label)||any(!nzchar(trimws(d$label)))||anyDuplicated(d$label))stop("需1–60个非空唯一情景名称。")
  for(k in setdiff(hg_fields,"label"))if(!is.numeric(d[[k]])||any(!is.finite(d[[k]])))stop("情景值需有限数值。")
  if(any(d$n<10|d$n>5000|d$n!=floor(d$n))||any(d$hr_scale<.01|d$hr_scale>5)||any(d$center_sd<0|d$center_sd>2)||any(d$subject_variance<0|d$subject_variance>5))stop("N为10–5000整数；HR倍数0.01–5；中心log-SD 0–2；个体Gamma方差0–5。")
  if(anyDuplicated(d[,setdiff(hg_fields,"label"),drop=FALSE]))stop("情景参数不能完全重复。")
  hrs<-outer(d$hr_scale,cfg$strata$hr);if(any(hrs<.01|hrs>5))stop("每个情景乘倍数后的各层HR须0.01–5。")
  d$null_status<-vapply(d$hr_scale,function(q)if(all(cfg$strata$hr*q==1))"global_survival_null" else "stratum_HR_scenario",character(1));d$SCENARIO<-seq_len(nrow(d));rownames(d)<-NULL;d
}
validate_heterogeneity <- function(cfg) {
  scalar<-function(x,lo,hi,int=FALSE)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi&&(!int||x==floor(x))
  if(!hg_is(cfg)||!identical(cfg$mode,"fixed_truth")||!cfg$primary %in% names(hg_names)||any(!hg_methods(cfg) %in% names(hg_names)))stop("选择异质患者研究的预定主/比较方法。")
  if(!scalar(cfg$reps,20,10000,TRUE)||!scalar(cfg$seed,0,.Machine$integer.max,TRUE)||!scalar(cfg$treatment_fraction,.01,.99)||!scalar(cfg$alpha,.0001,.2)||!cfg$sided %in% c("benefit","two"))stop("B20–10000、整数种子、分配0.01–0.99、alpha0.0001–0.2及预定方向。")
  if(!scalar(cfg$n_centers,1,100,TRUE)||length(cfg$center_weights)!=cfg$n_centers||any(!is.finite(cfg$center_weights)|cfg$center_weights<=0)||!cfg$center_analysis %in% c("none","fixed","cluster"))stop("中心数1–100整数，每中心正权重；分析选择无、固定效应或cluster稳健方差。")
  if(cfg$center_analysis=="cluster"&&cfg$n_centers<2)stop("cluster稳健方差需至少两个计划中心。")
  for(k in c("binary_beta","normal_beta"))if(!scalar(cfg[[k]],-5,5))stop("协变量log风险系数需−5至5。")
  if(!scalar(cfg$binary_probability,0,1)||!scalar(cfg$normal_mean,-100,100)||!scalar(cfg$normal_sd,.001,100))stop("二元概率0–1；正态均值−100至100，SD0.001–100。")
  hg_strata(paste(capture.output(write.csv(cfg$strata,row.names=FALSE)),collapse="\n"))
  if(!cfg$control_model$id %in% parameter_catalog()$id||length(cfg$dropout_rates)!=2||any(!is.finite(cfg$dropout_rates)|cfg$dropout_rates<0))stop("选择Control分布及两组非负独立脱落率。")
  if(!cfg$enroll_mode %in% c("batch","constant","piecewise"))stop("选择一次/恒定/分段入组。")
  if(cfg$enroll_mode=="constant"&&!scalar(cfg$enroll_rate,.Machine$double.eps,1e6))stop("总体入组率需为正。")
  if(cfg$enroll_mode=="piecewise")parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts)
  if(!cfg$cut_rule %in% c("fixed","target")||!scalar(cfg$max_day,.001,3650))stop("固定或事件目标截点，研究窗口最长3650日。")
  if(cfg$cut_rule=="target"&&(!scalar(cfg$target,1,100000,TRUE)||!cfg$miss_policy %in% c("analyze","no_reject")))stop("事件目标需整数，明确未达处理。")
  time_factor(cfg$display_unit);if(is.na(as.Date(cfg$origin))||!cfg$paramcd %in% c("PFS","OS"))stop("选择研究起点及终点。")
  sc<-hg_scenarios(cfg);if(nrow(sc)*cfg$reps>100000||sum(sc$n)*cfg$reps>3e7||nrow(sc)*cfg$reps*length(hg_methods(cfg))>500000)stop("最多10万轮、3000万患者轮、50万方法轮。")
  invisible(TRUE)
}
hg_truth <- function(cfg,sc,b) {
  n<-sc$n;entry<-switch(cfg$enroll_mode,batch=rep(0,n),constant=cumsum(rexp(n,cfg$enroll_rate)),piecewise=inverse_cumhaz(parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts),cumsum(rexp(n))))
  si<-sample.int(nrow(cfg$strata),n,replace=TRUE,prob=cfg$strata$weight);center<-sample.int(cfg$n_centers,n,replace=TRUE,prob=cfg$center_weights);arm<-rbinom(n,1,cfg$treatment_fraction)
  xb<-rbinom(n,1,cfg$binary_probability);xn<-rnorm(n,cfg$normal_mean,cfg$normal_sd)
  uc<-if(sc$center_sd==0)rep(1,cfg$n_centers) else exp(rnorm(cfg$n_centers,-sc$center_sd^2/2,sc$center_sd))
  us<-if(sc$subject_variance==0)rep(1,n) else rgamma(n,shape=1/sc$subject_variance,rate=1/sc$subject_variance)
  hr<-cfg$strata$hr[si]*sc$hr_scale;eta<-cfg$binary_beta*(xb-cfg$binary_probability)+cfg$normal_beta*(xn-cfg$normal_mean)
  multiplier<-cfg$strata$hazard_q[si]*exp(eta)*uc[center]*us*ifelse(arm==1,hr,1)
  if(any(!is.finite(multiplier)|multiplier<=0))stop("患者风险乘数超出数值范围，该轮保留为失败。")
  event<-inverse_cumhaz(cfg$control_model,-log(runif(n))/multiplier);dr<-cfg$dropout_rates[arm+1L];de<-rexp(n);drop<-ifelse(dr>0,de/ifelse(dr>0,dr,1),Inf)
  data.frame(SIMID=b,USUBJID=sprintf("HET%04d-%05d",b,seq_len(n)),group=ifelse(arm==1,"Treatment","Control"),entry_day=entry,event_time_day=event,dropout_time_day=drop,event_day=entry+event,dropout_day=entry+drop,ARM_TRT=arm,XBIN=xb,XNORM=xn,STRATUM=cfg$strata$stratum[si],CENTER=paste0("C",center),U_CENTER=uc[center],U_SUBJECT=us,conditional_hr=hr,hazard_multiplier=multiplier)
}
hg_analysis <- function(o,cfg,method) {
  empty<-list(valid=FALSE,z=NA_real_,p=NA_real_,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,note="")
  tryCatch({
    if(method %in% c("logrank","stratified_logrank")){
      sets<-if(method=="logrank")list(o) else split(o,o$STRATUM)
      scores<-lapply(sets,function(d){if(length(unique(d$group))<2||!nrow(d)||!sum(d$event))return(list(score=0,variance=0));study_logrank(d$time_day,d$event,d$ARM_TRT)})
      u<-sum(vapply(scores,`[[`,numeric(1),"score"));v<-sum(vapply(scores,`[[`,numeric(1),"variance"));if(!is.finite(v)||v<=0)stop("合计log-rank方差无效。")
      z<--u/sqrt(v);return(list(valid=TRUE,z=z,p=if(cfg$sided=="benefit")pnorm(z,lower.tail=FALSE) else 2*pnorm(-abs(z)),estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,note=""))
    }
    if(length(unique(o$ARM_TRT))!=2||sum(o$event)<1)stop("两组风险集或事件不足。")
    terms<-"ARM_TRT";if(length(unique(o$XBIN))>1)terms<-c(terms,"XBIN");if(sd(o$XNORM)>0)terms<-c(terms,"XNORM");if(length(unique(o$STRATUM))>1)terms<-c(terms,"strata(STRATUM)")
    if(cfg$center_analysis=="fixed"&&length(unique(o$CENTER))>1)terms<-c(terms,"factor(CENTER)")
    if(cfg$center_analysis=="cluster"){if(length(unique(o$CENTER))<2)stop("观察到的中心不足，不能计算cluster稳健方差。");terms<-c(terms,"cluster(CENTER)")}
    form<-as.formula(paste("Surv(time_day,event) ~",paste(terms,collapse=" + ")),env=asNamespace("survival"));warnings<-character()
    fit<-withCallingHandlers(survival::coxph(form,data=o,ties="efron",control=survival::coxph.control(iter.max=50,timefix=FALSE)),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")})
    beta<-unname(coef(fit)["ARM_TRT"]);se<-sqrt(vcov(fit)["ARM_TRT","ARM_TRT"])
    if(!is.finite(beta)||!is.finite(se)||se<=0||any(grepl("infinite|converg|singular",warnings,ignore.case=TRUE)))stop(paste(c(warnings,"Treatment系数/方差或收敛无效"),collapse="；"))
    z<--beta/se;list(valid=TRUE,z=z,p=if(cfg$sided=="benefit")pnorm(z,lower.tail=FALSE) else 2*pnorm(-abs(z)),estimate=beta,se=se,lower=beta-qnorm(.975)*se,upper=beta+qnorm(.975)*se,note=paste(warnings,collapse="；"))
  },error=function(e){empty$note<-conditionMessage(e);empty})
}
hg_trial <- function(cfg,sc,b,prepared=NULL,keep_sample=FALSE) {
  seed<-batch_replicate_seed(cfg$seed,sc$SCENARIO,b)
  batch_with_rng(seed,{
    failure<-function(message){mr<-do.call(rbind,lapply(hg_methods(cfg),function(m)data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,method=m,is_primary=m==cfg$primary,generated=FALSE,decision_valid=FALSE,decision_reject=NA,target_reached=NA,action="generation_failed",note=message)));list(row=data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,generated=FALSE,decision_valid=FALSE,decision_reject=NA,generation_note=message),method_rows=mr,looks=data.frame(),draw_rows=data.frame())}
    tryCatch({t<-hg_truth(cfg,sc,b);dates<-sort(t$event_day[is.finite(t$event_day)&t$event_time_day<=t$dropout_time_day&t$event_day<=cfg$max_day]);hit<-if(cfg$cut_rule=="target")length(dates)>=cfg$target else NA;day<-if(cfg$cut_rule=="target"&&hit)dates[cfg$target] else cfg$max_day
      o<-survival_at_cut(t,day,1L);ix<-match(o$USUBJID,t$USUBJID);for(k in c("ARM_TRT","XBIN","XNORM","STRATUM","CENTER"))o[[k]]<-t[[k]][ix]
      performed<-cfg$cut_rule=="fixed"||isTRUE(hit)||cfg$miss_policy=="analyze";closed<-!performed
      hrs<-cfg$strata$hr*sc$hr_scale;common<-length(unique(hrs))==1;target_valid<-common&&sc$subject_variance==0&&(sc$center_sd==0||cfg$center_analysis=="fixed");truth_beta<-if(target_valid)log(hrs[1]) else NA_real_
      mr<-do.call(rbind,lapply(hg_methods(cfg),function(m){a<-hg_analysis(o,cfg,m);data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,method=m,is_primary=m==cfg$primary,generated=TRUE,analysis_valid=a$valid,analysis_performed=performed,decision_valid=closed||a$valid,decision_reject=if(closed)FALSE else if(a$valid)a$p<=cfg$alpha else NA,target_reached=hit,action=if(closed)"window_unreached" else if(!a$valid)"analysis_invalid" else "tested",p_value=a$p,benefit_z=a$z,estimate=a$estimate,se=a$se,lower=a$lower,upper=a$upper,true_loghr=if(m=="adjusted_cox")truth_beta else NA_real_,estimand_status=if(m!="adjusted_cox")"score_only" else if(target_valid)"common_conditional_logHR" else "working_Cox_coefficient",covered=if(m=="adjusted_cox"&&a$valid&&target_valid)a$lower<=truth_beta&&a$upper>=truth_beta else NA,n_observed=nrow(o),events=sum(o$event),n_centers_observed=length(unique(o$CENTER)),n_strata_observed=length(unique(o$STRATUM)),final_day=day,wait_day=day,future_analysis_count=as.integer(performed),null_status=sc$null_status,note=a$note)}))
      p<-mr[mr$is_primary,,drop=FALSE];row<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,generated=TRUE,decision_valid=p$decision_valid,decision_reject=p$decision_reject,final_day=day,events=sum(o$event),n_observed=nrow(o),generation_note="")
      center_draws<-t[!duplicated(t$CENTER),c("CENTER","U_CENTER"),drop=FALSE];draw<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,center=center_draws$CENTER,center_frailty=center_draws$U_CENTER)
      out<-list(row=row,method_rows=mr,looks=data.frame(),draw_rows=draw)
      if(keep_sample){o$SCENARIO<-rep(sc$SCENARIO,nrow(o));t$SCENARIO<-sc$SCENARIO;out$observed<-o;out$truth<-t;out$study_config<-cfg};out
    },error=function(e)failure(conditionMessage(e)))
  })
}
hg_aggregate <- function(state){state<-research_aggregate(state,hg_methods(state$config),hg_names,metric_fields=if(state$config$cut_rule=="fixed")"decision_reject" else c("decision_reject","target_reached"));d<-state$method_rows;rec<-list()
  for(id in state$scenarios$SCENARIO){x<-if(nrow(d))d[d$SCENARIO==id&d$method=="adjusted_cox",,drop=FALSE] else d;valid<-if("analysis_valid" %in% names(x))x$analysis_valid %in% TRUE else logical();cv<-if("covered" %in% names(x))!is.na(x$covered) else logical();err<-if("true_loghr" %in% names(x)&&"estimate" %in% names(x))x$estimate-x$true_loghr else numeric();err<-err[is.finite(err)]
    rec[[length(rec)+1L]]<-data.frame(SCENARIO=id,valid_estimates=sum(valid),recovery_n=length(err),bias_loghr=if(length(err))mean(err) else NA_real_,rmse_loghr=if(length(err))sqrt(mean(err^2)) else NA_real_,coverage_n=sum(cv),coverage=if(any(cv))mean(x$covered[cv]) else NA_real_,note="仅可识别的共同条件logHR计算恢复/覆盖；其他情景保留工作Cox系数，不映射到边际HR真值。")}
  state$recovery<-do.call(rbind,rec);state}
hg_initial <- function(cfg,initial=NULL)research_initial(cfg,hg_scenarios(cfg),hg_aggregate,initial)
run_heterogeneity <- function(cfg,progress=function(state)NULL,should_cancel=function()FALSE,replay_keys=NULL,resume_state=NULL,continuation_operation="resume") {validate_heterogeneity(cfg);research_family_run(cfg,hg_scenarios(cfg),hg_trial,hg_aggregate,progress,should_cancel,replay_keys,resume_state,continuation_operation)}
hg_sample <- function(r,scenario,replicate){sc<-r$scenarios[r$scenarios$SCENARIO==scenario,,drop=FALSE];if(nrow(sc)!=1||!any(r$rows$SCENARIO==scenario&r$rows$SIMID==replicate&r$rows$generated))stop("请选择已生成轮次。");hg_trial(r$config,sc,replicate,keep_sample=TRUE)}
hg_report <- function(r) {tab<-function(d)capture.output(print(d,row.names=FALSE));c("# 协变量、分层、中心与脆弱性模拟研究（开发稿，待复核）","",paste0("研究",r$run_title %||% "","；v",r$version,"；处理",r$completed,"/",r$total,"；",r$status),"基准条件PH，层基准风险与层HR、二元/正态协变量、中心log-normal及个体Gamma脆弱性。简单随机分配；潜在脆弱性不进入观察数据或分析模型。",
    "预定主方法与同轮附加普通/分层log-rank及调整Cox比较。层HR不同或遗漏脆弱性时，Cox为工作系数，不声称同一边际HR真值。cluster稳健方差不等于调整中心风险，也不估计中心随机效应。",
    "","## 情景与分母","","```text",tab(r$scenarios),tab(r$overview),tab(r$method_overview),"```","","## 配对、资源与条件效应恢复","","```text",tab(r$method_pairs),tab(r$resources),tab(r$recovery),"```","",
    "所有新增代码、方法/数值、结果、浏览器、公式显示和实际重放未验证；不包含集群随机化或实际异质数据拟合。")}
