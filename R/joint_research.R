# Two-endpoint studies on a shared illness-death trajectory; development draft.
jr_is <- function(cfg)identical(cfg$research_family,"joint_endpoints")
jr_policy_names <- c(co_primary="共同主要终点：两者均通过",bonferroni="加权Bonferroni",holm="Holm逐步拒绝",fixed_sequence="预定固定顺序",unadjusted="未调整逐终点（描述参照）")
jr_policies <- function(cfg)unique(c(cfg$primary,cfg$compare_policies %||% character()))
jr_fields <- function(cfg)c("label","n","q01","q02","q12","enroll_scale","dropout_scale",switch(cfg$cut_rule,fixed="dco",pfs="target_pfs",os="target_os",both=c("target_pfs","target_os"),first=c("target_pfs","target_os")))
jr_profiles <- function(text) {
  lines<-trimws(strsplit(text,"\n",fixed=TRUE)[[1]]);lines<-lines[nzchar(lines)]
  if(!length(lines)||length(lines)>12)stop("转移效应定义需1–12行。")
  out<-lapply(lines,function(line){v<-trimws(strsplit(line,"|",fixed=TRUE)[[1]]);if(length(v)!=2||!nzchar(v[1]))stop("每行：名称 | q01,q02,q12。");q<-parse_unit_numbers(v[2]);if(length(q)!=3||any(q<.01|q>100))stop("每行需3个0.01–100的转移风险倍数。");data.frame(profile=v[1],q01=q[1],q02=q[2],q12=q[3])})
  d<-do.call(rbind,out);if(anyDuplicated(d$profile))stop("转移效应名称需唯一。");d
}
jr_scenarios <- function(cfg) {
  fields<-jr_fields(cfg)
  if(cfg$scenario_source=="csv") {
    d<-cfg$scenario_table
    if(!is.data.frame(d)||!identical(sort(names(d)),sort(fields)))stop(paste("当前情景CSV列名：",paste(fields,collapse=",")))
    d<-d[,fields,drop=FALSE]
  } else if(cfg$scenario_source=="grid") {
    axes<-cfg$axes
    if(!identical(sort(names(axes)),sort(c("n","enroll_scale","dropout_scale",setdiff(fields,c("label","n","q01","q02","q12","enroll_scale","dropout_scale"))))))stop("联合情景轴不完整。")
    count<-prod(vapply(axes,length,integer(1)))*nrow(cfg$profiles)
    if(any(vapply(axes,length,integer(1))<1)||count<1||count>60)stop("所有轴与转移效应定义组合需1–60情景。")
    d<-merge(expand.grid(axes,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE),cfg$profiles,by=NULL,sort=FALSE)
    d$label<-paste0("S",seq_len(nrow(d)),"_",d$profile);d<-d[,fields,drop=FALSE]
  } else stop("请选择列表组合或情景CSV。")
  if(!nrow(d)||nrow(d)>60||anyNA(d$label)||any(!nzchar(trimws(d$label)))||anyDuplicated(d$label))stop("需1–60个非空唯一情景名称。")
  for(k in setdiff(fields,"label"))if(!is.numeric(d[[k]])||any(!is.finite(d[[k]])))stop(paste0(k,"需为有限数值。"))
  if(any(d$n<10|d$n>5000|d$n!=floor(d$n))||any(as.matrix(d[,c("q01","q02","q12")])<.01|as.matrix(d[,c("q01","q02","q12")])>100)||any(d$enroll_scale<.01|d$enroll_scale>100)||any(d$dropout_scale<0|d$dropout_scale>100))stop("N为10–5000整数；转移/入组倍数0.01–100；退出倍数0–100。")
  for(k in intersect(fields,c("target_pfs","target_os")))if(any(d[[k]]<1|d[[k]]>100000|d[[k]]!=floor(d[[k]])))stop("事件目标需1–100000整数。")
  if(cfg$cut_rule=="fixed"){
    d$dco_day<-d$dco*time_factor(if(cfg$scenario_source=="csv")cfg$table_unit else cfg$display_unit)
    if(any(d$dco_day<=0|d$dco_day>3650))stop("固定DCO换算后需在(0,3650]日。")
  }
  if(anyDuplicated(d[,setdiff(fields,"label"),drop=FALSE]))stop("联合情景参数不能完全重复。")
  d$SCENARIO<-seq_len(nrow(d));rownames(d)<-NULL;d
}
validate_joint_research <- function(cfg) {
  scalar<-function(x,lo,hi,integer=FALSE)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi&&(!integer||x==floor(x))
  if(!jr_is(cfg)||!identical(cfg$mode,"fixed_truth")||!identical(cfg$design_mode,"fixed")||!cfg$cut_rule %in% c("fixed","pfs","os","both","first"))stop("联合终点研究限定固定真值、共同一次Final及支持的截点规则。")
  if(!scalar(cfg$reps,20,10000,TRUE)||!scalar(cfg$seed,0,.Machine$integer.max,TRUE)||!scalar(cfg$alpha,.0001,.2)||!cfg$sided %in% c("benefit","two"))stop("重复数20–10000、种子为整数；alpha 0.0001–0.2，选择统一方向。")
  if(any(!jr_policies(cfg) %in% names(jr_policy_names))||!cfg$primary %in% names(jr_policy_names)||length(cfg$primary)!=1)stop("未知预定联合规则。")
  if(!cfg$success_goal %in% c("any","both","PFS","OS")||(cfg$primary=="co_primary"&&cfg$success_goal!="both"))stop("共同主要终点的主成功定义须为两者均通过。")
  if("bonferroni" %in% jr_policies(cfg)&&!scalar(cfg$pfs_alpha_weight,.01,.99))stop("PFS Bonferroni alpha权重需0.01–0.99，OS用1−w。")
  if("fixed_sequence" %in% jr_policies(cfg)&&!cfg$first_endpoint %in% c("PFS","OS"))stop("需预定固定顺序的首终点。")
  if(!cfg$miss_policy %in% c("analyze","no_reject")||(cfg$cut_rule!="fixed"&&!scalar(cfg$max_day,.001,3650)))stop("事件规则需最大窗口及未达目标处理。")
  if(length(cfg$joint_base$groups)!=2||!cfg$joint_base$enroll_mode %in% c("constant","piecewise"))stop("联合设计研究限Control/Treatment及恒定/分段Poisson总体入组。")
  validate_joint_config(cfg$joint_base)
  for(e in c("PFS","OS")){
    a<-cfg$analyses[[e]]
    if(is.null(a)||!a$method %in% names(batch_method_names))stop("需预定每终点分析方法。")
    if(a$method=="fh"&&(!scalar(a$rho,0,5)||!scalar(a$gamma,0,5)))stop("FH rho/gamma需0–5。")
    if(a$method %in% c("rmst","survival")&&!scalar(a$tau_day,.001,3650))stop("每终点tau需正、最长3650日。")
  }
  sc<-jr_scenarios(cfg)
  if(nrow(sc)*cfg$reps>100000||sum(sc$n)*cfg$reps>3e7||nrow(sc)*cfg$reps*length(jr_policies(cfg))>500000)stop("联合研究最多10万轮、3000万患者×轮次、50万规则×轮次。")
  invisible(TRUE)
}
jr_trial_config <- function(cfg,sc) {
  c<-cfg$joint_base;c$n<-sc$n;c$reps<-1;c$seed<-cfg$seed
  c$groups[[2]]$multipliers<-setNames(as.numeric(unlist(sc[1,c("q01","q02","q12")],use.names=FALSE)),c("01","02","12"))
  for(j in 1:2)c$groups[[j]]$dropout_rate<-c$groups[[j]]$dropout_rate*sc$dropout_scale
  if(c$enroll_mode=="constant")c$enroll_rate<-c$enroll_rate*sc$enroll_scale else c$enroll_rates<-c$enroll_rates*sc$enroll_scale
  c
}
jr_analysis_cut <- function(truth,cfg,sc) {
  if(cfg$cut_rule=="fixed")return(list(day=sc$dco_day,reached=TRUE,pfs_target_day=NA_real_,os_target_day=NA_real_))
  event_day<-function(e,k){if(is.null(k))return(NA_real_);t<-joint_endpoint_truth(truth,e);d<-sort(t$event_day[is.finite(t$event_day)&t$event_time_day<=t$dropout_time_day&t$event_day<=cfg$max_day]);if(length(d)>=k)d[k] else Inf}
  p<-event_day("PFS",sc$target_pfs);o<-event_day("OS",sc$target_os)
  planned<-switch(cfg$cut_rule,pfs=p,os=o,both=max(p,o),first=min(p,o))
  reached<-is.finite(planned)&&planned<=cfg$max_day
  list(day=if(reached)planned else cfg$max_day,reached=reached,pfs_target_day=if(is.finite(p))p else NA_real_,os_target_day=if(is.finite(o))o else NA_real_)
}
jr_endpoint_analysis <- function(observed,cfg,endpoint) {
  a<-cfg$analyses[[endpoint]];c<-list(primary=a$method,sided=cfg$sided,alpha=cfg$alpha,estimate_hr=a$method=="cox",fh_rho=a$rho,fh_gamma=a$gamma,analysis_tau=a$tau_day)
  empty<-list(valid=FALSE,p=NA_real_,benefit_z=NA_real_,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,note="")
  tryCatch({
    if(a$method %in% c("logrank","cox")){
      d<-analyze_study_trial(observed,c,NA_real_)
      if(a$method=="logrank")list(valid=d$logrank_valid,p=d$logrank_p,benefit_z=-d$logrank_z,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,note=d$logrank_note)
      else list(valid=d$cox_valid,p=d$cox_p,benefit_z=-d$beta/d$se_beta,estimate=d$beta,se=d$se_beta,lower=log(d$hr_lower),upper=log(d$hr_upper),note=d$cox_note)
    } else {
      d<-study_extended_analysis(observed,c,NA_real_)
      if(a$method=="fh")list(valid=d$fh_valid,p=d$fh_p,benefit_z=-d$fh_z,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,note=d$fh_note)
      else list(valid=d$contrast_valid,p=d$contrast_p,benefit_z=d$contrast_z,estimate=d$contrast_estimate,se=d$contrast_se,lower=d$contrast_lower,upper=d$contrast_upper,note=d$contrast_note)
    }
  },error=function(e){empty$note<-conditionMessage(e);empty})
}
jr_apply_policy <- function(p,policy,cfg) {
  p<-setNames(as.numeric(p),c("PFS","OS"));a<-cfg$alpha
  if(policy=="co_primary")return(setNames(rep(all(p<=a),2),names(p)))
  if(policy=="bonferroni")return(p<=a*c(PFS=cfg$pfs_alpha_weight,OS=1-cfg$pfs_alpha_weight))
  if(policy=="holm")return(stats::p.adjust(p,method="holm")<=a)
  if(policy=="unadjusted")return(p<=a)
  if(policy=="fixed_sequence"){
    first<-cfg$first_endpoint;second<-setdiff(names(p),first);d<-setNames(c(FALSE,FALSE),names(p));d[first]<-p[first]<=a;d[second]<-d[first]&&p[second]<=a;return(d)
  }
  stop("未知联合规则。")
}
jr_adjusted_p <- function(p,policy,cfg) {
  p<-setNames(as.numeric(p),c("PFS","OS"))
  switch(policy,co_primary=setNames(rep(max(p),2),names(p)),bonferroni=pmin(1,p/c(PFS=cfg$pfs_alpha_weight,OS=1-cfg$pfs_alpha_weight)),holm=stats::p.adjust(p,"holm"),
    fixed_sequence={first<-cfg$first_endpoint;second<-setdiff(names(p),first);d<-p;d[second]<-max(p);d},unadjusted=p,stop("未知规则。"))
}
jr_tri_any <- function(x)if(any(x %in% TRUE))TRUE else if(anyNA(x))NA else FALSE
jr_tri_all <- function(x)if(any(x %in% FALSE))FALSE else if(anyNA(x))NA else TRUE
jr_resolve <- function(low,high)ifelse(low==high,low,NA)
jr_policy_decisions <- function(p,policy,cfg,closed=FALSE) {
  p[!is.finite(p)]<-NA_real_
  if(closed)return(list(claims=c(PFS=FALSE,OS=FALSE),any=FALSE,both=FALSE,success=FALSE,adjusted=c(PFS=NA_real_,OS=NA_real_),adjusted_lower=c(PFS=NA_real_,OS=NA_real_),adjusted_upper=c(PFS=NA_real_,OS=NA_real_)))
  lower_p<-ifelse(is.na(p),0,p);upper_p<-ifelse(is.na(p),1,p)
  low<-jr_apply_policy(upper_p,policy,cfg);high<-jr_apply_policy(lower_p,policy,cfg)
  claims<-jr_resolve(low,high);names(claims)<-c("PFS","OS")
  any<-jr_resolve(any(low),any(high));both<-jr_resolve(all(low),all(high))
  success<-switch(cfg$success_goal,any=any,both=both,PFS=claims["PFS"],OS=claims["OS"])
  adjusted_low<-jr_adjusted_p(lower_p,policy,cfg);adjusted_high<-jr_adjusted_p(upper_p,policy,cfg)
  list(claims=claims,any=unname(any),both=unname(both),success=unname(success),adjusted=ifelse(adjusted_low==adjusted_high,adjusted_low,NA_real_),adjusted_lower=adjusted_low,adjusted_upper=adjusted_high)
}
# Sufficient null conditions only; differences in transition risks do not prove a marginal alternative.
jr_nulls <- function(cfg,sc) {
  q<-unlist(sc[1,c("q01","q02","q12")],use.names=FALSE);joint<-all(q==1)
  pfs<-if(q[1]==1&&q[2]==1)TRUE else NA
  m02<-cfg$joint_base$transitions[["02"]]$model;m12<-cfg$joint_base$transitions[["12"]]$model
  memoryless_os<-m02$id=="exponential"&&m12$id=="exponential"&&identical(unname(m02$params),unname(m12$params))&&q[2]==1&&q[3]==1
  os<-if(joint||memoryless_os)TRUE else NA
  # RMST and survival nulls follow from the sufficient equality of full marginal laws.
  c(PFS=pfs,OS=os)
}
jr_false_claim <- function(claims,nulls) {
  known_null<-nulls %in% TRUE
  if(any(claims[known_null] %in% TRUE))return(TRUE)
  if(anyNA(claims[known_null])||any((claims[!known_null] %in% TRUE)|is.na(claims[!known_null])))return(NA)
  FALSE
}
jr_trial <- function(cfg,sc,b,keep_sample=FALSE) {
  seed<-batch_replicate_seed(cfg$seed,sc$SCENARIO,b)
  batch_with_rng(seed,{
    tc<-jr_trial_config(cfg,sc);nulls<-jr_nulls(cfg,sc)
    failed<-function(message){
      row<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,n=sc$n,generated=FALSE,DCO_DAY=NA_real_,n_observed=NA_integer_,pfs_events=NA_integer_,os_events=NA_integer_,rule_reached=NA,formal_test_performed=NA,pfs_target_day=NA_real_,os_target_day=NA_real_,generation_note=message)
      endpoints<-do.call(rbind,lapply(c("PFS","OS"),function(e)data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,method=e,PARAMCD=e,analysis_method=cfg$analyses[[e]]$method,generated=FALSE,analysis_valid=FALSE,analysis_performed=FALSE,decision_valid=FALSE,decision_reject=NA,p_value=NA_real_,benefit_z=NA_real_,estimate=NA_real_,se=NA_real_,lower=NA_real_,upper=NA_real_,estimate_unit=if(cfg$analyses[[e]]$method=="rmst")"days" else if(cfg$analyses[[e]]$method=="survival")"probability" else if(cfg$analyses[[e]]$method=="cox")"logHR" else "score_only",analysis_note=message)))
      policies<-do.call(rbind,lapply(jr_policies(cfg),function(m)data.frame(SCENARIO=sc$SCENARIO,SIMID=b,policy=m,is_primary=m==cfg$primary,formal_test_performed=FALSE,pfs_adjusted_p=NA_real_,os_adjusted_p=NA_real_,pfs_adjusted_lower=NA_real_,pfs_adjusted_upper=NA_real_,os_adjusted_lower=NA_real_,os_adjusted_upper=NA_real_,pfs_claim=NA,os_claim=NA,any_claim=NA,both_claim=NA,success=NA,false_claim=NA)))
      row$decision_valid<-FALSE;row$decision_reject<-NA;list(row=row,method_rows=endpoints,policy_rows=policies)
    }
    tryCatch({
      truth<-simulate_joint_truth(tc,b);cut<-jr_analysis_cut(truth,cfg,sc)
      observed<-setNames(lapply(c("PFS","OS"),function(e)joint_observed_at_cut(truth,e,cut$day,1L)),c("PFS","OS"))
      analyses<-lapply(names(observed),function(e)jr_endpoint_analysis(observed[[e]],cfg,e));names(analyses)<-names(observed)
      performed<-cut$reached||cfg$miss_policy=="analyze";closed<-!performed
      p<-vapply(analyses,function(a)if(isTRUE(a$valid))a$p else NA_real_,numeric(1))
      method_rows<-do.call(rbind,lapply(names(analyses),function(e){a<-analyses[[e]];m<-cfg$analyses[[e]]$method
        data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,method=e,PARAMCD=e,analysis_method=m,generated=TRUE,analysis_valid=isTRUE(a$valid),analysis_performed=performed,
          decision_valid=closed||isTRUE(a$valid),decision_reject=if(closed)FALSE else if(isTRUE(a$valid))a$p<=cfg$alpha else NA,p_value=a$p,benefit_z=a$benefit_z,estimate=a$estimate,se=a$se,lower=a$lower,upper=a$upper,
          estimate_unit=if(m=="rmst")"days" else if(m=="survival")"probability" else if(m=="cox")"logHR" else "score_only",analysis_note=a$note)}))
      policies<-do.call(rbind,lapply(jr_policies(cfg),function(m){d<-jr_policy_decisions(p,m,cfg,closed)
        data.frame(SCENARIO=sc$SCENARIO,SIMID=b,policy=m,is_primary=m==cfg$primary,formal_test_performed=performed,pfs_adjusted_p=unname(d$adjusted[1]),os_adjusted_p=unname(d$adjusted[2]),pfs_adjusted_lower=unname(d$adjusted_lower[1]),pfs_adjusted_upper=unname(d$adjusted_upper[1]),os_adjusted_lower=unname(d$adjusted_lower[2]),os_adjusted_upper=unname(d$adjusted_upper[2]),pfs_claim=d$claims["PFS"],os_claim=d$claims["OS"],any_claim=d$any,both_claim=d$both,success=d$success,false_claim=jr_false_claim(d$claims,nulls))}))
      primary<-policies[policies$is_primary,,drop=FALSE]
      row<-data.frame(SCENARIO=sc$SCENARIO,SIMID=b,replicate_seed=seed,label=sc$label,n=sc$n,generated=TRUE,DCO_DAY=cut$day,n_observed=nrow(observed$PFS),pfs_events=sum(observed$PFS$event),os_events=sum(observed$OS$event),
        rule_reached=cut$reached,formal_test_performed=performed,pfs_target_day=cut$pfs_target_day,os_target_day=cut$os_target_day,generation_note="",decision_valid=!is.na(primary$success),decision_reject=primary$success)
      out<-list(row=row,method_rows=method_rows,policy_rows=policies)
      if(keep_sample){out$truth<-truth;out$observed<-do.call(rbind,observed);out$states<-joint_state_records(truth,cut$day,1L,tc$post_clock);out$study_config<-tc}
      out
    },error=function(e)failed(conditionMessage(e)))
  })
}
jr_probability <- function(x,B) {
  valid<-!is.na(x);V<-sum(valid);K<-sum(x %in% TRUE);p<-if(V)K/V else NA_real_;w<-study_wilson(K,V)
  data.frame(requested=B,processed=length(x),not_run=B-length(x),valid=V,invalid_processed=length(x)-V,successes=K,success_conditional=p,mcse=if(V)sqrt(p*(1-p)/V) else NA_real_,wilson_lower=w[1],wilson_upper=w[2],success_lower_all=K/B,success_upper_all=(K+B-V)/B)
}
jr_aggregate <- function(state) {
  cfg<-state$config;sc<-state$scenarios;rows<-state$rows;mr<-state$method_rows;pr<-state$policy_rows
  state$overview<-do.call(rbind,lapply(sc$SCENARIO,function(id){x<-rows[rows$SCENARIO==id,,drop=FALSE];a<-sc[sc$SCENARIO==id,,drop=FALSE];cbind(a,jr_probability(if(nrow(x))ifelse(x$decision_valid,x$decision_reject,NA) else logical(),cfg$reps))}))
  tables<-list();resources<-list();correlations<-list();pairs<-list()
  for(id in sc$SCENARIO){
    a<-sc[sc$SCENARIO==id,,drop=FALSE];x<-if(nrow(rows))rows[rows$SCENARIO==id,,drop=FALSE] else rows;nulls<-jr_nulls(cfg,a)
    for(m in jr_policies(cfg)){
      d<-if(nrow(pr))pr[pr$SCENARIO==id&pr$policy==m,,drop=FALSE] else pr
      for(metric in c("success","pfs_claim","os_claim","any_claim","both_claim","false_claim"))tables[[length(tables)+1L]]<-cbind(data.frame(SCENARIO=id,label=a$label,policy=m,policy_label=jr_policy_names[[m]],is_primary=m==cfg$primary,metric=metric,null_pfs=unname(nulls["PFS"]),null_os=unname(nulls["OS"])),jr_probability(if(nrow(d))d[[metric]] else logical(),cfg$reps))
    }
    ok<-if(nrow(x))x$generated %in% TRUE else logical();q<-if(any(ok))as.numeric(quantile(x$DCO_DAY[ok],c(.05,.5,.95))) else rep(NA_real_,3)
    resources[[length(resources)+1L]]<-data.frame(SCENARIO=id,label=a$label,requested=cfg$reps,generated=sum(ok),generation_failures=sum(!ok),mean_dco_day=if(any(ok))mean(x$DCO_DAY[ok]) else NA_real_,dco_p05_day=q[1],dco_median_day=q[2],dco_p95_day=q[3],mean_observed=if(any(ok))mean(x$n_observed[ok]) else NA_real_,mean_pfs_events=if(any(ok))mean(x$pfs_events[ok]) else NA_real_,mean_os_events=if(any(ok))mean(x$os_events[ok]) else NA_real_,rule_reached_fraction=if(any(ok))mean(x$rule_reached[ok]) else NA_real_)
    d<-if(nrow(mr))mr[mr$SCENARIO==id,,drop=FALSE] else mr
    z<-if(nrow(d))merge(d[d$PARAMCD=="PFS",c("SIMID","benefit_z","analysis_valid")],d[d$PARAMCD=="OS",c("SIMID","benefit_z","analysis_valid")],by="SIMID",suffixes=c("_pfs","_os")) else data.frame()
    good<-if(nrow(z))z$analysis_valid_pfs&z$analysis_valid_os&is.finite(z$benefit_z_pfs)&is.finite(z$benefit_z_os) else logical()
    correlations[[length(correlations)+1L]]<-data.frame(SCENARIO=id,paired_valid=sum(good),benefit_z_correlation=if(sum(good)>1&&sd(z$benefit_z_pfs[good])>0&&sd(z$benefit_z_os[good])>0)cor(z$benefit_z_pfs[good],z$benefit_z_os[good]) else NA_real_,note="有效双终点轮次子集的经验相关；不用于校准拒绝界")
    if(nrow(pr))for(m in setdiff(jr_policies(cfg),cfg$primary)){
      d<-pr[pr$SCENARIO==id,,drop=FALSE];u<-d[d$policy==cfg$primary,c("SIMID","success")];v<-d[d$policy==m,c("SIMID","success")];z<-merge(u,v,by="SIMID",suffixes=c("_primary","_comparison"));good<-!is.na(z$success_primary)&!is.na(z$success_comparison);delta<-as.integer(z$success_comparison[good])-as.integer(z$success_primary[good])
      low<-function(v)ifelse(is.na(v),0,as.integer(v));high<-function(v)ifelse(is.na(v),1,as.integer(v))
      pairs[[length(pairs)+1L]]<-data.frame(SCENARIO=id,primary_policy=cfg$primary,comparison_policy=m,paired_valid=sum(good),paired_difference=if(length(delta))mean(delta) else NA_real_,paired_mcse=if(length(delta)>1)sd(delta)/sqrt(length(delta)) else NA_real_,difference_lower_all=(sum(low(z$success_comparison)-high(z$success_primary))-(cfg$reps-nrow(z)))/cfg$reps,difference_upper_all=(sum(high(z$success_comparison)-low(z$success_primary))+(cfg$reps-nrow(z)))/cfg$reps)
    }
  }
  state$policy_overview<-do.call(rbind,tables);state$resources<-do.call(rbind,resources);state$endpoint_correlations<-do.call(rbind,correlations);state$policy_pairs<-batch_bind_columns(pairs)
  state$method_overview<-do.call(rbind,lapply(sc$SCENARIO,function(id)do.call(rbind,lapply(c("PFS","OS"),function(e){d<-if(nrow(mr))mr[mr$SCENARIO==id&mr$PARAMCD==e,,drop=FALSE] else mr;cbind(data.frame(SCENARIO=id,PARAMCD=e,analysis_method=cfg$analyses[[e]]$method,note="原始名义终点拒绝；不等于多重性调整后的终点声明"),jr_probability(if(nrow(d))ifelse(d$decision_valid,d$decision_reject,NA) else logical(),cfg$reps))}))))
  state$comparisons<-data.frame();state$candidates<-data.frame();state$method_pairs<-data.frame();state$looks<-data.frame();state$gs_plans<-data.frame();state$stops<-data.frame();state
}
run_joint_research <- function(cfg,progress=function(state)NULL,should_cancel=function()FALSE,replay_keys=NULL,resume_state=NULL,continuation_operation="resume") {
  validate_joint_research(cfg);if(!is.null(resume_state)){if(!is.null(replay_keys))stop("重放与续跑不能同时使用。");batch_validate_continuation(resume_state,cfg,continuation_operation)}
  sc<-jr_scenarios(cfg);seed_rows<-function(k){d<-resume_state[[k]];if(is.data.frame(d)&&nrow(d))list(d) else list()}
  rows<-seed_rows("rows");mr<-seed_rows("method_rows");pr<-seed_rows("policy_rows");handled<-matrix(FALSE,nrow(sc),cfg$reps)
  if(length(rows))handled[cbind(rows[[1]]$SCENARIO,rows[[1]]$SIMID)]<-TRUE
  initial_completed<-sum(handled);done<-initial_completed;cancelled<-FALSE;next_publish<-Sys.time();hash<-batch_source_hash();created<-format(Sys.time(),tz="UTC",usetz=TRUE)
  snapshot<-function(status)jr_aggregate(list(config=cfg,scenarios=sc,rows=batch_bind_columns(rows),method_rows=batch_bind_columns(mr),policy_rows=batch_bind_columns(pr),completed=done,total=nrow(sc)*cfg$reps,status=status,version=batch_version,review_status="pending_v0.35_review",schema="event_pred.batch_research.v4",created_at=created,dependencies=batch_dependencies(cfg),source_hash=hash,initial_completed=initial_completed,resumed_from_run_id=resume_state$run_id %||% "",continuation_operation=if(is.null(resume_state))"new" else continuation_operation))
  for(j in seq_len(nrow(sc))){for(b in seq_len(cfg$reps)){
    if(handled[j,b])next
    if(!is.null(replay_keys)&&!any(replay_keys$SCENARIO==j&replay_keys$SIMID==b))next
    if(should_cancel()){cancelled<-TRUE;break}
    a<-jr_trial(cfg,sc[j,,drop=FALSE],b);rows[[length(rows)+1L]]<-a$row;mr[[length(mr)+1L]]<-a$method_rows;pr[[length(pr)+1L]]<-a$policy_rows;done<-done+1L;handled[j,b]<-TRUE
    if(done==initial_completed+1L||b==cfg$reps||Sys.time()>=next_publish){progress(snapshot("running"));next_publish<-Sys.time()+2}
  };if(cancelled)break}
  snapshot(if(cancelled)"cancelled" else if(!is.null(replay_keys)&&done<nrow(sc)*cfg$reps)"partial_replay" else "complete")
}
jr_initial <- function(cfg,initial=NULL)jr_aggregate(list(config=cfg,scenarios=jr_scenarios(cfg),rows=initial$rows %||% data.frame(),method_rows=initial$method_rows %||% data.frame(),policy_rows=initial$policy_rows %||% data.frame(),completed=initial$completed %||% 0L,total=nrow(jr_scenarios(cfg))*cfg$reps,status="running",version=batch_version,review_status="pending_v0.35_review",schema="event_pred.batch_research.v4",created_at=format(Sys.time(),tz="UTC",usetz=TRUE),dependencies=batch_dependencies(cfg),source_hash=batch_source_hash()))
jr_sample <- function(r,scenario,replicate){sc<-r$scenarios[r$scenarios$SCENARIO==scenario,,drop=FALSE];if(nrow(sc)!=1||!any(r$rows$SCENARIO==scenario&r$rows$SIMID==replicate&r$rows$generated))stop("请选择已生成轮次。");jr_trial(r$config,sc,replicate,TRUE)}
jr_report <- function(r) {
  tab<-function(d)capture.output(print(d,row.names=FALSE))
  c("# PFS/OS联合终点设计研究（开发稿，待复核）","",paste0("研究：",r$run_title %||% "", "；版本",r$version,"；状态",r$status,"；处理",r$completed,"/",r$total),paste0("预定主规则：",jr_policy_names[[r$config$primary]],"；主成功指标：",r$config$success_goal),
    "","## 配置","","```json",as.character(jsonlite::toJSON(batch_config_document(r$config),auto_unbox=TRUE,pretty=TRUE,digits=NA)),"```","","## 计划与主结果","","```text",tab(r$scenarios),tab(r$overview),"```","","## 各预定规则与完整请求分母","","```text",tab(r$policy_overview),"```","","## 同轮规则比较","","```text",tab(r$policy_pairs),"```","","## 共同截点资源与相关","","```text",tab(r$resources),tab(r$endpoint_correlations),"```","",
    "转移q不是边际终点HR。未调整策略仅为模拟参照；终点名义p与普通区间不等于多重性调整推断。只在所识别的充分零假设条件下标记真零；其他零假设状态未知，不把任意拒绝比例称为FWER。",
    "同轮各规则复用同一患者/两终点/共同DCO；跨情景独立生成。未知p根据单调规则的0/1补全保留已知/未知决策，生成失败保持未知。窗口关闭不拒绝与分析失败分别记录。",
    "多重性程序的理论保护要求各真零假设p值有效；有限样本生存检验、事件触发DCO和双目标/先到截点均待校准。未提供IA自适应、组序贯联合终点或同时置信区间。",
    "本轮代码、方法/数值、结果、浏览器、重放及公式显示均未验证，安排2026-10-06。")
}
