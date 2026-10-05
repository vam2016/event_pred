# Exact observed illness-death snapshots; prediction is separate from testing.
ms_is <- function(cfg)identical(cfg$research_family,"multistate_conditional")
ms_fields <- c("USUBJID","GROUP","ENTRY","TIME","STATE","EXIT","PROG")
ms_snapshot <- function(d,cut,unit="days") {
  if(!is.data.frame(d)||!all(ms_fields %in% names(d)))stop("状态CSV需USUBJID,GROUP,ENTRY,TIME,STATE,EXIT,PROG。")
  d<-d[,ms_fields];f<-time_factor(unit);d[c("ENTRY","TIME","PROG")]<-lapply(d[c("ENTRY","TIME","PROG")],function(x){if(all(is.na(x)))x<-as.numeric(x);if(!is.numeric(x))stop("时间列须为数值；无进展PROG留空。");x*f})
  if(!nrow(d)||nrow(d)>5000||anyNA(d[setdiff(ms_fields,"PROG")])||anyDuplicated(d$USUBJID)||any(!nzchar(trimws(d$USUBJID)))||any(!nzchar(trimws(d$GROUP))))stop("需1–5000名唯一受试者，除PROG外必填。")
  if(any(!is.finite(d$ENTRY)|!is.finite(d$TIME)|d$ENTRY<0|d$TIME<0|d$ENTRY+d$TIME>cut+1e-7)||any(!d$STATE %in% 0:2)||any(!d$EXIT %in% c("active","dropout","event")))stop("状态0/1/2；EXIT为active/dropout/event；观察时间不得超过IA。")
  if(any(d$STATE==2&d$TIME<=0)||any((d$STATE==2)!=(d$EXIT=="event"))||any(d$EXIT=="active"&abs(d$ENTRY+d$TIME-cut)>1e-7))stop("STATE2与死亡event须一致；active须确认到IA。")
  progressed<-is.finite(d$PROG)
  if(any(!is.na(d$PROG)&!progressed)||any(progressed&(d$PROG<=0|d$PROG>d$TIME))||any(d$STATE==0&progressed)||any(d$STATE==1&!progressed)||any(d$STATE==2&progressed&d$PROG>=d$TIME))stop("PROG是入组至精确进展的时长：STATE0留空，STATE1必填，死亡后的进展不得存在；进展与死亡并列暂不支持。")
  d$USUBJID<-as.character(d$USUBJID);d$GROUP<-as.character(d$GROUP);d
}
ms_from_adtte <- function(d,cut,group="TRTP") {
  fields<-c("USUBJID","PARAMCD","ENGINE_ENTRY_DAY","ENGINE_OBS_DAY","ENGINE_TIME_DAY","CNSR",group)
  if(!all(fields %in% names(d)))stop("双终点ADTTE需USUBJID,PARAMCD,ENGINE_ENTRY_DAY,ENGINE_OBS_DAY,ENGINE_TIME_DAY,CNSR和组别；ENGINE连续日用于精确时间。")
  d<-d[d$PARAMCD %in% c("PFS","OS"),];if(!nrow(d)||anyNA(d[,fields]))stop("双终点必填字段不能缺失。");keys<-paste(d$USUBJID,d$PARAMCD);if(anyDuplicated(keys))stop("每人每终点仅一条IA记录。")
  out<-lapply(unique(d$USUBJID),function(id){a<-d[d$USUBJID==id,];if(nrow(a)!=2||!setequal(a$PARAMCD,c("PFS","OS")))stop("每人须同时提供PFS与OS。")
    p<-a[a$PARAMCD=="PFS",];o<-a[a$PARAMCD=="OS",];num<-c(p$ENGINE_ENTRY_DAY,p$ENGINE_OBS_DAY,p$ENGINE_TIME_DAY,o$ENGINE_ENTRY_DAY,o$ENGINE_OBS_DAY,o$ENGINE_TIME_DAY)
    if(any(!is.finite(num))||p$ENGINE_ENTRY_DAY!=o$ENGINE_ENTRY_DAY||p[[group]]!=o[[group]]||any(!a$CNSR %in% 0:2)||any(abs(a$ENGINE_ENTRY_DAY+a$ENGINE_TIME_DAY-a$ENGINE_OBS_DAY)>1e-7)||p$ENGINE_OBS_DAY>o$ENGINE_OBS_DAY+1e-7)stop("双终点组别/入组/ENGINE时间不一致。")
    if(o$CNSR==0&&p$CNSR!=0)stop("死亡必须同时是PFS事件。")
    progressed<-p$CNSR==0&&(o$CNSR!=0||p$ENGINE_TIME_DAY<o$ENGINE_TIME_DAY)
    if(p$CNSR!=0&&(p$CNSR!=o$CNSR||abs(p$ENGINE_OBS_DAY-o$ENGINE_OBS_DAY)>1e-7))stop("未发生PFS事件时，两终点观察截点/退出须一致。")
    data.frame(USUBJID=id,GROUP=as.character(o[[group]]),ENTRY=o$ENGINE_ENTRY_DAY,TIME=o$ENGINE_TIME_DAY,STATE=if(o$CNSR==0)2L else if(progressed)1L else 0L,EXIT=c("event","active","dropout")[o$CNSR+1L],PROG=if(progressed)p$ENGINE_TIME_DAY else NA_real_)
  });ms_snapshot(do.call(rbind,out),cut)
}
ms_intervals <- function(d,clock="reset") {
  if(!nrow(d))return(data.frame(USUBJID=character(),GROUP=character(),transition=character(),start=numeric(),stop=numeric(),event=integer()))
  out<-lapply(seq_len(nrow(d)),function(i){x<-d[i,];p<-is.finite(x$PROG);end0<-if(p)x$PROG else x$TIME
    row<-function(tr,start,stop,event)data.frame(USUBJID=x$USUBJID,GROUP=x$GROUP,transition=tr,start=start,stop=stop,event=as.integer(event))
    z<-list(row("01",0,end0,p),row("02",0,end0,!p&&x$STATE==2))
    if(p)z[[3]]<-row("12",if(clock=="reset")0 else x$PROG,if(clock=="reset")x$TIME-x$PROG else x$TIME,x$STATE==2)
    do.call(rbind,z)
  });do.call(rbind,out)
}
ms_model_hazard <- function(m,t) switch(m$id,exponential=rep(m$params[1],length(t)),pwe=m$params[1L+vapply(t,function(x)sum(x>m$cuts),integer(1))],weibull={k<-1/m$params[2];eta<-exp(m$params[1]);k/eta*(t/eta)^(k-1)},stop("仅支持指数、Weibull、PWE转移风险。"))
ms_fit_transition <- function(x,method,cuts=numeric()) {
  x<-x[x$stop>x$start,,drop=FALSE];N<-sum(x$event);Y<-sum(x$stop-x$start)
  if(!nrow(x)||Y<=0)stop("转移无风险暴露，需指定该转移参数，或增加实际状态记录。")
  m<-list(id=method,label=method,events=N,exposure=Y,warning=character())
  if(method=="exponential") {r<-N/Y;m$params<-c(rate=r);m$loglik<-if(N)N*log(r)-r*Y else 0;m$aic<-2-2*m$loglik}
  else if(method=="pwe") {
    if(any(!is.finite(cuts)|cuts<=0)||is.unsorted(cuts,strictly=TRUE))stop("PWE切点需正递增。")
    bounds<-c(0,cuts,Inf);y<-vapply(seq_len(length(bounds)-1),function(j)sum(pmax(0,pmin(x$stop,bounds[j+1])-pmax(x$start,bounds[j]))),numeric(1));n<-tabulate(1L+vapply(x$stop[x$event==1],function(t)sum(t>cuts),integer(1)),nbins=length(y))
    if(any(y<=0))stop("PWE存在未暴露分段，需减少切点或改为参数输入；不自动估计未观察尾部。")
    m$params<-n/y;m$cuts<-cuts;m$events<-n;m$exposure<-y;m$loglik<-sum(ifelse(n>0,n*log(pmax(m$params,.Machine$double.xmin)),0)-m$params*y);m$aic<-2*length(y)-2*m$loglik
  } else if(method=="weibull") {
    if(N<2)stop("Weibull每组每转移至少2个事件。")
    ll<-function(v){k<-exp(v[2]);eta<-exp(v[1]);Hdiff<-(x$stop/eta)^k-(x$start/eta)^k;t<-x$stop[x$event==1];val<-sum(log(k)-log(eta)+(k-1)*(log(t)-log(eta)))-sum(Hdiff);if(is.finite(val))val else -1e100}
    opt<-optim(c(log(Y/N),0),function(v)-ll(v),method="L-BFGS-B",lower=c(-20,-5),upper=c(20,5));if(opt$convergence!=0||!is.finite(opt$value)||any(abs(opt$par-c(-20,-5))<1e-5)||any(abs(opt$par-c(20,5))<1e-5))stop("Weibull极大似然未获得内部有限解。")
    m$params<-c(location=opt$par[1],scale=exp(-opt$par[2]));m$loglik<--opt$value;m$aic<-4-2*m$loglik
  } else stop("未知转移模型。")
  if(N<5)m$warning<-c(m$warning,"少于5个转移事件；参数不确定性和尾部需另行评估。")
  m
}
ms_fit <- function(d,cfg) {
  ints<-ms_intervals(d,cfg$clock);groups<-names(cfg$group_weights)
  models<-setNames(lapply(groups,function(g)setNames(lapply(c("01","02","12"),function(tr){s<-cfg$transition_specs[[tr]];if(s$source=="parameters"){m<-s$model;if(g=="Treatment")m<-je_model_scale(m,s$treatment_q %||% 1);m} else ms_fit_transition(ints[ints$GROUP==g&ints$transition==tr,],s$method,s$cuts %||% numeric())}),c("01","02","12"))),groups)
  list(models=models,intervals=ints)
}
ms_model_table <- function(prepared)batch_bind_columns(lapply(names(prepared$models),function(g)batch_bind_columns(lapply(names(prepared$models[[g]]),function(tr){m<-prepared$models[[g]][[tr]];data.frame(GROUP=g,transition=tr,model=m$id,parameter=paste(signif(m$params,7),collapse=","),events=sum(m$events %||% NA_real_),exposure_day=sum(m$exposure %||% NA_real_),loglik=m$loglik %||% NA_real_,aic=m$aic %||% NA_real_,note=paste(m$warning,collapse="; "))}))))
ms_truth_from_snapshot <- function(d,models,cfg,b=1L) {
  # Only frozen observations determine conditioning; no latent records accepted.
  out<-lapply(seq_len(nrow(d)),function(i){x<-d[i,];m<-models[[x$GROUP]];age<-x$TIME;prog<-if(is.finite(x$PROG))x$PROG else Inf;death<-if(x$STATE==2)age else Inf;drop<-if(x$EXIT=="dropout")age else Inf
    if(x$EXIT=="active"){
      if(x$STATE==0){a<-sample_conditional(m[["01"]],age);z<-sample_conditional(m[["02"]],age);if(is.finite(a)&&a<z)prog<-a else death<-z}
      if(is.finite(prog)&&!is.finite(death)){start<-if(cfg$clock=="reset")max(0,age-prog) else max(age,prog);death<-max(age,prog)+sample_conditional(m[["12"]],start)-start}
      dr<-cfg$dropout_rates[[x$GROUP]];drop<-if(dr>0)age+rexp(1,dr) else Inf
    }
    data.frame(SIMID=b,USUBJID=x$USUBJID,group=x$GROUP,entry_day=x$ENTRY,progression_time_day=prog,death_time_day=death,post_progression_duration_day=if(is.finite(prog)&&is.finite(death))death-prog else NA_real_,pfs_time_day=min(prog,death),os_time_day=death,dropout_time_day=drop,progression_day=x$ENTRY+prog,death_day=x$ENTRY+death,pfs_day=x$ENTRY+min(prog,death),os_day=x$ENTRY+death,dropout_day=x$ENTRY+drop,path=if(is.finite(prog))"progression_then_death" else "direct_death",source="frozen_IA")
  })
  batch_bind_columns(out)
}
ms_continue <- function(d,models,cfg,b=1L) {
  old<-if(nrow(d))ms_truth_from_snapshot(d,models,cfg,b) else data.frame()
  if(cfg$new_n>0){entry<-cfg$cut+cumsum(rexp(cfg$new_n,cfg$enroll_rate));g<-sample(names(models),cfg$new_n,replace=TRUE,prob=cfg$group_weights[names(models)]);fresh<-data.frame(USUBJID=paste0("FUTURE_",b,"_",seq_len(cfg$new_n)),GROUP=g,ENTRY=entry,TIME=0,STATE=0L,EXIT="active",PROG=NA_real_);new<-ms_truth_from_snapshot(fresh,models,cfg,b);new$source<-"future_entry";batch_bind_columns(list(old,new))} else old
}
ms_scenarios <- function(cfg)data.frame(label="冻结IA条件预测",SCENARIO=1L)
validate_multistate <- function(cfg) {
  if(!ms_is(cfg)||!cfg$purpose %in% c("counts","target")||!cfg$clock %in% c("reset","forward")||!cfg$uncertainty %in% c("plugin","bootstrap"))stop("请选择多状态条件预测需求、时钟及参数处理。")
  d<-ms_snapshot(cfg$data,cfg$cut);g<-names(cfg$group_weights)
  if(length(g)<1||length(g)>2||!all(d$GROUP %in% g)||any(!is.finite(cfg$group_weights)|cfg$group_weights<=0)||!setequal(names(cfg$dropout_rates),g)||any(!is.finite(cfg$dropout_rates)|cfg$dropout_rates<0))stop("提供1–2组正权重及每组非负脱落率，组名与数据一致。")
  if(cfg$new_n<0||cfg$new_n!=floor(cfg$new_n)||cfg$new_n+nrow(d)>5000||(cfg$new_n>0&&(!is.finite(cfg$enroll_rate)||cfg$enroll_rate<=0)))stop("未来人数为非负整数，合计≤5000；未来入组率须正。")
  if(!is.finite(cfg$max_day)||cfg$max_day<=cfg$cut||cfg$max_day>3650||cfg$reps<20||cfg$reps>10000||cfg$reps!=floor(cfg$reps)||!is.finite(cfg$seed)||cfg$seed<0||cfg$seed>.Machine$integer.max||cfg$seed!=floor(cfg$seed))stop("最大研究时间大于IA且≤3650日；B20–10000整数，种子为非负整数。")
  if(cfg$purpose=="counts"&&(length(cfg$horizons)<1||length(cfg$horizons)>20||any(!is.finite(cfg$horizons)|cfg$horizons<=cfg$cut|cfg$horizons>cfg$max_day)||is.unsorted(cfg$horizons,strictly=TRUE)))stop("未来研究截点1–20个，正递增且在IA之后、窗口以内。")
  if(cfg$purpose=="target"&&(!cfg$target_endpoint %in% c("PFS","OS")||cfg$target<1||cfg$target!=floor(cfg$target)))stop("选择单一达标终点及正整数事件目标。")
  if(!setequal(names(cfg$transition_specs),c("01","02","12")))stop("需全部三转移定义。")
  for(s in cfg$transition_specs){if(!s$source %in% c("fit","parameters"))stop("转移输入来源错误。");if(s$source=="fit"&&!s$method %in% c("exponential","weibull","pwe"))stop("拟合支持指数、Weibull或PWE。");if(s$source=="parameters"&&(!ms_valid_model(s$model)||!is.finite(s$treatment_q %||% 1)||(s$treatment_q %||% 1)<=0))stop("需有限转移参数。")}
  if((nrow(d)+cfg$new_n)*cfg$reps>3e7)stop("最多3000万患者×轮次。")
  invisible(TRUE)
}
ms_prepare <- function(state){if(is.null(state$prepared))state$prepared<-ms_fit(state$config$data,state$config);state}
ms_trial <- function(cfg,sc,b,prepared,keep_sample=FALSE)batch_with_rng(batch_replicate_seed(cfg$seed,sc$SCENARIO,b),{
  tryCatch({models<-prepared$models
    if(cfg$uncertainty=="bootstrap"){boot<-batch_bind_columns(lapply(split(cfg$data,cfg$data$GROUP),function(x)x[sample(seq_len(nrow(x)),replace=TRUE),,drop=FALSE]));boot$USUBJID<-paste0("BOOT_",seq_len(nrow(boot)));models<-ms_fit(boot,cfg)$models}
    truth<-ms_continue(cfg$data,models,cfg,b)
    known<-sum(cfg$data$STATE==2|is.finite(cfg$data$PROG));knownos<-sum(cfg$data$STATE==2)
    dates<-setNames(lapply(c("PFS","OS"),function(e){t<-joint_endpoint_truth(truth,e);sort(t$event_day[is.finite(t$event_day)&t$event_time_day<=t$dropout_time_day])}),c("PFS","OS"))
    reached<-TRUE;day<-cfg$max_day
    if(cfg$purpose=="target"){v<-dates[[cfg$target_endpoint]];day<-if(length(v)>=cfg$target)v[cfg$target] else Inf;reached<-day<=cfg$max_day;day<-if(reached)max(cfg$cut,day) else cfg$max_day}
    h<-if(cfg$purpose=="counts")cfg$horizons else day
    draws<-batch_bind_columns(lapply(h,function(t)data.frame(SCENARIO=1L,SIMID=b,DCO_DAY=t,pfs_events=sum(dates$PFS<=t),os_events=sum(dates$OS<=t),pfs_new_events=sum(dates$PFS>cfg$cut&dates$PFS<=t),os_new_events=sum(dates$OS>cfg$cut&dates$OS<=t))))
    row<-data.frame(SCENARIO=1L,SIMID=b,replicate_seed=batch_replicate_seed(cfg$seed,1L,b),generated=TRUE,decision_valid=TRUE,decision_reject=if(cfg$purpose=="target")reached else NA,closing_day=day,known_pfs=known,known_os=knownos,n_generated=nrow(truth),note="")
    mr<-data.frame(SCENARIO=1L,SIMID=b,method="prediction",generated=TRUE,decision_valid=cfg$purpose=="target",decision_reject=if(cfg$purpose=="target")reached else NA,target_reached=if(cfg$purpose=="target")reached else NA,closing_day=day,n_observed=sum(truth$entry_day<=day),observed_events=if(cfg$purpose=="target")sum(dates[[cfg$target_endpoint]]<=day) else NA_real_,stop_reason=if(cfg$purpose=="counts")"count_window" else if(reached)"target" else "window")
    mr$final_day<-mr$closing_day;mr$events<-mr$observed_events;mr$action<-mr$stop_reason;mr$wait_day<-day-cfg$cut;mr$future_analysis_count<-0L
    out<-list(row=row,method_rows=mr,draw_rows=draws,looks=data.frame())
    if(keep_sample){out$truth<-truth;out$observed<-batch_bind_columns(lapply(c("PFS","OS"),function(e)joint_observed_at_cut(truth,e,day,1L)));out$intervals<-prepared$intervals;out$models<-models;out$study_config<-list(origin=cfg$origin,display_unit="days")}
    out
  },error=function(e){list(row=data.frame(SCENARIO=1L,SIMID=b,replicate_seed=batch_replicate_seed(cfg$seed,1L,b),generated=FALSE,decision_valid=FALSE,decision_reject=NA,note=conditionMessage(e)),method_rows=data.frame(SCENARIO=1L,SIMID=b,method="prediction",generated=FALSE,decision_valid=FALSE,decision_reject=NA,target_reached=NA),draw_rows=data.frame(),looks=data.frame())})
})
ms_aggregate <- function(state){state<-research_aggregate(state,"prediction",c(prediction="多状态条件预测"));state$target_overview<-ms_target_table(state);state$model_overview<-if(is.null(state$prepared))data.frame() else ms_model_table(state$prepared);d<-state$draw_rows;cfg<-state$config
  state$count_overview<-if(nrow(d))batch_bind_columns(lapply(split(d,d$DCO_DAY),function(x)batch_bind_columns(lapply(c("pfs_events","os_events","pfs_new_events","os_new_events"),function(k){v<-x[[k]];q<-quantile(v,c(.025,.5,.975));data.frame(DCO_DAY=x$DCO_DAY[1],metric=k,requested=cfg$reps,valid=length(v),mean=mean(v),lower=q[1],median=q[2],upper=q[3],note="分位数基于成功生成轮次；未知/失败不补为零。") })))) else data.frame()
  state
}
run_multistate <- function(cfg,progress=function(state)NULL,should_cancel=function()FALSE,replay_keys=NULL,resume_state=NULL,continuation_operation="resume"){validate_multistate(cfg);research_family_run(cfg,ms_scenarios(cfg),ms_trial,ms_aggregate,progress,should_cancel,replay_keys,resume_state,continuation_operation,ms_prepare)}
ms_initial <- function(cfg,initial=NULL)research_initial(cfg,ms_scenarios(cfg),ms_aggregate,initial)
ms_sample <- function(r,scenario,replicate){if(!any(r$rows$SIMID==replicate&r$rows$generated))stop("请选择已生成轮次。");ms_trial(r$config,r$scenarios[scenario,,drop=FALSE],replicate,r$prepared,TRUE)}
ms_report <- function(r)c("# 实际多状态条件预测开发稿",paste0("版本",r$version,"；处理",r$completed,"/",r$total),"冻结精确IA状态；三转移各组独立拟合或指定；12使用reset或forward时钟。患者Bootstrap重拟合只用于预测，失败保留完整分母。",capture.output(print(r$model_overview,row.names=FALSE)),capture.output(print(r$count_overview,row.names=FALSE)),capture.output(print(r$target_overview,row.names=FALSE)),capture.output(print(r$method_overview,row.names=FALSE)),"预测区间不等于参数置信区间。已进展PFS不重新抽样；死亡与永久退出终止。此拟合模型不自动作为联合正式检验的已知零假设。未开展验证。")
# Continuous days retain exact paths even when calendar AVAL is integer.
ms_adtte <- function(observed,cfg){a<-joint_adtte(observed,cfg);if(!nrow(a))return(a);ix<-c(which(observed$PARAMCD=="PFS"),which(observed$PARAMCD=="OS"));o<-observed[ix,,drop=FALSE];a$ENGINE_ENTRY_DAY<-o$entry_day;a$ENGINE_OBS_DAY<-o$obs_day;a$ENGINE_TIME_DAY<-o$time_day;if("design" %in% names(o))a$METHOD<-o$design;a}
ms_valid_model <- function(m){if(!is.list(m)||!m$id %in% c("exponential","weibull","pwe")||!is.numeric(m$params)||any(!is.finite(m$params)))return(FALSE);switch(m$id,exponential=length(m$params)==1&&m$params[1]>=0,weibull=length(m$params)==2&&m$params[2]>0,pwe=length(m$params)==length(m$cuts)+1&&all(m$params>=0)&&all(is.finite(m$cuts)&m$cuts>0)&&!is.unsorted(m$cuts,strictly=TRUE))}
ms_validate_saved <- function(state,cfg){if(is.null(state$prepared))return(invisible(TRUE));p<-state$prepared;if(!setequal(names(p$models),names(cfg$group_weights)))stop("保存的多状态模型组别不符。");for(g in names(p$models)){if(!setequal(names(p$models[[g]]),c("01","02","12"))||any(!vapply(p$models[[g]],ms_valid_model,logical(1))))stop("保存的多状态转移模型不完整。")};if(!identical(p$intervals,ms_intervals(cfg$data,cfg$clock)))stop("保存的风险区间与冻结IA不符。");invisible(TRUE)}
je_validate_saved <- function(state,cfg){d<-state$looks;if(!is.data.frame(d)||!nrow(d))return(invisible(TRUE));cols<-c("SCENARIO","SIMID","method","look","PARAMCD","DCO_DAY","log_e","log_running_e");if(!all(cols %in% names(d))||anyNA(d[,cols])||any(!d$method %in% je_methods(cfg))||any(!d$PARAMCD %in% c("PFS","OS"))||anyDuplicated(d[,c("SCENARIO","SIMID","method","look","PARAMCD")]))stop("联合序贯保存路径键不完整或重复。");for(x in split(d,paste(d$SCENARIO,d$SIMID,d$method,d$PARAMCD))){x<-x[order(x$look),];if(x$look[1]!=0||any(diff(x$DCO_DAY)<0)||any(diff(x$log_running_e)< -1e-8)||any(x$log_running_e+1e-8<x$log_e))stop("联合序贯保存路径次序/运行证据不一致。")};invisible(TRUE)}
ms_target_table <- function(state){cfg<-state$config;if(cfg$purpose!="target")return(data.frame());d<-state$method_rows;v<-if(nrow(d))d$target_reached else logical();ok<-v %in% TRUE;times<-if(any(ok))d$final_day[ok] else numeric();q<-if(length(times))as.numeric(quantile(times,c(.025,.5,.975))) else rep(NA_real_,3);cbind(data.frame(endpoint=cfg$target_endpoint,target=cfg$target,finite_target_dates=length(times),target_lower_day=q[1],target_median_day=q[2],target_upper_day=q[3],note="日期分位仅对窗口内达标轮次；概率保留完整请求分母，窗口未达不当作达标日期。"),jr_probability(v,cfg$reps))}
