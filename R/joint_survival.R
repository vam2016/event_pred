# Illness-death generation: 0=progression-free alive, 1=progressed alive, 2=dead.
# Endpoint analyses receive observed data only; latent clocks are exported separately.
joint_transition_names <- c(`01`="未进展 → 进展",`02`="未进展 → 死亡",`12`="进展 → 死亡")
joint_defaults <- function(unit="months",index=1) {
  d<-simulation_defaults(unit,index);d$design_model<-"exponential"
  d$median<-(if(index==2)60 else 12)*30.4375/time_factor(unit)
  d$exp_rate<-log(2)/d$median;d$eta<-d$median/log(2)^(1/d$shape)
  d
}
joint_model_values <- function(input,pre,unit,index) {
  d<-joint_defaults(unit,index)
  setNames(lapply(names(d),function(k)input[[paste0(pre,k)]] %||% d[[k]]),names(d))
}
joint_config_input <- function(input) {
  unit<-input$js_unit %||% "months";f<-time_factor(unit);keys<-names(joint_transition_names)
  transitions<-setNames(lapply(seq_along(keys),function(j) {
    v<-joint_model_values(input,paste0("js",keys[j],"_"),unit,j)
    model<-parameter_input_model(v$design_model,v,unit)
    active<-switch(v$design_model,exponential=c("design_model","exp_input",if(v$exp_input=="rate")"exp_rate" else "median"),
      weibull=c("design_model","weibull_input","shape",if(v$weibull_input=="eta")"eta" else "median"),
      pwe=c("design_model","parameter_cuts","parameter_rates"),stop("转移模型限指数、Weibull或PWE。"))
    list(model=model,input_basis=v[active])
  }),keys)
  mode<-input$js_enroll_mode;cut<-input$js_cut_mode;ng<-suppressWarnings(as.integer(input$js_groups))
  if(length(ng)!=1||is.na(ng)||!ng %in% c(1L,2L))stop("组数请选择1或2。")
  p<-if(ng==2)input$js_allocation else 0
  drops<-c(Control=input$js_drop_control/f,if(ng==2)c(Treatment=input$js_drop_treatment/f))
  groups<-list(list(name="Control",weight=if(ng==2)1-p else 1,dropout_rate=drops[1],multipliers=setNames(rep(1,3),keys)))
  if(ng==2)groups[[2]]<-list(name="Treatment",weight=p,dropout_rate=drops[2],multipliers=setNames(vapply(keys,function(k)input[[paste0("js_q",k)]],numeric(1)),keys))
  cuts<-if(cut=="fixed")parse_unit_numbers(input$js_cuts)*f else numeric()
  cfg<-list(n=input$js_n,reps=input$js_reps,seed=input$js_seed,origin=as.character(input$js_origin),
    display_unit=unit,engine_unit="days",rng_kind=c("Mersenne-Twister","Inversion","Rejection"),transitions=transitions,groups=groups,
    post_clock=input$js_clock,enroll_mode=mode,
    enroll_rate=if(mode=="constant")input$js_enroll_rate/f else NULL,
    enroll_cuts=if(mode=="piecewise")parse_unit_numbers(input$js_enroll_cuts)*f else numeric(),
    enroll_rates=if(mode=="piecewise")parse_unit_numbers(input$js_enroll_rates)/f else numeric(),
    entry_days=if(mode=="schedule")parse_unit_numbers(input$js_schedule)*f else numeric(),
    cut_mode=cut,cuts=cuts,target_endpoint=if(cut=="target")input$js_target_endpoint else NULL,
    target=if(cut=="target")input$js_target else NULL,max_day=if(cut=="target")input$js_max*f else max(cuts),
    fixed_times=parse_unit_numbers(input$js_fixed)*f,treatment_fraction=if(ng==2)p else NULL)
  validate_joint_config(cfg);cfg
}
validate_joint_config <- function(cfg) {
  integer_scalar<-function(x,lo,hi)is.numeric(x)&&length(x)==1&&is.finite(x)&&x==floor(x)&&x>=lo&&x<=hi
  scalar<-function(x,lo,hi)is.numeric(x)&&length(x)==1&&is.finite(x)&&x>=lo&&x<=hi
  if(!integer_scalar(cfg$n,1,5000)||!integer_scalar(cfg$reps,1,200)||!integer_scalar(cfg$seed,0,.Machine$integer.max))stop("N需1–5000、重复数1–200、种子0–2147483647，均为整数。")
  if(!is.null(cfg$rng_kind)&&!identical(cfg$rng_kind,c("Mersenne-Twister","Inversion","Rejection")))stop("本模块固定使用Mersenne-Twister/Inversion/Rejection随机数算法。")
  if(!cfg$post_clock %in% c("reset","forward"))stop("进展后死亡时钟请选择进展后时间或自入组时间。")
  if(!identical(sort(names(cfg$transitions)),c("01","02","12")))stop("必须指定01、02、12三个转移模型。")
  for(tr in cfg$transitions) {
    m<-tr$model;p<-m$params
    if(!m$id %in% c("exponential","weibull","pwe")||!is.numeric(p)||any(!is.finite(p)))stop("转移模型限有限参数的指数、Weibull或PWE。")
    if(m$id=="exponential"&&(length(p)!=1||p[1]<=0))stop("指数转移率需大于0。")
    if(m$id=="weibull"&&(length(p)!=2||p[2]<=0||!is.finite(exp(p[1]))||exp(p[1])<=0))stop("Weibull转移尺度及形状无效。")
    if(m$id=="pwe")parameter_model("pwe",list(rates=p),m$cuts)
  }
  if(length(cfg$groups)<1||length(cfg$groups)>2)stop("联合模拟当前支持单组或两组。")
  if(length(cfg$groups)==2&&!scalar(cfg$treatment_fraction,.01,.99))stop("Treatment分配概率需0.01–0.99。")
  labs<-vapply(cfg$groups,`[[`,character(1),"name")
  if(anyNA(labs)||any(!nzchar(trimws(labs)))||anyDuplicated(labs))stop("组名需非空且唯一。")
  for(g in cfg$groups) {
    if(!scalar(g$weight,.Machine$double.eps,1)||!scalar(g$dropout_rate,0,1e6)||!identical(sort(names(g$multipliers)),c("01","02","12"))||any(!is.finite(g$multipliers)|g$multipliers<.01|g$multipliers>100))stop("分配权重需正，退出率非负，三个转移风险倍数需0.01–100。")
  }
  if(length(cfg$groups)==2) {
    w<-vapply(cfg$groups,`[[`,numeric(1),"weight")
    if(abs(w[2]/sum(w)-cfg$treatment_fraction)>1e-12)stop("第二组权重与保存的Treatment分配概率不一致。")
  }
  if(!cfg$enroll_mode %in% c("constant","piecewise","schedule"))stop("请选择入组方式。")
  if(cfg$enroll_mode=="constant"&&!scalar(cfg$enroll_rate,.Machine$double.eps,1e6))stop("总体入组率需为正。")
  if(cfg$enroll_mode=="piecewise")parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts)
  if(cfg$enroll_mode=="schedule"&&(!is.numeric(cfg$entry_days)||length(cfg$entry_days)!=cfg$n||any(!is.finite(cfg$entry_days)|cfg$entry_days<0)||is.unsorted(cfg$entry_days)))stop("固定日程需N个非负、非递减研究时间。")
  if(!scalar(cfg$max_day,.Machine$double.eps,3650)||!cfg$cut_mode %in% c("fixed","target"))stop("研究窗口需在(0,3650]日，并选择固定DCO或事件驱动。")
  vec<-function(x)is.numeric(x)&&length(x)>=1&&length(x)<=20&&all(is.finite(x)&x>0&x<=3650)&&!is.unsorted(x,strictly=TRUE)
  if(cfg$cut_mode=="fixed"&&(!vec(cfg$cuts)||max(cfg$cuts)>cfg$max_day))stop("固定DCO需1–20个正递增研究时间，最长3650日。")
  if(cfg$cut_mode=="target"&&(!cfg$target_endpoint %in% c("PFS","OS")||!integer_scalar(cfg$target,1,100000)))stop("事件驱动需预定PFS或OS，D*为1–100000整数。")
  if(!vec(cfg$fixed_times))stop("固定随访时点需1–20个正递增值，最长3650日。")
  if(cfg$n*cfg$reps*(if(cfg$cut_mode=="fixed")length(cfg$cuts) else 1)>200000)stop("N×重复数×截点数最多20万，请减少规模。")
  time_factor(cfg$display_unit)
  if(length(cfg$origin)!=1||is.na(as.Date(cfg$origin)))stop("研究起点日期无效。")
  invisible(TRUE)
}
joint_entry_times <- function(cfg) switch(cfg$enroll_mode,
  constant=cumsum(rexp(cfg$n,cfg$enroll_rate)),
  piecewise=inverse_cumhaz(parameter_model("pwe",list(rates=cfg$enroll_rates),cfg$enroll_cuts),cumsum(rexp(cfg$n))),
  schedule=cfg$entry_days)
simulate_joint_truth <- function(cfg,b) {
  entry<-joint_entry_times(cfg)
  arm<-sample.int(length(cfg$groups),cfg$n,replace=TRUE,prob=vapply(cfg$groups,`[[`,numeric(1),"weight"))
  prog_clock<-death0_clock<-progression<-death<-dropout<-post_duration<-rep(Inf,cfg$n)
  path<-rep("no_transition",cfg$n)
  for(j in seq_along(cfg$groups)) {
    ix<-which(arm==j);if(!length(ix))next
    g<-cfg$groups[[j]];q<-g$multipliers
    a<-sample_conditional(cfg$transitions[["01"]]$model,rep(0,length(ix)),q[["01"]])
    d<-sample_conditional(cfg$transitions[["02"]]$model,rep(0,length(ix)),q[["02"]])
    prog_clock[ix]<-a;death0_clock[ix]<-d
    # A direct death wins a numerical tie. Infinity/Infinity leaves state 0 forever.
    direct<-is.finite(d)&d<=a
    death[ix[direct]]<-d[direct];path[ix[direct]]<-"direct_death"
    via<-is.finite(a)&a<d
    if(any(via)) {
      id<-ix[via];s<-a[via];progression[id]<-s
      age<-if(cfg$post_clock=="reset")rep(0,length(s)) else s
      v<-sample_conditional(cfg$transitions[["12"]]$model,age,q[["12"]])
      duration<-if(cfg$post_clock=="reset")v else v-s
      if(anyNA(duration)||any(duration<0))stop("进展后死亡时钟返回无效剩余时间。")
      post_duration[id]<-duration;death[id]<-s+duration;path[id]<-"progression_route"
    }
    if(g$dropout_rate>0)dropout[ix]<-rexp(length(ix),g$dropout_rate)
  }
  pfs<-pmin(progression,death)
  data.frame(SIMID=b,USUBJID=sprintf("JOINT%03d-%05d",b,seq_len(cfg$n)),
    group=vapply(cfg$groups,`[[`,character(1),"name")[arm],entry_day=entry,
    latent_progression_clock_day=prog_clock,latent_direct_death_clock_day=death0_clock,
    progression_time_day=progression,death_time_day=death,post_progression_duration_day=post_duration,
    pfs_time_day=pfs,os_time_day=death,dropout_time_day=dropout,
    progression_day=entry+progression,death_day=entry+death,pfs_day=entry+pfs,os_day=entry+death,
    dropout_day=entry+dropout,path=path,stringsAsFactors=FALSE)
}
joint_endpoint_truth <- function(truth,endpoint) {
  data.frame(SIMID=truth$SIMID,USUBJID=truth$USUBJID,group=truth$group,entry_day=truth$entry_day,
    event_time_day=truth[[if(endpoint=="PFS")"pfs_time_day" else "os_time_day"]],
    dropout_time_day=truth$dropout_time_day,event_day=truth[[if(endpoint=="PFS")"pfs_day" else "os_day"]],dropout_day=truth$dropout_day)
}
joint_observed_at_cut <- function(truth,endpoint,dco,k) {
  o<-survival_at_cut(joint_endpoint_truth(truth,endpoint),dco,k)
  ix<-match(o$USUBJID,truth$USUBJID);cause<-rep("censored",nrow(o));event<-o$event==1
  if(endpoint=="PFS")cause[event]<-ifelse(truth$progression_time_day[ix[event]]<truth$death_time_day[ix[event]],"progression","death") else cause[event]<-"death"
  o$PARAMCD<-rep(endpoint,nrow(o));o$event_cause<-cause;o
}
joint_state_records <- function(truth,dco,k,clock) {
  x<-truth[is.finite(truth$entry_day)&truth$entry_day<dco,,drop=FALSE]
  if(!nrow(x))return(list(states=data.frame(),intervals=data.frame()))
  admin<-dco-x$entry_day
  dead<-is.finite(x$death_time_day)&x$death_time_day<=x$dropout_time_day&x$death_day<=dco
  withdrawn<-!dead&is.finite(x$dropout_time_day)&x$dropout_day<=dco
  limit<-pmin(admin,x$dropout_time_day,x$death_time_day)
  # Use calendar membership so the event driving exact DCO is retained.
  progressed<-is.finite(x$progression_time_day)&x$progression_time_day<=x$dropout_time_day&x$progression_day<=dco
  last<-ifelse(dead,2L,ifelse(progressed,1L,0L))
  states<-data.frame(SIMID=x$SIMID,CUTID=k,DCO_DAY=dco,USUBJID=x$USUBJID,group=x$group,
    entry_day=x$entry_day,last_observed_day=x$entry_day+limit,last_known_state=last,
    state_at_dco=ifelse(dead,"dead",ifelse(withdrawn,"unknown_after_dropout",ifelse(progressed,"progressed_alive","progression_free_alive"))))
  intervals<-lapply(seq_len(nrow(x)),function(i) {
    make<-function(from,to,start,end,event) data.frame(SIMID=x$SIMID[i],CUTID=k,DCO_DAY=dco,USUBJID=x$USUBJID[i],group=x$group[i],
      from_state=from,to_state=to,start_age_day=start,stop_age_day=end,start_study_day=x$entry_day[i]+start,
      stop_study_day=x$entry_day[i]+end,event=event,
      transition=if(event==1)paste0(from,"-",to) else paste0(from,"-censored"),
      transition_clock_start_day=if(from==1&&clock=="reset")0 else start,
      transition_clock_stop_day=if(from==1&&clock=="reset")end-start else end)
    if(progressed[i]) {
      a<-make(0L,1L,0,x$progression_time_day[i],1L)
      end<-if(dead[i])x$death_time_day[i] else max(limit[i],x$progression_time_day[i])
      rbind(a,make(1L,if(dead[i])2L else NA_integer_,x$progression_time_day[i],end,as.integer(dead[i])))
    } else make(0L,if(dead[i])2L else NA_integer_,0,if(dead[i])x$death_time_day[i] else limit[i],as.integer(dead[i]))
  })
  list(states=states,intervals=do.call(rbind,intervals))
}
joint_trial <- function(cfg,b) {
  truth<-simulate_joint_truth(cfg,b)
  target_day<-NA_real_;hit<-NA
  if(cfg$cut_mode=="target") {
    et<-joint_endpoint_truth(truth,cfg$target_endpoint)
    times<-sort(et$event_day[is.finite(et$event_day)&et$event_time_day<=et$dropout_time_day&et$event_day<=cfg$max_day])
    hit<-length(times)>=cfg$target;target_day<-if(hit)times[cfg$target] else NA_real_
  }
  cuts<-if(cfg$cut_mode=="fixed")cfg$cuts else if(hit)target_day else cfg$max_day
  bins<-setNames(lapply(c("observed","summary","fixed","curves","states","intervals","cuts"),function(k)list()),c("observed","summary","fixed","curves","states","intervals","cuts"))
  for(k in seq_along(cuts)) {
    dco<-cuts[k];counts<-setNames(numeric(2),c("PFS","OS"))
    for(endpoint in c("PFS","OS")) {
      o<-joint_observed_at_cut(truth,endpoint,dco,k);counts[endpoint]<-sum(o$event)
      a<-analyze_simulated_survival(o,cfg,b,k,dco)
      bins$observed[[length(bins$observed)+1L]]<-o
      for(key in c("summary","fixed","curves"))if(!is.null(a[[key]])&&nrow(a[[key]]))bins[[key]][[length(bins[[key]])+1L]]<-cbind(PARAMCD=endpoint,a[[key]])
    }
    state<-joint_state_records(truth,dco,k,cfg$post_clock)
    bins$states[[k]]<-state$states;bins$intervals[[k]]<-state$intervals
    bins$cuts[[k]]<-data.frame(SIMID=b,CUTID=k,DCO_DAY=dco,n=sum(is.finite(truth$entry_day)&truth$entry_day<dco),
      pfs_events=counts[["PFS"]],os_events=counts[["OS"]],target_endpoint=cfg$target_endpoint %||% NA_character_,
      target=cfg$target %||% NA_real_,target_reached=hit,target_day=target_day,
      analysis_status=if(cfg$cut_mode=="target"&&!hit)"window_unreached" else "analyzed")
  }
  bind<-function(x){d<-do.call(rbind,x);if(is.null(d))data.frame() else {rownames(d)<-NULL;d}}
  c(list(truth=truth),lapply(bins,bind))
}
run_joint_simulation <- function(cfg,progress=function(value,detail)NULL) {
  validate_joint_config(cfg)
  cfg$rng_kind<-cfg$rng_kind %||% c("Mersenne-Twister","Inversion","Rejection")
  old_kind<-RNGkind()
  existed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(existed)old<-get(".Random.seed",envir=.GlobalEnv)
  on.exit({do.call(RNGkind,as.list(old_kind));if(existed)assign(".Random.seed",old,envir=.GlobalEnv) else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)},add=TRUE)
  do.call(RNGkind,as.list(cfg$rng_kind))
  keys<-c("truth","observed","summary","fixed","curves","states","intervals","cuts")
  bins<-setNames(lapply(keys,function(k)list()),keys);status<-list()
  for(b in seq_len(cfg$reps)) {
    seed<-as.integer((as.double(cfg$seed)+b*100003)%%2147483646+1);set.seed(seed)
    x<-tryCatch(joint_trial(cfg,b),error=function(e)list(error=conditionMessage(e)))
    ok<-is.null(x$error)
    status[[b]]<-data.frame(SIMID=b,replicate_seed=seed,completed=ok,note=if(ok)"" else x$error)
    if(ok)for(key in keys)bins[[key]][[length(bins[[key]])+1L]]<-x[[key]]
    progress(b/cfg$reps,paste0("试验",b,"/",cfg$reps))
  }
  bind<-function(x){d<-do.call(rbind,x);if(is.null(d))data.frame() else {rownames(d)<-NULL;d}}
  c(list(config=cfg,trial_status=bind(status),created_at=format(Sys.time(),tz="UTC",usetz=TRUE),
    version="0.30.0",status="pending_review",schema="event_pred.joint_survival.v1"),lapply(bins,bind))
}
joint_overview <- function(r) {
  s<-r$summary;if(!nrow(s))return(data.frame())
  groups<-split(seq_len(nrow(s)),interaction(s$PARAMCD,s$CUTID,s$scope,s$group,drop=TRUE))
  do.call(rbind,lapply(groups,function(ix) {
    x<-s[ix,,drop=FALSE];ok<-is.finite(x$median_day);n<-nrow(x)
    q<-if(any(ok))as.numeric(quantile(x$median_day[ok],c(.025,.5,.975))) else rep(NA_real_,3)
    data.frame(PARAMCD=x$PARAMCD[1],CUTID=x$CUTID[1],scope=x$scope[1],group=x$group[1],requested=r$config$reps,
      completed=n,failed=r$config$reps-n,mean_n=mean(x$n),mean_events=mean(x$events),
      mean_events_lower_all=sum(x$events)/r$config$reps,
      mean_events_upper_all=(sum(x$events)+(r$config$reps-n)*r$config$n)/r$config$reps,
      median_estimable=sum(ok),median_estimable_fraction=sum(ok)/n,
      conditional_lower_day=q[1],conditional_median_day=q[2],conditional_upper_day=q[3])
  }))
}
joint_adtte <- function(observed,cfg) {
  if(!nrow(observed))return(data.frame())
  d<-do.call(rbind,lapply(c("PFS","OS"),function(endpoint) {
    z<-cfg;z$paramcd<-endpoint;o<-observed[observed$PARAMCD==endpoint,,drop=FALSE]
    a<-simulation_adtte(o,z);a$EVNTDESC<-ifelse(o$event==1,toupper(o$event_cause),ifelse(o$status=="dropout","PERMANENT FOLLOW-UP EXIT","CUTOFF"));a
  }));rownames(d)<-NULL;d
}
joint_report <- function(r) {
  table<-function(d)capture.output(print(d,row.names=FALSE))
  c("# PFS/OS 联合生存模拟（开发稿，待复核）","",paste0("保存时间：",r$created_at,"；版本：",r$version),
    paste0("请求试验数：",r$config$reps,"；完成：",sum(r$trial_status$completed),"；失败：",sum(!r$trial_status$completed)),
    "","## 模型与计时","",
    "状态0未进展存活，状态1进展后存活，状态2死亡；0→1、0→2竞争，进展后取消原直接死亡时钟并启用1→2。PFS=min(进展,死亡)，OS=死亡。",
    paste0("进展后死亡时钟：",if(r$config$post_clock=="reset")"进展后时间（semi-Markov）。" else "自入组时间（Markov、clock-forward）。"),
    "三转移的中位时间对应单转移潜在时钟，不是边际PFS/OS中位数；转移HR不等于边际终点HR。退出为每组独立共同随访退出，两终点共用同一时间。",
    "","## 已提交配置","","```json",jsonlite::toJSON(r$config,auto_unbox=TRUE,pretty=TRUE,digits=NA),"```",
    "","## 联合结果汇总","","```text",table(joint_overview(r)),"```",
    "","## 每轮状态","","```text",table(r$trial_status),"```",
    "","KM区间为单次估计区间，不是重复试验预测区间。中位数分位数只以可估计的完成试验为分母；NR、空组、窗口未达目标和生成/分析失败分别保留。",
    "退出后的当前状态标为未知，不用潜在未来事件填充观察记录。真值表另行导出，包含未观察时钟及结局；观察数据、状态表和转移区间没有未来临床字段。",
    "本功能只做设计阶段联合生成与描述，不计算联合检验功效、终点多重性或确认性推断。尚未接实际多状态资料拟合或IA后的状态条件续推。",
    "2026-10-05仅开发，代码、模型/数值、结果、界面、下载复现及本章公式显示均待2026-10-06复核。")
}
joint_script <- function(r) {
  dump<-paste(capture.output(dput(r$config,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  c("# event_pred v0.30.0 joint survival development draft; replay not yet verified.",
    'if (!exists("%||%", mode="function")) `%||%` <- function(x,y) if (is.null(x)) y else x',
    'for (f in c("units","models","forecast","inputs","simulation","joint_survival")) source(paste0("R/",f,".R"))',
    paste0('if (as.character(packageVersion("survival")) != "',as.character(utils::packageVersion("survival")),'") stop("survival version differs")'),
    paste0("cfg <- ",dump),'result <- run_joint_simulation(cfg)',
    'saveRDS(result,"joint_survival_replayed.rds")',
    'write.csv(joint_overview(result),"joint_overview_replayed.csv",row.names=FALSE)',
    'write.csv(result$summary,"joint_summary_replayed.csv",row.names=FALSE)',
    'write.csv(result$cuts,"joint_cuts_replayed.csv",row.names=FALSE)',
    'write.csv(result$trial_status,"joint_trial_status_replayed.csv",row.names=FALSE)')
}
