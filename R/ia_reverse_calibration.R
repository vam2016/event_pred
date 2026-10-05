# Conditional expected observed events; fixed IA observations are never refitted here.
ic_snapshot <- function(d,cut,unit="days") {
  keys<-c("USUBJID","ENTRY","AGE","STATUS")
  if(!is.data.frame(d)||!identical(sort(names(d)),sort(keys))||!nrow(d)||nrow(d)>5000)stop("IA CSV严格四列USUBJID,ENTRY,AGE,STATUS，1–5000名患者。")
  d<-d[,keys];f<-time_factor(unit)
  for(k in c("ENTRY","AGE")){if(!is.numeric(d[[k]]))stop("ENTRY/AGE须数值。");d[[k]]<-d[[k]]*f}
  d$USUBJID<-as.character(d$USUBJID);d$STATUS<-as.character(d$STATUS)
  if(anyNA(d)||anyDuplicated(d$USUBJID)||any(!nzchar(trimws(d$USUBJID)))||any(!is.finite(d$ENTRY)|d$ENTRY<0|!is.finite(d$AGE)|d$AGE<0|d$ENTRY+d$AGE>cut+1e-7)||any(!d$STATUS %in% c("active","event","dropout")))stop("唯一ID，时间非负且≤IA；STATUS active/event/dropout。")
  if(any(d$STATUS=="active"&abs(d$ENTRY+d$AGE-cut)>1e-7)||any(d$STATUS=="event"&d$AGE<=0))stop("active须确认到IA，事件年龄须正；不自动填补确认间隙。")
  d
}
ic_from_adtte <- function(d,cut,endpoint,group="") {
  fields<-c("USUBJID","PARAMCD","CNSR","ENGINE_ENTRY_DAY","ENGINE_TIME_DAY","ENGINE_OBS_DAY")
  if(!all(fields %in% names(d)))stop("ADTTE需标识/终点/CNSR及三个连续日ENGINE字段。")
  d<-d[!is.na(d$PARAMCD)&d$PARAMCD==endpoint,,drop=FALSE]
  if(nzchar(trimws(group))){if(!"TRTP" %in% names(d))stop("指定组需TRTP。");d<-d[!is.na(d$TRTP)&d$TRTP==group,,drop=FALSE]}
  if(!nrow(d)||anyNA(d[,fields])||any(!d$CNSR %in% 0:2)||any(abs(d$ENGINE_ENTRY_DAY+d$ENGINE_TIME_DAY-d$ENGINE_OBS_DAY)>1e-7))stop("所选ADTTE为空或CNSR/连续时间不一致。")
  ic_snapshot(data.frame(USUBJID=d$USUBJID,ENTRY=d$ENGINE_ENTRY_DAY,AGE=d$ENGINE_TIME_DAY,STATUS=c("event","active","dropout")[d$CNSR+1L]),cut)
}
ic_age_counts <- function(c) {
  if(c$source=="parameters")return(c$age_counts)
  d<-c$data[c$data$STATUS=="active",,drop=FALSE]
  if(!nrow(d))return(data.frame(AGE=numeric(),N=integer()))
  ages<-sort(unique(d$AGE));data.frame(AGE=ages,N=vapply(ages,function(a)sum(d$AGE==a),integer(1)))
}
ic_known <- function(c)if(c$source=="parameters")c$known_events else sum(c$data$STATUS=="event")
ic_validate <- function(c) {
  if(!c$source %in% c("parameters","data")||!c$mode %in% c("single","joint")||!c$paramcd %in% c("PFS","OS")||!ma_scalar(c$cut,0,3649)||!c$model$id %in% c("exponential","weibull","pwe","gompertz","lognormal","loglogistic"))stop("选择IA来源、校准需求、单一终点及支持的基准模型。")
  ma_validate_model(c$model);time_factor(c$display_unit)
  if(c$source=="data")ic_snapshot(c$data,c$cut) else {
    a<-c$age_counts
    if(!is.data.frame(a)||!identical(names(a),c("AGE","N"))||nrow(a)>1000||anyNA(a)||!is.numeric(a$AGE)||!is.numeric(a$N)||any(!is.finite(a$AGE)|a$AGE<0|a$AGE>c$cut)||any(!is.finite(a$N)|a$N<=0|a$N!=floor(a$N))||anyDuplicated(a$AGE)||sum(a$N)>5000||!ma_integer(c$known_events,0,100000))stop("年龄表AGE,N，0–1000个唯一年龄0至IA、正整数人数，总仍随访≤5000，已知事件非负整数。")
  }
  a<-ic_age_counts(c);if(nrow(a)&&any(!is.finite(model_cumhaz(c$model,a$AGE))))stop("当前年龄累计风险须有限，不能条件于模型不支持的风险集。")
  if(!ma_scalar(c$accrual_day,1e-6,3650)||!ma_scalar(c$q,.001,100)||!ma_scalar(c$dropout_rate,0,1e5)||!ma_integer(c$future_n,0,100000))stop("未来均匀入组期正，q0.001–100，退出率非负，未来N非负整数。")
  t<-c$targets
  if(!is.data.frame(t)||!identical(names(t),c("DCO_DAY","target","weight","tolerance"))||nrow(t)<1||nrow(t)>10||anyNA(t)||any(!vapply(t,is.numeric,logical(1)))||any(!is.finite(as.matrix(t)))||any(t$DCO_DAY<=c$cut|t$DCO_DAY>3650)||is.unsorted(t$DCO_DAY,strictly=TRUE)||any(t$target<ic_known(c)|t$weight<=0|t$tolerance<=0))stop("目标1–10个递增IA后DCO，目标≥已知事件，权重和容许事件偏差正。")
  allowed<-c("q","future_n","dropout_rate")
  if(c$mode=="single"){if(!c$solve %in% c(allowed,"dco")||nrow(t)!=1)stop("单变量仅q/未来N/退出率/DCO且只有一个目标。");free<-c$solve} else {free<-c$free;if(length(free)<2||length(free)>3||any(!free %in% allowed)||anyDuplicated(free)||nrow(t)<length(free)||!ma_integer(c$starts,1,7))stop("联合校准预定2–3个q/N/退出率自由度，目标数不少于自由度，多起点1–7。")}
  b<-c$bounds
  if(!is.data.frame(b)||!identical(names(b),c("parameter","lower","upper"))||!setequal(b$parameter,free)||anyDuplicated(b$parameter)||anyNA(b)||any(!is.finite(b$lower)|!is.finite(b$upper)|b$lower>=b$upper))stop("每个自由度提供parameter,lower,upper预定范围。")
  for(k in free){x<-b[b$parameter==k,];limits<-switch(k,q=c(.001,100),future_n=c(0,100000),dropout_rate=c(0,1e5),dco=c(c$cut,3650));if(x$lower<limits[1]||x$upper>limits[2])stop("自由度范围超过风险倍数/人数/退出率/日历上限。")}
  invisible(TRUE)
}
ic_old_events <- function(c,U,q,mu) {
  a<-ic_age_counts(c);if(U<=0||!nrow(a))return(0)
  H0<-model_cumhaz(c$model,a$AGE)
  if(mu==0)return(sum(a$N*(-expm1(-q*(model_cumhaz(c$model,a$AGE+U)-H0)))))
  # Sum patient probabilities inside the integral, preserving each exact risk age.
  integrand<-function(v)vapply(v,function(x){age<-a$AGE+x;H<-model_cumhaz(c$model,age);logh<-fx_density_any(c$model,age)+H;z<-log(q)+logh-q*(H-H0)-mu*x;z[is.infinite(H)&H>0]<--Inf;sum(a$N*exp(z))},numeric(1))
  breaks<-sort(unique(c(0,U,unlist(lapply(c$model$cuts %||% numeric(),function(k)k-a$AGE)))))
  breaks<-breaks[breaks>=0&breaks<=U]
  sum(vapply(seq_len(length(breaks)-1L),function(j)integrate(integrand,breaks[j],breaks[j+1L],subdivisions=400L,rel.tol=1e-7,abs.tol=1e-8)$value,numeric(1)))
}
ic_expectation <- function(c,T,q=c$q,N=c$future_n,mu=c$dropout_rate) {
  U<-max(0,T-c$cut);old<-ic_old_events(c,U,q,mu);p<-rc_integral(c$model,U,q,mu,c$accrual_day)
  list(total=ic_known(c)+old+N*p,known=ic_known(c),old=old,future=N*p,future_probability=p,expected_new_entered=N*min(U/c$accrual_day,1))
}
ic_apply <- function(c,values){for(k in names(values))if(k!="dco")c[[k]]<-unname(values[k]);if("dco" %in% names(values))c$targets$DCO_DAY<-unname(values["dco"]);c}
ic_constraints <- function(c){t<-c$targets;d<-batch_bind_columns(lapply(t$DCO_DAY,function(T){x<-ic_expectation(c,T);data.frame(expected=x$total,known=x$known,old=x$old,future=x$future,expected_new_entered=x$expected_new_entered)}));d<-cbind(t,d);d$residual<-d$expected-d$target;d$standardized_residual<-d$residual/d$tolerance;d$within_tolerance<-abs(d$residual)<=d$tolerance;d}
run_ia_reverse_calibration <- function(c) {
  ic_validate(c);free<-if(c$mode=="single")c$solve else c$free;b<-c$bounds[match(free,c$bounds$parameter),];trace<-list();weights<-c$targets$weight/sum(c$targets$weight)
  if(c$mode=="single"){
    evaluate<-function(x){s<-ic_apply(c,setNames(x,free));d<-ic_constraints(s);trace[[length(trace)+1L]]<<-data.frame(call=length(trace)+1L,start=1L,objective=d$standardized_residual^2,value=x,residual=d$residual);d$residual}
    lo<-evaluate(b$lower);hi<-evaluate(b$upper)
    if(!is.finite(lo)||!is.finite(hi)||abs(hi-lo)<1e-10)stop("上下界期望没有可区分变化；该目标不能在此区间识别自由度。")
    if(lo*hi>0)stop("目标超出预定区间，不自动改变假设或扩大求解范围。")
    value<-if(lo==0)b$lower else if(hi==0)b$upper else uniroot(evaluate,c(b$lower,b$upper),tol=1e-7)$root
    solved<-ic_apply(c,setNames(value,free));converged<-TRUE;convergence<-0L;start_records<-data.frame(start=1L,convergence=0L,objective=sum(weights*ic_constraints(solved)$standardized_residual^2));y<-(value-b$lower)/(b$upper-b$lower)
  } else {
    span<-b$upper-b$lower;current_start<-1L
    objective<-function(y){s<-ic_apply(c,setNames(b$lower+y*span,free));d<-tryCatch(ic_constraints(s),error=function(e)NULL);v<-if(is.null(d))1e100 else sum(weights*d$standardized_residual^2);trace[[length(trace)+1L]]<<-cbind(data.frame(call=length(trace)+1L,start=current_start,objective=v),as.data.frame(as.list(setNames(b$lower+y*span,free))));v}
    initial<-rbind(rep(.5,length(free)),rep(.2,length(free)),rep(.8,length(free)),vapply(seq_len(length(free)),function(j)ifelse(seq_along(free)==j,.2,.8),numeric(length(free))),vapply(seq_len(length(free)),function(j)ifelse(seq_along(free)==j,.8,.2),numeric(length(free))))
    initial<-unique(initial);initial<-initial[seq_len(min(c$starts,nrow(initial))),,drop=FALSE];fits<-vector("list",nrow(initial))
    for(j in seq_len(nrow(initial))){current_start<-j;fits[[j]]<-tryCatch(optim(initial[j,],objective,method="L-BFGS-B",lower=rep(0,length(free)),upper=rep(1,length(free)),control=list(maxit=200L,factr=1e7)),error=function(e)NULL)}
    start_records<-batch_bind_columns(lapply(seq_along(fits),function(j){x<-fits[[j]];data.frame(start=j,convergence=if(is.null(x))NA_integer_ else x$convergence,objective=if(is.null(x))NA_real_ else x$value)}))
    ok<-vapply(fits,function(x)!is.null(x)&&x$convergence==0&&is.finite(x$value)&&x$value<1e99,logical(1));if(!any(ok))stop("预定多起点未得到收敛有限方案；不改变目标、边界或模型。")
    idx<-which(ok)[which.min(vapply(fits[ok],`[[`,numeric(1),"value"))];fit<-fits[[idx]];y<-fit$par;solved<-ic_apply(c,setNames(b$lower+y*span,free));converged<-TRUE;convergence<-fit$convergence
  }
  constraints<-ic_constraints(solved);values<-vapply(free,function(k)if(k=="dco")solved$targets$DCO_DAY[1] else solved[[k]],numeric(1));solution<-data.frame(parameter=free,value=values,unit=ifelse(free=="dropout_rate","per_day",ifelse(free=="future_n","persons",ifelse(free=="dco","days","multiplier"))),lower=b$lower,upper=b$upper,at_boundary=y<1e-5|y>1-1e-5)
  # Local residual Jacobian in normalized parameter coordinates; not global uniqueness.
  J<-matrix(NA_real_,nrow(constraints),length(free));for(j in seq_along(free)){yl<-yh<-y;yl[j]<-max(0,y[j]-1e-4);yh[j]<-min(1,y[j]+1e-4);getr<-function(v)ic_constraints(ic_apply(c,setNames(b$lower+v*(b$upper-b$lower),free)))$standardized_residual;J[,j]<-(getr(yh)-getr(yl))/(yh[j]-yl[j])}
  singular<-svd(sweep(J,1,sqrt(weights),"*"),nu=0,nv=0)$d;rank<-sum(singular>max(1e-12,max(singular)*1e-6));condition<-if(length(singular)&&min(singular)>0)max(singular)/min(singular) else Inf
  identification<-data.frame(free_parameters=length(free),targets=nrow(constraints),local_rank=rank,local_condition_number=condition,local_full_rank=rank==length(free),global_uniqueness_established=FALSE)
  jacobian<-batch_bind_columns(lapply(seq_along(free),function(j)data.frame(DCO_DAY=constraints$DCO_DAY,parameter=free[j],normalized_residual_derivative=J[,j])))
  rounded<-NULL;round_constraints<-data.frame();if("future_n" %in% free){rounded<-solved;rounded$future_n<-ceiling(solved$future_n);round_constraints<-ic_constraints(rounded);round_constraints$rounded_future_n<-rounded$future_n}
  times<-sort(unique(c(seq(c$cut,max(solved$targets$DCO_DAY),length.out=41),solved$targets$DCO_DAY)));curve<-batch_bind_columns(lapply(times,function(T){x<-ic_expectation(solved,T);data.frame(DCO_DAY=T,expected=x$total,known=x$known,old=x$old,future=x$future)}))
  overview<-data.frame(mode=c$mode,converged=converged,convergence_code=convergence,weighted_objective=sum(weights*constraints$standardized_residual^2),all_targets_within_tolerance=all(constraints$within_tolerance),integer_plan_within_tolerance=if(nrow(round_constraints))all(round_constraints$within_tolerance) else NA,local_full_rank=identification$local_full_rank,interpretation="给定IA和模型的前瞻规划校准，不是估计真实HR/退出机制或保证事件数")
  list(config=c,solved=solved,overview=overview,solution=solution,constraints=constraints,rounded_constraints=round_constraints,identification=identification,jacobian=jacobian,starts=start_records,trace=batch_bind_columns(trace),curve=curve,age_overview=ic_age_counts(c),version=batch_version,source_hash=batch_source_hash(),dependencies=batch_dependencies(list()),review_status="pending_v0.35_review")
}
ic_report <- function(r)c("# 实际IA期望事件反向校准开发稿",paste0("版本",r$version),"固定IA已知事件与确认无事件年龄；未来q只作用于IA后的条件累计风险，永久退出不恢复。有限计划新人数在IA后均匀入组，独立指数退出。联合目标为预定加权容差残差最小化；收敛、满足全部目标、局部可识别与整数方案分别报告。局部Jacobian不证明全局唯一；不是自由估计真实治疗HR或预测区间。",capture.output(print(r$overview,row.names=FALSE)),capture.output(print(r$solution,row.names=FALSE)),capture.output(print(r$constraints,row.names=FALSE)),capture.output(print(r$rounded_constraints,row.names=FALSE)),capture.output(print(r$identification,row.names=FALSE)),"代码/推导/数值和真实导出未验证。")
