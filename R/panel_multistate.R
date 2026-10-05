# Homogeneous three-state Markov likelihood for panel observations, not interval midpoints.
pn_fields <- c("USUBJID","GROUP","ENTRY","AGE","STATE","KIND","EXIT")
pn_snapshot <- function(d,cut,unit="days") {
  if(!is.data.frame(d)||!identical(sort(names(d)),sort(pn_fields))||nrow(d)<2||nrow(d)>20000)stop("panel CSV严格USUBJID,GROUP,ENTRY,AGE,STATE,KIND,EXIT，2–20000行。")
  d<-d[,pn_fields];f<-time_factor(unit);for(k in c("ENTRY","AGE")){if(!is.numeric(d[[k]]))stop("ENTRY/AGE数值。");d[[k]]<-d[[k]]*f};for(k in setdiff(pn_fields,c("ENTRY","AGE")))d[[k]]<-as.character(d[[k]])
  if(anyNA(d)||any(!nzchar(d$USUBJID)|!nzchar(d$GROUP))||any(!is.finite(d$ENTRY)|d$ENTRY<0|!is.finite(d$AGE)|d$AGE<0|d$ENTRY+d$AGE>cut+1e-7)||any(!d$STATE %in% c("0","1","2","ALIVE"))||any(!d$KIND %in% c("panel","exact_death"))||any(!d$EXIT %in% c("followup","active","dropout","event")))stop("时间≤IA；STATE 0/1/2/ALIVE，KIND panel/exact_death，EXIT followup/active/dropout/event。")
  ids<-unique(d$USUBJID);if(length(ids)>3000||length(unique(d$GROUP))>8)stop("最多3000患者和8组。")
  out<-lapply(ids,function(id){x<-d[d$USUBJID==id,];x<-x[order(x$AGE),];n<-nrow(x)
    if(n<2||length(unique(x$GROUP))!=1||length(unique(x$ENTRY))!=1||x$AGE[1]!=0||x$STATE[1]!="0"||x$KIND[1]!="panel"||any(diff(x$AGE)<=0))stop("每人至少基线+一个观察，固定组/入组，基线AGE0/STATE0，年龄严格递增。")
    if(any(head(x$EXIT,-1)!="followup")||tail(x$EXIT,1)=="followup"||any(head(x$STATE,-1)=="2")||any(head(x$KIND,-1)=="exact_death"))stop("只有末条可终止；死亡后无后续记录。")
    last<-x[n,];if((last$STATE=="2")!=(last$EXIT=="event")||last$KIND=="exact_death"&&last$STATE!="2"||last$EXIT=="active"&&abs(last$ENTRY+last$AGE-cut)>1e-7)stop("末条死亡STATE2/EXITevent；exact_death须STATE2，active确认至IA。")
    known_one<-which(x$STATE=="1");if(length(known_one)&&any(x$STATE[seq.int(min(known_one),n)]=="0"))stop("不可逆进展后不能再STATE0。")
    x
  });row.names(d)<-NULL;batch_bind_columns(out)
}
pn_valid_rates <- function(r)length(r)==3L&&all(is.finite(r))&&all(r>0)&&identical(names(r),c("01","02","12"))
pn_transition <- function(r,t) {
  if(!pn_valid_rates(r)||length(t)!=1||!is.finite(t)||t<0)stop("三正转移率和非负有限间隔。")
  a<-r[1]+r[2];b<-r[3];p00<-exp(-a*t);p11<-exp(-b*t);difference<-abs(a-b)
  p01<-if(difference==0)r[1]*t*p00 else r[1]*exp(-min(a,b)*t)*(-expm1(-difference*t))/difference
  P<-rbind(c(p00,p01,1-p00-p01),c(0,p11,-expm1(-b*t)),c(0,0,1));dimnames(P)<-list(c("0","1","2"),c("0","1","2"))
  if(any(!is.finite(P))||any(P< -1e-12|P>1+1e-12))stop("转移概率超出有效范围。")
  P[P<0]<-0;P[P>1]<-1;P
}
pn_filter <- function(x,r,keep=FALSE) {
  v<-c(1,0,0);ll<-0;rows<-list()
  if(keep)rows[[1]]<-data.frame(USUBJID=x$USUBJID[1],GROUP=x$GROUP[1],AGE=x$AGE[1],STATE=x$STATE[1],KIND=x$KIND[1],increment_loglik=0,prob0=1,prob1=0,prob2=0)
  for(j in seq.int(2,nrow(x))){delta<-x$AGE[j]-x$AGE[j-1];u<-as.numeric(v%*%pn_transition(r,delta))
    if(x$KIND[j]=="exact_death"){mass<-u[1]*r[2]+u[2]*r[3];v<-c(0,0,1)} else {mask<-switch(x$STATE[j],"0"=c(1,0,0),"1"=c(0,1,0),"2"=c(0,0,1),ALIVE=c(1,1,0));u<-u*mask;mass<-sum(u);v<-if(mass>0)u/mass else c(NA_real_,NA_real_,NA_real_)}
    if(!is.finite(mass)||mass<=0)stop("给定观察序列的转移似然为零/非有限；不替换区间中点。")
    ll<-ll+log(mass);if(keep)rows[[j]]<-data.frame(USUBJID=x$USUBJID[j],GROUP=x$GROUP[j],AGE=x$AGE[j],STATE=x$STATE[j],KIND=x$KIND[j],increment_loglik=log(mass),prob0=v[1],prob1=v[2],prob2=v[3])
  }
  last<-tail(x,1);list(loglik=ll,posterior=v,records=if(keep)batch_bind_columns(rows) else data.frame(),current=data.frame(USUBJID=last$USUBJID,GROUP=last$GROUP,ENTRY=last$ENTRY,AGE=last$AGE,EXIT=last$EXIT,N=1L,prob0=v[1],prob1=v[2],prob2=v[3],known_pfs=as.integer(any(x$STATE %in% c("1","2"))),known_os=as.integer(last$STATE=="2")))
}
pn_rate_table <- function(c) {
  d<-c$rate_table
  if(!is.data.frame(d)||!identical(names(d),c("GROUP","lambda01","lambda02","lambda12"))||nrow(d)<1||nrow(d)>8||anyNA(d)||anyDuplicated(d$GROUP)||any(!nzchar(d$GROUP))||any(!vapply(d[-1],is.numeric,logical(1)))||any(!is.finite(as.matrix(d[-1]))|as.matrix(d[-1])<=0))stop("风险表GROUP,lambda01,lambda02,lambda12，1–8唯一组，三率正。")
  d
}
pn_validate <- function(c) {
  if(!c$source %in% c("parameters","data")||!c$purpose %in% c("fit","counts")||!ma_scalar(c$cut,0,3649))stop("选择模型参考或期望计数、参数或panel资料、IA时间。")
  time_factor(c$display_unit);rates<-pn_rate_table(c);g<-as.character(rates$GROUP)
  if(c$source=="data"){d<-pn_snapshot(c$data,c$cut);if(!setequal(unique(d$GROUP),g))stop("风险初值表须与panel资料组名完全相同。");for(group in g)if(length(unique(d$USUBJID[d$GROUP==group]))<5)stop("每组拟合至少5患者，阈值不保证可识别。")} else if(c$purpose=="counts") {
    s<-c$state_counts;if(!is.data.frame(s)||!identical(names(s),c("GROUP","n0","n1","n2"))||!setequal(s$GROUP,g)||anyDuplicated(s$GROUP)||anyNA(s)||any(!vapply(s[-1],is.numeric,logical(1)))||any(!is.finite(as.matrix(s[-1]))|as.matrix(s[-1])<0|as.matrix(s[-1])!=floor(as.matrix(s[-1])))||sum(as.matrix(s[-1]))>100000)stop("状态人数GROUP,n0,n1,n2，同模型组名，非负整数，总≤100000。")
  }
  if(length(c$reference_ages)<1||length(c$reference_ages)>30||any(!is.finite(c$reference_ages)|c$reference_ages<=0|c$reference_ages>3650)||is.unsorted(c$reference_ages,strictly=TRUE))stop("参考年龄1–30个正递增、≤3650日。")
  if(c$purpose=="counts"){
    if(!ma_integer(c$new_n,0,100000)||!ma_scalar(c$accrual_day,1e-6,3650)||length(c$horizons)<1||length(c$horizons)>20||any(!is.finite(c$horizons)|c$horizons<=c$cut|c$horizons>3650)||is.unsorted(c$horizons,strictly=TRUE))stop("未来N非负整数，有限均匀入组期正，未来时点IA后递增且≤3650日。")
    p<-c$future_table;if(!is.data.frame(p)||!identical(names(p),c("GROUP","weight","dropout_rate"))||!setequal(p$GROUP,g)||anyDuplicated(p$GROUP)||anyNA(p)||any(!is.finite(p$weight)|p$weight<=0|!is.finite(p$dropout_rate)|p$dropout_rate<0))stop("未来表GROUP,weight,dropout_rate，同组，权重正、退出率非负。")
  };invisible(TRUE)
}
pn_fit <- function(c) {
  tab<-pn_rate_table(c);models<-list();diagnostics<-list()
  for(i in seq_len(nrow(tab))){g<-tab$GROUP[i];r<-setNames(as.numeric(unlist(tab[i,-1],use.names=FALSE)),c("01","02","12"));ll<-NA_real_;eig<-rep(NA_real_,3);cond<-NA_real_;rank<-NA_integer_;convergence<-NA_integer_
    if(c$source=="data"){
      patients<-split(c$data[c$data$GROUP==g,,drop=FALSE],c$data$USUBJID[c$data$GROUP==g]);objective<-function(v){rates<-setNames(exp(v),names(r));value<-tryCatch(-sum(vapply(patients,function(x)pn_filter(x,rates)$loglik,numeric(1))),error=function(e)1e100);if(is.finite(value))value else 1e100}
      lo<-rep(-25,3);hi<-rep(5,3);initial<-log(r);if(any(initial<=lo|initial>=hi))stop("拟合初值须在log每日报险(-25,5)内。")
      starts<-list(initial,pmin(pmax(initial+log(2),lo+.01),hi-.01),pmin(pmax(initial-log(2),lo+.01),hi-.01));fits<-lapply(starts,function(v)tryCatch(optim(v,objective,method="L-BFGS-B",lower=lo,upper=hi,control=list(maxit=400L)),error=function(e)NULL));ok<-vapply(fits,function(x)!is.null(x)&&x$convergence==0&&is.finite(x$value)&&x$value<1e99&&all(x$par>lo+1e-5&x$par<hi-1e-5),logical(1));if(!any(ok))stop(paste0(g,"的预定多起点未获得内部收敛有限解。"));fit<-fits[[which(ok)[which.min(vapply(fits[ok],`[[`,numeric(1),"value"))]]];r<-setNames(exp(fit$par),c("01","02","12"));ll<--fit$value;convergence<-fit$convergence
      H<-tryCatch(optimHess(fit$par,objective),error=function(e)NULL);if(!is.null(H)&&all(is.finite(H))){eig<-eigen((H+t(H))/2,symmetric=TRUE,only.values=TRUE)$values;rank<-sum(eig>max(1e-8,max(abs(eig))*1e-6));cond<-if(min(eig)>0)max(eig)/min(eig) else Inf}
    }
    models[[g]]<-r;diagnostics[[g]]<-data.frame(GROUP=g,source=c$source,loglik=ll,aic=if(is.finite(ll))6-2*ll else NA_real_,convergence=convergence,observed_information_rank=rank,information_condition=cond,min_information_eigenvalue=min(eig),information_positive=if(all(is.finite(eig)))all(eig>0) else NA,global_identifiability_established=FALSE,note="三正常数转移风险；访视/退出可忽略假设；无自动Wald区间")
  };list(models=models,diagnostics=batch_bind_columns(diagnostics))
}
pn_current <- function(c,models) {
  if(c$source=="parameters"&&c$purpose=="fit")return(list(current=data.frame(),filter_records=data.frame()))
  if(c$source=="data"){a<-lapply(split(c$data,c$data$USUBJID),function(x)pn_filter(x,models[[x$GROUP[1]]],TRUE));return(list(current=batch_bind_columns(lapply(a,`[[`,"current")),filter_records=batch_bind_columns(lapply(a,`[[`,"records"))))}
  rows<-batch_bind_columns(lapply(seq_len(nrow(c$state_counts)),function(i){x<-c$state_counts[i,];batch_bind_columns(lapply(0:2,function(s)data.frame(USUBJID=paste0("AGG_",x$GROUP,"_STATE",s),GROUP=x$GROUP,ENTRY=NA_real_,AGE=NA_real_,EXIT=if(s==2)"event" else "active",N=x[[paste0("n",s)]],prob0=as.integer(s==0),prob1=as.integer(s==1),prob2=as.integer(s==2),known_pfs=as.integer(s>0),known_os=as.integer(s==2))))}));list(current=rows,filter_records=data.frame())
}
pn_event_prob <- function(r,U,mu=0) {
  if(U<=0)return(c(PFS0=0,OS0=0,OS1=0))
  a<-r[1]+r[2];b<-r[3];A<-a+mu;B<-b+mu;J<-function(z)-expm1(-z*U)/z
  # Nonnegative two-step convolution avoids cancellation when A and B are close.
  progression_death<-if(abs(A-B)>1e-4*max(A,B,1/U))unname(r[1]*r[3]*(J(B)-J(A))/(A-B)) else integrate(function(t)vapply(t,function(v)pn_transition(setNames(c(unname(r[1]),unname(r[2])+mu,unname(r[3])+mu),c("01","02","12")),v)[1,2]*r[3],numeric(1)),0,U,rel.tol=1e-7,abs.tol=1e-9)$value
  values<-c(PFS0=unname(a*J(A)),OS0=unname(r[2]*J(A)+progression_death),OS1=unname(b*J(B)))
  if(any(!is.finite(values))||any(values< -1e-9|values>1+1e-9))stop("退出前事件概率无效。");pmin(pmax(values,0),1)
}
pn_expected_counts <- function(c,models,current) {
  known_pfs<-sum(current$N*current$known_pfs);known_os<-sum(current$N*current$known_os);baseline_pfs<-sum(current$N*(current$prob1+current$prob2));baseline_os<-sum(current$N*current$prob2);plan<-c$future_table;plan$weight<-plan$weight/sum(plan$weight)
  batch_bind_columns(lapply(c$horizons,function(T){U<-T-c$cut;old<-c(PFS=0,OS=0);new<-c(PFS=0,OS=0)
    for(g in names(models)){r<-models[[g]];z<-plan[plan$GROUP==g,];p<-pn_event_prob(r,U,z$dropout_rate);x<-current[current$GROUP==g&current$EXIT=="active",];old<-old+c(PFS=sum(x$N*x$prob0)*p["PFS0"],OS=sum(x$N*x$prob0)*p["OS0"]+sum(x$N*x$prob1)*p["OS1"])
      if(c$new_n>0){end<-min(U,c$accrual_day);for(e in c("PFS","OS")){key<-if(e=="PFS")"PFS0" else "OS0";prob<-integrate(function(v)vapply(v,function(v0)pn_event_prob(r,U-v0,z$dropout_rate)[key],numeric(1)),0,end,rel.tol=1e-6,abs.tol=1e-8)$value/c$accrual_day;new[e]<-new[e]+c$new_n*z$weight*prob}}
    }
    data.frame(DCO_DAY=T,known_ia_pfs=known_pfs,known_ia_os=known_os,model_ia_unconfirmed_pfs=baseline_pfs-known_pfs,model_ia_unconfirmed_os=baseline_os-known_os,old_future_pfs=unname(old[1]),old_future_os=unname(old[2]),new_future_pfs=unname(new[1]),new_future_os=unname(new[2]),expected_model_pfs=baseline_pfs+unname(old[1])+unname(new[1]),expected_model_os=baseline_os+unname(old[2])+unname(new[2]),expected_new_entered=c$new_n*min(U/c$accrual_day,1),note="潜在疾病事件的模型期望+独立未来退出；IA未确认进展单列，非报告计数/预测区间")
  }))
}
run_panel_multistate <- function(c) {
  pn_validate(c);if(c$source=="data")c$data<-pn_snapshot(c$data,c$cut);fit<-pn_fit(c);post<-pn_current(c,fit$models)
  model_overview<-batch_bind_columns(lapply(names(fit$models),function(g)data.frame(GROUP=g,transition=c("01","02","12"),rate_per_day=unname(fit$models[[g]]))))
  reference<-batch_bind_columns(lapply(names(fit$models),function(g)batch_bind_columns(lapply(c$reference_ages,function(t){P<-pn_transition(fit$models[[g]],t);data.frame(GROUP=g,AGE_DAY=t,state0=P[1,1],state1=P[1,2],dead=P[1,3],PFS_survival=P[1,1],OS_survival=P[1,1]+P[1,2],OS_from_state1=P[2,2])}))))
  list(config=c,models=fit$models,model_overview=model_overview,fit_diagnostics=fit$diagnostics,current_states=post$current,filter_records=post$filter_records,reference=reference,count_overview=if(c$purpose=="counts")pn_expected_counts(c,fit$models,post$current) else data.frame(),version=batch_version,source_hash=batch_source_hash(),dependencies=batch_dependencies(list()),review_status="pending_v0.35_review",interpretation="常数Markov转移，非信息性访视/退出；模型期望，不是正式联合适应或访视选择联合似然")
}
pn_report <- function(r)c("# Panel三状态模型开发稿",paste0("版本",r$version),r$interpretation,"STATE ALIVE只提供存活信息，0/1组成由过滤分布给出；区间死亡和精确死亡使用不同似然，不插入中点或伪造精确进展。率按组联合估计或直接指定；状态人数参数无需患者文件。信息矩阵诊断不证明全局可识别，无自动Wald区间/预测区间。未知进展IA模型补全单列，不称为已报告事件。",capture.output(print(r$model_overview,row.names=FALSE)),capture.output(print(r$fit_diagnostics,row.names=FALSE)),capture.output(print(r$count_overview,row.names=FALSE)),"尚未开展加载/拟合/数值/结果/界面/导出验证。")
