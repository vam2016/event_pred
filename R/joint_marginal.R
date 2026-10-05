# Marginal survival from the specified illness-death model; not a fit to patient data.
joint_marginal_survival <- function(tc,t,group="Control",endpoint="OS") {
  if(!endpoint %in% c("PFS","OS")||!is.numeric(t)||any(!is.finite(t)|t<0|t>3650))stop("参考风险年龄需0–3650日，终点为PFS/OS。")
  names_g<-vapply(tc$groups,`[[`,character(1),"name");g<-tc$groups[[match(group,names_g)]];if(is.null(g))stop("参考组不存在。")
  q<-g$multipliers;m<-lapply(tc$transitions,`[[`,"model")
  s0<-function(x)exp(-q[["01"]]*model_cumhaz(m[["01"]],x)-q[["02"]]*model_cumhaz(m[["02"]],x))
  if(endpoint=="PFS")return(s0(t))
  if(all(vapply(m,function(x)x$id=="exponential",logical(1)))){
    a<-q[["01"]]*m[["01"]]$params[1];b<-q[["02"]]*m[["02"]]$params[1];g12<-q[["12"]]*m[["12"]]$params[1];r<-a+b
    mass<-if(r==g12)a*t*exp(-r*t) else if(r>g12)a*exp(-g12*t)*(-expm1(-(r-g12)*t))/(r-g12) else a*exp(-r*t)*(-expm1(-(g12-r)*t))/(g12-r)
    return(pmin(1,pmax(s0(t),exp(-r*t)+mass)))
  }
  vapply(t,function(age){
    if(age==0)return(1)
    h<-q[["01"]]*model_cumhaz(m[["01"]],age);mass<--expm1(-h)
    if(mass==0)return(s0(age))
    # Transform ds*q01*h01(s) to dF01(s), avoiding the Weibull origin singularity.
    time_knots<-sort(unique(c(0,age,m[["01"]]$cuts,m[["02"]]$cuts,if(tc$post_clock=="reset")age-m[["12"]]$cuts else m[["12"]]$cuts)))
    time_knots<-time_knots[is.finite(time_knots)&time_knots>=0&time_knots<=age]
    knots<-sort(unique(c(0,mass,-expm1(-q[["01"]]*model_cumhaz(m[["01"]],time_knots)))))
    integrand<-function(x){s<-inverse_cumhaz(m[["01"]],-log1p(-x)/q[["01"]])
      post<-if(tc$post_clock=="reset")model_cumhaz(m[["12"]],age-s) else model_cumhaz(m[["12"]],age)-model_cumhaz(m[["12"]],s)
      if(any(!is.finite(s))||anyNA(post)||any(post< -1e-10))stop("边际参考积分遇到无效时钟差。")
      exp(-q[["02"]]*model_cumhaz(m[["02"]],s)-q[["12"]]*pmax(post,0))
    }
    value<-s0(age)+sum(vapply(seq_len(length(knots)-1L),function(i)integrate(integrand,knots[i],knots[i+1],rel.tol=1e-8,abs.tol=1e-9,subdivisions=300)$value,numeric(1)))
    if(!is.finite(value)||value< s0(age)-1e-7||value>1+1e-7)stop("边际参考积分超出概率范围。")
    pmin(1,pmax(s0(age),value))
  },numeric(1))
}
joint_marginal_rmst <- function(tc,tau,group,endpoint) {
  if(!is.numeric(tau)||length(tau)!=1||!is.finite(tau)||tau<=0||tau>3650)stop("参考RMST上限需在(0,3650]日。")
  knots<-sort(unique(c(0,tau,unlist(lapply(tc$transitions,function(a)a$model$cuts),use.names=FALSE))));knots<-knots[knots>=0&knots<=tau]
  sum(vapply(seq_len(length(knots)-1L),function(i)integrate(function(t)joint_marginal_survival(tc,t,group,endpoint),knots[i],knots[i+1],rel.tol=1e-7,abs.tol=1e-8,subdivisions=200)$value,numeric(1)))
}
jr_marginal_reference <- function(r,scenario,times_day,display_unit=r$config$display_unit) {
  if(!length(times_day)||length(times_day)>12||any(!is.finite(times_day)|times_day<=0|times_day>3650)||anyDuplicated(times_day))stop("参考时点需1–12个不重复正风险年龄，最长3650日。")
  sc<-r$scenarios[r$scenarios$SCENARIO==scenario,,drop=FALSE];if(nrow(sc)!=1)stop("未知情景。");tc<-jr_trial_config(r$config,sc);tables<-list()
  for(e in c("PFS","OS"))for(t in times_day){
    evaluate<-function(g)tryCatch(list(s=joint_marginal_survival(tc,t,g,e),rmst=joint_marginal_rmst(tc,t,g,e),note=""),error=function(err)list(s=NA_real_,rmst=NA_real_,note=conditionMessage(err)))
    a<-evaluate("Control");b<-evaluate("Treatment")
    tables[[length(tables)+1L]]<-data.frame(SCENARIO=scenario,PARAMCD=e,time_day=t,control_survival=a$s,treatment_survival=b$s,survival_difference=b$s-a$s,control_rmst_day=a$rmst,treatment_rmst_day=b$rmst,rmst_difference_day=b$rmst-a$rmst,note=paste(c(a$note,b$note)[nzchar(c(a$note,b$note))],collapse="；"))
  }
  list(table=do.call(rbind,tables),config=r$config,scenario=scenario,run_id=r$run_id %||% "",display_unit=display_unit,version=batch_version,review_status="pending_v0.35_review",created_at=format(Sys.time(),tz="UTC",usetz=TRUE),note="参数模型边际概率/积分；排除入组、退出和研究DCO，不读取潜在患者或拟合临床资料；未验证")
}
jr_reproduction_script <- function(r) {
  dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n")
  keys<-if(nrow(r$rows))r$rows[,c("SCENARIO","SIMID"),drop=FALSE] else data.frame(SCENARIO=integer(),SIMID=integer())
  c("# Joint endpoint study v0.27 development draft; replay has not been executed.",
    'if(!exists("%||%",mode="function")) `%||%` <- function(x,y)if(is.null(x))y else x',
    'for(f in c("units","models","forecast","inputs","simulation","nph","sequential","study","parameter_calibration","batch_research","batch_analysis","batch_sequential","batch_workspace","joint_survival","joint_research","joint_marginal","adaptation","patient_adaptation","adaptive_batch"))source(paste0("R/",f,".R"))',
    paste0('if(as.character(getRversion())!="',r$dependencies$R,'")stop("R differs")'),paste0('if(as.character(packageVersion("survival"))!="',r$dependencies$survival,'")stop("survival differs")'),
    if(!is.null(r$source_hash))paste0('if(batch_source_hash()!="',r$source_hash,'")stop("Engine source differs")'),
    paste0("cfg <- ",dump(r$config)),paste0("saved_keys <- ",dump(keys)),
    'result <- run_joint_research(cfg,replay_keys=saved_keys)','saveRDS(result,"joint_research_replayed.rds")',
    vapply(c("scenarios","rows","method_rows","policy_rows","overview","policy_overview","policy_pairs","resources","endpoint_correlations"),function(k)paste0('write.csv(result$',k,',"joint_',k,'_replayed.csv",row.names=FALSE)'),character(1)))
}
