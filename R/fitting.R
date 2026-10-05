# Stable right-censored likelihoods, on a rescaled elapsed-time coordinate.
gompertz_loglik <- function(theta, time, event) {
  b <- exp(theta[1]); g <- theta[2]
  if(any(!is.finite(c(b,g)))||b<=0) return(-Inf)
  h <- if(g==0) b*time else b*expm1(g*time)/g
  ll <- event*(theta[1]+g*time)-h
  if(any(!is.finite(ll))) -Inf else sum(ll)
}
cure_weibull_loglik <- function(theta,time,event) {
  eta <- exp(theta[1]); k <- exp(theta[2]); pi <- plogis(theta[3])
  if(any(!is.finite(c(eta,k,pi)))||eta<=0||k<=0||pi<=0||pi>=1) return(-Inf)
  hs <- exp(k*(log(time)-theta[1]))
  log_f <- log1p(-pi)+theta[2]-theta[1]+(k-1)*(log(time)-theta[1])-hs
  log_s <- log_add(log(pi),log1p(-pi)-hs)
  ll <- ifelse(event==1,log_f,log_s)
  if(any(!is.finite(ll))) -Inf else sum(ll)
}
fit_extended_model <- function(data,method,label) {
  scale_t <- median(data$time); t <- data$time/scale_t; e <- data$event
  fn <- if(method=="gompertz") gompertz_loglik else cure_weibull_loglik
  starts <- if(method=="gompertz") lapply(c(-.3,0,.3),function(g)c(log(sum(e)/sum(t)),g)) else {
    init <- tryCatch(fit_model(data,"weibull")$params,error=function(e)c(location=log(scale_t),scale=1))
    lapply(c(.05,.3,.6),function(pi)c(unname(init[1]-log(scale_t)), -log(unname(init[2])), qlogis(pi)))
  }
  fits <- lapply(starts,function(start) tryCatch(optim(start,function(th) {v <- -fn(th,t,e); if(is.finite(v)) v else 1e100},method="BFGS",control=list(maxit=1000,reltol=1e-11),hessian=TRUE),error=function(e)NULL))
  fits <- Filter(function(x)!is.null(x)&&x$convergence==0&&is.finite(x$value)&&x$value<1e99&&all(is.finite(x$par)),fits)
  if(!length(fits)) stop("扩展模型优化未收敛，请检查事件及尾部随访信息。")
  best <- fits[[which.min(vapply(fits,`[[`,numeric(1),"value"))]]
  eigenvalues <- tryCatch(eigen(best$hessian,symmetric=TRUE,only.values=TRUE)$values,error=function(e)NA_real_)
  if(any(!is.finite(eigenvalues))||min(eigenvalues)<=max(eigenvalues)*1e-8||max(eigenvalues)<=0) stop("扩展模型局部曲率不足或协方差无效，不能稳定外推。")
  score <- vapply(seq_along(best$par),function(j) {h <- 1e-5; up <- down <- best$par; up[j] <- up[j]+h; down[j] <- down[j]-h; (fn(up,t,e)-fn(down,t,e))/(2*h)},numeric(1))
  if(any(!is.finite(score))||max(abs(score))>1e-3) stop("扩展模型似然梯度未通过数值检查。")
  warning <- character()
  if(method=="gompertz") {
    params <- c(rate=exp(best$par[1])/scale_t,shape=best$par[2]/scale_t)
    if(params[2]<0) warning <- "Gompertz形状<0，模型保留无穷事件时间质量；需核对远期假设。"
  } else {
    pi <- plogis(best$par[3])
    if(pi<1e-5||pi>1-1e-5) stop("治愈比例接近参数边界；请比较普通Weibull或延长尾部观察。")
    params <- c(location=best$par[1]+log(scale_t),scale=exp(-best$par[2]),cure=pi)
    warning <- "治愈比例为参数化尾部估计，不能据此判定个体治愈；需比较尾部情景。"
  }
  ll <- -best$value-sum(e)*log(scale_t)
  list(id=method,label=label,warning=warning,params=params,loglik=ll,aic=2*length(best$par)-2*ll,source="拟合",fit_diagnostics=list(convergence=best$convergence,max_abs_score=max(abs(score)),hessian_condition=max(eigenvalues)/min(eigenvalues),starts=length(starts),converged_starts=length(fits),time_scale=scale_t))
}
