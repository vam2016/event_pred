# Counter-clockwise signed stagewise ordering with one interval of continuation.
# W>0 denotes benefit; delta=-log(HR), I=D*p*(1-p).
# Uses only observed-stage information and predeclared boundaries up to that stage.
gs_ordering_tails <- function(delta,information,lower,upper,w) {
  k<-length(information)
  if(k<1||k>5||length(lower)!=k||length(upper)!=k||any(!is.finite(information)|information<=0)||is.unsorted(information,strictly=TRUE)||length(delta)!=1||!is.finite(delta)||length(w)!=1||!is.finite(w)||anyNA(c(lower,upper))||any(lower>=upper))stop("阶段排序需要1–5次递增正信息量、有效原边界及有限效应/Z。")
  # The last column partitions the entire current-stage real line at observed w.
  # It does not use future continuation, boundaries, information or observations.
  dm<-rbind(lower,upper);dm[,k]<-c(-Inf,w)
  dm<-sweep(dm,2,delta*sqrt(information),'-')
  a<-gs_preserve_rng(rpact::getGroupSequentialProbabilities(dm,information/information[k]))
  prev<-if(k>1)seq_len(k-1) else integer()
  high<-sum(a[3,prev]-a[2,prev])+a[3,k]-a[2,k]
  low<-sum(a[1,prev])+a[2,k]
  if(any(!is.finite(c(high,low)))||any(c(high,low) < -1e-7)||any(c(high,low)>1+1e-7)||abs(high+low-1)>1e-7)stop("阶段排序积分未通过概率质量核对。")
  c(upper=pmin(1,pmax(0,high)),lower=pmin(1,pmax(0,low)))
}
gs_ordering_inference <- function(information,lower,upper,w,alpha,sided) {
  if(length(alpha)!=1||!is.finite(alpha)||alpha<=0||alpha>=.5||length(sided)!=1||!sided %in% c('two','benefit'))stop("阶段排序需要有效alpha及获益单侧或双侧方向。")
  tail_alpha<-if(sided=='two')alpha/2 else alpha
  null<-gs_ordering_tails(0,information,lower,upper,w)
  center<-w/sqrt(tail(information,1));width<-8/sqrt(information[1])
  fun<-function(delta)unname(gs_ordering_tails(delta,information,lower,upper,w)['upper'])
  # A common bracket suffices for both confidence tails and the median root.
  for(j in seq_len(12)) {
    left<-center-width;right<-center+width
    if(fun(left)<tail_alpha&&fun(right)>1-tail_alpha)break
    width<-width*2
  }
  if(fun(left)>=tail_alpha||fun(right)<=1-tail_alpha)stop("阶段排序效应区间无法建立求根范围。")
  solve<-function(p)uniroot(function(delta)fun(delta)-p,c(left,right),tol=1e-9,maxiter=150)$root
  roots<-vapply(c(tail_alpha,.5,1-tail_alpha),solve,numeric(1))
  if(any(!is.finite(roots))||is.unsorted(roots))stop("阶段排序效应区间求根失败。")
  residual<-max(abs(vapply(roots,fun,numeric(1))-c(tail_alpha,.5,1-tail_alpha)))
  if(residual>1e-7)stop("阶段排序效应求根未通过尾概率核对。")
  list(adjusted_p=if(sided=='two')pmin(1,2*min(null)) else unname(null['upper']),
    median_unbiased_hr=exp(-roots[2]),adjusted_hr_lower=exp(-roots[3]),adjusted_hr_upper=exp(-roots[1]),
    upper_tail_null=unname(null['upper']),lower_tail_null=unname(null['lower']),
    interval_tail_alpha=tail_alpha,root_probability_error=residual,delta_roots=roots)
}
