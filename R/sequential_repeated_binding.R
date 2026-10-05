# Repeated inference for a binding design uses a separately calibrated positive
# boundary shape. Original efficacy/futility decisions remain frozen.
gs_shape_crossing <- function(scale,shape,timing,two=FALSE) {
  if(length(scale)!=1||!is.finite(scale)||scale<0||any(!is.finite(shape)|shape<=0))stop("重复推断需要正且有限的原效力边界形状。")
  if(scale==0)return(if(two)1 else 1)
  lower<-if(two)-scale*shape else rep(-Inf,length(shape));upper<-scale*shape
  a<-gs_preserve_rng(rpact::getGroupSequentialProbabilities(rbind(lower,upper),timing))
  p<-sum(a[3,]-a[2,])+if(two)sum(a[1,]) else 0
  if(!is.finite(p)||p< -1e-7||p>1+1e-7)stop("重复边界概率积分无效。")
  min(1,max(0,p))
}
gs_shape_scale <- function(probability,shape,timing,two=FALSE) {
  hi<-1;for(i in 1:20){if(gs_shape_crossing(hi,shape,timing,two)<probability)break;hi<-hi*2}
  if(gs_shape_crossing(hi,shape,timing,two)>=probability)stop("无法建立重复边界校准范围。")
  uniroot(function(s)gs_shape_crossing(s,shape,timing,two)-probability,c(0,hi),tol=1e-9)$root
}
gs_binding_repeated <- function(cfg,plan,path) {
  shape<-plan$upper_z;timing<-plan$information_fraction
  if(any(!is.finite(shape)|shape<=0))stop("原效力边界非正，当前边界形状重复推断不适用。")
  scale<-gs_shape_scale(2*cfg$alpha,shape,timing,TRUE)
  information<-path$events*cfg$treatment_fraction*(1-cfg$treatment_fraction)
  delta_low<-(path$benefit_z-scale*shape[seq_len(nrow(path))])/sqrt(information)
  delta_high<-(path$benefit_z+scale*shape[seq_len(nrow(path))])/sqrt(information)
  lo<-cummax(delta_low);hi<-cummin(delta_high);empty<-lo>hi
  m<-cummax(path$benefit_z/shape[seq_len(nrow(path))])
  p<-vapply(m,function(x)if(x<=0)1 else gs_shape_crossing(x,shape,timing,FALSE),numeric(1))
  list(p=p,hr_lower=ifelse(empty,NA,exp(-hi)),hr_upper=ifelse(empty,NA,exp(-lo)),empty=empty,scale=scale,
    coverage_crossing=gs_shape_crossing(scale,shape,timing,TRUE),engine="full_plan_shape_band_prefix_intersection")
}
