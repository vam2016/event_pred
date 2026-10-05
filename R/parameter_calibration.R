# Two point Weibull calibration from specified population survival probabilities.
calibrate_weibull_survival <- function(times, survival, unit="months") {
  if(!is.numeric(times)||length(times)!=2||any(!is.finite(times)|times<=0)||times[2]<=times[1]||
     !is.numeric(survival)||length(survival)!=2||any(!is.finite(survival)|survival<=0|survival>=1)||survival[2]>=survival[1])stop("需两个正递增时点及对应严格递减的(0,1)生存率。")
  t<-times*time_factor(unit);y<-log(-log(survival))
  k<-(y[2]-y[1])/(log(t[2])-log(t[1]));log_eta<-log(t[1])-y[1]/k
  eta<-exp(log_eta);median<-eta*log(2)^(1/k)
  if(!is.finite(k)||k<=0||!is.finite(eta)||eta<=0||!is.finite(median)||median<=0)stop("指定生存率无法构成有限正Weibull参数。")
  m<-parameter_model("weibull",list(median=median,shape=k))
  list(model=m,shape=k,eta_day=eta,median_day=median,
    targets=data.frame(time_day=t,survival=survival),display_unit=unit,
    status="pending_v0.23_review",interpretation="Specified population probabilities; not a patient-data fit")
}
scale_parameter_model <- function(model,time_scale) {
  if(!is.numeric(time_scale)||length(time_scale)!=1||!is.finite(time_scale)||time_scale<=0)stop("生存时间缩放倍数需为有限正数。")
  m<-model;l<-log(time_scale)
  if(m$id=="exponential")m$params[1]<-m$params[1]/time_scale
  else if(m$id %in% c("weibull","cure_weibull","mixture_weibull","lognormal","loglogistic")) {
    m$params[1]<-m$params[1]+l
    if(m$id=="mixture_weibull")m$params[3]<-m$params[3]+l
  } else if(m$id=="gompertz")m$params<-m$params/time_scale
  else if(m$id=="pwe"){m$cuts<-m$cuts*time_scale;m$params<-m$params/time_scale}
  else stop("时间缩放限参数化生成模型。")
  if(any(!is.finite(m$params))||any(!is.finite(m$cuts)))stop("缩放后的模型参数或切点超出数值范围。")
  m$source<-"scaled_parameters";m
}
