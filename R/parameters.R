# Equivalent parameterizations. Values here use the currently selected display unit.
parameter_input_model <- function(method,v,unit="months") {
  f <- time_factor(unit); get <- function(id,default=NULL) if(is.null(v[[id]])) default else v[[id]]
  basis <- if(method=="exponential") get("exp_input","median") else if(method %in% c("weibull","cure_weibull","mixture_weibull")) get("weibull_input","median") else if(method %in% c("lognormal","loglogistic")) get("log_input","median") else "native"
  allowed <- if(method=="exponential")c("median","rate") else if(method %in% c("weibull","cure_weibull","mixture_weibull"))c("median","eta") else if(method %in% c("lognormal","loglogistic"))c("median","mu") else "native"
  if(length(basis)!=1||!basis %in% allowed) stop("未知参数输入形式。")
  median <- get("median",12)
  if(method=="exponential" && basis=="rate") {
    rate <- get("exp_rate"); if(length(rate)!=1||!is.finite(rate)||rate<=0) stop("指数风险率需为正数。")
    median <- log(2)/rate
  }
  if(method %in% c("weibull","cure_weibull","mixture_weibull") && basis=="eta") {
    eta <- get("eta"); k <- get("shape",1.2)
    if(length(eta)!=1||!is.finite(eta)||eta<=0||!is.finite(k)||k<=0) stop("Weibull尺度eta和形状k需为正数。")
    median <- eta*log(2)^(1/k)
  }
  if(method %in% c("lognormal","loglogistic") && basis=="mu") {
    mu <- get("log_mu"); if(length(mu)!=1||!is.finite(mu)) stop("对数时间位置mu需为有限数。")
    median <- exp(mu)
  }
  p <- list(median=median*f,shape=get("shape",1.2),scale=get("scale",.8),cure=get("cure",.2),median2=get("median2",20)*f,shape2=get("shape2",1.2),mix=get("mix",.5),rate=get("g_rate",.06)/f,rates=if(method=="pwe")parse_unit_numbers(get("parameter_rates",".03,.06,.09,.06"))/f else numeric())
  if(method=="gompertz") p$shape <- get("g_shape",.03)/f
  m <- parameter_model(method,p,if(method=="pwe")parse_unit_numbers(get("parameter_cuts","3,6,12"))*f else numeric())
  m$input_basis <- basis; m
}
dropout_input_rate <- function(v) {
  if(is.null(v$drop_input)||v$drop_input=="rate") return(v$dropout_rate)
  if(v$drop_input!="probability") stop("未知脱落参数输入形式。")
  p <- v$drop_prob; dt <- v$drop_period
  if(length(p)!=1||length(dt)!=1||!is.finite(p)||!is.finite(dt)||p<0||p>=1||dt<=0) stop("脱落概率需在[0,1)，对应时间窗口需为正数。")
  -log1p(-p)/dt
}
model_equivalents <- function(m,unit="months") {
  f <- time_factor(unit); p <- m$params; rows <- list()
  add <- function(label,value,units,definition) rows[[length(rows)+1]] <<- data.frame(parameter=label,value=unname(value),unit=units,definition=definition)
  u <- time_label(unit); med <- inverse_cumhaz(m,log(2))/f
  if(m$id=="exponential") {
    add("风险率 lambda",p[1]*f,paste0("1/",u),"恒定事件风险"); add("中位时间 m",med,u,"S(m)=0.5"); add("均值",1/p[1]/f,u,"完整事件分布均值")
  } else if(m$id %in% c("weibull","cure_weibull","mixture_weibull")) {
    eta <- exp(p[1])/f; k <- 1/p[2]
    add("尺度 eta",eta,u,"第一成分；治愈模型为未治愈成分"); add("形状 k",k,"无量纲","Weibull形状"); add("成分中位时间",eta*log(2)^(1/k),u,"不含治愈或第二成分"); add("总体中位时间 m",med,u,"总体S(m)=0.5；可为无穷")
    if(m$id=="cure_weibull") add("治愈比例 pi",p[3],"比例","总体长期生存质量")
    if(m$id=="mixture_weibull") { add("第二成分尺度 eta2",exp(p[3])/f,u,"第二成分Weibull尺度"); add("第二成分形状 k2",1/p[4],"无量纲","第二成分Weibull形状"); add("第二成分中位时间",exp(p[3])/f*log(2)^p[4],u,"第二成分S2(m2)=0.5"); add("第一成分比例",p[5],"比例","分布成分比例") }
  } else if(m$id %in% c("lognormal","loglogistic")) {
    add("对数时间位置 mu",p[1]-log(f),paste0("log(",u,")"),"mu=log(m/所选单位)"); add("对数时间尺度 sigma",p[2],"无量纲","不是原始时间标准差"); add("中位时间 m",med,u,"exp(mu)")
    mean <- if(m$id=="lognormal") exp(p[1]+p[2]^2/2)/f else if(p[2]<1) exp(p[1])/f*pi*p[2]/sin(pi*p[2]) else Inf
    add("均值",mean,u,"Log-logistic仅sigma<1时均值有限")
  } else if(m$id=="gompertz") {
    add("初始风险 b",p[1]*f,paste0("1/",u),"h(0)"); add("风险变化 g",p[2]*f,paste0("1/",u),"h(t)=b exp(g t)"); add("中位时间 m",med,u,"若累计风险达不到log(2)，中位数无穷"); add("无穷事件时间质量",if(p[2]<0) exp(p[1]/p[2]) else 0,"比例","形状<0时可能非零")
  } else {
    for(j in seq_along(p)) add(paste0(if(m$id=="pwe")"分段风险 lambda" else "尾部风险",j),p[j]*f,paste0("1/",u),"对应模型风险率")
    add("中位时间 m",med,u,"广义逆生存分位数")
  }
  do.call(rbind,rows)
}
