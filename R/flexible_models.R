# Flexible/interval prediction engine. No hidden allocation or future ledger in likelihoods.
fx_ids <- c("gengamma","loghaz_spline","blinded_ph","predictive_mixture","rp_hazard","ispline_hazard")
fx_is_model <- function(m)m$id %in% fx_ids
fx_logsubtract <- function(a,b){z<-b-a;out<-a+log(-expm1(pmin(z,0)));out[!is.finite(a)]<--Inf;out}
fx_logsum_matrix <- function(x){if(is.null(dim(x)))return(je_logsum(x));apply(x,1,je_logsum)}
fx_spline_basis <- function(m,t){x<-log1p(pmin(pmax(t,0),m$tail)/m$time_scale);splines::ns(x,knots=log1p(m$knots/m$time_scale),Boundary.knots=c(0,log1p(m$tail/m$time_scale)),intercept=FALSE)}
fx_loghaz <- function(m,t){if(m$id=="loghaz_spline")return(as.numeric(m$params[1]+fx_spline_basis(m,t)%*%m$params[-1]));fx_logdensity(m,t)+fx_cumhaz(m,t)}
fx_spline_integral <- function(m,t){
  # Fixed 16-point Gauss-Legendre per knot interval, constant hazard past tail.
  a<-c(.0950125098376374,.281603550779259,.458016777657227,.617876244402644,.755404408355003,.865631202387832,.944575023073233,.98940093499165)
  w<-c(.189450610455069,.182603415044924,.169156519395003,.149595988816577,.124628971255534,.095158511682493,.062253523938648,.027152459411754);nodes<-c(-rev(a),a);weights<-c(rev(w),w)
  integrate_to<-function(x){if(x<=0)return(0);end<-min(x,m$tail);cuts<-sort(unique(c(0,m$knots[m$knots<end],end)));val<-0;for(j in seq_len(length(cuts)-1)){lo<-cuts[j];hi<-cuts[j+1];times<-(hi+lo)/2+(hi-lo)/2*nodes;val<-val+(hi-lo)/2*sum(weights*exp(fx_loghaz(m,times)))};val+max(0,x-m$tail)*exp(fx_loghaz(m,m$tail))}
  vapply(t,integrate_to,numeric(1))
}
fx_cumhaz <- function(m,t){t<-pmax(t,0);if(m$id %in% c("rp_hazard","ispline_hazard"))return(fs_cumhaz(m,t));switch(m$id,
  gengamma={mu<-m$params[1];s<-m$params[2];Q<-m$params[3];if(abs(Q)<1e-5)-plnorm(t,mu,s,lower.tail=FALSE,log.p=TRUE) else {logy<-Q*(log(t)-mu)/s-2*log(abs(Q));-pgamma(exp(logy),shape=Q^-2,lower.tail=Q<0,log.p=TRUE)}},
  loghaz_spline=fx_spline_integral(m,t),
  blinded_ph={H<-model_cumhaz(m$baseline,t);-log_add(log1p(-m$allocation)-H,log(m$allocation)-m$hr*H)},
  predictive_mixture={x<-sapply(seq_along(m$models),function(j)log(m$weights[j])-model_cumhaz(m$models[[j]],t));if(length(t)==1)x<-matrix(x,nrow=1);-fx_logsum_matrix(x)},stop("未知柔性模型。"))}
fx_logdensity <- function(m,t){if(m$id %in% c("rp_hazard","ispline_hazard"))return(fs_logdensity(m,t));switch(m$id,
  gengamma={mu<-m$params[1];s<-m$params[2];Q<-m$params[3];if(abs(Q)<1e-5)dlnorm(t,mu,s,log=TRUE) else {ly<-Q*(log(t)-mu)/s-2*log(abs(Q));log(abs(Q))-log(s)-log(t)+Q^-2*ly-exp(ly)-lgamma(Q^-2)}},
  loghaz_spline=fx_loghaz(m,t)-fx_cumhaz(m,t),
  blinded_ph={H<-model_cumhaz(m$baseline,t);lf<-fx_density_any(m$baseline,t);log_add(log1p(-m$allocation)+lf,log(m$allocation)+log(m$hr)+lf-(m$hr-1)*H)},
  predictive_mixture={x<-sapply(seq_along(m$models),function(j)log(m$weights[j])+fx_density_any(m$models[[j]],t));if(length(t)==1)x<-matrix(x,nrow=1);fx_logsum_matrix(x)},stop("未知柔性密度。"))}
fx_density_any <- function(m,t){if(fx_is_model(m))return(fx_logdensity(m,t));p<-m$params;switch(m$id,exponential=dexp(t,p[1],log=TRUE),weibull=dweibull(t,1/p[2],exp(p[1]),log=TRUE),lognormal=dlnorm(t,p[1],p[2],log=TRUE),loglogistic=dlogis((log(t)-p[1])/p[2],log=TRUE)-log(p[2])-log(t),pwe=log(ms_model_hazard(m,t))-model_cumhaz(m,t),gompertz=log(p[1])+p[2]*t-model_cumhaz(m,t),stop("密度不支持此候选模型。"))}
fx_inverse <- function(m,z){if(m$id=="gengamma"){Q<-m$params[3];if(abs(Q)<1e-5)return(qlnorm(-z,m$params[1],m$params[2],lower.tail=FALSE,log.p=TRUE));y<-qgamma(-z,shape=Q^-2,lower.tail=Q<0,log.p=TRUE);return(exp(m$params[1]+m$params[2]/Q*(log(y)+2*log(abs(Q)))))}
  vapply(z,function(target){if(target<=0)return(0);if(!is.finite(target))return(Inf);hi<-if(m$id=="loghaz_spline")m$tail else 30;while(model_cumhaz(m,hi)<target){hi<-hi*2;if(!is.finite(hi)||hi>1e15)stop("柔性模型反演无法在有限数值范围内括住分位数。")};uniroot(function(x)model_cumhaz(m,x)-target,c(0,hi),tol=1e-8)$root},numeric(1))}
fx_interval_data <- function(raw,cut,unit="days",blinded=FALSE){fields<-c("USUBJID","PARAMCD","ENTRY","L","R","TYPE","EXIT");if(!is.data.frame(raw)||!all(fields %in% names(raw)))stop("区间CSV需USUBJID,PARAMCD,ENTRY,L,R,TYPE,EXIT；已知组别另需TRTP。");d<-raw[,unique(c(fields,if("TRTP" %in% names(raw))"TRTP")),drop=FALSE]
  f<-time_factor(unit);for(k in c("ENTRY","L","R")){if(all(is.na(d[[k]])))d[[k]]<-as.numeric(d[[k]]);if(!is.numeric(d[[k]]))stop("ENTRY/L/R需数值。");d[[k]]<-d[[k]]*f};d$R[d$TYPE=="right"&is.na(d$R)]<-Inf
  if(!nrow(d)||nrow(d)>5000||anyNA(d[,setdiff(fields,"R")])||anyNA(d$R)||anyDuplicated(d$USUBJID)||any(!nzchar(as.character(d$USUBJID)))||length(unique(d$PARAMCD))!=1)stop("筛选后1–5000人，每人唯一且单一终点，必填非空。")
  if(any(!is.finite(d$ENTRY)|d$ENTRY<0|!is.finite(d$L)|d$L<0)||any(!d$TYPE %in% c("exact","left","interval","right"))||any(!d$EXIT %in% c("event","active","dropout")))stop("TYPE exact/left/interval/right；EXIT event/active/dropout；起点和下界非负有限。")
  e<-d$TYPE!="right";if(any(e&(!is.finite(d$R)|d$R<=0|d$R<d$L))||any(d$TYPE=="exact"&d$L!=d$R)||any(d$TYPE=="left"&d$L!=0)||any(d$TYPE=="interval"&d$R<=d$L)||any(d$TYPE=="right"&!is.infinite(d$R))||any(e!=(d$EXIT=="event")))stop("exact上下界相等；left下界0；interval上下界严格递增；right上界留空/Inf，只有已知事件EXIT=event。")
  latest<-ifelse(e,d$R,d$L);if(any(d$ENTRY+latest>cut+1e-7))stop("事件区间/末次确认不能超出IA；未上报未来事件不能填入输入。");if(blinded)d$TRTP<-"Combined" else if(!"TRTP" %in% names(d)||anyNA(d$TRTP)||any(!nzchar(d$TRTP)))stop("已知组别需TRTP；合并数据请选择盲态或单队列。");d
}
fx_from_adtte <- function(raw,endpoint,cut,blinded=FALSE){fields<-c("USUBJID","PARAMCD","CNSR","ENGINE_ENTRY_DAY","ENGINE_TIME_DAY");if(!all(fields %in% names(raw)))stop("精确ADTTE需连续日ENGINE_ENTRY_DAY/ENGINE_TIME_DAY及标识/终点/CNSR；整数AVAL不反推精确时间。");x<-raw[!is.na(raw$PARAMCD)&raw$PARAMCD==endpoint,];if(anyNA(x[,fields])||any(!x$CNSR %in% 0:2))stop("ADTTE必填无缺失，CNSR0/1/2。");d<-data.frame(USUBJID=x$USUBJID,PARAMCD=x$PARAMCD,ENTRY=x$ENGINE_ENTRY_DAY,L=x$ENGINE_TIME_DAY,R=ifelse(x$CNSR==0,x$ENGINE_TIME_DAY,Inf),TYPE=ifelse(x$CNSR==0,"exact","right"),EXIT=c("event","active","dropout")[x$CNSR+1L]);if(all(c("ENGINE_L_DAY","ENGINE_R_DAY","CNSTYPE") %in% names(x))){d$L<-x$ENGINE_L_DAY;d$R<-x$ENGINE_R_DAY;d$TYPE<-x$CNSTYPE};if("TRTP" %in% names(x))d$TRTP<-x$TRTP;fx_interval_data(d,cut,blinded=blinded)}
fx_record_loglik <- function(m,d){a<--model_cumhaz(m,d$L);v<-a;exact<-d$TYPE=="exact";interval<-d$TYPE %in% c("interval","left");if(any(exact))v[exact]<-fx_density_any(m,d$R[exact]);if(any(interval))v[interval]<-fx_logsubtract(a[interval],-model_cumhaz(m,d$R[interval]));v}
fx_wrap_blind <- function(m,cfg){if(cfg$analysis_mode!="blinded")return(m);list(id="blinded_ph",label=paste0(m$label," · 已指定PH盲态混合"),baseline=m,allocation=cfg$allocation,hr=cfg$assumed_hr,params=numeric())}
fx_model_from_vector <- function(v,id,cfg){switch(id,exponential=list(id=id,label="指数",params=c(rate=exp(v[1]))),weibull=list(id=id,label="Weibull",params=c(location=v[1],scale=exp(v[2]))),gengamma=list(id=id,label="广义Gamma Prentice",params=c(mu=v[1],sigma=exp(v[2]),Q=v[3])),loghaz_spline=list(id=id,label="自然样条log风险+指定恒定尾部",params=v,knots=cfg$knots,tail=cfg$tail,time_scale=cfg$time_scale),stop("候选模型错误。"))}
fx_fit_one <- function(d,id,cfg){if(id=="rp_hazard")return(fs_fit_rp(d,cfg));if(id=="ispline_hazard")return(fs_fit_i(d,cfg));if(sum(d$TYPE!="right")<2)stop("每组拟合至少2条已知事件记录。");ages<-ifelse(d$TYPE=="right",d$L,d$R);mid<-median(ages[ages>0]);r<-sum(d$TYPE!="right")/max(sum(ages),1)
  init<-switch(id,exponential=log(r),weibull=c(log(mid),0),gengamma=c(log(mid),0,.5),loghaz_spline=c(log(r),rep(0,length(cfg$knots)+1)))
  bounds<-switch(id,exponential=list(lo=-25,hi=5),weibull=list(lo=c(-15,-5),hi=c(20,5)),gengamma=list(lo=c(-15,-5,-3),hi=c(20,5,3)),loghaz_spline=list(lo=c(-25,rep(-10,length(init)-1)),hi=c(5,rep(10,length(init)-1))))
  objective<-function(v){m<-fx_wrap_blind(fx_model_from_vector(v,id,cfg),cfg);vals<-tryCatch(fx_record_loglik(m,d),error=function(e)rep(-Inf,nrow(d)));if(any(!is.finite(vals)))1e100 else -sum(vals)}
  starts<-list(init);if(id=="gengamma")starts<-lapply(c(-.5,0,.5),function(Q)c(init[1:2],Q))
  fits<-lapply(starts,function(v)tryCatch(optim(v,objective,method="L-BFGS-B",lower=bounds$lo,upper=bounds$hi,control=list(maxit=1000)),error=function(e)NULL));good<-vapply(fits,function(x)!is.null(x)&&x$convergence==0&&is.finite(x$value)&&x$value<1e99,logical(1));if(!any(good))stop("候选模型未获得有限收敛解。");fit<-fits[[which(good)[which.min(vapply(fits[good],`[[`,numeric(1),"value"))]]];if(any(abs(fit$par-bounds$lo)<1e-5|abs(fit$par-bounds$hi)<1e-5))stop("候选参数落在数值边界，不接受该拟合。");m<-fx_wrap_blind(fx_model_from_vector(fit$par,id,cfg),cfg);m$loglik<--fit$value;m$parameter_count<-length(fit$par);m$aic<-2*length(fit$par)+2*fit$value;m$fit_id<-id;m
}
fx_fit_group <- function(d,cfg){fits<-list();fail<-list();for(id in cfg$candidates){a<-tryCatch(fx_fit_one(d,id,cfg),error=function(e)e);if(inherits(a,"error"))fail[[id]]<-conditionMessage(a) else fits[[id]]<-a};if(!length(fits))stop(paste("全部候选失败：",paste(unlist(fail),collapse="；")))
  weights<-setNames(rep(0,length(fits)),names(fits));weights[1]<-1;fold_records<-data.frame()
  if(cfg$combination=="aic"){a<-vapply(fits,`[[`,numeric(1),"aic");weights<-exp(-.5*(a-min(a)));weights<-weights/sum(weights)}
  if(cfg$combination=="stacking"){
    K<-cfg$folds;if(nrow(d)<K*3)stop("stacking每折至少约3人；增加资料或减少折数。");fold<-integer(nrow(d));for(type in unique(d$TYPE)){ix<-which(d$TYPE==type);fold[ix]<-sample(rep(seq_len(K),length.out=length(ix)))}
    ll<-matrix(NA_real_,nrow(d),length(fits),dimnames=list(NULL,names(fits)))
    for(j in seq_along(fits))for(k in seq_len(K)){a<-tryCatch(fx_fit_one(d[fold!=k,,drop=FALSE],names(fits)[j],cfg),error=function(e)NULL);if(!is.null(a))ll[fold==k,j]<-fx_record_loglik(a,d[fold==k,,drop=FALSE])}
    accepted<-apply(ll,2,function(x)all(is.finite(x)));if(!any(accepted))stop("没有候选在全部预定折上产生有效预测似然。");for(id in names(fits)[!accepted])fail[[id]]<-"至少一个预定折未产生有效held-out似然；不进入stacking。";fits<-fits[accepted];ll<-ll[,accepted,drop=FALSE]
    softmax<-function(v){x<-c(v,0);a<-exp(x-max(x));a/sum(a)}
    if(ncol(ll)==1)weights<-setNames(1,names(fits)) else {opt<-optim(rep(0,ncol(ll)-1),function(v)-sum(fx_logsum_matrix(sweep(ll,2,log(softmax(v)),"+"))),method="L-BFGS-B",lower=rep(-15,ncol(ll)-1),upper=rep(15,ncol(ll)-1));if(opt$convergence!=0||!is.finite(opt$value))stop("stacking权重未收敛。");weights<-setNames(softmax(opt$par),names(fits))}
    fold_records<-batch_bind_columns(lapply(seq_len(ncol(ll)),function(j)data.frame(USUBJID=d$USUBJID,fold=fold,model=colnames(ll)[j],heldout_loglik=ll[,j])))
  }
  if(cfg$combination=="single"){wanted<-cfg$candidates[1];if(!wanted %in% names(fits))stop("预定单模型失败，不能自动换成其他模型。");fits<-fits[wanted];weights<-setNames(1,wanted)}
  m<-if(length(fits)==1)fits[[1]] else list(id="predictive_mixture",label=paste0(cfg$combination,"预测混合"),models=fits,weights=weights,params=numeric())
  list(model=m,candidates=fits,weights=weights,failures=fail,fold_records=fold_records)
}
