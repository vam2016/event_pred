# Equivalent inputs have a single authoritative coordinate, including in groups.
v6 <- list(median=12,shape=1.2,scale=.8,cure=.3,median2=20,shape2=1.5,mix=.4,exp_input="median",exp_rate=log(2)/12,weibull_input="median",eta=12/log(2)^(1/1.2),log_input="median",log_mu=log(12))
for(method in c("exponential","weibull","cure_weibull","mixture_weibull","lognormal","loglogistic")) {
  a <- parameter_input_model(method,v6,"months"); v <- v6
  if(method=="exponential")v$exp_input <- "rate" else if(method %in% c("weibull","cure_weibull","mixture_weibull"))v$weibull_input <- "eta" else v$log_input <- "mu"
  v$median <- 999 # Inactive fields must not change the selected model.
  b <- parameter_input_model(method,v,"months")
  check(near(a$params,b$params,1e-12),paste(method,"equivalent basis ignores inactive median field"))
  for(unit in c("days","weeks")) {
    vv <- convert_unit_inputs(v,"months",unit)
    check(near(a$params,parameter_input_model(method,vv,unit)$params,1e-12),paste(method,unit,"equivalent input is invariant"))
  }
  eq <- model_equivalents(a,"months")
  check(all(is.finite(eq$value)),paste(method,"interpretable equivalent output is finite"))
}
check(fails(parameter_input_model("exponential",list(exp_input="rate",exp_rate=0))),"zero exponential input rate rejected")
check(fails(parameter_input_model("weibull",list(weibull_input="eta",eta=-1,shape=1))),"negative Weibull scale rejected")
check(fails(parameter_input_model("lognormal",list(log_input="mu",log_mu=Inf))),"nonfinite log location rejected")
check(fails(parameter_input_model("exponential",list(exp_input="unknown"))),"unknown event parameter basis rejected")
vdrop6 <- list(drop_input="probability",drop_prob=.02,drop_period=1)
check(near(dropout_input_rate(vdrop6),-log(.98)),"independent dropout probability converts to hazard")
check(near(dropout_input_rate(convert_unit_inputs(vdrop6,"months","days"))*30.4375,-log(.98)),"dropout probability conversion respects time unit")
check(fails(dropout_input_rate(list(drop_input="probability",drop_prob=1,drop_period=1)))&&fails(dropout_input_rate(list(drop_input="probability",drop_prob=.1,drop_period=0))),"infinite dropout rate and zero window rejected")
cm6 <- parameter_input_model("cure_weibull",modifyList(v6,list(cure=.6)),"months")
check(is.infinite(model_equivalents(cm6,"months")$value[4]),"cure overall median is infinite when cure mass exceeds one half")
# Gompertz likelihood reference uses numerical hazard integration, independently
# of the closed-form expression in the engine.
t6 <- c(10,70,210,350); event6 <- c(1,0,1,0)
for(g6 in c(-.003,0,.003)) {
  b6 <- .002
  cum6 <- vapply(t6,function(t)integrate(function(u)b6*exp(g6*u),0,t,rel.tol=1e-12)$value,numeric(1))
  ref6 <- sum(event6*log(b6*exp(g6*t6))-cum6)
  check(near(gompertz_loglik(c(log(b6),g6),t6,event6),ref6,1e-11),paste("Gompertz likelihood vs numerical integration, g=",g6))
}
check(near(gompertz_loglik(c(log(.002),0),t6,event6),sum(ifelse(event6,log(dexp(t6,.002)),log(pexp(t6,.002,lower.tail=FALSE))))),"zero-shape Gompertz is exponential right-censored likelihood")
check(near(gompertz_loglik(c(log(1e-12),1e-12),1e12,1),log(1e-12)+1-expm1(1),1e-12),"small nonzero Gompertz shape retains long-horizon curvature")
# Independent built-in Weibull density/CDF mixture likelihood.
th6 <- c(log(220),log(1.5),qlogis(.3)); sf6 <- .3+.7*pweibull(t6,1.5,220,lower.tail=FALSE)
ref6 <- sum(log(ifelse(event6,.7*dweibull(t6,1.5,220),sf6)))
check(near(cure_weibull_loglik(th6,t6,event6),ref6,1e-11),"cure likelihood matches independent density/survival mixture")
set.seed(4206); n6 <- 1000; z6 <- rexp(n6); trueb6 <- .002; trueg6 <- .003
te6 <- log1p(trueg6*z6/trueb6)/trueg6; dt6 <- runif(n6,200,800)
go6 <- data.frame(time=pmin(te6,dt6),event=as.integer(te6<=dt6)); gm6 <- fit_model(go6,"gompertz")
# Eliminate b analytically, then maximize an independent one-dimensional profile.
profile6 <- function(g) {
  exposure <- sum(vapply(go6$time,function(t)integrate(function(u)exp(g*u),0,t,rel.tol=1e-9)$value,numeric(1)))
  bhat <- sum(go6$event)/exposure
  sum(go6$event)*(log(bhat)-1)+g*sum(go6$event*go6$time)
}
op6 <- optimize(profile6,c(-.01,.01),maximum=TRUE,tol=1e-10)
check(abs(gm6$params[2]-op6$maximum)<1e-7 && abs(gm6$loglik-op6$objective)<1e-6,"Gompertz multi-start MLE matches independent profile optimum")
check(abs(gm6$params[1]/trueb6-1)<.2 && abs(gm6$params[2]-trueg6)<.0006,"Gompertz synthetic parameter recovery")
set.seed(4260); te6 <- ifelse(runif(n6)<.3,Inf,rweibull(n6,1.5,220)); dt6 <- runif(n6,700,1100)
cu6 <- data.frame(time=pmin(te6,dt6),event=as.integer(te6<=dt6)); cw6 <- fit_model(cu6,"cure_weibull")
check(abs(exp(cw6$params[1])/220-1)<.08 && abs(1/cw6$params[2]-1.5)<.12 && abs(cw6$params[3]-.3)<.04,"cure Weibull recovers scale, shape and cure proportion in mature follow-up")
ll6 <- sum(log(ifelse(cu6$event,(1-cw6$params[3])*dweibull(cu6$time,1/cw6$params[2],exp(cw6$params[1])),cw6$params[3]+(1-cw6$params[3])*pweibull(cu6$time,1/cw6$params[2],exp(cw6$params[1]),lower.tail=FALSE))))
check(near(cw6$loglik,ll6,1e-10)&&near(cw6$aic,6-2*ll6),"cure fit loglikelihood and AIC restored to original time coordinate")
for(method in c("gompertz","cure_weibull")) {
  dd <- if(method=="gompertz")go6 else cu6; a <- fit_model(dd,method); dd$time <- dd$time/30.4375; b <- fit_model(dd,method)
  canonical <- if(method=="gompertz") b$params/30.4375 else c(b$params[1]+log(30.4375),b$params[-1])
  check(near(a$params,canonical,1e-7)&&abs(b$loglik-a$loglik-sum(dd$event)*log(30.4375))<1e-6,paste(method,"MLE and density-unit Jacobian are consistent"))
  check(a$fit_diagnostics$convergence==0 && a$fit_diagnostics$max_abs_score<=1e-3 && is.finite(a$fit_diagnostics$hessian_condition),paste(method,"optimization diagnostics retained"))
}
set.seed(4290); boundary6 <- data.frame(time=rweibull(1000,1.5,220),event=1)
check(fails(fit_model(boundary6,"cure_weibull")),"unidentified or boundary cure estimate excluded from prediction")
fc6 <- fit_candidates(boundary6,c("weibull","cure_weibull"))
check(identical(names(fc6$models),"weibull")&&length(fc6$failures)==1,"failed cure candidate reported alongside successful ordinary Weibull")
# Forecast the original risk set after bootstrap refitting, including long tails.
tocohort6 <- function(dd,group=NULL) {
  cut <- 1200; d <- data.frame(id=paste0(group %||% "p",seq_len(nrow(dd))),entry=cut-dd$time,time=dd$time,obs_day=cut,status=ifelse(dd$event==1,"event","active"),event=dd$event)
  if(!is.null(group)) d$group <- group
  d
}
cc6 <- complete_config(cfg); cc6$cut <- 1200; cc6$horizon <- 300; cc6$future_n <- 0; cc6$dropout_rate <- 0; cc6$target <- 800; cc6$sims <- 50; cc6$ensemble <- FALSE; cc6$methods <- c("gompertz","cure_weibull")
for(method in cc6$methods) {
  cc <- cc6; cc$methods <- method; dd <- tocohort6(if(method=="gompertz")go6 else cu6)
  cc$uncertainty <- "plugin"; fp <- run_forecast(dd,cc)
  check(fp$summary$simulations==50 && all(fp$counts[[1]][,1]==sum(dd$event)),paste(method,"pooled fitted forecast retains known event anchor"))
  cc$uncertainty <- "bootstrap"; fb <- run_forecast(dd,cc)
  check(fb$summary$simulations>=45,paste(method,"subject bootstrap refits and predicts"))
}
gd6 <- rbind(tocohort6(go6,"A"),tocohort6(cu6,"B")); gc6 <- cc6; gc6$analysis_mode <- "grouped"; gc6$target <- 1500; gc6$uncertainty <- "bootstrap"
gc6$groups <- list(g1=list(name="A",future_n=0,enroll_rate=.1,dropout_rate=0,multiplier=1,lag=0,fit_method="gompertz"),g2=list(name="B",future_n=0,enroll_rate=.1,dropout_rate=0,multiplier=1,lag=0,fit_method="cure_weibull"))
grf6 <- run_forecast(gd6,gc6)
check(grf6$summary$simulations>=45&&identical(grf6$counts$group_selected,Reduce(`+`,grf6$group_counts$group_selected)),"Gompertz and cure bootstrap group forecasts form the same joint trial totals")
# Direct independent Bernoulli check of conditional cure event probability.
cpc6 <- cc6; cpc6$input_mode <- "parameters"; cpc6$methods <- "cure_weibull"; cpc6$cut <- 100; cpc6$horizon <- 300; cpc6$sims <- 2000; cpc6$target <- 90
cpd6 <- parameter_cohort(100,100,0,100,"fixed")
cpm6 <- parameter_model("cure_weibull",list(median=220*log(2)^(1/1.5),shape=1.5,cure=.3))
cpr6 <- run_forecast(cpd6,cpc6,models_override=list(cure_weibull=cpm6))
p6 <- 1-(.3+.7*pweibull(400,1.5,220,lower.tail=FALSE))/(.3+.7*pweibull(100,1.5,220,lower.tail=FALSE))
check(abs(mean(cpr6$counts[[1]][,121])-100*p6)<4*sqrt(100*p6*(1-p6)/2000),"cure conditional forecast mean matches independent survival ratio")
# UI integration: authoritative rate input, dropout probability, per-group bases.
shiny::testServer(e$server, {
  session$setInputs(time_unit="months",input_mode="parameters",analysis_mode="pooled",cut=0,target=40,horizon=24,active_n=0,known_n=0,duration=0,age_mode="fixed",design_model="exponential",exp_input="rate",exp_rate=log(2)/12,median=999,shape=1.2,future_n=100,enroll_rate=10,drop_input="probability",drop_prob=.02,drop_period=1,dropout_rate=999,multiplier=1,lag=0,clock="occurred",sims=50,seed=42,origin="2025-01-01",cuts="3,6,12",enroll_mode="constant",tail_rate=.06,prior_shape=.5,prior_rate=2)
  session$setInputs(run=1)
  check(is.null(run_error())&&near(result()$models$exponential$params,log(2)/(12*30.4375)),"Shiny selected exponential rate overrides inactive median")
  check(near(result()$config$dropout_rate,-log(.98)/30.4375),"Shiny selected dropout probability overrides inactive risk rate")
  check(nzchar(output$parameter_conversion$html)&&nzchar(output$drop_conversion$html)&&nzchar(output$model_parameters)&&nzchar(output$input_context$html),"parameter previews, equivalent outputs and analysis strip render")
  check(any(equivalent_table()$parameter=="中位时间 m")&&near(equivalent_table()$value[2],12),"result equivalent table preserves successful forecast unit")
  session$setInputs(analysis_mode="grouped",group_count=2,g1_design_model="exponential",g1_exp_input="rate",g1_exp_rate=log(2)/12,g1_median=999,g1_future_n=40,g1_dropout_rate=0,g2_design_model="weibull",g2_weibull_input="eta",g2_eta=18/log(2)^(1/1.2),g2_median=999,g2_shape=1.2,g2_future_n=40,g2_drop_input="probability",g2_drop_prob=.02,g2_drop_period=1)
  session$setInputs(run=2)
  check(is.null(run_error())&&near(result()$group_models$g2$weibull$params[1],log(18*30.4375/log(2)^(1/1.2))),"Shiny group Weibull scale overrides inactive median")
  check(near(result()$config$groups$g2$dropout_rate,-log(.98)/30.4375)&&length(unique(equivalent_table()$group))==2,"per-group dropout conversion and parameter export values retained")
  check(nzchar(output$group_inputs$html)&&nzchar(output$g1_parameter_conversion$html)&&nzchar(output$g2_parameter_conversion$html),"group parameter tabs and separate previews render")
})
ui6 <- as.character(e$event_parameter_fields())
check(all(vapply(c("exp_input","exp_rate","weibull_input","eta","log_input","log_mu"),function(id)grepl(paste("参数说明",id),ui6,fixed=TRUE),logical(1))),"new equivalent event controls have parameter guidance")
ui6 <- as.character(e$dropout_parameter_fields("g1_"))
check(all(vapply(c("g1_drop_input","g1_drop_prob","g1_drop_period"),function(id)grepl(paste("参数说明",id),ui6,fixed=TRUE),logical(1))),"new group dropout controls have parameter guidance")
