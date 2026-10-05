# Independently verifiable known-group predictions.
graw <- adtte_template(); graw$TRTA <- ifelse(graw$TRTP=="A","B","A")
ga <- normalize_adtte(graw,"OS","2025-01-01",540,1,2,group_column="TRTP")
gb <- normalize_adtte(graw,"OS","2025-01-01",540,1,2,group_column="TRTA")
check(identical(ga$group,graw$TRTP)&&identical(gb$group,graw$TRTA),"selected planned or actual group mapping honored without fallback")
gbad <- graw; gbad$TRTA[1] <- NA
check(fails(normalize_adtte(gbad,"OS","2025-01-01",540,1,2,group_column="TRTA")),"missing selected group rejected despite valid other treatment column")
check(fails(normalize_adtte(graw,"OS","2025-01-01",540,1,2,group_column="UNKNOWN")),"unknown grouping variable rejected")
gcfg <- complete_config(cfg); gcfg$analysis_mode <- "grouped"; gcfg$methods <- c("exponential","weibull"); gcfg$sims <- 100; gcfg$future_n <- 0; gcfg$uncertainty <- "plugin"
gcfg$groups <- list(g1=list(name="A",future_n=0,enroll_rate=.5,enroll_mode="constant",dropout_rate=0,multiplier=1,lag=0,cuts=c(90,180),tail_rate=.002),g2=list(name="B",future_n=0,enroll_rate=.2,enroll_mode="constant",dropout_rate=.001,multiplier=1,lag=0,cuts=c(90,180),tail_rate=.002))
gr <- run_forecast(ga,gcfg)
check(length(gr$group_models)==2 && all(c("exponential","weibull","ensemble") %in% names(gr$counts)),"known-group candidate and direct AIC combination predictions complete")
check(all(gr$weights$weight>0) && all(abs(tapply(gr$weights$weight,gr$weights$group,sum)-1)<1e-10),"AIC weights normalized independently within each group")
for(id in names(gr$counts)) {
  check(identical(gr$counts[[id]],Reduce(`+`,gr$group_counts[[id]])),paste(id,"total count equals sum of same joint-trial group counts"))
  check(all(vapply(gr$event_dates[[id]],function(dates)all(diff(dates)>=0),logical(1))),paste(id,"joint event dates sorted"))
  actual_hit <- vapply(gr$event_dates[[id]],function(dates) if(length(dates)>=gcfg$target && dates[gcfg$target]<=gcfg$cut+gcfg$horizon) dates[gcfg$target] else Inf,numeric(1))
  check(near(gr$milestones[[id]],actual_hit),paste(id,"milestone is exact target-th merged event date"))
}
check(all(vapply(gr$counts,function(mat) all(mat[,1]==sum(ga$event)),logical(1))),"grouped known events fixed at cutoff")
for(key in names(gcfg$groups)) {
  dd <- ga[ga$group==gcfg$groups[[key]]$name,]
  check(near(gr$group_models[[key]]$exponential$params, sum(dd$event)/sum(dd$time)),paste(key,"fit uses only that group's sufficient statistics"))
}
gcfg2 <- gcfg; gcfg2$groups$g2$name <- "A"
check(fails(run_forecast(ga,gcfg2)),"duplicate configured group names rejected")
gcfg2 <- gcfg; gcfg2$groups$g1$future_n <- 1001
check(fails(run_forecast(ga,gcfg2)),"group and total future enrollment limits enforced")
missing_group <- ga; missing_group$group[1] <- "C"
check(fails(run_forecast(missing_group,gcfg)),"unconfigured observed group not silently dropped")
# Heterogeneous exponential groups: independent Poisson-binomial mean/variance.
pg <- gcfg; pg$input_mode <- "parameters"; pg$uncertainty <- "plugin"; pg$ensemble <- FALSE; pg$cut <- 10; pg$horizon <- 30; pg$sims <- 2000; pg$methods <- "exponential"; pg$target <- 50
pd <- data.frame(id=c(paste0("a",1:100),paste0("b",1:80)),entry=0,time=10,obs_day=10,status="active",group=c(rep("A",100),rep("B",80)))
pg$groups$g1$method <- "exponential"; pg$groups$g2$method <- "exponential"; pg$groups$g1$dropout_rate <- .01; pg$groups$g2$dropout_rate <- .005
mo <- list(g1=list(exponential=parameter_model("exponential",list(median=log(2)/.02))),g2=list(exponential=parameter_model("exponential",list(median=log(2)/.04))))
pr <- run_forecast(pd,pg,models_override=mo)
prob1 <- .02/.03*(1-exp(-.03*30)); prob2 <- .04/.045*(1-exp(-.045*30))
expect <- 100*prob1+80*prob2; var_expected <- 100*prob1*(1-prob1)+80*prob2*(1-prob2)
end_counts <- tail(pr$counts$group_parameters,1) # Actual independent replications are rows.
vals <- pr$counts$group_parameters[,121]
check(abs(mean(vals)-expect)<4*sqrt(var_expected/pg$sims),"heterogeneous-group mean agrees with independent competing-dropout formula")
check(abs(var(vals)/var_expected-1)<.12,"heterogeneous-group variance agrees with sum of independent Bernoulli variances")
check(abs(cor(pr$group_counts$group_parameters$g1[,121],pr$group_counts$group_parameters$g2[,121]))<.1,"group draws do not share identical random trajectories")
check(identical(pr$counts,run_forecast(pd,pg,models_override=mo)$counts),"joint grouped forecast reproducible by seed")
# Fixed allocations via separate caps; different distributions per group.
pc5 <- pg; pc5$cut <- 0; pc5$sims <- 50; pc5$groups$g1$future_n <- 20; pc5$groups$g2$future_n <- 30; pc5$groups$g2$method <- "weibull"; pc5$methods <- c("exponential","weibull")
ed5 <- parameter_cohort(0,0,0,0); ed5$group <- character()
mo5 <- mo; mo5$g2 <- list(weibull=parameter_model("weibull",list(median=40,shape=1.2)))
rr5 <- run_forecast(ed5,pc5,models_override=mo5)
check(rr5$potential_events==50 && max(rr5$group_counts$group_parameters$g1)<=20 && max(rr5$group_counts$group_parameters$g2)<=30,"startup separate group models and fixed allocation caps respected")
# All base parameter distributions work within known groups.
params5 <- list(median=365,shape=1.2,scale=.8,cure=.3,median2=650,shape2=1.5,mix=.4,rate=.002,rates=c(.001,.002,.003))
for(method in parameter_catalog()$id) {
  pp5 <- params5; if(method=="gompertz") pp5$shape <- .001
  m5 <- parameter_model(method,pp5,c(90,180)); tc5 <- pc5; tc5$methods <- method; for(key in names(tc5$groups)) tc5$groups[[key]]$method <- method
  check(!fails(run_forecast(ed5,tc5,models_override=list(g1=setNames(list(m5),method),g2=setNames(list(m5),method)))),paste(method,"known-group parameter simulation"))
}
# Bootstrap and Gamma retain group-specific event/process models.
for(mode in c("bootstrap","gamma")) {
  gc5 <- gcfg; gc5$sims <- 50; gc5$methods <- if(mode=="gamma")c("exponential","pwe") else c("exponential","weibull"); gc5$uncertainty <- mode; gc5$process_uncertainty <- "gamma"
  br5 <- run_forecast(ga,gc5)
  check(all(br5$summary$simulations>=45),paste(mode,"grouped uncertainty completes"))
  check(near(br5$group_process_posterior$g1$enroll["shape"],gc5$enroll_prior_shape+sum(ga$group=="A")),paste(mode,"process posterior uses own-group enrollment"))
}
# Historical plans must cover every cut/group; current caps are not substituted.
hp5 <- expand.grid(group=c("A","B"),cut=c(270,365),stringsAsFactors=FALSE); hp5$future_n <- c(60,40,20,10); hp5$enroll_rate <- c(.25,.25,.15,.15)
bc5 <- gcfg; bc5$sims <- 50; bc5$methods <- "exponential"; bc5$ensemble <- FALSE
bt5 <- backtest_forecast(ga,bc5,c(270,365),540,plan=hp5)
check(near(bt5$summary$planned_future_n,c(100,30)),"grouped backtest uses sum of per-cut historical group caps")
check(fails(backtest_forecast(ga,bc5,c(270,365),540,plan=hp5[-1,])),"incomplete group/cut historical plan rejected")
bc52 <- bc5; bc52$groups$g1$future_n <- 100; bc52$groups$g2$future_n <- 200
check(identical(bt5$curves,backtest_forecast(ga,bc52,c(270,365),540,plan=hp5)$curves),"grouped historical forecast unaffected by current group caps")
ss5 <- run_sensitivity(pd,pg,c(.5,1,2),models_override=mo)
check(ss5$median_events_end[1]<=ss5$median_events_end[2] && ss5$median_events_end[2]<=ss5$median_events_end[3],"risk sensitivity applies to each group's baseline multiplier")
# Generated UI help and unit fields must cover group controls.
ui5 <- as.character(e$group_input_card(1,"A","parameters","months"))
check(grepl("参数说明 g1_median",ui5,fixed=TRUE)&&grepl("参数说明 g1_future_n",ui5,fixed=TRUE),"dynamic group parameters include explanations")
gvals5 <- list(g1_median=12,g2_median=18,g1_enroll_rate=7.5,g2_dropout_rate=.0075,g1_enroll_rates="4.5,9,6",g2_parameter_cuts="3,6,12")
check(near(convert_unit_inputs(gvals5,"months","days")$g1_median,365.25)&&near(convert_unit_inputs(gvals5,"months","days")$g1_enroll_rate,7.5/30.4375),"group input durations and rates convert to day scale")
rt5 <- convert_unit_inputs(convert_unit_inputs(gvals5,"months","weeks"),"weeks","months")
check(near(rt5$g2_median,18)&&near(parse_unit_numbers(rt5$g1_enroll_rates),c(4.5,9,6)),"group inputs retain values through unit round trip")
different5 <- gcfg; different5$groups$g1$fit_method <- "exponential"; different5$groups$g2$fit_method <- "lognormal"
fixed5 <- run_forecast(ga,different5)
check(identical(names(fixed5$counts),"group_selected")&&identical(names(fixed5$group_models$g2),"lognormal"),"data groups can use different fixed distribution families without mixing")
check(near(fixed5$counts$group_selected,Reduce(`+`,fixed5$group_counts$group_selected)),"different fixed group models form one coherent total trajectory")
check(near(gr$curves$probability_mcse[gr$curves$method=="ensemble"],sqrt(gr$curves$probability[gr$curves$method=="ensemble"]*(1-gr$curves$probability[gr$curves$method=="ensemble"])/gcfg$sims)),"fresh grouped AIC joint paths use single-stage probability MCSE")
six5 <- pc5; six5$methods <- "exponential"; six5$groups <- setNames(lapply(1:6,function(i){g <- pc5$groups$g1; g$name <- LETTERS[i]; g$future_n <- 5; g$method <- "exponential"; g}),paste0("g",1:6)); sm5 <- setNames(rep(list(mo$g1),6),names(six5$groups))
sr5 <- run_forecast(ed5,six5,models_override=sm5)
check(nrow(sr5$group_summary)==6 && sr5$potential_events==30 && identical(sr5$counts$group_parameters,Reduce(`+`,sr5$group_counts$group_parameters)),"six-group parameter forecast respects all group caps and total counts")
shiny::testServer(e$server, {
  session$setInputs(time_unit="months",input_mode="parameters",analysis_mode="grouped",group_count=2,cut=0,target=30,horizon=24,sims=50,seed=42,origin="2025-01-01",
    design_model="weibull",cuts="3,6,12",tail_rate=.06,prior_shape=.5,prior_rate=2,enroll_mode="constant",enroll_rate=15,future_n=300,dropout_rate=.0075,multiplier=1,lag=0,clock="occurred",
    g1_name="对照组",g2_name="试验组",g1_future_n=30,g2_future_n=30,g1_design_model="exponential",g2_design_model="weibull",g1_median=12,g2_median=18,g2_shape=1.2,g1_enroll_rate=7.5,g2_enroll_rate=7.5)
  session$setInputs(run=1)
  check(is.null(run_error())&&result()$config$analysis_mode=="grouped", "Shiny parameter forecast runs with distinct known-group models")
  check(identical(vapply(result()$config$groups,`[[`,character(1),"name"),c(g1="对照组",g2="试验组")),"Shiny group names saved in result snapshot")
  check(nzchar(output$group_event_plot)&&nzchar(output$group_event_table)&&nzchar(output$model_parameters)&&nzchar(output$fit_plot),"Shiny group event results and survival plots render")
  old <- result(); session$setInputs(g1_median=24)
  check(identical(old,result()),"editing group inputs preserves successful result until rerun")
  session$setInputs(g2_name="对照组",run=2)
  check(!is.null(run_error())&&identical(old,result()),"invalid grouped run keeps previous successful result")
  tmp <- tempfile(fileext=".csv"); write.csv(adtte_template(),tmp,row.names=FALSE)
  session$setInputs(input_mode="adtte",group_column="TRTP",paramcd="OS",offset="1",dropout_codes="2",date_encoding="iso",analysis_flag="",flag_value="Y",gap_mode="strict",aval_unit="days",file=list(datapath=tmp,name="groups.csv"),cut=540/30.4375,methods=c("exponential","weibull"),uncertainty="plugin",ensemble=TRUE,process_uncertainty="fixed",g1_future_n=0,g2_future_n=0,g1_cuts="3,6,12",g2_cuts="3,6,12")
  session$setInputs(run=3)
  check(is.null(run_error())&&length(result()$group_models)==2,"Shiny ADTTE known-group fitting completes")
  check(all(nzchar(output$group_backtest_inputs))&&nzchar(output$fit_plot),"Shiny creates matching historical group plan controls and empirical KM curves")
  session$setInputs(backtest_cuts=as.character(270/30.4375),g1_backtest_future_n="50",g2_backtest_future_n="40",g1_backtest_enroll_rates="7.5",g2_backtest_enroll_rates="7.5",run_backtest=1)
  check(is.null(backtest_error())&&nrow(backtest()$plan)==2,"Shiny backtest passes a separate historical plan for each group")
  session$setInputs(g1_fit_method="exponential",g2_fit_method="lognormal",run=31)
  check(is.null(run_error())&&identical(names(result()$counts),"group_selected"),"Shiny fits explicitly different group distribution families")
  session$setInputs(g1_fit_method="",g2_fit_method="",process_uncertainty="gamma",recruit_start=0,recruit_end=540/30.4375,enroll_prior_shape=1,enroll_prior_rate=1/30.4375,drop_prior_shape=.5,drop_prior_rate=50/30.4375,run=4)
  check(is.null(run_error())&&nzchar(output$process_table),"Shiny displays each group's enrollment and dropout posterior")
  session$setInputs(methods="weibull",uncertainty="bayes_weibull",mcmc_chains=4,mcmc_warmup=1000,mcmc_draws=4000,log_eta_mean=log(450/30.4375),log_eta_sd=1,log_shape_mean=0,log_shape_sd=.75,run=5)
  check(is.null(run_error())&&all(vapply(result()$group_models,function(x)x$weibull$posterior$passed,logical(1))),"each known-group Weibull posterior independently passes chain diagnostics")
  session$setInputs(posterior_model="g2__weibull")
  check(nzchar(output$posterior_table)&&nzchar(output$trace_plot),"Shiny posterior selection displays the requested group")
  session$setInputs(lab_model="g1__weibull",age=8,lab_horizon=24,lab_multiplier=1)
  check(nzchar(output$conditional_plot),"conditional survival page uses requested group model")
  unlink(tmp)
})
