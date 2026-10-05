source("R/units.R")
check(near(time_factor("months"),30.4375) && near(time_factor("weeks"),7), "fixed month and week definition")
check(fails(time_factor("years")), "unsupported units rejected")
vals <- list(cut=18,duration=8,median=12,median2=20,horizon=48,age=8,lab_horizon=24,lag=1,recruit_start=0,recruit_end=18,
  prior_rate=2,enroll_prior_rate=1,drop_prior_rate=3,g_rate=.06,g_shape=-.03,tail_rate=.06,enroll_rate=15,dropout_rate=.0075,
  log_eta_mean=log(16),cuts="3,6,12",parameter_cuts="3,6,12",enroll_cuts="3,6",backtest_cuts="9,12",parameter_rates=".03,.06,.09,.06",enroll_rates="9,18,12")
for (unit in c("days","weeks")) {
  rt <- convert_unit_inputs(convert_unit_inputs(vals,"months",unit),unit,"months")
  num_ids <- c(unit_fields$duration,unit_fields$rate,unit_fields$log_time)
  check(all(vapply(num_ids,function(id) near(rt[[id]],vals[[id]]),logical(1))),paste(unit,"all numeric and prior round trips"))
  check(all(vapply(c(unit_fields$duration_text,unit_fields$rate_text),function(id) near(parse_unit_numbers(rt[[id]]),parse_unit_numbers(vals[[id]])),logical(1))),paste(unit,"all comma-list round trips"))
}
check(near(convert_unit_inputs(vals,"months","days")$median,365.25), "12 months equals 365.25 elapsed days")
check(fails(convert_unit_inputs(list(cuts="3,x"),"months","days")), "invalid list not silently converted")
for (unit in c("weeks","months")) {
  ac <- adtte_template(); ac$AVAL <- ac$AVAL/time_factor(unit); ac$AVALU <- toupper(unit)
  dc <- normalize_adtte(ac,"OS","2025-01-01",540,1,2,aval_unit=unit)
  check(near(dc$time,x$time) && near(dc$obs_day,x$obs_day),paste(unit,"ADTTE inclusive time and actual dates"))
  check(fails(normalize_adtte(ac,"OS","2025-01-01",540,1,2)),paste(unit,"file unit mismatch rejected"))
}
base_cfg <- complete_config(cfg); base_cfg$methods <- "weibull"; base_cfg$ensemble <- FALSE; base_cfg$sims <- 50
base_cfg$input_mode <- "parameters"; base_cfg$enroll_mode <- "piecewise"; base_cfg$enroll_cuts <- c(90,180); base_cfg$enroll_rates <- c(.3,.6,.4)
base_model <- parameter_model("weibull",list(median=365.25,shape=1.2)); base_trial <- parameter_cohort(540,100,30,180,"fixed")
base_forecast <- run_forecast(base_trial,base_cfg,models_override=list(weibull=base_model))
for (unit in c("days","weeks","months")) {
  f <- time_factor(unit); cc <- base_cfg
  for (id in c("cut","horizon","cuts","prior_rate","lag","enroll_cuts","recruit_start","recruit_end","enroll_prior_rate","drop_prior_rate")) cc[[id]] <- cc[[id]]/f
  for (id in c("tail_rate","enroll_rate","dropout_rate","enroll_rates")) cc[[id]] <- cc[[id]]*f
  cc$log_eta_mean <- cc$log_eta_mean-log(f); dd <- unit_config_to_days(cc,unit)
  check(near(dd$log_eta_mean,base_cfg$log_eta_mean) && near(dd$prior_rate,base_cfg$prior_rate) && near(dd$enroll_rates,base_cfg$enroll_rates),paste(unit,"prior and rate equivalence"))
  rr <- run_forecast(base_trial,dd,models_override=list(weibull=base_model))
  check(identical(rr$counts,base_forecast$counts) && near(rr$summary$median_day,base_forecast$summary$median_day),paste(unit,"full count trajectories and target dates unchanged"))
  ep <- unit_model_parameters(base_model,unit)
  check(near(ep["eta"]*f,exp(base_model$params["location"])) && near(ep["k"],1.2),paste(unit,"interpretable Weibull output parameters"))
}
ex <- unit_export(data.frame(day=c(0,365.25,Inf)),"months","day")
check(near(ex$day_months,c(0,12,Inf)) && all(ex$time_unit=="months"), "exports retain day and selected unit fields")
precise <- jsonlite::fromJSON(jsonlite::toJSON(list(rate=.0075/30.4375, log_eta=log(450/30.4375)), auto_unbox=TRUE, digits=NA))
check(near(precise$rate,.0075/30.4375,1e-14) && near(precise$log_eta,log(450/30.4375),1e-14), "JSON does not round small rates or transformed priors")
lambda <- log(2)/12; mu <- -log(.98); v <- 6; prob <- lambda/(lambda+mu)*(1-exp(-(lambda+mu)*v))
check(abs(prob-.2768017)<1e-7, "methodology competing dropout example")
new_exp <- 15*lambda/(lambda+mu)*(v-(1-exp(-(lambda+mu)*v))/(lambda+mu))
check(abs(new_exp-13.42369)<1e-5, "methodology Poisson enrollment example")
w <- parameter_model("weibull",list(median=12,shape=1.2))
check(abs((sample_conditional(w,8,u=.5)-8)-9.889565)<1e-6, "methodology Weibull conditional median example")
app_source <- paste(readLines("app.R",warn=FALSE),collapse="\n")
ids <- regmatches(app_source,gregexpr('(numericInput|textInput|selectInput|dateInput|radioButtons|checkboxInput|checkboxGroupInput|fileInput)\\("[a-z_0-9]+"',app_source))[[1]]
ids <- unique(sub('.*\\("','',ids)); ids <- sub('"$','',ids)
check(all(ids %in% names(e$parameter_help)) && all(nchar(e$parameter_help)>=40), "all static and dynamic parameters have substantive explanation and guidance")
check(grepl('selected = "months"',app_source,fixed=TRUE), "UI starts in months")
check(grepl('type="math/tex">R_i</script>',e$handbook_html(),fixed=TRUE), "inline method math delimiters survive Markdown rendering")
shiny::testServer(e$server, {
  session$setInputs(time_unit="months",input_mode="parameters",cut=18,target=100,horizon=24,active_n=100,known_n=30,
    duration=8,age_mode="fixed",design_model="weibull",median=12,shape=1.2,future_n=30,enroll_rate=15,dropout_rate=.0075,
    multiplier=1,lag=0,clock="occurred",sims=50,seed=42,origin="2025-01-01",cuts="3,6,12",parameter_cuts="3,6,12",
    parameter_rates=".03,.06,.09,.06",enroll_mode="constant",enroll_cuts="3,6",enroll_rates="9,18,12",tail_rate=.06,
    prior_shape=.5,prior_rate=2,lab_model="weibull",age=8,lab_horizon=24,lab_multiplier=1)
  session$setInputs(run=1)
  check(is.null(run_error()) && near(result()$config$cut,18*30.4375), "Shiny monthly input builds canonical day forecast")
  check(result()$config$display_unit=="months" && near(result()$config$cohort_parameters$duration,8*30.4375), "Shiny saves selected unit and actual current ages")
  check(nzchar(output$model_parameters) && nzchar(output$conditional_quantiles) && nzchar(output$data_table), "unit-aware model and conditional and data tables render")
  old <- result(); session$setInputs(time_unit="weeks")
  check(identical(old,result()), "unit switch alone preserves successful forecast snapshot")
})
