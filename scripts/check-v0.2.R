# ADTTE mapping, calendar conventions, gaps and file encodings.
a <- adtte_template()
x <- normalize_adtte(a, "OS", "2025-01-01", 540, 1, 2)
check(near(x$obs_day, as.numeric(as.Date(a$ADT) - as.Date("2025-01-01"))), "inclusive AVAL preserves actual calendar dates")
check(all(x$status[a$CNSR == 0] == "event") && all(x$status[a$CNSR == 2] == "dropout"), "CNSR polarity and permanent exit mapping")
check(fails(normalize_adtte(a, "OS", "2025-01-01", 540, 0, 2)), "wrong inclusive convention rejected")
bad <- a; bad$AVAL[1] <- bad$AVAL[1] + 1
check(fails(normalize_adtte(bad, "OS", "2025-01-01", 540, 1, 2)), "inconsistent AVAL and dates rejected")
bad <- rbind(a, a[1, ]); bad$ANL01FL[nrow(bad)] <- "N"
check(fails(normalize_adtte(bad, "OS", "2025-01-01", 540, 1, 2)), "duplicate analysis records require explicit selection")
check(nrow(normalize_adtte(bad, "OS", "2025-01-01", 540, 1, 2, "ANL01FL")) == nrow(a), "analysis flag yields unique record per subject")
bad <- a; bad$AVALU <- "MONTHS"
check(fails(normalize_adtte(bad, "OS", "2025-01-01", 540, 1, 2)), "non-day units not guessed")
bad <- a; bad$CNSR[1] <- -1
check(fails(normalize_adtte(bad, "OS", "2025-01-01", 540, 1, 2)), "invalid censor code rejected")
sas <- a; sas$STARTDT <- as.numeric(as.Date(a$STARTDT) - as.Date("1960-01-01")); sas$ADT <- as.numeric(as.Date(a$ADT) - as.Date("1960-01-01"))
check(identical(normalize_adtte(sas, "OS", "2025-01-01", 540, 1, 2, date_encoding = "sas")$obs_day, x$obs_day), "SAS numeric date conversion")
check(fails(adtte_date("2025-02-30")), "invalid calendar date rejected")
check(as.character(adtte_date("2024-02-29")) == "2024-02-29", "leap day accepted")
if (requireNamespace("haven", quietly = TRUE)) {
  for (format in c("xpt", "sas7bdat")) {
    tmp <- tempfile(fileext = paste0(".", format))
    aa <- a; aa$STARTDT <- as.Date(aa$STARTDT); aa$ADT <- as.Date(aa$ADT)
    if (format == "xpt") haven::write_xpt(aa, tmp) else haven::write_sas(aa, tmp)
    z <- normalize_adtte(read_adtte(tmp), "OS", "2025-01-01", 540, 1, 2)
    check(near(z$obs_day, x$obs_day), paste(format, "read and date normalization")); unlink(tmp)
  }
}
gap <- x; i <- which(gap$status == "active")[1]; gap$time[i] <- gap$time[i] - 100; gap$obs_day[i] <- gap$obs_day[i] - 100
check(fails(validate_data(gap, 540)), "gaps require explicit mode")
check(!fails(validate_data(gap, 540, gap_mode = "impute")), "gap imputation retains last confirmed age")
cc <- complete_config(cfg); cc$gap_mode <- "impute"; cc$methods <- "exponential"; cc$sims <- 50; cc$ensemble <- FALSE
rg <- run_forecast(gap, cc)
check(all(rg$counts$exponential[, 1] >= sum(gap$status == "event")), "imputed cutoff can include latent interval events")
# Analytic latent interval event probability, with no artificial survival extension.
one <- data.frame(id = "A", entry = 0, time = 10, obs_day = 10, status = "active")
tc <- complete_config(cfg); tc$cut <- 50; tc$future_n <- 0; tc$dropout_rate <- 0; tc$lag <- 0
set.seed(314)
latent <- replicate(12000, sum(simulate_trial(one, exp_model, tc)$occurred <= 50))
p <- 1 - exp(-.01 * 40)
check(abs(mean(latent) - p) < 4 * sqrt(p * (1 - p) / length(latent)), "gap event probability agrees with conditional exponential law")

# All supplied parameter models; cure mass and mixture conditioning.
pp <- list(median = 365, shape = 1.2, scale = .8, cure = .3, median2 = 650, shape2 = 1.5, mix = .4, rate = .002, rates = c(.001, .002, .003))
for (method in parameter_catalog()$id) {
  p <- pp; if (method == "gompertz") p$shape <- .001
  m <- parameter_model(method, p, c(90, 180))
  t <- c(0, 50, 180, 365, 1000)
  s <- model_survival(m, t)
  check(near(s[1], 1) && all(diff(s) <= 0), paste(method, "parameter survival monotonicity"))
  check(near(inverse_cumhaz(m, model_cumhaz(m, t)), t, 1e-6), paste(method, "parameter hazard inverse"))
}
cm <- parameter_model("cure_weibull", pp)
check(near(model_survival(cm, 1e8), .3) && is.infinite(inverse_cumhaz(cm, -log(.3) + .01)), "cure model preserves nonzero survival mass")
set.seed(819)
ag <- rep(500, 10000); generated <- sample_conditional(cm, ag)
cond_cure <- .3 / model_survival(cm, 500)
check(abs(mean(is.infinite(generated)) - cond_cure) < 4 * sqrt(cond_cure * (1 - cond_cure) / 10000), "cure class probability updates conditional on survival age")
mm <- parameter_model("mixture_weibull", pp)
z <- sample_conditional(mm, c(0, 100, 300), u = c(.2, .5, .8))
check(near(model_survival(mm, z) / model_survival(mm, c(0, 100, 300)), c(.2, .5, .8), 1e-6), "mixture conditional law does not reuse unconditional mixing weights")
gm <- parameter_model("gompertz", list(rate = .002, shape = -.001))
check(near(model_survival(gm, 1e8), exp(-2)) && is.infinite(inverse_cumhaz(gm, 2.1)), "negative Gompertz shape keeps infinite event mass")

empty <- parameter_cohort(0, 0, 0, 0)
check(nrow(empty) == 0 && all(c("id", "entry", "time", "obs_day", "status") %in% names(empty)), "zero-current-cohort parameter design")
pc <- complete_config(cfg); pc$cut <- 0; pc$input_mode <- "parameters"; pc$methods <- "weibull"; pc$sims <- 50; pc$future_n <- 200; pc$ensemble <- FALSE
pm <- parameter_model("weibull", pp)
rdesign <- run_forecast(empty, pc, models_override = list(weibull = pm))
check(all(rdesign$counts$weibull[, 1] == 0) && rdesign$potential_events == 200, "startup forecast uses supplied model without fitting")
current <- parameter_cohort(540, 100, 30, 180, "fixed")
check(near(current$time[current$status == "active"], rep(180, 100)) && all(current$obs_day == 540), "aggregate current cohort preserves specified counts and follow-up")
pcc <- pc; pcc$cut <- 540; pcc$future_n <- 0
rc <- run_forecast(current, pcc, models_override = list(weibull = pm))
check(all(rc$counts$weibull[, 1] == 30), "aggregate historical count not resimulated")
pc$enroll_mode <- "piecewise"; pc$enroll_cuts <- c(50, 100); pc$enroll_rates <- c(.5, 1, 0)
rstop <- run_forecast(empty, pc, models_override = list(weibull = pm))
check(max(rstop$counts$weibull) < pc$future_n, "zero recruitment tail stops enrollment")

# Backtest leakage and observed comparison.
base <- validate_data(demo_data(), 540)
early <- snapshot_at(base, 270)
changed <- base; ix <- which(changed$obs_day > 270 & changed$status == "event")[1]
changed$obs_day[ix] <- 400; changed$time[ix] <- 400 - changed$entry[ix]
check(identical(snapshot_at(changed, 270)[, c("id", "entry", "time", "obs_day", "status")], early[, c("id", "entry", "time", "obs_day", "status")]), "late event date changes cannot leak into reconstructed early snapshot")
bc <- complete_config(cfg); bc$methods <- c("exponential", "weibull"); bc$sims <- 50; bc$horizon <- 100; bc$future_n <- 30
bt <- backtest_forecast(base, bc, c(270, 365), 540, plan = data.frame(cut = c(270,365), future_n = c(30,30), enroll_rate = c(.5,.5)))
check(nrow(bt$summary) == 6 && all(bt$summary$brier >= 0 & bt$summary$brier <= 1), "rolling cut backtest and Brier score")
check(all(bt$curves$day <= 540), "backtest never evaluates beyond observed truth cutoff")
check(near(bt$summary$error, bt$summary$predicted_events - bt$summary$observed_events), "backtest count error uses observed final events")
check(fails(backtest_forecast(base, bc, 600, 540)), "future backtest cut rejected")

# Process conjugacy uses recruitment exposure and observed risk exposure only.
bc$process_uncertainty <- "gamma"; bc$recruit_start <- 0; bc$recruit_end <- 540
proc <- process_posterior(base, bc)
check(near(proc$enroll, c(1 + nrow(base), 1 + 540)) && near(proc$dropout, c(.5 + sum(base$status == "dropout"), 50 + sum(base$time))), "entry/dropout conjugate sufficient statistics")
set.seed(315); rates <- replicate(10000, process_draw(bc, proc)$enroll_rate)
check(abs(mean(rates) - proc$enroll[1] / proc$enroll[2]) < .002, "entry posterior draws match analytical mean")
rr <- run_forecast(base, bc)
check(!is.null(rr$process_posterior), "joint process parameter draws integrated into forecasts")

# Independent quadrature reference for the Weibull posterior.
mc <- complete_config(cfg); mc$mcmc_warmup <- 1000; mc$mcmc_draws <- 4000
set.seed(51); bm <- fit_bayesian_weibull(base, mc)
check(bm$posterior$passed, "four-chain Weibull MCMC passes rank R-hat and bulk/tail ESS thresholds")
check(all(bm$posterior$acceptance > .1 & bm$posterior$acceptance < .7), "MCMC retained acceptance rates in numerical check range")
eta_grid <- seq(5.7, 6.4, length.out = 151); k_grid <- seq(-.1, .5, length.out = 151)
grid <- expand.grid(log_eta = eta_grid, log_shape = k_grid)
lp <- apply(grid, 1, function(th) weibull_log_posterior(th, base, bm$posterior$prior)); w <- exp(lp - max(lp)); w <- w / sum(w)
qmean <- colSums(as.matrix(grid) * w)
mcm <- bm$posterior$diagnostics$mean
check(max(abs(qmean - mcm)) < .015, "MCMC posterior mean agrees with independent two-dimensional quadrature")
bc$uncertainty <- "bayes_weibull"; bc$methods <- "weibull"; bc$mcmc_draws <- 4000
rb <- run_forecast(base, bc)
check(rb$models$weibull$posterior$passed && nrow(rb$models$weibull$posterior_params) == 16000, "event and entry/dropout posterior predictive forecast")
short <- mc; short$mcmc_draws <- 200
set.seed(51); bm_short <- fit_bayesian_weibull(base, short)
check(!bm_short$posterior$passed, "insufficient MCMC diagnostics remain failed instead of silently accepted")

for (method in parameter_catalog()$id) {
  p <- pp; if (method == "gompertz") p$shape <- -.001
  m <- parameter_model(method, p, c(90, 180))
  c3 <- pc; c3$methods <- method; c3$enroll_mode <- "constant"; c3$future_n <- 30; c3$target <- 20
  z <- run_forecast(empty, c3, models_override = setNames(list(m), method))
  check(nrow(z$summary) == 1 && all(z$curves$median >= 0 & z$curves$median <= 30), paste(method, "parameter-only trial forecast"))
}
check(near(x$time, a$AVAL - 1), "inclusive ADTTE AVAL converted to continuous elapsed time")
bad <- a; bad$ADT[1] <- bad$STARTDT[1]; bad$AVAL[1] <- 1
check(fails(normalize_adtte(bad, "OS", "2025-01-01", 540, 1, 2)), "same-day risk origin record rejected by explicit continuous-time rule")
bc2 <- complete_config(cfg); bc2$methods <- c("exponential", "pwe"); bc2$sims <- 50; bc2$horizon <- 100
btx <- backtest_forecast(base, bc2, 270, 540, plan = data.frame(cut = 270, future_n = bc2$future_n, enroll_rate = bc2$enroll_rate))
check(any(btx$failures$method == "pwe") && any(btx$summary$method == "exponential"), "backtest preserves unavailable model reasons while evaluating fitted models")
