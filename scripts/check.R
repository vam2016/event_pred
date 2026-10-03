source("R/models.R")
source("R/forecast.R")
checks <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop(paste("FAILED:", label))
  checks <<- checks + 1L
  cat(sprintf("PASS %02d: %s\n", checks, label))
}
near <- function(x, y, tol = 1e-8) isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tol))
fails <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
cfg <- list(cut = 540, horizon = 730, sims = 100, target = 180, future_n = 30,
  enroll_rate = 0.5, dropout_rate = 0.00025, lag = 0, multiplier = 1, seed = 42,
  prior_shape = 0.5, prior_rate = 50, tail_rate = 0.002, uncertainty = "plugin",
  clock = "occurred", methods = model_catalog()$id, cuts = c(90, 180, 365), ensemble = TRUE,
  origin = "2025-01-01")
d <- validate_data(demo_data(), cfg$cut)
check(all(d$entry + d$time <= cfg$cut + 1e-8), "synthetic data obey cut")
bad <- d; bad$entry[1] <- NA
check(fails(validate_data(bad, cfg$cut)), "missing entry rejected")
bad <- d; bad$status[1] <- "unknown"
check(fails(validate_data(bad, cfg$cut)), "ambiguous censoring status rejected")
bad <- d; bad$time[which(bad$status == "active")[1]] <- 1
check(fails(validate_data(bad, cfg$cut)), "stale active follow-up rejected")
bad <- cfg; bad$sims <- 49
check(fails(validate_config(bad, d)), "invalid simulation count rejected")
bad <- cfg; bad$uncertainty <- "gamma"
check(fails(validate_config(bad, d)), "unsupported conjugate models rejected")

for (method in model_catalog()$id) {
  m <- fit_model(d, method, cfg$cuts, cfg$tail_rate)
  t <- seq(0, 2000, length.out = 100)
  s <- model_survival(m, t)
  check(near(s[1], 1) && all(diff(s) <= 1e-10) && all(s >= 0 & s <= 1), paste(method, "proper monotone survival"))
  age <- c(0, 100, 200)
  u <- c(0.2, 0.5, 0.8)
  x <- sample_conditional(m, age, u = u)
  check(all(x > age), paste(method, "conditional samples exceed observed ages"))
  if (method != "km_tail") {
    check(near(exp(-(model_cumhaz(m, x) - model_cumhaz(m, age))), u), paste(method, "inverse conditional survival identity"))
    check(near(inverse_cumhaz(m, model_cumhaz(m, t)), t), paste(method, "cumulative hazard inverse round trip"))
    x2 <- sample_conditional(m, age, multiplier = 0.7, u = u)
    check(all(x2 >= x), paste(method, "reduced future hazard delays events"))
  }
}
exp_model <- list(id = "exponential", params = c(rate = 0.01), events = 3, exposure = 100)
check(near(sample_conditional(exp_model, c(0, 100), u = c(0.5, 0.5)) - c(0, 100), rep(log(2) / .01, 2)), "exponential memoryless identity")
pwe <- list(id = "pwe", params = c(0.1, 0.2, 0), cuts = c(10, 20))
check(near(model_cumhaz(pwe, c(10, 20, 30)), c(1, 3, 3)), "PWE cumulative hazard at boundaries")
check(near(inverse_cumhaz(pwe, c(0, 1, 2, 3)), c(0, 10, 15, 20)), "PWE inverse at boundaries")
check(is.infinite(inverse_cumhaz(pwe, 4)), "zero-risk PWE tail retains infinite event time")
boundary_data <- data.frame(time = c(10, 20, 30), event = c(1, 1, 0))
m <- fit_model(boundary_data, "pwe", cuts = c(10, 20))
check(near(m$events, c(1, 1, 0)) && near(m$exposure, c(30, 20, 10)), "PWE event/person-time attribution at cutpoints")
check(fails(fit_model(boundary_data, "pwe", cuts = c(10, 40))), "empty PWE interval rejected")
set.seed(17)
post <- replicate(10000, gamma_posterior_draw(exp_model, 2, 50)$params[1])
check(abs(mean(post) - 5 / 150) < 0.0008, "Gamma posterior mean matches analytical conjugate update")
check(near(quantile_with_inf(c(1, Inf)), c(1, 1, Inf)), "empirical inverse CDF preserves beyond-horizon quantiles")

r <- run_forecast(d, cfg)
check(length(r$counts) == 7, "all six candidate models and predictive ensemble run")
check(all(vapply(r$counts, function(x) all(x[, 1] == sum(d$event)), logical(1))), "known events fixed at current cut")
check(all(vapply(r$counts, function(x) all(apply(x, 1, function(z) all(diff(z) >= 0))), logical(1))), "all event trajectories are monotone")
check(all(vapply(r$counts, function(x) max(x) <= r$potential_events, logical(1))), "dropouts never resurrect; enrollment cap respected")
check(all(r$summary$reached >= 0 & r$summary$reached <= 1), "milestone probabilities bounded")
check(near(sum(r$weights$weight), 1) && !"km_tail" %in% r$weights$method, "AIC weights normalized and exclude KM")
r2 <- run_forecast(d, cfg)
check(identical(r$counts, r2$counts), "seed makes complete forecast reproducible")
impossible <- cfg; impossible$target <- r$potential_events + 1; impossible$sims <- 50
ri <- run_forecast(d, impossible)
check(all(ri$summary$reached == 0) && all(is.infinite(ri$summary$median_day)), "unreachable target remains in probability denominator")
past <- cfg; past$target <- 2; past$sims <- 50; past$methods <- "exponential"
rp <- run_forecast(d, past)
known_second <- sort(d$entry[d$event == 1] + d$time[d$event == 1])[2]
check(all(rp$summary$reached == 1) && all(rp$summary$median_day == known_second), "already-achieved milestone fixed to observed date")
lagged <- cfg; lagged$methods <- "exponential"; lagged$clock <- "reported"; lagged$lag <- 90; lagged$ensemble <- FALSE
rl <- run_forecast(d, lagged)
unlagged <- lagged; unlagged$lag <- 0
ru <- run_forecast(d, unlagged)
check(all(rl$counts$exponential <= ru$counts$exponential) && all(rl$counts$exponential[, 1] == sum(d$event)), "future reporting lag delays only future observed events")
for (mode in c("bootstrap", "gamma")) {
  c2 <- cfg; c2$uncertainty <- mode; c2$methods <- c("exponential", "pwe"); c2$sims <- 50
  rr <- run_forecast(d, c2)
  check(all(rr$summary$simulations >= 45), paste(mode, "forecast completes with successful draws"))
}
for (method in c("weibull", "lognormal", "loglogistic", "km_tail")) {
  c2 <- cfg; c2$uncertainty <- "bootstrap"; c2$methods <- method; c2$sims <- 50
  rr <- run_forecast(d, c2)
  check(all(rr$summary$simulations >= 45), paste(method, "bootstrap works on original cohort"))
}

# Independent analytical check of competing event/dropout probabilities.
small <- data.frame(id = c("e1", "e2", paste0("a", 1:100)), entry = 0,
  time = c(2, 3, rep(10, 100)), status = c("event", "event", rep("active", 100)))
cs <- cfg; cs$cut <- 10; cs$horizon <- 30; cs$sims <- 2000; cs$target <- 50
cs$future_n <- 0; cs$methods <- "exponential"; cs$dropout_rate <- 0.01; cs$ensemble <- FALSE
rs <- run_forecast(small, cs)
lambda <- 2 / sum(small$time); mu <- cs$dropout_rate
prob <- lambda / (lambda + mu) * (1 - exp(-(lambda + mu) * cs$horizon))
expected <- 2 + 100 * prob
mcse <- sqrt(100 * prob * (1 - prob) / cs$sims)
check(abs(tail(rs$curves$mean, 1) - expected) < 4 * mcse, "Monte Carlo event count agrees with independent competing-risk calculation")

# Exercise actual Shiny server and exported/rendered result snapshots.
e <- new.env(parent = globalenv())
invisible(sys.source("app.R", envir = e))
html <- as.character(shiny::includeMarkdown("docs/METHODS.md"))
check(!grepl("(?<!\\$)\\$[^$\\n]+\\$(?!\\$)", html, perl = TRUE), "inline method formulas converted for MathJax")
shiny::testServer(e$server, {
  session$setInputs(run = 1, cut = 540, target = 180, horizon = 730, methods = c("exponential", "weibull", "pwe"),
    cuts = "90,180,365", tail_rate = 0.002, uncertainty = "plugin", prior_shape = 0.5, prior_rate = 50,
    ensemble = TRUE, future_n = 30, enroll_rate = 0.5, dropout_rate = 0.00025, multiplier = 1,
    lag = 0, clock = "occurred", sims = 50, seed = 42, origin = "2025-01-01", file = NULL,
    lab_model = "weibull", age = 180, lab_horizon = 730, lab_multiplier = 1)
  check(!is.null(result()) && is.null(run_error()), "Shiny prediction action produces result")
  check(nzchar(output$event_plot) && nzchar(output$prob_plot) && nzchar(output$fit_plot) && nzchar(output$conditional_plot), "four interactive chart renderers complete")
  old <- result()
  session$setInputs(target = 999)
  check(identical(old, result()), "input edits preserve last successful result snapshot")
  session$setInputs(run = 2, cut = 100)
  check(!is.null(run_error()) && identical(old, result()), "invalid run shows failure and preserves previous result")
})
cat(sprintf("\nAll %d checks passed.\n", checks))
