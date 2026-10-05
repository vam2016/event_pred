source("R/models.R"); source("R/forecast.R"); source("R/inputs.R"); source("R/bayes.R"); source("R/validation.R")
args <- commandArgs(trailingOnly = TRUE)
reps <- if (length(args)) as.integer(args[1]) else 30L
if (!is.finite(reps) || reps < 2 || reps > 1000) stop("repetitions must be 2–1000")
dir.create("validation", showWarnings = FALSE)
all <- list()
for (mode in c("plugin", "gamma", "bootstrap")) {
  cat("Calibration:", mode, "\n")
  x <- calibration_experiment(reps, uncertainty = mode)
  x$uncertainty <- mode; all[[mode]] <- x
}
x <- do.call(rbind, all)
write.csv(x, "validation/calibration_raw.csv", row.names = FALSE)
success <- x[x$failure == "", ]
groups <- split(success, interaction(success$scenario, success$uncertainty, success$model, drop = TRUE))
s <- do.call(rbind, lapply(groups, function(z) {
  cov <- mean(z$count_covered)
  n <- nrow(z); zz <- qnorm(.975); center <- (cov + zz^2 / (2 * n)) / (1 + zz^2 / n)
  half <- zz * sqrt(cov * (1 - cov) / n + zz^2 / (4 * n^2)) / (1 + zz^2 / n)
  data.frame(scenario = z$scenario[1], uncertainty = z$uncertainty[1], model = z$model[1], trials = nrow(z),
    bias = mean(z$error), mae = mean(abs(z$error)), rmse = sqrt(mean(z$error^2)), count_coverage = cov,
    coverage_mcse = sqrt(cov * (1 - cov) / nrow(z)), coverage_lower = center - half, coverage_upper = center + half, mean_interval_width = mean(z$count_interval_width),
    mean_brier = mean(z$brier), finite_target_errors = sum(is.finite(z$target_error)),
    target_mae = if (any(is.finite(z$target_error))) mean(abs(z$target_error), na.rm = TRUE) else NA_real_)
}))
write.csv(s, "validation/calibration_summary.csv", row.names = FALSE)
metadata <- list(version = "0.2.0", generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  repetitions_per_scenario = reps, simulations_per_fit = 300, truth_seed = 20261005,
  scenarios = 5, modes = c("plugin", "gamma", "bootstrap"), failures = sum(x$failure != ""),
  model_failures_in_successful_runs = "See runtime diagnostics; this raw file records trial-level failures only.",
  enrollment = "planned 200; 1 subject every 2 days; forecast Poisson rate=0.5/day",
  observed_cut = 300, evaluation_cut = 700, target = 100, event_median = 350,
  note = "Synthetic internal calibration; small repetitions and no external validation. All candidate-model draws may differ from generating family. Recruitment truth is regular spacing; forecasting assumes Poisson arrivals.",
  R = R.version.string)
jsonlite::write_json(metadata, "validation/calibration_config.json", auto_unbox = TRUE, pretty = TRUE)
cat("Saved", nrow(x), "results, trial-level failures:", metadata$failures, "\n")
