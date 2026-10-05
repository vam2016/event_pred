# The calculation engine uses elapsed days. A month is a fixed duration,
# not a variable-length calendar month; calendar dates are never rounded here.
time_factor <- function(unit = "months") {
  factors <- c(days = 1, weeks = 7, months = 365.25 / 12)
  if (length(unit) != 1 || is.na(unit) || !unit %in% names(factors)) stop("时间单位需为日、周或月。")
  unname(factors[unit])
}
time_label <- function(unit = "months") {
  time_factor(unit)
  unname(c(days = "日", weeks = "周", months = "月")[unit])
}
unit_fields <- list(
  duration = c("cut", "duration", "median", "median2", "eta", "drop_period", "prior_rate", "enroll_prior_rate", "drop_prior_rate", "lag", "recruit_start", "recruit_end", "horizon", "age", "lab_horizon"),
  duration_text = c("cuts", "parameter_cuts", "enroll_cuts", "backtest_cuts"),
  rate = c("g_rate", "g_shape", "exp_rate", "tail_rate", "enroll_rate", "dropout_rate"),
  rate_text = c("parameter_rates", "enroll_rates", "backtest_enroll_rates"), log_time = c("log_eta_mean","log_mu"))
base_unit_fields <- unit_fields
for (kind in names(unit_fields)) {
  group_ids <- intersect(base_unit_fields[[kind]],c("duration","median","median2","eta","drop_period","log_mu","exp_rate","g_rate","g_shape","cuts","tail_rate","parameter_cuts","parameter_rates","enroll_rate","enroll_cuts","enroll_rates","dropout_rate","lag","backtest_enroll_rates"))
  if(length(group_ids)) unit_fields[[kind]] <- c(unit_fields[[kind]],unlist(lapply(1:6,function(i)paste0("g",i,"_",group_ids)),use.names=FALSE))
}
parse_unit_numbers <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(numeric())
  y <- suppressWarnings(as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]])))
  if (anyNA(y) || any(!is.finite(y))) stop("逗号分隔的参数必须全部为有限数值。")
  y
}
convert_unit_inputs <- function(values, from, to) {
  ratio <- time_factor(from) / time_factor(to)
  out <- values
  for (id in intersect(unit_fields$duration, names(values))) out[[id]] <- values[[id]] * ratio
  for (id in intersect(unit_fields$rate, names(values))) out[[id]] <- values[[id]] / ratio
  for (id in intersect(unit_fields$log_time, names(values))) out[[id]] <- values[[id]] + log(ratio)
  # Do not round stored values: a unit round trip must preserve the assumptions.
  for (id in intersect(unit_fields$duration_text, names(values))) out[[id]] <- paste(format(parse_unit_numbers(values[[id]]) * ratio, digits = 16, trim = TRUE), collapse = ",")
  for (id in intersect(unit_fields$rate_text, names(values))) out[[id]] <- paste(format(parse_unit_numbers(values[[id]]) / ratio, digits = 16, trim = TRUE), collapse = ",")
  out
}
unit_config_to_days <- function(cfg, unit) {
  f <- time_factor(unit)
  durations <- c("cut", "horizon", "cuts", "prior_rate", "lag", "enroll_cuts", "recruit_start", "recruit_end", "enroll_prior_rate", "drop_prior_rate")
  rates <- c("tail_rate", "enroll_rate", "dropout_rate", "enroll_rates")
  for (id in intersect(durations, names(cfg))) cfg[[id]] <- cfg[[id]] * f
  for (id in intersect(rates, names(cfg))) cfg[[id]] <- cfg[[id]] / f
  if (!is.null(cfg$log_eta_mean)) cfg$log_eta_mean <- cfg$log_eta_mean + log(f)
  cfg$display_unit <- unit; cfg$days_per_unit <- f; cfg$engine_unit <- "days"
  cfg
}
unit_model_parameters <- function(m, unit) {
  f <- time_factor(unit); p <- m$params
  if (m$id %in% c("exponential", "pwe", "gompertz")) {
    p <- p * f
    if (is.null(names(p))) names(p) <- paste0("lambda", seq_along(p))
  } else if (m$id %in% c("weibull", "cure_weibull", "mixture_weibull")) {
    p <- c(eta = exp(p["location"]) / f, k = 1 / p["scale"], p[setdiff(names(p), c("location", "scale", "location2", "scale2"))])
    names(p)[1:2] <- c("eta", "k")
    if (m$id == "mixture_weibull") p <- c(p, eta2 = unname(exp(m$params["location2"]) / f), k2 = unname(1 / m$params["scale2"]))
  } else if (m$id %in% c("lognormal", "loglogistic")) {
    p <- c(mu = unname(p["location"] - log(f)), sigma = unname(p["scale"]))
  } else p <- c(tail_rate = unname(m$params["tail_rate"]) * f)
  p
}
# Exports retain canonical day fields and add explicitly named selected-unit fields.
unit_export <- function(data, unit, columns) {
  f <- time_factor(unit)
  for (id in intersect(columns, names(data))) data[[paste0(id, "_", unit)]] <- data[[id]] / f
  data$time_unit <- rep(unit, nrow(data)); data$days_per_unit <- rep(f, nrow(data))
  data
}
unit_table <- function(data, unit, columns) {
  for (id in intersect(columns, names(data))) {
    data[[id]] <- data[[id]] / time_factor(unit)
    names(data)[names(data) == id] <- paste0(id, "（", time_label(unit), "）")
  }
  data
}
