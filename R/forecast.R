demo_data <- function(n = 240, cut = 540, seed = 20261003) {
  set.seed(seed)
  entry <- sort(runif(n, 0, cut - 14))
  t <- rweibull(n, shape = 1.15, scale = 420)
  dropout <- rexp(n, 0.00025)
  admin <- cut - entry
  time <- pmin(t, dropout, admin)
  status <- ifelse(t <= dropout & t <= admin, "event", ifelse(dropout < t & dropout < admin, "dropout", "active"))
  data.frame(id = sprintf("DEMO%04d", seq_len(n)), entry = entry, time = time, status = status)
}

fit_candidates <- function(data, methods, cuts, tail_rate) {
  models <- list(); failures <- character()
  for (method in methods) {
    m <- tryCatch(fit_model(data, method, cuts, tail_rate), error = function(e) e)
    if (inherits(m, "error")) failures[method] <- conditionMessage(m) else models[[method]] <- m
  }
  if (!length(models)) stop(paste("没有模型成功拟合：", paste(failures, collapse = "；")))
  list(models = models, failures = failures)
}

validate_config <- function(cfg, data) {
  cfg <- complete_config(cfg)
  scalar <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  for (field in c("cut", "horizon", "sims", "target", "future_n", "enroll_rate", "dropout_rate", "lag", "multiplier", "seed", "prior_shape", "prior_rate", "tail_rate")) {
    if (!scalar(cfg[[field]])) stop(paste("配置必须为有限单值：", field))
  }
  if (cfg$horizon <= 0 || cfg$horizon > 3650) stop("预测窗口必须在 0–3650 天内。")
  if (cfg$sims < 50 || cfg$sims > 2000 || cfg$sims != floor(cfg$sims)) stop("模拟次数必须为 50–2000 的整数。")
  if (cfg$target < 1 || cfg$target != floor(cfg$target)) stop("目标事件数必须为正整数。")
  if (cfg$future_n < 0 || cfg$future_n > 1000 || cfg$future_n != floor(cfg$future_n)) stop("未来入组人数必须为 0–1000 的整数。")
  if (cfg$enroll_rate < 0 || (cfg$analysis_mode != "grouped" && cfg$future_n > 0 && cfg$enroll_rate == 0 && cfg$enroll_mode == "constant" && cfg$process_uncertainty == "fixed")) stop("恒定率入组时，每天入组率必须大于 0。")
  if (cfg$dropout_rate < 0 || cfg$lag < 0 || cfg$multiplier <= 0) stop("脱落率及上报延迟必须非负，风险倍数必须为正。")
  if (cfg$prior_shape <= 0 || cfg$prior_rate <= 0 || cfg$tail_rate <= 0) stop("先验参数及 KM 尾部风险必须为正。")
  if (!cfg$uncertainty %in% c("plugin", "bootstrap", "gamma", "bayes_weibull")) stop("未知不确定性方式。")
  if (!cfg$clock %in% c("occurred", "reported")) stop("未知事件计数时钟。")
  if (cfg$seed < 0 || cfg$seed > .Machine$integer.max || cfg$seed != floor(cfg$seed)) stop("随机种子必须在整数范围内。")
  allowed <- if (cfg$input_mode == "parameters") parameter_catalog()$id else model_catalog()$id
  if (length(cfg$methods) < 1 || any(!cfg$methods %in% allowed)) stop("请至少选择一个支持的模型。")
  if (cfg$uncertainty == "gamma" && any(!cfg$methods %in% c("exponential", "pwe"))) stop("Gamma 后验仅支持指数/PWE，请调整所选模型。")
  if (cfg$uncertainty == "bayes_weibull" && !identical(cfg$methods, "weibull")) stop("Weibull MCMC 只使用 Weibull 模型。")
  if (cfg$input_mode == "parameters" && (cfg$uncertainty != "plugin" || cfg$process_uncertainty != "fixed")) stop("参数输入模式使用固定参数，不拟合数据后验。")
  if (!cfg$gap_mode %in% c("strict", "impute")) stop("未知随访间隔模式。")
  if (!cfg$process_uncertainty %in% c("fixed", "gamma")) stop("未知未来过程参数方式。")
  if (!cfg$enroll_mode %in% c("constant", "piecewise")) stop("未知入组率方式。")
  if (cfg$process_uncertainty == "gamma") {
    vals <- c(cfg$enroll_prior_shape, cfg$enroll_prior_rate, cfg$drop_prior_shape, cfg$drop_prior_rate)
    if (any(!is.finite(vals) | vals <= 0)) stop("入组/脱落 Gamma 先验参数需为正。")
    if (cfg$enroll_mode != "constant") stop("入组后验抽样仅支持恒定入组率。")
  }
  if (cfg$enroll_mode == "piecewise") parameter_model("pwe", list(rates = cfg$enroll_rates), cfg$enroll_cuts)
  if (nrow(data) * cfg$sims * length(cfg$methods) > 2e7) stop("数据与模拟组合过大，请减少模型、受试者或模拟次数。")
  invisible(TRUE)
}

quantile_with_inf <- function(x, probs = c(0.025, 0.5, 0.975)) as.numeric(quantile(x, probs, type = 1, names = FALSE))

# Conditional Monte Carlo error: fixed fitted parameters / fixed posterior bank.
# Mixtures include both independent model-bank error and resampling error.
probability_mcse <- function(counts, target, weights = NULL, resample_n = NULL) {
  probabilities <- lapply(counts, function(x) colMeans(x >= target))
  n <- vapply(counts, nrow, integer(1))
  if (is.null(weights)) {
    p <- probabilities[[1]]
    return(sqrt(p * (1 - p) / n[[1]]))
  }
  if (is.null(names(weights)) || !all(names(weights) %in% names(counts))) stop("MCSE 权重与模型不匹配。")
  if (length(resample_n) != 1 || resample_n < 1) stop("MCSE 需要重抽样次数。")
  ids <- names(weights)
  p <- Reduce(`+`, lapply(ids, function(id) weights[[id]] * probabilities[[id]]))
  bank_var <- Reduce(`+`, lapply(ids, function(id)
    weights[[id]]^2 * probabilities[[id]] * (1 - probabilities[[id]]) / (n[[id]] - 1)))
  sqrt(pmax(0, p * (1 - p) / resample_n + bank_var))
}

simulate_trial <- function(data, model, cfg) {
  known <- data$obs_day[data$status == "event"]
  active <- data[data$status == "active", , drop = FALSE]
  if (nrow(active)) {
    total_time <- sample_conditional(model, active$time, cfg$multiplier)
    event_date <- active$obs_day + total_time - active$time
    remaining_dropout <- if (cfg$dropout_rate == 0) rep(Inf, nrow(active)) else rexp(nrow(active), cfg$dropout_rate)
    event_date <- event_date[event_date <= active$obs_day + remaining_dropout]
  } else event_date <- numeric()
  if (cfg$future_n > 0) {
    enroll <- if (cfg$enroll_mode == "piecewise") {
      em <- parameter_model("pwe", list(rates = cfg$enroll_rates), cfg$enroll_cuts)
      cfg$cut + inverse_cumhaz(em, cumsum(rexp(cfg$future_n)))
    } else cfg$cut + cumsum(rexp(cfg$future_n, cfg$enroll_rate))
    future_event <- sample_conditional(model, rep(0, cfg$future_n), cfg$multiplier)
    future_dropout <- if (cfg$dropout_rate == 0) rep(Inf, cfg$future_n) else rexp(cfg$future_n, cfg$dropout_rate)
    event_date <- c(event_date, (enroll + future_event)[future_event <= future_dropout])
  }
  # Known events are already observed at cut. Lag applies only to future events.
  event_date <- event_date[is.finite(event_date)]
  occurred <- sort(c(known, event_date))
  reported <- sort(c(known, event_date + cfg$lag))
  list(occurred = occurred, reported = reported)
}

complete_config <- function(cfg) {
  if(identical(cfg$task,"count")&&is.null(cfg$target))cfg$target<-1
  defaults <- list(analysis_mode = "pooled", input_mode = "adtte", gap_mode = "strict", process_uncertainty = "fixed", enroll_mode = "constant",
    recruit_start = 0, recruit_end = cfg$cut, enroll_prior_shape = 1, enroll_prior_rate = 1,
    drop_prior_shape = 0.5, drop_prior_rate = 50, mcmc_chains = 4, mcmc_warmup = 1000, mcmc_draws = 2000,
    log_eta_mean = log(450), log_eta_sd = 1, log_shape_mean = 0, log_shape_sd = 0.75)
  for (name in names(defaults)) if (is.null(cfg[[name]])) cfg[[name]] <- defaults[[name]]
  cfg
}

run_forecast <- function(data, cfg, progress = function(value, detail) NULL, models_override = NULL) {
  cfg <- complete_config(cfg)
  if (identical(cfg$analysis_mode, "grouped")) return(run_grouped_forecast(data,cfg,progress,models_override))
  data <- validate_data(data, cfg$cut, require_events = cfg$input_mode != "parameters", gap_mode = cfg$gap_mode)
  validate_config(cfg, data)
  set.seed(cfg$seed)
  fit <- if (!is.null(models_override)) list(models = models_override, failures = character()) else fit_candidates(data, cfg$methods, cfg$cuts, cfg$tail_rate)
  models <- fit$models
  if (cfg$uncertainty == "bayes_weibull") {
    progress(0, "Weibull MCMC")
    models$weibull <- fit_bayesian_weibull(data, cfg)
    dg <- models$weibull$posterior$diagnostics
    if (!models$weibull$posterior$passed) stop(paste("MCMC 诊断未通过：", paste(sprintf("%s R-hat=%.3f bulk-ESS=%.0f tail-ESS=%.0f", dg$variable, dg$rhat, dg$ess_bulk, dg$ess_tail), collapse = "; "), "。请增加保留次数或调整先验。"))
  }
  pp <- process_posterior(data, cfg)
  grid <- seq(cfg$cut, cfg$cut + cfg$horizon, length.out = 121)
  predictions <- list(); milestones <- list(); counts <- list(); weights <- list(); diagnostics <- list()
  for (method in names(models)) {
    model <- models[[method]]
    mat <- matrix(NA_real_, cfg$sims, length(grid))
    hit <- rep(Inf, cfg$sims)
    failures <- 0L
    for (b in seq_len(cfg$sims)) {
      draw <- model
      if (cfg$uncertainty == "bootstrap") {
        # Refit resampled subjects, then forecast the original observed cohort.
        draw <- tryCatch(fit_model(data[sample.int(nrow(data), replace = TRUE), , drop = FALSE], method, cfg$cuts, cfg$tail_rate), error = function(e) NULL)
      } else if (cfg$uncertainty == "gamma") {
        draw <- gamma_posterior_draw(model, cfg$prior_shape, cfg$prior_rate)
      } else if (cfg$uncertainty == "bayes_weibull") {
        draw <- posterior_model_draw(model)
      }
      trial <- if (is.null(draw)) NULL else tryCatch(simulate_trial(data, draw, process_draw(cfg, pp)), error = function(e) NULL)
      if (is.null(trial)) { failures <- failures + 1L; next }
      dates <- trial[[cfg$clock]]
      mat[b, ] <- findInterval(grid, dates)
      # Censor the milestone at the chosen forecast window, never silently drop misses.
      if (length(dates) >= cfg$target && dates[cfg$target] <= max(grid)) hit[b] <- dates[cfg$target]
      if (b %% 25 == 0) progress((match(method, names(models)) - 1 + b / cfg$sims) / length(models), paste(model$label, b, "/", cfg$sims))
    }
    ok <- !is.na(mat[, 1])
    if (sum(ok) < 0.9 * cfg$sims) stop(paste(model$label, "超过 10% 模拟失败；请检查数据/切点或更换模型。"))
    mat <- mat[ok, , drop = FALSE]; hit <- hit[ok]
    if (failures) model$warning <- c(model$warning, paste(failures, "次拟合/模拟失败并已排除；结果基于成功轮次，请检查稳定性。"))
    q <- apply(mat, 2, quantile_with_inf)
    predictions[[method]] <- data.frame(model = model$label, method = method, day = grid,
      mean = colMeans(mat), lower = q[1, ], median = q[2, ], upper = q[3, ], probability = colMeans(mat >= cfg$target),
      probability_mcse = probability_mcse(setNames(list(mat), method), cfg$target))
    counts[[method]] <- mat; milestones[[method]] <- hit
    diagnostics[[method]] <- data.frame(method = method, model = model$label, aic = model$aic,
      simulations = nrow(mat), failed = failures, note = paste(model$warning, collapse = "；"))
  }
  # AIC weights are predictive-mixture heuristics, not posterior model probabilities.
  eligible <- names(models)[vapply(models, function(m) is.finite(m$aic), logical(1))]
  if (length(eligible) >= 2 && isTRUE(cfg$ensemble)) {
    aic <- vapply(models[eligible], function(m) m$aic, numeric(1))
    w <- exp(-0.5 * (aic - min(aic))); w <- w / sum(w)
    chosen <- sample(eligible, cfg$sims, replace = TRUE, prob = w)
    mat <- matrix(0, cfg$sims, length(grid)); hit <- rep(Inf, cfg$sims)
    for (b in seq_len(cfg$sims)) {
      m <- chosen[b]; r <- sample.int(nrow(counts[[m]]), 1)
      mat[b, ] <- counts[[m]][r, ]; hit[b] <- milestones[[m]][r]
    }
    q <- apply(mat, 2, quantile_with_inf)
    predictions$ensemble <- data.frame(model = "AIC 加权预测混合", method = "ensemble", day = grid,
      mean = colMeans(mat), lower = q[1, ], median = q[2, ], upper = q[3, ], probability = colMeans(mat >= cfg$target),
      probability_mcse = probability_mcse(counts[eligible], cfg$target, w, cfg$sims))
    counts$ensemble <- mat; milestones$ensemble <- hit
    weights <- data.frame(method = eligible, weight = as.numeric(w))
  }
  summary <- do.call(rbind, lapply(names(milestones), function(m) {
    hit <- milestones[[m]]; q <- quantile_with_inf(hit)
    label <- if (m == "ensemble") "AIC 加权预测混合" else models[[m]]$label
    data.frame(model = label, method = m, reached = mean(is.finite(hit)),
      reached_mcse = tail(predictions[[m]]$probability_mcse, 1), lower_day = q[1], median_day = q[2], upper_day = q[3],
      median_events_end = quantile_with_inf(counts[[m]][, length(grid)])[2], simulations = length(hit))
  }))
  capacity <- sum(data$status == "event") + sum(data$status == "active") + cfg$future_n
  list(curves = do.call(rbind, predictions), summary = summary, diagnostics = do.call(rbind, diagnostics),
    models = models, counts = counts, milestones = milestones, weights = weights, failures = fit$failures,
    config = cfg, known_events = sum(data$status == "event"), potential_events = capacity,
    process_posterior = pp,
    mcse_scope = if (cfg$uncertainty == "bayes_weibull") "条件于当前有限 MCMC 后验样本库；不含后验采样误差" else "条件于输入数据、模型设定与成功轮次；混合结果含轨迹库及重抽样两层误差",
    data_summary = list(n = nrow(data), events = sum(data$status == "event"), active = sum(data$status == "active"), dropout = sum(data$status == "dropout")))
}
