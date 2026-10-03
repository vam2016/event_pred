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
  scalar <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  for (field in c("cut", "horizon", "sims", "target", "future_n", "enroll_rate", "dropout_rate", "lag", "multiplier", "seed", "prior_shape", "prior_rate", "tail_rate")) {
    if (!scalar(cfg[[field]])) stop(paste("配置必须为有限单值：", field))
  }
  if (cfg$horizon <= 0 || cfg$horizon > 3650) stop("预测窗口必须在 0–3650 天内。")
  if (cfg$sims < 50 || cfg$sims > 2000 || cfg$sims != floor(cfg$sims)) stop("模拟次数必须为 50–2000 的整数。")
  if (cfg$target < 1 || cfg$target != floor(cfg$target)) stop("目标事件数必须为正整数。")
  if (cfg$future_n < 0 || cfg$future_n > 1000 || cfg$future_n != floor(cfg$future_n)) stop("未来入组人数必须为 0–1000 的整数。")
  if (cfg$enroll_rate < 0 || (cfg$future_n > 0 && cfg$enroll_rate == 0)) stop("未来入组时，每天入组率必须大于 0。")
  if (cfg$dropout_rate < 0 || cfg$lag < 0 || cfg$multiplier <= 0) stop("脱落率及上报延迟必须非负，风险倍数必须为正。")
  if (cfg$prior_shape <= 0 || cfg$prior_rate <= 0 || cfg$tail_rate <= 0) stop("先验参数及 KM 尾部风险必须为正。")
  if (!cfg$uncertainty %in% c("plugin", "bootstrap", "gamma")) stop("未知不确定性方式。")
  if (!cfg$clock %in% c("occurred", "reported")) stop("未知事件计数时钟。")
  if (cfg$seed < 0 || cfg$seed > .Machine$integer.max || cfg$seed != floor(cfg$seed)) stop("随机种子必须在整数范围内。")
  if (length(cfg$methods) < 1 || any(!cfg$methods %in% model_catalog()$id)) stop("请至少选择一个支持的模型。")
  if (cfg$uncertainty == "gamma" && any(!cfg$methods %in% c("exponential", "pwe"))) stop("Gamma 后验仅支持指数/PWE，请调整所选模型。")
  if (nrow(data) * cfg$sims * length(cfg$methods) > 2e7) stop("数据与模拟组合过大，请减少模型、受试者或模拟次数。")
  invisible(TRUE)
}

quantile_with_inf <- function(x, probs = c(0.025, 0.5, 0.975)) as.numeric(quantile(x, probs, type = 1, names = FALSE))

simulate_trial <- function(data, model, cfg) {
  known <- data$entry[data$status == "event"] + data$time[data$status == "event"]
  active <- data[data$status == "active", , drop = FALSE]
  if (nrow(active)) {
    total_time <- sample_conditional(model, active$time, cfg$multiplier)
    event_date <- active$entry + total_time
    remaining_dropout <- if (cfg$dropout_rate == 0) rep(Inf, nrow(active)) else rexp(nrow(active), cfg$dropout_rate)
    event_date <- event_date[event_date <= cfg$cut + remaining_dropout]
  } else event_date <- numeric()
  if (cfg$future_n > 0) {
    enroll <- cfg$cut + cumsum(rexp(cfg$future_n, cfg$enroll_rate))
    future_event <- sample_conditional(model, rep(0, cfg$future_n), cfg$multiplier)
    future_dropout <- if (cfg$dropout_rate == 0) rep(Inf, cfg$future_n) else rexp(cfg$future_n, cfg$dropout_rate)
    event_date <- c(event_date, (enroll + future_event)[future_event <= future_dropout])
  }
  # Known events are already observed at cut. Lag applies only to future events.
  occurred <- sort(c(known, event_date))
  reported <- sort(c(known, event_date + cfg$lag))
  list(occurred = occurred, reported = reported)
}

run_forecast <- function(data, cfg, progress = function(value, detail) NULL) {
  data <- validate_data(data, cfg$cut)
  validate_config(cfg, data)
  set.seed(cfg$seed)
  fit <- fit_candidates(data, cfg$methods, cfg$cuts, cfg$tail_rate)
  models <- fit$models
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
      }
      trial <- if (is.null(draw)) NULL else tryCatch(simulate_trial(data, draw, cfg), error = function(e) NULL)
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
      mean = colMeans(mat), lower = q[1, ], median = q[2, ], upper = q[3, ], probability = colMeans(mat >= cfg$target))
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
      mean = colMeans(mat), lower = q[1, ], median = q[2, ], upper = q[3, ], probability = colMeans(mat >= cfg$target))
    counts$ensemble <- mat; milestones$ensemble <- hit
    weights <- data.frame(method = eligible, weight = as.numeric(w))
  }
  summary <- do.call(rbind, lapply(names(milestones), function(m) {
    hit <- milestones[[m]]; q <- quantile_with_inf(hit)
    label <- if (m == "ensemble") "AIC 加权预测混合" else models[[m]]$label
    data.frame(model = label, method = m, reached = mean(is.finite(hit)), lower_day = q[1], median_day = q[2], upper_day = q[3],
      median_events_end = quantile_with_inf(counts[[m]][, length(grid)])[2], simulations = length(hit))
  }))
  capacity <- sum(data$status == "event") + sum(data$status == "active") + cfg$future_n
  list(curves = do.call(rbind, predictions), summary = summary, diagnostics = do.call(rbind, diagnostics),
    models = models, counts = counts, milestones = milestones, weights = weights, failures = fit$failures,
    config = cfg, known_events = sum(data$status == "event"), potential_events = capacity,
    data_summary = list(n = nrow(data), events = sum(data$status == "event"), active = sum(data$status == "active"), dropout = sum(data$status == "dropout")))
}
