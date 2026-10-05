# Reconstruct only what is observable at an earlier calendar cut.
snapshot_at <- function(data, cut) {
  d <- data[data$entry < cut, , drop = FALSE]
  late <- d$obs_day > cut
  offset <- if ("day_offset" %in% names(d)) d$day_offset else rep(0, nrow(d))
  d$time[late] <- cut - d$entry[late] + offset[late]
  d$obs_day[late] <- cut
  d$status[late] <- "active"
  d$event <- as.integer(d$status == "event")
  d
}

backtest_forecast <- function(data, cfg, cuts, truth_cut, progress = function(value, detail) NULL, plan = NULL) {
  cfg <- complete_config(cfg)
  if (cfg$input_mode != "adtte") stop("历史回测需要 ADTTE 数据模式。")
  if (!length(cuts) || length(cuts) > 6 || any(!is.finite(cuts) | cuts <= 0 | cuts >= truth_cut)) stop("回测截点需为 1–6 个正数，且早于当前数据截点。")
  if (cfg$clock != "occurred") stop("当前 ADTTE 未提供历史报告日期，不能评价上报口径回测。请以实际发生口径运行预测后回测。")
  if (anyDuplicated(cuts)) stop("回测截点不得重复。")
  required <- c("cut", "future_n", "enroll_rate")
  grouped <- identical(cfg$analysis_mode,"grouped")
  labels <- if(grouped) vapply(cfg$groups,`[[`,character(1),"name") else ""
  if (!is.data.frame(plan) || !all(required %in% names(plan)) || nrow(plan) != length(cuts)*length(labels)) stop("请逐截点、逐组提供历史计划：cut、future_n、enroll_rate；分组时需group组名。不沿用当前剩余人数。")
  if (any(!vapply(plan[required], is.numeric, logical(1))) || any(!is.finite(as.matrix(plan[required])))) stop("历史计划需为有限数值。")
  if(grouped) {
    if(!"group" %in% names(plan) || anyNA(plan$group) || any(!plan$group %in% labels) || anyDuplicated(plan[c("cut","group")])) stop("历史计划需包含有效且唯一的截点/组别组合。")
    expected <- expand.grid(group=unname(labels),cut=cuts,stringsAsFactors=FALSE)
    at <- match(paste(expected$group,expected$cut,sep="\r"),paste(plan$group,plan$cut,sep="\r"))
    if(anyNA(at)) stop("每个历史截点都必须有全部组的计划。")
    plan <- plan[at,c("group",required),drop=FALSE]
  } else {
    if(anyDuplicated(plan$cut)) stop("历史计划每个截点唯一。")
    at <- match(cuts,plan$cut); if(anyNA(at)) stop("历史计划的cut必须匹配回测截点。")
    plan <- plan[at,required,drop=FALSE]
  }
  if (any(plan$future_n < 0 | plan$future_n > 1000 | plan$future_n != floor(plan$future_n)) || any(plan$enroll_rate < 0)) stop("历史剩余人数需为0–1000整数，历史入组率需非负。")
  validate_data(data, truth_cut, gap_mode = cfg$gap_mode)
  rows <- list(); curves <- list(); snapshots <- list(); failures <- list()
  for (j in seq_along(cuts)) {
    early <- cuts[j]
    d <- snapshot_at(data, early)
    c2 <- cfg
    if(grouped) {
      for(id in names(c2$groups)) {
        row <- plan[plan$cut==early & plan$group==c2$groups[[id]]$name,,drop=FALSE]
        c2$groups[[id]]$future_n <- row$future_n; c2$groups[[id]]$enroll_rate <- row$enroll_rate; c2$groups[[id]]$enroll_mode <- "constant"
      }
      c2$future_n <- sum(vapply(c2$groups,`[[`,numeric(1),"future_n")); c2$enroll_rate <- sum(vapply(c2$groups,`[[`,numeric(1),"enroll_rate"))
    } else {c2$future_n <- plan$future_n[j]; c2$enroll_rate <- plan$enroll_rate[j]}
    c2$enroll_mode <- "constant"
    c2$cut <- early; c2$horizon <- min(cfg$horizon, truth_cut - early)
    c2$recruit_end <- min(cfg$recruit_end, early)
    c2$seed <- cfg$seed + j - 1L
    r <- run_forecast(d, c2, function(value, detail) progress((j - 1 + value) / length(cuts), detail))
    if (length(r$failures)) failures[[length(failures) + 1L]] <- data.frame(cut = early, method = names(r$failures), message = unname(r$failures))
    actual_dates <- sort(data$obs_day[data$status == "event"])
    truth_hit <- if (length(actual_dates) >= cfg$target) actual_dates[cfg$target] else Inf
    actual_hit <- if (truth_hit <= early + c2$horizon) truth_hit else Inf
    for (m in unique(r$curves$method)) {
      q <- r$curves[r$curves$method == m, ]; actual <- findInterval(q$day, actual_dates)
      s <- r$summary[r$summary$method == m, ]
      end <- nrow(q)
      rows[[length(rows) + 1L]] <- data.frame(cut = early, end = early + c2$horizon, method = m,
        model = s$model, planned_future_n = c2$future_n, historical_enroll_rate_per_day = c2$enroll_rate,
        enrollment_source = if (c2$process_uncertainty == "gamma") "historical_posterior" else "historical_plan",
        observed_events = actual[end], predicted_events = q$median[end],
        error = q$median[end] - actual[end], count_lower = q$lower[end], count_upper = q$upper[end],
        count_covered = q$lower[end] <= actual[end] && q$upper[end] >= actual[end],
        count_interval_width = q$upper[end] - q$lower[end], reached_probability = s$reached,
        target_reached = as.integer(is.finite(actual_hit)), brier = (s$reached - as.integer(is.finite(actual_hit)))^2,
        actual_target_day = actual_hit, predicted_target_day = s$median_day,
        target_error = if (is.finite(actual_hit) && is.finite(s$median_day)) s$median_day - actual_hit else NA_real_,
        truncated_target_covered = s$lower_day <= actual_hit && s$upper_day >= actual_hit)
      q$actual <- actual; q$backtest_cut <- early; curves[[length(curves) + 1L]] <- q
    }
    snapshots[[as.character(early)]] <- d
  }
  list(summary = do.call(rbind, rows), curves = do.call(rbind, curves), snapshots = snapshots,
    failures = if (length(failures)) do.call(rbind, failures) else data.frame(cut = numeric(), method = character(), message = character()),
    config = cfg, cuts = cuts, truth_cut = truth_cut, plan = plan,
    note = "按逐截点历史剩余人数和恒定入组率回测；过程后验模式按截至各截点的数据更新率。其余情景参数沿用最近预测。仅评价实际发生事件；单一最终文件不能恢复历史报告或访视版本。")
}

run_sensitivity <- function(data, cfg, values, models_override = NULL, progress = function(value, detail) NULL) {
  if (!length(values) || length(values) > 8 || any(!is.finite(values) | values <= 0)) stop("风险倍数需为 1–8 个正数。")
  rows <- list()
  for (j in seq_along(values)) {
    c2 <- cfg; c2$multiplier <- values[j]
    if(identical(c2$analysis_mode,"grouped")) for(id in names(c2$groups)) c2$groups[[id]]$multiplier <- cfg$groups[[id]]$multiplier*values[j]
    r <- run_forecast(data, c2, function(v, d) progress((j - 1 + v) / length(values), d), models_override)
    s <- r$summary; s$multiplier <- values[j]; end<-r$curves[r$curves$day==max(r$curves$day),];ix<-match(s$method,end$method);s$mean_events_end<-end$mean[ix];s$lower_events_end<-end$lower[ix];s$upper_events_end<-end$upper[ix];rows[[j]] <- s
  }
  do.call(rbind, rows)
}

# Prespecified synthetic calibration experiments with known full trial paths.
calibration_experiment <- function(repetitions = 30, seed = 20261005, uncertainty = "plugin") {
  if (!uncertainty %in% c("plugin", "bootstrap", "gamma")) stop("未知校准方式。")
  scenarios <- data.frame(name = c("exponential", "increasing", "decreasing", "high_dropout", "cure"),
    shape = c(1, 1.5, .7, 1, 1), dropout = c(.00025, .00025, .00025, .002, .00025), cure = c(0, 0, 0, 0, .3))
  rows <- list()
  for (j in seq_len(nrow(scenarios))) for (b in seq_len(repetitions)) {
    set.seed(seed + j * 10000 + b)
    n <- 200; enroll <- seq(1, n) * 2; cut <- 300; end <- 700
    life <- rweibull(n, scenarios$shape[j], 350 / log(2)^(1 / scenarios$shape[j]))
    life[runif(n) < scenarios$cure[j]] <- Inf
    drop <- rexp(n, scenarios$dropout[j]); follow <- pmax(0, end - enroll)
    time <- pmin(life, drop, follow)
    truth <- data.frame(id = sprintf("C%d", seq_len(n)), entry = enroll, time = time,
      obs_day = enroll + time, status = ifelse(life <= drop & life <= follow, "event", ifelse(drop < life & drop < follow, "dropout", "active")))
    train <- snapshot_at(truth, cut)
    methods <- if (uncertainty == "gamma") c("exponential", "pwe") else c("exponential", "weibull", "pwe")
    cfg <- list(cut = cut, horizon = end - cut, sims = 300, target = 100, future_n = n - nrow(train),
      enroll_rate = .5, dropout_rate = scenarios$dropout[j], lag = 0, multiplier = 1, seed = seed + b,
      prior_shape = .5, prior_rate = 50, tail_rate = .002, uncertainty = uncertainty, clock = "occurred",
      methods = methods, cuts = c(90, 180), ensemble = TRUE, origin = "2025-01-01")
    result <- tryCatch(backtest_forecast(truth, cfg, cut, end, plan = data.frame(cut = cut, future_n = n - nrow(train), enroll_rate = .5)), error = function(e) e)
    if (inherits(result, "error")) {
      rows[[length(rows) + 1L]] <- data.frame(scenario = scenarios$name[j], repetition = b, model = "FAILED", error = NA_real_,
        count_covered = NA, count_interval_width = NA_real_, brier = NA_real_, target_error = NA_real_, failure = conditionMessage(result))
    } else {
      s <- result$summary
      rows[[length(rows) + 1L]] <- data.frame(scenario = scenarios$name[j], repetition = b, model = s$model,
        error = s$error, count_covered = s$count_covered, count_interval_width = s$count_interval_width,
        brier = s$brier, target_error = s$target_error, failure = "")
    }
  }
  do.call(rbind, rows)
}
