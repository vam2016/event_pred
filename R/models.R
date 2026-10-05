model_catalog <- function() {
  data.frame(id = c("exponential", "weibull", "pwe", "lognormal", "loglogistic", "km_tail", "gompertz", "cure_weibull"),
    label = c("Exponential · 指数", "Weibull", "PWE · 分段指数", "Log-normal", "Log-logistic", "KM + 指定指数尾部", "Gompertz", "Cure-Weibull · 治愈"),
    stringsAsFactors = FALSE)
}

validate_data <- function(data, cut, require_events = TRUE, gap_mode = "strict") {
  required <- c("id", "entry", "time", "status")
  if (!all(required %in% names(data))) stop("CSV 必须包含 id、entry、time、status。")
  if (!is.numeric(cut) || length(cut) != 1 || !is.finite(cut) || cut < 0 || (require_events && cut == 0)) stop("数据截点需为正数；参数模式允许从第 0 天开始。")
  if (nrow(data) > 5000 || (require_events && nrow(data) < 3)) stop("数据拟合支持 3–5,000 名受试者。")
  if (anyNA(data[required])) stop("必填字段不能缺失。")
  if (anyDuplicated(data$id) || any(!nzchar(trimws(as.character(data$id))))) stop("id 必须非空且唯一。")
  if (!is.numeric(data$entry) || !is.numeric(data$time)) stop("entry 和 time 必须为数值，单位为天。")
  if (any(!is.finite(data$entry)) || any(!is.finite(data$time)) || any(data$entry < 0) || any(data$time <= 0)) stop("entry 必须非负；time 必须大于 0，且均为有限数。")
  if (any(!data$status %in% c("event", "active", "dropout"))) stop("status 只能是 event、active 或 dropout。")
  if (!"obs_day" %in% names(data)) data$obs_day <- data$entry + data$time
  if (anyNA(data$obs_day) || any(!is.finite(data$obs_day)) || any(data$obs_day < data$entry) || any(data$obs_day > cut + 1e-7)) stop("观察日期必须在起始日期与截点之间。")
  offset <- if ("day_offset" %in% names(data)) data$day_offset else rep(0, nrow(data))
  if (any(abs(data$obs_day - (data$entry + data$time - offset)) > 1e-6)) stop("随访时间与观察日期不一致。")
  active <- data$status == "active"
  if (gap_mode == "strict" && any(abs(data$obs_day[active] - cut) > 1e-7)) stop("仍随访记录的 ADT 早于截点。请选择末次确认后补全模式，或提供截至截点的确认记录。")
  if (require_events && sum(data$status == "event") < 2) stop("模型拟合至少需要 2 个事件；无事件时可使用参数输入模式。")
  data$event <- as.integer(data$status == "event")
  data
}

pwe_exposure <- function(time, cuts) {
  bounds <- c(0, cuts, Inf)
  vapply(seq_len(length(bounds) - 1), function(j) sum(pmax(0, pmin(time, bounds[j + 1]) - bounds[j])), numeric(1))
}

fit_model <- function(data, method, cuts = c(90, 180, 365), tail_rate = 0.002) {
  if (!method %in% model_catalog()$id) stop("未知模型。")
  if (!"event" %in% names(data)) data$event <- as.integer(data$status == "event")
  if (sum(data$event) < 2 || sum(data$time) <= 0) stop("拟合至少需要 2 个事件和正的随访时间。")
  label <- model_catalog()$label[match(method, model_catalog()$id)]
  base <- list(id = method, label = label, warning = character())
  if (method == "exponential") {
    rate <- sum(data$event) / sum(data$time)
    ll <- sum(data$event) * log(rate) - rate * sum(data$time)
    return(c(base, list(params = c(rate = rate), loglik = ll, aic = 2 - 2 * ll,
      events = sum(data$event), exposure = sum(data$time))))
  }
  if (method == "pwe") {
    if (any(!is.finite(cuts)) || any(cuts <= 0) || is.unsorted(cuts, strictly = TRUE)) stop("PWE 切点必须严格递增且大于 0。")
    exposure <- pwe_exposure(data$time, cuts)
    # An event on a boundary belongs to the interval ending at that boundary.
    interval <- vapply(data$time[data$event == 1], function(t) 1L + sum(t > cuts), integer(1))
    events <- tabulate(interval, nbins = length(cuts) + 1)
    if (any(exposure == 0)) stop("PWE 存在无随访时间的区间，请减少切点或缩短最后一个切点。")
    rates <- events / exposure
    ll <- sum(ifelse(events > 0, events * log(pmax(rates, .Machine$double.xmin)), 0) - rates * exposure)
    if (any(events < 5)) base$warning <- c(base$warning, "部分区间事件数 < 5。")
    if (tail(rates, 1) == 0) base$warning <- c(base$warning, "末段风险率 = 0；保留无限事件时间。")
    return(c(base, list(params = rates, cuts = cuts, events = events, exposure = exposure,
      loglik = ll, aic = 2 * length(rates) - 2 * ll)))
  }
  if (method == "km_tail") {
    if (!is.finite(tail_rate) || tail_rate <= 0) stop("KM 尾部风险必须大于 0。")
    sf <- survival::survfit(survival::Surv(time, event) ~ 1, data = data)
    base$warning <- "观察范围：KM；外推：指定指数尾部。"
    return(c(base, list(params = c(tail_rate = tail_rate), knots = sf$time, surv = sf$surv,
      max_time = max(data$time), loglik = NA_real_, aic = NA_real_)))
  }
  if(method %in% c("gompertz","cure_weibull")) return(fit_extended_model(data,method,label))
  dist <- c(weibull = "weibull", lognormal = "lognormal", loglogistic = "loglogistic")[[method]]
  # survreg can return finite parameters after non-convergence. Treat warnings
  # as a rejected candidate rather than allowing unreliable AIC/predictions.
  fit <- withCallingHandlers(
    survival::survreg(survival::Surv(time, event) ~ 1, data = data, dist = dist),
    warning = function(w) stop(paste("AFT 拟合未通过数值检查：", conditionMessage(w)), call. = FALSE))
  if (any(!is.finite(c(coef(fit), fit$scale))) || fit$scale <= 0 || any(!is.finite(fit$var))) stop("模型未获得有限参数和协方差，不能用于预测。")
  ll <- as.numeric(logLik(fit))
  if (!is.finite(ll)) stop("模型对数似然不是有限值，不能用于预测。")
  c(base, list(params = c(location = unname(coef(fit)[1]), scale = fit$scale),
    loglik = ll, aic = 4 - 2 * ll))
}

model_survival <- function(model, t) {
  if(model$id %in% c("gengamma","loghaz_spline","blinded_ph","predictive_mixture"))return(exp(-fx_cumhaz(model,t)))
  t <- pmax(t, 0)
  p <- model$params
  switch(model$id,
    exponential = exp(-p[[1]] * t),
    weibull = exp(-(t / exp(p[[1]]))^(1 / p[[2]])),
    lognormal = plnorm(t, meanlog = p[[1]], sdlog = p[[2]], lower.tail = FALSE),
    loglogistic = plogis((log(t) - p[[1]]) / p[[2]], lower.tail = FALSE),
    pwe = exp(-model_cumhaz(model, t)),
    gompertz = exp(-model_cumhaz(model, t)),
    cure_weibull = exp(-model_cumhaz(model, t)),
    mixture_weibull = exp(-model_cumhaz(model, t)),
    km_tail = {
      s <- c(1, model$surv)[findInterval(pmin(t, model$max_time), model$knots) + 1L]
      s * exp(-p[[1]] * pmax(0, t - model$max_time))
    })
}

model_cumhaz <- function(model, t) {
  if(model$id %in% c("gengamma","loghaz_spline","blinded_ph","predictive_mixture"))return(fx_cumhaz(model,t))
  t <- pmax(t, 0)
  p <- model$params
  switch(model$id,
    exponential = p[[1]] * t,
    weibull = (t / exp(p[[1]]))^(1 / p[[2]]),
    lognormal = -plnorm(t, meanlog = p[[1]], sdlog = p[[2]], lower.tail = FALSE, log.p = TRUE),
    loglogistic = -plogis((log(t) - p[[1]]) / p[[2]], lower.tail = FALSE, log.p = TRUE),
    gompertz = if (p[[2]] == 0) p[[1]] * t else p[[1]] * expm1(p[[2]] * t) / p[[2]],
    cure_weibull = {
      h <- (t / exp(p[[1]]))^(1 / p[[2]])
      if (p[[3]] == 0) h else -log_add(log(p[[3]]), log1p(-p[[3]]) - h)
    },
    mixture_weibull = -log_add(log(p[[5]]) - (t / exp(p[[1]]))^(1 / p[[2]]),
      log1p(-p[[5]]) - (t / exp(p[[3]]))^(1 / p[[4]])),
    pwe = {
      bounds <- c(0, model$cuts, Inf)
      vapply(t, function(x) sum(model$params * pmax(0, pmin(x, bounds[-1]) - head(bounds, -1))), numeric(1))
    },
    km_tail = -log(model_survival(model, t)))
}

inverse_cumhaz <- function(model, z) {
  if(model$id %in% c("gengamma","loghaz_spline","blinded_ph","predictive_mixture"))return(fx_inverse(model,z))
  p <- model$params
  switch(model$id,
    exponential = z / p[[1]],
    weibull = exp(p[[1]]) * z^p[[2]],
    lognormal = qlnorm(-z, meanlog = p[[1]], sdlog = p[[2]], lower.tail = FALSE, log.p = TRUE),
    loglogistic = exp(p[[1]] + p[[2]] * qlogis(-z, lower.tail = FALSE, log.p = TRUE)),
    gompertz = {
      if (p[[2]] == 0) z / p[[1]] else {
        x <- 1 + p[[2]] * z / p[[1]]
        out <- rep(Inf, length(z)); ok <- x > 0
        out[ok] <- log1p(p[[2]] * z[ok] / p[[1]]) / p[[2]]; out
      }
    },
    cure_weibull = {
      if (p[[3]] == 0) exp(p[[1]]) * z^p[[2]] else {
        out <- rep(Inf, length(z)); ok <- z < -log(p[[3]])
        hz <- -log(pmax(0, (exp(-z[ok]) - p[[3]]) / (1 - p[[3]])))
        out[ok] <- exp(p[[1]]) * hz^p[[2]]; out
      }
    },
    mixture_weibull = vapply(z, function(target) {
      if (target <= 0) return(0)
      if (is.infinite(target)) return(Inf)
      # At survival exp(-target), the mixture quantile is between the two
      # component quantiles. Solve in log-time: no arbitrary calendar cap.
      bracket <- p[c(1, 3)] + p[c(2, 4)] * log(target)
      lo <- min(bracket); hi <- max(bracket)
      if (lo == hi) return(exp(lo))
      objective <- function(log_t) -log_add(
        log(p[[5]]) - exp((log_t - p[[1]]) / p[[2]]),
        log1p(-p[[5]]) - exp((log_t - p[[3]]) / p[[4]])) - target
      fl <- objective(lo); fh <- objective(hi)
      if (fl >= 0) return(exp(lo))
      if (fh <= 0) return(exp(hi))
      exp(uniroot(objective, c(lo, hi), f.lower = fl, f.upper = fh, tol = 1e-11)$root)
    }, numeric(1)),
    pwe = {
      bounds <- c(0, model$cuts, Inf)
      vapply(z, function(target) {
        acc <- 0
        for (j in seq_along(p)) {
          if (target <= acc) return(bounds[j])
          if (p[j] == 0) next
          next_acc <- acc + p[j] * (bounds[j + 1] - bounds[j])
          if (target <= next_acc) return(bounds[j] + (target - acc) / p[j])
          acc <- next_acc
        }
        Inf
      }, numeric(1))
    },
    km_tail = {
      h <- -log(model$surv)
      last_h <- tail(h, 1)
      vapply(z, function(target) {
        index <- which(h >= target)[1]
        if (!is.na(index)) return(model$knots[index])
        model$max_time + (target - last_h) / p[[1]]
      }, numeric(1))
    })
}

log_add <- function(a, b) {
  m <- pmax(a, b)
  out <- m + log(exp(a - m) + exp(b - m))
  out[is.infinite(m) & m < 0] <- -Inf
  out
}

sample_conditional <- function(model, age, multiplier = 1, u = stats::runif(length(age))) {
  if (!is.finite(multiplier) || multiplier <= 0) stop("未来风险倍数必须大于 0。")
  if (length(u) != length(age) || any(u <= 0 | u >= 1)) stop("u 必须在 (0,1) 内且长度与 age 相同。")
  h <- model_cumhaz(model, age)
  if (any(!is.finite(h))) stop("当前无事件患者超过模型的生存支持范围，不能做条件预测。")
  inverse_cumhaz(model, h - log(u) / multiplier)
}

gamma_posterior_draw <- function(model, shape = 0.5, rate = 50) {
  if (!model$id %in% c("exponential", "pwe")) stop("共轭后验仅支持 Exponential 和 PWE。")
  if (shape <= 0 || rate <= 0) stop("Gamma 先验 shape 和 rate 必须为正。")
  model$params <- stats::rgamma(length(model$params), shape = shape + model$events, rate = rate + model$exposure)
  model
}

source("R/fitting.R", local = TRUE)
