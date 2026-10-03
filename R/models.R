model_catalog <- function() {
  data.frame(id = c("exponential", "weibull", "pwe", "lognormal", "loglogistic", "km_tail"),
    label = c("Exponential · 指数", "Weibull", "PWE · 分段指数", "Log-normal", "Log-logistic", "KM + 指定指数尾部"),
    stringsAsFactors = FALSE)
}

validate_data <- function(data, cut) {
  required <- c("id", "entry", "time", "status")
  if (!all(required %in% names(data))) stop("CSV 必须包含 id、entry、time、status。")
  if (!is.numeric(cut) || length(cut) != 1 || !is.finite(cut) || cut <= 0) stop("截点必须为正数。")
  if (nrow(data) < 3 || nrow(data) > 5000) stop("首版支持 3–5,000 名受试者。")
  if (anyNA(data[required])) stop("必填字段不能缺失。")
  if (anyDuplicated(data$id) || any(!nzchar(trimws(as.character(data$id))))) stop("id 必须非空且唯一。")
  if (!is.numeric(data$entry) || !is.numeric(data$time)) stop("entry 和 time 必须为数值，单位为天。")
  if (any(!is.finite(data$entry)) || any(!is.finite(data$time)) || any(data$entry < 0) || any(data$time <= 0)) stop("entry 必须非负；time 必须大于 0，且均为有限数。")
  if (any(!data$status %in% c("event", "active", "dropout"))) stop("status 只能是 event、active 或 dropout。")
  if (any(data$entry + data$time > cut + 1e-7)) stop("观察时间不能超过数据截点。")
  active <- data$status == "active"
  if (any(abs(data$entry[active] + data$time[active] - cut) > 1e-7)) stop("active 患者必须持续观察到截点：entry + time = cut。尚未补齐的随访需先完成数据核查。")
  if (sum(data$status == "event") < 2) stop("至少需要 2 个事件；早期或启动前规划将在后续版本支持。")
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
    if (any(events < 5)) base$warning <- c(base$warning, "PWE 部分区间不足 5 个事件，尾部和区间风险估计不稳定。")
    if (tail(rates, 1) == 0) base$warning <- c(base$warning, "PWE 最后一段估计风险为零，部分模拟将永不发生事件；可减少切点或使用贝叶斯 PWE。")
    return(c(base, list(params = rates, cuts = cuts, events = events, exposure = exposure,
      loglik = ll, aic = 2 * length(rates) - 2 * ll)))
  }
  if (method == "km_tail") {
    if (!is.finite(tail_rate) || tail_rate <= 0) stop("KM 尾部风险必须大于 0。")
    sf <- survival::survfit(survival::Surv(time, event) ~ 1, data = data)
    base$warning <- "KM 在观察范围内使用阶梯生存函数；超出最长观察随访后使用用户指定的指数尾部。尾部是假设，不由 KM 识别。"
    return(c(base, list(params = c(tail_rate = tail_rate), knots = sf$time, surv = sf$surv,
      max_time = max(data$time), loglik = NA_real_, aic = NA_real_)))
  }
  dist <- c(weibull = "weibull", lognormal = "lognormal", loglogistic = "loglogistic")[[method]]
  fit <- survival::survreg(survival::Surv(time, event) ~ 1, data = data, dist = dist)
  if (any(!is.finite(c(coef(fit), fit$scale))) || fit$scale <= 0 || any(!is.finite(fit$var))) stop("模型未获得有限参数和协方差，不能用于预测。")
  ll <- as.numeric(logLik(fit))
  c(base, list(params = c(location = unname(coef(fit)[1]), scale = fit$scale),
    loglik = ll, aic = 4 - 2 * ll))
}

model_survival <- function(model, t) {
  t <- pmax(t, 0)
  p <- model$params
  switch(model$id,
    exponential = exp(-p[[1]] * t),
    weibull = exp(-(t / exp(p[[1]]))^(1 / p[[2]])),
    lognormal = plnorm(t, meanlog = p[[1]], sdlog = p[[2]], lower.tail = FALSE),
    loglogistic = plogis((log(t) - p[[1]]) / p[[2]], lower.tail = FALSE),
    pwe = exp(-model_cumhaz(model, t)),
    km_tail = {
      s <- c(1, model$surv)[findInterval(pmin(t, model$max_time), model$knots) + 1L]
      s * exp(-p[[1]] * pmax(0, t - model$max_time))
    })
}

model_cumhaz <- function(model, t) {
  t <- pmax(t, 0)
  p <- model$params
  switch(model$id,
    exponential = p[[1]] * t,
    weibull = (t / exp(p[[1]]))^(1 / p[[2]]),
    lognormal = -plnorm(t, meanlog = p[[1]], sdlog = p[[2]], lower.tail = FALSE, log.p = TRUE),
    loglogistic = -plogis((log(t) - p[[1]]) / p[[2]], lower.tail = FALSE, log.p = TRUE),
    pwe = {
      bounds <- c(0, model$cuts, Inf)
      vapply(t, function(x) sum(model$params * pmax(0, pmin(x, bounds[-1]) - head(bounds, -1))), numeric(1))
    },
    km_tail = -log(model_survival(model, t)))
}

inverse_cumhaz <- function(model, z) {
  p <- model$params
  switch(model$id,
    exponential = z / p[[1]],
    weibull = exp(p[[1]]) * z^p[[2]],
    lognormal = qlnorm(-z, meanlog = p[[1]], sdlog = p[[2]], lower.tail = FALSE, log.p = TRUE),
    loglogistic = exp(p[[1]] + p[[2]] * qlogis(-z, lower.tail = FALSE, log.p = TRUE)),
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
