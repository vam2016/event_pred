if (!exists("time_factor", mode = "function")) source("R/units.R", local = TRUE)

read_adtte <- function(path, filename = path) {
  ext <- tolower(tools::file_ext(filename))
  switch(ext,
    csv = read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")),
    xpt = { if (!requireNamespace("haven", quietly = TRUE)) stop("读取 XPT 需要 haven 包。"); as.data.frame(haven::read_xpt(path)) },
    sas7bdat = { if (!requireNamespace("haven", quietly = TRUE)) stop("读取 SAS 数据需要 haven 包。"); as.data.frame(haven::read_sas(path)) },
    stop("支持 CSV、XPT 和 SAS7BDAT。"))
}

adtte_date <- function(x, encoding = "iso") {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
  if (encoding == "sas") {
    if (!is.numeric(x) || any(!is.finite(x))) stop("SAS 日期需为自 1960-01-01 起的数值天数。")
    if (any(x != floor(x))) stop("SAS 日期不得包含时间部分。")
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (!is.character(x) || anyNA(x) || any(!grepl("^\\d{4}-\\d{2}-\\d{2}$", x))) stop("日期必须为 YYYY-MM-DD；数值 SAS 日期请选择 SAS 编码。")
  y <- as.Date(x, format = "%Y-%m-%d")
  if (anyNA(y) || any(format(y, "%Y-%m-%d") != x)) stop("存在无效日期。")
  y
}

normalize_adtte <- function(raw, paramcd, origin, cut, offset = 1, dropout_codes = numeric(),
                            flag = "", flag_value = "Y", date_encoding = "iso", gap_mode = "strict", aval_unit = "days", group_column = NULL) {
  required <- c("USUBJID", "PARAMCD", "STARTDT", "ADT", "AVAL", "CNSR")
  if (!all(required %in% names(raw))) stop(paste("ADTTE 缺少：", paste(setdiff(required, names(raw)), collapse = ", ")))
  if (!offset %in% c(0, 1)) stop("AVAL 日期加项只能为 0 或 1。")
  if (length(paramcd) != 1 || is.na(paramcd) || !nzchar(paramcd)) stop("请选择 PARAMCD。")
  rows <- !is.na(raw$PARAMCD) & raw$PARAMCD == paramcd
  if (nzchar(flag)) {
    if (!flag %in% names(raw)) stop("分析标志变量不存在。")
    rows <- rows & !is.na(raw[[flag]]) & raw[[flag]] == flag_value
  }
  x <- raw[rows, , drop = FALSE]
  if (!nrow(x)) stop("筛选后没有记录。")
  if (anyNA(x[required])) stop("ADTTE 必填变量不能缺失。")
  if (anyDuplicated(x$USUBJID)) stop("每个 USUBJID 在筛选后只能有一行。请使用分析标志选择唯一分析记录。")
  if (!is.numeric(x$CNSR) || any(!is.finite(x$CNSR)) || any(x$CNSR < 0 | x$CNSR != floor(x$CNSR))) stop("CNSR 必须是非负整数：0=事件，正数=删失。")
  f <- time_factor(aval_unit)
  if (!is.numeric(x$AVAL) || any(!is.finite(x$AVAL)) || any(x$AVAL <= 0)) stop("AVAL 必须是大于 0 的时间值。")
  allowed <- switch(aval_unit, days = c("DAYS", "DAY", "D", "天", "日"), weeks = c("WEEKS", "WEEK", "W", "周"), months = c("MONTHS", "MONTH", "M", "月"))
  if ("AVALU" %in% names(x) && any(!is.na(x$AVALU) & !toupper(trimws(x$AVALU)) %in% allowed)) stop("AVALU 与所选文件 AVAL 单位不一致。")
  if (any(!is.finite(dropout_codes)) || any(dropout_codes <= 0 | dropout_codes != floor(dropout_codes))) stop("永久退出随访的 CNSR 编码必须为正整数。")
  start <- adtte_date(x$STARTDT, date_encoding); adt <- adtte_date(x$ADT, date_encoding)
  if (any(adt < start)) stop("ADT 不能早于 STARTDT。")
  expected <- as.numeric(adt - start) + offset
  if (any(abs(x$AVAL * f - expected) > 1e-5)) stop("AVAL 与 ADT - STARTDT + 日期加项不一致。文件单位与日期加项需按数据定义选择；月按 365.25/12 日换算。")
  if (any(expected - offset <= 0)) stop("当前连续时间拟合要求 ADT 晚于 STARTDT；同日起止记录需要单独的分析规则。")
  study_origin <- as.Date(origin)
  if (length(study_origin) != 1 || is.na(study_origin)) stop("研究起点日期无效。")
  d <- data.frame(id = as.character(x$USUBJID), entry = as.numeric(start - study_origin),
    time = as.numeric(adt - start), obs_day = as.numeric(adt - study_origin),
    status = ifelse(x$CNSR == 0, "event", ifelse(x$CNSR %in% dropout_codes, "dropout", "active")),
    day_offset = 0, stringsAsFactors = FALSE)
  if (!is.null(group_column)) {
    if (length(group_column)!=1 || !group_column %in% names(x)) stop("所选组别变量不存在。")
    if (anyNA(x[[group_column]]) || any(!nzchar(trimws(as.character(x[[group_column]]))))) stop("筛选后的组别不能缺失或为空。")
    d$group <- as.character(x[[group_column]])
  } else if ("TRTP" %in% names(x)) d$group <- as.character(x$TRTP)
  if ("EVNTDESC" %in% names(x)) d$description <- as.character(x$EVNTDESC)
  validate_data(d, cut, require_events = FALSE, gap_mode = gap_mode)
}

parameter_catalog <- function() {
  data.frame(id = c("exponential", "weibull", "pwe", "lognormal", "loglogistic", "gompertz", "cure_weibull", "mixture_weibull"),
    label = c("Exponential", "Weibull", "PWE", "Log-normal", "Log-logistic", "Gompertz", "Cure-Weibull", "双成分 Weibull"))
}

parameter_model <- function(method, p, cuts = numeric()) {
  label <- parameter_catalog()$label[match(method, parameter_catalog()$id)]
  if (is.na(label)) stop("参数模型不存在。")
  finite <- function(x) length(x) == 1 && is.numeric(x) && is.finite(x)
  positive <- function(x) finite(x) && x > 0
  if (method == "exponential") {
    if (!positive(p$median)) stop("中位生存时间需大于 0。")
    params <- c(rate = log(2) / p$median)
  } else if (method %in% c("weibull", "cure_weibull", "mixture_weibull")) {
    if (!positive(p$median) || !positive(p$shape)) stop("中位生存时间与 Weibull shape 需大于 0。")
    eta <- p$median / log(2)^(1 / p$shape)
    params <- c(location = log(eta), scale = 1 / p$shape)
    if (method == "cure_weibull") {
      if (!finite(p$cure) || p$cure < 0 || p$cure >= 1) stop("治愈比例需在 [0,1) 内。")
      params <- c(params, cure = p$cure)
    }
    if (method == "mixture_weibull") {
      if (!positive(p$median2) || !positive(p$shape2) || !finite(p$mix) || p$mix <= 0 || p$mix >= 1) stop("第二成分参数需为正，混合比例需在 (0,1) 内。")
      params <- c(params, location2 = log(p$median2 / log(2)^(1 / p$shape2)), scale2 = 1 / p$shape2, mix = p$mix)
    }
  } else if (method %in% c("lognormal", "loglogistic")) {
    if (!positive(p$median) || !positive(p$scale)) stop("中位生存时间与 log-time scale 需大于 0。")
    params <- c(location = log(p$median), scale = p$scale)
  } else if (method == "gompertz") {
    if (!positive(p$rate) || !finite(p$shape)) stop("Gompertz 初始风险需为正，shape 需为有限数。")
    params <- c(rate = p$rate, shape = p$shape)
  } else {
    if (any(!is.finite(cuts)) || any(cuts <= 0) || is.unsorted(cuts, strictly = TRUE)) stop("PWE 切点需严格递增且为正。")
    if (!is.numeric(p$rates) || length(p$rates) != length(cuts) + 1 || any(!is.finite(p$rates)) || any(p$rates < 0)) stop("PWE 风险率数目需为切点数 + 1，且为非负数。")
    params <- p$rates
  }
  list(id = method, label = label, params = params, cuts = cuts, aic = NA_real_, loglik = NA_real_, warning = character(), source = "parameters")
}

parameter_cohort <- function(cut, active_n = 0, known_n = 0, duration = 0, age_mode = "uniform", seed = 1) {
  for (x in c(cut, active_n, known_n, duration)) if (!is.finite(x) || x < 0) stop("队列参数需为非负有限数。")
  if (active_n != floor(active_n) || known_n != floor(known_n) || active_n + known_n > 5000) stop("队列人数需为整数，总数不能超过 5,000。")
  if (active_n > 0 && (duration <= 0 || duration > cut)) stop("当前随访范围需大于 0 且不超过截点。")
  set.seed(seed)
  ages <- if (age_mode == "fixed") rep(duration, active_n) else runif(active_n, max(1e-6, duration / 1000), duration)
  # Event dates are unknown in aggregate input: anchor historical counts at cut.
  data.frame(id = c(sprintf("A%d", seq_len(active_n)), sprintf("E%d", seq_len(known_n))),
    entry = c(cut - ages, rep(0, known_n)), time = c(ages, rep(max(cut, 1e-6), known_n)),
    obs_day = rep(cut, active_n + known_n), status = c(rep("active", active_n), rep("event", known_n)),
    stringsAsFactors = FALSE)
}

adtte_template <- function() {
  d <- demo_data(120); origin <- as.Date("2025-01-01")
  start <- origin + floor(d$entry); adt <- origin + floor(d$entry + d$time)
  data.frame(STUDYID = "SYNTHETIC", USUBJID = d$id, PARAMCD = "OS", PARAM = "Overall survival (days)",
    STARTDT = start, ADT = adt, AVAL = as.numeric(adt - start) + 1, AVALU = "DAYS",
    CNSR = ifelse(d$status == "event", 0L, ifelse(d$status == "dropout", 2L, 1L)),
    TRTP = rep(c("A", "B"),length.out=nrow(d)), TRTA = rep(c("A", "B"),length.out=nrow(d)),
    EVNTDESC = ifelse(d$status == "event", "EVENT", ifelse(d$status == "dropout", "PERMANENT FOLLOW-UP EXIT", "CUTOFF")), ANL01FL = "Y")
}

source("R/parameters.R", local = TRUE)
