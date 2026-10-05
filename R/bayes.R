weibull_log_posterior <- function(theta, data, prior) {
  if (any(!is.finite(theta))) return(-Inf)
  log_eta <- theta[1]; k <- exp(theta[2]); lt <- log(data$time)
  h <- exp(k * (lt - log_eta))
  if (any(!is.finite(h))) return(-Inf)
  ll <- sum(data$event * (log(k) - log_eta + (k - 1) * (lt - log_eta)) - h)
  ll + dnorm(log_eta, prior$log_eta_mean, prior$log_eta_sd, log = TRUE) +
    dnorm(theta[2], prior$log_shape_mean, prior$log_shape_sd, log = TRUE)
}

fit_bayesian_weibull <- function(data, cfg) {
  if (!requireNamespace("posterior", quietly = TRUE)) stop("Weibull MCMC 需要 posterior 包，请运行安装脚本。")
  prior <- list(log_eta_mean = cfg$log_eta_mean, log_eta_sd = cfg$log_eta_sd,
    log_shape_mean = cfg$log_shape_mean, log_shape_sd = cfg$log_shape_sd)
  if (any(!is.finite(unlist(prior))) || prior$log_eta_sd <= 0 || prior$log_shape_sd <= 0) stop("正态先验标准差需为正，均值需为有限数。")
  if (cfg$mcmc_chains < 2 || cfg$mcmc_chains > 4 || cfg$mcmc_chains != floor(cfg$mcmc_chains)) stop("MCMC 链数需为 2–4 的整数。")
  if (cfg$mcmc_warmup < 200 || cfg$mcmc_draws < 200 || cfg$mcmc_warmup > 10000 || cfg$mcmc_draws > 10000 || any(c(cfg$mcmc_warmup, cfg$mcmc_draws) != floor(c(cfg$mcmc_warmup, cfg$mcmc_draws)))) stop("预热与每链保留次数需为 200–10,000 的整数。")
  if (nrow(data) * cfg$mcmc_chains * (cfg$mcmc_warmup + cfg$mcmc_draws) > 2e8) stop("MCMC 计算量超过本机限制，请减少链迭代次数或记录数。")
  init <- fit_model(data, "weibull")$params
  mode <- optim(c(init[1], -log(init[2])), function(th) -weibull_log_posterior(th, data, prior), method = "BFGS", hessian = TRUE)
  if (mode$convergence != 0 || any(!is.finite(mode$hessian))) stop("后验模式计算失败。")
  cov <- tryCatch(solve(mode$hessian), error = function(e) NULL)
  if (is.null(cov) || min(eigen(cov, symmetric = TRUE)$values) <= 0) stop("后验提议协方差计算失败。")
  root <- chol(cov)
  arr <- array(NA_real_, c(cfg$mcmc_draws, cfg$mcmc_chains, 2), dimnames = list(NULL, NULL, c("log_eta", "log_shape")))
  acceptance <- numeric(cfg$mcmc_chains)
  for (chain in seq_len(cfg$mcmc_chains)) {
    theta <- mode$par + drop(rnorm(2) %*% root) * 2
    lp <- weibull_log_posterior(theta, data, prior)
    if (!is.finite(lp)) { theta <- mode$par; lp <- weibull_log_posterior(theta, data, prior) }
    scale <- 2.38 / sqrt(2); window <- 0; retained <- 0
    for (iter in seq_len(cfg$mcmc_warmup + cfg$mcmc_draws)) {
      proposal <- theta + scale * drop(rnorm(2) %*% root)
      proposed_lp <- weibull_log_posterior(proposal, data, prior)
      accepted <- is.finite(proposed_lp) && log(runif(1)) < proposed_lp - lp
      if (accepted) { theta <- proposal; lp <- proposed_lp }
      if (iter <= cfg$mcmc_warmup) {
        window <- window + as.integer(accepted)
        if (iter %% 50 == 0) {
          scale <- min(20, max(0.05, scale * exp(window / 50 - 0.30)))
          window <- 0
        }
      } else {
        retained <- retained + as.integer(accepted)
        arr[iter - cfg$mcmc_warmup, chain, ] <- theta
      }
    }
    acceptance[chain] <- retained / cfg$mcmc_draws
  }
  draws <- posterior::as_draws_array(arr)
  diag <- as.data.frame(posterior::summarise_draws(draws, "mean", "sd", "rhat", "ess_bulk", "ess_tail"))
  ok <- all(is.finite(as.matrix(diag[, c("rhat", "ess_bulk", "ess_tail")]))) && all(diag$rhat <= 1.01 & diag$ess_bulk >= 400 & diag$ess_tail >= 400)
  flat <- do.call(rbind, lapply(seq_len(cfg$mcmc_chains), function(j) arr[, j, ]))
  model <- fit_model(data, "weibull")
  model$posterior_params <- cbind(location = flat[, 1], scale = exp(-flat[, 2]))
  model$posterior <- list(draws = arr, diagnostics = diag, acceptance = acceptance, passed = ok, prior = prior)
  model$params <- c(location = median(flat[, 1]), scale = median(exp(-flat[, 2])))
  model$source <- "posterior"
  model
}

posterior_model_draw <- function(model) {
  i <- sample.int(nrow(model$posterior_params), 1)
  model$params <- model$posterior_params[i, ]; model
}

process_posterior <- function(data, cfg) {
  if (!identical(cfg$process_uncertainty, "gamma")) return(NULL)
  if (cfg$recruit_start < 0 || cfg$recruit_end > cfg$cut || cfg$recruit_end <= cfg$recruit_start) stop("入组观测窗口需在研究起点至截点内且长度为正。")
  enroll_count <- sum(data$entry >= cfg$recruit_start & data$entry <= cfg$recruit_end)
  list(enroll = c(shape = cfg$enroll_prior_shape + enroll_count, rate = cfg$enroll_prior_rate + cfg$recruit_end - cfg$recruit_start),
    dropout = c(shape = cfg$drop_prior_shape + sum(data$status == "dropout"), rate = cfg$drop_prior_rate + sum(data$time)))
}

process_draw <- function(cfg, posterior) {
  if (is.null(posterior)) return(cfg)
  cfg$enroll_rate <- rgamma(1, posterior$enroll["shape"], posterior$enroll["rate"])
  cfg$dropout_rate <- rgamma(1, posterior$dropout["shape"], posterior$dropout["rate"])
  cfg
}
