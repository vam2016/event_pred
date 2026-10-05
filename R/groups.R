# Known groups: independently fitted/drawn group models, exact joint trial paths.
group_config <- function(cfg, group) {
  out <- cfg
  allowed <- c("future_n", "enroll_rate", "enroll_mode", "enroll_cuts", "enroll_rates", "dropout_rate", "multiplier", "lag", "cuts", "tail_rate")
  for (id in intersect(names(group), allowed)) out[[id]] <- group[[id]]
  out$analysis_mode <- "pooled"; out$groups <- NULL
  out$methods <- if (cfg$input_mode == "parameters") group$method else if(!is.null(group$fit_method)&&nzchar(group$fit_method)) group$fit_method else if(!is.null(cfg$common_methods)) cfg$common_methods else cfg$methods
  out
}
validate_groups <- function(data, cfg) {
  if (!is.list(cfg$groups) || length(cfg$groups) < 2 || length(cfg$groups) > 6 || is.null(names(cfg$groups)) || any(!nzchar(names(cfg$groups))) || anyDuplicated(names(cfg$groups))) stop("分组预测需为 2–6 个命名组提供参数。")
  labels <- vapply(cfg$groups, function(g) if (is.character(g$name) && length(g$name)==1 && !is.na(g$name)) g$name else "", character(1))
  if (any(!nzchar(trimws(labels))) || anyDuplicated(labels)) stop("组名必须非空且唯一。")
  if (!"group" %in% names(data) || anyNA(data$group) || any(!nzchar(trimws(as.character(data$group)))) || any(!data$group %in% labels)) stop("每条分析记录都必须有已配置的非缺失组别。")
  subsets <- lapply(labels, function(g) validate_data(data[data$group==g,,drop=FALSE],cfg$cut,require_events=cfg$input_mode!="parameters",gap_mode=cfg$gap_mode))
  names(subsets) <- names(cfg$groups)
  future <- vapply(cfg$groups,function(g) g$future_n,numeric(1))
  if (sum(future)>1000 || nrow(data)>5000) stop("分组总人数限制：当前≤5000，未来入组合计≤1000。")
  for (id in names(cfg$groups)) validate_config(group_config(cfg,cfg$groups[[id]]),subsets[[id]])
  subsets
}
curve_from_counts <- function(mat, grid, label, method, target) {
  q <- apply(mat,2,quantile_with_inf)
  data.frame(model=label,method=method,day=grid,mean=colMeans(mat),lower=q[1,],median=q[2,],upper=q[3,],probability=colMeans(mat>=target),probability_mcse=probability_mcse(list(mat),target))
}
run_grouped_forecast <- function(data,cfg,progress=function(value,detail)NULL,models_override=NULL) {
  data <- validate_data(data,cfg$cut,require_events=cfg$input_mode!="parameters",gap_mode=cfg$gap_mode)
  subsets <- validate_groups(data,cfg)
  cfg$future_n <- sum(vapply(cfg$groups,`[[`,numeric(1),"future_n"))
  validate_config(cfg,data)
  set.seed(cfg$seed)
  group_models <- list(); group_process <- list(); failures <- character(); weights <- list(); flat <- list()
  for (id in names(subsets)) {
    gc <- group_config(cfg,cfg$groups[[id]]); dd <- subsets[[id]]; label <- cfg$groups[[id]]$name
    fit <- if (!is.null(models_override)) list(models=models_override[[id]],failures=character()) else fit_candidates(dd,gc$methods,gc$cuts,gc$tail_rate)
    if (!length(fit$models)) stop(paste(label,"没有有效模型。"))
    if (length(fit$failures)) failures[paste(label,names(fit$failures),sep=" / ")] <- fit$failures
    if (cfg$uncertainty=="bayes_weibull") {
      progress(0,paste(label,"Weibull MCMC")); fit$models$weibull <- fit_bayesian_weibull(dd,gc)
      if (!fit$models$weibull$posterior$passed) stop(paste(label,"MCMC 诊断未通过；请增加采样或调整先验。"))
    }
    group_models[[id]] <- fit$models; group_process[id] <- list(process_posterior(dd,gc))
    eligible <- names(fit$models)[vapply(fit$models,function(m)is.finite(m$aic),logical(1))]
    if (length(eligible)) {
      aic <- vapply(fit$models[eligible],`[[`,numeric(1),"aic"); w <- exp(-.5*(aic-min(aic))); weights[[id]] <- w/sum(w)
    }
    for (method in names(fit$models)) {
      m <- fit$models[[method]]; m$group <- label; m$label <- paste(label,m$label,sep=" / "); flat[[paste(id,method,sep="__")]] <- m
    }
  }
  scenarios <- if (cfg$input_mode=="parameters") "group_parameters" else Reduce(intersect,lapply(group_models,names))
  if (cfg$input_mode=="adtte" && !length(scenarios) && all(lengths(group_models)==1)) scenarios <- "group_selected"
  if (cfg$input_mode=="adtte" && isTRUE(cfg$ensemble) && any(lengths(weights)>1) && length(weights)==length(subsets)) scenarios <- c(scenarios,"ensemble")
  if (!length(scenarios)) stop("各组没有共同成功候选，且无法构建 AIC 组合；请调整模型或数据。")
  if (cfg$input_mode=="adtte") {
    requested <- Reduce(intersect,lapply(cfg$groups,function(g)group_config(cfg,g)$methods))
    excluded <- setdiff(requested,Reduce(intersect,lapply(group_models,names)))
    for (m in excluded) failures[paste("总体",m,sep=" / ")] <- "至少一组拟合失败，未计算同模型的总体预测；成功组模型仍可参与组内 AIC 组合。"
  }
  grid <- seq(cfg$cut,cfg$cut+cfg$horizon,length.out=121)
  curves <- list(); summary <- list(); counts <- list(); milestones <- list(); group_counts <- list(); group_curves <- list(); group_summary <- list(); diagnostics <- list(); all_dates <- list()
  for (scenario in scenarios) {
    label <- if (scenario=="ensemble") "各组 AIC 独立组合" else if (scenario=="group_selected") "各组指定模型" else if (scenario=="group_parameters") "各组给定分布" else model_catalog()$label[match(scenario,model_catalog()$id)]
    mats <- setNames(lapply(subsets,function(x)matrix(NA_real_,cfg$sims,length(grid))),names(subsets)); mat <- matrix(NA_real_,cfg$sims,length(grid)); hit <- rep(Inf,cfg$sims); dates_bank <- vector("list",cfg$sims); failed <- 0L; failure_reasons <- character()
    for (b in seq_len(cfg$sims)) {
      trial <- tryCatch({
        paths <- list()
        for (id in names(subsets)) {
          gc <- group_config(cfg,cfg$groups[[id]])
          method <- if (scenario=="ensemble") sample(names(weights[[id]]),1,prob=weights[[id]]) else if (scenario %in% c("group_parameters","group_selected")) names(group_models[[id]])[[1]] else scenario
          draw <- group_models[[id]][[method]]
          if (cfg$uncertainty=="bootstrap") draw <- fit_model(subsets[[id]][sample.int(nrow(subsets[[id]]),replace=TRUE),,drop=FALSE],method,gc$cuts,gc$tail_rate)
          else if (cfg$uncertainty=="gamma") draw <- gamma_posterior_draw(draw,cfg$prior_shape,cfg$prior_rate)
          else if (cfg$uncertainty=="bayes_weibull") draw <- posterior_model_draw(draw)
          paths[[id]] <- simulate_trial(subsets[[id]],draw,process_draw(gc,group_process[[id]]))[[cfg$clock]]
        }
        paths
      },error=function(e)e)
      if (inherits(trial,"error")) { failed <- failed+1L; failure_reasons <- c(failure_reasons,conditionMessage(trial)); next }
      dates <- sort(unlist(trial,use.names=FALSE)); mat[b,] <- findInterval(grid,dates); dates_bank[[b]] <- dates
      for (id in names(subsets)) mats[[id]][b,] <- findInterval(grid,trial[[id]])
      if (length(dates)>=cfg$target && dates[cfg$target]<=max(grid)) hit[b] <- dates[cfg$target]
      if (b%%25==0) progress((match(scenario,scenarios)-1+b/cfg$sims)/length(scenarios),paste(label,b,"/",cfg$sims))
    }
    ok <- !is.na(mat[,1]); if (sum(ok)<.9*cfg$sims) stop(paste(label,"超过10%联合模拟失败，请检查各组拟合。"))
    mat <- mat[ok,,drop=FALSE]; hit <- hit[ok]; dates_bank <- dates_bank[ok]
    curves[[scenario]] <- curve_from_counts(mat,grid,label,scenario,cfg$target); q <- quantile_with_inf(hit)
    summary[[scenario]] <- data.frame(model=label,method=scenario,reached=mean(is.finite(hit)),reached_mcse=tail(curves[[scenario]]$probability_mcse,1),lower_day=q[1],median_day=q[2],upper_day=q[3],median_events_end=quantile_with_inf(mat[,length(grid)])[2],simulations=length(hit))
    counts[[scenario]] <- mat; milestones[[scenario]] <- hit; all_dates[[scenario]] <- dates_bank
    diagnostics[[scenario]] <- data.frame(method=scenario,model=label,aic=NA_real_,simulations=nrow(mat),failed=failed,note=if(failed) paste("联合拟合/模拟失败已计数，结果条件于成功轮次。",paste(head(unique(failure_reasons),3),collapse="；")) else "各组独立生成；逐轮相加计数并合并事件日期。")
    group_counts[[scenario]] <- list()
    for (id in names(subsets)) {
      gm <- mats[[id]][ok,,drop=FALSE]; group_counts[[scenario]][[id]] <- gm
      gc <- curve_from_counts(gm,grid,label,scenario,cfg$target); gc <- gc[,setdiff(names(gc),c("probability","probability_mcse"))]; gc$group <- cfg$groups[[id]]$name; group_curves[[paste(scenario,id)]] <- gc
      eq <- quantile_with_inf(gm[,length(grid)])
      group_summary[[paste(scenario,id)]] <- data.frame(group=cfg$groups[[id]]$name,model=label,method=scenario,known_events=sum(subsets[[id]]$event),active=sum(subsets[[id]]$status=="active"),future_n=cfg$groups[[id]]$future_n,mean_events_end=mean(gm[,length(grid)]),lower_events_end=eq[1],median_events_end=eq[2],upper_events_end=eq[3],simulations=nrow(gm))
    }
  }
  weight_table <- if(length(weights)) do.call(rbind,lapply(names(weights),function(id)data.frame(group=cfg$groups[[id]]$name,method=names(weights[[id]]),weight=unname(weights[[id]])))) else list()
  group_fit <- do.call(rbind,lapply(flat,function(m)data.frame(group=m$group,model=m$label,aic=m$aic,note=paste(m$warning,collapse="；"))))
  known <- sum(data$event); capacity <- known+sum(data$status=="active")+cfg$future_n
  list(curves=do.call(rbind,curves),summary=do.call(rbind,summary),diagnostics=do.call(rbind,diagnostics),models=flat,counts=counts,milestones=milestones,weights=weight_table,failures=failures,config=cfg,known_events=known,potential_events=capacity,process_posterior=NULL,group_process_posterior=group_process,group_models=group_models,group_fit=group_fit,group_counts=group_counts,group_curves=do.call(rbind,group_curves),group_summary=do.call(rbind,group_summary),event_dates=all_dates,
    mcse_scope=if(cfg$uncertainty=="bayes_weibull") "各组联合轨迹；条件于每组有限MCMC后验库，不含链采样误差" else "各组联合轨迹独立生成；条件于数据、设定及成功轮次；AIC组合直接抽模型并生成新轨迹",
    data_summary=list(n=nrow(data),events=known,active=sum(data$status=="active"),dropout=sum(data$status=="dropout")))
}
