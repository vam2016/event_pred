library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
source("R/units.R", local = TRUE)
source("R/help.R", local = TRUE)
source("R/models.R", local = TRUE)
source("R/forecast.R", local = TRUE)
source("R/inputs.R", local = TRUE)
source("R/bayes.R", local = TRUE)
source("R/groups.R", local = TRUE)


core_version <- "1.0.0-rc.1"
core_models <- c("exponential", "weibull", "pwe")
catalog <- model_catalog(); catalog <- catalog[catalog$id %in% core_models,,drop=FALSE]
choices <- setNames(catalog$id, catalog$label)
parameter_choices <- choices
theme <- bs_theme(version=5,bg="#f3f6f8",fg="#173d50",primary="#126b72",secondary="#607685",
  base_font=font_collection("Helvetica Neue","PingFang SC","Microsoft YaHei","sans-serif"))
section <- function(title,...) card(card_header(title),...,fill=FALSE)
fields <- function(...) layout_columns(...,col_widths=6)
source("R/group_ui.R",local=TRUE)
source("R/parameter_ui.R",local=TRUE)
source("R/parameter_server.R",local=TRUE)
source("R/handbook.R",local=TRUE)
source("R/simulation.R",local=TRUE)
source("R/simulation_ui.R",local=TRUE)
source("R/core_simulation_server.R",local=TRUE)
source("R/core_workflow.R",local=TRUE)
source("R/core_ui.R",local=TRUE)
source("R/core_handbook.R",local=TRUE)

ui <- core_ui()

server <- function(input, output, session) {
  result <- reactiveVal(NULL); run_error <- reactiveVal(NULL)
  parse_numbers <- function(x) {
    if (is.null(x) || !nzchar(trimws(x))) return(numeric())
    y <- suppressWarnings(as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]])))
    if (anyNA(y) || any(!is.finite(y))) stop("逗号分隔的参数必须全部为有限数值。")
    y
  }
  current_unit <- reactiveVal("months")
  input_unit <- reactive(input$time_unit %||% "days")
  register_parameter_controls(input,output,session,input_unit)
  register_core_simulation_server(input,output,session)
  register_core_workflow_server(input,output,session,result)
  output$input_context <- renderUI({
    u <- time_label(input_unit()); div(class="context-strip",
      div(span("输入方式"),strong(if(input$input_mode=="adtte")"ADTTE" else "参数")),
      div(span("组别处理"),strong(if(identical(input$analysis_mode,"grouped"))"已知组别" else "合并")),
      div(span("当前 DCO"),strong(paste(input$cut %||% 0,u))),div(span("时间单位"),strong(u)))
  })
  unit_labels <- function(unit) {
    u <- time_label(unit)
    c(cut = paste0("IA / 当前 DCO（自研究起点起的", u, "数）"), duration = paste0("已无事件随访时间 a / 最大值（",u,"）"),
      eta = paste0("Weibull 尺度 eta（",u,"）"), exp_rate = paste0("指数风险率 lambda（每",u,"）"), drop_period = paste0("脱落概率对应窗口（",u,"）"), log_mu = paste0("对数时间位置 mu = log(m/",u,")"),
      median = paste0("中位事件时间 m（",u,"）"), median2 = paste0("第二成分 mPFS / mOS（",u,"）"),
      g_rate = paste0("初始风险率 b（每",u,"）"), g_shape = paste0("Gompertz 形状 g（每",u,"）"),
      cuts = paste0("PWE 切点（随访",u,"数）"), parameter_cuts = paste0("风险切点（随访",u,"数，逗号分隔）"),
      parameter_rates = paste0("各段风险率 lambda（每",u,"，数目 = 切点数 + 1）"), tail_rate = paste0("KM 指数尾部风险率（每",u,"）"),
      prior_rate = paste0("事件 Gamma 先验 rate（受试者·",u,"）"), log_eta_mean = paste0("log(Weibull 尺度 eta/",u,") 先验均值"),
      enroll_rate = paste0("入组率 r（人/",u,"）"), enroll_cuts = paste0("入组切点（当前 DCO 后的",u,"数）"), enroll_rates = paste0("各段入组率（人/",u,"）"),
      dropout_rate = paste0("独立永久脱落风险率 mu（每",u,"）"), lag = paste0("新增事件上报延迟 L（",u,"）"),
      recruit_start = paste0("历史入组窗口起点（研究",u,"数）"), recruit_end = paste0("历史入组窗口终点（研究",u,"数）"),
      enroll_prior_rate = paste0("入组 Gamma 先验 rate（",u,"）"), drop_prior_rate = paste0("脱落 Gamma 先验 rate（受试者·",u,"）"),
      horizon = paste0("当前 DCO 后预测窗口（",u,"）"), age = paste0("已无事件随访时间 a（",u,"）"), lab_horizon = paste0("剩余时间 U 的窗口（",u,"）"),
      backtest_cuts = paste0("回测 DCO（研究",u,"数，逗号分隔）"),
      backtest_enroll_rates = paste0("各回测截点的历史恒定入组率（人/",u,"）"))
  }
  observeEvent(input$time_unit, {
    to <- input$time_unit; from <- current_unit()
    ids <- unlist(unit_fields, use.names = FALSE)
    vals <- setNames(lapply(ids, function(id) input[[id]]), ids)
    vals <- vals[!vapply(vals, is.null, logical(1))]
    converted <- tryCatch(convert_unit_inputs(vals, from, to), error = function(e) {
      showNotification(paste("单位未切换：", conditionMessage(e)), type = "error"); NULL
    })
    if (is.null(converted)) { updateSelectInput(session, "time_unit", selected = from); return() }
    labels <- unit_labels(to)
    for (i in 1:6) labels <- c(labels,setNames(unit_labels(to),paste0("g",i,"_",names(unit_labels(to)))))
    for (id in names(converted)) {
      if (from != to) freezeReactiveValue(input, id)
      label <- parameter_label(id, labels[[id]])
      if (id %in% c(unit_fields$duration_text, unit_fields$rate_text)) updateTextInput(session, id, label = label, value = converted[[id]])
      else {
        limits <- if (id %in% c("horizon", "lab_horizon")) list(min = 1 / time_factor(to), max = 3650 / time_factor(to)) else list()
        do.call(updateNumericInput, c(list(session = session, inputId = id, label = label, value = converted[[id]]), limits))
      }
    }
    current_unit(to)
  }, ignoreInit = FALSE)
  raw <- reactive({ req(input$file)
    if (tolower(tools::file_ext(input$file$name))!="csv") stop("1.0 数据入口使用 ADTTE CSV。")
    read_adtte(input$file$datapath, input$file$name)
  })
  output$endpoint_ui <- renderUI({
    x <- tryCatch(raw(), error = function(e) NULL)
    vals <- if (is.null(x) || !"PARAMCD" %in% names(x)) character() else unique(na.omit(as.character(x$PARAMCD)))
    selectInput("paramcd", "终点 PARAMCD", vals)
  })
  output$flag_ui <- renderUI({
    x <- tryCatch(raw(), error = function(e) NULL)
    vals <- if (is.null(x)) character() else names(x)[grepl("FL$", names(x))]
    selectInput("analysis_flag", "分析标志变量（可不筛选）", c("不筛选" = "", setNames(vals, vals)))
  })
  output$group_column_ui <- renderUI({
    x <- tryCatch(raw(),error=function(e)NULL)
    vals <- if(is.null(x)) character() else names(x)[vapply(x,function(z)is.character(z)||is.factor(z)||is.numeric(z),logical(1))]
    vals <- c(intersect(c("TRTP","TRTA"),vals),setdiff(vals,c("TRTP","TRTA")))
    selectInput("group_column","分析组别变量",setNames(vals,vals),selected=if("TRTP" %in% vals) "TRTP" else "")
  })
  group_slots <- reactive({
    if ((input$analysis_mode %||% "pooled")!="grouped") return(character())
    if(input$input_mode=="parameters") {
      n <- input$group_count %||% 2
      validate(need(is.finite(n) && n==floor(n) && n==2,"1.0 已知组别模式使用两组。"))
      return(LETTERS[seq_len(n)])
    }
    x <- raw(); req(input$paramcd,input$group_column)
    validate(need(input$group_column %in% names(x),"组别变量不存在。"))
    rows <- !is.na(x$PARAMCD)&x$PARAMCD==input$paramcd
    if(nzchar(input$analysis_flag %||% "")) rows <- rows & !is.na(x[[input$analysis_flag]]) & x[[input$analysis_flag]]==input$flag_value
    g <- as.character(x[[input$group_column]][rows])
    validate(need(!anyNA(g)&&all(nzchar(trimws(g))),"筛选后组别不能缺失或为空。"))
    g <- sort(unique(g)); validate(need(length(g)==2,"1.0 已知组别模式筛选后需恰好两组。")); g
  })
  output$group_inputs <- renderUI({
    labels <- group_slots(); do.call(navset_card_tab,lapply(seq_along(labels),function(i) nav_panel(paste("组",i,"·",labels[i]),group_input_card(i,labels[i],input$input_mode,isolate(input_unit())))))
  })
  build_inputs <- function() {
    am <- input$analysis_mode %||% "pooled"
    cfg <- list(common_methods = input$methods, analysis_mode = am %||% "pooled", input_mode = input$input_mode, cut = input$cut, horizon = input$horizon, target = if (identical(input$task,"count")) 1 else input$target,
      methods = if (input$input_mode == "parameters") input$design_model else input$methods,
      cuts = if(input$input_mode=="adtte" && am!="grouped" && "pwe" %in% input$methods)parse_numbers(input$cuts) else numeric(), tail_rate = if(input$input_mode=="adtte" && "km_tail" %in% input$methods)input$tail_rate else .002,
      uncertainty = if (input$input_mode == "parameters") "plugin" else "plugin",
      prior_shape = if(input$input_mode=="adtte" && "plugin"=="gamma")input$prior_shape else .5, prior_rate = if(input$input_mode=="adtte" && "plugin"=="gamma")input$prior_rate else 50/time_factor(input_unit()),
      ensemble = input$input_mode == "adtte" && FALSE,
      future_n = if(am=="grouped")0 else input$future_n, enroll_rate = if(am=="grouped" || input$future_n==0)0 else input$enroll_rate, dropout_rate = if(am=="grouped")0 else dropout_input_rate(list(drop_input=input$drop_input,dropout_rate=input$dropout_rate,drop_prob=input$drop_prob,drop_period=input$drop_period)),
      multiplier = if(am=="grouped")1 else 1, lag = if(am!="grouped" && "occurred"=="reported")input$lag else 0, clock = "occurred", sims = input$sims, seed = input$seed,
      origin = as.character(input$origin), gap_mode = if (input$input_mode == "parameters") "strict" else "strict",
      enroll_mode = if(am=="grouped" || input$future_n==0)"constant" else input$enroll_mode, enroll_cuts = if(am!="grouped" && input$enroll_mode=="piecewise" && input$future_n>0)parse_numbers(input$enroll_cuts) else numeric(), enroll_rates = if(am!="grouped" && input$enroll_mode=="piecewise" && input$future_n>0)parse_numbers(input$enroll_rates) else numeric(),
      process_uncertainty = if (input$input_mode == "parameters") "fixed" else "fixed",
      recruit_start = input$recruit_start, recruit_end = input$recruit_end,
      enroll_prior_shape = input$enroll_prior_shape, enroll_prior_rate = input$enroll_prior_rate,
      drop_prior_shape = input$drop_prior_shape, drop_prior_rate = input$drop_prior_rate,
      log_eta_mean = input$log_eta_mean, log_eta_sd = input$log_eta_sd, log_shape_mean = input$log_shape_mean, log_shape_sd = input$log_shape_sd,
      mcmc_chains = input$mcmc_chains, mcmc_warmup = input$mcmc_warmup, mcmc_draws = input$mcmc_draws)
    cfg$task <- input$task %||% "target"
    # Parse only branches used by this task.
    if (input$input_mode=="parameters" || !any(cfg$methods %in% c("pwe"))) cfg$cuts <- numeric()
    if (cfg$enroll_mode!="piecewise" || cfg$future_n==0) {cfg$enroll_cuts<-numeric();cfg$enroll_rates<-numeric()}
    if (cfg$clock!="reported") cfg$lag<-0
    entered_config <- cfg
    cfg <- unit_config_to_days(cfg, input_unit())
    cfg$entered_config <- entered_config
    cfg <- complete_config(cfg)
    override <- NULL
    if (input$input_mode == "parameters" && cfg$analysis_mode != "grouped") {
      d <- parameter_cohort(cfg$cut, input$active_n, input$known_n, input$duration * time_factor(input_unit()), input$age_mode, input$seed)
      ids <- names(group_input_defaults(input_unit()))
      v <- setNames(lapply(ids,function(id)input[[id]]),ids)
      m <- parameter_input_model(input$design_model,v,input_unit())
      override <- setNames(list(m),m$id)
      cfg$entered_parameters <- c(v,list(time_unit=input_unit()))
      cfg$parameter_model <- list(method=m$id,params=m$params,cuts=m$cuts,input_basis=m$input_basis)
      cfg$cohort_parameters <- list(active_n = input$active_n, known_n = input$known_n, duration = input$duration * time_factor(input_unit()), age_mode = input$age_mode)
    } else if (input$input_mode == "adtte") {
      if (is.null(input$file)) stop("请选择 ADTTE 文件，或切换为参数输入。")
      d <- normalize_adtte(raw(), input$paramcd, cfg$origin, cfg$cut, as.numeric(input$offset), parse_numbers(input$dropout_codes),
        input$analysis_flag %||% "", input$flag_value, input$date_encoding, "strict", input$aval_unit %||% "days",group_column=if(cfg$analysis_mode=="grouped")input$group_column else NULL)
      cfg$adtte_mapping <- list(paramcd = input$paramcd, offset = as.numeric(input$offset), dropout_codes = parse_numbers(input$dropout_codes),
        flag = input$analysis_flag, flag_value = input$flag_value, date_encoding = input$date_encoding, aval_unit = input$aval_unit %||% "days",group_column=if(cfg$analysis_mode=="grouped")input$group_column else NULL)
    }
    if(cfg$analysis_mode=="grouped") {
      labels <- group_slots(); cfg$groups <- list(); override <- if(input$input_mode=="parameters") list() else NULL; cohorts <- list()
      for(i in seq_along(labels)) {
        key <- paste0("g",i); defaults <- group_input_defaults(input_unit(),i)
        val <- function(id) input[[paste0(key,"_",id)]] %||% defaults[[id]]
        gg <- list(name=if(input$input_mode=="parameters")val("name") else labels[i],future_n=val("future_n"),enroll_mode=val("enroll_mode"),enroll_rate=val("enroll_rate"),enroll_cuts=if(val("future_n")>0 && val("enroll_mode")=="piecewise")parse_numbers(val("enroll_cuts")) else numeric(),enroll_rates=if(val("future_n")>0 && val("enroll_mode")=="piecewise")parse_numbers(val("enroll_rates")) else numeric(),dropout_rate=dropout_input_rate(setNames(lapply(names(defaults),val),names(defaults))),multiplier=1,lag=if("occurred"=="reported")val("lag") else 0,cuts=if(input$input_mode=="adtte" && (val("fit_method")=="pwe" || (!nzchar(val("fit_method")) && "pwe" %in% input$methods)))parse_numbers(val("cuts")) else numeric(),tail_rate=if(input$input_mode=="adtte" && (val("fit_method")=="km_tail" || (!nzchar(val("fit_method")) && "km_tail" %in% input$methods)))val("tail_rate") else .002)
        if(gg$future_n==0){gg$enroll_mode<-"constant";gg$enroll_rate<-0}
        gg$fit_method <- val("fit_method"); entered <- gg; gg <- unit_config_to_days(gg,input_unit()); gg$entered_config <- entered
        if(input$input_mode=="parameters") {
          dd <- parameter_cohort(cfg$cut,val("active_n"),val("known_n"),val("duration")*time_factor(input_unit()),val("age_mode"),as.integer((cfg$seed+i)%%.Machine$integer.max))
          if(nrow(dd)) dd$id <- paste(key,dd$id,sep="_"); dd$group <- rep(gg$name,nrow(dd)); cohorts[[key]] <- dd
          f <- time_factor(input_unit()); vv <- setNames(lapply(names(defaults),val),names(defaults))
          m <- parameter_input_model(val("design_model"),vv,input_unit()); gg$method <- m$id
          gg$cohort_parameters <- list(active_n=val("active_n"),known_n=val("known_n"),duration=val("duration")*f,age_mode=val("age_mode"))
          gg$parameter_model <- list(method=m$id,params=m$params,cuts=m$cuts,input_basis=m$input_basis)
          gg$entered_parameters <- lapply(names(defaults),val); names(gg$entered_parameters) <- names(defaults)
          override[[key]] <- setNames(list(m),m$id)
        }
        cfg$groups[[key]] <- gg
      }
      if(input$input_mode=="parameters") { d <- do.call(rbind,cohorts); cfg$methods <- unique(vapply(cfg$groups,`[[`,character(1),"method")) }
      if(input$input_mode=="adtte") cfg$methods <- unique(unlist(lapply(cfg$groups,function(g) if(nzchar(g$fit_method)) g$fit_method else input$methods)))
      cfg$future_n <- sum(vapply(cfg$groups,`[[`,numeric(1),"future_n"))
      cfg$enroll_rate <- sum(vapply(cfg$groups,`[[`,numeric(1),"enroll_rate")); cfg$enroll_mode <- "constant"
      cfg$dropout_rate <- 0; cfg$multiplier <- 1; cfg$lag <- 0 # Actual values reside in each group configuration.
    }
    if (!cfg$task %in% c("count","target")) stop("请先选择事件数或达标日期任务。")
    if (!length(cfg$methods) || any(!cfg$methods %in% core_models)) stop("1.0 使用指数、Weibull 或 PWE 模型。")
    cfg$core_version <- core_version
    list(data = d, config = cfg, override = override)
  }
  observeEvent(input$run, {
    run_error(NULL)
    tryCatch({
      args <- build_inputs()
      r <- withProgress(message = "事件预测", value = 0,
        run_forecast(args$data, args$config, function(v, d) setProgress(value = v, detail = d), args$override))
      r$observed_data <- args$data; r$models_override <- args$override
      r$created_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      r$core_version <- core_version; result(r)
      nav_select("forecast_tabs","main",session=session)
      nav_select("nav", "forecast", session = session)
    }, error = function(e) { run_error(conditionMessage(e)); showNotification(conditionMessage(e), type = "error", duration = 15) })
  }, ignoreInit = FALSE, ignoreNULL = TRUE)
  `%||%` <- function(x, y) if (is.null(x)) y else x
  display_date <- function(day, origin) vapply(day, function(x) if (is.finite(x)) as.character(as.Date(origin) + ceiling(x)) else "超过预测窗口", character(1))
  milestone_dates <- function(r) {
    if (r$config$input_mode == "parameters" && r$config$target <= r$known_events) rep("已达到；历史日期未提供", nrow(r$summary)) else display_date(r$summary$median_day, r$config$origin)
  }
  theme_chart <- function() theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.background = element_rect(fill = "white", colour = NA), legend.position = "bottom")
  chart_widget <- function(p, tooltip = "all") {
    g <- ggplotly(p, tooltip = tooltip)
    g$x$data <- lapply(g$x$data, function(t) { if (!is.null(t$name) && grepl("^\\(", t$name)) t$showlegend <- FALSE; t })
    plotly::layout(g, legend = list(orientation = "h", x = 0, y = -.23, xanchor = "left", yanchor = "top"),
      margin = list(l = 60, r = 20, b = 100, t = 20)) |> config(displaylogo = FALSE)
  }
  palette <- c("#126b72", "#22818b", "#ba7141", "#a55375", "#548054", "#777486", "#333147")
  output$run_label <- renderUI({ r <- result(); if (is.null(r)) span("尚未运行", class = "field-note") else span(paste(if (r$config$input_mode == "parameters") "参数输入" else paste("ADTTE", r$config$adtte_mapping$paramcd), "·", r$config$sims, "次模拟 ·", time_label(r$config$display_unit)), class = "run-label") })
  output$run_status <- renderUI({
    if (!is.null(run_error())) return(div(class = "notice error", run_error(), p("已有结果仍为上一次成功运行。")))
    r <- result(); if (is.null(r)) return(div(class = "empty-state", "在输入设置中填写参数或选择 ADTTE 文件，然后运行预测。"))
    notes <- "预测区间采用给定或拟合参数，未包含参数估计不确定性。"
    if (r$config$task != "count" && r$config$target > r$potential_events) notes <- c(notes, "目标大于当前事件数 + 当前仍随访人数 + 未来入组人数。")
    if (length(r$failures)) notes <- c(notes, paste(names(r$failures), r$failures, collapse = "；"))
    gaps <- sum(r$observed_data$status == "active" & r$observed_data$obs_day < r$config$cut - 1e-7)
    if (gaps) notes <- c(notes, paste(gaps, "名仍随访患者的末次确认日早于截点；未观察区间已模拟。"))
    if (!length(notes)) return(NULL)
    div(class = "notice", paste(notes, collapse = " "))
  })
  output$metrics <- renderUI({
    r <- result(); req(r)
    div(class = "result-strip", div(span("已记录事件"), strong(r$known_events)), div(span("当前仍随访"), strong(r$data_summary$active)),
      if(r$config$task!="count")div(span("目标事件"), strong(r$config$target)), div(span("预测窗口"), strong(paste(signif(r$config$horizon / time_factor(r$config$display_unit), 5), time_label(r$config$display_unit)))))
  })
  output$grouped_result <- renderText({r <- result(); if(!is.null(r)&&r$config$analysis_mode=="grouped") "yes" else "no"})
  outputOptions(output,"grouped_result",suspendWhenHidden=FALSE)
  output$group_result_note <- renderUI({r <- result(); req(r$group_summary); p(class="field-note","分组区间分别由联合模拟计算；总事件目标 D* 使用全部组累计事件之和。总体区间端点不由各组端点相加。")})
  output$group_event_plot <- renderPlotly({r <- result(); req(r$group_curves); d <- r$group_curves; d$date <- as.Date(r$config$origin)+d$day
    p <- ggplot(d,aes(date,median,colour=model,fill=model))+geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.12,colour=NA,show.legend=FALSE)+geom_line()+facet_wrap(~group)+labs(x="日期",y="各组累计事件数",colour=NULL,fill=NULL)+theme_chart(); chart_widget(p)})
  output$group_fit_table <- renderDT({r <- result(); req(r$group_fit); d <- r$group_fit; names(d) <- c("组别","拟合模型","组内AIC","信息"); tabular(d)})
  output$group_event_table <- renderDT({r <- result(); req(r$group_summary); d <- r$group_summary; names(d) <- c("组别","模型","方法","已记录事件","仍随访","计划入组","窗口末均值","2.5%事件数","窗口末中位数","97.5%事件数","模拟次数"); tabular(d)})
  output$event_plot <- renderPlotly({
    r <- result(); req(r); d <- r$curves; d$date <- as.Date(r$config$origin) + d$day
    p <- ggplot(d, aes(date, median, colour = model, fill = model, group = model)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .12, colour = NA, show.legend = FALSE) + geom_line(linewidth = .8) +
      (if(r$config$task!="count")geom_hline(yintercept = r$config$target, linetype = "dashed", colour = "#777486") else NULL) +
      scale_colour_manual(values = rep(palette,length.out=length(unique(d$model)))) + scale_fill_manual(values = rep(palette,length.out=length(unique(d$model)))) +
      labs(x = "日期", y = "累计事件数", colour = NULL, fill = NULL) + theme_chart()
    chart_widget(p, tooltip = c("x", "y", "colour", "ymin", "ymax"))
  })
  count_end <- function(r) {d<-r$curves;d[d$day==max(d$day),c("model","mean","lower","median","upper"),drop=FALSE]}
  output$count_end_table <- renderDT({r<-result();req(r);d<-count_end(r);names(d)<-c("模型","均值","2.5%事件数","中位事件数","97.5%事件数");tabular(d)})
  output$new_count_table <- renderDT({r<-result();req(r);d<-count_end(r)
    d[,c("mean","lower","median","upper")] <- d[,c("mean","lower","median","upper")] - r$known_events
    names(d)<-c("模型","新增均值","新增2.5%事件数","新增中位事件数","新增97.5%事件数");tabular(d)
  })
  export_config <- function(r) {c<-r$config;if(c$task=="count"){c$target<-NULL;c$entered_config$target<-NULL};c}
  output$prob_plot <- renderPlotly({
    r <- result(); req(r); d <- r$curves; d$date <- as.Date(r$config$origin) + d$day
    p <- ggplot(d, aes(date, probability, colour = model)) + geom_line(linewidth = .8) + scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
      scale_colour_manual(values = rep(palette,length.out=length(unique(d$model)))) + labs(x = "日期", y = "达标概率", colour = NULL) + theme_chart()
    chart_widget(p)
  })
  table_options <- list(scrollX = TRUE, language = list(search = "检索：", lengthMenu = "每页 _MENU_ 条", info = "第 _START_–_END_ 条，共 _TOTAL_ 条", infoEmpty = "0 条", zeroRecords = "无匹配记录", paginate = list(previous = "上一页", "next" = "下一页")))
  tabular <- function(d) {
    g <- datatable(d, rownames = FALSE, options = c(table_options, list(dom = "t")), escape = TRUE)
    num <- names(d)[vapply(d, is.numeric, logical(1))]
    for (name in num) g <- formatRound(g,name,if(all(is.na(d[[name]]) | d[[name]]==round(d[[name]])))0 else 3)
    g
  }
  output$milestones <- renderDT({
    r <- result(); req(r); d <- r$summary
    tabular(data.frame(模型 = d$model, 窗口内达标概率 = sprintf("%.1f%%", 100 * d$reached), `概率 MCSE（百分点）` = 100 * d$reached_mcse, 未达标比例 = sprintf("%.1f%%",100*(1-d$reached)), 日期中位数 = milestone_dates(r),
      `2.5%日期` = if (r$config$input_mode == "parameters" && r$config$target <= r$known_events) "历史日期未提供" else display_date(d$lower_day, r$config$origin),
      `97.5%日期` = if (r$config$input_mode == "parameters" && r$config$target <= r$known_events) "历史日期未提供" else display_date(d$upper_day, r$config$origin),
      窗口末事件数中位数 = d$median_events_end, check.names = FALSE))
  })
  output$mcse_note <- renderUI({ r <- result(); req(r); p(class = "field-note", paste("MCSE为概率估计的模拟标准误：", r$mcse_scope, "。它不是预测区间；边界概率下的0估计不代表真实概率已确定。")) })
  output$diagnostics <- renderDT({ r <- result(); req(r); d <- r$diagnostics[, c("model", "aic", "simulations", "failed", "note")]; names(d) <- c("模型", "AIC", "模拟次数", "排除次数", "信息"); tabular(d) })
  equivalent_table <- reactive({
    r <- result(); req(r)
    do.call(rbind,lapply(r$models,function(m) cbind(model=m$label,group=m$group %||% "合并",model_equivalents(m,r$config$display_unit))))
  })
  output$model_parameters <- renderDT({
    d <- equivalent_table(); d$value <- vapply(d$value,function(x)if(is.infinite(x))"∞" else format(signif(x,6),trim=TRUE),character(1))
    names(d) <- c("模型","组别","参数","值","单位","定义"); tabular(d)
  })
  output$parameters_download <- downloadHandler(filename=function()"equivalent_parameters.csv",content=function(file)write.csv(equivalent_table(),file,row.names=FALSE,na=""))
  output$extended_fit_table <- renderDT({
    r <- result(); req(r)
    models <- Filter(function(m)!is.null(m$fit_diagnostics),r$models)
    if(!length(models)) return(NULL)
    d <- do.call(rbind,lapply(models,function(m) {x <- m$fit_diagnostics; data.frame(模型=m$label,收敛编码=x$convergence,最大绝对梯度=x$max_abs_score,Hessian条件数=x$hessian_condition,起点数=x$starts,收敛起点=x$converged_starts)}))
    tabular(d)
  })
  output$weights <- renderUI({ r <- result(); req(r); if (!is.data.frame(r$weights)) return(NULL); if(!is.null(r$weights$group)) return(p(class="field-note",paste("各组 AIC 权重：",paste(sprintf("%s / %s %.1f%%",r$weights$group,r$weights$method,100*r$weights$weight),collapse=" · ")))); p(class = "field-note", paste("AIC 权重：", paste(sprintf("%s %.1f%%", r$weights$method, 100 * r$weights$weight), collapse = " · "))) })
  output$data_status <- renderUI({ r <- result(); if (is.null(r)) return(p("尚无运行记录。")); p(class = "field-note", if (r$config$input_mode == "parameters") "参数模式：以下为随访年龄与事件人数的计算记录。" else "数据模式：以下为筛选后的内部记录。") })
  output$data_table <- renderDT({ r <- result(); req(r); datatable(unit_table(r$observed_data, r$config$display_unit, c("entry", "time", "obs_day")), rownames = FALSE, escape = TRUE, options = c(table_options, list(pageLength = 10))) })
  output$fit_plot <- renderPlotly({
    r <- result(); req(r); d <- r$observed_data
    maxt <- if (nrow(d)) max(d$time) else 0
    t <- seq(0, maxt + r$config$horizon, length.out = 300)
    fitted <- do.call(rbind, lapply(r$models, function(m) data.frame(time = t / time_factor(r$config$display_unit), survival = model_survival(m, t), model = m$label,group=if(is.null(m$group))"合并" else m$group)))
    p <- ggplot(fitted, aes(time, survival, colour = model)) + geom_line(linewidth = .8) + scale_colour_manual(values = rep(palette,length.out=length(unique(fitted$model))))
    if (r$config$input_mode == "adtte" && r$config$analysis_mode != "grouped") {
      d$event <- as.integer(d$status == "event"); sf <- survival::survfit(survival::Surv(time, event) ~ 1, data = d)
      obs <- data.frame(time = c(0, sf$time) / time_factor(r$config$display_unit), survival = c(1, sf$surv))
      p <- p + geom_step(data = obs, aes(time, survival), inherit.aes = FALSE, colour = "#173d50") + geom_vline(xintercept = maxt / time_factor(r$config$display_unit), linetype = "dotted")
    }
    if(r$config$analysis_mode=="grouped") {
      if(r$config$input_mode=="adtte") {
        obs <- do.call(rbind,lapply(split(d,d$group),function(dd) {sf <- survival::survfit(survival::Surv(time,as.integer(status=="event"))~1,data=dd); data.frame(time=c(0,sf$time)/time_factor(r$config$display_unit),survival=c(1,sf$surv),group=dd$group[1])}))
        p <- p+geom_step(data=obs,aes(time,survival),inherit.aes=FALSE,colour="#173d50")
      }
      p <- p+facet_wrap(~group)
    }
    p <- p + labs(x = paste0("自风险起始日起的随访时间（", time_label(r$config$display_unit), "）"), y = "S(t)", colour = NULL) + theme_chart()
    chart_widget(p)
  })
  output$group_curves_download <- downloadHandler(filename="group_event_curves.csv",content=function(file){r <- result(); req(r$group_curves); write.csv(unit_export(r$group_curves,r$config$display_unit,"day"),file,row.names=FALSE)})
  output$group_summary_download <- downloadHandler(filename="group_event_summary.csv",content=function(file){r <- result(); req(r$group_summary); write.csv(r$group_summary,file,row.names=FALSE)})
  output$handbook_download <- downloadHandler(filename="event_pred_core_handbook.md",content=function(file)file.copy("docs/CORE_HANDBOOK.md",file,overwrite=TRUE),contentType="text/markdown; charset=utf-8")
  output$handbook_html_download <- downloadHandler(filename="event_pred_core_handbook.html",content=function(file)writeLines(core_handbook_standalone_html(),file,useBytes=TRUE),contentType="text/html; charset=utf-8")
  output$template <- downloadHandler(filename = "adtte_synthetic.csv", content = function(file) write.csv(adtte_template(), file, row.names = FALSE))
  output$curves_download <- downloadHandler(filename = "event_curves.csv", content = function(file) { r <- result(); req(r); d<-r$curves;if(r$config$task=="count")d<-d[,setdiff(names(d),c("probability","probability_mcse")),drop=FALSE];write.csv(unit_export(d, r$config$display_unit, "day"), file, row.names = FALSE) })
  output$summary_download <- downloadHandler(filename = "event_milestones.csv", content = function(file) { r <- result(); req(r); d <- r$summary; d$median_date <- milestone_dates(r); write.csv(unit_export(d, r$config$display_unit, c("lower_day", "median_day", "upper_day")), file, row.names = FALSE) })
  output$config_download <- downloadHandler(filename = "event_pred_config.json", content = function(file) { r <- result(); req(r); jsonlite::write_json(list(version = core_version, created_at = r$created_at, config = export_config(r), data_summary = r$data_summary, session = R.version.string), file, auto_unbox = TRUE, pretty = TRUE, digits = NA) })
  output$report_download <- downloadHandler(filename="event_pred_report.md",content=function(file) {
    r<-result();req(r);counts<-identical(r$config$task,"count");d<-count_end(r)
    lines<-c(if(counts)"# 未来事件数预测" else "# 目标事件日期预测","",paste("核心入口版本：",core_version),paste("运行时间：",r$created_at),paste("显示单位：",time_label(r$config$display_unit)),"","## 配置","","```json",jsonlite::toJSON(export_config(r),auto_unbox=TRUE,pretty=TRUE,digits=NA),"```","")
    if(counts)lines<-c(lines,"## 窗口末累计事件数","","|模型|均值|2.5%|中位数|97.5%|","|---|---:|---:|---:|---:|",sprintf("|%s|%.3f|%.3f|%.3f|%.3f|",d$model,d$mean,d$lower,d$median,d$upper))
    else lines<-c(lines,"## 目标日期","","|模型|达标概率|概率MCSE（百分点）|日期中位数|","|---|---:|---:|---|",sprintf("|%s|%.1f%%|%.3f|%s|",r$summary$model,100*r$summary$reached,100*r$summary$reached_mcse,milestone_dates(r)),paste("MCSE范围：",r$mcse_scope))
    writeLines(c(lines,"","已记录事件固定；区间为条件于给定或拟合参数的逐点预测区间，未包含参数估计不确定性。"),file,useBytes=TRUE)
  })

}
shinyApp(ui, server)
