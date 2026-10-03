library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
source("R/models.R", local = TRUE)
source("R/forecast.R", local = TRUE)

catalog <- model_catalog()
choices <- setNames(catalog$id, catalog$label)
theme <- bs_theme(version = 5, bg = "#f3f7fb", fg = "#142d46", primary = "#126c86", secondary = "#66788b",
  base_font = font_collection("Aptos", "PingFang SC", "Microsoft YaHei", "sans-serif"),
  heading_font = font_collection("Avenir Next", "PingFang SC", "sans-serif"))

ui <- page_navbar(title = tags$span("EVENT / PRED", class = "brand"), theme = theme,
  header = tagList(tags$head(tags$link(rel = "stylesheet", href = "style.css")),
    div(class = "masthead", div(class = "eyebrow", "临床试验 · 日历时间预测"),
      h1("下一次分析，何时成熟？"),
      p("从今天的随访状态出发，探索未来事件累积、目标日期与预测不确定性。"),
      div(class = "release-note", "v0.1 · 首版可运行原型 · 示例为合成数据 · 专门模型见开发路线"))),
  nav_panel("预测工作台", value = "forecast",
    layout_sidebar(
      sidebar = sidebar(width = 325, title = "预测设定", open = "desktop",
        fileInput("file", "受试者数据（CSV）", accept = ".csv"),
        helpText("未上传时使用 240 名合成受试者。单位统一为天。"),
        numericInput("cut", "数据截点（自研究起点的第几天）", 540, min = 1),
        dateInput("origin", "研究起点日期（用于换算日历日期）", "2025-01-01"),
        numericInput("target", "目标累计事件数", 180, min = 1, step = 1),
        numericInput("horizon", "截点后预测窗口（天）", 730, min = 1, max = 3650),
        checkboxGroupInput("methods", "比较模型", choices, selected = c("exponential", "weibull", "pwe")),
        textInput("cuts", "PWE 切点（随访天数，逗号分隔）", "90,180,365"),
        numericInput("tail_rate", "KM 超出观察范围后的风险率（每天）", 0.002, min = 0.000001, step = 0.0001),
        selectInput("uncertainty", "参数不确定性", c("固定拟合参数 + 未来随机性" = "plugin", "受试者 bootstrap + 未来随机性" = "bootstrap", "Gamma 共轭后验（仅指数 / PWE）" = "gamma")),
        conditionalPanel("input.uncertainty === 'gamma'",
          numericInput("prior_shape", "Gamma 先验 shape（每段）", 0.5, min = 0.01),
          numericInput("prior_rate", "Gamma 先验 rate（受试者·天）", 50, min = 0.01),
          helpText("默认先验均值 0.01/天；请结合疾病背景调整。")),
        checkboxInput("ensemble", "加入 AIC 加权预测混合", TRUE),
        tags$hr(),
        h5("未来过程"),
        numericInput("future_n", "计划继续入组人数", 60, min = 0, max = 1000, step = 1),
        numericInput("enroll_rate", "未来入组率（人/天）", 0.5, min = 0, step = 0.1),
        numericInput("dropout_rate", "未来独立脱落风险（每天）", 0.00025, min = 0, step = 0.0001),
        numericInput("multiplier", "未来事件风险倍数（情景）", 1, min = 0.01, step = 0.1),
        helpText("倍数调整截点后的风险增量，不改变已发生事件。不是治疗组间 HR。"),
        numericInput("lag", "未来事件固定上报延迟（天）", 0, min = 0),
        selectInput("clock", "目标按哪种事件日期计算", c("事件实际发生" = "occurred", "事件上报（未来延迟情景）" = "reported")),
        numericInput("sims", "模拟次数", 200, min = 50, max = 2000, step = 50),
        numericInput("seed", "随机种子", 20261003, min = 0, step = 1),
        actionButton("run", "运行事件预测", class = "btn-primary w-100"),
        downloadButton("template", "下载合成数据模板")),
      div(class = "work-area",
        uiOutput("run_status"), uiOutput("metrics"),
        card(full_screen = TRUE, card_header("事件累积轨迹", tags$span("中位数与逐点 95% 预测区间", class = "subtitle")),
          plotlyOutput("event_plot", height = "420px")),
        layout_columns(col_widths = c(7, 5),
          card(card_header("目标事件日期"), DTOutput("milestones"), p(class = "footnote", "未在窗口内达标的轮次保留在概率分母中；日期分位数若超出窗口，显示为窗口内未达到。")),
          card(card_header("在此之前达标的概率"), plotlyOutput("prob_plot", height = "310px"))),
        card(card_header("模型状态与解释"), DTOutput("diagnostics"), uiOutput("weights")),
        div(class = "export-bar", downloadButton("curves_download", "下载事件曲线 CSV"), downloadButton("summary_download", "下载目标日期 CSV"), downloadButton("config_download", "下载运行配置 JSON"), downloadButton("report_download", "下载预测摘要 Markdown")),
        p(class = "footnote", "导出基于最近一次成功运行的配置。调整左侧参数后请重新运行。上报延迟仅作用于未来事件，首版不处理截点时未知的历史未上报事件。")))),
  nav_panel("数据与拟合", value = "data",
    layout_columns(col_widths = c(7, 5),
      card(card_header("当前受试者数据"), DTOutput("data_table")),
      card(card_header("数据规则"), includeMarkdown("docs/DATA_CONTRACT.md"))),
    card(card_header("观察生存曲线与模型外推"), plotlyOutput("fit_plot", height = "440px"),
      p(class = "footnote", "横轴为自受试者入组起的随访时间。这里的生存拟合图与日历时间事件预测图使用不同的时间轴。"))),
  nav_panel("条件生存实验室", value = "lab",
    layout_columns(col_widths = c(4, 8),
      card(card_header("从已观察的状态继续"),
        selectInput("lab_model", "使用模型（最近一次运行的拟合）", choices = choices[1:3]),
        numericInput("age", "已无事件随访（天）", 180, min = 0),
        numericInput("lab_horizon", "观察未来（天）", 730, min = 1),
        numericInput("lab_multiplier", "未来风险倍数", 1, min = 0.01),
        uiOutput("conditional_formula"), tableOutput("conditional_quantiles")),
      card(card_header("剩余生存的条件分布"), plotlyOutput("conditional_plot", height = "440px"),
        p("改变已随访时间，观察后续风险和剩余时间分布。指数模型在风险倍数相同的条件下具有无记忆性。")))),
  nav_panel("方法与推导", value = "methods",
    div(class = "reading-layout", withMathJax(includeMarkdown("docs/METHODS.md")))),
  nav_panel("开发路线", value = "roadmap",
    div(class = "reading-layout", includeMarkdown("docs/DEVELOPMENT_PLAN.md"))),
  id = "nav", footer = div(class = "site-footer", "EVENT / PRED · 透明的假设，可复现的预测 · v0.1"))

server <- function(input, output, session) {
  result <- reactiveVal(NULL)
  run_error <- reactiveVal(NULL)
  uploaded <- reactive({
    if (is.null(input$file)) return(demo_data())
    read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  })
  observeEvent(input$uncertainty, {
    if (identical(input$uncertainty, "gamma")) {
      updateCheckboxGroupInput(session, "methods", selected = intersect(input$methods, c("exponential", "pwe")))
    }
  }, ignoreInit = TRUE)
  observeEvent(input$run, {
    run_error(NULL)
    tryCatch({
      cuts <- if (!nzchar(trimws(input$cuts))) numeric() else as.numeric(trimws(strsplit(input$cuts, ",", fixed = TRUE)[[1]]))
      cfg <- list(cut = input$cut, horizon = input$horizon, target = input$target, methods = input$methods,
        cuts = cuts, tail_rate = input$tail_rate, uncertainty = input$uncertainty,
        prior_shape = input$prior_shape, prior_rate = input$prior_rate, ensemble = input$ensemble,
        future_n = input$future_n, enroll_rate = input$enroll_rate, dropout_rate = input$dropout_rate,
        multiplier = input$multiplier, lag = input$lag, clock = input$clock,
        sims = input$sims, seed = input$seed, origin = as.character(input$origin))
      d <- uploaded()
      withProgress(message = "正在模拟未来事件", value = 0, {
        r <- run_forecast(d, cfg, function(value, detail) setProgress(value = value, detail = detail))
      })
      r$observed_data <- d
      r$created_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
      result(r)
      updateSelectInput(session, "lab_model", choices = setNames(names(r$models), vapply(r$models, `[[`, character(1), "label")))
    }, error = function(e) {
      run_error(conditionMessage(e))
      showNotification(conditionMessage(e), type = "error", duration = 15)
    })
  }, ignoreNULL = FALSE)
  display_date <- function(day, origin) {
    vapply(day, function(x) if (is.finite(x)) as.character(as.Date(origin) + ceiling(x)) else "窗口内未达到", character(1))
  }
  theme_chart <- function() theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.background = element_rect(fill = "white", colour = NA), legend.position = "bottom")
  palette <- c("#126c86", "#7457a7", "#bd6c25", "#278578", "#ba4d6b", "#66788b", "#163451")
  output$run_status <- renderUI({
    if (!is.null(run_error())) return(div(class = "notice error", strong("本次运行失败："), run_error(), p("下方如有结果，仍为最近一次成功运行。")))
    r <- result(); if (is.null(r)) return(div(class = "notice", "上传数据或使用示例，点击运行事件预测。"))
    notes <- character()
    if (r$config$target > r$potential_events) notes <- c(notes, "目标超过当前事件 + 仍在随访人数 + 未来入组人数，按本次假设无法达到。")
    if (length(r$failures)) notes <- c(notes, paste("部分模型未拟合：", paste(names(r$failures), r$failures, collapse = "；")))
    if (r$config$uncertainty == "plugin") notes <- c(notes, "当前区间仅包含未来事件、脱落和入组随机性，未包含拟合参数的不确定性。")
    if (r$config$uncertainty == "bootstrap") notes <- c(notes, "Bootstrap 仅传播事件模型拟合不确定性；入组率、脱落率、尾部和延迟仍是固定情景参数。")
    if (r$config$uncertainty == "gamma") notes <- c(notes, "Gamma 后验传播事件模型参数不确定性；其它未来过程仍按固定情景参数。")
    div(class = "notice", paste(notes, collapse = " "))
  })
  output$metrics <- renderUI({
    r <- result(); req(r)
    layout_columns(col_widths = c(3, 3, 3, 3),
      div(class = "metric", span("当前已观察事件"), strong(r$known_events)),
      div(class = "metric", span("仍在随访"), strong(r$data_summary$active)),
      div(class = "metric", span("目标累计事件"), strong(r$config$target)),
      div(class = "metric", span("预测窗口"), strong(paste(r$config$horizon, "天"))))
  })
  output$event_plot <- renderPlotly({
    r <- result(); req(r); d <- r$curves
    d$date <- as.Date(r$config$origin) + d$day
    p <- ggplot(d, aes(date, median, colour = model, fill = model, group = model)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.10, colour = NA, show.legend = FALSE) + geom_line(linewidth = 0.85) +
      geom_hline(yintercept = r$config$target, linetype = "dashed", colour = "#ad4860") +
      scale_colour_manual(values = palette) + scale_fill_manual(values = palette) +
      labs(x = "日历日期", y = "累计事件数", colour = NULL, fill = NULL) + theme_chart()
    g <- ggplotly(p, tooltip = c("x", "y", "colour", "ymin", "ymax"))
    g$x$data <- lapply(g$x$data, function(trace) {
      if (!is.null(trace$name) && grepl("^\\(", trace$name)) trace$showlegend <- FALSE
      trace
    })
    config(g, displaylogo = FALSE)
  })
  output$prob_plot <- renderPlotly({
    r <- result(); req(r); d <- r$curves; d$date <- as.Date(r$config$origin) + d$day
    p <- ggplot(d, aes(date, probability, colour = model)) + geom_line(linewidth = 0.8) +
      scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
      scale_colour_manual(values = palette) + labs(x = "日历日期", y = "达到目标的预测概率", colour = NULL) + theme_chart()
    ggplotly(p) |> config(displaylogo = FALSE)
  })
  output$milestones <- renderDT({
    r <- result(); req(r); d <- r$summary
    display <- data.frame(模型 = d$model, 窗口内达标概率 = sprintf("%.1f%%", 100 * d$reached),
      日期中位数 = display_date(d$median_day, r$config$origin),
      `95%区间下限` = display_date(d$lower_day, r$config$origin),
      `95%区间上限` = display_date(d$upper_day, r$config$origin),
      窗口末事件数中位数 = d$median_events_end, check.names = FALSE)
    datatable(display, rownames = FALSE, options = list(dom = "t", scrollX = TRUE), escape = TRUE)
  })
  output$diagnostics <- renderDT({
    r <- result(); req(r)
    datatable(r$diagnostics[, c("model", "aic", "simulations", "failed", "note")], rownames = FALSE,
      colnames = c("模型", "AIC", "成功模拟", "失败次数", "说明"), options = list(dom = "t", scrollX = TRUE), escape = TRUE) |> formatRound("aic", 2)
  })
  output$weights <- renderUI({
    r <- result(); req(r)
    if (!is.data.frame(r$weights)) return(NULL)
    p(class = "footnote", "AIC 混合权重：", paste(sprintf("%s %.1f%%", r$weights$method, 100 * r$weights$weight), collapse = " · "), "。这些是预测混合的启发式权重，不是贝叶斯后验模型概率；KM 不参与 AIC 加权。")
  })
  output$data_table <- renderDT({
    d <- uploaded()
    datatable(d, rownames = FALSE, escape = TRUE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$fit_plot <- renderPlotly({
    r <- result(); req(r); d <- r$observed_data; d$event <- as.integer(d$status == "event")
    sf <- survival::survfit(survival::Surv(time, event) ~ 1, data = d)
    observed <- data.frame(time = c(0, sf$time), survival = c(1, sf$surv))
    t <- seq(0, max(d$time) + r$config$horizon, length.out = 400)
    fitted <- do.call(rbind, lapply(r$models, function(m) data.frame(time = t, survival = model_survival(m, t), model = m$label)))
    p <- ggplot(fitted, aes(time, survival, colour = model)) + geom_line(linewidth = 0.8) +
      geom_step(data = observed, aes(time, survival), inherit.aes = FALSE, colour = "#142d46", linewidth = 1) +
      geom_vline(xintercept = max(d$time), linetype = "dotted") + scale_colour_manual(values = palette) +
      labs(x = "自入组起的随访天数", y = "无事件概率 S(t)", colour = NULL) + theme_chart()
    ggplotly(p) |> config(displaylogo = FALSE)
  })
  lab <- reactive({
    r <- result(); req(r, input$lab_model); m <- r$models[[input$lab_model]]; req(m)
    validate(need(is.finite(input$age) && input$age >= 0, "已随访天数必须非负。"),
      need(input$lab_horizon > 0 && input$lab_horizon <= 3650, "未来窗口应在 0–3650 天内。"),
      need(input$lab_multiplier > 0, "风险倍数必须为正。"),
      need(is.finite(model_cumhaz(m, input$age)), "所选模型在此时间已无生存支持。"))
    list(model = m, age = input$age, multiplier = input$lab_multiplier)
  })
  output$conditional_formula <- renderUI({
    withMathJax(p("给定已无事件至 a，剩余时间 U 的生存概率为："),
      p("$$P(U>u\\mid T>a)=\\exp[-q\\{H(a+u)-H(a)\\}].$$"))
  })
  output$conditional_plot <- renderPlotly({
    l <- lab(); t <- seq(0, input$lab_horizon, length.out = 300)
    d <- data.frame(remaining = t, probability = exp(-l$multiplier * (model_cumhaz(l$model, l$age + t) - model_cumhaz(l$model, l$age))))
    p <- ggplot(d, aes(remaining, probability)) + geom_line(colour = "#126c86", linewidth = 1) +
      labs(x = "从现在起的剩余天数", y = "条件无事件概率") + theme_chart()
    ggplotly(p) |> config(displaylogo = FALSE)
  })
  output$conditional_quantiles <- renderTable({
    l <- lab(); q <- sample_conditional(l$model, rep(l$age, 3), l$multiplier, u = c(0.975, 0.5, 0.025)) - l$age
    data.frame(剩余时间分位数 = c("2.5%", "50%", "97.5%"), 天数 = ifelse(is.finite(q), round(q, 1), "无限"))
  })
  output$template <- downloadHandler(filename = "event_pred_synthetic_template.csv", content = function(file) write.csv(demo_data(), file, row.names = FALSE))
  output$curves_download <- downloadHandler(filename = "event_curves.csv", content = function(file) { r <- result(); req(r); write.csv(r$curves, file, row.names = FALSE) })
  output$summary_download <- downloadHandler(filename = "event_milestones.csv", content = function(file) { r <- result(); req(r); d <- r$summary; d$median_date <- display_date(d$median_day, r$config$origin); write.csv(d, file, row.names = FALSE) })
  output$config_download <- downloadHandler(filename = "event_pred_config.json", content = function(file) {
    r <- result(); req(r)
    jsonlite::write_json(list(version = "0.1.0", created_at = r$created_at, config = r$config, data_summary = r$data_summary,
      session = R.version.string), file, auto_unbox = TRUE, pretty = TRUE)
  })
  output$report_download <- downloadHandler(filename = "event_pred_report.md", content = function(file) {
    r <- result(); req(r)
    lines <- c("# 事件数预测摘要", "", paste("运行时间（UTC）：", r$created_at), "", "## 配置", "", "```json",
      jsonlite::toJSON(r$config, auto_unbox = TRUE, pretty = TRUE), "```", "", "## 目标事件日期", "",
      "|模型|窗口内达标概率|日期中位数|", "|---|---:|---|",
      sprintf("|%s|%.1f%%|%s|", r$summary$model, 100 * r$summary$reached, display_date(r$summary$median_day, r$config$origin)),
      "", "## 假设与范围", "", "当前已发生事件固定；active 患者按条件生存继续预测；dropout 不再贡献未来事件。",
      "未来入组为恒定率 Poisson 过程，脱落为独立指数过程。风险倍数、上报延迟、入组率和脱落率为固定情景。",
      paste("参数不确定性方式：", r$config$uncertainty),
      "未在预测窗口内达到目标的轮次保留；上报延迟只作用于未来事件，不校正截点时的历史上报积压。",
      "AIC 加权是启发式预测混合，不是贝叶斯后验模型概率。区间为逐点预测区间。",
      "", "## 模型诊断", "", paste(r$diagnostics$model, r$diagnostics$note, sep = ": "))
    writeLines(lines, file, useBytes = TRUE)
  })
}
shinyApp(ui, server)
