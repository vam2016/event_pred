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
source("R/validation.R", local = TRUE)
source("R/groups.R", local = TRUE)

catalog <- model_catalog()
choices <- setNames(catalog$id, catalog$label)
parameter_choices <- setNames(parameter_catalog()$id, parameter_catalog()$label)
theme <- bs_theme(version = 5, bg = "#f3f6f8", fg = "#173d50", primary = "#126b72", secondary = "#607685",
  base_font = font_collection("Helvetica Neue", "PingFang SC", "Microsoft YaHei", "sans-serif"),
  heading_font = font_collection("Avenir Next", "PingFang SC", "sans-serif"))
section <- function(title, ...) card(card_header(title), ...,fill=FALSE)
fields <- function(...) layout_columns(..., col_widths = 6)
source("R/group_ui.R", local = TRUE)
source("R/parameter_ui.R", local = TRUE)
source("R/parameter_server.R", local = TRUE)
source("R/handbook.R", local = TRUE)
source("R/simulation.R", local = TRUE)
source("R/simulation_ui.R", local = TRUE)
source("R/simulation_server.R", local = TRUE)
source("R/joint_survival.R", local = TRUE)
source("R/joint_survival_ui.R", local = TRUE)
register_joint_help()
source("R/joint_survival_server.R", local = TRUE)
source("R/conditional_simulation.R", local = TRUE)
source("R/conditional_ui.R", local = TRUE)
register_conditional_help()
source("R/nph.R", local = TRUE)
source("R/sequential.R", local = TRUE)
source("R/sequential_inference.R", local = TRUE)
source("R/study.R", local = TRUE)
source("R/study_ui.R", local = TRUE)
source("R/study_server.R", local = TRUE)
source("R/sequential_server.R", local = TRUE)
source("R/conditional_prediction.R", local = TRUE)
source("R/ia_history.R", local = TRUE)
source("R/conditional_prediction_server.R", local = TRUE)
source("R/history_inference.R", local = TRUE)
source("R/history_inference_ui.R", local = TRUE)
register_history_inference_help()
source("R/history_inference_server.R", local = TRUE)
register_study_help()
source("R/parameter_calibration.R", local = TRUE)
source("R/batch_research.R", local = TRUE)
source("R/batch_analysis.R", local = TRUE)
source("R/batch_sequential.R", local = TRUE)
source("R/batch_workspace.R", local = TRUE)
source("R/joint_research.R", local = TRUE)
source("R/joint_marginal.R", local = TRUE)
source("R/joint_research_ui.R", local = TRUE)
register_joint_research_help()
source("R/joint_research_server.R", local = TRUE)
source("R/batch_workspace_ui.R", local = TRUE)
source("R/batch_workspace_server.R", local = TRUE)
source("R/batch_research_ui.R", local = TRUE)
register_batch_help()
source("R/batch_research_server.R", local = TRUE)
source("R/adaptation.R", local = TRUE)
source("R/patient_adaptation.R", local = TRUE)
source("R/patient_adaptive_prediction.R", local = TRUE)
source("R/patient_prediction_process.R", local = TRUE)
source("R/patient_adaptation_ui.R", local = TRUE)
register_patient_adaptive_help()
source("R/patient_adaptation_server.R", local = TRUE)
source("R/adaptation_ui.R", local = TRUE)
register_adaptive_help()
source("R/adaptation_server.R", local = TRUE)
source("R/adaptive_batch.R", local = TRUE)
source("R/adaptive_batch_ui.R", local = TRUE)
register_adaptive_batch_help()
source("R/adaptive_batch_server.R", local = TRUE)
source("R/research_runtime.R", local = TRUE)
source("R/conditional_batch.R", local = TRUE)
source("R/heterogeneity.R", local = TRUE)
source("R/research_workbench_ui.R", local = TRUE)
register_research_workbench_help()
source("R/research_workbench_server.R", local = TRUE)
source("R/shared_adaptation.R", local = TRUE)
source("R/shared_riskset.R", local = TRUE)
source("R/shared_adaptation_ui.R", local = TRUE)
register_shared_adaptation_help()
source("R/shared_adaptation_server.R", local = TRUE)
source("R/round_two_help.R", local = TRUE)
register_round_two_help()
source("R/multistate_prediction.R", local = TRUE)
source("R/joint_sequential.R", local = TRUE)
source("R/round_three_ui.R", local = TRUE)
round_three_help()
source("R/round_three_server.R", local = TRUE)
source("R/flexible_models.R", local = TRUE)
source("R/flexible_splines.R", local = TRUE)
source("R/flexible_prediction.R", local = TRUE)
source("R/observation_process.R", local = TRUE)
source("R/round_four_ui.R", local = TRUE)
register_round_four_help()
source("R/round_four_server.R", local = TRUE)
source("R/multiarm_design.R", local = TRUE)
source("R/multiarm_multiplicity.R", local = TRUE)
source("R/design_search.R", local = TRUE)
source("R/reverse_calibration.R", local = TRUE)
source("R/round_five_ui.R", local = TRUE)
register_round_five_help()
source("R/expansion_help.R")
register_expansion_help()
source("R/round_five_server.R", local = TRUE)
source("R/ia_reverse_calibration.R", local = TRUE)
source("R/panel_multistate.R", local = TRUE)
source("R/expansion_two_ui.R", local = TRUE)
register_expansion_two_help()
source("R/expansion_two_server.R", local = TRUE)
source("R/workflow.R", local = TRUE)

ui <- page_fillable(title = "event_pred", theme = theme, fillable = FALSE,
  tags$head(tags$script(src="batch_workspace.js"),tags$script(HTML("Shiny.addCustomMessageHandler('studyBusy',function(s){document.getElementById('st_run').disabled=s.busy;document.getElementById('st_cancel').disabled=!s.busy;});")),tags$link(rel = "stylesheet", href = "style.css"),tags$link(rel="stylesheet",href="handbook.css")),
  div(class = "app-header", div(tags$span("event_pred", class = "brand"), tags$span("生存模拟与事件数预测", class = "app-title")),
    div(class = "header-meta", "内部开发 · v0.35.0（待复核）")),
  navset_pill_list(id = "nav", widths = c(2, 10), well = FALSE,
    workflow_home(),
    nav_panel("任务参数", value = "inputs",
      div(class = "page-title", uiOutput("task_heading"), actionButton("run", "运行预测", class = "btn-primary")),
      uiOutput("input_context"),
      navset_card_tab(id="input_tabs",
        nav_panel("队列",value="cohort",
          radioButtons("input_mode", "输入方式", c("参数输入" = "parameters", "ADTTE 数据" = "adtte"), selected = "parameters", inline = TRUE),
          radioButtons("analysis_mode", "组别处理", c("盲态 / 合并"="pooled", "已知组别"="grouped"),selected="pooled",inline=TRUE),
          conditionalPanel("input.analysis_mode === 'grouped'", conditionalPanel("input.input_mode === 'parameters'",numericInput("group_count","组数",2,min=2,max=6,step=1)), conditionalPanel("input.input_mode === 'adtte'",uiOutput("group_column_ui"))),
          selectInput("time_unit", "时间单位", c("日" = "days", "周" = "weeks", "月" = "months"), selected = "months"),
          p(class = "field-note", "1 周 = 7 日；1 月 = 365.25/12 日。切换单位同步转换已填时间、速率和先验，不改变日期。"),
          fields(dateInput("origin", "研究起点日期", "2025-01-01"), numericInput("cut", "IA / 当前 DCO（自研究起点起的月数）", 0, min = 0)),
          conditionalPanel("input.input_mode === 'parameters' && input.analysis_mode !== 'grouped'",
            fields(numericInput("active_n", "IA 仍随访人数 Nactive", 0, min = 0, max = 5000, step = 1), numericInput("known_n", "IA 时已记录事件数 D0", 0, min = 0, step = 1)),
            conditionalPanel("input.active_n > 0", fields(selectInput("age_mode", "当前随访时间设定", c("所有患者相同" = "fixed", "0 至最大随访时间的均匀分布" = "uniform")), numericInput("duration", "已无事件随访时间 a / 最大值（月）", 8, min = 0))),
            p(class = "field-note", "启动前模拟：截点、当前人数和已发生事件数均设为 0。当前已发生事件的具体日期不由人数推导。")),
          conditionalPanel("input.input_mode === 'adtte'",
            fileInput("file", "ADTTE 文件", accept = c(".csv", ".xpt", ".sas7bdat")),
            p(class = "field-note", "必填：USUBJID、PARAMCD、STARTDT、ADT、AVAL、CNSR。文件 AVAL 单位在下方单独设置。"),
            fields(uiOutput("endpoint_ui"), selectInput("date_encoding", "CSV 日期编码", c("YYYY-MM-DD" = "iso", "SAS 日期（1960-01-01 起的天数）" = "sas"))),
            selectInput("aval_unit", "文件 AVAL 单位", c("日" = "days", "周" = "weeks", "月" = "months"), selected = "days"),
            fields(selectInput("offset", "AVAL 换算为日后 = ADT - STARTDT +", c("1" = 1, "0" = 0)), textInput("dropout_codes", "永久退出随访的 CNSR 编码（逗号分隔）", "2")),
            fields(uiOutput("flag_ui"), textInput("flag_value", "分析标志保留值", "Y")),
            selectInput("gap_mode", "仍随访记录的 ADT 早于截点时", c("要求确认至截点" = "strict", "从末次确认日补全未观察区间" = "impute")),
            p(class = "field-note", "CNSR=0 为事件。其余编码按上方设置映射为永久退出或仍随访；不会根据 EVNTDESC 自动判断。补全模式允许未观察区间内发生事件。"),
            downloadButton("template", "下载 ADTTE 合成模板"))),
        nav_panel("事件模型",value="eventmodel",
          conditionalPanel("input.input_mode === 'parameters' && input.analysis_mode !== 'grouped'",
            event_parameter_fields()),
          conditionalPanel("input.input_mode === 'adtte'",
            checkboxGroupInput("methods", "拟合模型", choices, selected = c("exponential", "weibull", "pwe"), inline = TRUE),
            conditionalPanel("input.analysis_mode !== 'grouped'", conditionalPanel("input.methods.indexOf('pwe') >= 0",textInput("cuts", "PWE 切点（随访月数）", "3,6,12")), conditionalPanel("input.methods.indexOf('km_tail') >= 0",numericInput("tail_rate", "KM 指数尾部风险率（每月）", .06, min = .000001))),
            selectInput("uncertainty", "事件模型参数处理", c("固定拟合参数（仅未来随机性）" = "plugin", "受试者 Bootstrap" = "bootstrap", "指数 / PWE Gamma 后验" = "gamma", "Weibull MCMC 后验" = "bayes_weibull"), selected = "bootstrap"),
            conditionalPanel("input.uncertainty === 'gamma'", fields(numericInput("prior_shape", "事件 Gamma 先验 shape", .5, min = .01), numericInput("prior_rate", "事件 Gamma 先验 rate（受试者·月）", 50 / time_factor("months"), min = .01))),
            conditionalPanel("input.uncertainty === 'bayes_weibull'",
              fields(numericInput("log_eta_mean", "log(Weibull 尺度 eta/月) 先验均值", log(450 / time_factor("months"))), numericInput("log_eta_sd", "log(Weibull eta) 先验标准差", 1, min = .01)),
              fields(numericInput("log_shape_mean", "log(Weibull shape) 先验均值", 0), numericInput("log_shape_sd", "log(Weibull shape) 先验标准差", .75, min = .01)),
              fields(numericInput("mcmc_chains", "MCMC 链数", 4, min = 2, max = 4, step = 1), numericInput("mcmc_warmup", "每链预热次数", 1000, min = 200, max = 10000, step = 200)),
              numericInput("mcmc_draws", "每链保留次数", 3000, min = 200, max = 10000, step = 500)),
            checkboxInput("ensemble", "计算 AIC 加权预测混合", TRUE))),
        nav_panel("分组参数",value="groupparams", conditionalPanel("input.analysis_mode === 'grouped'",p(class="field-note","各组分别设定。数据模式的拟合方法、参数处理及先验在事件模型页选择，先验分别作用于各组；目标 D* 为各组事件数合计。"),uiOutput("group_inputs"))),
        nav_panel("未来过程",value="future",
          conditionalPanel("input.analysis_mode !== 'grouped'", fields(numericInput("future_n", "计划继续入组人数", 300, min = 0, max = 1000, step = 1), conditionalPanel("input.future_n > 0",selectInput("enroll_mode", "未来入组率", c("恒定" = "constant", "分段" = "piecewise")))),
          conditionalPanel("input.future_n > 0 && input.enroll_mode === 'constant'", numericInput("enroll_rate", "入组率 r（人/月）", 15, min = 0, step = .1)),
          conditionalPanel("input.future_n > 0 && input.enroll_mode === 'piecewise'", fields(textInput("enroll_cuts", "入组切点（当前 DCO 后的月数）", "3,6"), textInput("enroll_rates", "各段入组率（人/月）", "9,18,12"))),
          dropout_parameter_fields(), numericInput("multiplier", "未来风险调整系数 q", 1, min = .01, step = .1)),
          fields(selectInput("clock", "事件计数日期", c("实际发生" = "occurred", "上报" = "reported")), conditionalPanel("input.analysis_mode !== 'grouped' && input.clock === 'reported'",numericInput("lag", "新增事件上报延迟 L（月）", 0, min = 0))),
          conditionalPanel("input.input_mode === 'adtte'",
            selectInput("process_uncertainty", "入组 / 脱落参数处理", c("使用输入率" = "fixed", "Poisson-Gamma / Exponential-Gamma 后验" = "gamma")),
            conditionalPanel("input.process_uncertainty === 'gamma'",
              fields(numericInput("recruit_start", "历史入组窗口起点（研究月数）", 0, min = 0), numericInput("recruit_end", "历史入组窗口终点（研究月数）", 540 / time_factor("months"), min = 1)),
              fields(numericInput("enroll_prior_shape", "入组 Gamma 先验 shape", 1, min = .01), numericInput("enroll_prior_rate", "入组 Gamma 先验 rate（月）", 1 / time_factor("months"), min = .01)),
              fields(numericInput("drop_prior_shape", "脱落 Gamma 先验 shape", .5, min = .01), numericInput("drop_prior_rate", "脱落 Gamma 先验 rate（受试者·月）", 50 / time_factor("months"), min = .01))))),
        nav_panel("运行设置",value="runsettings",
          fields(conditionalPanel("output.task_goal === 'target'",numericInput("target", "Final DCO 目标事件数 D*", 180, min = 1, step = 1)), numericInput("horizon", "当前 DCO 后预测窗口（月）", 48, min = 1 / time_factor("months"), max = 3650 / time_factor("months"))),
          fields(numericInput("sims", "模拟次数", 300, min = 50, max = 2000, step = 50), numericInput("seed", "随机种子", 20261003, min = 0, step = 1))))),
    simulation_ui(),
    joint_survival_ui(),
    conditional_simulation_ui(),
    history_inference_ui(),
    study_ui(),
    batch_research_ui(),
    joint_research_ui(),
    adaptive_batch_ui(),
    conditional_batch_ui(),
    heterogeneity_ui(),
    shared_adaptation_ui(),
    multistate_ui(),
    joint_sequential_ui(),
    flexible_prediction_ui(),
    observation_ui(),
    multiarm_ui(),
    design_search_ui(),
    reverse_calibration_ui(),
    ia_reverse_ui(),
    panel_multistate_ui(),
    adaptation_ui(),
    nav_panel("预测结果", value = "forecast",
      div(class = "page-title", h2("预测结果"), uiOutput("run_label")),
      uiOutput("run_status"), uiOutput("metrics"),
      navset_card_tab(id="forecast_tabs",
        nav_panel("任务结果",value="main",
          conditionalPanel("output.task_goal === 'count'",section("累计事件数 · 中位数与逐点 95% 预测区间",plotlyOutput("event_plot",height="400px")),section("窗口末累计事件数",DTOutput("count_end_table",fill=FALSE))),
          conditionalPanel("output.task_goal === 'target'",section("目标事件日期",DTOutput("milestones",fill=FALSE),uiOutput("mcse_note")),
          section("达标概率",plotlyOutput("prob_plot",height="300px")))),
        nav_panel("分组事件",value="groupresults",conditionalPanel("output.grouped_result === 'yes'",uiOutput("group_result_note"),plotlyOutput("group_event_plot",height="380px"),DTOutput("group_event_table",fill=FALSE),div(class="export-bar",downloadButton("group_curves_download","分组事件曲线 CSV"),downloadButton("group_summary_download","分组窗口事件数 CSV"))),conditionalPanel("output.grouped_result !== 'yes'",p(class="field-note","选择已知组别后运行预测，可查看各组事件数。"))),
        nav_panel("参数与诊断",section("模型参数与等价换算",p(class="field-note","换算值按模型参数计算。MCMC使用参数后验中位数；分布分位数不等同于参数后验区间或目标日期分位数。"),DTOutput("model_parameters",fill=FALSE),downloadButton("parameters_download","参数换算 CSV")),
          section("拟合与模拟",conditionalPanel("output.grouped_result === 'yes'",DTOutput("group_fit_table",fill=FALSE)),DTOutput("diagnostics",fill=FALSE),DTOutput("extended_fit_table",fill=FALSE),uiOutput("weights"))),
        nav_panel("导出",div(class="export-bar",conditionalPanel("output.task_goal === 'count'",downloadButton("curves_download","事件曲线 CSV")),conditionalPanel("output.task_goal === 'target'",downloadButton("summary_download","目标日期 CSV")),downloadButton("config_download","配置 JSON"),downloadButton("report_download","摘要 Markdown"))))),
    nav_panel("数据与分布", value = "data",
      div(class = "page-title", h2("数据与分布")),
      section("输入记录", uiOutput("data_status"), DTOutput("data_table",fill=FALSE)),
      section("生存分布", plotlyOutput("fit_plot", height = "400px")),
      section("ADTTE 数据格式", includeMarkdown("docs/DATA_CONTRACT.md"))),
    nav_panel("附加分析", value = "validation",
      div(class = "page-title", h2("附加分析")),
      navset_card_tab(id="aux_tabs",nav_panel("历史截点回测",value="aux_backtest",section("历史截点回测",
        fields(textInput("backtest_cuts", "回测 DCO（研究月数，逗号分隔）", "9,12"), actionButton("run_backtest", "运行回测", class = "btn-primary")),
        conditionalPanel("output.grouped_result !== 'yes'", fields(textInput("backtest_future_n", "各回测截点的历史剩余入组人数", ""), textInput("backtest_enroll_rates", "各回测截点的历史恒定入组率（人/月）", ""))), uiOutput("group_backtest_inputs"),
        p(class = "field-note", "人数、入组率按回测截点顺序一一填写。过程后验模式使用截至各截点的率后验；其余参数沿用最近成功预测。仅支持实际发生口径，不评价缺少历史报告日期的上报预测。"),
        uiOutput("backtest_status"), DTOutput("backtest_table",fill=FALSE), conditionalPanel("output.task_goal === 'count'",plotlyOutput("backtest_plot", height = "340px")), downloadButton("backtest_download", "回测 CSV"), downloadButton("backtest_plan_download","历史计划 CSV"))),
      nav_panel("风险倍数情景",value="aux_sensitivity",section("风险倍数情景",
        fields(textInput("sensitivity_values", "风险倍数（逗号分隔）", ".75,1,1.25"), actionButton("run_sensitivity", "运行情景", class = "btn-primary")),
        DTOutput("sensitivity_table",fill=FALSE), downloadButton("sensitivity_download", "情景 CSV"))),
      nav_panel("历史校准记录",value="aux_calibration",section("v0.2 合成校准记录",
        p(class = "field-note", "以下为 v0.2 历史实验，未作为 v0.6 重新校准结果。研究截点 300 天，评价日 700 天；5 类事件过程，每类 30 个试验，每次 300 条预测轨迹。入组计划 200 人。覆盖率为评价日事件数区间的经验覆盖率。"),
        plotlyOutput("calibration_plot", height = "580px"), DTOutput("calibration_table",fill=FALSE))))),
    nav_panel("条件生存", value = "lab",
      div(class = "page-title", h2("条件生存")),
      section("参数", fields(selectInput("lab_model", "已运行模型", choices), numericInput("age", "已无事件随访时间 a（月）", 8, min = 0)),
        fields(numericInput("lab_horizon", "剩余时间 U 的窗口（月）", 24, min = 1 / time_factor("months"), max = 3650 / time_factor("months")), numericInput("lab_multiplier", "未来风险倍数", 1, min = .01)),
        uiOutput("conditional_formula"), tableOutput("conditional_quantiles")),
      section("剩余时间分布", plotlyOutput("conditional_plot", height = "360px"))),
    nav_panel("后验诊断", value = "posterior",
      div(class = "page-title", h2("后验诊断")),
      section("Weibull MCMC", selectInput("posterior_model","后验模型",character()),uiOutput("posterior_status"), DTOutput("posterior_table",fill=FALSE), plotlyOutput("trace_plot", height = "400px")),
      section("入组 / 脱落后验", tableOutput("process_table"))),
    nav_panel("方法与推导", value = "methods", handbook_ui()),
    nav_panel("开发记录", value = "roadmap", div(class = "reading-layout", includeMarkdown("docs/DEVELOPMENT_PLAN.md")))),
  div(class = "site-footer", "event_pred · 日 / 周 / 月（默认月）· v0.35.0"))

server <- function(input, output, session) {
  result <- reactiveVal(NULL); run_error <- reactiveVal(NULL)
  backtest <- reactiveVal(NULL); backtest_error <- reactiveVal(NULL); sensitivity <- reactiveVal(NULL)
  parse_numbers <- function(x) {
    if (is.null(x) || !nzchar(trimws(x))) return(numeric())
    y <- suppressWarnings(as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]])))
    if (anyNA(y) || any(!is.finite(y))) stop("逗号分隔的参数必须全部为有限数值。")
    y
  }
  current_unit <- reactiveVal("months")
  input_unit <- reactive(input$time_unit %||% "days")
  register_parameter_controls(input,output,session,input_unit)
  design_result <- register_simulation_server(input,output,session)
  register_joint_server(input,output,session)
  register_workflow_server(input,output,session,result)
  register_study_server(input,output,session)
  batch_controller<-register_batch_server(input,output,session)
  conditional_batch_bridge<-register_conditional_batch_server(input,output,session,batch_controller)
  register_heterogeneity_server(input,output,session,batch_controller)
  register_shared_adaptation_server(input,output,session,batch_controller)
  register_round_three_server(input,output,session,batch_controller)
  register_round_four_server(input,output,session,batch_controller)
  register_round_five_server(input,output,session,batch_controller)
  register_expansion_two_server(input,output,session)
  register_conditional_simulation_server(input,output,session,design_result,conditional_batch_bridge)
  register_joint_research_server(input,output,session,batch_controller)
  register_adaptive_batch_server(input,output,session,batch_controller)
  register_history_inference_server(input,output,session)
  register_adaptive_server(input,output,session,conditional_batch_bridge)
  register_patient_adaptive_server(input,output,session,conditional_batch_bridge)
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
      median = paste0("中位生存时间 mPFS / mOS（",u,"；治愈模型为未治愈成分）"), median2 = paste0("第二成分 mPFS / mOS（",u,"）"),
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
  raw <- reactive({ req(input$file); read_adtte(input$file$datapath, input$file$name) })
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
      validate(need(is.finite(n) && n==floor(n) && n>=2 && n<=6,"组数需为2–6整数。"))
      return(LETTERS[seq_len(n)])
    }
    x <- raw(); req(input$paramcd,input$group_column)
    validate(need(input$group_column %in% names(x),"组别变量不存在。"))
    rows <- !is.na(x$PARAMCD)&x$PARAMCD==input$paramcd
    if(nzchar(input$analysis_flag %||% "")) rows <- rows & !is.na(x[[input$analysis_flag]]) & x[[input$analysis_flag]]==input$flag_value
    g <- as.character(x[[input$group_column]][rows])
    validate(need(!anyNA(g)&&all(nzchar(trimws(g))),"筛选后组别不能缺失或为空。"))
    g <- sort(unique(g)); validate(need(length(g)>=2 && length(g)<=6,"筛选后需有2–6个组。")); g
  })
  output$group_inputs <- renderUI({
    labels <- group_slots(); do.call(navset_card_tab,lapply(seq_along(labels),function(i) nav_panel(paste("组",i,"·",labels[i]),group_input_card(i,labels[i],input$input_mode,isolate(input_unit())))))
  })
  output$group_backtest_inputs <- renderUI({
    r <- result(); if(is.null(r)||r$config$analysis_mode!="grouped") return(NULL)
    u <- time_label(isolate(input_unit()))
    tagList(lapply(seq_along(r$config$groups),function(i) section(r$config$groups[[i]]$name,
      fields(textInput(paste0("g",i,"_backtest_future_n"),"各历史截点的剩余入组人数",""),textInput(paste0("g",i,"_backtest_enroll_rates"),paste0("各历史截点的恒定入组率（人/",u,"）"),"")))))
  })
  observeEvent(input$uncertainty, {
    if (input$uncertainty == "gamma") updateCheckboxGroupInput(session, "methods", selected = c("exponential", "pwe"))
    if (input$uncertainty == "bayes_weibull") updateCheckboxGroupInput(session, "methods", selected = "weibull")
  }, ignoreInit = TRUE)
  build_inputs <- function() {
    am <- input$analysis_mode %||% "pooled"
    cfg <- list(common_methods = input$methods, analysis_mode = am %||% "pooled", input_mode = input$input_mode, cut = input$cut, horizon = input$horizon, target = if (identical(input$task,"count")) 1 else input$target,
      methods = if (input$input_mode == "parameters") input$design_model else input$methods,
      cuts = if(input$input_mode=="adtte" && am!="grouped" && "pwe" %in% input$methods)parse_numbers(input$cuts) else numeric(), tail_rate = if(input$input_mode=="adtte" && "km_tail" %in% input$methods)input$tail_rate else .002,
      uncertainty = if (input$input_mode == "parameters") "plugin" else input$uncertainty,
      prior_shape = if(input$input_mode=="adtte" && input$uncertainty=="gamma")input$prior_shape else .5, prior_rate = if(input$input_mode=="adtte" && input$uncertainty=="gamma")input$prior_rate else 50/time_factor(input_unit()),
      ensemble = input$input_mode == "adtte" && input$ensemble,
      future_n = if(am=="grouped")0 else input$future_n, enroll_rate = if(am=="grouped" || input$future_n==0)0 else input$enroll_rate, dropout_rate = if(am=="grouped")0 else dropout_input_rate(list(drop_input=input$drop_input,dropout_rate=input$dropout_rate,drop_prob=input$drop_prob,drop_period=input$drop_period)),
      multiplier = if(am=="grouped")1 else input$multiplier, lag = if(am!="grouped" && input$clock=="reported")input$lag else 0, clock = input$clock, sims = input$sims, seed = input$seed,
      origin = as.character(input$origin), gap_mode = if (input$input_mode == "parameters") "strict" else input$gap_mode,
      enroll_mode = if(am=="grouped" || input$future_n==0)"constant" else input$enroll_mode, enroll_cuts = if(am!="grouped" && input$enroll_mode=="piecewise" && input$future_n>0)parse_numbers(input$enroll_cuts) else numeric(), enroll_rates = if(am!="grouped" && input$enroll_mode=="piecewise" && input$future_n>0)parse_numbers(input$enroll_rates) else numeric(),
      process_uncertainty = if (input$input_mode == "parameters") "fixed" else input$process_uncertainty,
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
        input$analysis_flag %||% "", input$flag_value, input$date_encoding, input$gap_mode, input$aval_unit %||% "days",group_column=if(cfg$analysis_mode=="grouped")input$group_column else NULL)
      cfg$adtte_mapping <- list(paramcd = input$paramcd, offset = as.numeric(input$offset), dropout_codes = parse_numbers(input$dropout_codes),
        flag = input$analysis_flag, flag_value = input$flag_value, date_encoding = input$date_encoding, aval_unit = input$aval_unit %||% "days",group_column=if(cfg$analysis_mode=="grouped")input$group_column else NULL)
    }
    if(cfg$analysis_mode=="grouped") {
      labels <- group_slots(); cfg$groups <- list(); override <- if(input$input_mode=="parameters") list() else NULL; cohorts <- list()
      for(i in seq_along(labels)) {
        key <- paste0("g",i); defaults <- group_input_defaults(input_unit(),i)
        val <- function(id) input[[paste0(key,"_",id)]] %||% defaults[[id]]
        gg <- list(name=if(input$input_mode=="parameters")val("name") else labels[i],future_n=val("future_n"),enroll_mode=val("enroll_mode"),enroll_rate=val("enroll_rate"),enroll_cuts=if(val("future_n")>0 && val("enroll_mode")=="piecewise")parse_numbers(val("enroll_cuts")) else numeric(),enroll_rates=if(val("future_n")>0 && val("enroll_mode")=="piecewise")parse_numbers(val("enroll_rates")) else numeric(),dropout_rate=dropout_input_rate(setNames(lapply(names(defaults),val),names(defaults))),multiplier=val("multiplier"),lag=if(input$clock=="reported")val("lag") else 0,cuts=if(input$input_mode=="adtte" && (val("fit_method")=="pwe" || (!nzchar(val("fit_method")) && "pwe" %in% input$methods)))parse_numbers(val("cuts")) else numeric(),tail_rate=if(input$input_mode=="adtte" && (val("fit_method")=="km_tail" || (!nzchar(val("fit_method")) && "km_tail" %in% input$methods)))val("tail_rate") else .002)
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
      result(r); backtest(NULL); sensitivity(NULL)
      updateSelectInput(session, "lab_model", choices = setNames(names(r$models), vapply(r$models, `[[`, character(1), "label")))
      pm <- r$models[vapply(r$models,function(m)!is.null(m$posterior),logical(1))]
      updateSelectInput(session,"posterior_model",choices=setNames(names(pm),vapply(pm,`[[`,character(1),"label")))
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
    notes <- character()
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
    tabular(data.frame(模型 = d$model, 窗口内达标概率 = sprintf("%.1f%%", 100 * d$reached), `概率 MCSE（百分点）` = 100 * d$reached_mcse, 日期中位数 = milestone_dates(r),
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
  lab <- reactive({
    r <- result(); req(r, input$lab_model); m <- r$models[[input$lab_model]]; req(m)
    validate(need(is.finite(input$age) && input$age >= 0, "随访时间需非负。"), need(input$lab_horizon > 0 && input$lab_horizon * time_factor(input_unit()) <= 3650, "窗口换算为日后需在 1–3650 日内。"),
      need(input$lab_multiplier > 0, "风险倍数需为正。"), need(is.finite(model_cumhaz(m, input$age * time_factor(input_unit()))), "该时间已超出生存支持范围。"))
    list(model = m, age = input$age * time_factor(input_unit()), multiplier = input$lab_multiplier)
  })
  output$conditional_formula <- renderUI(withMathJax(p("$$P(U>u\\mid T>a)=\\exp[-q\\{H(a+u)-H(a)\\}].$$")))
  output$conditional_plot <- renderPlotly({ l <- lab(); t <- seq(0, input$lab_horizon, length.out = 250); d <- data.frame(remaining = t, probability = exp(-l$multiplier * (model_cumhaz(l$model, l$age + t * time_factor(input_unit())) - model_cumhaz(l$model, l$age)))); p <- ggplot(d, aes(remaining, probability)) + geom_line(colour = palette[1]) + labs(x = paste0("剩余时间 U（", time_label(input_unit()), "）"), y = "条件生存概率") + theme_chart(); chart_widget(p) })
  output$conditional_quantiles <- renderTable({ l <- lab(); q <- sample_conditional(l$model, rep(l$age, 3), l$multiplier, u = c(.975, .5, .025)) - l$age; data.frame(分位数 = c("2.5%", "50%", "97.5%"), 剩余时间 = ifelse(is.finite(q), round(q / time_factor(input_unit()), 3), "无限"), 单位 = time_label(input_unit())) })
  selected_posterior <- reactive({r <- result(); req(r); key <- input$posterior_model; if(is.null(key)||!nzchar(key)) key <- "weibull"; r$models[[key]]$posterior})
  post <- reactive({p <- selected_posterior(); req(p); p})
  output$posterior_status <- renderUI({ r <- result(); if (is.null(r) || is.null(selected_posterior())) return(p("事件模型参数处理选择 Weibull MCMC 后运行预测。")); p(class = "field-note", paste("保留期接受率：", paste(round(selected_posterior()$acceptance, 3), collapse = ", "), "；通过条件：R-hat ≤ 1.01，bulk-ESS 与 tail-ESS ≥ 400。")) })
  output$posterior_table <- renderDT({ p <- post(); d <- p$diagnostics; f <- time_factor(result()$config$display_unit); d$mean[d$variable == "log_eta"] <- d$mean[d$variable == "log_eta"] - log(f); d$variable[d$variable == "log_eta"] <- paste0("log_eta（",time_label(result()$config$display_unit),"）"); tabular(d) })
  output$trace_plot <- renderPlotly({ p <- post(); a <- p$draws; a[, , "log_eta"] <- a[, , "log_eta"] - log(time_factor(result()$config$display_unit)); dimnames(a)[[3]][1] <- paste0("log_eta（",time_label(result()$config$display_unit),"）"); d <- do.call(rbind, lapply(seq_len(dim(a)[2]), function(j) do.call(rbind, lapply(seq_len(dim(a)[3]), function(k) data.frame(iteration = seq_len(dim(a)[1]), value = a[, j, k], chain = factor(j), parameter = dimnames(a)[[3]][k]))))); g <- ggplot(d, aes(iteration, value, colour = chain)) + geom_line(linewidth = .25, alpha = .6) + facet_wrap(~parameter, scales = "free_y", ncol = 1) + labs(x = "保留期迭代", y = NULL, colour = "链") + theme_chart(); chart_widget(g) })
  output$process_table <- renderTable({ r <- result(); req(r); pp <- r$process_posterior; if(r$config$analysis_mode=="grouped") pp <- unlist(lapply(names(r$group_process_posterior),function(id) {x <- r$group_process_posterior[[id]]; if(!is.null(x)) names(x) <- paste(r$config$groups[[id]]$name,names(x),sep=" / "); x}),recursive=FALSE); req(pp); do.call(rbind, lapply(names(pp), function(n) data.frame(过程 = n, shape = unname(pp[[n]][1]), rate = unname(pp[[n]][2] / time_factor(r$config$display_unit)), 后验均值 = unname(pp[[n]][1] / pp[[n]][2] * time_factor(r$config$display_unit)), rate单位 = paste0(if (grepl("dropout$",n)) "受试者·" else "", time_label(r$config$display_unit)), 均值单位 = paste0(if (grepl("enroll$",n)) "人/" else "1/", time_label(r$config$display_unit))))) }, digits = 6)
  observeEvent(input$run_backtest, {
    backtest_error(NULL)
    tryCatch({
      r <- result(); if (is.null(r)) stop("请先运行 ADTTE 预测。")
      cuts <- parse_numbers(input$backtest_cuts) * time_factor(input_unit())
      if(r$config$analysis_mode=="grouped") {
        plan <- do.call(rbind,lapply(seq_along(r$config$groups),function(i) {
          remaining <- parse_numbers(input[[paste0("g",i,"_backtest_future_n")]])
          rates <- parse_numbers(input[[paste0("g",i,"_backtest_enroll_rates")]])/time_factor(input_unit())
          if(!length(cuts)||length(remaining)!=length(cuts)||length(rates)!=length(cuts)) stop("请为每组、每个回测截点填写历史剩余人数和恒定入组率。")
          data.frame(group=r$config$groups[[i]]$name,cut=cuts,future_n=remaining,enroll_rate=rates)
        }))
      } else {
        remaining <- parse_numbers(input$backtest_future_n); rates <- parse_numbers(input$backtest_enroll_rates)/time_factor(input_unit())
        if (!length(cuts) || length(remaining) != length(cuts) || length(rates) != length(cuts)) stop("请为每个回测截点填写一个历史剩余人数和一个历史恒定入组率。")
        plan <- data.frame(cut=cuts,future_n=remaining,enroll_rate=rates)
      }
      v <- withProgress(message = "历史回测", value = 0,
        backtest_forecast(r$observed_data, r$config, cuts, r$config$cut, function(v, d) setProgress(value = v, detail = d), plan = plan))
      backtest(v)
    },
      error = function(e) { backtest_error(conditionMessage(e)); showNotification(conditionMessage(e), type = "error") })
  })
  output$backtest_status <- renderUI({
    if (!is.null(backtest_error())) return(div(class = "notice error", backtest_error()))
    v <- backtest(); if (is.null(v)) return(NULL)
    tagList(p(class = "field-note", v$note), if (nrow(v$failures)) div(class = "notice", paste(sprintf("截点 %s / %s：%s", v$failures$cut, v$failures$method, v$failures$message), collapse = "；")))
  })
  task_backtest <- function(d,cfg) {
    common<-c("cut","end","method","model","planned_future_n","historical_enroll_rate_per_day","enrollment_source")
    cols<-if(identical(cfg$task,"count"))c(common,"observed_events","predicted_events","error","count_lower","count_upper","count_covered","count_interval_width") else c(common,"reached_probability","target_reached","brier","actual_target_day","predicted_target_day","target_error","truncated_target_covered")
    d[,intersect(cols,names(d)),drop=FALSE]
  }
  task_sensitivity <- function(d,cfg) {
    cols<-if(identical(cfg$task,"count"))c("model","method","multiplier","mean_events_end","lower_events_end","median_events_end","upper_events_end","simulations") else c("model","method","multiplier","reached","reached_mcse","lower_day","median_day","upper_day","simulations")
    d[,intersect(cols,names(d)),drop=FALSE]
  }
  output$backtest_table <- renderDT({ v <- backtest(); req(v); d <- task_backtest(v$summary,v$config)
    d$historical_enroll_rate_per_day <- d$historical_enroll_rate_per_day * time_factor(v$config$display_unit)
    names(d)[names(d) == "historical_enroll_rate_per_day"] <- paste0("历史计划入组率（人/",time_label(v$config$display_unit),"）")
    tabular(unit_table(d, v$config$display_unit, c("cut", "end", "actual_target_day", "predicted_target_day", "target_error"))) })
  output$backtest_plot <- renderPlotly({ v <- backtest(); req(v); d <- v$curves; d$day <- d$day / time_factor(v$config$display_unit); d$backtest_cut <- d$backtest_cut / time_factor(v$config$display_unit); p <- ggplot(d, aes(day, median, colour = model)) + geom_line() + geom_line(aes(y = actual), colour = "#173d50", linetype = "dashed") + facet_wrap(~backtest_cut) + labs(x = paste0("研究时间（",time_label(v$config$display_unit),"）"), y = "累计事件数", colour = NULL) + theme_chart(); chart_widget(p) })
  observeEvent(input$run_sensitivity, {
    tryCatch({ r <- result(); if (is.null(r)) stop("请先运行预测。"); s <- withProgress(message = "风险情景", value = 0, run_sensitivity(r$observed_data, r$config, parse_numbers(input$sensitivity_values), r$models_override, function(v, d) setProgress(value = v, detail = d))); sensitivity(s) }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
  output$sensitivity_table <- renderDT({ s <- sensitivity(); req(s); tabular(unit_table(task_sensitivity(s,result()$config), result()$config$display_unit, c("lower_day", "median_day", "upper_day"))) })
  calibration <- reactive({
    validate(need(file.exists("validation/calibration_summary.csv"), "尚无合成校准记录。"))
    read.csv("validation/calibration_summary.csv", stringsAsFactors = FALSE)
  })
  output$calibration_table <- renderDT({
    d <- calibration()
    d$target_mae <- d$target_mae / time_factor(input_unit())
    labels <- c(scenario = "生成过程", uncertainty = "参数处理", model = "模型", trials = "试验数", bias = "偏差", mae = "MAE", rmse = "RMSE", count_coverage = "事件数覆盖率", coverage_mcse = "覆盖率 MCSE", mean_interval_width = "平均区间宽度", mean_brier = "Brier", finite_target_errors = "可计算日期数", target_mae = paste0("日期 MAE（",time_label(input_unit()),"）"), coverage_lower = "Wilson 下限", coverage_upper = "Wilson 上限")
    num <- names(d)[vapply(d, is.numeric, logical(1))]
    g <- datatable(d, colnames = unname(labels[names(d)]), rownames = FALSE, escape = TRUE, options = c(table_options, list(pageLength = 10)))
    formatRound(g, num, 3)
  })
  output$calibration_plot <- renderPlotly({
    d <- calibration()
    d$scenario <- factor(d$scenario, levels = c("exponential", "increasing", "decreasing", "high_dropout", "cure"), labels = c("指数", "递增风险", "递减风险", "高脱落", "治愈"))
    p <- ggplot(d, aes(scenario, count_coverage, colour = model, group = model)) +
      geom_point(position = position_dodge(width = .4)) +
      geom_errorbar(aes(ymin = coverage_lower, ymax = coverage_upper), width = .12, position = position_dodge(width = .4)) +
      facet_wrap(~uncertainty, ncol = 1) + geom_hline(yintercept = .95, linetype = "dashed", colour = "#777486") +
      scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
      labs(x = "生成过程", y = "经验覆盖率 / Wilson 95% 区间", colour = NULL) + theme_chart()
    chart_widget(p)
  })
  output$group_curves_download <- downloadHandler(filename="group_event_curves.csv",content=function(file){r <- result(); req(r$group_curves); write.csv(unit_export(r$group_curves,r$config$display_unit,"day"),file,row.names=FALSE)})
  output$group_summary_download <- downloadHandler(filename="group_event_summary.csv",content=function(file){r <- result(); req(r$group_summary); write.csv(r$group_summary,file,row.names=FALSE)})
  output$handbook_download <- downloadHandler(filename="event_pred_handbook_v0.30.0.md",content=function(file)file.copy("docs/METHODS.md",file,overwrite=TRUE),contentType="text/markdown; charset=utf-8")
  output$handbook_html_download <- downloadHandler(filename="event_pred_handbook_v0.30.0.html",content=function(file)writeLines(handbook_standalone_html(),file,useBytes=TRUE),contentType="text/html; charset=utf-8")
  output$template <- downloadHandler(filename = "adtte_synthetic.csv", content = function(file) write.csv(adtte_template(), file, row.names = FALSE))
  output$curves_download <- downloadHandler(filename = "event_curves.csv", content = function(file) { r <- result(); req(r); d<-r$curves;if(r$config$task=="count")d<-d[,setdiff(names(d),c("probability","probability_mcse")),drop=FALSE];write.csv(unit_export(d, r$config$display_unit, "day"), file, row.names = FALSE) })
  output$summary_download <- downloadHandler(filename = "event_milestones.csv", content = function(file) { r <- result(); req(r); d <- r$summary; d$median_date <- milestone_dates(r); write.csv(unit_export(d, r$config$display_unit, c("lower_day", "median_day", "upper_day")), file, row.names = FALSE) })
  output$config_download <- downloadHandler(filename = "event_pred_config.json", content = function(file) { r <- result(); req(r); jsonlite::write_json(list(version = "0.8.0", created_at = r$created_at, config = export_config(r), data_summary = r$data_summary, session = R.version.string), file, auto_unbox = TRUE, pretty = TRUE, digits = NA) })
  output$report_download <- downloadHandler(filename="event_pred_report.md",content=function(file) {
    r<-result();req(r);counts<-identical(r$config$task,"count");d<-count_end(r)
    lines<-c(if(counts)"# 未来事件数预测" else "# 目标事件日期预测","",paste("运行时间：",r$created_at),paste("显示单位：",time_label(r$config$display_unit)),"","## 配置","","```json",jsonlite::toJSON(export_config(r),auto_unbox=TRUE,pretty=TRUE,digits=NA),"```","")
    if(counts)lines<-c(lines,"## 窗口末累计事件数","","|模型|均值|2.5%|中位数|97.5%|","|---|---:|---:|---:|---:|",sprintf("|%s|%.3f|%.3f|%.3f|%.3f|",d$model,d$mean,d$lower,d$median,d$upper))
    else lines<-c(lines,"## 目标日期","","|模型|达标概率|概率MCSE（百分点）|日期中位数|","|---|---:|---:|---|",sprintf("|%s|%.1f%%|%.3f|%s|",r$summary$model,100*r$summary$reached,100*r$summary$reached_mcse,milestone_dates(r)),paste("MCSE范围：",r$mcse_scope))
    writeLines(c(lines,"","已记录事件固定；区间为逐点预测区间，模型/参数假设按上述配置。"),file,useBytes=TRUE)
  })
  output$backtest_plan_download <- downloadHandler(filename="historical_enrollment_plan.csv",content=function(file){v <- backtest(); req(v); d <- v$plan; names(d)[names(d)=="enroll_rate"] <- "enroll_rate_per_day"; d[[paste0("enroll_rate_per_",v$config$display_unit)]] <- d$enroll_rate_per_day*time_factor(v$config$display_unit); write.csv(unit_export(d,v$config$display_unit,"cut"),file,row.names=FALSE)})
  output$backtest_download <- downloadHandler(filename = "event_backtest.csv", content = function(file) { v <- backtest(); req(v); write.csv(unit_export(task_backtest(v$summary,v$config), v$config$display_unit, c("cut", "end", "actual_target_day", "predicted_target_day", "target_error")), file, row.names = FALSE) })
  output$sensitivity_download <- downloadHandler(filename = "event_sensitivity.csv", content = function(file) { s <- sensitivity(); req(s); write.csv(unit_export(task_sensitivity(s,result()$config), result()$config$display_unit, c("lower_day", "median_day", "upper_day")), file, row.names = FALSE) })
}
shinyApp(ui, server)
