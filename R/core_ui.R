parameter_help["group_column"] <- "选择本次分析组别变量，如 TRTP 或 TRTA。所选终点/分析标志筛选后每人一行、恰好两组且组别不缺失。"
parameter_help["group_count"] <- "核心版的已知组别预测使用两组；分别设置各组当前队列、事件分布和未来计划。"
parameter_help["sim_groups"] <- "1 为单组，2 为已知两组。组名及分配权重在组别与分布页填写；两组各用自己的分布参数。"
parameter_help["sim_cut_mode"] <- "核心版采用固定 DCO；多个截点共享同一患者轨迹。"
parameter_help["median"] <- "事件时间分布中位数 m>0，单位随日/周/月选择。例如 m=12 月。指数风险率为 log(2)/m；Weibull 还需给定形状 k。"
parameter_help["methods"] <- "右删失数据候选模型：指数、Weibull、PWE。至少选择一个；PWE 需指定切点。各模型单独预测，不计算模型混合。"
for (i in 1:2) {
  parameter_help[paste0("g",i,"_median")] <- parameter_help["median"]
  parameter_help[paste0("s",i,"_median")] <- parameter_help["median"]
}
# Release fields reuse the shared parameter conversion controls and identifiers.
event_parameter_fields <- function(prefix="",unit="months",d=group_input_defaults(unit,1),simulation=FALSE) {
  id <- function(x)paste0(prefix,x); u <- time_label(unit)
  cond <- function(js,...)conditionalPanel(gsub("GROUP",paste0("input.",prefix),js,fixed=TRUE),...)
  tagList(selectInput(id("design_model"),"事件时间分布",parameter_choices,"weibull"),
    cond("GROUPdesign_model === 'exponential'",selectInput(id("exp_input"),"指数输入形式",c("中位时间 m"="median","风险率 lambda"="rate"))),
    cond("GROUPdesign_model === 'weibull'",selectInput(id("weibull_input"),"Weibull 输入形式",c("中位时间 m + 形状 k"="median","尺度 eta + 形状 k"="eta"))),
    cond("(GROUPdesign_model === 'exponential' && GROUPexp_input !== 'rate') || (GROUPdesign_model === 'weibull' && GROUPweibull_input !== 'eta')",numericInput(id("median"),paste0("中位事件时间 m（",u,"）"),d$median,min=.01)),
    cond("GROUPdesign_model === 'exponential' && GROUPexp_input === 'rate'",numericInput(id("exp_rate"),paste0("事件风险率 lambda（每",u,"）"),d$exp_rate,min=.000001)),
    cond("GROUPdesign_model === 'weibull' && GROUPweibull_input === 'eta'",numericInput(id("eta"),paste0("Weibull 尺度 eta（",u,"）"),d$eta,min=.01)),
    cond("GROUPdesign_model === 'weibull'",numericInput(id("shape"),"Weibull 形状 k",d$shape,min=.01)),
    cond("GROUPdesign_model === 'pwe'",fields(textInput(id("parameter_cuts"),paste0("风险切点（随访",u,"）"),d$parameter_cuts),textInput(id("parameter_rates"),paste0("各段风险率（每",u,"）"),d$parameter_rates))),
    uiOutput(id("parameter_conversion")))
}
group_input_card <- function(index,label,mode,unit) {
  pre <- paste0("g",index,"_"); id <- function(x)paste0(pre,x); d <- group_input_defaults(unit,index); u <- time_label(unit)
  cond <- function(js,...)conditionalPanel(gsub("GROUP",paste0("input.",pre),js,fixed=TRUE),...)
  section(paste("组",index,"·",label),
    if(mode=="parameters") tagList(textInput(id("name"),"组名",d$name),
      fields(numericInput(id("active_n"),"当前仍随访人数 Nactive",0,min=0,max=5000,step=1),numericInput(id("known_n"),"当前已记录事件数 D0",0,min=0,step=1)),
      cond("GROUPactive_n > 0",fields(selectInput(id("age_mode"),"随访年龄",c("全部相同"="fixed","0 至最大值均匀分布"="uniform")),numericInput(id("duration"),paste0("已随访年龄 a / 最大值（",u,"）"),d$duration,min=0))),event_parameter_fields(pre,unit,d))
    else tagList(selectInput(id("fit_method"),"本组拟合模型",c("使用公共候选模型"="",choices)),
      cond("GROUPfit_method === 'pwe' || (GROUPfit_method === '' && input.methods.indexOf('pwe') >= 0)",textInput(id("cuts"),paste0("PWE 切点（随访",u,"）"),d$cuts))),
    numericInput(id("future_n"),"未来入组人数",150,min=0,max=1000,step=1),
    cond("GROUPfuture_n > 0",selectInput(id("enroll_mode"),"未来入组方式",c("恒定"="constant","分段"="piecewise"))),
    cond("GROUPfuture_n > 0 && GROUPenroll_mode === 'constant'",numericInput(id("enroll_rate"),paste0("入组率（人/",u,"）"),d$enroll_rate,min=0)),
    cond("GROUPfuture_n > 0 && GROUPenroll_mode === 'piecewise'",fields(textInput(id("enroll_cuts"),paste0("入组切点（当前 DCO 后",u,"）"),d$enroll_cuts),textInput(id("enroll_rates"),paste0("分段入组率（人/",u,"）"),d$enroll_rates))),
    dropout_parameter_fields(pre,unit,d))
}
core_prediction_inputs <- function() {
  nav_panel("任务参数",value="inputs",
    div(class="page-title",uiOutput("task_heading"),actionButton("run","运行预测",class="btn-primary")),uiOutput("input_context"),
    navset_card_tab(id="input_tabs",
      nav_panel("队列与资料",value="cohort",
        radioButtons("input_mode","输入方式",c("参数输入"="parameters","ADTTE CSV"="adtte"),"parameters",inline=TRUE),
        radioButtons("analysis_mode","组别处理",c("盲态 / 合并"="pooled","已知两组"="grouped"),"pooled",inline=TRUE),
        div(style="display:none",numericInput("group_count","组数",2,min=2,max=2)),
        selectInput("time_unit","时间单位",c("日"="days","周"="weeks","月"="months"),"months"),
        p(class="field-note","1 周 = 7 日；1 月 = 365.25/12 日。切换单位同步换算已填时间和率。"),
        fields(dateInput("origin","研究起点日期","2025-01-01"),numericInput("cut","当前 DCO（研究月数）",0,min=0)),
        conditionalPanel("input.input_mode === 'parameters' && input.analysis_mode !== 'grouped'",
          fields(numericInput("active_n","当前仍随访人数 Nactive",0,min=0,max=5000,step=1),numericInput("known_n","当前已记录事件数 D0",0,min=0,step=1)),
          conditionalPanel("input.active_n > 0",fields(selectInput("age_mode","随访年龄",c("全部相同"="fixed","0 至最大值均匀分布"="uniform")),numericInput("duration","已随访年龄 a / 最大值（月）",8,min=0))),
          p(class="field-note","设计起点预测：当前 DCO、Nactive 和 D0 均填 0。永久退出者不计入 Nactive。")),
        conditionalPanel("input.input_mode === 'adtte'",fileInput("file","ADTTE CSV",accept=".csv"),
          p(class="field-note","必填 USUBJID、PARAMCD、STARTDT、ADT、AVAL、CNSR；每位患者每个所选终点保留一条分析记录。"),
          fields(uiOutput("endpoint_ui"),selectInput("date_encoding","CSV 日期格式",c("YYYY-MM-DD"="iso","SAS 日期数值"="sas"))),
          selectInput("aval_unit","文件 AVAL 单位",c("日"="days","周"="weeks","月"="months"),"days"),
          fields(selectInput("offset","AVAL 换算为日 = ADT - STARTDT +",c("1"=1,"0"=0)),textInput("dropout_codes","永久退出 CNSR 编码","2")),
          fields(uiOutput("flag_ui"),textInput("flag_value","分析标志保留值","Y")),
          conditionalPanel("input.analysis_mode === 'grouped'",uiOutput("group_column_ui")),
          p(class="field-note","CNSR=0 为事件；仍随访者的 ADT 需确认至当前 DCO。"),downloadButton("template","下载合成模板"))),
      nav_panel("事件模型",value="eventmodel",
        conditionalPanel("input.input_mode === 'parameters' && input.analysis_mode !== 'grouped'",event_parameter_fields()),
        conditionalPanel("input.input_mode === 'adtte'",checkboxGroupInput("methods","拟合模型",choices,c("exponential","weibull","pwe"),inline=TRUE),
          conditionalPanel("input.analysis_mode !== 'grouped' && input.methods.indexOf('pwe') >= 0",textInput("cuts","PWE 切点（随访月数）","3,6,12")),
          p(class="field-note","使用拟合参数预测，区间包含未来随机性，未包含参数估计不确定性。"))),
      nav_panel("分组参数",value="groupparams",uiOutput("group_inputs")),
      nav_panel("未来入组与退出",value="future",
        conditionalPanel("input.analysis_mode !== 'grouped'",numericInput("future_n","未来入组人数",300,min=0,max=1000,step=1),
          conditionalPanel("input.future_n > 0",selectInput("enroll_mode","未来入组方式",c("恒定"="constant","分段"="piecewise"))),
          conditionalPanel("input.future_n > 0 && input.enroll_mode === 'constant'",numericInput("enroll_rate","入组率（人/月）",15,min=0)),
          conditionalPanel("input.future_n > 0 && input.enroll_mode === 'piecewise'",fields(textInput("enroll_cuts","入组切点（当前 DCO 后月数）","3,6"),textInput("enroll_rates","分段入组率（人/月）","9,18,12"))),dropout_parameter_fields()),
        conditionalPanel("input.analysis_mode === 'grouped'",p(class="field-note","各组的入组与退出参数在分组参数页填写。"))),
      nav_panel("运行设置",value="runsettings",
        conditionalPanel("output.task_goal === 'target'",numericInput("target","累计目标事件数 D*",180,min=1,step=1)),
        numericInput("horizon","当前 DCO 后预测窗口（月）",48,min=1/30.4375,max=3650/30.4375),
        fields(numericInput("sims","模拟次数",300,min=50,max=2000,step=50),numericInput("seed","随机种子",20261003,min=0,max=.Machine$integer.max,step=1)))))
}
core_simulation_ui <- function() {
  nav_panel("生存数据模拟",value="simulation",
    div(class="page-title",h2("生成生存数据"),actionButton("sim_run","生成数据",class="btn-primary")),
    navset_card_tab(id="sim_tabs",
      nav_panel("研究设置",fields(selectInput("sim_unit","时间单位",c("日"="days","周"="weeks","月"="months"),"months"),dateInput("sim_origin","研究起点日期","2025-01-01")),
        fields(selectInput("sim_endpoint","终点",c("PFS","OS")),numericInput("sim_n","计划总人数 N",300,min=1,max=5000,step=1)),
        numericInput("sim_groups","组数（1 或 2）",2,min=1,max=2,step=1),
        selectInput("sim_enroll_mode","总体入组方式",c("恒定 Poisson"="constant","分段 Poisson"="piecewise")),
        conditionalPanel("input.sim_enroll_mode === 'constant'",numericInput("sim_enroll_rate","总体入组率（人/月）",15,min=.000001)),
        conditionalPanel("input.sim_enroll_mode === 'piecewise'",fields(textInput("sim_enroll_cuts","入组切点（研究月数）","3,6"),textInput("sim_enroll_rates","各段入组率（人/月）","9,18,12"))),
        div(style="display:none",selectInput("sim_cut_mode","分析截点规则",c("固定 DCO"="fixed"),"fixed")),
        textInput("sim_cuts","分析 DCO（研究月数，可多个）","12,24,36"),textInput("sim_fixed","描述生存率的随访时点（月）","6,12,18"),
        fields(numericInput("sim_reps","重复生成次数",1,min=1,max=200,step=1),numericInput("sim_seed","随机种子",20261004,min=0,max=.Machine$integer.max,step=1))),
      nav_panel("组别与分布",uiOutput("sim_group_inputs")),
      nav_panel("模拟结果",uiOutput("sim_status"),DTOutput("sim_overview",fill=FALSE),
        fields(selectInput("sim_trial","查看试验",character()),selectInput("sim_view_cut","查看截点",character())),
        plotlyOutput("sim_km",height="390px"),DTOutput("sim_summary",fill=FALSE),DTOutput("sim_fixed_table",fill=FALSE),DTOutput("sim_risk",fill=FALSE),
        div(class="export-bar",downloadButton("sim_observed_download","观察数据 CSV"),downloadButton("sim_adtte_download","ADTTE CSV")),uiOutput("sim_export_note"),DTOutput("sim_data",fill=FALSE)),
      nav_panel("导出",div(class="export-bar",downloadButton("sim_truth_download","模拟真值 CSV"),downloadButton("sim_summary_download","生存摘要 CSV"),downloadButton("sim_fixed_download","固定时点 CSV"),downloadButton("sim_cuts_download","截点 CSV"),downloadButton("sim_config_download","配置 JSON"),downloadButton("sim_script_download","复现 R 脚本"),downloadButton("sim_report_download","报告 Markdown")))))
}
core_ui <- function() page_fillable(title="event_pred",theme=theme,fillable=FALSE,
  tags$head(tags$link(rel="icon",href="data:,"),tags$link(rel="stylesheet",href="style.css"),tags$link(rel="stylesheet",href="handbook.css"),core_math_head()),
  div(class="app-header",div(span("event_pred",class="brand"),span("生存模拟与事件数预测",class="app-title")),div(class="header-meta",paste("1.0 核心候选版 ·",core_version,"· 发布候选"))),
  navset_pill_list(id="nav",widths=c(2,10),well=FALSE,
    core_workflow_home(),core_prediction_inputs(),core_simulation_ui(),
    nav_panel("预测结果",value="forecast",div(class="page-title",h2("预测结果"),uiOutput("run_label")),uiOutput("run_status"),uiOutput("metrics"),
      navset_card_tab(id="forecast_tabs",
        nav_panel("任务结果",value="main",
          conditionalPanel("output.task_goal === 'count'",section("累计事件数与逐点 95% 预测区间",plotlyOutput("event_plot",height="390px")),section("窗口末累计及新增事件数",DTOutput("count_end_table",fill=FALSE),DTOutput("new_count_table",fill=FALSE))),
          conditionalPanel("output.task_goal === 'target'",section("达标日期",DTOutput("milestones",fill=FALSE),uiOutput("mcse_note")),section("窗口内达标概率",plotlyOutput("prob_plot",height="300px")))),
        nav_panel("分组事件",value="groupresults",uiOutput("group_result_note"),plotlyOutput("group_event_plot",height="350px"),DTOutput("group_event_table",fill=FALSE),downloadButton("group_curves_download","分组曲线 CSV"),downloadButton("group_summary_download","分组汇总 CSV")),
        nav_panel("参数与计算记录",DTOutput("model_parameters",fill=FALSE),downloadButton("parameters_download","等价参数 CSV"),DTOutput("diagnostics",fill=FALSE),DTOutput("group_fit_table",fill=FALSE)),
        nav_panel("导出",conditionalPanel("output.task_goal === 'count'",downloadButton("curves_download","事件曲线 CSV")),conditionalPanel("output.task_goal === 'target'",downloadButton("summary_download","达标日期 CSV")),downloadButton("config_download","配置 JSON"),downloadButton("report_download","报告 Markdown")))),
    nav_panel("输入数据与分布",value="data",uiOutput("data_status"),DTOutput("data_table",fill=FALSE),plotlyOutput("fit_plot",height="360px")),
    nav_panel("工作手册",value="methods",core_handbook_ui())),
  div(class="site-footer",paste("event_pred · 默认月 ·",core_version,"· 核心检查记录随源码提供")))
