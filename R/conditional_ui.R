register_conditional_help <- function() {
 parameter_help <- get("parameter_help",envir=parent.frame())
conditional_help <- c(
 ci_purpose="选择生存估计变化、首次IA未来拒绝概率，或历史多次IA后的组序贯预测。历史需求需每次实际ADTTE快照，不能从最后一次资料恢复已知的历史分析。",
 ci_pr_control="明确指定Control组，另一组作为Treatment。正Z表示Treatment获益；不能使用合并盲态资料。",
 ci_pr_design="单次Final仅使用一个预设检验；普通组序贯从首次IA继续，历史多次IA从选定次序继续。均沿用原计划边界和最终D*，本入口不重估事件目标。",
 ci_pr_sided="单侧Treatment获益或双侧两方向差异。双侧反方向跨界也记为拒绝H0，并分别报告。",
 ci_pr_alpha="原方案的单次Final或完整组序贯路径总alpha，0.0001–0.2；例如单侧0.025或双侧0.05。",
 ci_pr_miss="单次Final事件目标未达时预设关闭且不拒绝，或在窗口末执行一次最终检验。不是看到结果后选择。组序贯只关闭不追加检验。",
 ci_pr_timing="原计划2–5个递增信息比例，从第一次分析开始并含最终1，首个至少0.1。历史模式需各次事件数等于ceil(D*×该次比例)，不能只填写尚未完成的比例。",
 ci_pr_spending="沿用原组序贯方案的OBF型或Pocock型alpha消耗；取整后的事件比例用于边界。",
 ci_pr_futility="沿用首次IA及后续的非约束性无效停止规则，单侧可选预设Z，双侧不设置。两次分析例如预先设0。",
 ci_pr_futility_z="原方案K−1个有限无效Z，−5至5且低于对应效力界。两次分析例如0；当前IA已无效停止时不再模拟检验。",
 ci_source="选择IA个体数据来源。可使用ADTTE文件、设计模拟最近成功运行中的一个截点，或内置合成示例；示例只用于试用功能。",
 ci_history_look="选择文件内原计划的历史分析次序。建议先选最近已完成IA；从1至该次必须全部提供，后续快照不进入校验、模型拟合或未来预测。",
 ci_unit="本任务输入与结果显示的时间单位，默认月。切换会转换时间与风险率；不改变日期或概率，1月按30.4375日换算。",
 ci_file="上传CSV、XPT或SAS7BDAT。必填USUBJID、PARAMCD、STARTDT、ADT、AVAL、CNSR；历史模式另需IASEQ、DCO及组别，每个截点保留完整患者快照。",
 ci_origin="研究起点日期，用于把STARTDT与ADT转换为研究经过时间；建议填与研究计划一致的起点，且不晚于任一STARTDT。",
 ci_cut="IA或当前DCO，自研究起点起的时间。仍随访者ADT须确认至该截点；本入口暂不补全IA之前未观察的区间。",
 ci_endpoint="选择文件中的PARAMCD。一次分析一个终点，如PFS或OS；筛选后每个USUBJID需恰有一条记录。",
 ci_aval_unit="文件AVAL自身的单位，与平台显示单位分别指定。换算为日后必须与ADT-STARTDT及所选首日偏移相符。",
 ci_offset="AVAL换算为日后是否包含首日：1表示ADT-STARTDT+1，0表示ADT-STARTDT。内部分析使用不含首日偏移的经过时间。",
 ci_dropout_codes="明确填写永久退出随访的正CNSR编码，逗号分隔。0为事件，其余正编码为仍随访；例如2表示永久退出，1为行政删失。",
 ci_date_encoding="CSV日期可为YYYY-MM-DD或SAS日期数值；XPT/SAS文件按读取后日期类型处理。建议使用ISO日期以减少编码歧义。",
 ci_flag="可选分析标志变量名，留空表示不筛选。如ANL01FL；填写时需存在于文件中并与下方保留值配合使用。",
 ci_flag_value="分析标志保留值，例如Y。仅在填写分析标志变量名时使用；筛选必须按所选终点保持每人一条分析记录。",
 ci_grouped="合并时使用一套分布和未来过程；已知组别时各组分别拟合或指定模型。合并处理不尝试恢复未知治疗分组。",
 ci_group_column="选择文件中的组别变量，如TRTP或TRTA。已知组别模式需完整非空且最多6组；变量含义按研究分析计划确定。",
 ci_design_trial="选择设计阶段最近成功生成的一轮试验，作为条件模拟IA数据。只读取该轮已观察记录，不读取未观察模拟真值。",
 ci_design_cut="选择该轮试验的IA截点，研究起点与终点沿用该次生成配置。多个Final截点必须晚于这个选定IA。",
 ci_model_mode="从IA数据拟合需要每组至少2个事件；指定分布可用于信息不足时，但仍需IA个体记录才能建立历史KM。",
 ci_uncertainty="固定参数、受试者Bootstrap、指数/PWE Gamma后验或Weibull MCMC。Bootstrap只重拟合模型，续推使用原IA风险集。",
 ci_cut_mode="选择固定Final DCO列表或累计事件达到D*。事件驱动未达标时按最大窗口末分析，保留该次模拟并标记未达标。",
 ci_cuts="自研究起点起的Final DCO列表，1–20个递增且晚于IA的时间。生存估计可填24,30,36月；拒绝概率只填一个预定Final DCO。不是IA之后的增量。",
 ci_target="目标累计事件数D*，包含IA已记录事件。历史组序贯需填写原计划最终目标，不能只填写剩余事件；未达下一目标时只关闭窗口、不新增检验。",
 ci_horizon="事件驱动最长允许从IA延伸的时间，需为正且换算后不超过3650日。窗口末未达标不当作失败拟合或删除。",
 ci_fixed="固定个体随访时点，例如6,12,18月。比较相同个体年龄下IA与Final的KM生存率；超过观察支持时不外推。",
 ci_reps="重复条件模拟数，1–500整数。正式比较建议至少200；可先使用20–50调试设置。模拟次数影响Monte Carlo误差。",
 ci_seed="0至2147483647的随机种子。相同版本、数据和配置可复现，q情景共享每轮模型抽样及未来随机数以便比较。",
 ci_scenarios="勾选后输入多个统一q情景；实际各组未来风险调整为组内q乘以情景q。仅调整条件续推与未来入组的事件过程。",
 ci_q_values="1–10个不同正数，逗号分隔。例如0.75,1,1.25；小于1表示未来风险降低。历史事件和退出记录均不调整。",
 ci_process="输入入组/退出率，或从IA资料估计Gamma过程后验。后验按组使用历史入组计数与永久退出随访人时，仅支持恒定入组。",
 ci_recruit_start="估计入组后验的历史研究时间窗口起点，需在0与IA之间。应使用包含完整入组记录的窗口，不以随访年龄代替研究时间。",
 ci_recruit_end="历史入组窗口终点，晚于窗口起点且不超过IA。入组Gamma后验以窗口内该组入组数与窗口长度更新。",
 ci_trial="查看最近成功运行的一次条件模拟，不会重新运行。Final个体数据与ADTTE下载对应所选模拟、截点及q情景。",
 ci_view_cut="选择Final截点序号；固定DCO模式各截点共享同一条条件患者轨迹。事件驱动各轮实际DCO可能不同。",
 ci_view_q="查看一次运行中的一个q情景；选择只影响单轮KM、记录及单轮导出，不改变全轮汇总和配置。")
parameter_help<-c(parameter_help,conditional_help)
for(id in c("prior_shape","prior_rate","log_eta_mean","log_eta_sd","log_shape_mean","log_shape_sd","mcmc_chains","mcmc_warmup","mcmc_draws","enroll_prior_shape","enroll_prior_rate","drop_prior_shape","drop_prior_rate"))parameter_help[paste0("ci_",id)]<-parameter_help[id]
for(j in 1:6) {
 pre<-paste0("c",j,"_");base<-names(group_input_defaults())
 for(id in base)if(id %in% names(parameter_help))parameter_help[paste0(pre,id)]<-parameter_help[id]
 extra<-c("survival_time","survival_prob","pwe_input","pwe_survivals","pwe_last_rate")
 parameter_help[paste0(pre,extra)]<-parameter_help[paste0("s1_",extra)]
}
 assign("parameter_help",parameter_help,envir=parent.frame())
}
conditional_simulation_ui <- function() {
 nav_panel("IA→Final 条件模拟",value="conditional",
  div(class="page-title",h2("从 IA 继续模拟 Final"),div(actionButton("ci_run","运行条件模拟",class="btn-primary"),conditionalPanel("input.ci_purpose === 'rejection' || input.ci_purpose === 'history'",actionButton("ci_batch_freeze","冻结当前参数至批量研究")))),
  navset_card_tab(id="ci_tabs",
   nav_panel("IA 数据",value="ci_data",
    section("本次需求",selectInput("ci_purpose","预测内容",c("生存估计与IA变化"="survival","未来拒绝概率"="rejection","历史多次IA后的组序贯预测"="history"))),
    section("数据来源",conditionalPanel("input.ci_purpose !== 'history'",selectInput("ci_source","IA 数据来源",c("ADTTE 文件"="adtte","设计模拟最近成功运行"="design","内置合成示例（240人，IA=540日）"="demo"),"adtte")),
     selectInput("ci_unit","时间单位",c("日"="days","周"="weeks","月"="months"),"months"),
     conditionalPanel("input.ci_source === 'adtte' || input.ci_purpose === 'history'",fileInput("ci_file","ADTTE 文件",accept=c(".csv",".xpt",".sas7bdat")),
      dateInput("ci_origin","研究起点日期","2025-01-01"),conditionalPanel("input.ci_purpose !== 'history'",numericInput("ci_cut","IA DCO（研究月数）",18,min=.001)),
      conditionalPanel("input.ci_purpose === 'history'",selectInput("ci_history_look","从哪次历史IA继续",character()),p(class="field-note","历史长表需增加IASEQ和DCO。每个IA须为完整患者快照；IASEQ为原分析次序，DCO为该次日期。只读取截至所选IA的记录。"),downloadButton("ci_history_template","下载历史IA合成模板")),
      fields(uiOutput("ci_endpoint_ui"),selectInput("ci_date_encoding","CSV 日期编码",c("YYYY-MM-DD"="iso","SAS 日期"="sas"))),
      fields(selectInput("ci_aval_unit","文件 AVAL 单位",c("日"="days","周"="weeks","月"="months"),"days"),selectInput("ci_offset","AVAL 日数首日偏移",c("1"=1,"0"=0))),
      fields(textInput("ci_dropout_codes","永久退出 CNSR 编码","2"),textInput("ci_flag","分析标志变量（可留空）","")),textInput("ci_flag_value","分析标志保留值","Y"),conditionalPanel("input.ci_purpose !== 'history'",downloadButton("ci_template","下载 ADTTE 合成模板"))),
     conditionalPanel("input.ci_source === 'design' && input.ci_purpose !== 'history'",fields(selectInput("ci_design_trial","设计模拟试验",character()),selectInput("ci_design_cut","IA 截点",character()))),
     conditionalPanel("input.ci_purpose === 'survival'",radioButtons("ci_grouped","组别处理",c("合并"="pooled","已知组别"="grouped"),"pooled",inline=TRUE)),
     conditionalPanel("(input.ci_grouped === 'grouped' || (input.ci_purpose === 'rejection' || input.ci_purpose === 'history')) && (input.ci_source === 'adtte' || input.ci_purpose === 'history')",uiOutput("ci_group_column_ui")),
     conditionalPanel("(input.ci_purpose === 'rejection' || input.ci_purpose === 'history')",selectInput("ci_pr_control","Control组",character()),p(class="field-note","需要已知组别的两组个体记录。原历史不重抽样；正Z表示另一组Treatment获益。")),
     p(class="field-note","需要 IA 个体记录以构建历史 KM。仍随访者须确认至 IA；已发生事件和永久退出保持原记录。"),uiOutput("ci_data_note"),DTOutput("ci_baseline_data",fill=FALSE),conditionalPanel("input.ci_purpose === 'history'",uiOutput("ci_history_note"),DTOutput("ci_history_preview",fill=FALSE)))),
   nav_panel("续推设置",value="settings",conditionalPanel("input.ci_purpose !== 'history' || output.ci_history_state !== 'stopped'",
    section("事件模型",radioButtons("ci_model_mode","模型来源",c("从 IA 数据拟合"="fit","指定分布参数"="manual"),"fit",inline=TRUE),
     conditionalPanel("input.ci_model_mode === 'fit'",selectInput("ci_uncertainty","事件参数处理",c("固定拟合参数"="plugin","受试者 Bootstrap"="bootstrap","指数/PWE Gamma 后验"="gamma","Weibull MCMC 后验"="bayes_weibull")),
      conditionalPanel("input.ci_uncertainty === 'gamma'",fields(numericInput("ci_prior_shape","事件 Gamma 先验 shape",.5,min=.01),numericInput("ci_prior_rate","事件 Gamma 先验 rate（受试者·月）",50/30.4375,min=.001))),
      conditionalPanel("input.ci_uncertainty === 'bayes_weibull'",
       fields(numericInput("ci_log_eta_mean","log(eta/月)先验均值",log(450/30.4375)),numericInput("ci_log_eta_sd","log(eta)先验标准差",1,min=.01)),
       fields(numericInput("ci_log_shape_mean","log(k)先验均值",0),numericInput("ci_log_shape_sd","log(k)先验标准差",.75,min=.01)),
       fields(numericInput("ci_mcmc_chains","MCMC 链数",4,min=2,max=4),numericInput("ci_mcmc_warmup","每链预热",1000,min=200,max=10000)),numericInput("ci_mcmc_draws","每链保留",3000,min=200,max=10000)))),
    uiOutput("ci_groups_ui"),
    section("过程参数与情景",selectInput("ci_process","入组 / 脱落参数处理",c("使用输入率"="fixed","按组 Gamma 后验"="gamma")),
     conditionalPanel("input.ci_process === 'gamma'",
      fields(numericInput("ci_recruit_start","历史入组窗口起点（研究月数）",0,min=0),numericInput("ci_recruit_end","历史入组窗口终点（研究月数）",12,min=.001)),
      fields(numericInput("ci_enroll_prior_shape","入组先验 shape",1,min=.01),numericInput("ci_enroll_prior_rate","入组先验 rate（月）",1/30.4375,min=.000001)),
      fields(numericInput("ci_drop_prior_shape","脱落先验 shape",.5,min=.01),numericInput("ci_drop_prior_rate","脱落先验 rate（受试者·月）",50/30.4375,min=.000001))),
     checkboxInput("ci_scenarios","比较多个统一 q 情景",FALSE),conditionalPanel("input.ci_scenarios",textInput("ci_q_values","q 情景值",".75,1,1.25"))))),
   nav_panel("Final 与运行",value="final",
    section("Final 分析规则",conditionalPanel("(input.ci_purpose === 'rejection' || input.ci_purpose === 'history')",conditionalPanel("input.ci_purpose !== 'history'",selectInput("ci_pr_design","检验路径",c("单次Final检验"="fixed","原计划组序贯续推"="sequential"))),fields(selectInput("ci_pr_sided","检验方向",c("单侧Treatment获益"="benefit","双侧差异"="two")),numericInput("ci_pr_alpha","总alpha",.025,min=.0001,max=.2))),
     conditionalPanel("input.ci_purpose === 'survival' || (input.ci_pr_design !== 'sequential' && input.ci_purpose !== 'history')",selectInput("ci_cut_mode","Final 截点规则",c("固定 DCO 列表"="fixed","达到累计目标事件数"="target"))),
     conditionalPanel("input.ci_cut_mode === 'fixed' && (input.ci_purpose === 'survival' || (input.ci_pr_design !== 'sequential' && input.ci_purpose !== 'history'))",textInput("ci_cuts","Final DCO（研究月数）","24,30,36")),
     conditionalPanel("input.ci_cut_mode === 'target' || ((input.ci_purpose === 'rejection' || input.ci_purpose === 'history') && (input.ci_pr_design === 'sequential' || input.ci_purpose === 'history'))",numericInput("ci_target","累计目标事件数 D*",180,min=1,step=1),conditionalPanel("input.ci_purpose !== 'history' || output.ci_history_state !== 'stopped'",numericInput("ci_horizon","IA 后最大窗口（月）",36,min=.01))),
     conditionalPanel("(input.ci_purpose === 'rejection' || input.ci_purpose === 'history') && (input.ci_pr_design === 'fixed' && input.ci_purpose !== 'history') && input.ci_cut_mode === 'target'",selectInput("ci_pr_miss","窗口末未达D*时",c("关闭试验，不拒绝"="no_reject","窗口末执行一次Final检验"="analyze"))),
     conditionalPanel("(input.ci_purpose === 'rejection' || input.ci_purpose === 'history') && (input.ci_pr_design === 'sequential' || input.ci_purpose === 'history')",textInput("ci_pr_timing","原计划信息比例（从分析1至最终）","0.5,1"),selectInput("ci_pr_spending","原alpha消耗函数",c("OBF型"="asOF","Pocock型"="asP")),conditionalPanel("input.ci_pr_sided === 'benefit'",selectInput("ci_pr_futility","原无效停止规则",c("不设"="none","非约束性Z"="z","约束性beta消耗"="beta")),conditionalPanel("input.ci_pr_futility === 'z'",textInput("ci_pr_futility_z","原期中无效Z界值","0")),conditionalPanel("input.ci_pr_futility === 'beta'",
      fields(numericInput("ci_pr_beta","原设计beta",.2,min=.01,max=.5),selectInput("ci_pr_beta_spending","原beta消耗",c("OBF型"="bsOF","Pocock型"="bsP","HSD型"="bsHSD"))),
      conditionalPanel("input.ci_pr_beta_spending === 'bsHSD'",numericInput("ci_pr_beta_gamma","原HSD gamma",-2,min=-10,max=5)),
      fields(numericInput("ci_pr_design_hr","原设计备择HR",.67,min=.01,max=.99),numericInput("ci_pr_allocation","原Treatment分配概率",.5,min=.01,max=.99)))),p(class="field-note","普通组序贯从首次IA继续；历史模式从所选次序继续，并核对之前各次事件数与停止规则。填写从第一次分析开始的完整原计划；不改变最终D*。"),DTOutput("ci_prediction_plan",fill=FALSE)),
     conditionalPanel("input.ci_purpose === 'survival'",textInput("ci_fixed","固定个体随访时点（月）","6,12,18")),conditionalPanel("input.ci_purpose !== 'history' || output.ci_history_state !== 'stopped'",fields(numericInput("ci_reps","重复条件模拟数",100,min=1,max=500,step=10),numericInput("ci_seed","随机种子",20261004,min=0,step=1))))),
   nav_panel("条件模拟结果",value="results",uiOutput("ci_status"),
    conditionalPanel("output.ci_result_purpose === 'rejection'",section("原IA观察与决策",DTOutput("ci_prediction_ia",fill=FALSE),conditionalPanel("output.ci_result_has_history === 'yes'",DTOutput("ci_history_result",fill=FALSE),downloadButton("ci_history_download","已观察历史分析 CSV"))),section("条件拒绝概率",uiOutput("ci_prediction_note"),DTOutput("ci_prediction_overview",fill=FALSE)),section("停止与资源",DTOutput("ci_prediction_resources",fill=FALSE))),
    conditionalPanel("output.ci_result_purpose !== 'rejection'",
    section("IA 基线",DTOutput("ci_baseline_summary",fill=FALSE)),section("Final 中位时间与 IA 变化",DTOutput("ci_overview",fill=FALSE),p(class="field-note","分位数在 Final 中位时间可估计的模拟中计算。增长概率分别报告联合概率和可估计条件下概率；IA 为 NR 时增长概率未定义。")),
    section("中位时间增长概率",DTOutput("ci_probability",fill=FALSE)),
    section("固定时点生存率与 IA 变化",DTOutput("ci_fixed_overview",fill=FALSE)),
    conditionalPanel("output.ci_result_cut_mode === 'target'",section("事件目标",DTOutput("ci_target_summary",fill=FALSE)))),
    section("查看一轮",fields(selectInput("ci_trial","查看模拟",character()),conditionalPanel("output.ci_result_purpose !== 'rejection'",selectInput("ci_view_cut","查看 Final 截点",character()))),selectInput("ci_view_q","查看 q 情景",character()),conditionalPanel("output.ci_result_purpose !== 'rejection'",plotlyOutput("ci_km",height="390px"),DTOutput("ci_summary",fill=FALSE)),conditionalPanel("output.ci_result_purpose === 'rejection'",DTOutput("ci_prediction_trial",fill=FALSE),DTOutput("ci_prediction_path",fill=FALSE)),DTOutput("ci_observed",fill=FALSE),
     div(class="export-bar",downloadButton("ci_observed_download","本轮连续观察数据 CSV"),downloadButton("ci_adtte_download","本轮 ADTTE CSV")),uiOutput("ci_export_note")),
    section("拟合与模拟诊断",DTOutput("ci_model_table",fill=FALSE),DTOutput("ci_diagnostics",fill=FALSE),DTOutput("ci_process_table",fill=FALSE),DTOutput("ci_failures",fill=FALSE))),
   nav_panel("导出",value="export",div(class="export-bar",conditionalPanel("output.ci_result_purpose !== 'rejection'",downloadButton("ci_all_summary_download","每轮中位数与变化 CSV"),downloadButton("ci_all_fixed_download","每轮生存率与变化 CSV")),conditionalPanel("output.ci_result_purpose === 'rejection'",downloadButton("ci_prediction_trials_download","每轮决策 CSV"),downloadButton("ci_prediction_looks_download","逐次分析 CSV"),downloadButton("ci_prediction_summary_download","拒绝概率汇总 CSV")),downloadButton("ci_cuts_download","分析截点 CSV"),downloadButton("ci_config_download","数据与配置 JSON"),downloadButton("ci_script_download","复现 R 脚本"),downloadButton("ci_report_download","模拟记录 Markdown")))))
}
conditional_group_ui <- function(j,label,unit,d) {
 pre<-paste0("c",j,"_");id<-function(x)paste0(pre,x);u<-time_label(unit)
 section(label,
  conditionalPanel("input.ci_model_mode === 'fit'",selectInput(id("fit_method"),"该组拟合模型",choices,if(nzchar(d$fit_method))d$fit_method else "weibull"),
   conditionalPanel(paste0("input.",pre,"fit_method === 'pwe'"),textInput(id("cuts"),paste0("PWE切点（随访",u,"数）"),d$cuts)),
   conditionalPanel(paste0("input.",pre,"fit_method === 'km_tail'"),numericInput(id("tail_rate"),paste0("KM尾部风险率（每",u,"）"),d$tail_rate,min=.000001))),
  conditionalPanel("input.ci_model_mode === 'manual'",event_parameter_fields(pre,unit,d,simulation=TRUE)),
  numericInput(id("multiplier"),"未来风险调整系数 q",d$multiplier,min=.001),numericInput(id("future_n"),"该组继续入组人数",0,min=0,max=1000,step=1),
  conditionalPanel(paste0("input.",pre,"future_n > 0 && input.ci_process === 'fixed'"),selectInput(id("enroll_mode"),"未来入组方式",c("恒定"="constant","分段"="piecewise")),
   conditionalPanel(paste0("input.",pre,"enroll_mode === 'constant'"),numericInput(id("enroll_rate"),paste0("该组入组率（人/",u,"）"),d$enroll_rate,min=0)),
   conditionalPanel(paste0("input.",pre,"enroll_mode === 'piecewise'"),fields(textInput(id("enroll_cuts"),paste0("IA后入组切点（",u,"）"),d$enroll_cuts),textInput(id("enroll_rates"),paste0("各段入组率（人/",u,"）"),d$enroll_rates)))),
  conditionalPanel("input.ci_process === 'fixed'",dropout_parameter_fields(pre,unit,d)))
}
register_conditional_simulation_server <- function(input,output,session,design_result,batch_bridge=NULL) {
 res<-reactiveVal(NULL);err<-reactiveVal(NULL);unit<-reactive(input$ci_unit %||% "months");last_unit<-reactiveVal("months")
 register_parameter_controls(input,output,session,unit,paste0("c",1:6,"_"),simulation_defaults,simulation_parameter_model)
 raw<-reactive({req(input$ci_file);read_adtte(input$ci_file$datapath,input$ci_file$name)})
 output$ci_endpoint_ui<-renderUI({x<-tryCatch(raw(),error=function(e)NULL);selectInput("ci_endpoint","终点 PARAMCD",if(is.null(x))character() else unique(as.character(x$PARAMCD)))})
 output$ci_group_column_ui<-renderUI({x<-tryCatch(raw(),error=function(e)NULL);v<-if(is.null(x))character() else names(x);selectInput("ci_group_column","组别变量",v,if("TRTP" %in% v)"TRTP" else if("TRTA" %in% v)"TRTA" else "")})
 observeEvent(design_result(),{r<-design_result();req(r);updateSelectInput(session,"ci_design_trial",choices=unique(r$cuts$SIMID),selected=1);updateSelectInput(session,"ci_design_cut",choices=unique(r$cuts$CUTID),selected=1)})
 source_data<-reactive({
  src<-if(identical(input$ci_purpose,"history"))"history" else input$ci_source %||% "adtte"
  if(src=="history") {
   if(is.null(input$ci_file))stop("请上传包含IASEQ与DCO的历史ADTTE长表，或先下载合成模板。")
   req(input$ci_endpoint,input$ci_history_look);return(normalize_ia_history(raw(),input$ci_endpoint,as.character(input$ci_origin),as.integer(input$ci_history_look),as.numeric(input$ci_offset),parse_unit_numbers(input$ci_dropout_codes),input$ci_flag %||% "",input$ci_flag_value %||% "Y",input$ci_date_encoding %||% "iso",input$ci_aval_unit %||% "days",input$ci_group_column))
  } else if(src=="demo") {
   x<-validate_data(demo_data(),540);x$group<-rep(c("A","B"),length.out=nrow(x));cut<-540;origin<-"2025-01-01";endpoint<-"OS"
  } else if(src=="design") {
   r<-design_result();if(is.null(r))stop("请先在设计阶段生成生存数据，再选择一轮及一个IA截点。")
   req(input$ci_design_trial,input$ci_design_cut);o<-r$observed[r$observed$SIMID==as.integer(input$ci_design_trial)&r$observed$CUTID==as.integer(input$ci_design_cut),,drop=FALSE]
   if(!nrow(o))stop("选定IA截点前无入组者。")
   x<-data.frame(id=o$USUBJID,entry=o$entry_day,time=o$time_day,obs_day=o$obs_day,status=o$status,event=o$event,group=o$group)
   cut<-o$DCO_DAY[1];origin<-r$config$origin;endpoint<-r$config$paramcd
  } else {
   if(is.null(input$ci_file))stop("选择ADTTE文件，或使用设计模拟记录 / 内置合成示例。")
   req(input$ci_endpoint);cut<-input$ci_cut*time_factor(unit());origin<-as.character(input$ci_origin);endpoint<-input$ci_endpoint
   x<-normalize_adtte(raw(),endpoint,origin,cut,as.numeric(input$ci_offset),parse_unit_numbers(input$ci_dropout_codes),input$ci_flag %||% "",input$ci_flag_value %||% "Y",input$ci_date_encoding %||% "iso","strict",input$ci_aval_unit %||% "days",group_column=if(input$ci_grouped=="grouped"||identical(input$ci_purpose,"rejection"))input$ci_group_column else NULL)
  }
  if(!identical(input$ci_grouped,"grouped")&&!identical(input$ci_purpose,"rejection"))x$group<-"合并"
  if(anyNA(x$group)||any(!nzchar(trimws(x$group)))||length(unique(x$group))>6)stop("组别需非空且最多6组。")
  list(data=x,cut=cut,origin=origin,paramcd=endpoint,source=src)
 })
 labels<-reactive({z<-tryCatch(source_data(),error=function(e)NULL);if(is.null(z))character() else sort(unique(z$data$group))})
 output$ci_groups_ui<-renderUI({labs<-labels();if(!length(labs))return(p(class="field-note","选择有效IA数据后显示各组续推参数。"));u<-isolate(unit())
  do.call(navset_card_tab,lapply(seq_along(labs),function(j){d<-simulation_defaults(u,j);for(id in names(d)){v<-isolate(input[[paste0("c",j,"_",id)]]);if(!is.null(v))d[[id]]<-v};nav_panel(labs[j],conditional_group_ui(j,labs[j],u,d))}))
 })
 outputOptions(output,"ci_groups_ui",suspendWhenHidden=FALSE)
 output$ci_data_note<-renderUI({tryCatch({z<-source_data();p(class="field-note",paste("IA研究时间",signif(z$cut/time_factor(unit()),6),time_label(unit()),"；",nrow(z$data),"人；",sum(z$data$status=="event"),"个事件；",sum(z$data$status=="active"),"人仍随访。",if(z$source=="demo")"当前为合成示例。" else ""))},error=function(e)p(class="field-note",conditionMessage(e)))})
 datatable<-function(d){
  labs<-c(SIMID="模拟序号",CUTID="截点序号",scenario_q="统一q情景",scope="范围",group="组别",DCO_DAY="研究DCO",n="人数",events="事件数",dropouts="永久退出",administrative="行政删失",median_day="Final中位时间",lower_day="KM中位时间95%下限",upper_day="KM中位时间95%上限",median_status="中位数状态",IA_median_day="IA中位时间",median_change_day="中位时间变化",time_day="随访时点",survival="生存率",IA_survival="IA生存率",survival_change="生存率变化",n_risk="在险人数",requested="请求次数",successful="成功次数",median_estimable="Final中位数可估计比例",conditional_lower_day="可估计条件下2.5%分位",conditional_median_day="可估计条件下50%分位",conditional_upper_day="可估计条件下97.5%分位",change_lower_day="变化2.5%分位",change_median_day="变化50%分位",change_upper_day="变化97.5%分位",joint_increase_probability="成功轮次中可估计且增长的概率",failure_bound_lower="含失败轮次的概率下界",failure_bound_upper="含失败轮次的概率上界",conditional_increase_probability="可估计条件下增长概率",conditional_increase_mcse="条件增长概率MCSE",estimable="生存率可估计比例",lower="2.5%分位",median="50%分位",upper="97.5%分位",change_lower="生存率变化2.5%分位",change_median="生存率变化50%分位",change_upper="生存率变化97.5%分位")
  if("scope" %in% names(d))d$scope<-ifelse(d$scope=="overall","总体","组内")
  for(id in names(labs))names(d)<-sub(paste0("^",id,"(?=（|$)"),labs[id],names(d),perl=TRUE)
  t<-DT::datatable(d,rownames=FALSE,class="compact stripe nowrap",options=list(scrollX=TRUE,pageLength=8,dom="tip",language=list(info="显示 _START_ 至 _END_，共 _TOTAL_ 条",infoEmpty="无记录",emptyTable="无记录",paginate=list(previous="上一页",`next`="下一页"))))
  cols<-names(d)[vapply(d,is.double,logical(1))];if(length(cols))DT::formatRound(t,cols,digits=3) else t
 }
 view<-function(d,u){cols<-names(d)[grepl("_day$|^DCO_DAY$",names(d))];unit_table(d,u,cols)}
 output$ci_baseline_data<-renderDT({z<-tryCatch(source_data(),error=function(e)NULL);req(z);datatable(unit_table(z$data,unit(),c("entry","time","obs_day")))})
 observeEvent(input$ci_unit,{
  to<-unit();from<-last_unit();if(to==from)return();ratio<-time_factor(from)/time_factor(to);u<-time_label(to)
  kinds<-list(duration=c("ci_cut","ci_horizon","ci_recruit_start","ci_recruit_end"),duration_text=c("ci_cuts","ci_fixed"),exposure=c("ci_prior_rate","ci_enroll_prior_rate","ci_drop_prior_rate"),log_time="ci_log_eta_mean")
  for(j in 1:6)for(k in names(base_unit_fields))kinds[[k]]<-c(kinds[[k]],paste0("c",j,"_",base_unit_fields[[k]]))
  kinds$duration<-c(kinds$duration,paste0("c",1:6,"_survival_time"));kinds$rate<-c(kinds$rate,paste0("c",1:6,"_pwe_last_rate"))
  for(k in names(kinds))for(id in unique(kinds[[k]])) {
   val<-isolate(input[[id]]);if(is.null(val))next
   v<-tryCatch(switch(k,duration=val*ratio,exposure=val*ratio,rate=val/ratio,log_time=val+log(ratio),duration_text=paste(format(parse_unit_numbers(val)*ratio,digits=16),collapse=","),rate_text=paste(format(parse_unit_numbers(val)/ratio,digits=16),collapse=",")),error=function(e)val)
   freezeReactiveValue(input,id)
   # Update value and the unit label; dimensionless fields are untouched.
   label<-if(id=="ci_cut")paste0("IA DCO（研究",u,"数）") else if(id=="ci_cuts")paste0("Final DCO（研究",u,"数）") else if(id=="ci_fixed")paste0("固定个体随访时点（",u,"）") else if(id=="ci_horizon")paste0("IA 后最大窗口（",u,"）") else {
    field<-sub("^c[1-6]_","",id)
    switch(field,median=paste0("中位时间 mPFS / mOS（",u,"）"),median2=paste0("第二成分中位时间（",u,"）"),eta=paste0("Weibull尺度eta（",u,"）"),log_mu=paste0("对数位置mu=log(m/",u,")"),exp_rate=paste0("指数风险率（每",u,"）"),g_rate=paste0("初始风险率b（每",u,"）"),g_shape=paste0("Gompertz形状g（每",u,"）"),parameter_cuts=paste0("风险切点（",u,"）"),parameter_rates=paste0("各段事件风险率（每",u,"）"),cuts=paste0("PWE切点（随访",u,"数）"),tail_rate=paste0("KM尾部风险率（每",u,"）"),enroll_rate=paste0("该组入组率（人/",u,"）"),enroll_cuts=paste0("IA后入组切点（",u,"）"),enroll_rates=paste0("各段入组率（人/",u,"）"),dropout_rate=paste0("永久脱落风险率（每",u,"）"),drop_period=paste0("脱落概率窗口（",u,"）"),survival_time=paste0("随访时点（",u,"）"),pwe_last_rate=paste0("末段风险率（每",u,"）"),
      ci_prior_rate=paste0("事件先验rate（受试者·",u,"）"),ci_enroll_prior_rate=paste0("入组先验rate（",u,"）"),ci_drop_prior_rate=paste0("脱落先验rate（受试者·",u,"）"),ci_log_eta_mean=paste0("log(eta/",u,")先验均值"),ci_recruit_start=paste0("历史入组窗口起点（研究",u,"数）"),ci_recruit_end=paste0("历史入组窗口终点（研究",u,"数）"),NULL)
   }
   if(k %in% c("duration_text","rate_text"))updateTextInput(session,id,value=v,label=if(is.null(label))NULL else parameter_label(id,label)) else updateNumericInput(session,id,value=v,label=if(is.null(label))NULL else parameter_label(id,label))
  }
  last_unit(to)
 },ignoreInit=TRUE)
 observeEvent(input$ci_uncertainty,{
  method<-switch(input$ci_uncertainty,gamma="exponential",bayes_weibull="weibull",NULL)
  if(!is.null(method))for(j in seq_along(labels()))updateSelectInput(session,paste0("c",j,"_fit_method"),selected=method)
 },ignoreInit=TRUE)
 freeze_input<-function(){
   z<-source_data();f<-time_factor(unit());mode<-input$ci_model_mode %||% "fit";unc<-if(mode=="manual")"plugin" else input$ci_uncertainty %||% "plugin"
   stopped_history<-FALSE
   if(z$source=="history") {hc<-conditional_prediction_input(input,list(target=input$ci_target,reps=1,cut_mode="target"),labels());stopped_history<-tail(conditional_history_path(z$snapshots,hc)$action,1)!="continue"}
   gs<-if(stopped_history)lapply(labels(),function(lab)list(name=lab)) else lapply(seq_along(labels()),function(j){d<-simulation_defaults(unit(),j);pre<-paste0("c",j,"_");v<-setNames(lapply(names(d),function(id)input[[paste0(pre,id)]] %||% d[[id]]),names(d));future<-input[[paste0(pre,"future_n")]] %||% 0
    em<-if(input$ci_process=="gamma"||future==0)"constant" else v$enroll_mode
    list(name=labels()[j],fit_method=input[[paste0(pre,"fit_method")]] %||% "weibull",cuts=if(identical(input[[paste0(pre,"fit_method")]],"pwe"))parse_unit_numbers(v$cuts)*f else numeric(),tail_rate=v$tail_rate/f,
     model=if(mode=="manual")simulation_parameter_model(v$design_model,v,unit()) else NULL,future_n=future,enroll_mode=em,enroll_rate=if(input$ci_process=="gamma"||future==0)0 else v$enroll_rate/f,
     enroll_cuts=if(em=="piecewise")parse_unit_numbers(v$enroll_cuts)*f else numeric(),enroll_rates=if(em=="piecewise")parse_unit_numbers(v$enroll_rates)/f else numeric(),
     dropout_rate=if(input$ci_process=="gamma")0 else dropout_input_rate(v)/f,multiplier=v$multiplier)
   })
   prediction<-(input$ci_purpose %||% "survival") %in% c("rejection","history");sequential<-prediction&&(identical(input$ci_pr_design,"sequential")||identical(input$ci_purpose,"history"));cutmode<-if(sequential)"target" else input$ci_cut_mode
   fixedmode<-cutmode=="fixed";dc<-if(fixedmode)parse_unit_numbers(input$ci_cuts)*f else numeric()
   cfg<-list(cut=z$cut,origin=z$origin,paramcd=z$paramcd,source=z$source,display_unit=unit(),engine_unit="days",groups=gs,model_mode=mode,uncertainty=unc,
    cut_mode=cutmode,cuts=dc,target=if(fixedmode)1 else input$ci_target,max_day=if(stopped_history)z$cut+1 else if(fixedmode)max(dc) else z$cut+input$ci_horizon*f,
    fixed_times=if(prediction)1 else parse_unit_numbers(input$ci_fixed)*f,reps=if(stopped_history)1L else input$ci_reps,seed=if(stopped_history)0L else input$ci_seed,q_values=if(stopped_history)1 else if(isTRUE(input$ci_scenarios))parse_unit_numbers(input$ci_q_values) else 1,
    process_uncertainty=input$ci_process %||% "fixed",recruit_start=(input$ci_recruit_start %||% 0)*f,recruit_end=(input$ci_recruit_end %||% 12)*f,
    prior_shape=input$ci_prior_shape %||% .5,prior_rate=(input$ci_prior_rate %||% (50/f))*f,
    enroll_prior_shape=input$ci_enroll_prior_shape %||% 1,enroll_prior_rate=(input$ci_enroll_prior_rate %||% (1/f))*f,drop_prior_shape=input$ci_drop_prior_shape %||% .5,drop_prior_rate=(input$ci_drop_prior_rate %||% (50/f))*f,
    log_eta_mean=(input$ci_log_eta_mean %||% log(450/f))+log(f),log_eta_sd=input$ci_log_eta_sd %||% 1,log_shape_mean=input$ci_log_shape_mean %||% 0,log_shape_sd=input$ci_log_shape_sd %||% .75,
    mcmc_chains=input$ci_mcmc_chains %||% 4,mcmc_warmup=input$ci_mcmc_warmup %||% 1000,mcmc_draws=input$ci_mcmc_draws %||% 3000)
   cfg<-conditional_prediction_input(input,cfg,labels());if(z$source=="history")cfg$prediction_history<-z$snapshots
   list(data=z$data,config=cfg,prediction=prediction)
 }
 observeEvent(input$ci_batch_freeze,{tryCatch({if(is.null(batch_bridge))stop("批量入口未接入。");p<-freeze_input();batch_bridge$stage(p$data,p$config,"existing_patients")},error=function(e){err(conditionMessage(e));showNotification(conditionMessage(e),type="error",duration=15)})},ignoreInit=TRUE)
 observeEvent(input$ci_run,{
  err(NULL);removeNotification("ci_error")
  tryCatch({
   p<-freeze_input();z<-list(data=p$data);cfg<-p$config;prediction<-p$prediction
   r<-withProgress(message="IA→Final 条件模拟",value=0,if(prediction)run_conditional_prediction(z$data,cfg,function(v,d)setProgress(value=v,detail=d)) else run_conditional_simulation(z$data,cfg,function(v,d)setProgress(value=v,detail=d)))
   index<-if(prediction)r$decisions else r$summary;if(prediction)index$CUTID<-1L
   res(r);updateSelectInput(session,"ci_trial",choices=sort(unique(index$SIMID)),selected=min(index$SIMID));updateSelectInput(session,"ci_view_cut",choices=sort(unique(index$CUTID)),selected=1);updateSelectInput(session,"ci_view_q",choices=cfg$q_values,selected=cfg$q_values[1]);nav_select("ci_tabs","results",session=session)
  },error=function(e){err(conditionMessage(e));showNotification(conditionMessage(e),type="error",duration=15,id="ci_error")})
 })
 selected<-reactive({r<-res();req(r,input$ci_trial,input$ci_view_cut,input$ci_view_q);list(r=r,b=as.integer(input$ci_trial),k=as.integer(input$ci_view_cut),q=as.numeric(input$ci_view_q))})
 subsel<-function(d,s)d[d$SIMID==s$b&d$CUTID==s$k&d$scenario_q==s$q,,drop=FALSE]
 output$ci_result_cut_mode<-renderText({r<-res();if(is.null(r))"" else r$config$cut_mode});outputOptions(output,"ci_result_cut_mode",suspendWhenHidden=FALSE)
 output$ci_status<-renderUI({r<-res();tagList(if(!is.null(err()))div(class="notice error",paste("本次运行失败：",err(),"。结果仍为最近成功运行。")),if(is.null(r))p("选择IA数据与续推参数后运行。") else div(class="context-strip",div(span("来源"),strong(switch(r$config$source,demo="合成示例",design="设计模拟观察记录",adtte="ADTTE",history="历史IA快照"))),div(span("请求 / 成功"),strong(paste(r$config$reps,"/",if(conditional_is_prediction(r$config))sum(r$decisions$generated)/length(r$config$q_values) else length(unique(r$summary$SIMID))))),div(span("结果单位"),strong(time_label(r$config$display_unit))),div(span("q情景"),strong(paste(r$config$q_values,collapse=", ")))))})
 display_scopes<-function(d,r){if(length(r$config$groups)==1)d<-d[d$scope=="overall",,drop=FALSE];d}
 output$ci_baseline_summary<-renderDT({r<-res();req(r,!conditional_is_prediction(r$config));d<-display_scopes(r$baseline$summary,r);d<-d[,setdiff(names(d),c("SIMID","CUTID")),drop=FALSE];names(d)[names(d)=="median_day"]<-"IA_median_day";datatable(view(d,r$config$display_unit))})
 output$ci_overview<-renderDT({r<-res();req(r,!conditional_is_prediction(r$config));d<-display_scopes(conditional_overview(r),r);d<-d[,setdiff(names(d),c("requested","successful","joint_increase_probability","failure_bound_lower","failure_bound_upper","conditional_increase_probability","conditional_increase_mcse")),drop=FALSE];datatable(view(d,r$config$display_unit))})
 output$ci_fixed_overview<-renderDT({r<-res();req(r,!conditional_is_prediction(r$config));datatable(view(display_scopes(conditional_fixed_overview(r),r),r$config$display_unit))})
 output$ci_probability<-renderDT({r<-res();req(r,!conditional_is_prediction(r$config));d<-display_scopes(conditional_overview(r),r);d<-d[,c("scenario_q","CUTID","scope","group","joint_increase_probability","conditional_increase_probability","conditional_increase_mcse","failure_bound_lower","failure_bound_upper"),drop=FALSE];datatable(d)})
 output$ci_target_summary<-renderDT({r<-res();req(r,!conditional_is_prediction(r$config));d<-r$cuts;parts<-split(d,d$scenario_q);datatable(do.call(rbind,lapply(parts,function(x)data.frame(q=x$scenario_q[1],成功次数=nrow(x),窗口内达标概率=mean(x$target_reached),未达标次数=sum(!x$target_reached),达标概率MCSE=sqrt(mean(x$target_reached)*(1-mean(x$target_reached))/nrow(x))))))})
 output$ci_summary<-renderDT({s<-selected();req(!conditional_is_prediction(s$r$config));datatable(view(subsel(s$r$summary,s),s$r$config$display_unit))})
 output$ci_observed<-renderDT({s<-selected();datatable(view(subsel(s$r$observed,s),s$r$config$display_unit))})
 output$ci_km<-renderPlotly({s<-selected();req(!conditional_is_prediction(s$r$config));d<-subsel(s$r$curves,s);a<-s$r$baseline$curves;a$phase<-"IA";d$phase<-"Final";cols<-intersect(names(a),names(d));x<-rbind(a[,cols,drop=FALSE],d[,cols,drop=FALSE]);x<-x[x$scope=="group",,drop=FALSE];x$time<-x$time_day/time_factor(s$r$config$display_unit)
  p<-ggplot(x,aes(time,survival,colour=group,linetype=phase))+geom_step(linewidth=.8)+coord_cartesian(ylim=c(0,1))+labs(x=paste0("个体随访时间（",time_label(s$r$config$display_unit),"）"),y="KM 生存率",colour="组别",linetype="分析截点")+theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),legend.position="bottom")+scale_colour_manual(values=c("#126b72","#173d50","#b68245","#79608c","#477c66","#9b5661"));ggplotly(p)
 })
 output$ci_model_table<-renderDT({r<-res();req(r);d<-do.call(rbind,lapply(seq_along(r$models),function(j)cbind(group=r$config$groups[[j]]$name,model_equivalents(r$models[[j]],r$config$display_unit))));datatable(if(is.null(d))data.frame(记录="当前IA已决策，未拟合未来模型。") else d)})
 output$ci_diagnostics<-renderDT({r<-res();req(r);d<-lapply(seq_along(r$models),function(j){m<-r$models[[j]];if(!is.null(m$posterior))cbind(group=r$config$groups[[j]]$name,m$posterior$diagnostics) else data.frame(group=r$config$groups[[j]]$name,method=m$id,warning=paste(m$warning,collapse="；"))});# MCMC is a uniform model mode, so diagnostic shapes agree.
  datatable(if(length(d))do.call(rbind,d) else data.frame(记录="当前IA已决策，未拟合未来模型。"))})
 output$ci_process_table<-renderDT({r<-res();req(r);rows<-list()
  for(j in seq_along(r$models)) {
   name<-r$config$groups[[j]]$name;m<-r$models[[j]];pp<-r$process_posteriors[[j]]
   if(r$config$uncertainty=="gamma")for(k in seq_along(m$events))rows[[length(rows)+1]]<-data.frame(组别=name,过程=paste("事件风险段",k),shape=r$config$prior_shape+m$events[k],rate=r$config$prior_rate+m$exposure[k],rate单位="受试者·日")
   if(!is.null(pp))for(k in names(pp))rows[[length(rows)+1]]<-data.frame(组别=name,过程=if(k=="enroll")"入组" else "永久退出",shape=pp[[k]]["shape"],rate=pp[[k]]["rate"],rate单位=if(k=="enroll")"日" else "受试者·日")
  }
  datatable(if(length(rows))do.call(rbind,rows) else data.frame(记录="本次未使用事件或过程Gamma后验。"))
 })
 output$ci_failures<-renderDT({r<-res();req(r);datatable(if(nrow(r$failures))r$failures else data.frame(记录="所有请求轮次均成功。未达事件目标的轮次保留在结果中。"))})
 output$ci_export_note<-renderUI({s<-selected();o<-subsel(s$r$observed,s);issues<-simulation_export_issues(simulation_adtte(o,s$r$config));p(class="field-note",paste("分析使用连续经过时间。ADTTE日期向下取整，AVAL含首日。",nrow(issues),"条同日起止记录被保留；当前文件导入不接受这些记录，可使用连续时间数据。"))})
 output$ci_template<-downloadHandler(filename="adtte_template.csv",content=function(file)write.csv(adtte_template(),file,row.names=FALSE))
 download<-function(id,name,fn)output[[id]]<-downloadHandler(filename=name,content=fn)
 download("ci_observed_download","conditional_final_observed.csv",function(file){s<-selected();write.csv(unit_export(subsel(s$r$observed,s),s$r$config$display_unit,c("DCO_DAY","entry_day","time_day","obs_day")),file,row.names=FALSE)})
 download("ci_adtte_download","conditional_final_adtte.csv",function(file){s<-selected();write.csv(simulation_adtte(subsel(s$r$observed,s),s$r$config),file,row.names=FALSE)})
 for(pair in list(c("ci_all_summary_download","summary"),c("ci_all_fixed_download","fixed"),c("ci_cuts_download","cuts")))local({id<-pair[1];key<-pair[2];download(id,paste0("conditional_",key,".csv"),function(file){r<-res();req(r);d<-r[[key]];if(key=="cuts"&&r$config$cut_mode=="fixed")d<-d[,setdiff(names(d),c("target_reached","target_day")),drop=FALSE];write.csv(unit_export(d,r$config$display_unit,names(d)[grepl("_day$|^DCO_DAY$",names(d))]),file,row.names=FALSE)})})
 bundle<-function(r){cfg<-r$config;if(cfg$cut_mode=="fixed")cfg$target<-NULL;if(conditional_is_prediction(cfg))cfg$fixed_times<-NULL;list(dependencies=r$dependencies,version=r$version,created_at=r$created_at,config=cfg,data=r$baseline$observed[,c("USUBJID","entry_day","time_day","obs_day","status","group")])}
 download("ci_config_download","conditional_simulation_config.json",function(file){r<-res();req(r);jsonlite::write_json(bundle(r),file,pretty=TRUE,auto_unbox=TRUE,digits=NA)})
 download("ci_script_download","reproduce_conditional_simulation.R",function(file){r<-res();req(r);if(conditional_is_prediction(r$config)){writeLines(conditional_prediction_script(r),file,useBytes=TRUE);return(invisible(NULL))};js<-jsonlite::toJSON(bundle(r),auto_unbox=TRUE,digits=NA,null="null");writeLines(c('# 在相同版本event_pred源码根目录运行；脚本内包含IA个体记录。',
  'source("R/units.R");source("R/models.R");source("R/inputs.R");source("R/forecast.R");source("R/bayes.R");source("R/simulation.R");source("R/conditional_simulation.R")',
  paste0('b <- jsonlite::fromJSON(',encodeString(js,quote='"'),', simplifyVector=TRUE)'),
  paste0('b$config$groups <- jsonlite::fromJSON(',encodeString(jsonlite::toJSON(r$config$groups,auto_unbox=TRUE,digits=NA,null="null"),quote='"'),', simplifyVector=FALSE)'),
  'b$config$groups<-lapply(b$config$groups,function(g){for(id in c("cuts","enroll_cuts","enroll_rates"))g[[id]]<-unlist(g[[id]]);if(!is.null(g$model)){g$model$params<-unlist(g$model$params);g$model$cuts<-unlist(g$model$cuts)};g})',
  'if(b$config$cut_mode=="fixed")b$config$target<-1',
  'd<-b$data;names(d)[1:3]<-c("id","entry","time");r<-run_conditional_simulation(d,b$config)',
  'write.csv(r$summary,"conditional_summary.csv",row.names=FALSE);write.csv(r$fixed,"conditional_fixed.csv",row.names=FALSE)'),file,useBytes=TRUE)})
 download("ci_report_download","conditional_simulation_report.md",function(file){r<-res();req(r);if(conditional_is_prediction(r$config)){writeLines(c("# IA个体条件拒绝概率",paste("版本：",r$version),"原历史保持不变；模型或后验抽样后只生成未来患者，按原检验路径决策。MCSE/Wilson描述模拟误差；并非总体功效或Ⅰ类错误验证，未执行事件数重估。","## 配置","```json",jsonlite::toJSON(bundle(r)$config,auto_unbox=TRUE,pretty=TRUE,digits=NA),"```",if(conditional_has_history(r$config))c("## 已观察历史分析（时间单位：日）","```",capture.output(print(r$history_path)),"```"),"## 拒绝概率","```",capture.output(print(conditional_prediction_overview(r))),"```"),file,useBytes=TRUE);return(invisible(NULL))};writeLines(c('# IA→Final 条件模拟记录','',paste('版本：',r$version),paste('时间：',r$created_at),paste('单位：',time_label(r$config$display_unit)),paste('IA研究时间：',r$config$cut/time_factor(r$config$display_unit)),paste('请求/成功：',r$config$reps,'/',if(conditional_is_prediction(r$config))sum(r$decisions$generated)/length(r$config$q_values) else length(unique(r$summary$SIMID))),
  '','历史事件和永久退出保持原记录；仍随访者使用条件分布，未来入组按各组过程模拟。Bootstrap只重拟合模型，使用原风险集续推。','',
  'Final中位数分位数只在可估计轮次中计算；IA为NR时中位数变化与增长概率未定义。未达目标轮次保留在窗口末。单次KM置信区间与模拟分位区间分别报告。','',
  '## 配置','','```json',jsonlite::toJSON(bundle(r)$config,auto_unbox=TRUE,pretty=TRUE,digits=NA),'```','',
  '## 模拟失败记录','','```',paste(capture.output(print(r$failures)),collapse='\n'),'```'),file,useBytes=TRUE)})
 register_conditional_prediction_server(input,output,session,res,selected,subsel,labels,source_data)
 res
}
