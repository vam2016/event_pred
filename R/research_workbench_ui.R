research_result_panels <- function(prefix,title,extra_tables=character()) {
  id<-function(x)paste0(prefix,x)
  tables<-c(overview="主结果与完整请求分母",method_overview="方法/指标汇总",method_pairs="同轮方法配对差",resources="关闭时间与资源",stops="路径/动作分布",extra_tables)
  tagList(uiOutput(id("status")),lapply(names(tables),function(k)section(tables[[k]],DTOutput(id(k),fill=FALSE))),
    section("样例",fields(selectInput(id("view_scenario"),"查看保存情景",character()),selectInput(id("view_trial"),"查看成功生成轮次",character())),actionButton(id("sample_run"),"恢复所选轮次样例"),p(class="field-note","选择变化后须再次恢复；样例和导出使用保存配置，不读取未提交页面参数。"),DTOutput(id("sample_rows"),fill=FALSE),DTOutput(id("sample_looks"),fill=FALSE),DTOutput(id("sample_observed"),fill=FALSE)))
}
research_export_panel <- function(prefix,title,extra_tables=character()) {
  id<-function(x)paste0(prefix,x);tables<-c(scenarios="情景CSV",rows="主轮次CSV",method_rows="方法轮次CSV",looks="分析路径CSV",draw_rows="抽样参数CSV",overview="主汇总CSV",method_overview="方法汇总CSV",method_pairs="方法配对CSV",resources="资源CSV",stops="路径汇总CSV",extra_tables)
  nav_panel("导出与研究库",value=id("exports"),actionButton(id("library"),"打开研究库"),p(class="field-note","原始时间字段以日导出；支持同引擎续跑和追加子研究，生成失败不重试。"),div(class="export-bar",lapply(names(tables),function(k)downloadButton(id(paste0(k,"_download")),tables[[k]])),downloadButton(id("config_download"),"无损配置JSON"),downloadButton(id("result_download"),"结果RDS"),downloadButton(id("report_download"),"报告Markdown"),downloadButton(id("script_download"),"复现R脚本"),downloadButton(id("observed_download"),"所选观察CSV"),downloadButton(id("truth_download"),"所选潜在真值CSV"),downloadButton(id("adtte_download"),"所选ADTTE")))
}
register_research_workbench_help <- function() {
  h<-get("parameter_help",envir=parent.frame())
  for(pre in c("cb_","hg_"))h<-c(h,setNames(c("页面参数或本平台无损JSON二选一，JSON模式只用冻结文件内容。","本机研究名称1–100字符，用于索引和报告。","新格式event_pred.batch_config.v1，最多5MB；只导入配置，不执行脚本。","每情景20–10000轮。新结果仍待复核，可在研究库追加B。","0–2147483647整数，每轮独立派生；相同研究键可能相关。","选择已保存情景，不改变原配置。","选择已生成轮次，点击恢复后查看观察与真值分离的样例。","使用该轮原配置和种子恢复，实际重放尚未验证。"),paste0(pre,c("run_source","run_name","config_file","reps","seed","view_scenario","view_trial","sample_run"))))
  for(pre in c("hgm_","hgdc_","hgdt_"))for(k in names(simulation_defaults()))if(paste0("p_",k) %in% names(h))h[paste0(pre,k)]<-h[[paste0("p_",k)]]
  new<-c(
    ci_batch_freeze="将当前IA未来拒绝/历史预测参数及规范IA记录冻结到批量入口，不先运行单次预测；保留原检验设计。",
    pg_batch_freeze="将当前IA、已预定重估规则和新队列预测参数冻结到批量入口；不先模拟新队列，批量情景不改变选定D2。",
    cb_unit="批量结果显示单位，默认沿用冻结IA入口，可选日/周/月。模型、窗口及IA原始时间已统一为日；不会重新解释上传AVAL。",
    cb_scenario_csv="附加情景CSV七列：label,control_event_q,treatment_event_q,control_enroll_q,treatment_enroll_q,control_dropout_q,treatment_dropout_q。自动加入输入基准，最多59附加。事件/入组0.01–10，脱落0–10。新独立队列总体入组要求两组倍率相等，一次入组须1。",
    cb_primary="既有患者原设计仅log-rank；新队列选定重估D2或原计划D2为主，另一规则同轮参照。不能看结果后改主规则。",
    cb_open_existing="到IA→Final填写实际IA/历史快照与未来拒绝参数，再点击冻结至批量。",
    cb_open_new="到事件重估入口选择患者模式及重估后的新队列条件预测，填写后冻结至批量。",
    hg_unit="默认月，可选日/周/月，时长和风险/入组率自动换算。HR、人数、协变量系数和脆弱性方差不换算。",
    hg_origin="研究起点日期，生成入组/截点日历日期用；内部使用连续经过日数。",
    hg_endpoint="单一生存终点PFS或OS，用于ADTTE标签。本入口不同时生成两终点。",
    hg_allocation="Treatment个体简单随机分配概率0.01–0.99，默认0.5；不是集群随机化或固定每层分配人数。",
    hg_strata="CSV四列stratum,weight,hazard_q,hr，1–12层；权重正且自动按总和分配；层基准风险倍数0.01–100，层HR0.01–5。层HR与情景HR倍数相乘。",
    hg_binary_probability="二元协变量概率0–1，默认0.4，独立于组别、层、中心与正态协变量。",
    hg_binary_beta="二元协变量log风险系数−5至5，默认log(1.5)；生成使用X−其概率中心化，Cox估计时保留原X。",
    hg_normal_mean="正态协变量均值−100至100，默认0；生成风险使用X−均值，不按SD再标准化。",
    hg_normal_sd="正态协变量SD0.001–100，默认1；系数按原数值单位解释。",
    hg_normal_beta="正态协变量每一原数值单位的log风险系数−5至5，默认log(1.2)。",
    hg_n_centers="计划中心数1–100整数，患者按中心权重独立分配；每轮重抽中心log-normal脆弱性。",
    hg_center_weights="与中心数一致的正权重逗号列表，不要求和为1；例如5中心填1,1,1,1,1。",
    hg_center_analysis="调整Cox的中心处理：无中心项、factor中心固定效应或cluster稳健方差。cluster只改方差，不等于调整中心风险或随机效应拟合。",
    hg_enroll_mode="研究起点一次入组、总体恒定或分段Poisson入组；各组不单独指定入组率。",
    hg_enroll_rate="总体入组率，人/当前单位，须为正；例20人/月。",
    hg_enroll_cuts="总体入组过程研究时间切点，正递增；例3,6月。",
    hg_enroll_rates="数量=切点数+1，各段非负总体率；末段须保证所需入组时间可定义。",
    hg_cut_rule="预定共同一次Final：固定DCO或累计事件目标。各方法使用同一观察截点，不择优选择分析日期。",
    hg_max="固定DCO或事件目标最大研究窗口，正、最长3650日；从研究起点计时。",
    hg_target="累计已观察事件目标1–100000整数；超过N时按最大窗口处理。",
    hg_miss="事件目标未达时关闭不拒绝，或预定窗口末一次Final，运行前选择。",
    hg_scenario_source="N/HR倍数/中心log-SD/个体Gamma方差列表组合，或严格五列CSV二选一，最多60情景。",
    hg_n_values="总N列表10–5000整数，例如200,400。各情景独立生成患者。",
    hg_hr_values="乘各层HR的共同效应倍数列表0.01–5，乘后每层HR仍需0.01–5；各层HR均为1且倍数1才是全局H0。",
    hg_center_sd_values="中心log脆弱性SD列表0–2，例0,0.3。Ucenter=exp(N(−SD²/2,SD²))，均值1；同中心患者共享。",
    hg_subject_variances="个体Gamma脆弱性方差列表0–5，例0,0.5。正方差时shape=rate=1/方差，均值1；0表示固定1。",
    hg_file="CSV严格五列label,n,hr_scale,center_sd,subject_variance。没有患者上传要求；模型/协变量/过程在共同参数中填写。",
    hg_primary="运行前指定普通log-rank、分层log-rank或调整Cox。分层检验合计层得分与方差，不按层p择优。",
    hg_extra="同轮预定附加方法，复用患者和共同Final；不将任意一种方法拒绝作为主成功。",
    hg_alpha="一次Final的预定alpha0.0001–0.2；单侧常用0.025、双侧常用0.05。",
    hg_sided="单侧Treatment获益或双侧差异；正向Z表示获益，双侧反方向也可以拒绝。")
  h[names(new)]<-new;assign("parameter_help",h,envir=parent.frame())
}
conditional_batch_ui <- function() {
  nav_panel("实际IA条件批量",value="conditional_batch",div(class="page-title",h2("实际IA条件预测批量研究"),actionButton("cb_run","运行条件研究",class="btn-primary"),actionButton("cb_cancel","取消后台研究")),p(class="field-note","冻结实际IA；仅生成未来。条件概率不等于从研究起点的功效或Ⅰ类错误。开发稿待复核。"),
    section("参数入口",radioButtons("cb_run_source","参数来源",c("使用冻结IA参数"="ui","导入无损配置JSON"="config_json"),inline=TRUE),textInput("cb_run_name","研究名称","IA条件研究"),conditionalPanel("input.cb_run_source === 'config_json'",fileInput("cb_config_file","条件研究配置JSON",accept=".json"),uiOutput("cb_import_note"))),
    navset_card_tab(id="cb_tabs",
      nav_panel("冻结IA与情景",value="cb_inputs",section("冻结来源",actionButton("cb_open_existing","填写既有患者IA→Final参数"),actionButton("cb_open_new","填写独立新队列条件预测参数"),p(class="field-note","在对应入口填写后点击“冻结至批量研究”，无需先运行单次模拟。配置/结果导出包含冻结IA个体记录。"),uiOutput("cb_draft_note"),DTOutput("cb_draft_summary",fill=FALSE)),
        section("未来情景",textAreaInput("cb_scenario_csv","附加情景CSV","label,control_event_q,treatment_event_q,control_enroll_q,treatment_enroll_q,control_dropout_q,treatment_dropout_q\nTreatment风险降低,1,0.8,1,1,1,1\n脱落增加,1,1,1,1,1.5,1.5",rows=5,width="100%"),downloadButton("cb_template","情景CSV模板")),
        section("批量运行",fields(selectInput("cb_unit","结果显示单位",c("日"="days","周"="weeks","月"="months"),"months"),selectInput("cb_primary","预定主方法",c("原设计log-rank"="logrank"))),fields(numericInput("cb_reps","每情景重复数 B",100,min=20,max=10000,step=20),numericInput("cb_seed","主种子",20261005,min=0,max=2147483647,step=1)))),
      nav_panel("结果",value="cb_results",research_result_panels("cb_","条件研究",c(scenario_pairs="相对输入基准的同轮情景差",ia_overview="冻结IA统计量",model_overview="准备模型与诊断状态"))),research_export_panel("cb_","条件研究",c(scenario_pairs="情景配对CSV",ia_overview="冻结IA统计CSV",model_overview="模型准备CSV")))))
}
heterogeneity_ui <- function() {
  nav_panel("异质患者研究",value="heterogeneity",div(class="page-title",h2("协变量、分层、中心与脆弱性模拟"),actionButton("hg_run","运行异质患者研究",class="btn-primary"),actionButton("hg_cancel","取消后台研究")),p(class="field-note","参数生成两组患者，预定共同一次Final和主/比较方法。潜在脆弱性单独导出；开发稿待复核。"),
    section("参数入口",radioButtons("hg_run_source","参数来源",c("填写页面参数"="ui","导入无损配置JSON"="config_json"),inline=TRUE),textInput("hg_run_name","研究名称","异质患者设计"),conditionalPanel("input.hg_run_source === 'config_json'",fileInput("hg_config_file","异质患者研究配置JSON",accept=".json"),uiOutput("hg_import_note"))),
    navset_card_tab(id="hg_tabs",
      nav_panel("患者与模型",value="hg_baseline",section("共同研究参数",fields(selectInput("hg_unit","时间单位",c("日"="days","周"="weeks","月"="months"),"months"),dateInput("hg_origin","研究起点","2025-01-01")),fields(selectInput("hg_endpoint","终点",c("PFS","OS")),numericInput("hg_allocation","Treatment分配概率",.5,min=.01,max=.99))),section("基准事件模型",event_parameter_fields("hgm_","months",simulation_defaults(),simulation=TRUE)),
        section("分层",textAreaInput("hg_strata","stratum,weight,hazard_q,hr","stratum,weight,hazard_q,hr\nS1,0.6,1,1\nS2,0.4,1.8,1",rows=4,width="100%")),
        section("测量协变量",fields(numericInput("hg_binary_probability","二元协变量概率",.4,min=0,max=1),numericInput("hg_binary_beta","二元协变量log风险系数",log(1.5),min=-5,max=5)),fields(numericInput("hg_normal_mean","正态协变量均值",0,min=-100,max=100),numericInput("hg_normal_sd","正态协变量SD",1,min=.001,max=100)),numericInput("hg_normal_beta","正态协变量log风险系数",log(1.2),min=-5,max=5)),
        section("中心",fields(numericInput("hg_n_centers","计划中心数",5,min=1,max=100,step=1),textInput("hg_center_weights","各中心权重","1,1,1,1,1")),selectInput("hg_center_analysis","调整Cox的中心处理",c("无中心项"="none","中心固定效应"="fixed","cluster稳健方差"="cluster"),"fixed"))),
      nav_panel("过程与情景",value="hg_scenarios",section("总体入组",selectInput("hg_enroll_mode","总体入组",c("研究起点一次入组"="batch","恒定Poisson"="constant","分段Poisson"="piecewise"),"constant"),conditionalPanel("input.hg_enroll_mode === 'constant'",numericInput("hg_enroll_rate","总体入组率（人/月）",20,min=.001)),conditionalPanel("input.hg_enroll_mode === 'piecewise'",fields(textInput("hg_enroll_cuts","入组切点（月）","3,6"),textInput("hg_enroll_rates","各段总体入组率（人/月）","15,25,20")))),section("独立脱落",navset_card_tab(nav_panel("Control",dropout_parameter_fields("hgdc_","months",simulation_defaults())),nav_panel("Treatment",dropout_parameter_fields("hgdt_","months",simulation_defaults())))),
        section("共同一次Final",selectInput("hg_cut_rule","截点规则",c("固定DCO"="fixed","累计事件目标"="target")),numericInput("hg_max","固定DCO / 最大窗口（月）",36,min=.001),conditionalPanel("input.hg_cut_rule === 'target'",numericInput("hg_target","累计事件目标 D*",150,min=1,step=1),selectInput("hg_miss","未达目标处理",c("关闭不拒绝"="no_reject","窗口末一次Final"="analyze")))),
        section("异质性情景",selectInput("hg_scenario_source","情景来源",c("列表组合"="grid","情景CSV"="csv")),conditionalPanel("input.hg_scenario_source === 'csv'",fileInput("hg_file","情景CSV",accept=".csv")),conditionalPanel("input.hg_scenario_source === 'grid'",fields(textInput("hg_n_values","总N列表","300"),textInput("hg_hr_values","层HR共同倍数列表","1,0.67")),fields(textInput("hg_center_sd_values","中心log脆弱性SD列表","0,0.3"),textInput("hg_subject_variances","个体Gamma脆弱性方差列表","0,0.5"))),downloadButton("hg_template","情景CSV模板"))),
      nav_panel("分析与运行",value="hg_analysis",section("预定分析",selectInput("hg_primary","主方法",setNames(names(hg_names),unname(hg_names)),"stratified_logrank"),checkboxGroupInput("hg_extra","附加同轮比较方法",setNames(names(hg_names),unname(hg_names)),selected=c("logrank","adjusted_cox")),fields(numericInput("hg_alpha","一次Final alpha",.025,min=.0001,max=.2),selectInput("hg_sided","检验方向",c("单侧Treatment获益"="benefit","双侧差异"="two")))),section("重复研究",fields(numericInput("hg_reps","每情景重复数 B",100,min=20,max=10000,step=20),numericInput("hg_seed","主种子",20261005,min=0,max=2147483647,step=1)))),
      nav_panel("结果",value="hg_results",research_result_panels("hg_","异质患者研究",c(recovery="可识别条件logHR的恢复与覆盖"))),research_export_panel("hg_","异质患者研究",c(recovery="条件效应恢复CSV")))))
}
