register_adaptive_batch_help <- function() {
  h<-get("parameter_help",envir=parent.frame())
  for(pre in c("abm_","abdc_","abdt_"))for(k in names(simulation_defaults()))if(paste0("p_",k) %in% names(h))h[paste0(pre,k)]<-h[[paste0("p_",k)]]
  h<-c(h,c(
    ab_run_source="填写参数或导入本平台无损配置JSON二选一。该入口不需要患者文件；JSON模式只读取文件参数。",
    ab_run_name="本机研究名称1–100字符，只用于索引和报告。",
    ab_config_file="event_pred.batch_config.v1，research_family为independent_cohort_adaptation，最多5MB；不上传RDS或执行脚本。",
    ab_unit="默认月，可选日/周。更改单位会换算时长、风险率和入组率；事件数、HR、CP和人数不换算。CSV情景列均无时间量。",
    ab_origin="研究起点日期，用于第二阶段截点的日历日期；内部阶段时间按连续日数计算。",
    ab_endpoint="PFS或OS用于样例ADTTE的PARAMCD。本入口每轮仅一个生存终点，不同时模拟PFS/OS。",
    ab_allocation="Treatment简单随机分配概率p，0<p<1，通常0.5；两个队列相同。权重使用原计划D1/Dplan，不按观察方差重算。",
    ab_alpha="预定单侧alpha，0.0001–0.2，例如0.025；无IA提前拒绝，Final使用固定逆正态组合。",
    ab_enrollment="每阶段开始一次入组或总体恒定Poisson入组。队列2在实际IA时开始；阶段1观察于IA冻结，后续事件不进入阶段2。",
    ab_rate1="队列1总体入组率，单位人/当前时间单位，正数；例30人/月。该队列达到IA即停止纳入。",
    ab_rate2="队列2总体入组率，正数；例30人/月。各情景入组倍数同时作用于两阶段的率。",
    ab_window1="研究起点至IA最大窗口，换算后(0,3650]日，例如36月。未达到D1关闭不拒绝，不生成阶段2。",
    ab_window2="实际IA至新队列Final的最大窗口，换算后(0,3650]日，例如60月。未达到选定D2关闭不拒绝。",
    ab_scenario_source="事件计划与列表组合，或严格情景CSV二选一。参数完全重复的情景拒绝；最多60情景。",
    ab_plans="每行：名称 | N1,N2,D1,Dplan,Dmax,HRassumed,CPtarget,CPmin。N1/N2为10–5000整数；D1<Dplan≤Dmax，D1≤N1、Dmax−D1≤N2。HRassumed在(0,1)，0.5<CPtarget<1，0<CPmin<CPtarget。Dmax−Dplan≤2000。",
    ab_true_hrs="真实恒定HR列表，0.01–5，例如1,0.67。明确纳入1才能评价H0下拒绝率；不会自动添加H0情景。真实HR不改变重估采用的HRassumed。",
    ab_time_scales="Control生存时间倍数列表0.01–100，默认1。仅改变事件模型，不改变入组、脱落和研究窗口；Treatment保持该情景的PH真实HR。",
    ab_enroll_scales="两阶段共同入组率倍数0.01–100，默认1。阶段开始一次入组时必须为1。",
    ab_dropout_scales="两组共同独立脱落风险率倍数0–100，默认1；0取消脱落，基准率0时倍率不会产生非零率。",
    ab_file="情景参数CSV严格13列，列名见模板，不含SCENARIO；最多60情景，不是ADTTE。",
    ab_primary="运行前指定主成功规则：原计划、上下限或promising-zone。结果主汇总只对应此规则，不能看结果后按任一规则成功判定。",
    ab_extra="预定比较规则，与主规则共用每轮两个队列患者真值，按各自D2截取；不会产生多规则择优检验。",
    ab_reps="每情景20–10000轮，最多10万轮/3000万患者×轮次/50万规则×轮次。可先保存小B，再在研究库追加子研究。",
    ab_seed="0–2147483647整数，按SCENARIO/SIMID固定派生种子。改变B保留前缀；跨研究同键/种子可能相关。",
    ab_view_scenario="查看已提交结果的情景，不运行或改变原配置。",
    ab_reference_z="已保存事件计划的假设IA获益Z列表，1–100个不重复有限数，范围−12至12，例如−1,0,1,2,3。不读取患者真值，不改变已提交设计。",
    ab_reference_run="按所选已保存情景计算各预定规则的D2、条件临界值和近似CP；无需完整模拟完成。输入改变后需重新点击，CSV保存该次冻结参考。",
    ab_sample_run="按已保存配置和种子恢复选定轮次，只用于样例。选择研究/情景/轮次改变后需重新点击；不会运行完整批量研究。",
    ab_view_trial="选择该情景成功生成的轮次，用冻结配置和原种子恢复各规则观察样例和潜在真值；实际重放仍待验证。"))
  h["ab_file"]<-paste0("情景参数CSV严格列名：",paste(abr_fields,collapse=","),"。共13列，不含SCENARIO；每行一个完整设计情景，最多60行，不是ADTTE。")
  assign("parameter_help",h,envir=parent.frame())
}
adaptive_batch_ui <- function() {
  nav_panel("独立队列重估研究",value="adaptive_batch",
    div(class="page-title",h2("独立队列事件数重估批量研究"),actionButton("ab_run","运行重估研究",class="btn-primary"),actionButton("ab_cancel","取消后台研究")),
    p(class="field-note","两个预先划分的独立队列，固定原计划组合权重，单侧获益。当前为开发稿，待复核。"),
    section("参数入口",radioButtons("ab_run_source","参数来源",c("填写页面参数"="ui","导入无损配置JSON"="config_json"),inline=TRUE),textInput("ab_run_name","研究名称","独立队列重估设计"),conditionalPanel("input.ab_run_source === 'config_json'",fileInput("ab_config_file","重估研究配置JSON",accept=".json"),uiOutput("ab_import_note"))),
    navset_card_tab(id="ab_tabs",
      nav_panel("患者与过程",value="ab_baseline",
        section("共同研究参数",fields(selectInput("ab_unit","时间单位",c("日"="days","周"="weeks","月"="months"),"months"),dateInput("ab_origin","研究起点","2025-01-01")),fields(selectInput("ab_endpoint","单一终点",c("PFS","OS")),numericInput("ab_allocation","Treatment分配概率 p",.5,min=.01,max=.99)),numericInput("ab_alpha","单侧alpha",.025,min=.0001,max=.2)),
        section("Control事件模型",event_parameter_fields("abm_","months",simulation_defaults(),simulation=TRUE)),
        section("队列入组与窗口",selectInput("ab_enrollment","各阶段入组",c("阶段开始一次入组"="batch","恒定Poisson入组"="constant")),conditionalPanel("input.ab_enrollment === 'constant'",fields(numericInput("ab_rate1","队列1总体入组率（人/月）",30,min=.001),numericInput("ab_rate2","队列2总体入组率（人/月）",30,min=.001))),fields(numericInput("ab_window1","阶段1最大窗口（月）",36,min=.001),numericInput("ab_window2","IA后阶段2最大窗口（月）",60,min=.001))),
        section("独立脱落",navset_card_tab(nav_panel("Control",dropout_parameter_fields("abdc_","months",simulation_defaults())),nav_panel("Treatment",dropout_parameter_fields("abdt_","months",simulation_defaults()))))),
      nav_panel("事件计划与情景",value="ab_scenarios",
        section("情景来源",selectInput("ab_scenario_source","情景参数",c("事件计划与列表组合"="grid","情景CSV"="csv")),conditionalPanel("input.ab_scenario_source === 'csv'",fileInput("ab_file","情景CSV",accept=".csv")),
          conditionalPanel("input.ab_scenario_source === 'grid'",textAreaInput("ab_plans","名称 | N1,N2,D1,Dplan,Dmax,HRassumed,CPtarget,CPmin","plan_A | 250,400,80,200,300,0.67,0.8,0.3\nplan_B | 250,400,100,240,340,0.67,0.8,0.3",rows=4,width="100%"),fields(textInput("ab_true_hrs","真实HR列表","1,0.67"),textInput("ab_time_scales","生存时间倍数列表","1")),conditionalPanel("input.ab_enrollment === 'constant'",textInput("ab_enroll_scales","入组率倍数列表","1")),textInput("ab_dropout_scales","脱落风险倍数列表","1")),downloadButton("ab_template","情景CSV模板"),p(class="field-note","真实HR含1才生成H0评价。每个事件计划内部固定原计划D1/Dplan权重；情景之间独立生成。"))),
      nav_panel("规则与运行",value="ab_rules",section("预定主规则与同轮比较",selectInput("ab_primary","主规则",setNames(names(abr_names),unname(abr_names)),"bounded"),checkboxGroupInput("ab_extra","附加比较规则",setNames(names(abr_names),unname(abr_names)),selected=c("original","promising"))),section("重复研究",fields(numericInput("ab_reps","每情景重复数 B",100,min=20,max=10000,step=20),numericInput("ab_seed","主种子",20261005,min=0,max=2147483647,step=1)))),
      nav_panel("结果",value="ab_results",uiOutput("ab_status"),section("主规则与全部请求分母",DTOutput("ab_overview",fill=FALSE)),section("各规则拒绝、达标与重估概率",DTOutput("ab_method_overview",fill=FALSE)),section("同轮规则配对差",DTOutput("ab_method_pairs",fill=FALSE)),section("患者、事件目标与关闭时间",DTOutput("ab_resources",fill=FALSE)),section("路径与重估原因",DTOutput("ab_stops",fill=FALSE)),fields(selectInput("ab_view_scenario","查看情景",character()),selectInput("ab_view_trial","查看生成轮次",character())),section("给定IA得分的重估规则表",textInput("ab_reference_z","IA获益Z列表","-1,0,1,2,3"),actionButton("ab_reference_run","计算规则参考表"),uiOutput("ab_reference_note"),DTOutput("ab_reference",fill=FALSE),downloadButton("ab_reference_download","规则参考CSV"),downloadButton("ab_reference_config","规则参考配置JSON")),section("所选情景规则明细",DTOutput("ab_method_rows",fill=FALSE)),actionButton("ab_sample_run","恢复所选轮次样例"),p(class="field-note","选定情景与轮次后点击恢复；样例使用该轮冻结配置和种子。"),section("所选轮次阶段路径",DTOutput("ab_sample_paths",fill=FALSE)),section("所选轮次观察记录",DTOutput("ab_sample_observed",fill=FALSE))),
      nav_panel("导出与研究库",value="ab_exports",actionButton("ab_library","打开研究库"),p(class="field-note","支持保存、续跑与追加子研究。CSV时间字段以日导出；各规则观察数据须按METHOD拆分。"),
        div(class="export-bar",
          downloadButton("ab_scenarios_download","计划CSV"),downloadButton("ab_rows_download","主轮次CSV"),downloadButton("ab_method_rows_download","规则轮次CSV"),downloadButton("ab_looks_download","阶段路径CSV"),downloadButton("ab_overview_download","主汇总CSV"),downloadButton("ab_method_overview_download","规则汇总CSV"),downloadButton("ab_method_pairs_download","配对差CSV"),downloadButton("ab_resources_download","资源CSV"),downloadButton("ab_stops_download","路径汇总CSV"),downloadButton("ab_config_download","配置JSON"),downloadButton("ab_result_download","结果RDS"),downloadButton("ab_report_download","报告Markdown"),downloadButton("ab_script_download","复现R脚本"),downloadButton("ab_observed_download","所选观察CSV"),downloadButton("ab_truth_download","所选潜在真值CSV"),downloadButton("ab_adtte_download","所选ADTTE")))))
}
