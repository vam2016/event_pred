register_patient_adaptive_help <- function() {
  h<-get("parameter_help",envir=parent.frame())
  new<-c(ad_engine="canonical只生成阶段正态得分；患者模式从两组生存观察记录计算log-rank。患者模式限于预先安排的两个独立队列，阶段1在IA冻结，阶段2只纳入新患者。",
    pa_purpose="实际IA目标计算、重估后新队列条件预测或从起点完整设计评价，按本次需求选择。条件预测固定当前IA；完整设计评价自动包含HR=1。",
    pa_source="读取已知两组的ADTTE，或使用合成示例核对接口。按已确定的分析终点和组别定义选择文件；不会从盲态合并数据恢复组别。",
    pa_control="明确指定Control组，另一组是Treatment。正Z表示Treatment获益。应与方案中组别定义一致。",
    pa_n1="阶段1最多入组人数，10–5000整数且不少于D1。恒定入组时达到IA后即停止该队列入组，尚未入组者不转入阶段2。例250。",
    pa_n2="IA后独立新队列最多入组人数，10–5000整数且不少于Dmax−D1；固定人数上限，重估只改变事件目标。例400。",
    pa_window1="从研究起点至IA的最大窗口，正数且最多3650日。未达到D1关闭且不拒绝，不执行新的检验。例36月。",
    pa_window2="从IA至第二阶段的最大窗口，正数且最多3650日。新队列未达到选定事件目标时关闭且不拒绝。例60月。",
    pa_enrollment="两个队列各自在阶段开始一次入组，或从阶段开始恒定Poisson入组；各阶段独立生成。阶段2时间0对应当前IA。",
    pa_rate1="第一队列总体Poisson入组率，单位人/当前时间单位，须为正。例30人/月。达到IA停止该队列入组。",
    pa_rate2="IA后新队列总体Poisson入组率，须为正。例30人/月；阶段1患者不重复入组。",
    pa_true_hr="Treatment/Control恒定风险比，0.01–5。两队列使用同一真值，自动另模拟HR=1。例0.67。",
    pa_reps="每HR情景从患者生成起点的完整试验数，20–10000整数。先100–500轮检查，再增加B；alpha=0.025且B=10000时名义MCSE约0.00156。",
    pa_seed="0至2147483647整数。每情景每轮单独派生种子，改变B保留已有轮次，保存配置可复现。")
  h[names(new)]<-new
  aliases<-c(pa_unit="time_unit",pa_file="file",pa_origin="origin",pa_cut="cut",pa_endpoint="paramcd",pa_group_column="ci_group_column",pa_aval_unit="aval_unit",pa_offset="offset",pa_dropout_codes="dropout_codes",pa_date_encoding="date_encoding",pa_flag="analysis_flag",pa_flag_value="flag_value")
  h[names(aliases)]<-h[unname(aliases)]
  ids<-intersect(names(simulation_defaults()),sub("^p_","",names(h)[grepl("^p_",names(h))]))
  for(pre in c("pa_","padc_","padt_","pgc_","pgt_","pgdc_","pgdt_"))h[paste0(pre,ids)]<-h[paste0("p_",ids)]
  # The Control model fields use pa_; restore task-specific help if names overlap.
  h[names(new)]<-new;h[names(aliases)]<-h[unname(aliases)]
  aliases2<-c(pg_model_mode="ci_model_mode",pg_uncertainty="ci_uncertainty",pg_fit_method="fit_method",pg_cuts="cuts",pg_tail_rate="tail_rate",pg_prior_shape="prior_shape",pg_prior_rate="prior_rate",pg_log_eta_mean="log_eta_mean",pg_log_eta_sd="log_eta_sd",pg_log_shape_mean="log_shape_mean",pg_log_shape_sd="log_shape_sd",pg_mcmc_chains="mcmc_chains",pg_mcmc_warmup="mcmc_warmup",pg_mcmc_draws="mcmc_draws",pg_enroll_cuts="enroll_cuts",pg_enroll_rates="enroll_rates",pg_n2="pa_n2",pg_window2="pa_window2",pg_rate2="pa_rate2",pg_seed="pa_seed")
  h[names(aliases2)]<-h[unname(aliases2)]
  h["pg_enrollment"]<-"仅生成IA之后的独立新队列。可在IA一次入组，或用总体恒定/分段Poisson入组；时间0为IA，采用原计划Treatment比例p简单随机分配。"
  h["pg_reps"]<-"每个情景的当前IA条件下新队列模拟数，20–10000整数，N2乘B乘情景数不超过2000万。先100–500检查配置；增加B只降低Monte Carlo误差，不减少模型不确定性。"
  h["pg_view_trial"]<-"选择最近成功运行中已生成的轮次。使用保存的数据、模型和该轮独立种子恢复同一轨迹，仅改变记录预览和本轮导出，不重新估计IA或事件目标。"
  h[c("pg_enroll_prior_shape","pg_enroll_prior_rate","pg_drop_prior_shape","pg_drop_prior_rate")]<-h[c("enroll_prior_shape","enroll_prior_rate","drop_prior_shape","drop_prior_rate")]
  h["pg_process"]<-"固定过程使用输入的入组与脱落率；仅脱落Gamma按两组退出计数和随访暴露更新；入组与脱落Gamma还估计总体恒定Poisson入组率。选择后只填写对应参数。"
  h["pg_recruit_start"]<-"历史恒定Poisson入组窗口起点，自研究起点计时，需非负且早于终点。窗口须包括全部入组记录；例如0月，不使用个体随访年龄代替研究时间。"
  h["pg_recruit_end"]<-"历史入组窗口终点，自研究起点计时且不晚于IA。选择研究实际开放恒定入组的窗口，如12月；若已经停止入组，不把之后的关闭区间计入入组暴露。"
  h["pg_recruit_complete"]<-"确认该窗口内全部入组者都有当前终点记录，且期间采用恒定Poisson过程。筛选终点/分析标志后遗漏入组者会低估入组率；未确认时不估计入组后验。"
  h["pg_compare"]<-"在输入基准之外加入最多11个未来情景。所有情景共用当前IA、选定D2、每轮事件/过程参数抽样及潜在随机数，仅改变指定的未来倍率。"
  h["pg_scenario_csv"]<-"CSV每行含名称及Control事件、Treatment事件、入组、Control脱落、Treatment脱落五个倍率。事件/入组倍率0.01–10，脱落0–10；1表示基准，一次入组时入组倍率必须为1；基准率为0时倍率不产生非零率。"
  h["pg_view_scenario"]<-"选择最近成功结果中的一个未来情景，再查看该情景已生成的模拟轮次。只恢复保存的配置与随机数，不改IA得分或D2，也不重拟合模型。"
  assign("parameter_help",h,envir=parent.frame())
}
patient_adaptive_inputs <- function() {
  conditionalPanel("input.ad_engine === 'patient'",
    p(class="field-note","两个独立患者队列：阶段1在IA冻结；阶段2仅使用IA后新入组者。事件上限表示两队列用于组合检验的事件之和。"),
    selectInput("pa_purpose","患者资料需求",c("已观察 IA：计算新队列事件目标"="patient_interim","重估后：新队列条件预测"="patient_prediction","从起点：患者级完整设计评价"="patient_simulation")),
    selectInput("pa_unit","时间单位",c("月"="months","周"="weeks","日"="days"),"months"),
    conditionalPanel("input.pa_purpose !== 'patient_simulation'",section("阶段1 IA记录",
      selectInput("pa_source","IA 数据来源",c("ADTTE文件"="adtte","合成示例（240人，IA=540日）"="demo"),"adtte"),
      conditionalPanel("input.pa_source === 'adtte'",fileInput("pa_file","阶段1 ADTTE",accept=c(".csv",".xpt",".sas7bdat")),
        p(class="field-note","必需：USUBJID、PARAMCD、STARTDT、ADT、AVAL、CNSR和已知组别变量。筛选后每人一行；仍随访者须确认至IA。"),
        fields(dateInput("pa_origin","研究起点日期","2025-01-01"),numericInput("pa_cut","IA DCO（研究月数）",18,min=.001)),
        fields(uiOutput("pa_endpoint_ui"),uiOutput("pa_group_ui")),
        fields(selectInput("pa_aval_unit","文件 AVAL 单位",c("日"="days","周"="weeks","月"="months")),selectInput("pa_offset","AVAL 首日加项",c("+1日"="1","+0日"="0"))),
        fields(textInput("pa_dropout_codes","永久退出 CNSR 编码","2"),selectInput("pa_date_encoding","日期编码",c("YYYY-MM-DD"="iso","SAS数值日"="sas"))),
        fields(textInput("pa_flag","分析标志变量（可留空）",""),textInput("pa_flag_value","分析标志保留值","Y"))),
      uiOutput("pa_control_ui"),uiOutput("pa_data_note"))),
    conditionalPanel("input.pa_purpose === 'patient_prediction'",patient_prediction_inputs()),
    conditionalPanel("input.pa_purpose === 'patient_simulation'",
      section("两阶段患者与时间上限",fields(numericInput("pa_n1","阶段1最多入组 N1",250,min=10,max=5000,step=1),numericInput("pa_n2","新队列最多入组 N2",400,min=10,max=5000,step=1)),
        fields(numericInput("pa_window1","阶段1最大窗口（月）",36,min=.01),numericInput("pa_window2","IA后新队列最大窗口（月）",60,min=.01))),
      section("Control生存分布",event_parameter_fields("pa_","months",simulation_defaults(),simulation=TRUE)),
      section("入组过程",selectInput("pa_enrollment","各阶段入组方式",c("阶段开始一次入组"="batch","恒定Poisson入组"="constant")),
        conditionalPanel("input.pa_enrollment === 'constant'",fields(numericInput("pa_rate1","阶段1总体入组率（人/月）",30,min=.001),numericInput("pa_rate2","新队列总体入组率（人/月）",30,min=.001)))),
      section("独立脱落",navset_card_tab(nav_panel("Control",dropout_parameter_fields("padc_","months",simulation_defaults())),nav_panel("Treatment",dropout_parameter_fields("padt_","months",simulation_defaults())))),
      section("完整设计评价",numericInput("pa_true_hr","模拟真实 HR",.67,min=.01,max=5,step=.05),fields(numericInput("pa_reps","每HR完整试验数 B",500,min=20,max=10000,step=20),numericInput("pa_seed","随机种子",20261004,min=0,step=1)))))
}
patient_adaptive_results_ui <- function() {
  conditionalPanel("input.ad_engine === 'patient'",uiOutput("pa_status"),conditionalPanel("output.pa_busy === 'true'",actionButton("pa_cancel","取消本次运行")),
    p(class="field-note","固定原计划逆正态权重，阶段内标准log-rank，单侧Treatment获益。小样本阶段p值是渐近近似；完整患者模拟用于检验所选情景下的错误率。"),
    conditionalPanel("output.pa_result_purpose === 'patient_interim'",section("IA阶段检验",DTOutput("pa_ia",fill=FALSE)),section("新队列事件数重估",DTOutput("pa_decision",fill=FALSE)),p(class="field-note","这是当前IA条件下的目标计算，不是从起点的Ⅰ类错误或功效。阶段1患者IA后的事件不进入阶段2检验。")),
    conditionalPanel("output.pa_result_purpose === 'patient_prediction'",patient_prediction_results_ui()),
    conditionalPanel("output.pa_result_purpose === 'patient_simulation'",section("患者级完整设计评价",DTOutput("pa_summary",fill=FALSE)),
      p(class="field-note","原计划和重估设计共用同一患者轨迹，按各自事件目标截取。未达目标关闭且不拒绝；无效阶段检验另记，并报告全部请求轮次的概率界。取消后的有效拒绝率仅描述已完成轮次。"),
      section("逐轮决策（前200行）",DTOutput("pa_trials",fill=FALSE)),section("首轮两阶段观察样例",DTOutput("pa_observed",fill=FALSE))))
}
patient_adaptive_exports_ui <- function() {
  conditionalPanel("input.ad_engine === 'patient'",div(class="export-bar",downloadButton("pa_config_download","数据与配置 JSON"),downloadButton("pa_result_download","结果 CSV"),downloadButton("pa_script_download","复现 R 脚本"),downloadButton("pa_report_download","说明 Markdown"),
    conditionalPanel("output.pa_result_purpose === 'patient_prediction'",downloadButton("pg_resources_download","时间与资源 CSV"),downloadButton("pg_paired_download","配对差值 CSV"),downloadButton("pg_trials_download","条件逐轮 CSV"),downloadButton("pg_observed_download","本轮观察记录 CSV"),downloadButton("pg_models_download","模型与诊断 RDS"),conditionalPanel("output.pg_has_scenarios === 'true'",downloadButton("pg_contrasts_download","情景配对差值 CSV")),conditionalPanel("output.pg_extended === 'true'",downloadButton("pg_process_download","逐轮过程参数 CSV"))),
    conditionalPanel("output.pa_result_purpose === 'patient_simulation'",downloadButton("pa_trials_download","逐轮 CSV"),downloadButton("pa_observed_download","首轮观察记录 CSV"))))
}

patient_prediction_inputs <- function() {
 tagList(actionButton("pg_batch_freeze","冻结当前IA与新队列参数至批量研究"),
  section("IA后新队列",fields(numericInput("pg_n2","新队列最多入组 N2",400,min=10,max=5000,step=1),numericInput("pg_window2","IA后最大窗口（月）",60,min=.01)),
   p(class="field-note","按当前IA先选定D2，再模拟新队列；阶段1后续事件不进入组合检验。")),
  section("新队列事件模型",radioButtons("pg_model_mode","模型来源",c("从IA各组分别拟合"="fit","分别输入两组参数"="manual"),"fit",inline=TRUE),
   conditionalPanel("input.pg_model_mode === 'fit'",
    fields(selectInput("pg_fit_method","两组拟合分布",choices,"exponential"),selectInput("pg_uncertainty","事件参数处理",c("固定拟合参数"="plugin","受试者Bootstrap"="bootstrap","指数/PWE Gamma后验"="gamma","Weibull MCMC后验"="bayes_weibull"))),
    conditionalPanel("input.pg_fit_method === 'pwe'",textInput("pg_cuts","PWE切点（随访月数）","3,6,12")),
    conditionalPanel("input.pg_fit_method === 'km_tail'",numericInput("pg_tail_rate","KM尾部风险率（每月）",.06,min=.000001)),
    conditionalPanel("input.pg_uncertainty === 'gamma'",fields(numericInput("pg_prior_shape","事件Gamma先验shape",.5,min=.001),numericInput("pg_prior_rate","事件Gamma先验rate（受试者·月）",50/30.4375,min=.000001))),
    conditionalPanel("input.pg_uncertainty === 'bayes_weibull'",
     fields(numericInput("pg_log_eta_mean","log(eta/月)先验均值",log(450/30.4375)),numericInput("pg_log_eta_sd","log(eta)先验标准差",1,min=.01)),
     fields(numericInput("pg_log_shape_mean","log(k)先验均值",0),numericInput("pg_log_shape_sd","log(k)先验标准差",.75,min=.01)),
     fields(numericInput("pg_mcmc_chains","MCMC链数",4,min=2,max=4,step=1),numericInput("pg_mcmc_warmup","每链预热",1000,min=200,max=10000,step=100)),numericInput("pg_mcmc_draws","每链保留",3000,min=200,max=10000,step=100))),
   conditionalPanel("input.pg_model_mode === 'manual'",navset_card_tab(nav_panel("指定Control组",event_parameter_fields("pgc_","months",simulation_defaults(),simulation=TRUE)),nav_panel("另一组Treatment",event_parameter_fields("pgt_","months",simulation_defaults(),simulation=TRUE)))),
   p(class="field-note","拟合使用原IA组内删失似然；Bootstrap只重拟合事件模型。每轮每组抽取一套参数，新队列该组患者共用。")),
  section("入组与脱落参数处理",selectInput("pg_process","未来过程参数",c("输入固定过程参数"="fixed","仅脱落Gamma后验"="gamma_dropout","入组与脱落Gamma后验"="gamma")),
   p(class="field-note","入组率为新队列总体率，分组按原比例p；脱落率按组估计。每轮抽取一次过程率，情景间共享。")),
  conditionalPanel("input.pg_process !== 'gamma'",section("新队列入组",selectInput("pg_enrollment","入组方式",c("IA时一次入组"="batch","恒定Poisson入组"="constant","分段Poisson入组"="piecewise")),
   conditionalPanel("input.pg_enrollment === 'constant'",numericInput("pg_rate2","总体入组率（人/月）",30,min=.001)),
   conditionalPanel("input.pg_enrollment === 'piecewise'",fields(textInput("pg_enroll_cuts","入组切点（IA后月数）","6,12"),textInput("pg_enroll_rates","各段总体入组率（人/月）","20,30,40"))))),
  conditionalPanel("input.pg_process === 'gamma'",section("恒定Poisson入组后验",p(class="field-note","未来使用总体恒定Poisson入组。历史窗口以研究起点计时，必须在IA之前。"),
   fields(numericInput("pg_recruit_start","历史入组窗口起点（研究月数）",0,min=0),numericInput("pg_recruit_end","历史入组窗口终点（研究月数）",12,min=.001)),
   checkboxInput("pg_recruit_complete","该窗口内入组记录完整，期间采用恒定Poisson入组",FALSE),
   fields(numericInput("pg_enroll_prior_shape","入组Gamma先验shape",1,min=.001),numericInput("pg_enroll_prior_rate","入组Gamma先验rate（月）",1,min=.000001)))),
  conditionalPanel("input.pg_process === 'fixed'",section("新队列独立脱落",navset_card_tab(nav_panel("指定Control组",dropout_parameter_fields("pgdc_","months",simulation_defaults())),nav_panel("另一组Treatment",dropout_parameter_fields("pgdt_","months",simulation_defaults()))))),
  conditionalPanel("input.pg_process !== 'fixed'",section("分组脱落后验",p(class="field-note","使用原IA各组永久退出计数和所有患者随访暴露；行政删失与终点事件也贡献截至末次观察的暴露。两组独立更新，共用下面的先验设置。"),
   fields(numericInput("pg_drop_prior_shape","脱落Gamma先验shape",.5,min=.001),numericInput("pg_drop_prior_rate","脱落Gamma先验rate（受试者·月）",50/30.4375,min=.000001)))),
  section("未来情景",checkboxInput("pg_compare","比较多个未来情景",FALSE),
   conditionalPanel("input.pg_compare",textAreaInput("pg_scenario_csv","情景表 CSV",value="情景,Control事件倍率,Treatment事件倍率,入组倍率,Control脱落倍率,Treatment脱落倍率\nTreatment风险降低,1,0.75,1,1,1\n脱落增加,1,1,1,1.5,1.5",rows=5),
    p(class="field-note","自动保留输入基准。只调整IA后的过程；D2与原方案权重在各情景中相同。"),uiOutput("pg_scenario_note"),DTOutput("pg_scenario_preview",fill=FALSE))),
  section("条件模拟",fields(numericInput("pg_reps","新队列模拟数 B",500,min=20,max=10000,step=20),numericInput("pg_seed","随机种子",20261004,min=0,step=1))))
}
patient_prediction_results_ui <- function() {
 tagList(section("冻结的IA与选定目标",DTOutput("pg_ia",fill=FALSE),DTOutput("pg_decision",fill=FALSE)),
  conditionalPanel("output.pg_has_scenarios === 'true'",section("未来情景定义",DTOutput("pg_scenario_table",fill=FALSE))),
  section("条件拒绝与达标概率",DTOutput("pg_summary",fill=FALSE),
   p(class="field-note","概率基于当前IA、输入的未来模型和固定权重；不是从起点的功效或Ⅰ类错误。目标未达时关闭且不拒绝。全部请求概率界包括失败与未完成轮次；Wilson区间仅描述有效决策的模拟误差。")),
  section("IA后达标时间与资源",DTOutput("pg_resources",fill=FALSE),
   p(class="field-note","达标轮次分位数只描述窗口内达标者；生成轮次分位数把窗口未达记作∞。全部请求中位数给出失败/未完成轮次的上下界。关闭时间另报；日历日期按研究起点加连续研究日向下取整。资源平均值以成功生成的轮次为分母。")),
  section("与原计划的配对差值",DTOutput("pg_paired",fill=FALSE),p(class="field-note","同一新队列轨迹按重估D2和原定D2分别截取。差值为重估减原计划；不同截点的检验决策不必逐轮嵌套。")),
  conditionalPanel("output.pg_has_scenarios === 'true'",section("情景相对输入基准的配对差值",DTOutput("pg_scenario_contrasts",fill=FALSE),p(class="field-note","差值为该情景减输入基准；达标等待差只使用两情景均达标的轮次，关闭等待差使用两情景均生成的轮次。每情景请求分母为B。"))),conditionalPanel("output.pg_has_process === 'true'",section("过程后验",DTOutput("pg_process_table",fill=FALSE))),
  section("事件模型与诊断",DTOutput("pg_models",fill=FALSE),DTOutput("pg_diagnostics",fill=FALSE)),
  section("逐轮结果（前200行）",DTOutput("pg_trials",fill=FALSE)),
  section("查看一轮",conditionalPanel("output.pg_has_scenarios === 'true'",selectInput("pg_view_scenario","查看未来情景",character())),selectInput("pg_view_trial","成功生成的模拟轮次",character()),DTOutput("pg_sample_decision",fill=FALSE),DTOutput("pg_observed",fill=FALSE)))
}
