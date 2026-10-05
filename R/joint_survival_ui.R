register_joint_help <- function() {
  h<-get("parameter_help",envir=parent.frame())
  h<-c(h,c(
    js_unit="本任务输入/显示单位可选日、周、月，默认月；1月固定30.4375日。切换转换已填写时间、速率及风险切点，形状/分配概率/风险倍数不变；提交结果保留原单位。",
    js_origin="研究经过时间0的日期，用于ADTTE日历导出。建议用方案设定的研究起点；不会改变患者相对风险起点或生成分布。",
    js_n="计划总人数，整数1–5000。每轮独立生成最多N名患者；在截点前尚未入组者不进入当时分析，Poisson入组不保证窗口内全部入组。",
    js_groups="单组生成Control轨迹，两组增加Treatment及其三个转移风险倍数。首批范围为1–2组；两终点总来自同一患者，不分别随机分配。",
    js_allocation="Treatment简单随机分配概率p，0.01–0.99，例如1:1填0.5、Treatment:Control=2:1填2/3。实际人数随机，不强制各组精确比例；单组不读取此参数。",
    js_clock="进展后时间表示在进入状态1后重新起算1→2年龄；自入组时间使用H12(s+u)−H12(s)。指数模型两种时钟相同，Weibull/PWE通常不同，按建模假设选择。",
    js_q01="Treatment相对于Control的0→1进展转移风险倍数，0.01–100，1为相同、0.7表示降低30%。它不是边际PFS HR；两组时才使用，需按转移层面的研究假设填写。",
    js_q02="Treatment相对于Control的0→2未进展死亡转移风险倍数，0.01–100。该时钟与进展竞争；进展后不继续使用原直接死亡时钟。不能把总体OS HR直接填为所有转移HR。",
    js_q12="Treatment相对于Control的1→2进展后死亡转移风险倍数，0.01–100。风险年龄由所选死亡时钟定义；1表示同一转移风险，不保证边际OS分布相同。",
    js_drop_control="Control独立永久退出全部终点随访的恒定风险mu，非负，单位为每当前时间单位。PFS与OS共享同一退出时间；无退出假设填0，不将仅停药等同于退出OS随访。",
    js_drop_treatment="Treatment独立永久退出全部终点随访的恒定风险mu，非负。两终点共享这次退出，独立于潜在状态轨迹；单组不读取。分终点退出及状态依赖退出尚未接入。",
    js_enroll_mode="选择总体恒定Poisson、分段Poisson或固定入组日程。入组日为研究时间，患者风险年龄从自己的入组开始，两组患者共用同一总体入组过程。",
    js_enroll_rate="总体Poisson入组率，人/当前单位，需为正；例如15人/月。达到计划N后停止，不表示每月固定人数；只有恒定入组分支读取。",
    js_enroll_cuts="分段总体入组切点，逗号分隔、正递增，按研究经过时间填写，如3,6月。与转移模型的患者风险年龄切点不同；仅分段入组读取。",
    js_enroll_rates="分段总体入组率，非负，人/当前单位，数量为入组切点数+1；例如9,18,12。尾段0可能使部分计划患者永不入组，仍在真值表保留。",
    js_schedule="固定入组日程需N个非负、非递减研究时间，允许同日多人。无文件要求，逗号分隔；仅固定日程分支读取，不用患者随访年龄替代研究时间。",
    js_cut_mode="固定DCO列表共用同一患者轨迹；事件驱动只选一个预定触发终点PFS或OS。达到该终点D*时两终点在同一DCO分析，不能看结果后挑更早者。",
    js_cuts="1–20个正递增研究DCO，逗号分隔，不超过3650日对应时间。例如12,24,36月；两终点共同截断，不分别指定各自DCO。",
    js_target_endpoint="预先选择PFS或OS作为事件驱动触发终点。仅其已观察、退出前且窗口内的累计事件触发DCO；另一个终点在同一DCO只做描述分析。",
    js_target="预定触发终点的累计事件目标D*，整数1–100000。每人最多一个该终点事件；目标超过实际可观察人数时不能达到，仍保留窗口末分析和未达标标志。",
    js_max="事件驱动最大研究经过时间，正数、换算后最长3650日。窗口内未达D*仍分析窗口末的两终点，不能丢弃这些轮次后只统计成功试验。",
    js_fixed="1–20个正递增个体随访年龄，用于PFS/OS KM生存率和在险人数。例如6,12,18月；不超过3650日，超出观察支持时保留不可估计值，不外推。",
    js_reps="重复试验数，整数1–200，每轮独立生成患者。可以先用20探索输入；N×重复数×截点数最多20万，本模块尚未提供批量功效或错误率评价。",
    js_seed="随机种子为0–2147483647整数。每轮由主种子和原轮次派生，失败轮次不移动后续轮次种子；同版本/config可重现，复现脚本尚未验证。",
    js_trial="选择最近成功提交中完成生成与分析的一轮。只改变单轮图表与观察/ADTTE导出，不重新运行；失败轮次另在状态表保留。",
    js_view_cut="选择该轮共同DCO序号。固定模式同轮各截点共享轨迹，事件驱动每轮一个实际DCO；所选PFS/OS使用相同患者入组信息。"))
  fields<-c("design_model","exp_input","median","exp_rate","weibull_input","eta","shape","parameter_cuts","parameter_rates")
  for(key in names(joint_transition_names)) {
    pre<-paste0("js",key,"_")
    for(id in fields)h[paste0(pre,id)]<-paste0("本转移：",joint_transition_names[[key]],"。",h[[id]])
    h[paste0(pre,"design_model")]<-"本转移的潜在时钟分布限指数、Weibull或分段指数。指数恒定风险，Weibull由形状控制变化，PWE按风险年龄分段；不是直接指定边际PFS或OS分布。"
    h[paste0(pre,"median")]<-"本转移单独作用时的潜在时钟中位时间m，需大于0。0→1/0→2从入组起算，1→2按所选时钟定义；竞争与状态转移后边际PFS/OS中位数通常不同。按转移层面的依据填写。"
    h[paste0(pre,"exp_rate")]<-"本转移恒定原因别风险lambda，需大于0，单位1/当前单位；潜在时钟m=log(2)/lambda。不是每期事件概率，也不直接等于总体PFS/OS风险率。切换中位/率会换算。"
    h[paste0(pre,"weibull_input")]<-"本转移Weibull可用潜在时钟中位时间m或尺度eta，均需同时给形状k；eta=m/[log(2)]^(1/k)。切换保留本转移分布，不代表边际PFS或OS分布相同。"
    h[paste0(pre,"eta")]<-"本转移Weibull尺度eta>0，按当前单位计时，H(eta)=1；潜在时钟中位时间为eta*[log(2)]^(1/k)。不是边际PFS/OS中位数，1→2时间含义依所选时钟。"
    h[paste0(pre,"parameter_cuts")]<-"本转移PWE风险年龄切点，逗号分隔、正递增。0→1/0→2为自入组时间；1→2依所选时钟用进展后年龄或自入组年龄。默认3,6,12月仅为输入示例。"
    h[paste0(pre,"parameter_rates")]<-"本转移各段原因别风险率，非负，数量为切点数+1，单位1/当前单位。0风险段或零尾部允许该转移不发生；转移与另一竞争事件的关联通过状态路径形成。"
  }
  assign("parameter_help",h,envir=parent.frame())
}
joint_transition_ui <- function(key,index,unit="months",namespace="js") {
  pre<-paste0(namespace,key,"_");id<-function(x)paste0(pre,x);d<-joint_defaults(unit,index);u<-time_label(unit)
  cond<-function(js,...)conditionalPanel(gsub("GROUP",paste0("input.",pre),js,fixed=TRUE),...)
  section(paste0(joint_transition_names[[key]]," · Control 转移参数"),
    selectInput(id("design_model"),"转移时钟分布",c("指数"="exponential","Weibull"="weibull","分段指数 PWE"="pwe"),"exponential"),
    cond("GROUPdesign_model === 'exponential'",selectInput(id("exp_input"),"指数参数输入",c("潜在时钟中位时间 m"="median","原因别风险率 lambda"="rate"))),
    cond("GROUPdesign_model === 'weibull'",selectInput(id("weibull_input"),"Weibull参数输入",c("潜在时钟中位时间 m + 形状 k"="median","尺度 eta + 形状 k"="eta"))),
    cond("(GROUPdesign_model === 'exponential' && GROUPexp_input !== 'rate') || (GROUPdesign_model === 'weibull' && GROUPweibull_input !== 'eta')",numericInput(id("median"),paste0("潜在转移时钟中位时间 m（",u,"）"),d$median,min=.001)),
    cond("GROUPdesign_model === 'exponential' && GROUPexp_input === 'rate'",numericInput(id("exp_rate"),paste0("原因别风险率 lambda（每",u,"）"),d$exp_rate,min=.000001)),
    cond("GROUPdesign_model === 'weibull' && GROUPweibull_input === 'eta'",numericInput(id("eta"),paste0("Weibull尺度 eta（",u,"）"),d$eta,min=.001)),
    cond("GROUPdesign_model === 'weibull'",numericInput(id("shape"),"形状 k",d$shape,min=.01)),
    cond("GROUPdesign_model === 'pwe'",fields(textInput(id("parameter_cuts"),paste0("转移风险年龄切点（",u,"）"),d$parameter_cuts),textInput(id("parameter_rates"),paste0("各段转移风险率（每",u,"）"),d$parameter_rates))),
    uiOutput(id("parameter_conversion")),p(class="field-note","这里的中位时间描述单转移潜在时钟；边际PFS/OS分布由竞争与后续转移共同决定。"))
}
joint_survival_ui <- function() {
  nav_panel("PFS/OS 联合模拟",value="joint",
    div(class="page-title",h2("PFS/OS 联合生存模拟"),actionButton("js_run","生成联合数据",class="btn-primary")),
    p(class="field-note","设计阶段 · 参数输入。0未进展存活，1进展后存活，2死亡；支持0→1、0→2、1→2。新增功能待复核。"),
    navset_card_tab(id="js_tabs",
      nav_panel("研究设置",value="js_settings",
        section("研究与组别",fields(selectInput("js_unit","时间单位",c("日"="days","周"="weeks","月"="months"),"months"),dateInput("js_origin","研究起点日期","2025-01-01")),
          fields(numericInput("js_n","计划总人数 N",300,min=1,max=5000,step=1),selectInput("js_groups","组别",c("单组 Control"=1,"Control / Treatment 两组"=2),2)),
          conditionalPanel("input.js_groups === '2'",numericInput("js_allocation","Treatment分配概率 p",.5,min=.01,max=.99)),
          selectInput("js_clock","进展后死亡的计时方式",c("进展后时间（clock-reset）"="reset","自入组时间（clock-forward）"="forward"))),
        section("入组与共同退出",selectInput("js_enroll_mode","总体入组方式",c("恒定Poisson"="constant","分段Poisson"="piecewise","固定入组日程"="schedule")),
          conditionalPanel("input.js_enroll_mode === 'constant'",numericInput("js_enroll_rate","总体入组率（人/月）",15,min=.000001)),
          conditionalPanel("input.js_enroll_mode === 'piecewise'",fields(textInput("js_enroll_cuts","入组切点（研究月数）","3,6"),textInput("js_enroll_rates","各段入组率（人/月）","9,18,12"))),
          conditionalPanel("input.js_enroll_mode === 'schedule'",textInput("js_schedule","N个入组研究时间（月）","")),
          numericInput("js_drop_control","Control共同退出率（每月）",.005,min=0),
          conditionalPanel("input.js_groups === '2'",numericInput("js_drop_treatment","Treatment共同退出率（每月）",.005,min=0)),
          p(class="field-note","每位患者的PFS与OS共用一次独立永久退出。仅停止治疗但继续OS随访不由这个共同退出模型表示。"))),
      nav_panel("转移模型",value="js_models",
        p(class="field-note","先指定Control三个转移。两组时，Treatment在对应转移风险上乘以各自q；无需另填边际PFS/OS中位数。"),
        joint_transition_ui("01",1),joint_transition_ui("02",2),joint_transition_ui("12",3),
        conditionalPanel("input.js_groups === '2'",section("Treatment转移风险倍数",fields(numericInput("js_q01","进展转移 q01",.7,min=.01,max=100),numericInput("js_q02","未进展死亡 q02",1,min=.01,max=100)),numericInput("js_q12","进展后死亡 q12",.85,min=.01,max=100)))),
      nav_panel("截点与运行",value="js_runsettings",
        section("共同DCO",selectInput("js_cut_mode","分析截点规则",c("固定DCO列表"="fixed","预定终点达到事件目标"="target")),
          conditionalPanel("input.js_cut_mode === 'fixed'",textInput("js_cuts","共同分析DCO（研究月数）","12,24,36")),
          conditionalPanel("input.js_cut_mode === 'target'",fields(selectInput("js_target_endpoint","触发终点",c("PFS","OS")),numericInput("js_target","触发终点 D*",180,min=1,max=100000,step=1)),numericInput("js_max","最大研究窗口（月）",48,min=.001)),
          textInput("js_fixed","固定个体随访时点（月）","6,12,18")),
        section("模拟设置",fields(numericInput("js_reps","重复试验数",20,min=1,max=200,step=1),numericInput("js_seed","随机种子",20261005,min=0,max=2147483647,step=1)))),
      nav_panel("联合结果",value="js_results",uiOutput("js_status"),
        section("重复试验汇总",DTOutput("js_overview",fill=FALSE)),
        section("每轮生成/分析状态",DTOutput("js_trial_status",fill=FALSE)),
        fields(selectInput("js_trial","查看试验",character()),selectInput("js_view_cut","查看共同截点",character())),
        section("两终点KM",plotlyOutput("js_km",height="450px")),
        section("共同DCO与事件计数",DTOutput("js_cuts_table",fill=FALSE)),
        section("该轮中位时间",DTOutput("js_summary",fill=FALSE)),
        section("固定随访时点",DTOutput("js_fixed_table",fill=FALSE)),
        section("DCO状态记录",DTOutput("js_states",fill=FALSE)),
        section("连续观察数据 · 两终点",DTOutput("js_observed",fill=FALSE)),
        section("已观察转移区间",DTOutput("js_intervals",fill=FALSE))),
      nav_panel("导出",value="js_exports",p(class="field-note","下载保留最近成功提交的配置及结果。连续时间固定以日导出；真值单独下载，含未观察时钟与结局。"),
        div(class="export-bar",downloadButton("js_observed_download","所选观察数据 CSV"),downloadButton("js_adtte_download","所选双终点 ADTTE CSV"),
          downloadButton("js_intervals_download","所选转移区间 CSV"),downloadButton("js_states_download","所选状态 CSV"),
          downloadButton("js_summary_download","全部每轮分析 CSV"),downloadButton("js_fixed_download","全部固定时点 CSV"),downloadButton("js_cuts_download","全部共同截点 CSV"),
          downloadButton("js_overview_download","联合汇总 CSV"),downloadButton("js_trial_status_download","生成/分析状态 CSV"),downloadButton("js_truth_download","全部模拟真值 CSV"),
          downloadButton("js_config_download","配置 JSON"),downloadButton("js_result_download","完整结果 RDS"),downloadButton("js_script_download","复现 R 脚本"),downloadButton("js_report_download","摘要 Markdown")),
        uiOutput("js_export_note"))))
}
