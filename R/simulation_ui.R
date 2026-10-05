# Definitions are added before the shared input wrappers construct labels.
simulation_help <- c(sim_unit="输入及结果单位；默认月，切换会换算已填时间和率。1月=30.4375日。",
 sim_origin="研究时间0对应的日期。用于导出日历日期，不改变经过时间。",sim_endpoint="每次生成一个终点：PFS或OS。不会自动生成联合PFS/OS。",
 sim_n="计划总人数，1–5000整数。未在截点前入组者不进入该截点分析。",sim_groups="单组或2–6组；各组分布分别填写。",
 sim_enroll_mode="恒定/分段Poisson入组或固定入组日程。Poisson模式达到计划N后停止。",sim_enroll_rate="研究总体入组率，人/所选单位；恒定入组需为正。",
 sim_enroll_cuts="自研究起点起的正递增入组切点，逗号分隔；与个体风险切点不同。",sim_enroll_rates="各段总体入组率，非负，数量=入组切点数+1。末段0可能使入组无法完成。",
 sim_schedule="填写N个非负、非递减研究时间，可同日多人入组；例如N=3填0,1,2。",
 sim_cut_mode="固定DCO可输入多个截点；事件驱动以首次累计达到D*的事件时间为DCO。未达标按最大窗口末分析并记录未达标。",
 sim_cuts="自研究起点起的1–20个正递增DCO，不超过最大研究窗口。相同试验的各截点共用一条轨迹。",
 sim_target="总累计目标事件数D*，正整数。固定DCO时也报告是否达到该目标。",sim_max="最大研究观察窗口，换算为日后不超过3650日。",
 sim_fixed="个体随访年龄上的1–20个正递增时点，如6,12,18月；超过当前观察支持时报告不可估计，不外推KM。",
 sim_reps="1–200个独立重复试验，用于生成与描述生存估计分布；本阶段没有功效或Ⅰ类错误检验。",sim_seed="0至2147483647整数；相同版本和配置可复现。",
 sim_trial="查看已完成运行中某个试验，所选试验和截点也用于样例数据导出。",sim_view_cut="查看该试验的一个分析截点；固定DCO模式各截点共用同一患者轨迹。")
simulation_help["sim_unit"] <- paste0(simulation_help["sim_unit"],"时间、入组率、事件率和脱落率同步转换，概率与形状不变。")
simulation_help["sim_origin"] <- paste0(simulation_help["sim_origin"],"建议填计划首例入组的研究起点，固定日程也以此为零点。")
simulation_help["sim_endpoint"] <- paste0(simulation_help["sim_endpoint"],"按研究分析定义选一项；若需要两个终点关联，需后续联合模型。")
simulation_help["sim_n"] <- paste0(simulation_help["sim_n"],"例如300表示最多计划入组300人，并不保证最大窗口内全部入组。")
simulation_help["sim_groups"] <- paste0(simulation_help["sim_groups"],"1表示单组，2可用于治疗与对照；组名和权重在各组页设置。")
simulation_help["sim_enroll_rate"] <- paste0(simulation_help["sim_enroll_rate"],"例如15人/月表示Poisson强度，不代表每月必定恰入组15人。")
simulation_help["sim_enroll_cuts"] <- paste0(simulation_help["sim_enroll_cuts"],"例如3,6表示启动后第3与第6月改变入组强度。")
simulation_help["sim_enroll_rates"] <- paste0(simulation_help["sim_enroll_rates"],"例如9,18,12对应两个切点的三段入组强度。")
simulation_help["sim_schedule"] <- paste0(simulation_help["sim_schedule"],"允许不同患者同日入组；单位随时间选择同步换算。")
simulation_help["sim_target"] <- paste0(simulation_help["sim_target"],"例如180；若可观察事件不足则记录未达标，不排除该次试验。")
simulation_help["sim_max"] <- paste0(simulation_help["sim_max"],"例如48月；未达目标时仍保留窗口末观察数据。")
simulation_help["sim_seed"] <- paste0(simulation_help["sim_seed"],"改变种子生成另一批患者时间；设置后保存到配置记录。")
simulation_help["sim_trial"] <- paste0(simulation_help["sim_trial"],"每次试验有独立患者抽样；切换选择不会重新运行模拟。")
simulation_help["sim_view_cut"] <- paste0(simulation_help["sim_view_cut"],"截点序号对应预设DCO列表；事件驱动每轮只有一个实际截点。")
parameter_help <- c(parameter_help,simulation_help)
for(j in 1:6) {
  pre <- paste0("s",j,"_")
  base <- c("design_model","exp_input","median","exp_rate","weibull_input","eta","shape","log_input","log_mu","scale","g_rate","g_shape","cure","median2","shape2","mix","parameter_cuts","parameter_rates","drop_input","dropout_rate","drop_prob","drop_period")
  parameter_help[paste0(pre,base)] <- parameter_help[base]
  parameter_help[paste0(pre,c("name","weight","survival_time","survival_prob","pwe_input","pwe_survivals","pwe_last_rate"))] <- c(
    "组名需非空且唯一，用于数据和结果标签。例如Control与Treatment；不得把相同组名用于两个不同分布。", "正分配权重，如1与1代表1:1。简单随机按权重分配，实际人数随机，不强制每组相同。",
    "指数模型固定随访时点，需为正数。例如12月；这是从患者入组起的年龄，不是研究DCO日历时间。", "该时点生存率，需在(0,1)。lambda=−log(S)/t。例如12月生存率0.5对应中位时间12月。",
    "PWE可输入各段风险率，或切点对应的生存率并另给最后一段率。后者转换为分段风险后生成事件时间。", "数量与风险切点相同；(0,1]且非递增。相邻生存率相等对应零风险段；例如.91,.76,.50对应三个切点。",
    "最后风险切点之后的率，非负。未观察尾部由此给定，不由切点生存率决定；0表示此后不再发生事件。")
}
simulation_defaults <- function(unit="months",index=1) {
  c(group_input_defaults(unit,index),list(weight=1,survival_time=12*30.4375/time_factor(unit),survival_prob=.5,
    pwe_input="rates",pwe_survivals=".91,.76,.50",pwe_last_rate=.06*time_factor(unit)/30.4375))
}
simulation_ui <- function() {
  nav_panel("生存数据模拟",value="simulation",
    div(class="page-title",h2("生存数据模拟"),actionButton("sim_run","生成并分析",class="btn-primary")),
    p(class="field-note","设计阶段 · 参数输入。生成完整受试者轨迹，再按DCO构建观察数据。"),
    navset_card_tab(id="sim_tabs",
      nav_panel("研究设置",section("研究与入组",
        fields(selectInput("sim_unit","时间单位",c("日"="days","周"="weeks","月"="months"),"months"),dateInput("sim_origin","研究起点日期","2025-01-01")),
        fields(selectInput("sim_endpoint","终点",c("PFS","OS"),"PFS"),numericInput("sim_n","计划总人数 N",300,min=1,max=5000,step=1)),
        numericInput("sim_groups","组数",2,min=1,max=6,step=1),
        selectInput("sim_enroll_mode","总体入组方式",c("恒定Poisson"="constant","分段Poisson"="piecewise","固定入组日程"="schedule")),
        conditionalPanel("input.sim_enroll_mode === 'constant'",numericInput("sim_enroll_rate","总体入组率（人/月）",15,min=.000001)),
        conditionalPanel("input.sim_enroll_mode === 'piecewise'",fields(textInput("sim_enroll_cuts","入组切点（研究月数）","3,6"),textInput("sim_enroll_rates","各段入组率（人/月）","9,18,12"))),
        conditionalPanel("input.sim_enroll_mode === 'schedule'",textInput("sim_schedule","N个入组研究时间（月）",""))),
        section("截点与运行",
          selectInput("sim_cut_mode","分析截点规则",c("固定DCO列表"="fixed","首次达到目标事件数"="target")),
          conditionalPanel("input.sim_cut_mode === 'fixed'",textInput("sim_cuts","分析DCO（研究月数）","12,24,36")),
          conditionalPanel("input.sim_cut_mode === 'target'",fields(numericInput("sim_target","目标事件数 D*",180,min=1,step=1),numericInput("sim_max","最大研究窗口（月）",48,min=.01))),
          textInput("sim_fixed","固定随访时点（月）","6,12,18"),
          fields(numericInput("sim_reps","重复试验数",20,min=1,max=200,step=1),numericInput("sim_seed","随机种子",20261004,min=0,step=1)))),
      nav_panel("组别与分布",uiOutput("sim_group_inputs")),
      nav_panel("模拟结果",uiOutput("sim_status"),DTOutput("sim_overview",fill=FALSE),
        fields(selectInput("sim_trial","查看试验",character()),selectInput("sim_view_cut","查看截点",character())),
        plotlyOutput("sim_km",height="390px"),DTOutput("sim_summary",fill=FALSE),DTOutput("sim_fixed_table",fill=FALSE),DTOutput("sim_risk",fill=FALSE),
        div(class="export-bar",downloadButton("sim_observed_download","连续时间观察数据 CSV"),downloadButton("sim_adtte_download","ADTTE CSV")),
        uiOutput("sim_export_note"),DTOutput("sim_data",fill=FALSE)),
      nav_panel("导出与记录",p(class="field-note","下载结果对应最近成功运行。真值数据包含未观察事件时间，单独导出。"),
        div(class="export-bar",downloadButton("sim_summary_download","每轮分析 CSV"),downloadButton("sim_fixed_download","每轮固定时点 CSV"),downloadButton("sim_cuts_download","截点及达标 CSV"),downloadButton("sim_truth_download","模拟真值 CSV"),downloadButton("sim_config_download","配置 JSON"),downloadButton("sim_script_download","复现 R 脚本"),downloadButton("sim_report_download","模拟报告 Markdown"))))
  )
}
simulation_group_ui <- function(j,unit) {
  pre <- paste0("s",j,"_"); id <- function(x)paste0(pre,x); d <- simulation_defaults(unit,j); u <- time_label(unit)
  cond <- function(js,...)conditionalPanel(gsub("GROUP",paste0("input.",pre),js,fixed=TRUE),...)
  model_fields <- event_parameter_fields(pre,unit,d,simulation=TRUE)
  section(paste("组",j),fields(textInput(id("name"),"组名",d$name),numericInput(id("weight"),"随机分配权重",1,min=.001)),
    model_fields,dropout_parameter_fields(pre,unit,d))
}
