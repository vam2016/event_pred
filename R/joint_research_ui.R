register_joint_research_help <- function() {
  h<-get("parameter_help",envir=parent.frame())
  common<-c("unit","origin","allocation","clock","enroll_mode","enroll_rate","enroll_cuts","enroll_rates","drop_control","drop_treatment")
  for(k in common)h[paste0("jr_",k)]<-h[[paste0("js_",k)]]
  for(e in names(joint_transition_names))for(k in c("design_model","exp_input","median","exp_rate","weibull_input","eta","shape","parameter_cuts","parameter_rates"))h[paste0("jr",e,"_",k)]<-h[[paste0("js",e,"_",k)]]
  h<-c(h,c(jr_run_source="页面参数或v0.27联合研究无损配置JSON二选一，文件模式不读取页面其他参数。无需患者数据。",
    jr_run_name="研究名称1–100字符，用于本机研究库与报告，不影响模拟。",
    jr_config_file="导入event_pred.batch_config.v1且research_family为joint_endpoints的配置，最多5MB。旧描述性联合模拟配置不属于该格式。",
    jr_cut_rule="预定共同一次Final：固定DCO、PFS或OS达标、两者均达标、任一先达标。所有方法在同一个DCO上计算；事件触发规则有限样本错误率尚未校准。",
    jr_max="非固定规则最大研究窗口，正、最长3650日，未满足规则按预定窗口处理；不是每位患者随访长度。",
    jr_miss="未满足共同规则时选择窗口关闭不声明，或窗口末一次Final。前者统计量可作描述，不能用名义p改写不声明决策。",
    jr_scenario_source="列表组合或情景CSV二选一；CSV是设计参数表，不是ADTTE。列表组合各N、效应定义、入组/退出倍数和截点参数。",
    jr_file="严格列名随当前截点规则变化：label,n,q01,q02,q12,enroll_scale,dropout_scale，加dco或target_pfs/target_os。最多60情景，完全重复参数拒绝。",
    jr_csv_unit="情景CSV中dco的单位，默认月；只固定DCO文件模式读取。事件目标始终整数，不按时间单位换算。",
    jr_n_values="N列表，逗号分隔的10–5000整数；不同情景独立生成，不共用患者。",
    jr_profiles="每行：名称 | q01,q02,q12；1–12个唯一名称，倍数0.01–100。转移q不是PFS/OS边际HR；1,1,1为两终点共同零效应。",
    jr_enroll_scales="总体入组强度倍数列表，0.01–100；相乘基准Poisson率，不改变风险年龄。",
    jr_dropout_scales="两组共同随访退出风险倍数列表，0–100，0表示本情景不退出；同一患者两终点共用退出。",
    jr_dco_values="固定共同研究DCO列表，正、换算后最长3650日，例如24,36月。列表各DCO属于独立情景，不把不同情景当同患者配对。",
    jr_pfs_targets="PFS累计已观察事件目标列表，1–100000整数，只有需要PFS目标的规则读取。",
    jr_os_targets="OS累计死亡事件目标列表，1–100000整数，只有需要OS目标的规则读取；大于N无法达标，按最大窗口处理。",
    jr_primary="预定联合主规则。共同主要终点只在两者均通过alpha时同时声明；加权Bonferroni/Holm/固定顺序分别处理可分离终点声明；未调整规则仅描述参照。",
    jr_success_goal="主成功指标是any、both、PFS或OS声明。共同主要终点固定both；改变指标不改变多重性规则或单终点p。",
    jr_alpha="一次Final统一alpha，0.0001–0.2，例如单侧0.025；两终点统一检验方向。不是每次组序贯alpha，不实现重复分析。",
    jr_sided="单侧Treatment获益或双侧差异，两个终点统一；负score/logHR和正RMST/生存率差对应获益。双侧声明不代表一定获益。",
    jr_weight="加权Bonferroni中PFS的alpha份额w，0.01–0.99；OS为1−w，默认0.5。运行前预定，不按模拟p调整。",
    jr_first="固定顺序的首终点，预定PFS或OS。首终点不通过时不声明后终点；顺序不是按本轮p排序。",
    jr_compare="选择预定附加联合规则，共用本轮两终点p与DCO，作同轮配对模拟比较；不以任一策略成功重写主成功。",
    jr_extra="附加规则列表；各规则仍沿用所选主成功指标。共同主要终点本身只有两者同时声明；未调整结果不提供FWER保护。",
    jr_reps="每情景重复数20–10000；最多10万轮、3000万患者×轮次、50万规则×轮次。普通固定B MCSE不能作为可选停止校正。",
    jr_seed="主种子0–2147483647整数，按SCENARIO/SIMID派生并固定RNG类型；失败不重新编号。相同键跨研究可能共享种子，独立性仍需解释。",
    jr_view_scenario="查看最近提交的情景，只改变明细和样例，不运行新研究。",
    jr_reference_times="给定已保存模型的1–12个风险年龄时点，正且最长3650日，按当前参考输入单位。用于计算边际生存率和RMST，不使用入组、退出或模拟真值；该积分仍待验证。",
    jr_view_trial="选择所选情景已生成的轮次，按原配置/种子恢复联合观察、状态和真值，实际重放尚未验证。"))
  for(e in c("pfs","os")){
    h[paste0("jr_",e,"_method")]<-"预定该终点的log-rank、Cox Wald、FH、RMST差或生存率差，统一方向。转移PH不能保证边际PH；Cox普通区间不作单一HR真值覆盖或多重性调整。"
    h[paste0("jr_",e,"_tau")]<-"该终点RMST积分上限或生存率时点，按个体风险年龄填写，正、最长3650日。两组观察不支持时不外推，保留未知决策。"
    h[paste0("jr_",e,"_rho")]<-"该终点FH rho，0–5，仅FH读取，权重S(t−)^rho[1−S(t−)]^gamma。预定值，不看结果后调整。"
    h[paste0("jr_",e,"_gamma")]<-"该终点FH gamma，0–5，仅FH读取；如rho=0,gamma=1增加较晚事件权重，零有效方差保留无效。"
  }
  assign("parameter_help",h,envir=parent.frame())
}
jr_endpoint_ui <- function(e) {
  pre<-paste0("jr_",tolower(e),"_");m<-paste0("input.",pre,"method")
  section(paste0(e,"预定分析"),selectInput(paste0(pre,"method"),"分析方法",setNames(names(batch_method_names),unname(batch_method_names))),
    conditionalPanel(paste0(m," === 'fh'"),fields(numericInput(paste0(pre,"rho"),"FH rho",0,min=0,max=5),numericInput(paste0(pre,"gamma"),"FH gamma",1,min=0,max=5))),
    conditionalPanel(paste0(m," === 'rmst' || ",m," === 'survival'"),numericInput(paste0(pre,"tau"),"预定随访年龄tau（月）",12,min=.001)))
}
joint_research_ui <- function() {
  nav_panel("联合终点设计研究",value="joint_research",
    div(class="page-title",h2("PFS/OS联合终点设计研究"),actionButton("jr_run","运行联合研究",class="btn-primary"),actionButton("jr_cancel","取消后台研究")),
    p(class="field-note","两组、固定真值、共同一次Final。联合终点规则和生成/检验代码均待复核。"),
    section("参数入口",radioButtons("jr_run_source","参数来源",c("填写页面参数"="ui","导入无损配置JSON"="config_json"),inline=TRUE),textInput("jr_run_name","研究名称","PFS-OS联合设计"),
      conditionalPanel("input.jr_run_source === 'config_json'",fileInput("jr_config_file","联合研究无损配置JSON",accept=".json"),uiOutput("jr_import_note"))),
    navset_card_tab(id="jr_tabs",
      nav_panel("研究与转移",value="jr_baseline",
        section("共同研究参数",fields(selectInput("jr_unit","时间单位",c("日"="days","周"="weeks","月"="months"),"months"),dateInput("jr_origin","研究起点","2025-01-01")),numericInput("jr_allocation","Treatment分配概率",.5,min=.01,max=.99),selectInput("jr_clock","进展后死亡计时",c("进展后时间"="reset","自入组时间"="forward"))),
        section("总体入组与共同退出",selectInput("jr_enroll_mode","总体入组",c("恒定Poisson"="constant","分段Poisson"="piecewise")),
          conditionalPanel("input.jr_enroll_mode === 'constant'",numericInput("jr_enroll_rate","总体入组率（人/月）",15,min=.000001)),
          conditionalPanel("input.jr_enroll_mode === 'piecewise'",fields(textInput("jr_enroll_cuts","入组切点（月）","3,6"),textInput("jr_enroll_rates","各段入组率（人/月）","9,18,12"))),
          fields(numericInput("jr_drop_control","Control共同退出率（每月）",.005,min=0),numericInput("jr_drop_treatment","Treatment共同退出率（每月）",.005,min=0))),
        joint_transition_ui("01",1,namespace="jr"),joint_transition_ui("02",2,namespace="jr"),joint_transition_ui("12",3,namespace="jr")),
      nav_panel("情景与截点",value="jr_scenarios",
        section("预定共同截点",selectInput("jr_cut_rule","共同Final规则",c("固定DCO"="fixed","PFS达标"="pfs","OS达标"="os","PFS和OS均达标"="both","PFS或OS任一先达标"="first")),conditionalPanel("input.jr_cut_rule !== 'fixed'",numericInput("jr_max","最大研究窗口（月）",48,min=.001),selectInput("jr_miss","窗口未达规则",c("关闭不声明"="no_reject","窗口末一次Final"="analyze")))),
        section("情景输入",selectInput("jr_scenario_source","情景来源",c("列表组合"="grid","情景CSV"="csv")),
          conditionalPanel("input.jr_scenario_source === 'csv'",fileInput("jr_file","情景CSV",accept=".csv"),conditionalPanel("input.jr_cut_rule === 'fixed'",selectInput("jr_csv_unit","文件DCO单位",c("日"="days","周"="weeks","月"="months"),"months"))),
          conditionalPanel("input.jr_scenario_source === 'grid'",textInput("jr_n_values","N列表","200,300"),textAreaInput("jr_profiles","转移效应：名称 | q01,q02,q12","joint_null | 1,1,1\nactive | 0.7,1,0.85",rows=4,width="100%"),fields(textInput("jr_enroll_scales","入组倍数列表","1"),textInput("jr_dropout_scales","共同退出倍数列表","1")),
            conditionalPanel("input.jr_cut_rule === 'fixed'",textInput("jr_dco_values","共同DCO列表（月）","36")),conditionalPanel("input.jr_cut_rule === 'pfs' || input.jr_cut_rule === 'both' || input.jr_cut_rule === 'first'",textInput("jr_pfs_targets","PFS事件目标列表","150")),conditionalPanel("input.jr_cut_rule === 'os' || input.jr_cut_rule === 'both' || input.jr_cut_rule === 'first'",textInput("jr_os_targets","OS事件目标列表","100"))),
          downloadButton("jr_template","当前情景模板CSV"),p(class="field-note","效应定义给出三转移q，不直接填写边际PFS/OS HR；全部组合最多60情景。"))),
      nav_panel("分析与声明",value="jr_analysis",jr_endpoint_ui("PFS"),jr_endpoint_ui("OS"),
        section("预定联合主规则",selectInput("jr_primary","主规则",setNames(names(jr_policy_names),unname(jr_policy_names))),conditionalPanel("input.jr_primary !== 'co_primary'",selectInput("jr_success_goal","主成功指标",c("至少一个终点声明"="any","两个终点均声明"="both","PFS声明"="PFS","OS声明"="OS"))),
          fields(numericInput("jr_alpha","一次Final alpha",.025,min=.0001,max=.2),selectInput("jr_sided","两终点检验方向",c("单侧Treatment获益"="benefit","双侧差异"="two"))),checkboxInput("jr_compare","同轮比较附加规则",FALSE),conditionalPanel("input.jr_compare",checkboxGroupInput("jr_extra","附加规则",setNames(names(jr_policy_names),unname(jr_policy_names)),selected=c("bonferroni","holm","fixed_sequence"))),
          conditionalPanel("input.jr_primary === 'bonferroni' || (input.jr_compare && input.jr_extra && input.jr_extra.indexOf('bonferroni') >= 0)",numericInput("jr_weight","PFS alpha权重",.5,min=.01,max=.99)),conditionalPanel("input.jr_primary === 'fixed_sequence' || (input.jr_compare && input.jr_extra && input.jr_extra.indexOf('fixed_sequence') >= 0)",selectInput("jr_first","预定首终点",c("PFS","OS")))),
        section("重复研究",fields(numericInput("jr_reps","每情景重复数",100,min=20,max=10000,step=20),numericInput("jr_seed","主种子",20261005,min=0,max=2147483647,step=1)))),
      nav_panel("结果",value="jr_results",uiOutput("jr_status"),section("主成功指标与完整分母",DTOutput("jr_overview",fill=FALSE)),section("各规则的终点/联合声明与已识别假阳性",DTOutput("jr_policy_overview",fill=FALSE)),section("同轮规则配对比较",DTOutput("jr_policy_pairs",fill=FALSE)),section("共同Final资源",DTOutput("jr_resources",fill=FALSE)),section("两终点统计量经验相关",DTOutput("jr_endpoint_correlations",fill=FALSE)),
        section("保存模型的边际分布参考",textInput("jr_reference_times","风险年龄时点（月）","6,12,18"),actionButton("jr_reference_run","计算边际生存率与RMST"),uiOutput("jr_reference_note"),DTOutput("jr_reference",fill=FALSE),downloadButton("jr_reference_download","边际参考CSV"),downloadButton("jr_reference_config","边际参考与配置JSON")),
        fields(selectInput("jr_view_scenario","查看情景",character()),selectInput("jr_view_trial","查看生成轮次",character())),section("所选情景的主轮次",DTOutput("jr_rows",fill=FALSE)),section("终点名义分析",DTOutput("jr_method_rows",fill=FALSE)),section("联合规则轮次",DTOutput("jr_policy_rows",fill=FALSE))),
      nav_panel("导出与研究库",value="jr_exports",p(class="field-note","研究共用本机研究库、续跑和追加子研究；点击研究库管理。原始时间以日导出。"),actionButton("jr_library","打开研究库"),
        div(class="export-bar",
          downloadButton("jr_scenarios_download","计划CSV"),downloadButton("jr_rows_download","轮次CSV"),downloadButton("jr_method_rows_download","终点分析CSV"),downloadButton("jr_policy_rows_download","规则轮次CSV"),downloadButton("jr_overview_download","主汇总CSV"),downloadButton("jr_policy_overview_download","规则汇总CSV"),downloadButton("jr_policy_pairs_download","配对CSV"),downloadButton("jr_resources_download","资源CSV"),downloadButton("jr_endpoint_correlations_download","相关CSV"),downloadButton("jr_config_download","配置JSON"),downloadButton("jr_result_download","结果RDS"),downloadButton("jr_report_download","报告Markdown"),downloadButton("jr_script_download","复现R脚本"),downloadButton("jr_observed_download","所选观察CSV"),downloadButton("jr_adtte_download","所选双终点ADTTE"),downloadButton("jr_truth_download","所选潜在真值CSV"),downloadButton("jr_states_download","所选状态CSV"),downloadButton("jr_intervals_download","所选转移区间CSV")))))
}
