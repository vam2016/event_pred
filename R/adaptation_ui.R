register_adaptive_help <- function() {
  parameter_help <- get("parameter_help",envir=parent.frame())
  parameter_help[c("ad_purpose","ad_d1","ad_plan","ad_max","ad_p","ad_alpha","ad_hr","ad_cp","ad_rule","ad_cp_min","ad_z1","ad_true_hr","ad_reps","ad_seed")] <- c(
    "选择给定IA的事件数重估，或从起点重复两阶段试验。只使用canonical独立正态增量，不读取ADTTE。",
    "预先安排的第一次分析累计事件数D1，正整数且小于原定Final事件数。例：原定280、IA140。", 
    "运行前原定Final累计事件数Dplan。用于固定组合权重，也作为重估下限；看到IA后不改权重。例：280。",
    "重估允许的Final累计事件数上限Dmax，至少为原定Final；本版最多增加2000事件。例：420。",
    "Treatment分配概率p，0<p<1。信息近似为Dp(1-p)，1:1分配填0.5。",
    "整个两阶段设计的单侧获益alpha，0.0001–0.2。常用演算值0.025；本入口无IA提前效力停止。",
    "重估公式中的未来HR，0<HR<1，Treatment/Control。运行前给定，与模拟真值不同；例如0.75。",
    "希望重估后的条件功效CP达到的目标，大于0.5且小于1。例：0.9；事件上限可能使目标无法达到。",
    "上下限CP规则按目标CP求事件数；promising-zone规则若到上限仍低于最低CP则维持原计划。规则需预先选定。",
    "仅promising-zone使用：最大允许事件数仍达不到该CP时不增加事件。0<最低CP<目标CP；例0.8。不是无效停止界。",
    "在预定D1时观察到的标准化Z1，正值表示Treatment获益。必须对应canonical阶段1得分，范围−12至12。例：1.5；不能把任意估计HR直接作为Z。",
    "从研究起点生成两个阶段得分（H0下条件独立）的真实HR，>0且≤5。自动另模拟HR=1核对Ⅰ类错误；HR>1是反方向拒绝率。",
    "每个HR情景的完整试验重复数1000–200000。默认50000；alpha=0.025时MCSE约0.0007。仅反映Monte Carlo误差。",
    "非负整数随机种子，最大2147483647。保存配置和种子可在同一R环境复现逐轮结果。")
  assign("parameter_help",parameter_help,envir=parent.frame())
}
adaptation_ui <- function() {
  nav_panel("事件数重估",value="adaptive",
    div(class="page-title",h2("两阶段事件数重估"),actionButton("ad_run","计算",class="btn-primary")),
    p(class="field-note","固定原计划逆正态权重、单侧获益Final；IA不提前拒绝。选择阶段正态模型或两个独立患者队列。"),
    navset_card_tab(id="ad_tabs",
      nav_panel("需求与参数",value="parameters",
        selectInput("ad_engine","阶段资料",c("Canonical阶段正态模型"="canonical","两个独立患者队列"="patient")),
        conditionalPanel("input.ad_engine !== 'patient'",selectInput("ad_purpose","本次需求",c("给定 IA 的事件数重估"="interim","从研究起点评价重估设计"="simulation"))),
        section("原计划与事件上限",fields(numericInput("ad_d1","预定 IA 事件数 D1",140,min=1,step=1),numericInput("ad_plan","原定 Final 事件数 Dplan",280,min=2,step=1)),
          fields(numericInput("ad_max","Final 事件数上限 Dmax",420,min=2,step=1),numericInput("ad_p","Treatment 分配比例 p",.5,min=.01,max=.99,step=.05)),
          numericInput("ad_alpha","单侧获益 alpha",.025,min=.0001,max=.2,step=.005)),
        section("重估规则",fields(numericInput("ad_hr","重估采用的未来 HR",.75,min=.01,max=.99,step=.05),numericInput("ad_cp","目标条件功效 CP",.9,min=.51,max=.999,step=.05)),
          selectInput("ad_rule","事件数规则",c("上下限 CP 规则"="bounded","Promising-zone 规则"="promising"),"promising"),
          conditionalPanel("input.ad_rule === 'promising'",numericInput("ad_cp_min","Promising-zone 最低 CP",.8,min=.01,max=.99,step=.05))),
        conditionalPanel("input.ad_engine !== 'patient' && input.ad_purpose === 'interim'",section("当前 IA",numericInput("ad_z1","IA 获益方向 Z1",1.5,min=-12,max=12,step=.1))),
        conditionalPanel("input.ad_engine !== 'patient' && input.ad_purpose === 'simulation'",section("重复设计评价",numericInput("ad_true_hr","模拟真实 HR",.67,min=.01,max=5,step=.05),
          fields(numericInput("ad_reps","每情景完整试验重复数 B",50000,min=1000,max=200000,step=1000),numericInput("ad_seed","随机种子",20261004,min=0,step=1)))),patient_adaptive_inputs()),
      nav_panel("计算结果",value="results",conditionalPanel("input.ad_engine !== 'patient'",uiOutput("ad_status"),uiOutput("ad_plan_note")),
        patient_adaptive_results_ui(),
        conditionalPanel("input.ad_engine !== 'patient' && output.ad_result_purpose === 'interim'",section("IA 重估结果",DTOutput("ad_decision",fill=FALSE)),section("Final 事件数随 IA Z1 变化",plotlyOutput("ad_curve",height="380px"))),
        conditionalPanel("input.ad_engine !== 'patient' && output.ad_result_purpose === 'simulation'",section("完整试验拒绝率与事件数",DTOutput("ad_summary",fill=FALSE)),p(class="field-note","每个情景同时比较重估设计和原计划；两者使用同一阶段1及阶段2标准正态随机数。Ⅰ类错误仅适用于此处的独立正态阶段模型。"),section("事件数分布",DTOutput("ad_events",fill=FALSE)),section("逐轮阶段与决策",DTOutput("ad_trials",fill=FALSE)))),
      nav_panel("导出",value="exports",patient_adaptive_exports_ui(),conditionalPanel("input.ad_engine !== 'patient'",div(class="export-bar",downloadButton("ad_config_download","配置 JSON"),downloadButton("ad_result_download","结果 CSV"),downloadButton("ad_report_download","说明 Markdown"),
        conditionalPanel("input.ad_engine !== 'patient' && output.ad_result_purpose === 'simulation'",downloadButton("ad_trials_download","逐轮 CSV"),downloadButton("ad_script_download","复现 R 脚本")))))))
}
