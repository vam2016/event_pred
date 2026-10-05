parameter_help["task"] <- "按本次需求选择入口，每个入口只显示相关参数与结果。可以返回需求入口切换任务；已填参数保留，切换事件预测需求后需重新运行。"
workflow_home <- function() {
  task_card <- function(id,title,stage,inputs,outputs) div(class="task-card",
    span(class="task-stage",stage),h3(title),tags$dl(tags$dt("输入"),tags$dd(inputs),tags$dt("结果"),tags$dd(outputs)),
    actionButton(id,"进入",class="btn-primary"))
  nav_panel("需求入口",value="home",
    div(class="page-title",h2("选择本次需求")),p(class="field-note","选择一个入口，只填写该任务显示的参数。数据文件仅在选择对应的资料输入方式时需要。"),
    div(style="display:none",selectInput("task","任务",c("未选择"="home","未来事件数"="count","达标日期"="target","设计模拟"="design","IA→Final"="conditional","设计评价"="study","事件数重估"="adaptive","历史IA推断"="history_inference","PFS/OS联合模拟"="joint","联合终点设计研究"="joint_research","独立队列重估研究"="adaptive_batch","实际IA条件批量"="conditional_batch","异质患者研究"="heterogeneity","共享患者重估"="shared_adaptation","多状态条件预测"="multistate","联合序贯与重估"="joint_sequential","柔性与盲态预测"="flexible_prediction","访视与报告过程"="observation","多臂与两阶段"="multiarm","设计搜索与独立确认"="design_search","期望事件反向校准"="reverse_calibration","IA期望事件反校准"="ia_reverse","Panel多状态模型"="panel_model","批量研究与参数校准"="batch"),"home")),
    div(class="task-grid",
      task_card("task_count","预测未来事件数","设计或研究运行中","参数或 ADTTE；事件模型、入组与脱落、预测窗口","事件数曲线、窗口末事件数与预测区间"),
      task_card("task_target","预测达标日期","设计或研究运行中","参数或 ADTTE；事件模型、未来过程、目标事件数 D*","达标日期分布、窗口内达标概率"),
      task_card("task_design","生成并分析生存数据","设计阶段 · 从研究起点生成","各组分布、入组、脱落、固定 DCO 或事件目标","模拟观察数据、KM、中位时间、固定时点生存率"),
      task_card("task_conditional","从 IA 继续模拟 Final","运行阶段 · 保留已有个体记录","IA 个体数据、拟合或指定分布、未来过程、Final 规则","生存估计变化，或两组未来拒绝概率与停止路径"),
      task_card("task_study","评价功效与候选设计","设计阶段 · 两组重复试验","对照分布、效应、N / D*、入组与脱落、固定或组序贯规则","功效 / Ⅰ类错误、估计恢复、停止资源与期中条件功效"),
      task_card("task_adaptive","重估两阶段事件数","设计或预定 IA · 正态模型 / 独立患者队列","原计划 D1 / Dplan、Final上限、固定权重规则、未来HR / CP","给定 IA 的事件数重估，或完整设计功效与Ⅰ类错误"),
      task_card("task_history_inference","分析历史 IA 的调整推断","运行阶段 · 已观察两组分析","完整 ADTTE 快照、截至哪次原分析、原组序贯设计","历史路径、重复p/HR区间、停止时调整p/HR区间"),
      task_card("task_joint","模拟同一患者的 PFS / OS","设计阶段 · 三状态转移模型","三个转移模型、单/两组、入组与退出、共同DCO规则","联合患者记录、两终点事件数/KM、中位时间及转移区间"),
      task_card("task_joint_research","评价PFS/OS联合终点设计","设计阶段 · 同轮两终点及多重性","转移模型、情景、共同Final、每终点方法及声明规则","联合/逐终点成功概率、规则配对比较、相关与资源"),
      task_card("task_adaptive_batch","批量评价独立队列事件数重估","设计阶段 · 预定两队列与组合检验","生存模型、事件计划、真实HR与过程情景、主/比较重估规则","拒绝与达标概率、配对差、重估原因和资源"),
      task_card("task_conditional_batch","批量预测实际IA后的拒绝与资源","运行阶段 · 冻结原IA与检验设计","既有患者或独立新队列的冻结IA参数、未来情景","条件拒绝概率、配对差、路径/资源与持久研究记录"),
      task_card("task_heterogeneity","模拟协变量、分层与中心差异","设计阶段 · 参数生成异质患者","生存模型、协变量、分层、中心/个体脆弱性与预定分析","方法比较、条件效应估计、资源及观察/真值数据"),
      task_card("task_shared_adaptation","重估共享患者的事件目标与推断","设计或实际IA · 已知风险 / 共同入组队列","原事件计划/混合检验、参数或两组IA风险集、未来情景","条件错误上界、适应后证据/区间、拒绝与资源"),
      task_card("task_multistate","拟合并续推实际多状态记录","运行阶段 · 精确状态或参数","IA状态、三转移模型、未来入组/退出、事件数或目标需求","按组转移拟合、PFS/OS事件数或达标日期分布"),
      task_card("task_joint_sequential","联合序贯检验与事件目标重估","设计或实际IA · 已指定风险","已知进展/状态无关死亡模型、原混合、联合规则及事件计划","联合声明、条件预测、重估路径、配对与资源"),
      task_card("task_flexible_prediction","拟合柔性、区间或盲态模型","运行阶段 · 参数或实际记录","区间/精确资料、模型/组合、外部盲态假设、预测需求","模型参考、盲态组成、事件数/达标日期与间隙补全"),
      task_card("task_observation","研究访视、退出与报告过程","设计阶段或已知报告积压","联合模型、访视/退出/送出机制，或已知临床积压清单","观察区间、临床/中央计数、工作模型/IPCW或发布预测"),
      task_card("task_multiarm","研究多臂与独立两阶段设计","设计阶段 · 共享对照/选臂/2-in-1","各臂风险、预定阶段/选择/组合与校正规则","有效臂声明、FWER、逐臂选择及资源"),
      task_card("task_design_search","搜索候选并独立确认","设计阶段 · 预定候选列表","多臂基准、真值情景、候选、功效/FWER门槛和成本","搜索资格、冻结选择、独立确认MC界"),
      task_card("task_reverse_calibration","反求期望事件设计参数","设计阶段 · 确定性期望","事件模型、有限均匀入组/退出、单一目标和求解区间","风险倍数/N/DCO、生存率校准、残差和期望曲线"),
      task_card("task_ia_reverse","基于IA反求未来参数","运行阶段 · 单变量/多目标期望校准","IA风险年龄或实际记录、基准模型、未来过程和目标/边界","风险倍数/N/退出率/DCO、偏差/整数方案/局部识别"),
      task_card("task_panel_model","拟合访视状态的多状态模型","运行阶段 · Panel Markov","访视状态CSV或常数转移率/状态人数；参考或期望计数","转移率、过滤状态、联合生存及模型期望事件数"),
      task_card("task_batch","批量研究与参数校准","设计阶段 · 情景比较 / assurance / 参数校准","基准分布与情景列表，或两个时点的生存率","成功概率与参照比较、统一报告；或Weibull等价参数")))
}
register_workflow_server <- function(input,output,session,result) {
  for(goal in c("count","target","design","conditional","study","adaptive","history_inference","joint","joint_research","adaptive_batch","conditional_batch","heterogeneity","shared_adaptation","multistate","joint_sequential","flexible_prediction","observation","multiarm","design_search","reverse_calibration","ia_reverse","panel_model","batch"))local({g<-goal
    observeEvent(input[[paste0("task_",g)]],{
      if(identical(isolate(input$task),g))nav_select("nav",switch(g,count="inputs",target="inputs",design="simulation",conditional="conditional",study="study",adaptive="adaptive",history_inference="history_inference",joint="joint",joint_research="joint_research",adaptive_batch="adaptive_batch",conditional_batch="conditional_batch",heterogeneity="heterogeneity",shared_adaptation="shared_adaptation",multistate="multistate",joint_sequential="joint_sequential",flexible_prediction="flexible_prediction",observation="observation",multiarm="multiarm",design_search="design_search",reverse_calibration="reverse_calibration",ia_reverse="ia_reverse",panel_model="panel_model",batch="batch"),session=session)
      else updateSelectInput(session,"task",selected=g)
    },ignoreInit=TRUE)
  })
  goal <- reactive(input$task %||% "home")
  output$task_goal <- renderText(goal());outputOptions(output,"task_goal",suspendWhenHidden=FALSE)
  output$task_heading <- renderUI(h2(if(goal()=="count")"预测未来事件数 · 参数" else "预测达标日期 · 参数"))
  old <- reactiveVal("home")
  observeEvent(goal(),{
    g<-goal();previous<-old()
    if(g %in% c("count","target") && !is.null(result()) && !identical(result()$config$task,g))result(NULL)
    for(id in c("inputs","forecast","data","validation","lab","posterior","simulation","conditional","study","adaptive","history_inference","joint","joint_research","adaptive_batch","conditional_batch","heterogeneity","shared_adaptation","multistate","joint_sequential","flexible_prediction","observation","multiarm","design_search","reverse_calibration","ia_reverse","panel_model","batch"))nav_hide("nav",id,session=session)
    if(g %in% c("count","target")) {
      for(id in c("inputs","forecast","data"))nav_show("nav",id,session=session)
      nav_select("nav","inputs",session=session)
    } else if(g=="design") {nav_show("nav","simulation",session=session);nav_select("nav","simulation",session=session)}
    else if(g=="conditional") {nav_show("nav","conditional",session=session);nav_select("nav","conditional",session=session)}
    else if(g=="study") {nav_show("nav","study",session=session);nav_select("nav","study",session=session)}
    else if(g=="adaptive") {nav_show("nav","adaptive",session=session);nav_select("nav","adaptive",session=session)}
    else if(g=="history_inference") {nav_show("nav","history_inference",session=session);nav_select("nav","history_inference",session=session)}
    else if(g=="joint") {nav_show("nav","joint",session=session);nav_select("nav","joint",session=session)}
    else if(g=="joint_research") {nav_show("nav","joint_research",session=session);nav_select("nav","joint_research",session=session)}
    else if(g=="adaptive_batch") {nav_show("nav","adaptive_batch",session=session);nav_select("nav","adaptive_batch",session=session)}
    else if(g=="conditional_batch") {nav_show("nav","conditional_batch",session=session);nav_select("nav","conditional_batch",session=session)}
    else if(g=="heterogeneity") {nav_show("nav","heterogeneity",session=session);nav_select("nav","heterogeneity",session=session)}
    else if(g=="shared_adaptation") {nav_show("nav","shared_adaptation",session=session);nav_select("nav","shared_adaptation",session=session)}
    else if(g=="multistate") {nav_show("nav","multistate",session=session);nav_select("nav","multistate",session=session)}
    else if(g=="joint_sequential") {nav_show("nav","joint_sequential",session=session);nav_select("nav","joint_sequential",session=session)}
    else if(g=="flexible_prediction") {nav_show("nav","flexible_prediction",session=session);nav_select("nav","flexible_prediction",session=session)}
    else if(g=="observation") {nav_show("nav","observation",session=session);nav_select("nav","observation",session=session)}
    else if(g=="multiarm") {nav_show("nav","multiarm",session=session);nav_select("nav","multiarm",session=session)}
    else if(g=="design_search") {nav_show("nav","design_search",session=session);nav_select("nav","design_search",session=session)}
    else if(g=="reverse_calibration") {nav_show("nav","reverse_calibration",session=session);nav_select("nav","reverse_calibration",session=session)}
    else if(g=="ia_reverse") {nav_show("nav","ia_reverse",session=session);nav_select("nav","ia_reverse",session=session)}
    else if(g=="panel_model") {nav_show("nav","panel_model",session=session);nav_select("nav","panel_model",session=session)}
    else if(g=="batch") {nav_show("nav","batch",session=session);nav_select("nav","batch",session=session)}
    else nav_select("nav","home",session=session)
    old(g)
  })
  observe({
    grouped<-identical(input$analysis_mode,"grouped");imode<-input$input_mode %||% "parameters"
    if(grouped)nav_show("input_tabs","groupparams",session=session) else nav_hide("input_tabs","groupparams",session=session)
    if(grouped && imode=="parameters")nav_hide("input_tabs","eventmodel",session=session) else nav_show("input_tabs","eventmodel",session=session)
    if(!grouped && imode=="parameters")nav_show("input_tabs","eventmodel",session=session)
  })
  observe({
    r<-result();g<-goal()
    for(id in c("validation","lab","posterior"))nav_hide("nav",id,session=session)
    if(!is.null(r)&&g %in% c("count","target")) {
      if(r$config$input_mode=="adtte")nav_show("aux_tabs","aux_backtest",session=session) else nav_hide("aux_tabs","aux_backtest",session=session)
      for(id in c("validation","lab"))nav_show("nav",id,session=session)
      if(any(vapply(r$models,function(m)!is.null(m$posterior),logical(1)))||r$config$process_uncertainty=="gamma")nav_show("nav","posterior",session=session)
    }
    if(!is.null(r)&&r$config$analysis_mode=="grouped"&&g=="count")nav_show("forecast_tabs","groupresults",session=session) else nav_hide("forecast_tabs","groupresults",session=session)
  })
}
