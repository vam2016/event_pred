# Three release tasks; advanced workflow registration stays in app.R.
parameter_help["task"] <- "选择本次需求：事件数、达标日期或生存数据生成。每个入口仅显示该任务的参数与结果。"
core_workflow_home <- function() {
  task_card <- function(id,title,inputs,results) div(class="task-card",h3(title),
    tags$dl(tags$dt("输入"),tags$dd(inputs),tags$dt("结果"),tags$dd(results)),actionButton(id,"进入",class="btn-primary"))
  nav_panel("需求入口",value="home",
    div(class="page-title",h2("选择本次任务")),
    p(class="field-note","预测可选择参数或 ADTTE 数据（CSV / XPT / SAS7BDAT）；数据模拟只需参数。"),
    div(style="display:none",selectInput("task","任务",c("未选择"="home","事件数"="count","达标日期"="target","生存数据"="design"),"home")),
    div(class="task-grid",
      task_card("task_count","预测未来事件数","当前队列、事件模型、入组/退出和预测窗口","新增及累计事件数、预测区间"),
      task_card("task_target","预测达标日期","当前队列、事件模型、未来计划、D* 和预测窗口","达标日期、窗口内达标概率"),
      task_card("task_design","生成生存数据","人数、分组、事件分布、入组/退出和固定 DCO","观察数据、真值、ADTTE、KM 与生存摘要")))
}
register_core_workflow_server <- function(input,output,session,result) {
  for(g in c("count","target","design")) local({ task <- g
    observeEvent(input[[paste0("task_",task)]],{
      if(identical(isolate(input$task),task))
        nav_select("nav",if(task=="design")"simulation" else "inputs",session=session)
      else updateSelectInput(session,"task",selected=task)
    },ignoreInit=TRUE)
  })
  goal <- reactive(input$task %||% "home")
  output$task_goal <- renderText(goal());outputOptions(output,"task_goal",suspendWhenHidden=FALSE)
  output$task_heading <- renderUI(h2(if(goal()=="count")"预测未来事件数" else "预测达标日期"))
  observeEvent(goal(),{
    g <- goal()
    if(g %in% c("count","target") && !is.null(result()) && !identical(result()$config$task,g)) result(NULL)
    visible <- if(g %in% c("count","target")) c("inputs","forecast","data") else if(g=="design") "simulation" else character()
    for(id in c("inputs","forecast","data","simulation")) {
      if(id %in% visible) nav_show("nav",id,session=session) else nav_hide("nav",id,session=session)
    }
    if(g %in% c("count","target")) {
      nav_select("nav","inputs",session=session)
    } else if(g=="design") nav_select("nav","simulation",session=session)
    else nav_select("nav","home",session=session)
  })
  observe({
    grouped <- identical(input$analysis_mode,"grouped")
    if(grouped)nav_show("input_tabs","groupparams",session=session) else nav_hide("input_tabs","groupparams",session=session)
    if(grouped && input$input_mode=="parameters")nav_hide("input_tabs","eventmodel",session=session) else nav_show("input_tabs","eventmodel",session=session)
  })
  observe({
    r <- result()
    if(!is.null(r) && r$config$analysis_mode=="grouped" && goal()=="count")nav_show("forecast_tabs","groupresults",session=session)
    else nav_hide("forecast_tabs","groupresults",session=session)
  })
}
