# Versioned synthetic examples, shared by the manual, JSON download and replay.
parameter_help["core_case"] <- "选择2.0合成教学案例。页面显示设置与核对要点；下载JSON和R脚本可在同版本源码目录复现。选择案例不会修改当前任务参数或结果。"
core_case_catalog <- function() c(log_time="对数时间：条件事件数",bootstrap="ADTTE：患者Bootstrap",validation="达标：历史回测与风险情景",event_simulation="生成数据：事件驱动截点")
core_case_spec <- function(id) {
  if(!id%in%names(core_case_catalog()))stop("未知核心案例。")
  cfg <- list(task="count",cut=100,horizon=80,sims=100,target=45,future_n=0,enroll_rate=0,
    dropout_rate=0,lag=0,multiplier=1,seed=6401,prior_shape=.5,prior_rate=50,ensemble=FALSE,tail_rate=.002,uncertainty="plugin",
    clock="occurred",methods="exponential",cuts=c(20,50),input_mode="adtte",
    origin="2025-01-01",display_unit="days",engine_unit="days")
  data <- data.frame(id=paste0("CASE",1:60),entry=0,time=c(round(seq(10,80,length.out=36)),rep(70,6),rep(100,18)),
    status=c(rep("event",36),rep("dropout",6),rep("active",18)))
  x <- list(schema="survcast-core-case-1",version=core_version,id=id,title=unname(core_case_catalog()[id]),
    synthetic=TRUE,config=cfg,data=data,model_specs=NULL,auxiliary=NULL)
  if(id=="log_time") {
    x$config$input_mode <- "parameters"; x$config$cut <- 20; x$config$horizon <- 80;x$config$methods <- "lognormal"
    x$data <- data.frame(id=paste0("CASE",1:10),entry=0,time=20,status="active")
    x$model_specs <- list(lognormal=list(params=list(median=50,scale=.8),cuts=numeric()))
    x$steps <- c("选择未来事件数、参数输入、合并，单位日，研究起点2025-01-01，DCO=20。",
      "当前仍随访10人、已记录事件0、相同随访年龄20日；Log-normal，中位时间50日、sigma=0.8。",
      "未来入组0、永久退出率0、上报延迟0、风险倍数1、窗口80日、次数100、种子6401；运行后核对累计/新增读数。")
    x$check <- "初始累计事件为0；新增与累计相同；条件事件概率为1-S(100)/S(20)，不是1-S(80)。固定参数区间不含参数估计不确定性。"
  } else if(id=="bootstrap") {
    x$config$methods <- c("exponential","weibull","pwe");x$config$uncertainty <- "bootstrap"
    x$steps <- c("下载案例ADTTE CSV，选择未来事件数、ADTTE、合并、日、DCO=100，OS，无分析标志筛选，CNSR退出编码2。",
      "选指数、Weibull、PWE；PWE随访切点20,50；患者Bootstrap；未来入组0、退出率0、延迟0、风险倍数1。",
      "窗口80日、次数100、种子6401；核对计算次数CSV及逐轮失败CSV。")
    x$check <- "60人：36事件、6永久退出、18仍随访；初始累计事件36。每个模型请求=有效+失败，失败不补抽。"
  } else if(id=="validation") {
    x$config$task <- "target";x$auxiliary <- list(cuts=c(40,60),plan=data.frame(cut=c(40,60),future_n=0,enroll_rate=0),values=c(.75,1.25))
    x$steps <- c("下载案例ADTTE CSV，选择达标日期；合并、日、DCO=100，OS，无分析标志筛选，退出编码2；仅指数、固定拟合参数。",
      "目标45、未来入组0、退出率0、延迟0、风险倍数1、窗口80日、次数100、种子6401；运行预测。",
      "回测与情景：历史截点40,60，历史剩余人数0,0，历史入组率0,0；风险倍数0.75,1.25，分别运行并核对CSV和图。")
    x$check <- "历史评价止于100日，实际仅36事件、未达45。风险基准1自动保留；独立情景MCSE与预测区间不同。"
  } else {
    x$data <- NULL
    x$config <- list(n=20,reps=5,seed=7301,origin="2025-01-01",paramcd="OS",display_unit="days",engine_unit="days",
      enroll_mode="constant",enroll_rate=2,enroll_cuts=numeric(),enroll_rates=numeric(),entry_days=numeric(),
      cut_mode="target",cuts=numeric(),target=10,max_day=20,fixed_times=c(4,6,9))
    x$model_specs <- list(A=list(method="exponential",params=list(median=log(2)/.05),cuts=numeric(),weight=1,dropout_rate=0))
    x$steps <- c("生成生存数据：日、研究起点2025-01-01、OS、20人、5试验、种子7301、单组A、权重1。",
      "A组指数，中位时间13.862943611198906日，退出率0；恒定入组率2人/日。",
      "事件驱动、总体目标10、最大研究窗口20日、固定随访时点4,6,9；核对实际DCO、达标比例、观察与真值导出。")
    x$check <- "每个试验保留一条实际DCO；未达标也按20日分析，达标概率分母为全部5个试验。潜在真值不进入KM。"
  }
  x
}
core_run_case <- function(x) {
  if(!identical(x$schema,"survcast-core-case-1")||!identical(x$version,core_version)||!isTRUE(x$synthetic))stop("案例版本或格式不匹配。")
  if(x$id=="event_simulation") {
    cfg <- x$config
    cfg$groups <- lapply(names(x$model_specs),function(name){m<-x$model_specs[[name]];list(name=name,weight=m$weight,dropout_rate=m$dropout_rate,model=parameter_model(m$method,m$params,m$cuts))})
    return(list(simulation=run_survival_simulation(cfg)))
  }
  validate_core_uncertainty(x$config)
  models <- if(is.null(x$model_specs))NULL else setNames(lapply(names(x$model_specs),function(id){m<-x$model_specs[[id]];parameter_model(id,m$params,m$cuts)}),names(x$model_specs))
  data <- validate_data(x$data,x$config$cut,require_events=x$config$input_mode!="parameters")
  out <- list(forecast=run_forecast(data,x$config,models_override=models))
  if(!is.null(x$auxiliary)) {
    out$backtest <- backtest_forecast(data,x$config,x$auxiliary$cuts,x$config$cut,plan=x$auxiliary$plan)
    out$scenario <- core_scenarios(data,x$config,x$auxiliary$values,models)
  }
  out
}
core_case_tables <- function(r) {
  if(!is.null(r$simulation))return(list(summary=r$simulation$summary,cuts=r$simulation$cuts,overview=simulation_target_overview(r$simulation)))
  out <- list(summary=r$forecast$summary,diagnostics=r$forecast$diagnostics)
  if(!is.null(r$backtest))out$backtest <- core_validation_export(r$backtest,"backtest")
  if(!is.null(r$scenario))out$scenario <- core_validation_export(r$scenario,"scenario")
  out
}
core_case_adtte <- function(x) {
  if(is.null(x$data)||x$config$input_mode!="adtte")stop("该案例不使用ADTTE。")
  d <- x$data; origin <- as.Date(x$config$origin)
  # These fixtures use whole days and the default inclusive-date AVAL convention.
  data.frame(STUDYID="SYNTHETIC-CASE",USUBJID=d$id,PARAMCD="OS",STARTDT=as.character(origin+d$entry),
    ADT=as.character(origin+floor(d$entry+d$time)),AVAL=d$time+1,AVALU="DAYS",CNSR=match(d$status,c("event","active","dropout"))-1L)
}
core_cases_ui <- function() section("2.0 合成案例",
  selectInput("core_case","选择案例",setNames(names(core_case_catalog()),core_case_catalog()),"log_time"),
  uiOutput("core_case_detail"),div(class="export-bar",downloadButton("core_case_json","案例JSON"),downloadButton("core_case_script","案例复现R脚本"),uiOutput("core_case_data_button")),
  p(class="field-note","下载的教学案例与当前任务独立。JSON保存案例输入；复现脚本读取该JSON，使用同版本源码。执行方法见手册第9章。"))
register_core_cases_server <- function(input,output,session) {
  spec <- shiny::reactive(core_case_spec(input$core_case %||% "log_time"))
  output$core_case_detail <- shiny::renderUI({x<-spec();tagList(tags$ol(lapply(x$steps,tags$li)),p(class="field-note",x$check))})
  output$core_case_data_button <- shiny::renderUI({x<-spec();if(!is.null(x$data)&&x$config$input_mode=="adtte")downloadButton("core_case_data","案例ADTTE CSV")})
  output$core_case_json <- shiny::downloadHandler(filename="survcast_case.json",content=function(file)jsonlite::write_json(spec(),file,auto_unbox=TRUE,pretty=TRUE,digits=NA,null="null",dataframe="rows"))
  output$core_case_data <- shiny::downloadHandler(filename="survcast_case_adtte.csv",content=function(file)write.csv(core_case_adtte(spec()),file,row.names=FALSE))
  output$core_case_script <- shiny::downloadHandler(filename="survcast_case_replay.R",content=function(file)writeLines(core_case_replay_script(),file,useBytes=TRUE))
}
core_case_replay_script <- function() c('# 在同版本源码根目录执行；将对应survcast_case.json放在当前目录。',
  'e <- new.env(parent=globalenv()); invisible(sys.source("app_core.R",envir=e))',
  'x <- jsonlite::read_json("survcast_case.json",simplifyVector=TRUE)',
  'r <- e$core_run_case(x)',
  'tables <- e$core_case_tables(r)',
  'for (id in names(tables)) write.csv(tables[[id]],paste0("survcast_case_",id,".csv"),row.names=FALSE)')
core_case_markdown <- function() {
  out <- c("# SurvCast 2.0 合成案例", "", "案例版本：2.0.0-dev；所有记录均为合成教学资料。案例设置来自R/core_cases.R，与页面和下载JSON同源。", "",
    "在在线工作手册顶部选择案例，下载案例JSON和案例复现R脚本；需要ADTTE时另下载案例CSV。把JSON与R脚本放在同版本源码根目录，执行 `Rscript survcast_case_replay.R`，输出以survcast_case_开头的结果CSV。四个案例请分别保存，文件名相同会覆盖前一个案例。", "",
    "复现脚本读取下载JSON中的参数及合成记录；不读取当前页面参数。需相同R和依赖环境。CSV可导入页面，ISO日期、AVAL单位日、日期加项1、退出编码2；手工设置其余参数后再核对结果。", "")
  for(id in names(core_case_catalog())) {
    x <- core_case_spec(id)
    out <- c(out,paste0("## ",x$title,"（",id,"）"),"",paste0(seq_along(x$steps),". ",x$steps),"",paste("核对：",x$check),"")
  }
  c(out,"案例只示范已验收的核心方法。它们不替代独立方法检查，也不证明临床假设、覆盖率或功效。")
}
