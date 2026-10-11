parameter_help <- c(parameter_help,
  cv_cuts="1–6个不同正截点，早于原预测DCO；使用原预测冻结的日/周/月，不随后续主参数单位编辑换算。训练只保留截点前入组患者，将未来事件/退出截断为仍随访。",
  cv_values="1–5个不同累计风险倍数，0.1–5；自动保留基准1。改变累计风险，不直接把中位生存时间乘以倍数。各情景独立种子，MCSE按有效次数和独立方差和计算。")
for(id in c("pooled","g1","g2")) {
  parameter_help[paste0("cv_",id,"_n")] <- "逐截点的当时历史计划剩余人数，0–1000整数，数量与截点一致。不从最终患者文件或当前剩余人数推断。"
  parameter_help[paste0("cv_",id,"_rate")] <- "逐截点当时的恒定入组率，非负且单位使用原预测冻结单位；人数大于0时率必须大于0。数量与截点一致。"
}
# Auxiliary analyses always start from the last successful frozen prediction.
core_validation_seed <- function(seed, index) as.integer((as.double(seed) + 104729 * index) %% (as.double(.Machine$integer.max)+1))
core_scenarios <- function(data, cfg, values, models_override=NULL, progress=function(v,d)NULL) {
  if(!length(values)||length(values)>5||any(!is.finite(values)|values<.1|values>5)||anyDuplicated(values))
    stop("请填写1–5个不同的风险倍数，范围0.1–5；基准1自动保留。")
  values <- c(1,values[values!=1]); cfg <- complete_config(cfg)
  rows <- list(); forecasts <- list(); configurations <- list(); diagnostics <- list(); sf <- list(); failures <- list()
  for(j in seq_along(values)) {
    c2 <- cfg; c2$seed <- core_validation_seed(cfg$seed,j-1L); c2$multiplier <- cfg$multiplier*values[j]
    if(identical(cfg$analysis_mode,"grouped")) for(id in names(c2$groups)) c2$groups[[id]]$multiplier <- cfg$groups[[id]]$multiplier*values[j]
    key <- as.character(values[j]); configurations[[key]] <- c2
    r <- tryCatch(run_forecast(data,c2,function(v,d)progress((j-1+v)/length(values),d),models_override),error=identity)
    if(inherits(r,"error")) {
      failures[[length(failures)+1L]] <- data.frame(multiplier=values[j],method="all",message=conditionMessage(r))
      if(j==1L)stop(paste("基准情景失败：",conditionMessage(r)))
      next
    }
    forecasts[[key]] <- r; configurations[[key]] <- r$config
    if(length(r$failures)) failures[[length(failures)+1L]] <- data.frame(multiplier=values[j],method=names(r$failures),message=unname(r$failures))
    dg <- r$diagnostics; dg$multiplier <- values[j]; diagnostics[[key]] <- dg
    f <- r$simulation_failures; f$multiplier <- rep(values[j],nrow(f)); sf[[key]] <- f
    for(m in names(r$counts)) {
      z <- r$summary[r$summary$method==m,,drop=FALSE]; v <- r$counts[[m]][,ncol(r$counts[[m]])]
      base <- forecasts[[1]]; bv <- base$counts[[m]]
      z$multiplier <- values[j]; z$seed <- c2$seed
      z$mean_events_end <- mean(v); z$count_mcse <- stats::sd(v)/sqrt(length(v))
      q <- quantile_with_inf(v); z$lower_events_end <- q[1]; z$upper_events_end <- q[3]
      if(is.null(bv)) {z$count_difference <- NA_real_; z$count_difference_mcse <- NA_real_; z$probability_difference <- NA_real_; z$probability_difference_mcse <- NA_real_}
      else {
        b <- base$summary[base$summary$method==m,,drop=FALSE]; bv <- bv[,ncol(bv)]
        z$count_difference <- mean(v)-mean(bv); z$probability_difference <- z$reached-b$reached
        z$count_difference_mcse <- if(j==1L)0 else sqrt(stats::var(v)/length(v)+stats::var(bv)/length(bv))
        z$probability_difference_mcse <- if(j==1L)0 else sqrt(z$reached_mcse^2+b$reached_mcse^2)
      }
      rows[[length(rows)+1L]] <- z
    }
  }
  list(summary=do.call(rbind,rows),config=cfg,values=values,configurations=configurations,forecasts=forecasts,
    diagnostics=do.call(rbind,diagnostics),simulation_failures=do.call(rbind,sf),
    failures=if(length(failures))do.call(rbind,failures) else data.frame(multiplier=numeric(),method=character(),message=character()),
    pairing="independent_seeds",note="风险倍数作用于累计风险；两组按各自基准风险同比例调整。各情景使用不同随机种子，差值MCSE按独立模拟计算；不提供配对MCSE。基准减自身的差及MCSE为0。超过窗口的达标时间在表格中为空；导出CSV保留Inf，JSON为null。区间含义沿用原预测；有限模拟MCSE不代表参数或模型不确定性。")
}
core_validation_summary <- function(x,kind) {
  common <- if(kind=="backtest")c("cut","end","method","model","planned_future_n","historical_enroll_rate_per_day","enrollment_source") else c("multiplier","seed","method","model")
  fields <- if(kind=="backtest") {
    if(x$config$task=="count")c("observed_events","predicted_events","error","count_lower","count_upper","count_covered","count_interval_width") else
      c("reached_probability","probability_mcse","target_reached","brier","actual_target_day","predicted_target_day","target_error","truncated_target_covered")
  } else if(x$config$task=="count")c("mean_events_end","count_mcse","lower_events_end","median_events_end","upper_events_end","count_difference","count_difference_mcse") else
    c("reached","reached_mcse","probability_difference","probability_difference_mcse","lower_day","median_day","upper_day")
  x$summary[,c(common,fields,"requested","simulations","failed"),drop=FALSE]
}
core_validation_export <- function(x,kind) {
  d <- core_validation_summary(x,kind)
  cols <- intersect(c("cut","end","actual_target_day","predicted_target_day","target_error","lower_day","median_day","upper_day"),names(d))
  d <- unit_export(d,x$config$display_unit,cols)
  if("historical_enroll_rate_per_day"%in%names(d)) d[[paste0("historical_enroll_rate_per_",x$config$display_unit)]] <- d$historical_enroll_rate_per_day*time_factor(x$config$display_unit)
  d
}
core_validation_plan_export <- function(x) {
  d <- unit_export(x$plan,x$config$display_unit,"cut")
  names(d)[names(d)=="enroll_rate"] <- "enroll_rate_per_day"
  d[[paste0("enroll_rate_per_",x$config$display_unit)]] <- x$plan$enroll_rate*time_factor(x$config$display_unit)
  d
}
core_validation_table <- function(x,kind) {
  d <- core_validation_summary(x,kind); u <- time_label(x$config$display_unit)
  times <- intersect(c("cut","end","actual_target_day","predicted_target_day","target_error","lower_day","median_day","upper_day"),names(d))
  for(n in times)d[[n]] <- d[[n]]/time_factor(x$config$display_unit)
  if("historical_enroll_rate_per_day"%in%names(d))d$historical_enroll_rate_per_day <- d$historical_enroll_rate_per_day*time_factor(x$config$display_unit)
  labels <- c(cut=paste0("历史截点（",u,"）"),end=paste0("评价窗口末（",u,"）"),method="方法",model="模型",planned_future_n="历史计划剩余人数",historical_enroll_rate_per_day=paste0("历史入组率（人/",u,"）"),enrollment_source="入组依据",observed_events="实际累计事件数",predicted_events="预测中位事件数",error="预测减实际",count_lower="2.5%事件数",count_upper="97.5%事件数",count_covered="本次事件数区间含实际",count_interval_width="事件数区间宽度",reached_probability="窗口内达标概率",probability_mcse="概率MCSE",target_reached="实际窗口内达标",brier="Brier分数",actual_target_day=paste0("实际达标研究时间（",u,"）"),predicted_target_day=paste0("预测达标中位研究时间（",u,"）"),target_error=paste0("达标时间误差（",u,"）"),truncated_target_covered="窗口截断日期区间含实际",multiplier="累计风险倍数",seed="随机种子",mean_events_end="窗口末平均事件数",count_mcse="均值MCSE",lower_events_end="2.5%事件数",median_events_end="中位事件数",upper_events_end="97.5%事件数",count_difference="平均事件数减基准",count_difference_mcse="独立差值MCSE",reached="窗口内达标概率",reached_mcse="概率MCSE",probability_difference="达标概率减基准",probability_difference_mcse="独立概率差MCSE",lower_day=paste0("2.5%达标研究时间（",u,"）"),median_day=paste0("达标中位研究时间（",u,"）"),upper_day=paste0("97.5%达标研究时间（",u,"）"),requested="请求次数",simulations="有效次数",failed="失败次数")
  for(n in names(d)[vapply(d,is.numeric,logical(1))])d[[n]][is.infinite(d[[n]])] <- NA_real_
  names(d) <- unname(labels[names(d)]); core_result_table(d)
}
core_validation_bundle <- function(x,kind) list(version="2.0.0-dev",kind=kind,created_at=x$created_at,base_created_at=x$base_created_at,
  config=x$config,configurations=x$configurations,cuts=x$cuts,truth_cut=x$truth_cut,plan=x$plan,values=x$values,pairing=x$pairing,
  summary=core_validation_export(x,kind),diagnostics=x$diagnostics,simulation_failures=x$simulation_failures,failures=x$failures,note=x$note)
core_validation_ui <- function() nav_panel("回测与情景",value="validation",
  uiOutput("cv_context"),navset_card_tab(id="cv_tabs",
    nav_panel("历史回测",value="backtest",p(class="field-note","仅用于最近成功的ADTTE预测。逐截点重新拟合，必须填写当时的计划，不从最终资料反推剩余人数。"),
      textInput("cv_cuts","历史截点（使用原预测单位，逗号分隔）",""),uiOutput("cv_plan_inputs"),actionButton("cv_backtest_run","运行历史回测",class="btn-primary"),uiOutput("cv_backtest_status"),DTOutput("cv_backtest_table",fill=FALSE),plotlyOutput("cv_backtest_plot",height="360px"),uiOutput("cv_backtest_note"),core_validation_downloads_ui("backtest")),
    nav_panel("风险情景",value="scenario",textInput("cv_values","累计风险倍数（1–5个，基准1自动保留）","0.75,1.25"),
      p(class="field-note","范围0.1–5。固定原始队列、入组与退出计划，只调整事件累计风险；独立随机种子，不提供配对MCSE。"),actionButton("cv_scenario_run","运行风险情景",class="btn-primary"),uiOutput("cv_scenario_status"),DTOutput("cv_scenario_table",fill=FALSE),uiOutput("cv_scenario_note"),core_validation_downloads_ui("scenario"))))
core_validation_downloads_ui <- function(kind) div(class="export-bar",
  if(kind=="backtest")downloadButton("cv_backtest_plan","历史计划CSV"),
  downloadButton(paste0("cv_",kind,"_candidate_failures"),"候选模型失败CSV"),downloadButton(paste0("cv_",kind,"_summary"),"汇总CSV"),downloadButton(paste0("cv_",kind,"_diagnostics"),"计算次数CSV"),downloadButton(paste0("cv_",kind,"_failures"),"失败记录CSV"),downloadButton(paste0("cv_",kind,"_config"),"配置JSON"),downloadButton(paste0("cv_",kind,"_input"),"逐人复现输入RDS"),downloadButton(paste0("cv_",kind,"_script"),"复现R脚本"))
register_core_validation_server <- function(input,output,session,result) {
  backtest <- shiny::reactiveVal(NULL); scenario <- shiny::reactiveVal(NULL)
  errors <- list(backtest=shiny::reactiveVal(NULL),scenario=shiny::reactiveVal(NULL))
  shiny::observeEvent(result(),{backtest(NULL);scenario(NULL);errors$backtest(NULL);errors$scenario(NULL);shiny::updateTextInput(session,"cv_cuts",value="")},ignoreNULL=FALSE)
  output$cv_context <- shiny::renderUI({r<-result();if(is.null(r))return(p("先完成一次预测。"));p(class="context-strip",paste("冻结预测：",r$created_at,"；时间单位：",time_label(r$config$display_unit),"；当前DCO：",r$config$cut/time_factor(r$config$display_unit),"；辅助分析不读取随后修改的主参数。"))})
  output$cv_plan_inputs <- shiny::renderUI({r<-result();shiny::req(r);ids<-if(identical(r$config$analysis_mode,"grouped"))names(r$config$groups) else "pooled";tagList(lapply(ids,function(id){label<-if(id=="pooled")"合并" else r$config$groups[[id]]$name;fields(textInput(paste0("cv_",id,"_n"),paste(label,"各截点历史剩余人数"),""),textInput(paste0("cv_",id,"_rate"),paste0(label," 各截点历史恒定入组率（人/",time_label(r$config$display_unit),"）"),""))}))})
  run <- function(kind) {
    old <- if(kind=="backtest")backtest else scenario
    tryCatch({r<-result();if(is.null(r))stop("先完成一次预测。");cfg<-r$config
      x <- shiny::withProgress(message=if(kind=="backtest")"历史回测" else "风险情景",value=0,{
        progress<-function(v,d)shiny::setProgress(value=v,detail=d)
        if(kind=="backtest") {
          if(cfg$input_mode!="adtte")stop("历史回测需要最近成功的ADTTE预测。")
          cuts<-parse_unit_numbers(input$cv_cuts)*time_factor(cfg$display_unit)
          ids<-if(identical(cfg$analysis_mode,"grouped"))names(cfg$groups) else "pooled"
          plan<-do.call(rbind,lapply(ids,function(id){n<-parse_unit_numbers(input[[paste0("cv_",id,"_n")]]);rate<-parse_unit_numbers(input[[paste0("cv_",id,"_rate")]])/time_factor(cfg$display_unit)
            if(length(n)!=length(cuts)||length(rate)!=length(cuts)||!length(cuts))stop("每组历史人数和入组率需与截点逐项对应。")
            z<-data.frame(cut=cuts,future_n=n,enroll_rate=rate);if(id!="pooled")z$group<-cfg$groups[[id]]$name;z}))
          backtest_forecast(r$observed_data,cfg,cuts,cfg$cut,progress,plan)
        } else core_scenarios(r$observed_data,cfg,parse_unit_numbers(input$cv_values),r$models_override,progress)
      })
      x$created_at<-format(Sys.time(),"%Y-%m-%d %H:%M:%OS3 UTC",tz="UTC");x$base_created_at<-r$created_at
      x$replay_input<-list(kind=kind,data=r$observed_data,config=cfg,cuts=x$cuts,truth_cut=x$truth_cut,plan=x$plan,values=x$values,models_override=r$models_override)
      old(x);errors[[kind]](NULL)
    },error=function(e)errors[[kind]](conditionMessage(e)))
  }
  shiny::observeEvent(input$cv_backtest_run,run("backtest"));shiny::observeEvent(input$cv_scenario_run,run("scenario"))
  for(kind in c("backtest","scenario"))local({k<-kind;rv<-if(k=="backtest")backtest else scenario;pre<-paste0("cv_",k,"_")
    output[[paste0(pre,"status")]]<-shiny::renderUI({err<-errors[[k]]();x<-rv();tagList(if(!is.null(err))div(class="alert alert-danger",err),if(!is.null(x))p(paste("成功运行：",x$created_at,"；保留请求、有效和失败次数。")))})
    output[[paste0(pre,"table")]]<-DT::renderDT({x<-rv();shiny::req(x);core_validation_table(x,k)})
    output[[paste0(pre,"note")]]<-shiny::renderUI({x<-rv();shiny::req(x);tagList(p(x$note),p(forecast_interval_note(x$config)),if(k=="backtest")p("实际值用最终文件在评价窗口内的事件，仅用于评分；训练只含历史截点前入组且截至该截点可观察的信息。事件数区间含实际和窗口截断日期覆盖均为本次描述，不是覆盖率校准。Brier为窗口内是否达标的概率误差；超过窗口的达标研究时间显示为空，时间误差仅在实际与预测均有限时计算。"),if(nrow(x$failures))core_result_table(x$failures))})
    output[[paste0(pre,"summary")]]<-shiny::downloadHandler(filename=paste0(k,"_summary.csv"),content=function(file){x<-rv();shiny::req(x);write.csv(core_validation_export(x,k),file,row.names=FALSE)})
    output[[paste0(pre,"candidate_failures")]]<-shiny::downloadHandler(filename=paste0(k,"_candidate_failures.csv"),content=function(file){x<-rv();shiny::req(x);write.csv(x$failures,file,row.names=FALSE)})
    output[[paste0(pre,"diagnostics")]]<-shiny::downloadHandler(filename=paste0(k,"_diagnostics.csv"),content=function(file){x<-rv();shiny::req(x);write.csv(x$diagnostics,file,row.names=FALSE)})
    output[[paste0(pre,"failures")]]<-shiny::downloadHandler(filename=paste0(k,"_failures.csv"),content=function(file){x<-rv();shiny::req(x);write.csv(x$simulation_failures,file,row.names=FALSE)})
    output[[paste0(pre,"config")]]<-shiny::downloadHandler(filename=paste0(k,"_config.json"),content=function(file){x<-rv();shiny::req(x);jsonlite::write_json(core_validation_bundle(x,k),file,auto_unbox=TRUE,pretty=TRUE,digits=NA,na="null",null="null")})
    output[[paste0(pre,"input")]]<-shiny::downloadHandler(filename=paste0(k,"_input.rds"),content=function(file){x<-rv();shiny::req(x);saveRDS(x$replay_input,file)})
    output[[paste0(pre,"script")]]<-shiny::downloadHandler(filename=paste0(k,"_replay.R"),content=function(file){shiny::req(rv());writeLines(c('# 在同版本源码根目录执行；将对应复现输入RDS放在当前目录。','e <- new.env(parent=globalenv()); invisible(sys.source("app_core.R", envir=e))',paste0('x <- readRDS("',k,'_input.rds")'),if(k=="backtest")'r <- e$backtest_forecast(x$data,x$config,x$cuts,x$truth_cut,plan=x$plan)' else 'r <- e$core_scenarios(x$data,x$config,x$values,x$models_override)',paste0('write.csv(e$core_validation_export(r,"',k,'"),"',k,'_replayed_summary.csv",row.names=FALSE)')),file,useBytes=TRUE)})
  })
  output$cv_backtest_plan<-shiny::downloadHandler(filename="backtest_plan.csv",content=function(file){x<-backtest();shiny::req(x);write.csv(core_validation_plan_export(x),file,row.names=FALSE)})
  output$cv_backtest_plot<-plotly::renderPlotly({x<-backtest();if(is.null(x))return(core_empty_plot("运行回测后显示预测与实际"));d<-x$curves;d$series<-paste(d$backtest_cut/time_factor(x$config$display_unit),time_label(x$config$display_unit),d$model)
    p<-plotly::plot_ly();for(s in unique(d$series)){z<-d[d$series==s,];p<-plotly::add_lines(p,x=z$day/time_factor(x$config$display_unit),y=z$median,name=paste(s,"预测"));p<-plotly::add_lines(p,x=z$day/time_factor(x$config$display_unit),y=z$actual,name=paste(s,"实际"),line=list(dash="dot"))};core_plot_finish(p,paste0("研究时间（",time_label(x$config$display_unit),"）"),"累计事件数")})
  invisible(list(backtest=backtest,scenario=scenario))
}
