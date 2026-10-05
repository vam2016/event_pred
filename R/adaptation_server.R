adaptive_report <- function(r) {
  c<-r$config;a<-r$plan
  paste0('# 两阶段事件数重估\n\n版本 v0.30.0；PH信息近似的canonical两阶段正态模型（H0条件独立）。\n\n',
    '预定IA事件数 ',c$d1,'；原定Final ',c$d_plan,'；Final上限 ',c$d_max,'。\n\n',
    '固定权重 w1=',format(a$w1,digits=10),'、w2=',format(a$w2,digits=10),'；单侧alpha=',c$alpha,'；临界值=',format(a$critical,digits=10),'。\n\n',
    '重估未来HR=',c$hr_assumed,'；目标CP=',c$cp_target,'；规则=',c$rule,if(c$rule=='promising')paste0('；最低CP=',c$cp_min)else '',
    '。IA不提前拒绝或无效停止。原定Final作为重估下限；在事件上限处无法达到CP时明确标记。\n\n',
    if(c$purpose=='interim')paste0('固定IA Z1=',c$z1,'。该计算条件于当前IA，不能作为从起点的Ⅰ类错误或功效。\n\n')else paste0('每个HR情景从起点重复 ',c$reps,' 次；种子=',c$seed,'。自动包含HR=1；原计划为配对参照。参考拒绝率由一维正态积分计算，H0为名义alpha。\n\n'),
    '拒绝规则：w1 Z1 + w2 Z2 ≥ z(1-alpha)。Z2在H0下给定Z1仍为独立N(0,1)，权重保持原计划。\n\n',
    '此模型不读取患者、入组、脱落或日历时间。不能直接用累计log-rank统计量相减构造独立阶段；真实生存资料的自适应推断需单独验证阶段检验与依赖结构。方法推导见工作手册第20章。\n')
}
register_adaptive_server <- function(input,output,session,batch_bridge=NULL) {
  result<-reactiveVal(NULL);error<-reactiveVal(NULL)
  output$ad_result_purpose<-renderText(if(is.null(result()))"none" else result()$config$purpose)
  outputOptions(output,"ad_result_purpose",suspendWhenHidden=FALSE)
  config<-function() {
    c<-list(purpose=input$ad_purpose,d1=input$ad_d1,d_plan=input$ad_plan,d_max=input$ad_max,p=input$ad_p,alpha=input$ad_alpha,
      hr_assumed=input$ad_hr,cp_target=input$ad_cp,rule=input$ad_rule)
    if(c$rule=="promising")c$cp_min<-input$ad_cp_min
    if(c$purpose=="interim")c$z1<-input$ad_z1 else {c$hr_true<-input$ad_true_hr;c$reps<-input$ad_reps;c$seed<-input$ad_seed}
    c
  }
  observeEvent(input$ad_run,{
    if(identical(input$ad_engine,"patient"))return()
    error(NULL)
    tryCatch({c<-config();a<-adaptive_plan(c)
      if(c$purpose=="simulation")r<-run_adaptive_simulation(c) else {
        curve<-adaptive_decision(seq(-4,6,length.out=501),c)
        r<-list(config=c,plan=a,decision=adaptive_decision(c$z1,c),curve=curve,version="0.30.0")
      }
      result(r);nav_select("ad_tabs","results",session=session)
    },error=function(e){error(conditionMessage(e));nav_select("ad_tabs","results",session=session)})
  },ignoreInit=TRUE)
  output$ad_status<-renderUI({r<-result();if(!is.null(error()))return(div(class="alert alert-warning",paste0("计算失败：",error(),if(!is.null(r))" 保留最近成功结果。" else "")))
    if(is.null(r))p(class="field-note","填写当前需求参数后点击计算。") else p(class="field-note",if(r$config$purpose=="interim")"给定IA的重估结果；使用本次成功计算的参数。" else "从起点的完整两阶段模拟已完成；自动包含HR=1。")})
  output$ad_plan_note<-renderUI({r<-result();req(r);a<-r$plan
    p(sprintf("原计划固定权重：w1=%.6f，w2=%.6f；Final组合Z临界值=%.6f。第二阶段事件数范围：%d–%d。",a$w1,a$w2,a$critical,a$d2_min,a$d2_max))})
  table<-function(x) {
    labels<-c(z1="IA Z1",conditional_error="原条件错误",conditional_critical="阶段2临界值",cp_original="原计划CP",cp_max="上限CP",
      required_d2="未约束所需新增事件",selected_d2="选定新增事件",final_events="Final累计事件",cp_selected="选定CP",target_reached="达到目标CP",reason="规则状态",
      true_hr="真实HR",hypothesis="假设",design="设计",repetitions="重复数",rejections="拒绝次数",rejection_rate="拒绝率",mcse="MCSE",
      wilson_low="Wilson95%下限",wilson_high="Wilson95%上限",expected_reference="解析 / 积分参照",mean_events="平均事件数",increased_fraction="增加事件比例",
      target_unattained_fraction="目标CP未达比例",minimum="最小值",q025="2.5%分位数",median="中位数",q975="97.5%分位数",maximum="最大值",
      stage2_z="阶段2 Z",combination_z="组合Z",reject_adaptive="重估设计拒绝",original_combination_z="原计划组合Z",reject_original="原计划拒绝")
    reasons<-c(maximum_below_cp_min="上限CP低于最低CP，维持原计划",maximum_target_unattainable="达到事件上限，目标CP未达",increase_to_target="增加至目标CP",original_sufficient="原计划CP已足够")
    if("reason" %in% names(x))x$reason<-unname(reasons[x$reason])
    if("设计" %in% names(x))x$设计<-ifelse(x$设计=="adaptive","重估","原计划")
    if("design" %in% names(x))x$design<-ifelse(x$design=="adaptive","重估","原计划")
    if("规则状态" %in% names(x))x$规则状态<-unname(reasons[x$规则状态])
    new<-unname(labels[names(x)]);names(x)[!is.na(new)]<-new[!is.na(new)]
    decimal<-names(x)[vapply(x,function(v)is.numeric(v)&&any(abs(v-round(v))>1e-12,na.rm=TRUE),logical(1))]
    d<-DT::datatable(x,rownames=FALSE,class="compact nowrap",options=list(scrollX=TRUE,autoWidth=TRUE,pageLength=10,
      language=list(search="筛选：",lengthMenu="每页 _MENU_ 行",info="第 _START_–_END_ 行，共 _TOTAL_ 行",infoEmpty="无记录",zeroRecords="无匹配记录",paginate=list(previous="上一页",`next`="下一页"))))
    if(length(decimal))d<-DT::formatSignif(d,columns=decimal,digits=5)
    d
  }
  output$ad_decision<-renderDT({r<-result();req(r,r$config$purpose=="interim");x<-r$decision
    names(x)<-c("IA Z1","原条件错误","阶段2临界值","原计划CP","上限CP","未约束所需新增事件","选定新增事件","Final总事件","选定CP","达到目标CP","规则状态")
    table(x)})
  output$ad_curve<-renderPlotly({r<-result();req(r,r$config$purpose=="interim")
    plotly::plot_ly(r$curve,x=~z1,y=~final_events,type="scatter",mode="lines",name="Final总事件") |>
      plotly::layout(xaxis=list(title="IA获益方向Z1"),yaxis=list(title="Final累计事件数"))})
  output$ad_summary<-renderDT({r<-result();req(r,r$config$purpose=="simulation");table(r$summary)})
  output$ad_events<-renderDT({r<-result();req(r,r$config$purpose=="simulation")
    table(do.call(rbind,lapply(split(r$trials,r$trials$true_hr),function(x)data.frame(true_hr=x$true_hr[1],
      minimum=min(x$final_events),q025=unname(quantile(x$final_events,.025)),median=median(x$final_events),q975=unname(quantile(x$final_events,.975)),maximum=max(x$final_events)))))})
  output$ad_trials<-renderDT({r<-result();req(r,r$config$purpose=="simulation");table(head(r$trials[,c("SIMID","true_hr","z1","final_events","cp_selected","combination_z","reject_adaptive","reason")],200))})
  output$ad_config_download<-downloadHandler(filename="event_pred_adaptive_config.json",content=function(file){r<-result();req(r);jsonlite::write_json(list(version=r$version,config=r$config,plan=r$plan,R_version=R.version.string),file,auto_unbox=TRUE,pretty=TRUE,digits=NA)})
  output$ad_result_download<-downloadHandler(filename="event_pred_adaptive_result.csv",content=function(file){r<-result();req(r);write.csv(if(r$config$purpose=="interim")r$decision else r$summary,file,row.names=FALSE)})
  output$ad_trials_download<-downloadHandler(filename="event_pred_adaptive_trials.csv",content=function(file){r<-result();req(r,r$config$purpose=="simulation");write.csv(r$trials,file,row.names=FALSE)})
  output$ad_script_download<-downloadHandler(filename="event_pred_adaptive_reproduce.R",content=function(file){r<-result();req(r,r$config$purpose=="simulation");writeLines(adaptive_reproduction(r),file)})
  output$ad_report_download<-downloadHandler(filename="event_pred_adaptive_report.md",content=function(file){r<-result();req(r);writeLines(adaptive_report(r),file)})
  invisible(result)
}
