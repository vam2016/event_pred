register_core_simulation_server <- function(input,output,session) {
  sim_result <- reactiveVal(NULL); sim_error <- reactiveVal(NULL)
  sim_unit <- reactive(input$sim_unit %||% "months"); previous_unit <- reactiveVal("months")
  register_parameter_controls(input,output,session,sim_unit,paste0("s",1:2,"_"),simulation_defaults,simulation_parameter_model)
  output$sim_group_inputs <- renderUI({
    n <- input$sim_groups %||% 2; req(n>=1,n<=2,n==floor(n))
    unit <- isolate(sim_unit())
    do.call(navset_card_tab,lapply(seq_len(n),function(j)nav_panel(paste("组",j),simulation_group_ui(j,unit))))
  })
  observeEvent(input$sim_unit,{
    to <- sim_unit(); from <- previous_unit(); if(identical(from,to))return()
    ratio <- time_factor(from)/time_factor(to); u <- time_label(to)
    settings <- list(duration=c("sim_max"),duration_text=c("sim_cuts","sim_fixed","sim_enroll_cuts","sim_schedule"),rate=c("sim_enroll_rate"),rate_text=c("sim_enroll_rates"))
    labels <- c(sim_max=paste0("最大研究窗口（",u,"）"),sim_cuts=paste0("分析DCO（研究",u,"数）"),sim_fixed=paste0("固定随访时点（",u,"）"),
      sim_enroll_cuts=paste0("入组切点（研究",u,"数）"),sim_schedule=paste0("N个入组研究时间（",u,"）"),sim_enroll_rate=paste0("总体入组率（人/",u,"）"),sim_enroll_rates=paste0("各段入组率（人/",u,"）"))
    for(kind in names(settings))for(id in settings[[kind]]) {
      val <- isolate(input[[id]]); if(is.null(val))next
      value <- if(kind %in% c("duration_text","rate_text"))tryCatch(paste(format(parse_unit_numbers(val)*if(kind=="duration_text")ratio else 1/ratio,digits=16),collapse=","),error=function(e)val) else val*if(kind=="duration")ratio else 1/ratio
      freezeReactiveValue(input,id)
      if(kind %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,labels[[id]]),value=value)
      else updateNumericInput(session,id,label=parameter_label(id,labels[[id]]),value=value)
    }
    for(j in 1:2) {
      pre <- paste0("s",j,"_")
      for(kind in names(base_unit_fields))for(field in intersect(base_unit_fields[[kind]],names(simulation_defaults(from)))) {
        id <- paste0(pre,field); val <- isolate(input[[id]]); if(is.null(val))next
        value <- tryCatch(switch(kind,duration=val*ratio,rate=val/ratio,log_time=val+log(ratio),
          duration_text=paste(format(parse_unit_numbers(val)*ratio,digits=16),collapse=","),rate_text=paste(format(parse_unit_numbers(val)/ratio,digits=16),collapse=",")),error=function(e)val)
        label <- switch(field,median=paste0("中位事件时间 m（",u,"）"),median2=paste0("第二成分中位时间（",u,"）"),eta=paste0("Weibull尺度eta（",u,"）"),log_mu=paste0("对数位置mu=log(m/",u,")"),
          exp_rate=paste0("指数风险率（每",u,"）"),g_rate=paste0("初始风险率b（每",u,"）"),g_shape=paste0("Gompertz形状g（每",u,"）"),parameter_cuts=paste0("风险切点（",u,"）"),parameter_rates=paste0("各段事件风险率（每",u,"）"),
          dropout_rate=paste0("永久脱落风险率（每",u,"）"),drop_period=paste0("脱落概率窗口（",u,"）"),NULL)
        freezeReactiveValue(input,id)
        if(kind %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,label),value=value)
        else updateNumericInput(session,id,label=parameter_label(id,label),value=value)
      }
      for(field in c("survival_time","pwe_last_rate")) {
        id <- paste0(pre,field); val <- isolate(input[[id]]); if(is.null(val))next
        freezeReactiveValue(input,id)
        updateNumericInput(session,id,label=parameter_label(id,paste0(if(field=="survival_time")"随访时点（" else "末段风险率（每",u,"）")),value=val*if(field=="survival_time")ratio else 1/ratio)
      }
    }
    previous_unit(to)
  },ignoreInit=TRUE)
  observeEvent(input$sim_run,{
    sim_error(NULL)
    tryCatch({
      unit <- sim_unit(); f <- time_factor(unit); n <- input$sim_groups
      if(length(n)!=1||!is.finite(n)||n<1||n>2||n!=floor(n))stop("组数需1或2。")
      if (!input$sim_cut_mode %in% "fixed") stop("核心数据模拟使用固定 DCO。")
      if (!input$sim_enroll_mode %in% c("constant","piecewise")) stop("核心入组方式使用恒定或分段 Poisson。")
      groups <- lapply(seq_len(n),function(j) {
        pre <- paste0("s",j,"_"); defaults <- simulation_defaults(unit,j)
        v <- setNames(lapply(names(defaults),function(x)input[[paste0(pre,x)]] %||% defaults[[x]]),names(defaults))
        if (!v$design_model %in% core_models) stop("核心模型为指数、Weibull 或 PWE。")
        list(name=v$name,weight=v$weight,dropout_rate=dropout_input_rate(v)/f,model=simulation_parameter_model(v$design_model,v,unit),entered=v)
      })
      cfg <- list(n=input$sim_n,reps=input$sim_reps,seed=input$sim_seed,origin=as.character(input$sim_origin),paramcd=input$sim_endpoint,
        display_unit=unit,engine_unit="days",groups=groups,enroll_mode=input$sim_enroll_mode,enroll_rate=input$sim_enroll_rate/f,
        enroll_cuts=if(input$sim_enroll_mode=="piecewise")parse_unit_numbers(input$sim_enroll_cuts)*f else numeric(),enroll_rates=if(input$sim_enroll_mode=="piecewise")parse_unit_numbers(input$sim_enroll_rates)/f else numeric(),
        entry_days=if(input$sim_enroll_mode=="schedule")parse_unit_numbers(input$sim_schedule)*f else numeric(),cut_mode=input$sim_cut_mode,cuts=if(input$sim_cut_mode=="fixed")parse_unit_numbers(input$sim_cuts)*f else numeric(),
        target=if(input$sim_cut_mode=="target")input$sim_target else 1,max_day=if(input$sim_cut_mode=="target")input$sim_max*f else max(parse_unit_numbers(input$sim_cuts))*f,fixed_times=parse_unit_numbers(input$sim_fixed)*f)
      r <- withProgress(message="生成生存数据",value=0,{run_survival_simulation(cfg,function(v,d)setProgress(value=v,detail=d))})
      r$engine_version <- r$version; r$version <- core_version; r$config$core_version <- core_version
      sim_result(r); updateSelectInput(session,"sim_trial",choices=seq_len(r$config$reps),selected=1)
      updateSelectInput(session,"sim_view_cut",choices=unique(r$cuts$CUTID),selected=1)
      nav_select("sim_tabs","模拟结果",session=session)
    },error=function(e){sim_error(conditionMessage(e));showNotification(conditionMessage(e),type="error")})
  })
  selected <- reactive({r<-sim_result();req(r,input$sim_trial,input$sim_view_cut);list(r=r,b=as.integer(input$sim_trial),k=as.integer(input$sim_view_cut))})
  subset_selected <- function(d,s)d[d$SIMID==s$b&d$CUTID==s$k,,drop=FALSE]
  subset_display <- function(d,s) {
    d<-subset_selected(d,s);g<-input$sim_display_group %||% "all"
    if(g!="all") d<-d[d$group==g,,drop=FALSE]
    d
  }
  view_unit <- function(d,cols,r)unit_table(d,r$config$display_unit,cols)
  table <- function(d) {
    if("scope" %in% names(d))d$scope <- ifelse(d$scope=="overall","总体","组内")
    if("status" %in% names(d))d$status <- ifelse(d$status=="event","事件",ifelse(d$status=="dropout","永久退出",ifelse(d$status=="active","行政删失",d$status)))
    labels <- c(SIMID="试验序号",CUTID="截点序号",DCO_DAY="研究DCO",scope="范围",group="组别",n="人数",events="事件数",dropouts="永久退出人数",administrative="行政删失人数",median_day="中位时间",lower_day="中位时间95%下限",upper_day="中位时间95%上限",median_status="中位数状态",time_day="随访时间",survival="生存率",lower="生存率95%下限",upper="生存率95%上限",n_risk="在险人数",status="状态",entry_day="入组研究时间",obs_day="观察研究时间",event="事件指示")
    for(id in names(labels))names(d)<-sub(paste0("^",id,"(?=（|$)"),labels[[id]],names(d),perl=TRUE)
    tab <- DT::datatable(d,rownames=FALSE,class="display nowrap",options=list(scrollX=TRUE,pageLength=8,dom="lftip",language=list(search="检索：",lengthMenu="每页 _MENU_ 条",zeroRecords="无匹配记录",info="显示 _START_ 至 _END_，共 _TOTAL_ 条",infoEmpty="无记录",emptyTable="无记录",paginate=list(previous="上一页",`next`="下一页"))))
    cols <- names(d)[vapply(d,is.double,logical(1))]
    if(length(cols))DT::formatRound(tab,cols,digits=3) else tab
  }
  output$sim_status <- renderUI({
    r<-sim_result();err<-sim_error()
    tagList(if(!is.null(err))p(class="field-note",paste("本次失败：",err)),if(is.null(r))p("设置研究与各组参数后，点击生成并分析。") else {
      tagList(div(class="context-strip",div(span("运行版本"),strong(r$version)),div(span("试验数"),strong(r$config$reps)),div(span("运行单位"),strong(time_label(r$config$display_unit))),div(span("种子"),strong(r$config$seed))),
        p(class="field-note",paste("最近成功运行：",r$created_at,"。区间为KM估计区间；汇总分位数只在中位数可估计的试验中计算。当前不计算功效。")))
    })
  })
  output$sim_overview <- renderDT({r<-sim_result();req(r);d<-simulation_result_overview(r);g<-input$sim_display_group %||% "all";if(g!="all")d<-d[d$group==g,,drop=FALSE];d$scope<-ifelse(d$scope=="overall","总体","组内");d<-view_unit(d,c("conditional_lower_day","conditional_median_day","conditional_upper_day"),r)
    names(d)[1:8]<-c("截点序号","范围","组别","试验数","平均人数","平均事件数","中位数可估计比例","可估计条件下2.5%分位数")
    names(d)[9:10]<-c("可估计条件下50%分位数","可估计条件下97.5%分位数");names(d)[8:10]<-paste0(names(d)[8:10],"（",time_label(r$config$display_unit),"）");table(d)})
  output$sim_summary <- renderDT({s<-selected();d<-subset_display(s$r$summary,s);d<-view_unit(d,c("DCO_DAY","median_day","lower_day","upper_day"),s$r);table(d)})
  output$sim_fixed_table <- renderDT({s<-selected();d<-subset_display(s$r$fixed,s);d<-view_unit(d,c("DCO_DAY","time_day"),s$r);table(d)})
  output$sim_risk <- renderDT({s<-selected();d<-subset_display(s$r$fixed,s);d<-d[d$scope=="group",c("group","time_day","n_risk","status"),drop=FALSE];table(view_unit(d,"time_day",s$r))})
  output$sim_data <- renderDT({s<-selected();table(view_unit(subset_display(s$r$observed,s),c("DCO_DAY","entry_day","time_day","obs_day"),s$r))})
  output$sim_export_note <- renderUI({s<-selected();x<-simulation_adtte(subset_selected(s$r$observed,s),s$r$config);bad<-simulation_export_issues(x)
    p(class="field-note",paste0("ADTTE日期向下取整，AVAL含首日；CNSR=0事件、1行政删失、2永久退出。",if(nrow(bad))paste0(nrow(bad),"条同日起止记录不能进入当前预测接口；CSV保留这些记录，不改日期。") else "本次日期记录满足正经过时间要求。","生存分析使用连续时间，日期导出可能产生并列时间。"))
  })
  download <- function(id,name,fun) output[[id]] <- downloadHandler(filename=name,content=fun)
  download("sim_observed_download","simulated_observed_continuous.csv",function(file){s<-selected();write.csv(unit_export(subset_selected(s$r$observed,s),s$r$config$display_unit,c("DCO_DAY","entry_day","time_day","obs_day")),file,row.names=FALSE)})
  download("sim_adtte_download","simulated_adtte.csv",function(file){s<-selected();write.csv(simulation_adtte(subset_selected(s$r$observed,s),s$r$config),file,row.names=FALSE)})
  for(pair in list(c("sim_summary_download","summary"),c("sim_fixed_download","fixed"),c("sim_cuts_download","cuts"),c("sim_truth_download","truth"))) local({
    id<-pair[1];key<-pair[2];download(id,paste0("simulation_",key,".csv"),function(file){r<-sim_result();req(r);d<-r[[key]];if(key=="cuts"&&r$config$cut_mode=="fixed")d<-d[,setdiff(names(d),c("target_reached","target_day")),drop=FALSE];cols<-names(d)[grepl("_day$|^DCO_DAY$",names(d))];write.csv(unit_export(d,r$config$display_unit,cols),file,row.names=FALSE)})
  })
  export_config<-function(r){cfg<-r$config;if(cfg$cut_mode=="fixed")cfg$target<-NULL;cfg}
  download("sim_config_download","survival_simulation_config.json",function(file){r<-sim_result();req(r);jsonlite::write_json(list(version=r$version,created_at=r$created_at,config=export_config(r),date_rule="floor; AVAL offset=1; same-day rows preserved",R=R.version.string),file,auto_unbox=TRUE,pretty=TRUE,digits=NA)})
  download("sim_script_download","reproduce_survival_simulation.R",function(file){r<-sim_result();req(r);writeLines(c('# 在工作台解压目录（含R文件夹）执行；与原运行使用相同版本的R和依赖。',
    'source("R/models.R"); source("R/inputs.R"); source("R/forecast.R"); source("R/simulation.R")',
    paste0('cfg <- jsonlite::fromJSON(',encodeString(jsonlite::toJSON(export_config(r),auto_unbox=TRUE,digits=NA,null="null"),quote='"'),', simplifyVector = TRUE)'),
    '# 修复JSON中的组列表，保持独立组配置。',
    paste0('cfg$groups <- jsonlite::fromJSON(',encodeString(jsonlite::toJSON(r$config$groups,auto_unbox=TRUE,digits=NA),quote='"'),', simplifyVector = FALSE)'),
    'cfg$groups <- lapply(cfg$groups, function(g) { g$model$params <- unlist(g$model$params); g$model$cuts <- unlist(g$model$cuts); g })',
    'if(cfg$cut_mode=="fixed")cfg$target<-1', 'res <- run_survival_simulation(cfg)', 'write.csv(res$summary, "simulation_summary.csv", row.names=FALSE)'),file,useBytes=TRUE)})
  download("sim_report_download","survival_simulation_report.md",function(file){r<-sim_result();req(r);o<-simulation_result_overview(r);writeLines(c('# 生存数据模拟记录','',paste('运行版本：',r$version),paste('运行时间：',r$created_at),paste('输入/显示单位：',time_label(r$config$display_unit)),
    '','设计阶段、给定参数、简单随机分组、独立脱落。重复生成试验后按设定截点计算KM；本阶段未计算功效或Ⅰ类错误。','',
    '## 配置','','```json',jsonlite::toJSON(export_config(r),auto_unbox=TRUE,pretty=TRUE,digits=NA),'```','',
    '## 中位数可估计性','','|截点序号|范围|组别|试验数|可估计比例|','|---|---|---|---:|---:|',sprintf('|%s|%s|%s|%s|%.1f%%|',o$CUTID,o$scope,o$group,o$trials,100*o$median_estimable),'',
    'NR不作为无限真值参与平均。固定时点超过观察支持时不外推。事件驱动窗口未达标轮次保留窗口末分析。ADTTE日期向下取整；同日起止记录保留并提示，连续时间分析与日期导出分别记录。'),file,useBytes=TRUE)})
  sim_result
}
