register_joint_server <- function(input,output,session) {
  result<-reactiveVal(NULL);error<-reactiveVal(NULL);unit<-reactive(input$js_unit %||% "months");previous_unit<-reactiveVal("months")
  prefixes<-paste0("js",names(joint_transition_names),"_")
  register_parameter_controls(input,output,session,unit,prefixes,joint_defaults,parameter_input_model)
  observeEvent(input$js_unit,{
    to<-unit();from<-previous_unit();if(identical(to,from))return()
    ratio<-time_factor(from)/time_factor(to);u<-time_label(to)
    specs<-list(duration=c("js_max"),duration_text=c("js_cuts","js_fixed","js_enroll_cuts","js_schedule"),rate=c("js_enroll_rate","js_drop_control","js_drop_treatment"),rate_text=c("js_enroll_rates"))
    labels<-c(js_max=paste0("最大研究窗口（",u,"）"),js_cuts=paste0("共同DCO（研究",u,"数）"),js_fixed=paste0("固定个体随访时点（",u,"）"),
      js_enroll_cuts=paste0("入组切点（研究",u,"数）"),js_schedule=paste0("N个入组研究时间（",u,"）"),js_enroll_rate=paste0("总体入组率（人/",u,"）"),
      js_drop_control=paste0("Control共同退出率（每",u,"）"),js_drop_treatment=paste0("Treatment共同退出率（每",u,"）"),js_enroll_rates=paste0("各段入组率（人/",u,"）"))
    for(kind in names(specs))for(id in specs[[kind]]) {
      val<-isolate(input[[id]]);if(is.null(val))next
      value<-tryCatch(if(kind %in% c("duration_text","rate_text"))paste(format(parse_unit_numbers(val)*if(kind=="duration_text")ratio else 1/ratio,digits=16,trim=TRUE),collapse=",") else val*if(kind=="duration")ratio else 1/ratio,error=function(e)val)
      freezeReactiveValue(input,id)
      if(kind %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,labels[[id]]),value=value) else updateNumericInput(session,id,label=parameter_label(id,labels[[id]]),value=value)
    }
    for(pre in prefixes)for(field in c("median","eta","exp_rate","parameter_cuts","parameter_rates")) {
      id<-paste0(pre,field);val<-isolate(input[[id]]);if(is.null(val))next
      kind<-if(field %in% c("median","eta"))"duration" else if(field=="exp_rate")"rate" else if(field=="parameter_cuts")"duration_text" else "rate_text"
      value<-tryCatch(if(kind %in% c("duration_text","rate_text"))paste(format(parse_unit_numbers(val)*if(kind=="duration_text")ratio else 1/ratio,digits=16,trim=TRUE),collapse=",") else val*if(kind=="duration")ratio else 1/ratio,error=function(e)val)
      label<-switch(field,median=paste0("潜在转移时钟中位时间 m（",u,"）"),eta=paste0("Weibull尺度 eta（",u,"）"),exp_rate=paste0("原因别风险率 lambda（每",u,"）"),parameter_cuts=paste0("转移风险年龄切点（",u,"）"),parameter_rates=paste0("各段转移风险率（每",u,"）"))
      freezeReactiveValue(input,id)
      if(kind %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,label),value=value) else updateNumericInput(session,id,label=parameter_label(id,label),value=value)
    }
    previous_unit(to)
  },ignoreInit=TRUE)
  observeEvent(input$js_run,{
    error(NULL)
    tryCatch({cfg<-joint_config_input(input);r<-withProgress(message="生成PFS/OS联合轨迹",value=0,run_joint_simulation(cfg,function(v,d)setProgress(value=v,detail=d)))
      result(r);choices<-r$trial_status$SIMID[r$trial_status$completed];updateSelectInput(session,"js_trial",choices=choices,selected=head(choices,1));updateSelectInput(session,"js_view_cut",choices=if(nrow(r$cuts))sort(unique(r$cuts$CUTID)) else character(),selected=1)
      nav_select("js_tabs","js_results",session=session)
    },error=function(e){error(conditionMessage(e));nav_select("js_tabs","js_results",session=session)})
  },ignoreInit=TRUE)
  selected<-reactive({r<-result();req(r,input$js_trial,input$js_view_cut);list(r=r,b=as.integer(input$js_trial),k=as.integer(input$js_view_cut))})
  subset_selected<-function(d,s){if(!nrow(d))return(d);d[d$SIMID==s$b&d$CUTID==s$k,,drop=FALSE]}
  table<-function(d,r) {
    if(!ncol(d))return(study_table(data.frame(说明=character())))
    duration<-intersect(names(d),names(d)[grepl("_day$|^DCO_DAY$",names(d))])
    for(k in duration){d[[k]]<-d[[k]]/time_factor(r$config$display_unit);names(d)[names(d)==k]<-paste0(k,"（",time_label(r$config$display_unit),"）")}
    if("scope" %in% names(d))d$scope<-ifelse(d$scope=="overall","总体","组内")
    if("state_at_dco" %in% names(d))d$state_at_dco<-unname(c(dead="死亡",unknown_after_dropout="退出后状态未知",progressed_alive="已进展存活",progression_free_alive="未进展存活")[d$state_at_dco])
    labels<-c(PARAMCD="终点",SIMID="试验序号",CUTID="共同截点序号",scope="范围",group="组别",n="人数",events="事件数",requested="请求试验数",completed="完成",failed="失败",mean_n="平均人数",mean_events="完成轮次平均事件数",mean_events_lower_all="全部请求平均事件数下界",mean_events_upper_all="全部请求平均事件数上界",median_estimable="中位数可估计轮次",median_estimable_fraction="完成轮次中位数可估计比例",dropouts="永久退出",administrative="行政删失",median_status="中位数状态",pfs_events="PFS事件",os_events="OS事件",target_endpoint="触发终点",target="D*",target_reached="窗口内达标",analysis_status="分析状态",state_at_dco="DCO状态",last_known_state="最后已知状态",from_state="起始状态",to_state="转入状态",event_cause="事件类型",transition="转移/删失",note="说明",status="删失/事件状态",event="事件指示",survival="KM生存率",lower="生存率95%下限",upper="生存率95%上限",n_risk="在险人数",replicate_seed="轮次种子")
    duration_labels<-c(DCO_DAY="研究DCO",median_day="中位时间",lower_day="中位时间95%下限",upper_day="中位时间95%上限",conditional_lower_day="可估计条件下2.5%分位数",conditional_median_day="可估计条件下50%分位数",conditional_upper_day="可估计条件下97.5%分位数",time_day="随访年龄",entry_day="入组研究时间",obs_day="观察研究时间",target_day="达标研究时间",last_observed_day="最后观察研究时间",start_age_day="区间起始年龄",stop_age_day="区间结束年龄",start_study_day="区间起始研究时间",stop_study_day="区间结束研究时间",transition_clock_start_day="转移时钟起点",transition_clock_stop_day="转移时钟终点")
    for(k in names(duration_labels))names(d)<-sub(paste0("^",k,"(?=（|$)"),duration_labels[[k]],names(d),perl=TRUE)
    for(k in intersect(names(d),names(labels)))names(d)[names(d)==k]<-labels[[k]]
    study_table(d)
  }
  output$js_status<-renderUI({r<-result();e<-error();tagList(if(!is.null(e))p(class="field-note",paste0("本次未生成新结果：",e,if(!is.null(r))"。下方及下载保留最近提交结果。" else "")),if(is.null(r))p(class="field-note","设置研究、转移模型及截点后点击生成。") else p(class="field-note",paste0("保存于",r$created_at," · v",r$version,"待复核；请求",r$config$reps,"轮、完成",sum(r$trial_status$completed),"轮、失败",sum(!r$trial_status$completed),"轮。分位数只统计中位数可估计的完成轮次；NR和未达目标分别保留。模型不提供联合检验或多重性控制。修改输入后需重新运行，结果及下载保留原配置。")))})
  for(pair in list(c("js_overview","overview"),c("js_trial_status","trial_status")))local({id<-pair[1];key<-pair[2];output[[id]]<-renderDT({r<-result();req(r);table(if(key=="overview")joint_overview(r) else r[[key]],r)})})
  for(pair in list(c("js_cuts_table","cuts"),c("js_summary","summary"),c("js_fixed_table","fixed"),c("js_states","states"),c("js_observed","observed"),c("js_intervals","intervals")))local({id<-pair[1];key<-pair[2];output[[id]]<-renderDT({s<-selected();table(subset_selected(s$r[[key]],s),s$r)})})
  output$js_km<-renderPlotly({s<-selected();d<-subset_selected(s$r$curves,s);validate(need(nrow(d)>0,"此轮截点没有可显示的观察曲线。"));d<-d[d$scope=="group",,drop=FALSE];d$time<-d$time_day/time_factor(s$r$config$display_unit)
    p<-ggplot(d,aes(time,survival,colour=group))+geom_step(linewidth=.8)+facet_wrap(~PARAMCD,ncol=2)+coord_cartesian(ylim=c(0,1))+labs(x=paste0("个体随访时间（",time_label(s$r$config$display_unit),"）"),y="KM生存率",colour="组别")+theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),legend.position="bottom")+scale_colour_manual(values=c("#126b72","#173d50"));plotly::ggplotly(p)
  })
  output$js_export_note<-renderUI({s<-selected();o<-subset_selected(s$r$observed,s);x<-joint_adtte(o,s$r$config);issues<-if(nrow(x))simulation_export_issues(x) else data.frame();p(class="field-note",paste0("ADTTE每人每截点两行，PARAMCD为PFS/OS。日期向下取整，AVAL含首日；CNSR=0事件、1行政删失、2共同退出。连续时间分析不取整。",if(nrow(issues))paste0(nrow(issues),"条同日起止记录在导出中保留，当前预测接口不接受这些记录。") else ""))})
  for(key0 in c("observed","states","intervals"))local({key<-key0;output[[paste0("js_",key,"_download")]]<-downloadHandler(filename=paste0("joint_selected_",key,"_days.csv"),content=function(file){s<-selected();write.csv(subset_selected(s$r[[key]],s),file,row.names=FALSE)})})
  for(key0 in c("summary","fixed","cuts","trial_status","truth","overview"))local({key<-key0;output[[paste0("js_",key,"_download")]]<-downloadHandler(filename=paste0("joint_all_",key,"_days.csv"),content=function(file){r<-result();req(r);write.csv(if(key=="overview")joint_overview(r) else r[[key]],file,row.names=FALSE)})})
  output$js_adtte_download<-downloadHandler(filename="joint_selected_adtte.csv",content=function(file){s<-selected();write.csv(joint_adtte(subset_selected(s$r$observed,s),s$r$config),file,row.names=FALSE)})
  output$js_config_download<-downloadHandler(filename="joint_survival_config_draft.json",content=function(file){r<-result();req(r);jsonlite::write_json(list(schema=r$schema,version=r$version,status=r$status,created_at=r$created_at,config=r$config),file,auto_unbox=TRUE,pretty=TRUE,digits=NA,na="null")})
  output$js_result_download<-downloadHandler(filename="joint_survival_full_draft.rds",content=function(file){r<-result();req(r);saveRDS(r,file,version=3)})
  output$js_script_download<-downloadHandler(filename="replay_joint_survival_draft.R",content=function(file){r<-result();req(r);writeLines(joint_script(r),file,useBytes=TRUE)})
  output$js_report_download<-downloadHandler(filename="joint_survival_draft.md",content=function(file){r<-result();req(r);writeLines(joint_report(r),file,useBytes=TRUE)},contentType="text/markdown; charset=utf-8")
  invisible(result)
}
