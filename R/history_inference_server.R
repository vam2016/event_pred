register_history_inference_server <- function(input,output,session) {
  result<-reactiveVal(NULL);run_error<-reactiveVal(NULL)
  raw<-reactive({req(input$hi_file);read_adtte(input$hi_file$datapath,input$hi_file$name)})
  output$hi_endpoint_ui<-renderUI({x<-tryCatch(raw(),error=function(e)NULL);labs<-if(!is.null(x)&&"PARAMCD" %in% names(x))sort(unique(as.character(x$PARAMCD[!is.na(x$PARAMCD)]))) else character();old<-isolate(input$hi_endpoint);selectInput("hi_endpoint","PARAMCD",labs,selected=if(length(old)==1&&old %in% labs)old else head(labs,1))})
  output$hi_group_column_ui<-renderUI({x<-tryCatch(raw(),error=function(e)NULL);cols<-if(is.null(x))character() else names(x);old<-isolate(input$hi_group_column);selectInput("hi_group_column","组别变量",cols,selected=if(length(old)==1&&old %in% cols)old else if("TRTP" %in% cols)"TRTP" else head(cols,1))})
  observe({req(input$hi_endpoint);choices<-tryCatch(ia_history_choices(raw(),input$hi_endpoint,input$hi_flag %||% "",input$hi_flag_value %||% "Y"),error=function(e)integer());old<-isolate(input$hi_look);updateSelectInput(session,"hi_look",choices=choices,selected=if(length(old)==1&&old %in% as.character(choices))old else tail(choices,1))})
  source_data<-reactive({req(input$hi_file,input$hi_endpoint,input$hi_group_column,input$hi_look,input$hi_origin)
    normalize_ia_history(raw(),input$hi_endpoint,input$hi_origin,as.numeric(input$hi_look),
      as.numeric(input$hi_offset),parse_unit_numbers(input$hi_dropout_codes),input$hi_flag %||% "",input$hi_flag_value %||% "Y",
      input$hi_date_encoding,input$hi_aval_unit,input$hi_group_column)
  })
  observe({z<-tryCatch(source_data(),error=function(e)NULL);labs<-if(is.null(z))character() else sort(unique(z$data$group));old<-isolate(input$hi_control);updateSelectInput(session,"hi_control",choices=labs,selected=if(length(old)==1&&old %in% labs)old else head(labs,1))})
  output$hi_source_note<-renderUI({if(is.null(input$hi_file))return(p(class="field-note","上传历史文件后选择终点、分析次序和Control。模板包含20/40/60事件的三次快照；配套原D*=80、信息比例0.25,0.5,0.75,1。"));z<-tryCatch(source_data(),error=function(e)list(error=conditionMessage(e)));if(!is.null(z$error))return(p(class="field-note",z$error));p(class="field-note",paste0("已读取截至原分析",length(z$snapshots),"的患者快照；最新DCO为",format(as.Date(z$origin)+z$cut),"，观察患者",nrow(z$data),"，累计事件",sum(z$data$event),"。组别、日期和结局只使用所选前缀。"))})
  observeEvent(input$hi_run,{
    run_error(NULL)
    tryCatch({
      z<-source_data();cfg<-history_inference_config(input)
      metadata<-list(filename=input$hi_file$name,paramcd=z$paramcd,origin=z$origin,
        group_column=input$hi_group_column,aval_unit=input$hi_aval_unit,date_encoding=input$hi_date_encoding,
        offset=as.numeric(input$hi_offset),dropout_codes=parse_unit_numbers(input$hi_dropout_codes),
        analysis_flag=input$hi_flag %||% "",flag_value=input$hi_flag_value %||% "Y")
      r<-withProgress(message="计算历史调整推断",value=0,{incProgress(.1,detail="读取所选前缀与原方案");v<-run_history_inference(z$snapshots,cfg,metadata);incProgress(.9);v})
      result(r);nav_select("hi_tabs","hi_results",session=session)
    },error=function(e){run_error(conditionMessage(e));nav_select("hi_tabs","hi_results",session=session)})
  },ignoreInit=TRUE)
  output$hi_status<-renderUI({e<-run_error();r<-result();if(!is.null(e))return(p(class="field-note",paste0("本次未生成新结果：",e,if(!is.null(r))"。下方及下载仍对应最近成功计算。" else "")));if(is.null(r))return(p(class="field-note","选择历史数据及原设计后点击计算。"));p(class="field-note",paste0("保存于",r$created_at," · v",r$version," · 新入口待复核"))})
  output$hi_result_note<-renderUI({r<-result();req(r);p(class="field-note",paste0("截至原分析",length(r$snapshots),"；Control：",r$control,"，Treatment：",r$treatment,"。正Z表示Treatment获益；I=D*p*(1−p)用于PH/canonical近似，实际log-rank方差仅作为诊断。近似HR不等同于Cox拟合HR。原决策：",gs_action_label(tail(r$path$action,1)),"。",r$inference$repeated_note," ",r$inference$final$reason," 修改输入后需重新计算；当前结果和下载保留提交时配置。"))})
  history_table<-function(d,r) {
    d<-history_inference_export_table(d)
    if("source" %in% names(d)){d$source<-"已观察历史";names(d)[names(d)=="source"]<-"来源"}
    if("DCO_DAY" %in% names(d)){d$DCO_DAY<-d$DCO_DAY/time_factor(r$config$display_unit);names(d)[names(d)=="DCO_DAY"]<-paste0("研究经过时间（",time_label(r$config$display_unit),"）")}
    labels<-c(events="累计事件数",nominal_p="阶段名义p",variance_to_event_information="实际方差 / 事件信息近似",n_control="Control人数",n_treatment="Treatment人数",events_control="Control事件",events_treatment="Treatment事件",active="仍随访人数",permanent_dropout="永久退出人数",note="说明")
    for(id in intersect(names(d),names(labels)))names(d)[names(d)==id]<-labels[[id]]
    gs_table(d)
  }
  for(pair in list(c("hi_plan","plan"),c("hi_path","path"),c("hi_diagnostics","diagnostics")))local({id<-pair[1];key<-pair[2];output[[id]]<-renderDT({r<-result();req(r);history_table(r[[key]],r)})})
  output$hi_repeated<-renderDT({r<-result();req(r);gs_inference_table(r$inference$repeated)})
  output$hi_final<-renderDT({r<-result();req(r);gs_inference_table(r$inference$final)})
  for(key0 in c("plan","path","diagnostics","records","repeated","final"))local({key<-key0;output[[paste0("hi_",key,"_download")]]<-downloadHandler(filename=paste0("observed_history_",key,".csv"),content=function(file){r<-result();req(r);d<-if(key=="records")history_inference_records(r) else if(key %in% c("repeated","final"))r$inference[[key]] else r[[key]];write.csv(history_inference_export_table(d),file,row.names=FALSE)})})
  output$hi_json_download<-downloadHandler(filename="observed_history_inference_draft.json",content=function(file){r<-result();req(r);jsonlite::write_json(history_inference_bundle(r),file,auto_unbox=TRUE,pretty=TRUE,digits=NA,na="null")})
  output$hi_rds_download<-downloadHandler(filename="observed_history_inference_draft.rds",content=function(file){r<-result();req(r);saveRDS(history_inference_bundle(r),file,version=3)})
  output$hi_report_download<-downloadHandler(filename="observed_history_inference_draft.md",content=function(file){r<-result();req(r);writeLines(history_inference_report(r),file,useBytes=TRUE)},contentType="text/markdown; charset=utf-8")
  output$hi_script_download<-downloadHandler(filename="replay_observed_history_draft.R",content=function(file){r<-result();req(r);writeLines(history_inference_script(r),file,useBytes=TRUE)})
  output$hi_template<-downloadHandler(filename="adtte_ia_history_structure.csv",content=function(file)write.csv(ia_history_template(),file,row.names=FALSE))
  invisible(result)
}
