adaptive_batch_input <- function(input) {
  u<-input$ab_unit %||% "months";f<-time_factor(u)
  values<-function(pre){d<-simulation_defaults(u);setNames(lapply(names(d),function(k)input[[paste0(pre,k)]] %||% d[[k]]),names(d))}
  v<-values("abm_");base<-list(display_unit=u,engine_unit="days",p=input$ab_allocation,alpha=input$ab_alpha,control_model=simulation_parameter_model(v$design_model,v,u),enrollment=input$ab_enrollment,
    window1=input$ab_window1*f,window2=input$ab_window2*f,dropout_rates=c(dropout_input_rate(values("abdc_")),dropout_input_rate(values("abdt_")))/f)
  if(base$enrollment=="constant"){base$rate1<-input$ab_rate1/f;base$rate2<-input$ab_rate2/f}
  cfg<-list(research_family="independent_cohort_adaptation",mode="fixed_truth",design_mode="fixed",effect_mode="ph",sided="benefit",cut_rule="two_cohort_events",paramcd=input$ab_endpoint,origin=as.character(input$ab_origin),display_unit=u,engine_unit="days",patient_base=base,enrollment=base$enrollment,treatment_fraction=base$p,alpha=base$alpha,
    primary=input$ab_primary,compare_rules=setdiff(input$ab_extra %||% character(),input$ab_primary),reps=input$ab_reps,seed=input$ab_seed,scenario_source=input$ab_scenario_source,filter_enabled=FALSE,reference_scenario=1)
  if(cfg$scenario_source=="csv"){if(is.null(input$ab_file))stop("请选择情景CSV。");cfg$scenario_table<-read.csv(input$ab_file$datapath,stringsAsFactors=FALSE,check.names=FALSE);cfg$scenario_filename<-input$ab_file$name}
  else{cfg$plans<-abr_parse_plans(input$ab_plans);cfg$axes<-list(true_hr=parse_unit_numbers(input$ab_true_hrs),time_scale=parse_unit_numbers(input$ab_time_scales),enroll_scale=if(cfg$enrollment=="batch")1 else parse_unit_numbers(input$ab_enroll_scales),dropout_scale=parse_unit_numbers(input$ab_dropout_scales))}
  validate_adaptive_batch(cfg);cfg
}
register_adaptive_batch_server <- function(input,output,session,controller) {
  result<-reactive({r<-controller$result();if(!is.null(r)&&abr_is(r$config))r else NULL});imported<-reactiveVal(NULL);import_note<-reactiveVal("请选择重估研究无损配置JSON。")
  unit<-reactive(input$ab_unit %||% "months");previous<-reactiveVal("months")
  register_parameter_controls(input,output,session,unit,c("abm_","abdc_","abdt_"),simulation_defaults,simulation_parameter_model)
  observeEvent(input$ab_unit,{
    to<-unit();from<-previous();if(to==from)return();ratio<-time_factor(from)/time_factor(to);u<-time_label(to)
    update<-function(id,kind,label){v<-isolate(input[[id]]);if(is.null(v))return();value<-tryCatch(switch(kind,duration=v*ratio,rate=v/ratio,log_time=v+log(ratio),duration_text=paste(format(parse_unit_numbers(v)*ratio,digits=16,trim=TRUE),collapse=","),rate_text=paste(format(parse_unit_numbers(v)/ratio,digits=16,trim=TRUE),collapse=",")),error=function(e)v);freezeReactiveValue(input,id);if(kind %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,label),value=value) else updateNumericInput(session,id,label=parameter_label(id,label),value=value)}
    for(id in c("ab_window1","ab_window2"))update(id,"duration",paste0(if(id=="ab_window1")"阶段1最大窗口（" else "IA后阶段2最大窗口（",u,"）"))
    for(id in c("ab_rate1","ab_rate2"))update(id,"rate",paste0(if(id=="ab_rate1")"队列1" else "队列2","总体入组率（人/",u,"）"))
    for(pre in c("abm_","abdc_","abdt_")){
      for(kind in names(base_unit_fields))for(k in intersect(base_unit_fields[[kind]],names(simulation_defaults(from)))){
        label<-switch(k,median=paste0("中位时间（",u,"；治愈模型为未治愈成分）"),median2=paste0("第二成分中位时间（",u,"）"),eta=paste0("Weibull尺度eta（",u,"）"),log_mu=paste0("对数位置mu=log(m/",u,")"),exp_rate=paste0("指数风险率（每",u,"）"),g_rate=paste0("初始风险率（每",u,"）"),g_shape=paste0("Gompertz形状（每",u,"）"),parameter_cuts=paste0("风险切点（",u,"）"),parameter_rates=paste0("各段风险率（每",u,"）"),dropout_rate=paste0("独立脱落风险率（每",u,"）"),drop_period=paste0("脱落概率窗口（",u,"）"),NULL)
        update(paste0(pre,k),kind,label)
      };update(paste0(pre,"survival_time"),"duration",paste0("随访时点（",u,"）"));update(paste0(pre,"pwe_last_rate"),"rate",paste0("末段风险率（每",u,"）"))
    };previous(to)
  },ignoreInit=TRUE)
  observeEvent(input$ab_primary,{choices<-abr_names[names(abr_names)!=input$ab_primary];old<-isolate(input$ab_extra);updateCheckboxGroupInput(session,"ab_extra",choices=setNames(names(choices),unname(choices)),selected=intersect(old,names(choices)))},ignoreInit=TRUE)
  observe({for(id in c("ab_baseline","ab_scenarios","ab_rules"))if((input$ab_run_source %||% "ui")=="config_json")nav_hide("ab_tabs",id,session=session) else nav_show("ab_tabs",id,session=session)})
  observeEvent(input$ab_run_source,{nav_select("ab_tabs",if((input$ab_run_source %||% "ui")=="config_json")"ab_results" else "ab_baseline",session=session)},ignoreInit=TRUE)
  observeEvent(input$ab_config_file,{imported(NULL);tryCatch({cfg<-batch_read_config(input$ab_config_file$datapath);if(!abr_is(cfg))stop("文件不是独立队列重估研究配置。");imported(cfg);import_note(paste0("已载入",nrow(abr_scenarios(cfg)),"情景，每情景",cfg$reps,"轮；主规则",abr_names[[cfg$primary]],"。页面其他模拟参数不参与运行。"))},error=function(e)import_note(conditionMessage(e)))},ignoreInit=TRUE)
  output$ab_import_note<-renderUI(p(class="field-note",import_note()))
  observeEvent(input$ab_run,{tryCatch({if(!is.null(controller$job()))stop("本会话已有后台研究，请等待或取消。");cfg<-if((input$ab_run_source %||% "ui")=="config_json")imported() else adaptive_batch_input(input);if(is.null(cfg))stop("先载入有效配置。");store<-batch_create_run(cfg,input$ba_workspace_token,input$ab_run_name);controller$start_saved(cfg,store=store);controller$workspace$refresh()},error=function(e){controller$error(conditionMessage(e));nav_select("ab_tabs","ab_results",session=session)})},ignoreInit=TRUE)
  observeEvent(input$ab_cancel,{j<-controller$job();if(!is.null(j))file.create(file.path(j$dir,"cancel.flag"))},ignoreInit=TRUE)
  observeEvent(input$ab_library,{updateSelectInput(session,"task",selected="batch");nav_select("ba_tabs","ba_library",session=session)},ignoreInit=TRUE)
  output$ab_status<-renderUI({r<-result();e<-controller$error();tagList(if(!is.null(e))p(class="field-note",e),if(is.null(r))p(class="field-note","运行后显示冻结结果；修改参数不会改写旧结果。") else p(class="field-note",paste0(r$run_title %||% "重估研究"," · v",r$version,"待复核；",r$completed,"/",r$total,"轮；",r$status,"；主规则",abr_names[[r$config$primary]],"。CP为近似计算，非患者模拟条件概率。")))})
  display<-function(d,r){if(is.null(d)||!ncol(d))return(study_table(data.frame(记录=character())));for(k in names(d)[grepl("_day$|^DCO_DAY$",names(d))]){d[[k]]<-d[[k]]/time_factor(r$config$display_unit);names(d)[names(d)==k]<-paste0(k,"（",time_label(r$config$display_unit),"）")};study_table(d)}
  for(k0 in c("overview","method_overview","method_pairs","resources","stops"))local({k<-k0;output[[paste0("ab_",k)]]<-renderDT({r<-result();req(r);display(r[[k]],r)})})
  observe({r<-result();if(is.null(r))return();old<-isolate(input$ab_view_scenario);updateSelectInput(session,"ab_view_scenario",choices=setNames(r$scenarios$SCENARIO,paste(r$scenarios$SCENARIO,r$scenarios$label)),selected=if(length(old)==1&&old %in% as.character(r$scenarios$SCENARIO))old else r$scenarios$SCENARIO[1])})
  observe({r<-result();req(r,input$ab_view_scenario);v<-if(nrow(r$rows))r$rows$SIMID[r$rows$SCENARIO==as.integer(input$ab_view_scenario)&r$rows$generated] else integer();old<-isolate(input$ab_view_trial);updateSelectInput(session,"ab_view_trial",choices=v,selected=if(length(old)==1&&old %in% as.character(v))old else head(v,1))})
  output$ab_method_rows<-renderDT({r<-result();req(r,input$ab_view_scenario);d<-r$method_rows;if(nrow(d))d<-d[d$SCENARIO==as.integer(input$ab_view_scenario),,drop=FALSE];display(d,r)})
  sample_saved<-eventReactive(input$ab_sample_run,{r<-result();req(r,input$ab_view_scenario,input$ab_view_trial);a<-abr_sample(r,as.integer(input$ab_view_scenario),as.integer(input$ab_view_trial));a$selection<-list(run_id=r$run_id,scenario=as.integer(input$ab_view_scenario),replicate=as.integer(input$ab_view_trial));a},ignoreInit=TRUE)
  sample<-reactive({r<-result();req(r);a<-sample_saved();req(identical(a$selection,list(run_id=r$run_id,scenario=as.integer(input$ab_view_scenario),replicate=as.integer(input$ab_view_trial))));a})
  output$ab_sample_paths<-renderDT({r<-result();req(r);display(sample()$looks,r)})
  output$ab_sample_observed<-renderDT({r<-result();req(r);display(sample()$observed,r)})
  reference<-reactiveVal(NULL);reference_error<-reactiveVal(NULL)
  observeEvent(input$ab_reference_run,{reference_error(NULL);tryCatch({r<-result();if(is.null(r))stop("先提交或载入重估研究配置。");reference(abr_decision_reference(r,as.integer(input$ab_view_scenario),parse_unit_numbers(input$ab_reference_z)))},error=function(e)reference_error(conditionMessage(e)))},ignoreInit=TRUE)
  reference_current<-reactive({r<-result();req(input$ab_view_scenario);a<-reference();if(is.null(r)||is.null(a)||!identical(a$run_id,r$run_id)||a$scenario$SCENARIO!=as.integer(input$ab_view_scenario))return(NULL);a})
  output$ab_reference_note<-renderUI({a<-reference_current();e<-reference_error();tagList(if(!is.null(e))p(class="field-note",e),if(!is.null(a))p(class="field-note",a$note))})
  output$ab_reference<-renderDT({a<-reference_current();req(a);study_table(a$table)})
  output$ab_reference_download<-downloadHandler(filename="adaptive_batch_ia_reference.csv",content=function(file){a<-reference_current();req(a);write.csv(a$table,file,row.names=FALSE)})
  output$ab_reference_config<-downloadHandler(filename="adaptive_batch_ia_reference.json",content=function(file){a<-reference_current();req(a);a$config<-batch_config_document(a$config);jsonlite::write_json(a,file,auto_unbox=TRUE,pretty=TRUE,digits=NA)})
  output$ab_template<-downloadHandler(filename="adaptive_batch_scenarios.csv",content=function(file){d<-data.frame(label=c("null_plan_A","active_plan_A"),n1=250,n2=400,d1=80,d_plan=200,d_max=300,hr_assumed=.67,cp_target=.8,cp_min=.3,true_hr=c(1,.67),time_scale=1,enroll_scale=1,dropout_scale=1);write.csv(d[,abr_fields],file,row.names=FALSE)})
  for(k0 in c("scenarios","rows","method_rows","looks","overview","method_overview","method_pairs","resources","stops"))local({k<-k0;output[[paste0("ab_",k,"_download")]]<-downloadHandler(filename=paste0("adaptive_batch_",k,"_days.csv"),content=function(file){r<-result();req(r);write.csv(r[[k]],file,row.names=FALSE)})})
  output$ab_config_download<-downloadHandler(filename="adaptive_batch_config.json",content=function(file){r<-result();req(r);jsonlite::write_json(batch_config_document(r$config),file,auto_unbox=TRUE,pretty=TRUE,digits=NA)})
  output$ab_result_download<-downloadHandler(filename="adaptive_batch_result_draft.rds",content=function(file){r<-result();req(r);saveRDS(r,file,version=3)})
  output$ab_report_download<-downloadHandler(filename="adaptive_batch_report_draft.md",content=function(file){r<-result();req(r);writeLines(abr_report(r),file,useBytes=TRUE)},contentType="text/markdown; charset=utf-8")
  output$ab_script_download<-downloadHandler(filename="replay_adaptive_batch_draft.R",content=function(file){r<-result();req(r);writeLines(abr_reproduction_script(r),file,useBytes=TRUE)})
  for(k0 in c("observed","truth"))local({k<-k0;output[[paste0("ab_",k,"_download")]]<-downloadHandler(filename=paste0("adaptive_batch_selected_",k,"_days.csv"),content=function(file)write.csv(sample()[[k]],file,row.names=FALSE))})
  output$ab_adtte_download<-downloadHandler(filename="adaptive_batch_selected_adtte.csv",content=function(file){r<-result();req(r);write.csv(abr_adtte(sample(),r$config),file,row.names=FALSE)})
  invisible(result)
}
