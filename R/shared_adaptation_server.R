sp_observed_source<-function(input){
 if(is.null(input$sp_file))stop("选择实际IA的两组ADTTE文件。")
 raw<-read_adtte(input$sp_file$datapath,input$sp_file$name);cut<-input$sp_cut*time_factor(input$sp_unit %||% "months")
 keys<-c("ENGINE_ENTRY_DAY","ENGINE_OBS_DAY","ENGINE_TIME_DAY")
 precise<-all(keys %in% names(raw))
 if(identical(input$sp_test_basis,"cohort_riskset")&&!precise)stop("队列风险集分支需ENGINE_ENTRY_DAY/ENGINE_OBS_DAY/ENGINE_TIME_DAY精确连续日；日期AVAL不能还原事件先后。")
 if(precise){
   reqcols<-c("USUBJID","PARAMCD","STARTDT","ADT","AVAL","CNSR",input$sp_group_column,keys)
   if(!all(reqcols %in% names(raw)))stop("精确IA资料缺少ADTTE/组别/ENGINE字段。")
   r<-raw[!is.na(raw$PARAMCD)&raw$PARAMCD==input$sp_endpoint,,drop=FALSE]
   if(!nrow(r)||anyNA(r[,reqcols,drop=FALSE])||anyDuplicated(r$USUBJID))stop("终点筛选后每人唯一，必填字段无缺失。")
   for(k in c(keys,"AVAL","CNSR"))if(!is.numeric(r[[k]])||any(!is.finite(r[[k]])))stop("精确时间/AVAL/CNSR需有限数值。")
   offset<-as.numeric(input$sp_offset);if(!offset %in% 0:1||any(r$CNSR<0|r$CNSR!=floor(r$CNSR)))stop("首日加项0/1；CNSR非负整数。")
   drop<-parse_unit_numbers(input$sp_dropout_codes);if(any(drop<=0|drop!=floor(drop)))stop("永久退出编码须正整数。")
   start<-adtte_date(r$STARTDT,input$sp_date_encoding);end<-adtte_date(r$ADT,input$sp_date_encoding);origin<-as.Date(input$sp_origin)
   entry<-r$ENGINE_ENTRY_DAY;obs<-r$ENGINE_OBS_DAY;age<-r$ENGINE_TIME_DAY
   if(any(abs(obs-entry-age)>1e-6)||any(floor(entry)!=as.numeric(start-origin)|floor(obs)!=as.numeric(end-origin)))stop("连续ENGINE时间与ADTTE日期不一致。")
   if(any(abs(r$AVAL*time_factor(input$sp_aval_unit)-(as.numeric(end-start)+offset))>1e-5))stop("AVAL与日期及首日加项不符。")
   d<-data.frame(id=as.character(r$USUBJID),entry=entry,time=age,obs_day=obs,status=ifelse(r$CNSR==0,"event",ifelse(r$CNSR %in% drop,"dropout","active")),group=as.character(r[[input$sp_group_column]]))
   d<-validate_data(d,cut,FALSE,"strict")
 }else d<-normalize_adtte(raw,input$sp_endpoint,as.character(input$sp_origin),cut,as.numeric(input$sp_offset),parse_unit_numbers(input$sp_dropout_codes),"","Y",input$sp_date_encoding,"strict",input$sp_aval_unit,group_column=input$sp_group_column)
 list(data=d,cut=cut,filename=input$sp_file$name,precision=if(precise)"exact_continuous_ENGINE_days" else "date_times_treated_as_exact")
}
sp_parameter_input<-function(input){
 unit<-input$sp_unit %||% "months";f<-time_factor(unit)
 v<-function(pre){d<-simulation_defaults(unit);setNames(lapply(names(d),function(k)input[[paste0(pre,k)]] %||% d[[k]]),names(d))}
 base<-v("spm_")
 cfg<-list(research_family="shared_patient_adaptation",mode=input$sp_purpose,display_unit=unit,engine_unit="days",origin=as.character(input$sp_origin),paramcd=input$sp_endpoint,control_model=simulation_parameter_model(base$design_model,base,unit),test_basis=input$sp_test_basis,
  control=if(input$sp_purpose=="fixed_truth")"Control" else input$sp_control,alpha=input$sp_alpha,sided="benefit",interval_alpha=input$sp_interval_alpha,test_grid=parse_unit_numbers(input$sp_test_grid),test_weights=parse_unit_numbers(input$sp_test_weights),ci_grid=parse_unit_numbers(input$sp_ci_grid),ci_weights=parse_unit_numbers(input$sp_ci_weights))
 sp_grid(cfg$test_grid,cfg$test_weights,TRUE);sp_grid(cfg$ci_grid,cfg$ci_weights)
 if(input$sp_purpose!="fixed_truth"){cfg$payload<-sp_observed_source(input);cfg$d1<-sum(cfg$payload$data$event);cfg$treatment<-setdiff(unique(cfg$payload$data$group),cfg$control)}else{cfg$d1<-input$sp_d1;cfg$treatment<-"Treatment"}
 if(input$sp_purpose=="observed")return(cfg)
 cfg$design_mode<-"adaptive_e_mixture";cfg$effect_mode<-if(cfg$test_basis=="known_hazard")"known_baseline_ph" else "exact_entry_cohort_ph";cfg$cut_rule<-"shared_patient_events";cfg$d_plan<-input$sp_dplan;cfg$d_max<-input$sp_dmax;cfg$p<-input$sp_allocation;cfg$treatment_fraction<-cfg$p
 cfg$hr_assumed<-input$sp_hr_assumed;cfg$cp_target<-input$sp_cp_target;cfg$cp_min<-input$sp_cp_min;cfg$rule<-"promising";cfg$primary<-input$sp_primary;cfg$compare_methods<-setdiff(input$sp_extra %||% character(),cfg$primary)
 cfg$enroll_mode<-input$sp_enroll_mode;cfg$enroll_rate<-if(cfg$enroll_mode=="constant")input$sp_enroll_rate/f else 1;cfg$dropout_rates<-c(dropout_input_rate(v("spdc_")),dropout_input_rate(v("spdt_")))/f
 cfg$max_day<-input$sp_max*f;cfg$scenario_table<-read.csv(text=input$sp_scenarios,stringsAsFactors=FALSE,check.names=FALSE);cfg$reps<-input$sp_reps;cfg$seed<-input$sp_seed;cfg$estimate_effect<-isTRUE(input$sp_estimate_effect);cfg$filter_enabled<-FALSE;cfg$reference_scenario<-1
 validate_shared_adaptation(cfg);cfg
}
register_shared_adaptation_server<-function(input,output,session,controller){
 observeEvent(input$sp_test_basis,{if(input$sp_test_basis=="cohort_riskset")updateSelectInput(session,"sp_enroll_mode",selected="batch")},ignoreInit=TRUE)
 unit<-reactive(input$sp_unit %||% "months");previous<-reactiveVal("months");prefixes<-c("spm_","spdc_","spdt_")
 register_parameter_controls(input,output,session,unit,prefixes,simulation_defaults,simulation_parameter_model)
 raw<-reactive({req(input$sp_file);read_adtte(input$sp_file$datapath,input$sp_file$name)})
 output$sp_group_ui<-renderUI({d<-tryCatch(raw(),error=function(e)NULL);cols<-names(d);old<-isolate(input$sp_group_column);selectInput("sp_group_column","组别变量",cols,if(length(old)==1&&old %in% cols)old else if("TRTP" %in% cols)"TRTP" else head(cols,1))})
 observe({req(input$sp_purpose!="fixed_truth",input$sp_file,input$sp_group_column);d<-tryCatch(sp_observed_source(input),error=function(e)NULL);labs<-if(is.null(d))character() else sort(unique(d$data$group));old<-isolate(input$sp_control);updateSelectInput(session,"sp_control",choices=labs,selected=if(length(old)==1&&old %in% labs)old else head(labs,1))})
 output$sp_ia_note<-renderUI({if(is.null(input$sp_file))return(p(class="field-note","实际IA模式需患者资料；设计模式不需上传。"));d<-tryCatch(sp_observed_source(input),error=function(e)list(error=conditionMessage(e)));p(class="field-note",if(!is.null(d$error))d$error else paste0("IA观察",nrow(d$data),"人、",sum(d$data$event),"事件；运行时冻结当前规范记录。"))})
 observeEvent(input$sp_primary,{old<-isolate(input$sp_extra);x<-setdiff(names(sp_names),input$sp_primary);updateCheckboxGroupInput(session,"sp_extra",choices=setNames(x,unname(sp_names[x])),selected=intersect(old,x))},ignoreInit=TRUE)
 observe({if(input$sp_purpose=="observed"&&input$sp_run_source!="config_json")nav_hide("sp_tabs","sp_exports",session=session) else nav_show("sp_tabs","sp_exports",session=session)})
 observeEvent(input$sp_unit,{
  to<-unit();from<-previous();if(to==from)return();ratio<-time_factor(from)/time_factor(to);u<-time_label(to)
  convert<-function(id,kind,label=NULL){x<-isolate(input[[id]]);if(is.null(x))return();v<-tryCatch(switch(kind,duration=x*ratio,rate=x/ratio,log_time=x+log(ratio),duration_text=paste(format(parse_unit_numbers(x)*ratio,digits=16,trim=TRUE),collapse=","),rate_text=paste(format(parse_unit_numbers(x)/ratio,digits=16,trim=TRUE),collapse=",")),error=function(e)x);freezeReactiveValue(input,id);if(kind %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,label),value=v) else updateNumericInput(session,id,label=parameter_label(id,label),value=v)}
  convert("sp_cut","duration",paste0("实际IA时间（",u,"）"));convert("sp_max","duration",paste0("最大窗口（",u,"）"));convert("sp_enroll_rate","rate",paste0("总体入组率（人/",u,"）"))
  for(pre in prefixes){for(kind in names(base_unit_fields))for(k in intersect(base_unit_fields[[kind]],names(simulation_defaults(from)))){label<-switch(k,median=paste0("中位时间（",u,"）"),median2=paste0("第二成分中位时间（",u,"）"),eta=paste0("Weibull尺度eta（",u,"）"),log_mu=paste0("对数位置mu（",u,"）"),exp_rate=paste0("指数风险率（每",u,"）"),g_rate=paste0("初始风险率（每",u,"）"),g_shape=paste0("Gompertz形状（每",u,"）"),parameter_cuts=paste0("风险切点（",u,"）"),parameter_rates=paste0("各段风险率（每",u,"）"),dropout_rate=paste0("独立退出率（每",u,"）"),drop_period=paste0("退出概率窗口（",u,"）"),NULL);convert(paste0(pre,k),kind,label)};convert(paste0(pre,"survival_time"),"duration",paste0("随访时点（",u,"）"));convert(paste0(pre,"pwe_last_rate"),"rate",paste0("末段风险率（每",u,"）"))};previous(to)
 },ignoreInit=TRUE)
 workbench<-register_research_workbench_server(input,output,session,controller,"sp_","shared_patient_adaptation","shared_adaptation","sp_tabs",function()sp_parameter_input(input),sp_sample,sp_report,"run_shared_adaptation",extra="recovery",input_tabs=c("sp_baseline","sp_plan","sp_analysis"))
 output$sp_sample_risksets<-renderDT({a<-workbench$sample();study_table(a$risksets %||% data.frame())})
 output$sp_risksets_download<-downloadHandler(filename="shared_selected_event_risksets.csv",content=function(file){a<-workbench$sample();write.csv(a$risksets %||% data.frame(),file,row.names=FALSE)})
 observed<-reactiveVal(NULL);error<-reactiveVal(NULL)
 observeEvent(input$sp_observed_run,{error(NULL);tryCatch({cfg<-sp_parameter_input(input);r<-sp_run_observed(cfg$payload$data,cfg);observed(r);nav_select("sp_tabs","sp_results",session=session)},error=function(e){error(conditionMessage(e));nav_select("sp_tabs","sp_results",session=session)})},ignoreInit=TRUE)
 output$sp_observed_status<-renderUI({r<-observed();p(class="field-note",if(!is.null(error()))paste0("本次未生成结果：",error(),"；保留最近成功结果。") else if(is.null(r))"填写当前IA和原模型后计算，不运行未来模拟。" else paste0("保存于",r$created_at,"；开发稿待复核。",r$note))})
 output$sp_observed_table<-renderDT({r<-observed();req(r);study_table(r$table)})
 output$sp_observed_csv<-downloadHandler(filename="shared_observed_inference.csv",content=function(file){r<-observed();req(r);write.csv(r$table,file,row.names=FALSE)})
 output$sp_observed_json<-downloadHandler(filename="shared_observed_inference_draft.json",content=function(file){r<-observed();req(r);jsonlite::write_json(sp_observed_bundle(r),file,auto_unbox=TRUE,pretty=TRUE,digits=NA,na="null")})
 output$sp_observed_script<-downloadHandler(filename="shared_observed_replay_draft.R",content=function(file){r<-observed();req(r);dump<-function(x)paste(capture.output(dput(x,control=c("keepNA","keepInteger","niceNames","showAttributes","hexNumeric"))),collapse="\n");writeLines(c("# Matching source root; draft replay is unverified.",'source("R/research_sources.R")',paste0("cfg <- ",dump(r$config)),paste0("data <- ",dump(r$observed)),'r <- sp_run_observed(data,cfg)','write.csv(r$table,"shared_observed_replayed.csv",row.names=FALSE)'),file,useBytes=TRUE)})
 output$sp_template<-downloadHandler(filename="shared_adaptation_scenarios.csv",content=function(file)write.csv(data.frame(label=c("null","active"),n=500,hr=c(1,.67),enroll_scale=1,dropout_scale=1),file,row.names=FALSE))
 invisible(observed)
}
