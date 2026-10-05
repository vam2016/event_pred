joint_research_input <- function(input) {
  u<-input$jr_unit %||% "months";f<-time_factor(u)
  base_input<-list(js_unit=u,js_origin=input$jr_origin,js_n=10,js_groups="2",js_allocation=input$jr_allocation,js_clock=input$jr_clock,
    js_drop_control=input$jr_drop_control,js_drop_treatment=input$jr_drop_treatment,js_enroll_mode=input$jr_enroll_mode,
    js_enroll_rate=input$jr_enroll_rate,js_enroll_cuts=input$jr_enroll_cuts,js_enroll_rates=input$jr_enroll_rates,
    js_cut_mode="fixed",js_cuts="1",js_fixed="1",js_q01=1,js_q02=1,js_q12=1,js_reps=1,js_seed=input$jr_seed)
  for(e in names(joint_transition_names))for(k in c("design_model","exp_input","median","exp_rate","weibull_input","eta","shape","parameter_cuts","parameter_rates"))base_input[[paste0("js",e,"_",k)]]<-input[[paste0("jr",e,"_",k)]]
  base<-joint_config_input(base_input)
  analyses<-setNames(lapply(c("pfs","os"),function(e){pre<-paste0("jr_",e,"_");m<-input[[paste0(pre,"method")]];a<-list(method=m)
    if(m=="fh"){a$rho<-input[[paste0(pre,"rho")]];a$gamma<-input[[paste0(pre,"gamma")]]}
    if(m %in% c("rmst","survival"))a$tau_day<-input[[paste0(pre,"tau")]]*f;a}),c("PFS","OS"))
  cfg<-list(research_family="joint_endpoints",mode="fixed_truth",design_mode="fixed",effect_mode="joint_transition",paramcd="PFS+OS",display_unit=u,engine_unit="days",origin=as.character(input$jr_origin),joint_base=base,analyses=analyses,
    treatment_fraction=input$jr_allocation,reps=input$jr_reps,seed=input$jr_seed,cut_rule=input$jr_cut_rule,miss_policy=if(input$jr_cut_rule=="fixed")"analyze" else input$jr_miss,
    max_day=if(input$jr_cut_rule!="fixed")input$jr_max*f else NULL,primary=input$jr_primary,success_goal=if(input$jr_primary=="co_primary")"both" else input$jr_success_goal,
    alpha=input$jr_alpha,sided=input$jr_sided,compare_policies=if(isTRUE(input$jr_compare))setdiff(input$jr_extra,input$jr_primary) else character(),
    scenario_source=input$jr_scenario_source,reference_scenario=1,filter_enabled=FALSE)
  if("bonferroni" %in% jr_policies(cfg))cfg$pfs_alpha_weight<-input$jr_weight
  if("fixed_sequence" %in% jr_policies(cfg))cfg$first_endpoint<-input$jr_first
  if(cfg$scenario_source=="csv"){
    if(is.null(input$jr_file))stop("请选择情景CSV。");cfg$scenario_table<-read.csv(input$jr_file$datapath,stringsAsFactors=FALSE,check.names=FALSE);cfg$scenario_filename<-input$jr_file$name
    cfg$table_unit<-if(cfg$cut_rule=="fixed")input$jr_csv_unit %||% "months" else "days"
  } else {
    cfg$profiles<-jr_profiles(input$jr_profiles);cfg$axes<-list(n=parse_unit_numbers(input$jr_n_values),enroll_scale=parse_unit_numbers(input$jr_enroll_scales),dropout_scale=parse_unit_numbers(input$jr_dropout_scales))
    if(cfg$cut_rule=="fixed")cfg$axes$dco<-parse_unit_numbers(input$jr_dco_values)
    if(cfg$cut_rule %in% c("pfs","both","first"))cfg$axes$target_pfs<-parse_unit_numbers(input$jr_pfs_targets)
    if(cfg$cut_rule %in% c("os","both","first"))cfg$axes$target_os<-parse_unit_numbers(input$jr_os_targets)
  }
  validate_joint_research(cfg);cfg
}
register_joint_research_server <- function(input,output,session,controller) {
  result<-reactive({r<-controller$result();if(!is.null(r)&&jr_is(r$config))r else NULL});imported<-reactiveVal(NULL);import_note<-reactiveVal("请选择联合研究的新格式配置JSON。")
  unit<-reactive(input$jr_unit %||% "months");previous<-reactiveVal("months");prefixes<-paste0("jr",names(joint_transition_names),"_")
  register_parameter_controls(input,output,session,unit,prefixes,joint_defaults,parameter_input_model)
  observeEvent(input$jr_config_file,{imported(NULL);tryCatch({cfg<-batch_read_config(input$jr_config_file$datapath);if(!jr_is(cfg))stop("文件不是联合终点研究配置。");imported(cfg);import_note(paste0("已载入",nrow(jr_scenarios(cfg)),"情景，每情景",cfg$reps,"轮；主规则",jr_policy_names[[cfg$primary]],"；显示单位",time_label(cfg$display_unit),"。页面其他模拟输入不参与本次运行。"))},error=function(e)import_note(conditionMessage(e)))},ignoreInit=TRUE)
  output$jr_import_note<-renderUI(p(class="field-note",import_note()))
  observe({for(id in c("jr_baseline","jr_scenarios","jr_analysis"))if((input$jr_run_source %||% "ui")=="config_json")nav_hide("jr_tabs",id,session=session) else nav_show("jr_tabs",id,session=session)})
  observeEvent(input$jr_run_source,{nav_select("jr_tabs",if((input$jr_run_source %||% "ui")=="config_json")"jr_results" else "jr_baseline",session=session)},ignoreInit=TRUE)
  observeEvent(input$jr_primary,{choices<-jr_policy_names[names(jr_policy_names)!=input$jr_primary];old<-isolate(input$jr_extra);updateCheckboxGroupInput(session,"jr_extra",choices=setNames(names(choices),unname(choices)),selected=intersect(old,names(choices)))},ignoreInit=TRUE)
  observeEvent(input$jr_unit,{
    to<-unit();from<-previous();if(to==from)return();ratio<-time_factor(from)/time_factor(to);u<-time_label(to)
    specs<-list(duration=c("jr_max","jr_pfs_tau","jr_os_tau"),duration_text=c("jr_enroll_cuts","jr_dco_values","jr_reference_times"),rate=c("jr_enroll_rate","jr_drop_control","jr_drop_treatment"),rate_text=c("jr_enroll_rates"))
    for(pre in prefixes){specs$duration<-c(specs$duration,paste0(pre,c("median","eta")));specs$rate<-c(specs$rate,paste0(pre,"exp_rate"));specs$duration_text<-c(specs$duration_text,paste0(pre,"parameter_cuts"));specs$rate_text<-c(specs$rate_text,paste0(pre,"parameter_rates"))}
    label_for<-function(id){if(grepl("median$",id))paste0("潜在转移时钟中位时间（",u,"）") else if(grepl("eta$",id))paste0("尺度eta（",u,"）") else if(grepl("exp_rate$|drop_control$|drop_treatment$|parameter_rates$",id))paste0("风险率（每",u,"）") else if(grepl("tau$",id))paste0("预定风险年龄tau（",u,"）") else switch(id,jr_max=paste0("最大研究窗口（",u,"）"),jr_dco_values=paste0("共同DCO列表（",u,"）"),jr_reference_times=paste0("风险年龄时点（",u,"）"),jr_enroll_rate=paste0("总体入组率（人/",u,"）"),jr_enroll_rates=paste0("各段入组率（人/",u,"）"),paste0("切点（",u,"）"))}
    for(k in names(specs))for(id in specs[[k]]){v<-isolate(input[[id]]);if(is.null(v))next
      value<-tryCatch(if(k %in% c("duration_text","rate_text"))paste(format(parse_unit_numbers(v)*if(k=="duration_text")ratio else 1/ratio,digits=16,trim=TRUE),collapse=",") else v*if(k=="duration")ratio else 1/ratio,error=function(e)v)
      freezeReactiveValue(input,id);if(k %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,label_for(id)),value=value) else updateNumericInput(session,id,label=parameter_label(id,label_for(id)),value=value)
    };previous(to)
  },ignoreInit=TRUE)
  observeEvent(input$jr_run,{
    tryCatch({if(!is.null(controller$job()))stop("本会话已有后台任务，请先等待或取消。")
      cfg<-if((input$jr_run_source %||% "ui")=="config_json")imported() else joint_research_input(input);if(is.null(cfg))stop("请先载入有效联合研究配置。")
      store<-batch_create_run(cfg,input$ba_workspace_token,input$jr_run_name);controller$start_saved(cfg,store=store);controller$workspace$refresh()
    },error=function(e){controller$error(conditionMessage(e));nav_select("jr_tabs","jr_results",session=session)})
  },ignoreInit=TRUE)
  observeEvent(input$jr_cancel,{j<-controller$job();if(!is.null(j))file.create(file.path(j$dir,"cancel.flag"))},ignoreInit=TRUE)
  observeEvent(input$jr_library,{updateSelectInput(session,"task",selected="batch");nav_select("ba_tabs","ba_library",session=session)},ignoreInit=TRUE)
  output$jr_status<-renderUI({r<-result();e<-controller$error();tagList(if(!is.null(e))p(class="field-note",e),if(!is.null(r))p(class="field-note",paste0(r$run_title %||% "联合研究"," · v",r$version,"待复核；主规则",jr_policy_names[[r$config$primary]],"；成功指标",r$config$success_goal,"；已处理",r$completed,"/",r$total,"；",r$status,"。时间按保存单位显示，CSV为日。")) else p(class="field-note","运行后显示冻结结果；旧结果不随当前页面参数改变。"))})
  display<-function(d,r){if(is.null(d)||!ncol(d))return(study_table(data.frame(记录=character())))
    for(k in names(d)[grepl("_day$|^DCO_DAY$",names(d))]){d[[k]]<-d[[k]]/time_factor(r$config$display_unit);names(d)[names(d)==k]<-paste0(k,"（",time_label(r$config$display_unit),"）")}
    if("estimate_unit" %in% names(d)){ix<-d$estimate_unit=="days";for(k in intersect(names(d),c("estimate","se","lower","upper")))d[[k]][ix]<-d[[k]][ix]/time_factor(r$config$display_unit);d$estimate_unit[ix]<-time_label(r$config$display_unit)}
    study_table(d)
  }
  for(k0 in c("overview","policy_overview","policy_pairs","resources","endpoint_correlations"))local({k<-k0;output[[paste0("jr_",k)]]<-renderDT({r<-result();req(r);display(r[[k]],r)})})
  observe({r<-result();if(is.null(r))return();old<-isolate(input$jr_view_scenario);choices<-setNames(r$scenarios$SCENARIO,paste(r$scenarios$SCENARIO,r$scenarios$label));updateSelectInput(session,"jr_view_scenario",choices=choices,selected=if(length(old)==1&&old %in% as.character(r$scenarios$SCENARIO))old else r$scenarios$SCENARIO[1])})
  observe({r<-result();req(r,input$jr_view_scenario);v<-if(nrow(r$rows))r$rows$SIMID[r$rows$SCENARIO==as.integer(input$jr_view_scenario)&r$rows$generated] else integer();old<-isolate(input$jr_view_trial);updateSelectInput(session,"jr_view_trial",choices=v,selected=if(length(old)==1&&old %in% as.character(v))old else head(v,1))})
  for(k0 in c("rows","method_rows","policy_rows"))local({k<-k0;output[[paste0("jr_",k)]]<-renderDT({r<-result();req(r,input$jr_view_scenario);d<-r[[k]];if(nrow(d))d<-d[d$SCENARIO==as.integer(input$jr_view_scenario),,drop=FALSE];display(d,r)})})
  reference<-reactiveVal(NULL);reference_error<-reactiveVal(NULL)
  observeEvent(input$jr_reference_run,{reference_error(NULL);tryCatch({r<-result();if(is.null(r))stop("先提交或载入联合研究配置。");a<-jr_marginal_reference(r,as.integer(input$jr_view_scenario),parse_unit_numbers(input$jr_reference_times)*time_factor(unit()),unit());reference(a)},error=function(e)reference_error(conditionMessage(e)))},ignoreInit=TRUE)
  observeEvent(list(result()$run_id,input$jr_view_scenario),{reference(NULL);reference_error(NULL)},ignoreInit=TRUE)
  output$jr_reference_note<-renderUI({a<-reference();e<-reference_error();tagList(if(!is.null(e))p(class="field-note",e),if(!is.null(a))p(class="field-note",a$note))})
  output$jr_reference<-renderDT({a<-reference();req(a);display(a$table,list(config=list(display_unit=a$display_unit)))})
  output$jr_reference_download<-downloadHandler(filename="joint_marginal_reference_days.csv",content=function(file){a<-reference();req(a);write.csv(a$table,file,row.names=FALSE)})
  output$jr_reference_config<-downloadHandler(filename="joint_marginal_reference_config.json",content=function(file){a<-reference();req(a);a$config<-batch_config_document(a$config);jsonlite::write_json(a,file,auto_unbox=TRUE,pretty=TRUE,digits=NA,na="null")})
  sample<-reactive({r<-result();req(r,input$jr_view_scenario,input$jr_view_trial);jr_sample(r,as.integer(input$jr_view_scenario),as.integer(input$jr_view_trial))})
  output$jr_template<-downloadHandler(filename="joint_research_scenarios.csv",content=function(file){cfg<-list(cut_rule=input$jr_cut_rule);d<-data.frame(label=c("joint_null","active"),n=c(200,300),q01=c(1,.7),q02=1,q12=c(1,.85),enroll_scale=1,dropout_scale=1)
    if(cfg$cut_rule=="fixed")d$dco<-36*time_factor("months")/time_factor(input$jr_csv_unit %||% "months")
    if(cfg$cut_rule %in% c("pfs","both","first"))d$target_pfs<-150
    if(cfg$cut_rule %in% c("os","both","first"))d$target_os<-100
    write.csv(d[,jr_fields(cfg),drop=FALSE],file,row.names=FALSE)})
  for(k0 in c("scenarios","rows","method_rows","policy_rows","overview","policy_overview","policy_pairs","resources","endpoint_correlations"))local({k<-k0;output[[paste0("jr_",k,"_download")]]<-downloadHandler(filename=paste0("joint_research_",k,"_days.csv"),content=function(file){r<-result();req(r);write.csv(r[[k]],file,row.names=FALSE)})})
  output$jr_config_download<-downloadHandler(filename="joint_research_config.json",content=function(file){r<-result();req(r);jsonlite::write_json(batch_config_document(r$config),file,auto_unbox=TRUE,pretty=TRUE,digits=NA)})
  output$jr_result_download<-downloadHandler(filename="joint_research_result_draft.rds",content=function(file){r<-result();req(r);saveRDS(r,file,version=3)})
  output$jr_report_download<-downloadHandler(filename="joint_research_report_draft.md",content=function(file){r<-result();req(r);writeLines(jr_report(r),file,useBytes=TRUE)},contentType="text/markdown; charset=utf-8")
  output$jr_script_download<-downloadHandler(filename="replay_joint_research_draft.R",content=function(file){r<-result();req(r);writeLines(jr_reproduction_script(r),file,useBytes=TRUE)})
  for(k0 in c("observed","truth"))local({k<-k0;output[[paste0("jr_",k,"_download")]]<-downloadHandler(filename=paste0("joint_selected_",k,"_days.csv"),content=function(file)write.csv(sample()[[k]],file,row.names=FALSE))})
  output$jr_adtte_download<-downloadHandler(filename="joint_selected_adtte.csv",content=function(file){a<-sample();write.csv(joint_adtte(a$observed,a$study_config),file,row.names=FALSE)})
  for(k0 in c("states","intervals"))local({k<-k0;output[[paste0("jr_",k,"_download")]]<-downloadHandler(filename=paste0("joint_selected_",k,"_days.csv"),content=function(file)write.csv(sample()$states[[k]],file,row.names=FALSE))})
  invisible(result)
}
