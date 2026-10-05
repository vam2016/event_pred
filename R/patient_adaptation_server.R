patient_adaptive_start_job <- function(cfg,root=getwd(),data=NULL) {
  if(cfg$purpose=="patient_prediction")patient_prediction_validate(data,cfg) else patient_adaptive_validate(cfg);dir<-tempfile("event-pred-patient-adaptive-");dir.create(dir);saveRDS(if(cfg$purpose=="patient_prediction")list(config=cfg,data=data) else cfg,file.path(dir,"config.rds"))
  p<-tryCatch(processx::process$new(file.path(R.home("bin"),"Rscript"),c(file.path(root,"scripts/patient_adaptive_worker.R"),file.path(dir,"config.rds"),dir),wd=root,
    stdout=file.path(dir,"stdout.log"),stderr=file.path(dir,"stderr.log"),supervise=TRUE,cleanup=TRUE,cleanup_tree=TRUE),error=function(e){unlink(dir,recursive=TRUE);stop(e)})
  list(process=p,dir=dir,config=cfg)
}
patient_adaptive_report <- function(r) {
  a<-r$plan;c<-r$config
  if(c$purpose=="patient_prediction")return(patient_prediction_report(r))
  c('# 独立患者队列的两阶段事件数重估','',paste('版本',r$version),
    '阶段1观察在预定IA冻结；阶段2检验仅纳入IA后独立新入组者。阶段1患者IA后的事件不用于组合检验。该队列划分和阶段截点需在观察数据前安排。',
    '',sprintf('D1=%d；Dplan=%d；Dmax=%d；原定固定w1=%.8f、w2=%.8f；单侧alpha=%g。',c$d1,c$d_plan,c$d_max,a$w1,a$w2,c$alpha),
    sprintf('重估HR=%g；目标CP=%g；规则=%s。原计划Final作为下限；无IA提前拒绝。',c$hr_assumed,c$cp_target,c$rule),
    '新队列事件目标为D2，组合检验事件目标为D1+D2，不是全研究所有患者的累计事件。CP用PH事件信息近似，患者阶段log-rank使用实际风险集与方差。',
    '',if(c$purpose=='patient_interim')c('## 当前IA',capture.output(print(r$ia,row.names=FALSE)),capture.output(print(r$decision,row.names=FALSE)),
      '当前IA只用于事件目标计算，不评价无条件Ⅰ类错误，也不模拟患者后续拒绝概率。') else c('## 从起点的完整患者试验',paste('状态',r$status,'；完成',r$completed,'/',r$total),capture.output(print(r$summary,row.names=FALSE)),
      '阶段内log-rank使用渐近正态p值，有限样本不声称精确alpha。未达事件目标按预设规则关闭且不拒绝；无效检验与未完成轮次保留在全部请求轮次概率界中。',
      'Wilson与MCSE仅描述有效完成决策的Monte Carlo误差。原计划与重估共用患者轨迹，按不同事件目标截取，H0下决策不必逐轮相同。'),
    '', '方法、条件错误与队列限制见工作手册第21章。时间字段按连续日分析，导出同时提供选定单位。',
    '', '## 配置快照','```json',jsonlite::toJSON(c,auto_unbox=TRUE,pretty=TRUE,digits=NA),'```')
}
register_patient_adaptive_server <- function(input,output,session,batch_bridge=NULL) {
  result<-reactiveVal(NULL);partial<-reactiveVal(NULL);error<-reactiveVal(NULL);job<-reactiveVal(NULL);progress<-reactiveVal(NULL);dirs<-character()
  unit<-reactive(input$pa_unit %||% "months");last_unit<-reactiveVal("months")
  register_parameter_controls(input,output,session,unit,c("pa_","padc_","padt_","pgc_","pgt_","pgdc_","pgdt_"),simulation_defaults,simulation_parameter_model)
  observeEvent(input$pa_unit,{
    to<-unit();from<-last_unit();if(to==from)return();ratio<-time_factor(from)/time_factor(to);u<-time_label(to)
    convert<-function(id,kind,label=NULL) {
      v<-isolate(input[[id]]);if(is.null(v))return()
      new<-tryCatch(switch(kind,duration=v*ratio,rate=v/ratio,log_time=v+log(ratio),duration_text=paste(format(parse_unit_numbers(v)*ratio,digits=16),collapse=","),rate_text=paste(format(parse_unit_numbers(v)/ratio,digits=16),collapse=",")),error=function(e)v)
      freezeReactiveValue(input,id)
      if(kind %in% c("duration_text","rate_text"))updateTextInput(session,id,label=parameter_label(id,label),value=new) else updateNumericInput(session,id,label=parameter_label(id,label),value=new)
    }
    for(id in c("pa_cut","pa_window1","pa_window2"))convert(id,"duration",paste0(c(pa_cut="IA DCO（研究",pa_window1="阶段1最大窗口（",pa_window2="IA后新队列最大窗口（")[[id]],u,if(id=="pa_cut")"数）" else "）"))
    for(id in c("pa_rate1","pa_rate2"))convert(id,"rate",paste0(if(id=="pa_rate1")"阶段1" else "新队列","总体入组率（人/",u,"）"))
    for(pre in c("pa_","padc_","padt_","pgc_","pgt_","pgdc_","pgdt_")) {
      for(kind in names(base_unit_fields))for(id in intersect(base_unit_fields[[kind]],names(simulation_defaults()))) {
        label<-switch(id,median=paste0("中位时间 mPFS / mOS（",u,"）"),median2=paste0("第二成分中位时间（",u,"）"),eta=paste0("Weibull尺度eta（",u,"）"),log_mu=paste0("对数位置mu=log(m/",u,")"),exp_rate=paste0("指数风险率（每",u,"）"),g_rate=paste0("初始风险率b（每",u,"）"),g_shape=paste0("Gompertz形状g（每",u,"）"),parameter_cuts=paste0("风险切点（",u,"）"),parameter_rates=paste0("各段风险率（每",u,"）"),dropout_rate=paste0("独立脱落风险率（每",u,"）"),drop_period=paste0("脱落概率窗口（",u,"）"),NULL)
        convert(paste0(pre,id),kind,label)
      }
      convert(paste0(pre,"survival_time"),"duration",paste0("随访时点（",u,"）"));convert(paste0(pre,"pwe_last_rate"),"rate",paste0("末段风险率（每",u,"）"))
    }
    convert("pg_window2","duration",paste0("IA后最大窗口（",u,"）"));convert("pg_rate2","rate",paste0("总体入组率（人/",u,"）"))
    for(id in c("pg_cuts","pg_enroll_cuts"))convert(id,"duration_text",paste0(if(id=="pg_cuts")"PWE切点（随访" else "入组切点（IA后",u,"数）"))
    convert("pg_enroll_rates","rate_text",paste0("各段总体入组率（人/",u,"）"));convert("pg_tail_rate","rate",paste0("KM尾部风险率（每",u,"）"))
    convert("pg_prior_rate","duration",paste0("事件Gamma先验rate（受试者·",u,"）"));convert("pg_log_eta_mean","log_time",paste0("log(eta/",u,")先验均值"))
    for(id in c("pg_recruit_start","pg_recruit_end"))convert(id,"duration",paste0(if(id=="pg_recruit_start")"历史入组窗口起点（研究" else "历史入组窗口终点（研究",u,"数）"))
    convert("pg_enroll_prior_rate","duration",paste0("入组Gamma先验rate（",u,"）"));convert("pg_drop_prior_rate","duration",paste0("脱落Gamma先验rate（受试者·",u,"）"))
    last_unit(to)
  },ignoreInit=TRUE)
  raw<-reactive({req(input$pa_file);read_adtte(input$pa_file$datapath,input$pa_file$name)})
  output$pa_endpoint_ui<-renderUI({x<-tryCatch(raw(),error=function(e)NULL);selectInput("pa_endpoint","终点 PARAMCD",unique(as.character(x$PARAMCD)))})
  output$pa_group_ui<-renderUI({x<-tryCatch(raw(),error=function(e)NULL);v<-names(x);selectInput("pa_group_column","组别变量",v,if("TRTP" %in% v)"TRTP" else if("TRTA" %in% v)"TRTA" else "")})
  ia_source<-reactive({
    if(identical(input$pa_source,"demo")) {d<-validate_data(demo_data(),540);d$group<-rep(c("Control","Treatment"),length.out=nrow(d));return(list(data=d,cut=540,source="demo",origin="2025-01-01"))}
    if(is.null(input$pa_file))stop("选择阶段1ADTTE文件，或切换合成示例。")
    cut<-input$pa_cut*time_factor(unit())
    d<-normalize_adtte(raw(),input$pa_endpoint,as.character(input$pa_origin),cut,as.numeric(input$pa_offset),parse_unit_numbers(input$pa_dropout_codes),input$pa_flag %||% "",input$pa_flag_value %||% "Y",input$pa_date_encoding %||% "iso","strict",input$pa_aval_unit %||% "days",group_column=input$pa_group_column)
    list(data=d,cut=cut,source="adtte",origin=as.character(input$pa_origin))
  })
  output$pa_control_ui<-renderUI({d<-tryCatch(ia_source()$data,error=function(e)NULL);labs<-sort(unique(d$group));selectInput("pa_control","Control组",labs,if("Control" %in% labs)"Control" else labs[1])})
  output$pa_data_note<-renderUI({tryCatch({z<-ia_source();p(class="field-note",sprintf("%d人；%d个IA事件；IA=%.6g%s。D1需与当前IA事件数一致，并沿用预定方案。",nrow(z$data),sum(z$data$event),z$cut/time_factor(unit()),time_label(unit())))},error=function(e)p(class="field-note",conditionMessage(e)))})
  config<-function() {
    c<-list(purpose=input$pa_purpose,d1=input$ad_d1,d_plan=input$ad_plan,d_max=input$ad_max,p=input$ad_p,alpha=input$ad_alpha,hr_assumed=input$ad_hr,cp_target=input$ad_cp,rule=input$ad_rule,display_unit=unit(),engine_unit="days")
    if(c$rule=="promising")c$cp_min<-input$ad_cp_min
    if(c$purpose %in% c("patient_interim","patient_prediction")) {z<-ia_source();c$cut<-z$cut;c$control<-input$pa_control;c$source<-z$source
      if(c$purpose=="patient_interim")return(list(config=c,data=z$data))
      f<-time_factor(unit());values<-function(pre){d<-simulation_defaults(unit());setNames(lapply(names(d),function(id)input[[paste0(pre,id)]] %||% d[[id]]),names(d))}
      c$origin<-z$origin;c$n2<-input$pg_n2;c$window2<-input$pg_window2*f;c$process_uncertainty<-input$pg_process %||% "fixed";c$enrollment<-if(c$process_uncertainty=="gamma")"constant" else input$pg_enrollment
      if(c$enrollment=="constant"&&c$process_uncertainty!="gamma")c$rate2<-input$pg_rate2/f
      if(c$enrollment=="piecewise"){c$enroll_cuts<-parse_unit_numbers(input$pg_enroll_cuts)*f;c$enroll_rates<-parse_unit_numbers(input$pg_enroll_rates)/f}
      if(c$process_uncertainty=="fixed")c$dropout_rates<-c(dropout_input_rate(values("pgdc_")),dropout_input_rate(values("pgdt_")))/f else {
        c$drop_prior_shape<-input$pg_drop_prior_shape;c$drop_prior_rate<-input$pg_drop_prior_rate*f
        if(c$process_uncertainty=="gamma") {
          c$recruit_start<-input$pg_recruit_start*f;c$recruit_end<-input$pg_recruit_end*f;c$recruit_complete<-isTRUE(input$pg_recruit_complete)
          c$enroll_prior_shape<-input$pg_enroll_prior_shape;c$enroll_prior_rate<-input$pg_enroll_prior_rate*f
        }
      }
      if(isTRUE(input$pg_compare))c$scenarios<-patient_prediction_parse_scenarios(input$pg_scenario_csv)
      c$model_mode<-input$pg_model_mode;c$uncertainty<-if(c$model_mode=="manual")"plugin" else input$pg_uncertainty
      if(c$model_mode=="manual") {
        v<-lapply(c("pgc_","pgt_"),values);c$manual_entered<-v;c$manual_models<-lapply(v,function(v)simulation_parameter_model(v$design_model,v,unit()))
      } else {
        c$fit_method<-input$pg_fit_method
        if(c$fit_method=="pwe")c$cuts<-parse_unit_numbers(input$pg_cuts)*f
        if(c$fit_method=="km_tail")c$tail_rate<-input$pg_tail_rate/f
        if(c$uncertainty=="gamma"){c$prior_shape<-input$pg_prior_shape;c$prior_rate<-input$pg_prior_rate*f}
        if(c$uncertainty=="bayes_weibull") {
          for(id in c("log_eta_mean","log_eta_sd","log_shape_mean","log_shape_sd","mcmc_chains","mcmc_warmup","mcmc_draws"))c[[id]]<-input[[paste0("pg_",id)]]
          c$log_eta_mean<-c$log_eta_mean+log(f)
        }
      }
      c$reps<-input$pg_reps;c$seed<-input$pg_seed
      return(list(config=c,data=z$data))
    }
    f<-time_factor(unit());values<-function(pre){d<-simulation_defaults(unit());setNames(lapply(names(d),function(id)input[[paste0(pre,id)]] %||% d[[id]]),names(d))}
    v<-values("pa_");c$control_model<-simulation_parameter_model(v$design_model,v,unit());c$control_entered<-v
    c$n1<-input$pa_n1;c$n2<-input$pa_n2;c$window1<-input$pa_window1*f;c$window2<-input$pa_window2*f;c$enrollment<-input$pa_enrollment
    if(c$enrollment=="constant"){c$rate1<-input$pa_rate1/f;c$rate2<-input$pa_rate2/f}
    c$dropout_rates<-c(dropout_input_rate(values("padc_")),dropout_input_rate(values("padt_")))/f
    c$hr_true<-input$pa_true_hr;c$reps<-input$pa_reps;c$seed<-input$pa_seed
    list(config=c)
  }
  observeEvent(input$pg_batch_freeze,{tryCatch({if(is.null(batch_bridge))stop("批量入口未接入。");z<-config();z$config$paramcd<-input$pa_endpoint %||% "PFS";batch_bridge$stage(z$data,z$config,"new_cohort")},error=function(e){error(conditionMessage(e));showNotification(conditionMessage(e),type="error",duration=15)})},ignoreInit=TRUE)
  observeEvent(input$ad_run,{
    if(!identical(input$ad_engine,"patient")||!is.null(job()))return()
    error(NULL)
    tryCatch({z<-config();if(z$config$purpose=="patient_prediction")patient_prediction_validate(z$data,z$config) else patient_adaptive_validate(z$config,z$data)
      if(z$config$purpose=="patient_interim") {result(patient_adaptive_interim(z$data,z$config));partial(NULL)} else {
        j<-patient_adaptive_start_job(z$config,data=z$data);dirs<<-c(dirs,j$dir);partial(NULL);job(j);progress(list(completed=0,total=if(z$config$purpose=="patient_prediction")z$config$reps*if(patient_prediction_is_extended(z$config))nrow(patient_prediction_scenarios(z$config)) else 1L else z$config$reps*length(unique(c(1,z$config$hr_true)))))
      }
      nav_select("ad_tabs","results",session=session)
    },error=function(e){error(conditionMessage(e));nav_select("ad_tabs","results",session=session)})
  },ignoreInit=TRUE)
  observeEvent(input$pa_cancel,{j<-job();if(!is.null(j))study_cancel_job(j)},ignoreInit=TRUE)
  observe({j<-job();if(is.null(j))return();invalidateLater(500,session);s<-study_read_job(j)
    if(!is.null(s$state)){progress(s$state[c("completed","total")]);if(!is.null(s$state$trials))partial(s$state)}
    if(isTRUE(s$finished)) {
      if(!is.null(s$result)){result(s$result);partial(NULL)} else {error(s$error);if(!is.null(partial())){r<-partial();r$status<-"interrupted";result(r);partial(NULL)}}
      job(NULL)
    }
  })
  session$onSessionEnded(function(){j<-isolate(job());if(!is.null(j)&&j$process$is_alive())j$process$kill_tree();for(d in dirs)unlink(d,recursive=TRUE)})
  current<-reactive(partial() %||% result())
  output$pa_result_purpose<-renderText(if(is.null(current()))"none" else current()$config$purpose);outputOptions(output,"pa_result_purpose",suspendWhenHidden=FALSE)
  output$pa_status<-renderUI({r<-current();j<-job();g<-progress();tagList(
    if(!is.null(error()))div(class="alert alert-warning",paste0("运行问题：",error(),"。保留最近成功结果。")),
    if(!is.null(j))tagList(div(class="study-progress",tags$progress(value=g$completed,max=g$total),span(sprintf("后台运行：已保存%d / %d轮。下方为最近保存的结果快照。",g$completed,g$total)))),
    if(is.null(j)&&!is.null(r))p(class="field-note",if(r$config$purpose=="patient_interim")"IA重估已完成；使用成功提交的IA记录和原计划。" else sprintf("%s：完成%d / %d次患者试验。",if(r$status=="complete")"运行完成" else "部分结果",r$completed,r$total)),
    if(!is.null(r))p(sprintf("固定权重w1=%.6f、w2=%.6f；组合Z临界值=%.6f。",r$plan$w1,r$plan$w2,r$plan$critical)))})
  outputOptions(output,"pa_status",suspendWhenHidden=FALSE)
  output$pa_busy<-renderText(if(is.null(job()))"false" else "true");outputOptions(output,"pa_busy",suspendWhenHidden=FALSE)
  table<-function(x) {
    labels<-c(selected_d2="新队列选定事件D2",combined_target_events="组合事件目标D1+D2",required_d2="未约束所需新队列事件",cp_original="原计划CP",cp_max="上限CP",cp_selected="选定CP",conditional_error="原条件错误",conditional_critical="阶段2临界值",target_reached="达到目标CP",z1="IA获益Z1",benefit_z="获益Z",variance="得分方差",one_sided_p="阶段1单侧p",control="Control组",action="决策状态",reject="拒绝",z2="新队列Z2",combination_z="组合Z",requested="请求轮次",completed="完成轮次",valid="有效决策",invalid="无效决策",pending="未完成轮次",rejection_lower="全部请求概率下界",rejection_upper="全部请求概率上界",mean_target_events="平均组合事件目标",mean_observed_events="平均检验队列事件",mean_n="平均观察人数",stage1_unreached="阶段1窗口未达",stage2_unreached="阶段2窗口未达",cohort="队列",ia_day="IA研究时间",final_day="研究终止时间",mean_final_day="平均研究终止时间",DCO_DAY="截点研究时间",entry_day="入组研究时间",time_day="观察随访时间",obs_day="末次确认时间",design="设计",rejections="拒绝次数",rejection_rate="有效决策拒绝率",mcse="MCSE",wilson_low="Wilson95%下限",wilson_high="Wilson95%上限",events1="阶段1事件",events2="新队列事件",variance1="阶段1方差",variance2="阶段2方差",group="组别",event="事件指示",status="观察状态",reason="规则状态",rule_reason="重估规则状态",stage1_hit="IA达标",stage2_hit="新队列达标")
    for(id in names(labels))names(x)<-sub(paste0("^",id,"(?=（|$)"),labels[id],names(x),perl=TRUE)
    if("决策状态" %in% names(x))x$决策状态<-unname(c(tested="已执行Final检验",stage1_window_unreached="阶段1窗口未达",stage2_window_unreached="新队列窗口未达",stage1_invalid="阶段1检验无效",stage2_invalid="阶段2检验无效",generation_failed="生成失败")[x$决策状态])
    if("设计" %in% names(x))x$设计<-ifelse(x$设计=="adaptive","重估","原计划")
    if("hypothesis" %in% names(x))x$hypothesis<-ifelse(x$hypothesis=="H0","H0","HR情景")
    for(id in c("规则状态","重估规则状态"))if(id %in% names(x))x[[id]]<-unname(c(maximum_below_cp_min="上限CP低于最低CP，维持原计划",maximum_target_unattainable="到达上限，目标CP未达",increase_to_target="增加至目标CP",original_sufficient="原计划CP足够",stage1_not_analyzed="阶段1未执行检验",generation_failed="生成失败")[x[[id]]])
    for(id in names(x)[vapply(x,is.logical,logical(1))])x[[id]]<-ifelse(is.na(x[[id]]),NA_character_,ifelse(x[[id]],"是","否"))
    names(x)[names(x)=="hypothesis"]<-"假设";names(x)[names(x)=="true_hr"]<-"真实HR";names(x)[names(x)=="n"]<-"人数";names(x)[names(x)=="events"]<-"事件数"
    decimal<-names(x)[vapply(x,function(v)is.numeric(v)&&any(abs(v-round(v))>1e-12,na.rm=TRUE),logical(1))]
    tab<-DT::datatable(x,rownames=FALSE,class="compact nowrap",options=list(scrollX=TRUE,autoWidth=TRUE,pageLength=6,dom="tip",language=list(emptyTable="尚无记录",info="_START_–_END_ / _TOTAL_",infoEmpty="无记录",paginate=list(previous="上一页",`next`="下一页"))))
    if(length(decimal))DT::formatSignif(tab,decimal,digits=5) else tab
  }
  output$pa_ia<-renderDT({r<-current();req(r,r$ia);table(r$ia)})
  output$pa_decision<-renderDT({r<-current();req(r,r$decision);table(r$decision)})
  output$pa_summary<-renderDT({r<-current();req(r,r$summary);table(unit_table(r$summary,r$config$display_unit,"mean_final_day"))})
  output$pa_trials<-renderDT({r<-current();req(r,r$trials);table(unit_table(head(r$trials[,c("SIMID","true_hr","design","z1","selected_d2","z2","combination_z","reject","action","events1","events2","ia_day","final_day")],200),r$config$display_unit,c("ia_day","final_day")))})
  output$pa_observed<-renderDT({r<-current();req(r,r$observed);table(unit_table(do.call(rbind,lapply(split(r$observed,interaction(r$observed$true_hr,r$observed$design,r$observed$cohort)),head,20)),r$config$display_unit,c("entry_day","time_day","obs_day","DCO_DAY")))})
  output$pa_config_download<-downloadHandler(filename="patient_adaptive_config.json",content=function(file){r<-current();req(r);z<-list(version=r$version,config=r$config,plan=r$plan,R_version=R.version.string);if(!is.null(r$data))z$data<-r$data;jsonlite::write_json(z,file,auto_unbox=TRUE,pretty=TRUE,digits=NA)})
  output$pa_result_download<-downloadHandler(filename="patient_adaptive_result.csv",content=function(file){r<-current();req(r);write.csv(if(r$config$purpose=="patient_prediction")r$summary else r$decision %||% unit_export(r$summary,r$config$display_unit,"mean_final_day"),file,row.names=FALSE)})
  output$pa_trials_download<-downloadHandler(filename="patient_adaptive_trials.csv",content=function(file){r<-current();req(r,r$trials);write.csv(unit_export(r$trials,r$config$display_unit,c("ia_day","final_day")),file,row.names=FALSE)})
  output$pa_observed_download<-downloadHandler(filename="patient_adaptive_observed.csv",content=function(file){r<-current();req(r,r$observed);write.csv(unit_export(r$observed,r$config$display_unit,c("entry_day","time_day","obs_day","DCO_DAY")),file,row.names=FALSE)})
  output$pa_script_download<-downloadHandler(filename="patient_adaptive_reproduce.R",content=function(file){r<-current();req(r);writeLines(if(r$config$purpose=="patient_prediction")patient_prediction_reproduction(r) else patient_adaptive_reproduction(r),file,useBytes=TRUE)})
  output$pa_report_download<-downloadHandler(filename="patient_adaptive_report.md",content=function(file){r<-current();req(r);writeLines(patient_adaptive_report(r),file,useBytes=TRUE)})
  prediction<-reactive({r<-current();req(r,identical(r$config$purpose,"patient_prediction"));r})
  output$pg_extended<-renderText(if(!is.null(current())&&isTRUE(current()$extended))"true" else "false");outputOptions(output,"pg_extended",suspendWhenHidden=FALSE)
  output$pg_has_scenarios<-renderText(if(!is.null(current())&&!is.null(current()$scenarios)&&nrow(current()$scenarios)>1)"true" else "false");outputOptions(output,"pg_has_scenarios",suspendWhenHidden=FALSE)
  output$pg_has_process<-renderText(if(!is.null(current())&&!is.null(current()$process_posteriors))"true" else "false");outputOptions(output,"pg_has_process",suspendWhenHidden=FALSE)
  scenario_form<-reactive({patient_prediction_scenarios(list(scenarios=if(isTRUE(input$pg_compare))patient_prediction_parse_scenarios(input$pg_scenario_csv) else NULL,enrollment=if(identical(input$pg_process,"gamma"))"constant" else input$pg_enrollment %||% "batch"))})
  output$pg_scenario_note<-renderUI({tryCatch(p(class="field-note",paste("共",nrow(scenario_form()),"个情景；每情景请求B轮，基准固定保留。")),error=function(e)p(class="field-note",conditionMessage(e)))})
  outputOptions(output,"pg_scenario_note",suspendWhenHidden=FALSE)
  output$pg_scenario_preview<-renderDT({x<-tryCatch(scenario_form(),error=function(e)NULL);req(x);names(x)<-c("情景","Control事件倍率","Treatment事件倍率","入组倍率","Control脱落倍率","Treatment脱落倍率");DT::datatable(x,rownames=FALSE,options=list(scrollX=TRUE,dom="t"))})
  pt<-function(x,r,times=FALSE) {
    if("SCENARIO" %in% names(x))x<-x[,c("SCENARIO",setdiff(names(x),"SCENARIO")),drop=FALSE]
    if(times)x<-unit_table(x,r$config$display_unit,names(x)[grepl("_day$|^DCO_DAY$",names(x))])
    labels<-c(SCENARIO="情景",reference="参照情景",name="情景名称",event_control_q="Control事件倍率",event_treatment_q="Treatment事件倍率",enroll_q="入组倍率",dropout_control_q="Control脱落倍率",dropout_treatment_q="Treatment脱落倍率",
      process="过程",historical_count="历史计数",historical_exposure="历史暴露",exposure_unit="暴露单位",posterior_shape="后验shape",posterior_rate="后验rate",rate_unit="率单位",posterior_mean="后验率均值",posterior_lower="后验率P2.5",posterior_median="后验率P50",posterior_upper="后验率P97.5",
      paired_generated="配对生成轮次",hit_difference="达标概率差",hit_difference_mcse="达标差MCSE",hit_difference_lower="全部请求达标差下界",hit_difference_upper="全部请求达标差上界",both_hit="两情景均达标轮次",both_hit_wait_difference_day="均达标时等待差",both_hit_wait_mcse_day="均达标等待差MCSE",termination_difference_day="关闭等待差",termination_difference_mcse_day="关闭等待差MCSE",
      generated="成功生成轮次",target_hits="达标轮次",target_hit_rate="生成轮次达标率",target_lower="全部请求达标下界",target_upper="全部请求达标上界",
      hit_wait_lower_day="达标者等待P2.5",hit_wait_median_day="达标者等待P50",hit_wait_upper_day="达标者等待P97.5",generated_wait_lower_day="生成轮次等待P2.5",generated_wait_median_day="生成轮次等待P50",generated_wait_upper_day="生成轮次等待P97.5",
      all_wait_median_lower_day="全部请求等待P50下界",all_wait_median_upper_day="全部请求等待P50上界",termination_lower_day="关闭等待P2.5",termination_median_day="关闭等待P50",termination_upper_day="关闭等待P97.5",
      mean_new_n="平均新队列观察人数",mean_new_events="平均新队列事件",mean_new_dropouts="平均新队列退出",frozen_ia_n="冻结IA人数",frozen_ia_events="冻结IA事件",hit_median_date="达标者P50日期",generated_median_date="生成轮次P50日期",termination_median_date="关闭P50日期",
      paired_valid="配对有效轮次",mean_rejection_difference="条件拒绝概率差",paired_mcse="配对MCSE",difference_lower="全部请求差值下界",difference_upper="全部请求差值上界",target_wait_day="达标等待",target_day="达标研究时间",wait_day="关闭等待",dropouts2="新队列永久退出",generation_valid="生成有效",decision_valid="决策有效",model="事件模型",parameters="拟合/输入参数（所选单位）",posterior_parameters="Gamma后验参数",event_uncertainty="事件参数处理",warning="诊断说明")
    for(id in names(labels))names(x)<-sub(paste0("^",id,"(?=（|$)"),labels[id],names(x),perl=TRUE)
    # Display Inf as a window miss while exports retain the numeric Inf sentinel.
    for(id in names(x))if(is.numeric(x[[id]])&&any(is.infinite(x[[id]])))x[[id]]<-ifelse(is.infinite(x[[id]]),"窗口内未达",format(x[[id]],digits=6,trim=TRUE))
    table(x)
  }
  output$pg_ia<-renderDT({r<-prediction();pt(r$ia,r)})
  output$pg_decision<-renderDT({r<-prediction();pt(r$decision,r)})
  output$pg_summary<-renderDT({r<-prediction();pt(r$summary,r)})
  output$pg_resources<-renderDT({r<-prediction();pt(r$resources,r,TRUE)})
  output$pg_paired<-renderDT({r<-prediction();pt(r$paired,r)})
  output$pg_models<-renderDT({r<-prediction();pt(patient_prediction_model_table(r),r)})
  output$pg_diagnostics<-renderDT({r<-prediction();pt(patient_prediction_diagnostics(r),r)})
  output$pg_trials<-renderDT({r<-prediction();pt(head(r$trials,200),r,TRUE)})
  observeEvent(current(),{r<-current();if(is.null(r)||r$config$purpose!="patient_prediction"||!isTRUE(r$extended))return();v<-r$scenarios$name;sel<-isolate(input$pg_view_scenario);if(length(sel)!=1||!sel %in% v)sel<-"输入基准";updateSelectInput(session,"pg_view_scenario",choices=v,selected=sel)})
  observeEvent(list(current(),input$pg_view_scenario),{r<-current();if(is.null(r)||r$config$purpose!="patient_prediction")return();x<-r$trials;if(isTRUE(r$extended)) {s<-input$pg_view_scenario %||% "输入基准";if(!s %in% r$scenarios$name)s<-"输入基准";x<-x[x$SCENARIO==s,,drop=FALSE]};v<-unique(x$SIMID[x$generation_valid]);sel<-isolate(input$pg_view_trial);if(length(sel)!=1||!sel %in% as.character(v))sel<-head(v,1);updateSelectInput(session,"pg_view_trial",choices=v,selected=sel)})
  sample<-reactive({r<-prediction();req(input$pg_view_trial);b<-as.integer(input$pg_view_trial);s<-if(isTRUE(r$extended))input$pg_view_scenario %||% "输入基准" else NULL;x<-r$trials;if(isTRUE(r$extended))x<-x[x$SCENARIO==s,,drop=FALSE];req(b %in% x$SIMID[x$generation_valid]);patient_prediction_sample(r,b,s)})
  output$pg_sample_decision<-renderDT({r<-prediction();pt(sample()$rows,r,TRUE)})
  output$pg_observed<-renderDT({r<-prediction();pt(sample()$observed,r,TRUE)})
  output$pg_scenario_table<-renderDT({r<-prediction();req(r$scenarios);pt(r$scenarios,r)})
  output$pg_scenario_contrasts<-renderDT({r<-prediction();req(r$scenario_contrasts);pt(r$scenario_contrasts,r,TRUE)})
  output$pg_process_table<-renderDT({r<-prediction();pt(patient_prediction_process_table(r),r)})
  output$pg_contrasts_download<-downloadHandler(filename="patient_prediction_scenario_contrasts.csv",content=function(file){r<-prediction();x<-r$scenario_contrasts;write.csv(unit_export(x,r$config$display_unit,names(x)[grepl("_day$",names(x))]),file,row.names=FALSE)})
  output$pg_process_download<-downloadHandler(filename="patient_prediction_process_draws.csv",content=function(file){write.csv(patient_prediction_process_export(prediction()),file,row.names=FALSE)})
  output$pg_resources_download<-downloadHandler(filename="patient_prediction_resources.csv",content=function(file){r<-prediction();write.csv(unit_export(r$resources,r$config$display_unit,names(r$resources)[grepl("_day$",names(r$resources))]),file,row.names=FALSE)})
  output$pg_paired_download<-downloadHandler(filename="patient_prediction_paired.csv",content=function(file){write.csv(prediction()$paired,file,row.names=FALSE)})
  output$pg_trials_download<-downloadHandler(filename="patient_prediction_trials.csv",content=function(file){r<-prediction();write.csv(unit_export(r$trials,r$config$display_unit,names(r$trials)[grepl("_day$",names(r$trials))]),file,row.names=FALSE)})
  output$pg_observed_download<-downloadHandler(filename="patient_prediction_observed.csv",content=function(file){r<-prediction();write.csv(unit_export(sample()$observed,r$config$display_unit,c("entry_day","time_day","obs_day","DCO_DAY")),file,row.names=FALSE)})
  output$pg_models_download<-downloadHandler(filename="patient_prediction_models.rds",content=function(file){r<-prediction();saveRDS(list(models=r$models,diagnostics=patient_prediction_diagnostics(r),data=r$data,config=r$config,version=r$version,process_posteriors=r$process_posteriors,scenarios=r$scenarios),file)})
  invisible(current)
}

patient_prediction_report <- function(r) {
 c("# 实际IA重估后的新队列条件预测","",paste("版本",r$version),paste("状态",r$status,"；已完成",r$completed,"/",r$total),
   "阶段1记录、Z1及重估D2在提交时固定。仅生成IA后独立新队列；旧患者IA后事件不进入检验。固定原计划逆正态权重；新队列未达目标则关闭且不拒绝。",
   "条件拒绝概率依赖当前IA及未来模型，不等同于从起点的功效、Ⅰ类错误或正态近似CP。事件参数处理与过程参数处理分别设置。固定过程使用输入率；过程Gamma使用原IA更新后每轮共享率。",
   "", "## IA与重估目标",capture.output(print(r$ia,row.names=FALSE)),capture.output(print(r$decision,row.names=FALSE)),
   "", "## 条件概率",capture.output(print(r$summary,row.names=FALSE)),
   "全部请求概率上下界保留无效及未完成轮次；Wilson和MCSE仅描述有效决策的Monte Carlo误差。",
   "", "## 时间与资源（连续日）",capture.output(print(r$resources,row.names=FALSE)),
   "达标者分位数条件于窗口内达标；生成轮次分位数含未达标Inf。Inf只表示本窗口内未达到，不断言永不达到。全部请求等待中位数报告未知轮次的0至Inf上下界。关闭时间独立报告。",
   "资源平均值使用成功生成轮次，不能在取消或拟合失败时直接代表全部请求。日历日期为研究起点加连续日向下取整；月按30.4375日。",
   "", "## 配对原计划",capture.output(print(r$paired,row.names=FALSE)),
   "重估和原计划共用每轮参数与患者轨迹，各自按D2截取。配对概率差是重估减原计划。",
   "", "## 模型",capture.output(print(patient_prediction_model_table(r),row.names=FALSE)),capture.output(print(patient_prediction_diagnostics(r),row.names=FALSE)),
   if(isTRUE(r$extended))c("", "## 未来情景",capture.output(print(r$scenarios,row.names=FALSE)),"## 过程后验",capture.output(print(patient_prediction_process_table(r),row.names=FALSE)),"## 情景配对差值",capture.output(print(r$scenario_contrasts,row.names=FALSE)),
    "各情景请求分母均为B，共享每轮参数抽样与潜在患者随机数。等待差条件于两个情景均达标；不能解释为全部请求的无条件等待差。入组后验要求明确的完整历史恒定Poisson窗口；不把关闭入组后的时间计入暴露。"),
   "方法与使用过程见工作手册第22–23章。","", "## 配置快照","```json",jsonlite::toJSON(r$config,auto_unbox=TRUE,pretty=TRUE,digits=NA),"```")
}
