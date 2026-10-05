gs_action_label <- function(x) {
  labels<-c(continue="继续",efficacy_benefit="效力停止：Treatment获益",efficacy_reverse="效力停止：反方向",futility="无效停止",final_no_reject="最终不拒绝",window_unreached="窗口关闭：未达下一目标",invalid="检验无效",generation_failure="生成失败")
  unname(labels[x])
}
gs_table <- function(d) {
  labels<-c(look="分析次序",target_events="事件目标",planned_fraction="输入信息比例",information_fraction="取整后计划比例",upper_z="效力上界Z",lower_efficacy_z="反方向效力Z",futility_z="无效界Z",alpha_spent="累计alpha消耗",nominal_stage_p="阶段名义p阈值",benefit_z="获益方向Z",score_variance="实际log-rank方差",event_information="事件信息近似",performed="执行检验",valid="检验有效",action="决策",target_reached="本次目标达成",DCO_DAY="研究时间（日）",n_observed="观察人数",early_efficacy="早期效力停止比例",efficacy_benefit="获益方向拒绝比例",efficacy_reverse="反方向拒绝比例",futility="无效停止比例",final_no_reject="最终不拒绝比例",window_unreached="窗口关闭比例",mean_looks="平均检验次数",mean_stop_day="平均停止时间（日）",future_hr="假设未来HR",full_cp="后续完整路径CP",final_only_cp="仅最终一次CP",decision_state="当前决策",D_FINAL="最终事件目标",future_futility_rule="未来无效规则",allocation="Treatment分配概率",binding_futility="约束性无效规则",beta_spent="累计beta消耗",beta_target="设计beta",beta_spending="beta消耗函数",design_hr="事件数参照设计HR",target_power="设计目标功效",standardized_final_drift="标准化最终漂移",reference_required_information="参照所需信息量",reference_required_events="参照所需事件数",reference_events_ceiling="参照事件数向上取整",implied_design_hr="所填D*隐含设计HR")
  if("beta_spending" %in% names(d))d$beta_spending<-unname(c(bsOF="OBF型",bsP="Pocock型",bsHSD="HSD型")[d$beta_spending])
  if("action" %in% names(d))d$action<-gs_action_label(d$action)
  if("decision_state" %in% names(d))d$decision_state<-gs_action_label(d$decision_state)
  for(id in intersect(names(d),names(labels)))names(d)[names(d)==id]<-labels[[id]]
  study_table(d)
}
register_sequential_server <- function(input,output,session,current,cfg) {
  output$st_gs_binding<-renderText({r<-current();if(!is.null(r)&&study_is_sequential(r$config)&&gs_is_binding(r$config))"yes" else "no"});outputOptions(output,"st_gs_binding",suspendWhenHidden=FALSE)
  previous_beta_basis<-reactiveVal("power")
  observeEvent(input$st_gs_beta_input,{
    to<-input$st_gs_beta_input;from<-previous_beta_basis()
    if(identical(to,from))return()
    value<-isolate(if(from=="beta")input$st_gs_beta else 1-input$st_gs_power)
    previous_beta_basis(to)
    if(length(value)!=1||!is.finite(value))return()
    if(to=="beta") {freezeReactiveValue(input,"st_gs_beta");updateNumericInput(session,"st_gs_beta",value=value)}
    else {freezeReactiveValue(input,"st_gs_power");updateNumericInput(session,"st_gs_power",value=1-value)}
  })
  output$st_gs_beta_conversion<-renderUI({
    req(input$st_gs_beta_input,input$st_sided=="benefit",input$st_gs_futility=="beta")
    beta<-if(input$st_gs_beta_input=="beta")input$st_gs_beta else 1-input$st_gs_power
    if(length(beta)!=1||!is.finite(beta))return(p(class="field-note","请输入有限设计beta或目标功效。"))
    p(class="field-note",paste0("设计beta=",signif(beta,6),"；目标功效=",signif(1-beta,6),"。"))
  })
  output$st_gs_plan_note<-renderUI({z<-tryCatch(cfg(),error=function(e)NULL);if(is.null(z))return(NULL);p(class="field-note",gs_beta_plan_note(z))})
  output$st_gs_beta_reference<-renderDT({z<-cfg();req(gs_is_binding(z));gs_table(gs_beta_plan_summary(z))})
  output$st_gs_beta_plan_download<-downloadHandler(filename="binding_beta_plan_draft.csv",content=function(file){z<-cfg();req(gs_is_binding(z));d<-do.call(rbind,lapply(names(z$gs_plans),function(key)cbind(D_FINAL=as.numeric(key),z$gs_plans[[key]])));write.csv(d,file,row.names=FALSE)})
  output$st_is_sequential<-renderText({r<-current();if(!is.null(r)&&study_is_sequential(r$config))"yes" else "no"});outputOptions(output,"st_is_sequential",suspendWhenHidden=FALSE)
  output$st_gs_has_futility<-renderText({r<-current();if(!is.null(r)&&study_is_sequential(r$config)&&r$config$gs_futility=="z")"yes" else "no"});outputOptions(output,"st_gs_has_futility",suspendWhenHidden=FALSE)
  observe({r<-current();for(tab in c("st_cp","st_inference"))if(!is.null(r)&&study_is_sequential(r$config))nav_show("st_tabs",tab,session=session) else nav_hide("st_tabs",tab,session=session)})
  output$st_gs_plan<-renderDT({z<-tryCatch(cfg(),error=function(e)NULL);req(z,study_is_sequential(z));d<-do.call(rbind,lapply(names(z$gs_plans),function(n)cbind(D_FINAL=as.numeric(n),z$gs_plans[[n]])));gs_table(d)})
  output$st_gs_boundary_plot<-renderPlotly({z<-tryCatch(cfg(),error=function(e)NULL);req(z,study_is_sequential(z));rows<-list()
    for(key in names(z$gs_plans))for(kind in c("upper_z","lower_efficacy_z","futility_z")){p<-z$gs_plans[[key]];ok<-is.finite(p[[kind]]);if(any(ok))rows[[length(rows)+1]]<-data.frame(t=p$information_fraction[ok],z=p[[kind]][ok],curve=paste0("D*=",key," · ",c(upper_z="效力",lower_efficacy_z="反方向",futility_z="无效")[[kind]]))}
    d<-do.call(rbind,rows);p<-ggplot(d,aes(t,z,colour=curve,group=curve))+geom_line()+geom_point(size=2)+labs(x="事件目标 / 最终D*（计划信息比例）",y="Z（正值表示Treatment获益）",colour=NULL)+theme_minimal()+theme(legend.position="bottom");plotly::layout(plotly::ggplotly(p),legend=list(orientation="h",x=0,y=-.35),margin=list(b=85))})
  output$st_gs_stopping<-renderDT({r<-current();req(r,study_is_sequential(r$config));d<-gs_overview(r);if("mean_stop_day" %in% names(d)){d$mean_stop_day<-d$mean_stop_day/time_factor(r$config$display_unit);names(d)[names(d)=="mean_stop_day"]<-paste0("平均停止时间（",time_label(r$config$display_unit),"）")};gs_table(d)})
  selected_path<-reactive({r<-current();req(r,study_is_sequential(r$config),input$st_view_scenario,input$st_view_rep);r$looks[r$looks$SCENARIO==as.integer(input$st_view_scenario)&r$looks$SIMID==as.integer(input$st_view_rep),,drop=FALSE]})
  output$st_gs_path<-renderDT({d<-selected_path();r<-current();d$DCO_DAY<-d$DCO_DAY/time_factor(r$config$display_unit);names(d)[names(d)=="DCO_DAY"]<-paste0("研究时间（",time_label(r$config$display_unit),"）");gs_table(d[,setdiff(names(d),c("BASEID","SIMID")),drop=FALSE])})
  observe({d<-selected_path();keep<-d$performed&d$valid;choices<-if(any(keep))setNames(d$look[keep],paste0("分析",d$look[keep]," · ",signif(d$information_fraction[keep]*100,4),"% · ",gs_action_label(d$action[keep]))) else character();old<-isolate(input$st_cp_look);updateSelectInput(session,"st_cp_look",choices=choices,selected=if(old %in% as.character(d$look[keep]))old else head(d$look[keep],1));old_inf<-isolate(input$st_inf_look);updateSelectInput(session,"st_inf_look",choices=choices,selected=if(old_inf %in% as.character(d$look[keep]))old_inf else tail(d$look[keep],1))})
  cp<-reactive(tryCatch({r<-current();d<-selected_path();if(!nrow(d))return(list(error="该轮没有保存分析记录。"));if(!any(d$performed&d$valid))return(list(error="该轮没有已完成且有效的分析可供计算。"));look<-as.integer(input$st_cp_look);if(length(look)!=1||is.na(look))return(list(error="该轮没有已完成且有效的分析可供计算。"));x<-d[d$look==look&d$performed&d$valid,,drop=FALSE];if(nrow(x)!=1)return(list(error="请选择已完成且有效的分析。"));cell<-r$scenarios[r$scenarios$SCENARIO==x$SCENARIO,];plan<-r$config$gs_plans[[as.character(cell$cut_value)]];a<-gs_conditional_power(plan,look,x$benefit_z,input$st_cp_hr,r$config$treatment_fraction,r$config$sided,gs_is_binding(r$config)||r$config$gs_futility=="none"||input$st_cp_futility!="ignore")
    list(value=a,row=data.frame(SCENARIO=x$SCENARIO,SIMID=x$SIMID,look=look,benefit_z=x$benefit_z,information_fraction=x$information_fraction,D_FINAL=cell$cut_value,future_hr=input$st_cp_hr,full_cp=a$full,final_only_cp=a$final_only,decision_state=a$status,future_futility_rule=if(r$config$gs_futility=="none")"不设" else if(gs_is_binding(r$config))"执行（约束性）" else if(input$st_cp_futility=="ignore")"忽略" else "执行",allocation=r$config$treatment_fraction,alpha=r$config$alpha,spending=r$config$gs_spending,information_assumption="D*p*(1-p)"))
  },error=function(e)list(error=conditionMessage(e))))
  output$st_cp_note<-renderUI({z<-cp();if(!is.null(z$error))return(p(class="field-note",z$error));p(class="field-note",paste0("当前状态：",gs_action_label(z$value$status),"。完整路径CP计后续任一次效力跨界；仅最终一次CP忽略之前的停止规则。已停止时完整路径为已决策的0或1，最终一次不再计算。信息量使用D×p×(1−p)近似，不代表个体数据条件预测概率。"))})
  output$st_cp_result<-renderDT({z<-cp();gs_table(if(!is.null(z$error))data.frame() else z$row)})
  output$st_cp_download<-downloadHandler(filename="study_conditional_power.csv",content=function(file){z<-cp();req(is.null(z$error));write.csv(z$row,file,row.names=FALSE)})
  inf<-reactive(tryCatch({r<-current();d<-selected_path();if(!nrow(d)||!any(d$performed&d$valid))return(list(error="该轮没有已完成且有效的分析可供推断。"));k<-as.integer(input$st_inf_look);if(length(k)!=1||is.na(k))return(list(error="请选择已完成且有效的分析。"));cell<-r$scenarios[r$scenarios$SCENARIO==d$SCENARIO[1],,drop=FALSE];list(value=gs_inference(r$config,r$config$gs_plans[[as.character(cell$cut_value)]],d,k))},error=function(e)list(error=conditionMessage(e))))
  output$st_inf_note<-renderUI({z<-inf();if(!is.null(z$error))return(p(class="field-note",z$error));a<-z$value;p(class="field-note",paste0("情景",a$path$SCENARIO[1]," · 重复轮次",a$path$SIMID[1]," · 截至分析",a$look,"。信息量I=D×p×(1−p)，近似HR=exp(−Z/√I)。双侧区间水平",signif(a$repeated$interval_level[1]*100,6),"%。效应换算及区间是事件信息近似，非Cox拟合；rpact ",a$rpact_version,"。",if(gs_is_binding(a$config))"约束性beta分支待复核；重复p/重复区间当前不提供，阶段排序区间使用原完整停止规则。" else ""))})
  output$st_inf_repeated_note<-renderUI({z<-inf();if(!is.null(z$error))return(NULL);p(class="field-note",z$value$repeated_note)})
  output$st_inf_repeated<-renderDT({
    z<-inf();if(!is.null(z$error))return(gs_inference_table(data.frame()))
    d<-z$value$repeated
    if(!z$value$repeated_available)d<-d[,setdiff(names(d),c("repeated_p","repeated_hr_lower","repeated_hr_upper","interval_level")),drop=FALSE]
    gs_inference_table(d)
  })
  output$st_inf_final_note<-renderUI({z<-inf();if(!is.null(z$error))return(NULL);p(class="field-note",z$value$final$reason)})
  output$st_inf_final<-renderDT({z<-inf();gs_inference_table(if(!is.null(z$error)||!z$value$final$available)data.frame() else z$value$final[,setdiff(names(z$value$final),c("available","reason","ordering","engine","root_probability_error","interval_tail_alpha")),drop=FALSE])})
  for(kind in c("repeated","final"))local({which<-kind;output[[paste0("st_inf_",which,"_download")]]<-downloadHandler(filename=paste0("sequential_",which,".csv"),content=function(file){z<-inf();req(is.null(z$error));write.csv(z$value[[which]],file,row.names=FALSE)})})
  output$st_inf_config_download<-downloadHandler(filename="sequential_inference.json",content=function(file){z<-inf();req(is.null(z$error));a<-z$value;jsonlite::write_json(a[setdiff(names(a),c("repeated","final"))],file,auto_unbox=TRUE,pretty=TRUE,na="null",digits=NA)})
  output$st_inf_script_download<-downloadHandler(filename="reproduce_sequential_inference.R",content=function(file){z<-inf();req(is.null(z$error));writeLines(gs_inference_script(z$value),file,useBytes=TRUE)})

}

gs_inference_table <- function(d) {
  labels<-c(interval_empty="重复区间为空",repeated_engine="重复推断引擎",band_scale="重复边界校准倍数",band_full_plan_crossing="全计划区间跨界概率",original_beta_decision="原beta设计决策",repeated_matches_original_test="重复p与原beta检验同一反演",SCENARIO="情景",SIMID="重复轮次",look="分析次序",events="累计事件数",information_fraction="计划信息比例",benefit_z="获益方向Z",event_information="事件信息近似",log_hr_approx="近似log HR",hr_approx="近似HR",repeated_p="重复p值",repeated_hr_lower="重复HR区间下限",repeated_hr_upper="重复HR区间上限",interval_level="双侧区间水平",adjusted_p="阶段排序调整p值",median_unbiased_hr="中位无偏HR近似",adjusted_hr_lower="调整HR区间下限",adjusted_hr_upper="调整HR区间上限",upper_tail_null="获益方向尾概率",lower_tail_null="反方向尾概率",p_agrees_original_decision="p阈值与原决策一致",action="决策")
  if("action" %in% names(d))d$action<-gs_action_label(d$action)
  for(id in intersect(names(d),names(labels)))names(d)[names(d)==id]<-labels[[id]]
  d<-d[,setdiff(names(d),c("情景","重复轮次","近似log HR")),drop=FALSE]
  tab<-DT::datatable(d,rownames=FALSE,options=list(scrollX=TRUE,columnDefs=list(list(className="dt-nowrap",targets="_all")),pageLength=6,dom="tip",language=list(emptyTable="尚无记录",info="_START_–_END_ / _TOTAL_",infoEmpty="无记录",paginate=list(previous="上一页",`next`="下一页"))))
  cols<-names(d)[vapply(d,is.double,logical(1))];if(length(cols))DT::formatSignif(tab,cols,digits=5) else tab
}
