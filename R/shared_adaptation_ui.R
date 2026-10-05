register_shared_adaptation_help<-function(){
 h<-get("parameter_help",envir=parent.frame())
 for(pre in c("spm_","spdc_","spdt_")){for(k in names(simulation_defaults())){base<-h[paste0("hgm_",k)];if(!length(base)||is.na(base))base<-h[k];if(length(base)&&!is.na(base))h[paste0(pre,k)]<-base}}
 for(k in c("run_source","run_name","config_file","reps","seed","view_scenario","view_trial"))h[paste0("sp_",k)]<-h[paste0("hg_",k)]
 for(k in c("exp_input","median","exp_rate","shape","eta","wei_input","log_mu","sigma","ll_shape","g_rate","g_shape","cure","median2","shape2","mix_weight","parameter_cuts","parameter_rates","pwe_input","pwe_survivals","pwe_last_rate","survival_time","survival_prob","dropout_rate","drop_period","drop_prob","drop_input"))for(pre in c("spm_","spdc_","spdt_")){base<-h[paste0("hgm_",k)];if(!length(base)||is.na(base))base<-h[k];if(length(base)&&!is.na(base))h[paste0(pre,k)]<-base}
 extra<-c(sp_test_basis="已指定基准风险，或精确共同入组队列条件标记风险集。后者只在队列内比较两组，不把近似相同入组日合并；无双组风险的事件不贡献信息，实际数据需精确ENGINE时间。",sp_purpose="从起点研究只需参数；实际IA条件研究需ADTTE；当前观察推断只用IA、已指定基准风险与原混合，不填写未来计划或B。",
 sp_unit="输入/显示日、周、月，默认月；内部连续日。基准模型时间/率、窗口和入组率随单位换算。",
 sp_origin="研究起点日期，需早于各STARTDT；在原计划定义中保持。",sp_endpoint="同一PFS或OS终点；实际资料筛选PARAMCD后每人唯一。",
 sp_file="两组单次IA ADTTE，必填USUBJID/PARAMCD/STARTDT/ADT/AVAL/CNSR及组别；原事件/永久退出保持，仍随访须确认至IA。",
 sp_cut="自研究起点至实际IA的时间；条件最大窗口另从IA计时，不是Final研究DCO。",
 sp_control="按原方案指定Control；其他组是Treatment，不能根据观察效应改变。",
 sp_d1="原设计IA事件目标D1，1–100000整数；实际IA时自动取冻结事件数。IA只是目标重估，不执行早停检验。",
 sp_dplan="原Final累计事件目标Dplan>D1，为重估下限；所有原患者未来事件继续计入。",
 sp_dmax="Final事件上限Dmax≥Dplan，最多增加2000，且不超过总计划患者数。",
 sp_allocation="Treatment简单随机分配概率0<p<1；用于患者生成与CP代理，不作为基准风险估计。",
 sp_alpha="获益混合似然检验alpha0.0001–0.2，阈值E≥1/alpha；常用0.025。",
 sp_hr_assumed="事件目标规则的假设HR在(0,1)，只用于canonical CP代理，不替换正式检验。",
 sp_cp_target="CP代理目标(0.5,1)，例如0.9；不是混合似然检验真实条件功效的保证。",
 sp_cp_min="Promising规则最低CP代理，0<CPmin<CPtarget；低于该值保留原事件目标。",
 sp_primary="运行前预定原计划、上下限或Promising主规则；比较规则复用同轮患者，不择优改写主拒绝。",
 sp_extra="额外预定事件规则，仅同轮配对模拟比较。所有规则使用同一原混合检验。",
 sp_enroll_mode="一次入组或总体恒定Poisson；条件模式指IA后的新患者，原风险集不重新入组。",
 sp_enroll_rate="总体入组率，人/当前单位，正值；条件模式从实际IA重新计算未来到达时间。",
 sp_max="设计为从研究起点的最大窗口；条件为IA后的最大窗口；最长3650日。未达事件目标关闭不拒绝。",
 sp_scenarios="严格五列CSV文本label,n,hr,enroll_scale,dropout_scale；设计n是总N，条件n是未来新人数，可为0。最多60情景。",
 sp_test_grid="原预定获益备择HR网格，每项>0且<1，1–30个不同值；例如0.4,0.6,0.8。不是根据本次IA拟合的HR。",
 sp_test_weights="与获益网格同长度正权重，自动归一化；IA后仅按原似然更新，不重新选择原先验。",
 sp_ci_grid="原区间参考混合HR网格，每项>0且≤20，1–30个不同值；建议覆盖小于/等于/大于1。不是参数真实支持范围截断。",
 sp_ci_weights="区间网格的原正权重，同长度且归一化；混合区间与获益单侧p不是同一个反演。",
 sp_interval_alpha="置信序列错误率0.0001–0.2，0.05对应95%；已知基准风险下允许适应停止，不称为普通Cox区间。",
 sp_estimate_effect="勾选后每轮计算已知基准风险HR估计与混合置信序列，增加计算量；未勾选时显式样例恢复仍给该轮区间。")
 h[names(extra)]<-extra
 for(pair in list(c("sp_date_encoding","date_encoding"),c("sp_aval_unit","aval_unit"),c("sp_offset","offset"),c("sp_dropout_codes","dropout_codes"),c("sp_group_column","hi_group_column")))h[pair[1]]<-h[pair[2]]
 assign("parameter_help",h,envir=parent.frame())
}
shared_adaptation_ui<-function(){
 nav_panel("共享患者事件重估",value="shared_adaptation",
  div(class="page-title",h2("共享患者事件重估与推断"),conditionalPanel("input.sp_purpose !== 'observed' || input.sp_run_source === 'config_json'",div(actionButton("sp_run","运行研究",class="btn-primary"),actionButton("sp_cancel","取消"))),conditionalPanel("input.sp_purpose === 'observed' && input.sp_run_source !== 'config_json'",actionButton("sp_observed_run","计算当前观察推断",class="btn-primary"))),
  section("输入来源",radioButtons("sp_run_source","参数来源",c("页面参数"="ui","无损研究配置JSON"="config_json"),"ui",inline=TRUE),textInput("sp_run_name","研究名称","共享患者事件重估"),conditionalPanel("input.sp_run_source === 'config_json'",fileInput("sp_config_file","本平台研究配置JSON",accept=".json"),uiOutput("sp_import_note")),conditionalPanel("input.sp_run_source !== 'config_json'",selectInput("sp_purpose","本次需求",c("从研究起点评价设计"="fixed_truth","实际IA后的条件研究"="conditional","只计算当前观察证据和区间"="observed")))),
  navset_card_tab(id="sp_tabs",
   nav_panel("数据与基准模型",value="sp_baseline",
    section("共同定义",selectInput("sp_test_basis","共享患者检验模型",c("已指定Control基准风险"="known_hazard","共同入组队列风险集（未知基准风险）"="cohort_riskset")),fields(selectInput("sp_unit","时间单位",c("日"="days","周"="weeks","月"="months"),"months"),selectInput("sp_endpoint","终点",c("PFS","OS"))),dateInput("sp_origin","研究起点日期","2025-01-01")),
    conditionalPanel("input.sp_purpose !== 'fixed_truth'",section("实际IA观察",fileInput("sp_file","IA ADTTE文件",accept=c(".csv",".xpt",".sas7bdat")),fields(numericInput("sp_cut","实际IA时间（月）",18,min=.001),uiOutput("sp_group_ui")),fields(selectInput("sp_date_encoding","CSV日期编码",c("YYYY-MM-DD"="iso","SAS日期"="sas")),selectInput("sp_aval_unit","文件AVAL单位",c("日"="days","周"="weeks","月"="months"),"days")),fields(selectInput("sp_offset","AVAL首日加项",c("0"=0,"1"=1)),textInput("sp_dropout_codes","永久退出CNSR","2")),selectInput("sp_control","Control组",character()),uiOutput("sp_ia_note"))),
    conditionalPanel("input.sp_purpose !== 'observed' || input.sp_test_basis === 'known_hazard'",section("Control模型",p(class="field-note","已指定风险分支把本模型用于检验；队列风险集分支只用于未来生成。检验假设见手册第42章。"),event_parameter_fields("spm_","months",simulation_defaults(),simulation=TRUE)))),
   nav_panel("事件与未来计划",value="sp_plan",
    conditionalPanel("input.sp_purpose !== 'observed'",section("原事件计划",conditionalPanel("input.sp_purpose === 'fixed_truth'",numericInput("sp_d1","原IA事件目标D1",100,min=1,step=1)),fields(numericInput("sp_dplan","原Final目标Dplan",220,min=2,step=1),numericInput("sp_dmax","Final事件上限Dmax",350,min=2,step=1)),fields(numericInput("sp_allocation","Treatment分配概率p",.5,min=.01,max=.99),numericInput("sp_hr_assumed","规则假设HR",.67,min=.01,max=.99)),fields(numericInput("sp_cp_target","规则CP代理目标",.9,min=.501,max=.999),numericInput("sp_cp_min","Promising最低CP代理",.8,min=.001,max=.998))),
    section("后续过程与情景",selectInput("sp_enroll_mode","入组方式",c("一次入组"="batch","恒定Poisson"="constant"),"constant"),conditionalPanel("input.sp_enroll_mode === 'constant'",numericInput("sp_enroll_rate","总体入组率（人/月）",20,min=.001)),numericInput("sp_max","最大窗口（月）",48,min=.001),fields(section("Control独立退出",dropout_parameter_fields("spdc_","months",simulation_defaults())),section("Treatment独立退出",dropout_parameter_fields("spdt_","months",simulation_defaults()))),textAreaInput("sp_scenarios","情景CSV：设计总N / 条件未来N",paste(capture.output(write.csv(data.frame(label=c("null","active"),n=500,hr=c(1,.67),enroll_scale=1,dropout_scale=1),row.names=FALSE)),collapse="\n"),rows=6),downloadButton("sp_template","下载情景模板")))),
   nav_panel("检验与区间",value="sp_analysis",
    section("原混合与预定规则",numericInput("sp_alpha","单侧获益alpha",.025,min=.0001,max=.2),fields(textInput("sp_test_grid","原获益混合HR网格","0.4,0.6,0.8"),textInput("sp_test_weights","原获益混合权重","1,1,1")),fields(textInput("sp_ci_grid","原区间混合HR网格","0.1,0.25,0.5,0.75,1,1.5,2,4"),textInput("sp_ci_weights","原区间混合权重","1,1,1,1,1,1,1,1")),numericInput("sp_interval_alpha","区间错误率",.05,min=.0001,max=.2),
     conditionalPanel("input.sp_purpose !== 'observed'",selectInput("sp_primary","主事件规则",setNames(names(sp_names),unname(sp_names)),"promising"),checkboxGroupInput("sp_extra","同轮比较事件规则",setNames(names(sp_names)[1:2],unname(sp_names)[1:2]),c("original","bounded")),checkboxInput("sp_estimate_effect","每轮计算混合置信区间",FALSE),fields(numericInput("sp_reps","每情景重复数B",200,min=20,max=10000,step=20),numericInput("sp_seed","随机种子",20261005,min=0,step=1))))),
   nav_panel("研究结果",value="sp_results",conditionalPanel("input.sp_purpose !== 'observed' || input.sp_run_source === 'config_json'",research_result_panels("sp_","共享患者事件重估",c(recovery="保存分支的效应与区间")),section("所选轮次事件风险集",DTOutput("sp_sample_risksets",fill=FALSE),downloadButton("sp_risksets_download","所选事件风险集CSV"))),conditionalPanel("input.sp_purpose === 'observed' && input.sp_run_source !== 'config_json'",uiOutput("sp_observed_status"),section("当前观察证据与混合区间",DTOutput("sp_observed_table",fill=FALSE)),div(class="export-bar",downloadButton("sp_observed_csv","观察推断CSV"),downloadButton("sp_observed_json","观察推断与记录JSON"),downloadButton("sp_observed_script","观察推断R脚本")))),
   research_export_panel("sp_","共享患者研究",c(recovery="效应与区间汇总CSV"))))
}
