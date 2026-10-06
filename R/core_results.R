parameter_help <- c(parameter_help,
  result_model="筛选已完成运行的模型。全部模型用于比较；选择单个模型可减少曲线重叠。不会重新拟合。",
  result_axis="日历日期或距当前DCO的经过时间。单位使用最近成功运行时保存的日/周/月。",
  result_count="累计事件包含当前已记录事件；新增事件扣除固定D0。仅切换展示口径。",
  result_center="中心曲线可显示中位数或均值；阴影始终为2.5%至97.5%逐点预测分位数。",
  result_time="选择距当前DCO的经过时间，单位使用原运行设置。读数取最近的已计算网格时点，默认窗口末；图上点击也可选择，不在网格间插值。",
  result_interval="显示或隐藏逐点95%预测区间。它不是均值置信区间，也不是整条曲线的同时区间。",
  result_paths="从全部成功轨迹中按序等距选取最多20条显示。只用于查看单次轨迹，区间仍使用全部成功轨迹。",
  sim_display_group="筛选已完成运行的观察组别；全部组别用于比较。导出保留所选试验/截点的全部组别。",
  sim_display_ci="显示KM的逐点95%置信区间，使用原分析的log-log区间；超出观察支持不外推。",
  sim_display_censor="在KM曲线上标记出现删失的随访时点，包括永久退出和行政删失。"
)
# Display-only transformations. Never alter the submitted configuration or RNG state.
core_result_palette <- c("#126b72", "#ba7141", "#466d95", "#99617d", "#548054")
core_result_table <- function(d) {
  w <- DT::datatable(d, rownames=FALSE, escape=TRUE, class="display nowrap", options=list(scrollX=TRUE,
    pageLength=8, lengthMenu=c(8,20,50), dom=if(nrow(d)>8)"lftip" else "t",
    language=list(search="检索：",lengthMenu="每页 _MENU_ 条",info="第 _START_–_END_ 条，共 _TOTAL_ 条",
      infoEmpty="无记录",zeroRecords="无匹配记录",emptyTable="无记录",paginate=list(previous="上一页",`next`="下一页"))))
  cols <- names(d)[vapply(d,is.numeric,logical(1))]
  for(nm in cols) w <- DT::formatRound(w,nm,if(all(is.na(d[[nm]]) | d[[nm]]==round(d[[nm]])))0 else 3)
  w
}
core_plot_finish <- function(p,x,y,probability=FALSE,bar=FALSE) {
  p <- plotly::layout(p, xaxis=list(title=x,zeroline=FALSE,automargin=TRUE),
    yaxis=list(title=y,zeroline=FALSE,automargin=TRUE,tickformat=if(probability)".0%" else NULL,
      range=if(probability&&!bar)c(0,1) else NULL),
    legend=list(orientation="h",x=0,y=-.23,xanchor="left",yanchor="top"),
    margin=list(l=65,r=20,t=20,b=100),hovermode=if(bar)"closest" else "x",barmode="group",
    paper_bgcolor="white",plot_bgcolor="white",font=list(family="Helvetica Neue, PingFang SC, sans-serif",size=12,color="#173d50"))
  plotly::config(p,displaylogo=FALSE,scrollZoom=FALSE,responsive=TRUE,
    modeBarButtonsToRemove=c("lasso2d","select2d"),toImageButtonOptions=list(format="png",filename="SurvCast",scale=2))
}
core_empty_plot <- function(text) {
  plotly::layout(plotly::plot_ly(),annotations=list(list(text=text,x=.5,y=.5,xref="paper",yref="paper",showarrow=FALSE)),
    xaxis=list(visible=FALSE),yaxis=list(visible=FALSE)) |> plotly::config(displaylogo=FALSE)
}
core_result_methods <- function(r,method="all") {
  ids <- unique(r$curves$method)
  if(!is.null(method)&&method!="all"&&method %in% ids) ids <- method
  ids
}
core_result_grid <- function(r) sort(unique(r$curves$day))
core_result_index <- function(r,index) {
  i <- suppressWarnings(as.integer(index)); if(length(i)!=1 || is.na(i)) i <- length(core_result_grid(r))
  max(1L,min(length(core_result_grid(r)),i))
}
core_result_time_index <- function(r,elapsed) {
  if(is.null(elapsed)||length(elapsed)!=1||!is.finite(elapsed)) return(length(core_result_grid(r)))
  which.min(abs(core_result_grid(r)-r$config$cut-elapsed*time_factor(r$config$display_unit)))
}
core_result_dates <- function(r,days) as.character(as.Date(r$config$origin)+ceiling(days))
core_forecast_readout <- function(r,index,method="all") {
  i <- core_result_index(r,index); day <- core_result_grid(r)[i]
  d <- r$curves[r$curves$day==day & r$curves$method %in% core_result_methods(r,method),,drop=FALSE]
  out <- data.frame(模型=d$model,日期=core_result_dates(r,d$day),距当前DCO=d$day-r$config$cut,
    累计均值=d$mean,`2.5%累计事件数`=d$lower,累计中位数=d$median,`97.5%累计事件数`=d$upper,
    新增均值=d$mean-r$known_events,新增中位数=d$median-r$known_events,
    成功模拟次数=vapply(d$method,function(m)nrow(r$counts[[m]]),integer(1)),check.names=FALSE)
  out$距当前DCO <- out$距当前DCO/time_factor(r$config$display_unit)
  names(out)[3] <- paste0("距当前DCO（",time_label(r$config$display_unit),"）")
  if(identical(r$config$task,"target")) {
    out <- out[,c(1:3,10),drop=FALSE]
    out$`达标概率（%）` <- 100*d$probability
    out$`概率MCSE（百分点）` <- 100*d$probability_mcse
  }
  out
}
core_count_mass <- function(r,index,method="all",new=FALSE) {
  i <- core_result_index(r,index)
  do.call(rbind,lapply(core_result_methods(r,method),function(m) {
    v <- r$counts[[m]][,i]-if(new)r$known_events else 0
    z <- table(v)
    data.frame(method=m,model=r$curves$model[match(m,r$curves$method)],count=as.numeric(names(z)),
      frequency=as.integer(z),probability=as.numeric(z)/length(v),simulations=length(v))
  }))
}
core_target_mass <- function(r,method="all",bins=16L) {
  # All successful simulations remain in the denominator, including infinite hits.
  edges <- seq(r$config$cut,r$config$cut+r$config$horizon,length.out=bins+1L)
  f <- time_factor(r$config$display_unit)
  labels <- c("截至当前已达到",paste0(format(round((head(edges,-1)-r$config$cut)/f,2),trim=TRUE),"–",
    format(round((tail(edges,-1)-r$config$cut)/f,2),trim=TRUE),time_label(r$config$display_unit)),"超过预测窗口")
  do.call(rbind,lapply(core_result_methods(r,method),function(m) {
    v <- r$milestones[[m]]
    future <- is.finite(v)&v>r$config$cut&v<=tail(edges,1)
    freq <- c(sum(is.finite(v)&v<=r$config$cut),tabulate(findInterval(v[future],edges,left.open=TRUE),nbins=bins),
      sum(!is.finite(v)|v>tail(edges,1)))
    data.frame(method=m,model=r$curves$model[match(m,r$curves$method)],bucket=labels,
      frequency=freq,probability=freq/length(v),simulations=length(v))
  }))
}
core_forecast_controls_ui <- function() {
  div(class="result-controls",
    div(class="result-control-grid",selectInput("result_model","显示模型",c("全部模型"="all")),
      selectInput("result_axis","时间轴",c("日历日期"="calendar","距当前 DCO"="elapsed")),
      conditionalPanel("output.task_goal === 'count'",selectInput("result_count","事件口径",c("累计事件"="total","当前 DCO 后新增"="new"))),
      conditionalPanel("output.task_goal === 'count'",selectInput("result_center","中心曲线",c("中位数"="median","均值"="mean")))),
    sliderInput("result_time","选择读数时点：距当前 DCO（月）",min=0,max=48,value=48,step=.4),
    div(class="result-switches",conditionalPanel("output.task_goal === 'count'",
      checkboxInput("result_interval","显示逐点 95% 预测区间",TRUE),checkboxInput("result_paths","显示 20 条模拟轨迹",FALSE))),
    uiOutput("result_time_label"),p(class="field-note","点击曲线选择时点；悬停读数，拖动缩放，双击恢复。点击图例隐藏或显示曲线；工具栏可保存 PNG。显示设置不重新运行模拟。"))
}
core_forecast_plot <- function(r,index,method="all",axis="calendar",new=FALSE,center="median",interval=TRUE,paths=FALSE,prob=FALSE,grouped=FALSE) {
  i <- core_result_index(r,index); grid <- core_result_grid(r); f <- time_factor(r$config$display_unit)
  xfun <- if(identical(axis,"elapsed"))function(day)(day-r$config$cut)/f else function(day)as.POSIXct(r$config$origin,tz="UTC")+day*86400
  source <- if(grouped)"forecast_group" else "forecast"
  p <- plotly::plot_ly(source=source)
  d <- if(grouped)r$group_curves else r$curves
  ids <- core_result_methods(r,method)
  if(grouped) d$series <- paste(d$group,d$model,sep=" · ") else d$series <- d$model
  all_series<-unique(d$series);d <- d[d$method %in% ids,,drop=FALSE]
  series <- unique(d$series)
  for(j in seq_along(series)) {
    z <- d[d$series==series[j],,drop=FALSE]; z <- z[order(z$day),,drop=FALSE]
    baseline <- if(new) {
      if(grouped) r$group_summary$known_events[match(paste(z$group[1],z$method[1]),paste(r$group_summary$group,r$group_summary$method))] else r$known_events
    } else 0
    color <- core_result_palette[(match(series[j],all_series)-1)%%length(core_result_palette)+1]; x <- xfun(z$day)
    key <- match(z$day,grid); y <- if(prob)z$probability else z[[center]]-baseline
    if(!prob && interval) {
      p <- plotly::add_trace(p,x=x,y=z$lower-baseline,type="scatter",mode="lines",line=list(width=0),
        showlegend=FALSE,legendgroup=series[j],hoverinfo="skip",name=series[j])
      p <- plotly::add_trace(p,x=x,y=z$upper-baseline,type="scatter",mode="lines",line=list(width=0),
        fill="tonexty",fillcolor=grDevices::adjustcolor(color,alpha.f=.13),showlegend=FALSE,legendgroup=series[j],hoverinfo="skip",name=series[j])
    }
    if(!prob&&paths) {
      mat <- if(grouped) {
        g <- names(r$config$groups)[vapply(r$config$groups,function(g)identical(g$name,z$group[1]),logical(1))]
        r$group_counts[[z$method[1]]][[g[1]]]
      } else r$counts[[z$method[1]]]
      if(!is.null(mat))for(b in unique(round(seq(1,nrow(mat),length.out=min(20,nrow(mat)))))) {
        p <- plotly::add_trace(p,x=xfun(grid),y=mat[b,]-baseline,type="scatter",mode="lines",
          line=list(color=grDevices::adjustcolor(color,alpha.f=.18),width=.7,shape="hv"),showlegend=FALSE,legendgroup=series[j],hoverinfo="skip",name=series[j])
      }
    }
    text <- paste0(series[j],"<br>日期：",core_result_dates(r,z$day),"<br>距当前 DCO：",round((z$day-r$config$cut)/f,3),time_label(r$config$display_unit),
      if(prob)paste0("<br>达标概率：",round(100*z$probability,2),"%<br>MCSE：",round(100*z$probability_mcse,3)," 个百分点") else
        paste0("<br>",if(new)"新增" else "累计","均值：",round(z$mean-baseline,3),"<br>中位数：",round(z$median-baseline,3),"<br>逐点 95% 预测区间：",round(z$lower-baseline,3),"–",round(z$upper-baseline,3)))
    p <- plotly::add_trace(p,x=x,y=y,type="scatter",mode="lines",name=series[j],legendgroup=series[j],
      line=list(color=color,width=2),customdata=key,text=text,hoverinfo="text")
    p <- plotly::add_trace(p,x=xfun(grid[i]),y=y[match(grid[i],z$day)],type="scatter",mode="markers",name=series[j],
      legendgroup=series[j],showlegend=FALSE,marker=list(color=color,size=8),customdata=i,text=text[match(grid[i],z$day)],hoverinfo="text")
  }
  p <- plotly::layout(p,shapes=list(list(type="line",x0=xfun(grid[i]),x1=xfun(grid[i]),y0=0,y1=1,yref="paper",line=list(color="#607685",dash="dot",width=1))))
  p <- core_plot_finish(p,if(axis=="elapsed")paste0("距当前 DCO（",time_label(r$config$display_unit),"）") else "日期",
    if(prob)"达标概率" else paste0(if(grouped)"各组" else "",if(new)"新增" else "累计","事件数"),probability=prob)
  plotly::event_register(p,"plotly_click")
}
register_core_forecast_results <- function(input,output,session,result) {
  observeEvent(result(),{
    r <- result(); req(r); ids <- unique(r$curves$method)
    updateSelectInput(session,"result_model",choices=c("全部模型"="all",setNames(ids,r$curves$model[match(ids,r$curves$method)])),selected="all")
    updateSliderInput(session,"result_time",label=paste0("选择读数时点：距当前 DCO（",time_label(r$config$display_unit),"）"),min=0,max=r$config$horizon/time_factor(r$config$display_unit),value=r$config$horizon/time_factor(r$config$display_unit),step=r$config$horizon/time_factor(r$config$display_unit)/(length(core_result_grid(r))-1))
  })
  view <- reactive({r<-result();req(r);list(r=r,i=core_result_time_index(r,input$result_time),m=input$result_model %||% "all")})
  for(src in c("forecast","forecast_group"))local({source<-src
    observeEvent(input[[paste0("plotly_click-",source)]],{
      click<-plotly::event_data("plotly_click",source=source,priority="event");req(click$customdata)
      r<-view()$r;i<-core_result_index(r,click$customdata[1])
      updateSliderInput(session,"result_time",value=(core_result_grid(r)[i]-r$config$cut)/time_factor(r$config$display_unit))
    },ignoreInit=TRUE)
  })
  output$result_time_label <- renderUI({s<-view();day<-core_result_grid(s$r)[s$i]
    div(class="selected-reading",span("当前读数"),strong(core_result_dates(s$r,day)),
      span(paste0("距当前 DCO ",round((day-s$r$config$cut)/time_factor(s$r$config$display_unit),3),time_label(s$r$config$display_unit))),
      span(paste0("网格 ",s$i," / ",length(core_result_grid(s$r)))) )})
  output$result_readout <- renderDT({s<-view();core_result_table(core_forecast_readout(s$r,s$i,s$m))})
  output$result_readout_download <- downloadHandler(filename="selected_time_readout.csv",content=function(file){s<-view();write.csv(core_forecast_readout(s$r,s$i,s$m),file,row.names=FALSE)})
  draw <- function(prob=FALSE,grouped=FALSE) {s<-view();if(grouped)req(s$r$group_curves)
    core_forecast_plot(s$r,s$i,s$m,input$result_axis %||% "calendar",identical(input$result_count,"new"),input$result_center %||% "median",
      isTRUE(input$result_interval),isTRUE(input$result_paths),prob,grouped)}
  output$count_curve_title <- renderText(paste0(if(identical(input$result_count,"new"))"新增" else "累计","事件数与逐点 95% 预测区间"))
  output$event_plot <- renderPlotly(draw())
  output$prob_plot <- renderPlotly(draw(prob=TRUE))
  output$group_event_plot <- renderPlotly(draw(grouped=TRUE))
  output$count_distribution <- renderPlotly({s<-view();d<-core_count_mass(s$r,s$i,s$m,identical(input$result_count,"new"));p<-plotly::plot_ly()
    for(j in seq_along(unique(d$model))) {z<-d[d$model==unique(d$model)[j],,drop=FALSE]
      p<-plotly::add_bars(p,textposition="none",x=z$count,y=z$probability,name=z$model[1],marker=list(color=core_result_palette[(match(z$method[1],core_result_methods(s$r))-1)%%length(core_result_palette)+1]),
        text=paste0(z$model,"<br>事件数：",z$count,"<br>频数：",z$frequency," / ",z$simulations,"<br>概率：",round(z$probability*100,2),"%"),hoverinfo="text")}
    core_plot_finish(p,if(identical(input$result_count,"new"))"新增事件数" else "累计事件数","模拟概率",TRUE,TRUE)})
  output$target_distribution <- renderPlotly({s<-view();d<-core_target_mass(s$r,s$m);p<-plotly::plot_ly()
    for(j in seq_along(unique(d$model))) {z<-d[d$model==unique(d$model)[j],,drop=FALSE]
      p<-plotly::add_bars(p,textposition="none",x=z$bucket,y=z$probability,name=z$model[1],marker=list(color=core_result_palette[(match(z$method[1],core_result_methods(s$r))-1)%%length(core_result_palette)+1]),
        text=paste0(z$model,"<br>",z$bucket,"<br>频数：",z$frequency," / ",z$simulations,"<br>概率：",round(z$probability*100,2),"%"),hoverinfo="text")}
    core_plot_finish(p,"达标时间区间（距当前 DCO）","占全部成功模拟的比例",TRUE,TRUE)})
  output$target_distribution_note <- renderUI({s<-view();p(class="field-note","柱形分布保留全部成功轮次；超过窗口的轮次单独列示。日期摘要仍按无条件分位数计算。",
    if(s$r$config$input_mode=="parameters"&&s$r$config$target<=s$r$known_events)"已达到目标，但参数输入未提供历史达标日期。")})
  output$group_readout <- renderDT({s<-view();req(s$r$group_curves);day<-core_result_grid(s$r)[s$i]
    d<-s$r$group_curves;d<-d[d$day==day & d$method %in% core_result_methods(s$r,s$m),,drop=FALSE]
    core_result_table(data.frame(组别=d$group,模型=d$model,日期=core_result_dates(s$r,d$day),均值=d$mean,
      `2.5%事件数`=d$lower,累计中位数=d$median,`97.5%累计事件数`=d$upper,check.names=FALSE))})
}

core_simulation_display_ui <- function() {
  tagList(fields(selectInput("sim_display_group","显示组别",c("全部组别"="all")),
    div(class="result-switches",checkboxInput("sim_display_ci","显示 KM 95% 置信区间",TRUE),checkboxInput("sim_display_censor","标记删失",FALSE))),
    p(class="field-note","以下图表使用所选试验和截点的观察数据。点击图例切换曲线；工具栏支持缩放和保存 PNG。显示设置不重新生成数据。"),
    navset_card_tab(id="sim_result_tabs",
      nav_panel("生存与在险",plotlyOutput("sim_km",height="420px"),DTOutput("sim_summary",fill=FALSE),
        section("固定随访时点的生存率",plotlyOutput("sim_fixed_plot",height="300px"),DTOutput("sim_fixed_table",fill=FALSE)),
        section("在险人数",plotlyOutput("sim_risk_plot",height="260px"),DTOutput("sim_risk",fill=FALSE))),
      nav_panel("事件与截点",section("所选截点的观察状态",plotlyOutput("sim_status_plot",height="320px")),
        section("同一试验的多个 DCO",p(class="field-note","各截点共享患者轨迹；每个点仅使用该 DCO 前的观察记录。"),plotlyOutput("sim_cut_plot",height="330px"),DTOutput("sim_cut_table",fill=FALSE))),
      nav_panel("重复试验",p(class="field-note","使用所选截点的全部重复试验。箱线图显示中位数、四分位数和 1.5×IQR 须；散点为各次试验。"),
        plotlyOutput("sim_repeat_events",height="330px"),plotlyOutput("sim_repeat_status",height="280px"),
        plotlyOutput("sim_repeat_median",height="330px"),p(class="field-note","中位生存时间图仅包含可估计的试验；NR 与无入组者在可估计状态图中保留，不参与时间分位数。"),DTOutput("sim_overview",fill=FALSE)),
      nav_panel("观察数据",DTOutput("sim_data",fill=FALSE))),
    div(class="export-bar",downloadButton("sim_observed_download","观察数据 CSV"),downloadButton("sim_adtte_download","ADTTE CSV")),uiOutput("sim_export_note"),
    p(class="field-note","观察数据与 ADTTE 导出包含所选试验/截点的全部组别，图表的显示组别筛选不改变导出文件。"))
}
core_sim_color <- function(r,group) {
  i<-match(group,vapply(r$config$groups,function(g)g$name,character(1)))
  if(is.na(i))i<-1L
  core_result_palette[(i-1)%%length(core_result_palette)+1]
}
core_sim_filter <- function(d,b=NULL,k=NULL,g="all") {
  if(!is.null(b))d<-d[d$SIMID==b,,drop=FALSE]
  if(!is.null(k))d<-d[d$CUTID==k,,drop=FALSE]
  if("scope" %in% names(d))d<-d[d$scope=="group",,drop=FALSE]
  if(!is.null(g)&&g!="all")d<-d[d$group==g,,drop=FALSE]
  d
}
register_core_simulation_results <- function(input,output,session,result) {
  observeEvent(result(),{r<-result();req(r);gs<-unique(r$observed$group)
    updateSelectInput(session,"sim_display_group",choices=c("全部组别"="all",setNames(gs,gs)),selected="all")})
  view <- reactive({r<-result();req(r,input$sim_trial,input$sim_view_cut)
    list(r=r,b=as.integer(input$sim_trial),k=as.integer(input$sim_view_cut),g=input$sim_display_group %||% "all",f=time_factor(r$config$display_unit),u=time_label(r$config$display_unit))})
  take <- function(d,s,b=s$b,k=s$k) core_sim_filter(d,b,k,s$g)
  output$sim_km <- renderPlotly({s<-view();d<-take(s$r$curves,s);if(!nrow(d))return(core_empty_plot("此截点和组别没有入组者。"));p<-plotly::plot_ly()
    for(j in seq_along(unique(d$group))) {z<-d[d$group==unique(d$group)[j],,drop=FALSE];z<-z[order(z$time_day),,drop=FALSE]
      col<-core_sim_color(s$r,z$group[1]);x<-z$time_day/s$f
      if(isTRUE(input$sim_display_ci)) {
        p<-plotly::add_trace(p,x=x,y=z$lower,type="scatter",mode="lines",line=list(width=0,shape="hv"),showlegend=FALSE,legendgroup=z$group[1],hoverinfo="skip",connectgaps=FALSE)
        p<-plotly::add_trace(p,x=x,y=z$upper,type="scatter",mode="lines",line=list(width=0,shape="hv"),fill="tonexty",fillcolor=grDevices::adjustcolor(col,alpha.f=.14),showlegend=FALSE,legendgroup=z$group[1],hoverinfo="skip",connectgaps=FALSE)
      }
      txt<-paste0(z$group,"<br>随访时间：",round(x,3),s$u,"<br>KM：",round(z$survival*100,2),"%<br>95% CI：",round(z$lower*100,2),"–",round(z$upper*100,2),"%<br>时点前在险：",z$n_risk,"<br>事件：",z$n_event,"；删失：",z$n_censor)
      p<-plotly::add_trace(p,x=x,y=z$survival,type="scatter",mode="lines",line=list(color=col,width=2,shape="hv"),name=z$group[1],legendgroup=z$group[1],text=txt,hoverinfo="text")
      if(isTRUE(input$sim_display_censor)) {q<-which(z$n_censor>0)
        p<-plotly::add_markers(p,x=x[q],y=z$survival[q],marker=list(color=col,symbol="line-ns-open",size=7),showlegend=FALSE,legendgroup=z$group[1],text=txt[q],hoverinfo="text")}
    }
    core_plot_finish(p,paste0("个体随访时间（",s$u,"）"),"KM 生存率",TRUE)})
  output$sim_fixed_plot <- renderPlotly({s<-view();d<-take(s$r$fixed,s);d<-d[is.finite(d$survival),,drop=FALSE]
    if(!nrow(d))return(core_empty_plot("固定时点没有可估计生存率；超出观察范围的时点不外推。"));p<-plotly::plot_ly()
    for(j in seq_along(unique(d$group))) {z<-d[d$group==unique(d$group)[j],,drop=FALSE]
      p<-plotly::add_markers(p,x=z$time_day/s$f,y=z$survival,name=z$group[1],marker=list(color=core_sim_color(s$r,z$group[1]),size=8),
        error_y=list(type="data",symmetric=FALSE,array=z$upper-z$survival,arrayminus=z$survival-z$lower,visible=TRUE),
        text=paste0(z$group,"<br>随访：",round(z$time_day/s$f,3),s$u,"<br>生存率：",round(z$survival*100,2),"%<br>95% CI：",round(z$lower*100,2),"–",round(z$upper*100,2),"%<br>在险人数：",z$n_risk),hoverinfo="text")}
    core_plot_finish(p,paste0("随访时间（",s$u,"）"),"KM 生存率及 95% CI",TRUE)})
  output$sim_risk_plot <- renderPlotly({s<-view();d<-take(s$r$fixed,s);if(!nrow(d))return(core_empty_plot("没有在险人数记录。"))
    times<-sort(unique(d$time_day));gs<-unique(d$group)
    z<-matrix(NA_real_,length(gs),length(times));txt<-matrix("",length(gs),length(times))
    for(i in seq_len(nrow(d))) {a<-match(d$group[i],gs);b<-match(d$time_day[i],times);z[a,b]<-d$n_risk[i]
      txt[a,b]<-paste0(d$group[i],"<br>随访：",round(d$time_day[i]/s$f,3),s$u,"<br>在险人数：",d$n_risk[i],"<br>生存率状态：",d$status[i])}
    p<-plotly::plot_ly(x=as.character(round(times/s$f,3)),y=gs,z=z,type="heatmap",text=txt,hoverinfo="text",
      colorscale=list(c(0,"#eff6f6"),c(1,"#126b72")),colorbar=list(title="人数"),showscale=TRUE)
    ann<-list()
    top<-max(z,na.rm=TRUE)
    for(a in seq_along(gs))for(b in seq_along(times))if(is.finite(z[a,b]))
      ann[[length(ann)+1]]<-list(x=as.character(round(times[b]/s$f,3)),y=gs[a],text=as.character(z[a,b]),showarrow=FALSE,font=list(color=if(z[a,b]>.55*top)"white" else "#173d50",size=13))
    core_plot_finish(p,paste0("随访时间（",s$u,"）"),"组别") |> plotly::layout(annotations=ann)})
  output$sim_status_plot <- renderPlotly({s<-view();d<-take(s$r$summary,s);if(!nrow(d))return(core_empty_plot("没有观察记录。"));p<-plotly::plot_ly()
    for(j in 1:3) {key<-c("events","dropouts","administrative")[j];nm<-c("事件","永久退出","行政删失")[j]
      p<-plotly::add_bars(p,textposition="none",x=d$group,y=d[[key]],name=nm,marker=list(color=core_result_palette[j]),text=paste0(d$group,"<br>",nm,"：",d[[key]],"<br>已入组：",d$n),hoverinfo="text")}
    core_plot_finish(p,"组别","人数",bar=TRUE) |> plotly::layout(barmode="stack")})
  cut_data <- reactive({s<-view();take(s$r$summary,s,k=NULL)})
  output$sim_cut_plot <- renderPlotly({s<-view();d<-cut_data();if(!nrow(d))return(core_empty_plot("没有截点记录。"));p<-plotly::plot_ly()
    for(j in seq_along(unique(d$group))) {z<-d[d$group==unique(d$group)[j],,drop=FALSE];z<-z[order(z$DCO_DAY),,drop=FALSE]
      p<-plotly::add_trace(p,x=z$DCO_DAY/s$f,y=z$events,type="scatter",mode="lines+markers",name=z$group[1],line=list(color=core_sim_color(s$r,z$group[1])),marker=list(color=core_sim_color(s$r,z$group[1]),size=7),
        text=paste0(z$group,"<br>研究 DCO：",round(z$DCO_DAY/s$f,3),s$u,"<br>事件：",z$events,"<br>已入组：",z$n,"<br>永久退出：",z$dropouts,"<br>行政删失：",z$administrative),hoverinfo="text")}
    core_plot_finish(p,paste0("研究 DCO（",s$u,"）"),"累计观察事件数")})
  output$sim_cut_table <- renderDT({s<-view();d<-cut_data();tab<-data.frame(截点=d$CUTID,研究DCO=d$DCO_DAY/s$f,组别=d$group,入组人数=d$n,事件数=d$events,永久退出=d$dropouts,行政删失=d$administrative,中位数状态=d$median_status);names(tab)[2]<-paste0("研究DCO（",s$u,"）");core_result_table(tab)})
  repeat_data <- reactive({s<-view();take(s$r$summary,s,b=NULL)})
  box <- function(median=FALSE) {s<-view();d<-repeat_data();if(median)d<-d[is.finite(d$median_day),,drop=FALSE]
    if(!nrow(d))return(core_empty_plot(if(median)"本截点没有可估计的中位生存时间；请查看可估计状态。" else "没有重复试验记录。"))
    p<-plotly::plot_ly()
    for(j in seq_along(unique(d$group))) {z<-d[d$group==unique(d$group)[j],,drop=FALSE];y<-if(median)z$median_day/s$f else z$events
      p<-plotly::add_trace(p,y=y,x=rep(z$group[1],length(y)),type="box",name=z$group[1],boxpoints="all",jitter=.25,pointpos=0,
        marker=list(color=core_sim_color(s$r,z$group[1]),size=5),line=list(color=core_sim_color(s$r,z$group[1])),
        text=paste0("试验：",z$SIMID,"<br>组别：",z$group,"<br>",if(median)paste0("中位随访时间（",s$u,"）：") else "观察事件数：",round(y,3)),hoverinfo="text")}
    core_plot_finish(p,"组别",if(median)paste0("KM 中位时间（",s$u,"；可估计试验）") else "各试验观察事件数",bar=TRUE)}
  output$sim_repeat_events <- renderPlotly(box())
  output$sim_repeat_median <- renderPlotly(box(TRUE))
  output$sim_repeat_status <- renderPlotly({d<-repeat_data();if(!nrow(d))return(core_empty_plot("没有重复试验记录。"));p<-plotly::plot_ly();gs<-unique(d$group)
    status<-ifelse(d$n==0,"无入组者",ifelse(is.finite(d$median_day),"可估计","NR"))
    for(j in 1:3) {nm<-c("可估计","NR","无入组者")[j];n<-vapply(gs,function(g)sum(d$group==g & status==nm),integer(1));tot<-vapply(gs,function(g)sum(d$group==g),integer(1))
      p<-plotly::add_bars(p,textposition="none",x=gs,y=n/tot,name=nm,marker=list(color=core_result_palette[j]),text=paste0(gs,"<br>",nm,"：",n," / ",tot,"<br>比例：",round(100*n/tot,2),"%"),hoverinfo="text")}
    core_plot_finish(p,"组别","中位数可估计状态",TRUE,TRUE) |> plotly::layout(barmode="stack",yaxis=list(range=c(0,1)))})
}
