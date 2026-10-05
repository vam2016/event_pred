# Protect authored TeX before CommonMark can interpret underscores or backslashes.
handbook_markdown_html <- function(text) {
  pattern <- "(?s)```.*?```|`[^`\\n]+`|\\$\\$.*?\\$\\$|(?<![\\\\$])\\$(?!\\$)[^\\n$]+(?<!\\\\)\\$"
  matches <- gregexpr(pattern,text,perl=TRUE)[[1]]
  if(matches[1]==-1) return(commonmark::markdown_html(text,extensions=TRUE))
  lengths <- attr(matches,"match.length"); snippets <- substring(text,matches,matches+lengths-1)
  replacements <- snippets; tokens <- character(length(snippets)); math <- startsWith(snippets,"$")
  for(j in which(math)) {
    tokens[j] <- sprintf("HANDBOOKMATHTOKEN%05dEND",j)
    replacements[j] <- tokens[j]
  }
  chunks <- character(); cursor <- 1L
  for(j in seq_along(matches)) {
    if(matches[j]>cursor) chunks<-c(chunks,substring(text,cursor,matches[j]-1))
    chunks<-c(chunks,replacements[j]);cursor<-matches[j]+lengths[j]
  }
  chunks<-c(chunks,substring(text,cursor));html<-commonmark::markdown_html(paste0(chunks,collapse=""),extensions=TRUE)
  for(j in which(math)) {
    display <- startsWith(snippets[j],"$$"); n<-if(display)2L else 1L
    tex<-substring(snippets[j],n+1,nchar(snippets[j])-n)
    script<-paste0('<script type="math/tex',if(display)'; mode=display' else '', '">',tex,'</script>')
    html<-gsub(tokens[j],script,html,fixed=TRUE)
  }
  html
}
handbook_html <- function(path="docs/METHODS.md") {
  handbook_markdown_html(paste(readLines(path,warn=FALSE,encoding="UTF-8"),collapse="\n"))
}
handbook_reading_html <- function(path="docs/METHODS.md") {
  cache <- "docs/handbook-rendered.html"; metadata <- "docs/handbook-rendered.json"
  if(file.exists(cache) && file.exists(metadata)) {
    m <- jsonlite::read_json(metadata)
    if(identical(unname(tools::md5sum(path)),m$markdown_md5))
      return(paste(readLines(cache,warn=FALSE,encoding="UTF-8"),collapse="\n"))
  }
  # Authored formulas remain available after edits pending cache regeneration.
  handbook_html(path)
}
handbook_standalone_html <- function(path="docs/METHODS.md") {
  css<-paste(readLines("www/handbook.css",warn=FALSE),collapse="\n")
  contents <- handbook_reading_html(path)
  runtime <- if(grepl('type="math/tex',contents,fixed=TRUE))
    '<script src="https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>' else ''
  paste0('<!DOCTYPE html><html lang="zh-CN"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>生存模拟与事件预测工作手册</title>',
    '<style>:root{--ink:#173d50;--accent:#126b72;--muted:#607685;--line:#dce5e9}body{margin:0;font-family:system-ui,"PingFang SC",sans-serif;background:#f3f6f8}.handbook-content{max-width:980px;margin:auto;background:white}table{border-collapse:collapse}td,th{padding:8px;border:1px solid #dce5e9}',css,'</style>',
    runtime,'</head><body><main class="handbook-content">',contents,'</main></body></html>')
}

handbook_index <- function() {
  list(
    "开始使用"=c("hb-workflow"="1 操作流程", "hb-foundation"="2 统计基础"),
    "事件时间模型"=c("hb-exponential"="3.1 指数", "hb-weibull"="3.2 Weibull", "hb-pwe"="3.3 分段指数", "hb-lognormal"="3.4 Log-normal", "hb-loglogistic"="3.5 Log-logistic", "hb-km"="3.6 KM 与指数尾部", "hb-gompertz"="3.7 Gompertz", "hb-cure"="3.8 Cure-Weibull", "hb-mixture"="3.9 双成分 Weibull"),
    "参数与未来过程"=c("hb-plugin"="4.1 固定参数", "hb-bootstrap"="4.2 Bootstrap", "hb-gamma"="4.3 Gamma 后验", "hb-mcmc"="4.4 Weibull MCMC", "hb-process"="5 入组、脱落与上报"),
    "联合预测与评价"=c("hb-counts"="6 事件数与目标日期", "hb-groups"="7 已知组别", "hb-aic"="8 AIC 预测混合", "hb-validation"="9 回测与风险情景", "hb-outputs"="10 输出与核对"),
    "附录"=c("hb-numerics"="11 数值实现", "hb-scope"="12 范围与参考资料", "hb-simulation"="13 设计阶段生存模拟", "hb-format"="14 文档与公式格式", "hb-conditional-simulation"="15 IA→Final 条件模拟", "hb-study"="16 重复试验与设计评价", "hb-nph"="17 非比例风险与预设比较", "hb-sequential"="18 组序贯与条件功效", "hb-patient-prediction"="19 IA个体预测拒绝概率", "hb-adaptation"="20 两阶段事件数重估", "hb-patient-adaptation"="21 独立患者队列重估", "hb-adaptive-prediction"="22 实际IA重估后的条件预测", "hb-process-scenarios"="23 新队列过程后验与情景比较", "hb-sequential-inference"="24 组序贯调整推断", "hb-ia-history"="25 历史多次IA续推", "hb-stagewise-extensions"="26 双侧与无效停止调整推断", "hb-binding-beta"="27 约束性beta消耗设计", "hb-real-ia-inference"="28 历史IA调整推断", "hb-joint-survival"="29 PFS/OS联合生存模拟", "hb-batch-research"="30 批量研究与assurance", "hb-calibration-cutrules"="31 Weibull校准与组合截点", "hb-batch-nph-methods"="32 批量NPH与同轮方法比较", "hb-batch-sequential"="33 批量组序贯与停止资源", "hb-batch-workspace"="34 研究库、续跑与配置导入", "hb-joint-research"="35 联合终点设计研究", "hb-joint-marginal"="36 边际生存率与RMST参考", "hb-adaptive-batch"="37 独立队列重估批量研究", "hb-adaptive-reference"="38 IA规则参考与条件错误", "hb-conditional-batch"="39 实际IA条件批量与研究库", "hb-heterogeneity"="40 协变量分层中心与脆弱性", "hb-beta-history-repeated"="41 beta历史与重复推断", "hb-shared-known-hazard"="42 已知风险共享患者重估", "hb-shared-cohort-riskset"="43 队列风险集共享患者推断", "hb-multistate-conditional"="44 实际多状态拟合与条件预测", "hb-joint-likelihood-sequential"="45 联合计数似然序贯与适应", "hb-observation-process"="46 访视退出与中央记录", "hb-interval-flexible"="47 区间似然与柔性生存", "hb-blinded-stacking"="48 盲态混合与预测组合", "hb-report-backlog"="49 已知临床报告积压", "hb-multiarm"="50 多臂一次Final", "hb-two-in-one"="51 独立两阶段与2-in-1", "hb-design-search"="52 候选搜索与独立确认", "hb-reverse-event-calibration"="53 期望事件反向校准", "hb-development-scope"="54 入口与开发范围", "hb-dunnett-closure"="55 Dunnett型与闭合交集", "hb-multiarm-event-driven"="56 多臂事件目标与搜索", "hb-cumulative-hazard-splines"="57 RP与I-spline累计风险", "hb-multiarm-intervals"="58 同时区间与组合反演", "hb-ia-expected-inverse"="59 IA期望事件反校准", "hb-multitarget-calibration"="60 多目标与识别诊断", "hb-panel-markov-likelihood"="61 Panel Markov过滤似然", "hb-panel-expected-counts"="62 Panel条件期望计数"))
}
handbook_ui <- function() {
  reading <- handbook_reading_html()
  reading <- if(grepl('type="math/tex',reading,fixed=TRUE)) withMathJax(HTML(reading)) else HTML(reading)
  index <- handbook_index()
  toc <- lapply(names(index),function(group) {
    links <- lapply(seq_along(index[[group]]),function(j)
      tags$a(href=paste0("#",names(index[[group]])[j]),unname(index[[group]][j]),onclick="event.preventDefault();document.getElementById(this.hash.slice(1)).scrollIntoView({behavior:'instant',block:'start'});"))
    div(class="toc-group",span(class="toc-heading",group),links)
  })
  toolbar <- div(class="handbook-toolbar",
    div(span(class="manual-label","event_pred · 工作手册"),p("模型推导、参数设置、平台操作与结果解释")),
    div(class="manual-actions",downloadButton("handbook_download","下载手册 Markdown"),downloadButton("handbook_html_download","下载阅读版 HTML"),
      tags$button(type="button",class="btn btn-outline-secondary",onclick="window.print()","打印 / 保存 PDF")))
  contents <- tags$details(class="handbook-toc",open=NA,tags$summary("章节目录"),
    tags$nav(`aria-label`="工作手册章节",toc))
  div(class="handbook",id="handbook",toolbar,
    div(class="handbook-layout",contents,
      div(class="reading-layout handbook-content",reading)))
}
