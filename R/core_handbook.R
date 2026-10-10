# Use the vendored equation runtime; the core manual does not load the full cache.
core_math_runtime <- function() paste0(
  "window.MathJax={loader:{load:[]},tex:{packages:['base','ams'],autoload:false},svg:{fontCache:'local'},startup:{typeset:false}};\n")
core_math_render_script <- function() paste0(
  "let coreManualRendering=false;async function renderCoreManual(){if(coreManualRendering)return;coreManualRendering=true;",
  "if(!window.MathJax||!MathJax.startup){coreManualRendering=false;return;}await MathJax.startup.promise;",
  "for(const s of document.querySelectorAll('.core-handbook script[type^=\"math/tex\"]')){",
  "try{const display=s.type.includes('mode=display');const node=await MathJax.tex2svgPromise(s.textContent,{display});",
  "const wrap=document.createElement('span');wrap.className=display?'hb-equation hb-display':'hb-equation hb-inline';",
  "wrap.appendChild(node);s.replaceWith(wrap);}catch(e){const note=document.createElement('pre');note.textContent=s.textContent;s.replaceWith(note);}}coreManualRendering=false;}",
  "document.addEventListener('DOMContentLoaded',renderCoreManual);document.addEventListener('shiny:connected',renderCoreManual);")
core_math_head <- function() tagList(
  tags$script(HTML(core_math_runtime())),tags$script(src="core-mathjax.js"),tags$script(HTML(core_math_render_script())))
core_handbook_ui <- function() div(class="handbook core-handbook",
  div(class="handbook-toolbar",div(span(class="manual-label","SurvCast · 2.0 核心开发手册"),p("操作、参数、模型推导与结果解释")),
    div(class="manual-actions",downloadButton("handbook_download","Markdown"),downloadButton("handbook_html_download","离线 HTML"),tags$button(type="button",class="btn btn-outline-secondary",onclick="window.print()","打印 / PDF"))),
  tags$nav(class="handbook-toc",`aria-label`="核心手册章节",lapply(1:8,function(i)tags$a(href=paste0("#core-section-",i),paste(i,c("任务与操作","时间与数据","条件预测","指数","Weibull","PWE","入组与模拟","结果与复现")[i])))),
  div(class="reading-layout handbook-content",HTML(handbook_html("docs/CORE_HANDBOOK.md"))))
core_handbook_standalone_html <- function() {
  css <- paste(readLines("www/handbook.css",warn=FALSE),collapse="\n")
  engine <- paste(readLines("www/core-mathjax.js",warn=FALSE),collapse="\n")
  # Escape script end markers for an inline standalone runtime.
  engine <- paste(strsplit(engine,"</script",fixed=TRUE)[[1]],collapse="<\\/script")
  paste0('<!doctype html><html lang="zh-CN"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>SurvCast 生存事件预测与模拟工作台 · 核心工作手册</title><style>body{font-family:system-ui,"PingFang SC",sans-serif;margin:24px;line-height:1.7}.handbook-content{max-width:980px;margin:auto}table{border-collapse:collapse}th,td{padding:8px;border:1px solid #ddd}',css,'</style><script>',core_math_runtime(),'</script><script>',engine,'</script><script>',core_math_render_script(),'</script></head><body><main class="core-handbook handbook-content">',handbook_html("docs/CORE_HANDBOOK.md"),'</main></body></html>')
}
