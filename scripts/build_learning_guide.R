# Run from the source root; emits a self-contained, offline learning HTML.
source("R/handbook.R")
source("R/core_handbook.R")
md <- paste(readLines("docs/SurvCast_STATISTICIAN_GUIDE.md",warn=FALSE,encoding="UTF-8"),collapse="\n")
e <- new.env(parent=globalenv()); invisible(sys.source("app_core.R",envir=e))
stopifnot(identical(readLines("docs/CORE_CASES.md",warn=FALSE),e$core_case_markdown()))
catalog <- paste(e$core_case_markdown()[-1],collapse="\n")
catalog <- gsub("(?m)^## ","#### ",catalog,perl=TRUE)
md <- sub("<!-- CORE_CASE_CATALOG -->",catalog,md,fixed=TRUE)
chapters <- regmatches(md,gregexpr('<a id="s[0-9]{2}"></a>',md,perl=TRUE))[[1]]
stopifnot(length(chapters)==34L,length(unique(chapters))==34L)
body <- handbook_markdown_html(md)
refs <- regmatches(body,gregexpr('src="learning-assets/[^\"]+\\.svg"',body,perl=TRUE))[[1]]
for(ref in unique(refs)) {
  relative <- sub('^src="(.*)"$','\\1',ref)
  path <- file.path("docs",relative)
  stopifnot(file.exists(path))
  data <- jsonlite::base64_enc(readBin(path,"raw",n=file.info(path)$size))
  body <- gsub(ref,paste0('src="data:image/svg+xml;base64,',gsub("\n","",data),'"'),body,fixed=TRUE)
}
# Move the authored TOC into a responsive navigation panel.
toc <- regmatches(body,regexpr('(?s)<h3>目录</h3>\\s*<ul>.*?</ul>',body,perl=TRUE))
stopifnot(length(toc)==1,nchar(toc)>0)
body <- sub('(?s)<h3>目录</h3>\\s*<ul>.*?</ul>','',body,perl=TRUE)
nav <- sub('<h3>目录</h3>','',toc,fixed=TRUE)
engine <- paste(readLines("www/core-mathjax.js",warn=FALSE),collapse="\n")
engine <- paste(strsplit(engine,"</script",fixed=TRUE)[[1]],collapse="<\\/script")
css <- paste(readLines("docs/learning-guide.css",warn=FALSE),collapse="\n")
js <- "document.addEventListener('DOMContentLoaded',()=>{const d=document.getElementById('contents');if(matchMedia('(min-width: 1000px)').matches)d.open=true;for(const t of document.querySelectorAll('article table')){let w=document.createElement('div');w.className='table-wrap';t.replaceWith(w);w.append(t);}document.getElementById('print').onclick=()=>print();document.addEventListener('scroll',()=>{let e=document.documentElement;let ratio=e.scrollHeight>e.clientHeight?e.scrollTop/(e.scrollHeight-e.clientHeight):1;document.getElementById('progress').style.width=(ratio*100)+'%';},{passive:true});});"
html <- paste0('<!doctype html><html lang="zh-CN"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><meta name="description" content="SurvCast核心平台统计师学习手册，含模型推导、参数、算法、结果解读和复现案例"><title>SurvCast · 统计师学习手册</title><style>',css,'</style><script>',core_math_runtime(),'</script><script>',engine,'</script><script>',core_math_render_script(),'</script><script>',js,'</script></head><body><div id="progress" aria-hidden="true"></div><header class="top"><span>SurvCast · 学习手册 2.0-dev</span><button type="button" id="print">打印 / 保存 PDF</button></header><div class="reading-shell"><aside><details id="contents"><summary>目录 · 34 章</summary><nav aria-label="章节目录">',nav,'</nav></details></aside><article class="core-handbook">',body,'<footer>学习稿 2.0-dev · 2026-10-11 · 正文、公式和插图支持离线阅读</footer></article></div></body></html>')
output <- Sys.getenv("LEARNING_OUTPUT","reports/learning-v2/SurvCast_STATISTICIAN_GUIDE.html")
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
writeLines(html,output,useBytes=TRUE)
for(id in names(e$core_case_catalog())) {
  folder <- file.path(dirname(output),"cases",id);dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  x <- e$core_case_spec(id)
  jsonlite::write_json(x,file.path(folder,"survcast_case.json"),auto_unbox=TRUE,pretty=TRUE,digits=NA,null="null",dataframe="rows")
  writeLines(e$core_case_replay_script(),file.path(folder,"survcast_case_replay.R"),useBytes=TRUE)
  if(isTRUE(x$config$input_mode %in% "adtte"))write.csv(e$core_case_adtte(x),file.path(folder,"survcast_case_adtte.csv"),row.names=FALSE)
}
cat('Self-contained HTML generated. Embedded illustrations:',length(unique(refs)),'\n')
