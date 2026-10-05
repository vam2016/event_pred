# Task entry and local research library controls.
register_batch_workspace_help <- function(h) {
  c(h,c(
    ba_run_source="选择当前页面参数或导入无损配置JSON，二选一。JSON模式直接使用文件中的全部模拟参数，不读取隐藏页面参数。",
    ba_run_name="本次研究名称，1–100字符，例如PH功效基准；只用于本机记录和导出，不参与模拟。",
    ba_config_file="导入本平台v0.26及以后导出的event_pred.batch_config.v1 JSON，最多5MB；exact_config保留双精度数值、类型及无限边界。旧版可读JSON不支持导入。",
    ba_saved_run="选择同一浏览器工作区保存的研究。载入只显示快照；续跑只处理未记录的情景/轮次，不重试已处理的失败。代码、R与统计依赖需一致。",
    ba_library_archived="默认隐藏归档研究；勾选后可查看并恢复。归档保留文件及父子研究关系。",
    ba_rename="所选研究的新名称，1–100字符。重命名不改变配置、轮次或计算结果；运行中的研究不能改名。",
    ba_extend_reps="新研究每情景总重复数，需大于原请求数，且满足当前批量规模限制（每情景最多10000）。原研究不变，子研究复制已有轮次，再计算缺失轮次。",
    ba_collection_ids="选择1–20个研究并列汇总。保留各研究版本、设计、种子和原分母；不合并轮次，不自动计算跨研究差值或选最优方案。共享种子另列。"))
}
batch_workspace_entry_ui <- function() {
  tagList(section("参数入口",radioButtons("ba_run_source","本次参数来源",c("填写页面参数"="ui","导入配置JSON"="config_json"),inline=TRUE),
    conditionalPanel("input.ba_run_source === 'config_json'",fileInput("ba_config_file","无损配置JSON",accept=".json"),uiOutput("ba_import_note"),DTOutput("ba_import_summary",fill=FALSE)),
    conditionalPanel("input.ba_run_source === 'config_json' || input.ba_mode !== 'calibration'",textInput("ba_run_name","本次研究名称","设计研究"))))
}
batch_workspace_ui <- function() {
  nav_panel("研究库",value="ba_library",
    p(class="field-note","模拟配置与结果保存在本机服务目录，由当前浏览器工作区索引。换浏览器、清除站点数据或迁移服务目录后不会自动找回该索引；支持配置与结果下载。本模块尚未提供账号权限或多人研究共享。"),
    actionButton("ba_library_refresh","刷新研究列表"),checkboxInput("ba_library_archived","显示已归档研究",FALSE),DTOutput("ba_library_table",fill=FALSE),
    section("载入与续跑",selectInput("ba_saved_run","已保存研究",character()),
      div(class="export-bar",actionButton("ba_library_load","载入结果"),actionButton("ba_library_resume","补算未处理轮次")),
      p(class="field-note","续跑保留已有成功、无效和失败记录，仅运行尚无记录的键。运行中的研究不能再次启动；旧引擎结果可查看，续跑要求代码与依赖一致。"),
      numericInput("ba_extend_reps","追加后的每情景总重复数",200,min=20,max=10000,step=20),actionButton("ba_library_extend","创建追加子研究")),
    section("名称与归档",textInput("ba_rename","新名称",""),div(class="export-bar",actionButton("ba_library_rename","保存名称"),actionButton("ba_library_archive","归档"),actionButton("ba_library_restore","恢复归档"))),
    uiOutput("ba_library_note"),downloadButton("ba_library_index","研究索引CSV"),
    section("多个研究并列汇总",selectizeInput("ba_collection_ids","汇总研究",character(),multiple=TRUE,options=list(maxItems=20)),actionButton("ba_collection_run","生成汇总"),
      p(class="field-note","原始时间量保留日单位及原显示单位标签；同种子可能产生相关轮次。仅并列表，不合并估计，不进行跨研究检验。"),
      DTOutput("ba_collection_overview",fill=FALSE),DTOutput("ba_collection_methods",fill=FALSE),DTOutput("ba_collection_policies",fill=FALSE),DTOutput("ba_collection_resources",fill=FALSE),DTOutput("ba_collection_overlaps",fill=FALSE),
      div(class="export-bar",downloadButton("ba_collection_overview_download","主分析CSV"),downloadButton("ba_collection_methods_download","方法CSV"),downloadButton("ba_collection_policies_download","联合规则CSV"),downloadButton("ba_collection_resources_download","资源CSV"),downloadButton("ba_collection_overlaps_download","共享种子CSV"),downloadButton("ba_collection_configs","各研究配置JSON"),downloadButton("ba_collection_rds","汇总RDS"),downloadButton("ba_collection_report","汇总Markdown"))))
}
