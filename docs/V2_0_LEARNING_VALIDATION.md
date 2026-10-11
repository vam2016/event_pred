# V2-402 学习资料核对与版本化验收

2026-10-11；development/v2.0；核心2.0.0-dev；已验收引擎基线98fe9b1263c22a55cede8eb1060c805d5f67b91e。本机验收完成，用户已授权本批同步、自动检查及结果回填，远程同步/CI待执行。下一项V2-403整批验收及应用候选包；本次资料包版本为2.0-dev。

## 来源与当前内容

找到原本机输出镜像、1.0学习文件夹、独立HTML及ZIP。三个Markdown副本SHA256均为d499ccf82c4c5c3d86cf2c3309d34b5e0fb5e721523789f71deb1f1cdda80a43，三个HTML副本均为1240b1b75eaa49a3ad1f91400ce145fb60e2df56cbe3a24d5709e46376cd72b3；ZIP完整性与清单20个文件哈希均通过。原正文更新为10月7日、清单authored_date为10月6日，保留差异，没有改写历史元数据。原28章、四个数值案例A–D、六个流程场景、7图和12自测均找到。六场景没有六套独立计算结果。

历史Markdown、原清单和旧阅读检查原样保存在docs/learning-history；本次核对证据在source-inventory.json。历史362公式/26表阅读检查不冒充当前检查。当前可编辑手册在docs/SurvCast_STATISTICIAN_GUIDE.md；保留原28章结构，按实际已验收能力更新输入/分布/模拟/不确定性字典及开放范围，追加29–34章：SAS、对数时间、Bootstrap、事件驱动、回测/情景、版本与复现。追加正文与LEARNING_V2_ADDENDUM.md一致；新版四例由R/core_cases.R共用定义，构建时嵌入，区别于原A–D。

## 本机实际检查

|检查|结果|范围|
|---|---|---|
|check_learning_guide.R|186项通过|132项源文件哈希；54项历史来源/章节/原案例数值/当前4例真实脚本重放|
|旧案例A|通过|40+Binomial(100,0.5)解析均值90、SD5、分位80/90/100；1000轮均值在4个解析MCSE内|
|旧案例B|通过|有限60人Poisson入组与退出的积分参照23.52552，总均值112.22204；模拟在4 MCSE内|
|旧案例C|通过|连续日45/64/59/23，191人日、2事件，独立MLE=2/191|
|旧案例D及原图CSV/JSON|通过|20试验×3DCO状态守恒；5个CSV及7个数值JSON分量重跑匹配|
|新增4例|通过|构建JSON/脚本与核心定义一致；真实Rscript重放及逐表CSV比较；2个ADTTE保持逐轮预测路径|
|check_learning_guide.cjs|53项通过|实际离线HTML：378公式、27表、7嵌入SVG、12自测、34章目录；桌面1440/手机390、滚动锚点、自测、打印CSS|
|plot_learning_figures.py|7图通过|从已核对CSV在独立临时目录重绘；全部SVG字节与原图一致|
|package_learning_guide.py|44个ZIP条目通过|43个载荷SHA256逐条校验，另有package-files.json；ZIP CRC完整性通过|
|check_extensions.R|153文件/9项通过|两个应用入口解析/UI构造/server注册，未运行高级研究方法|

浏览器全程阻断HTTP请求，实际零请求、零脚本错误、零公式merror/fallback、无文档水平溢出；首轮导航断言未等待CSS平滑滚动完成，修正等待位置稳定后最终53项通过。查看了390px截图。仅验证打印CSS，没有声明PDF分页完美。原图已核对，不为新增模型伪造新图。

环境为macOS、R4.6.0、现有R库/Playwright/Chrome；绘图使用原NumPy/Matplotlib依赖，脚本支持LEARNING_ASSETS输出到临时目录。生成HTML、病例输入/脚本、ZIP、截图仅在忽略的reports/learning-v2保存；不提交大型HTML/ZIP或实际应用下载。

## 可复现交付

在源码根目录执行：

```
Rscript scripts/learning_examples.R
python3 scripts/package_learning_guide.py --manifest-only
Rscript scripts/build_learning_guide.R
Rscript scripts/check_learning_guide.R
node scripts/check_learning_guide.cjs
python3 scripts/package_learning_guide.py
```

浏览器脚本需要Playwright和Chrome，可通过PLAYWRIGHT_MODULE/CHROME_PATH指定安装位置。原案例重跑会更新教学数据；修改文档、代码、数据或脚本后应更新源文件清单并重做数值/阅读检查。打包拒绝缺失、失败或与当前HTML/源清单不匹配的检查记录。已生成HTML不需要R/Python即可离线阅读。ZIP含editable Markdown、7图/数据、四例JSON/R/适用CSV、来源、脚本、验证和README；案例R脚本需要同版本完整源码和R依赖，本包不含应用运行源码。

工作流已新增离线稿构建与186项核对，文档/资源/脚本变更触发CI。远程将运行数值与Rscript重放，离线Chrome和Python重绘仍仅为本机证据。此处远程结果待实际同步运行后回填。

## 限制与下一步

本批全为项目资料及合成病例，无真实患者数据。未扩大Bootstrap范围、未新增方法推断、未改变核心应用计算代码；源码校验绑定学习稿及实际模型/运行依赖。理论章节不等于已开放功能，不验证临床假设、名义覆盖率或功效。旧六场景为流程说明。学习ZIP不构成2.0.0-rc.1应用包、目标环境验收、正式发布或公网部署。

同步文件清单及状态见validation/v2_learning_delivery.json；下一项V2-403独立整理整批验收、实际开放清单与候选应用包解压启动。

## 同步状态

本机源码提交14466ba；本批40个文档、脚本、资源、工作流和合成验证记录文件已完成本机验收。自动审批拒绝GitHub树对象上传，理由为此前明确授权仅覆盖V2-401，未覆盖V2-402本批40文件导出到vam2016/event_pred。未绕过拒绝，未创建远程树/提交/分支更新，远程CI未执行；用户先选择仅保留本机成果，随后明确说明点错并要求同步GitHub，已恢复本批40文件同步、自动检查及同批文档/JSON回填的授权。现在准备同步到vam2016/event_pred的development/v2.0并回填实际检查结果。文件清单见validation/v2_learning_delivery.json。
