# SurvCast 2.0 已完成与待完成盘点

核对日期：2026-10-10。基准：`development/v0.35` / `51f2127612fad08c983d7b9327cfc85b1f703fd8`。承接分支：`development/v2.0`。

## 1. 已完成的核心能力

|内容|开发结果|验证依据/尚需工作|
|---|---|---|
|三个核心入口|事件数、目标日期、单终点固定截点模拟|本次核心51项通过；当前为1.0.0-rc.1|
|模型与输入|指数、Weibull、PWE；参数/ADTTE CSV；合并/已知两组|本次覆盖独立似然/积分、单位、AVAL、删失、条件抽样和同轮分组相加|
|预测与模拟结果|新增/累计、逐点区间、目标概率/MCSE/未达标；观察/真值/ADTTE分离|本次核心检查通过；首版区间仍条件于给定/拟合参数|
|交互展示|曲线与时点联动、分布图、KM/在险、多截点/试验、表格及PNG/CSV|本次13项展示数据检查通过；31项浏览器交互通过为2026-10-06历史记录，未本轮重跑|
|冻结与复现|提交配置和种子、编辑输入不改旧结果、下载与脚本|历史7项下载重放及便携包证据；本轮未重跑浏览器下载|
|帮助与文档|核心手册、17个公式、参数帮助、启动说明|在线/离线公式检查为历史记录，本轮核对资料|
|本地启动与转移|Mac/Windows/Linux启动器、依赖准备、便携包|历史Mac实际启动/浏览器、Windows准备和Linux CI，不等同全部目标机器实测|
|学习资料|28章、4教学案例、6使用场景；Markdown/离线HTML/资料包|原本机输出中已有，2026-10-07场景更新；当前分支尚未纳入|

**1.0剩余收尾：**目标环境与访问方式、依赖固定、资源限制、目标环境核心示例、正式版本标记与回退包。当前资料没有正式公网发布完成证据。

## 2. 首批候选：已有实现，需要接通与验收

|能力|主要实现|当前状态|待完成|
|---|---|---|---|
|患者Bootstrap|`R/forecast.R`、`R/groups.R`|完整入口已有，核心限制plugin|患者/组别重抽样、拟合失败/有效次数、核心UI/解释/图表/导出及专项验证|
|XPT/SAS7BDAT|`R/inputs.R::read_adtte`及核心UI|核心已接通；37项输入、40项浏览器、16项实际下载重放通过|远程Linux/Windows通过；独立SAS系统/非英文编码样例未实测，见[SAS验收](V2_0_SAS_VALIDATION.md)|
|Log-normal/Log-logistic|`R/models.R`、`R/parameters.R`及参数UI/server|已有拟合/分布/参数实现|核心白名单、换算、参考曲线、独立分布/似然对照、帮助/导出|
|事件驱动模拟|`R/simulation.R`|已有cut_mode=target|核心接入、目标/窗口/并列/未达标、观察真值、复现|
|历史回测|`R/validation.R::backtest_forecast`|已有实现|当前截点、后续观察仅评分、未来计划与实际资料分离、图表与复现|
|少量情景比较|现有冻结配置、预测结果与研究工具|需确定轻量输出契约|预定情景、每场景配置；有配对才报告配对MCSE；不接入全部研究库复杂度|

主要来自历史预测/模拟功能，但历史v0.19通过不能替代当前代码验收。源码存在不等于扩展入口可加载或可发布。

## 3. 完整23个入口及后续安排

按`CAPABILITY_MATRIX.md`核对源码。高级项为“已有代码，待当前专项验证”；前三个核心范围以外没有在本次确认方法验收。第5、11项相关UI及第19–21项共用server的基线解析阻塞现已修复；高级方法本身仍待专项验收。

|序号|入口|主要源码（R/下同名.R）|批次|剩余重点|
|---|---|---|---|---|
|1|未来事件数|forecast/groups/core_workflow|A|Bootstrap、扩展模型、SAS、情景/回测|
|2|达标日期|forecast/groups/core_results|A|参数不确定性、未达标分母/日期|
|3|设计模拟|simulation/core_simulation_server|A|事件驱动截点与窗口状态|
|4|IA→Final|conditional_simulation/conditional_prediction|B|冻结IA、旧患者续推、原计划与未来假设|
|5|设计评价|study/nph/sequential/study_ui|B|先修UI解析；固定/标准组序贯独立校准|
|6|事件数重估|adaptation/patient_adaptation|B|独立队列、条件错误/功效、完整分母|
|7|历史IA推断|ia_history/history_inference/sequential_inference|B|完整快照、阶段排序、已停止路径、覆盖|
|8|PFS/OS联合模拟|joint_survival|C|联合生成、共享患者、转移时钟、真值隔离|
|9|联合终点设计研究|joint_research/joint_marginal|C|多重性、共同截点、失败/未知与边际参照|
|10|独立队列重估研究|adaptive_batch|B|队列隔离、配对、后台/下载重放|
|11|实际IA条件批量|conditional_batch/research_workbench_ui|B|先修UI解析；冻结起点、情景、失败与分母|
|12|异质患者研究|heterogeneity|C|协变量/层/中心/脆弱性、可识别效应|
|13|共享患者重估|shared_adaptation/shared_riskset|D|限定已知风险/精确共同队列；一般未知基准Cox仍缺口|
|14|多状态条件预测|multistate_prediction|C|精确路径、拟合/续推、Bootstrap完整患者路径|
|15|联合序贯与重估|joint_sequential|D|状态无关死亡限定模型；一般OS适应仍缺口|
|16|柔性与盲态预测|flexible_models/flexible_splines/flexible_prediction|C|RP/I-spline约束、区间似然、固定HR、权重；BMA/自由HR未完成|
|17|访视与报告过程|observation_process|C|临床/中央口径、已知积压、真值隔离；未知机制仍缺口|
|18|批量研究与参数校准|batch_research/batch_workspace/parameter_calibration|D|库/快照、RNG、取消、续跑、版本守卫、重放|
|19|多臂与两阶段|multiarm_design/multiarm_multiplicity/round_five_server|D|先修server解析；共享对照/独立阶段、FWER、近似区间|
|20|设计搜索与独立确认|design_search/round_five_server|D|有限候选、搜索/确认独立、冻结及禁止追加|
|21|期望事件反向校准|reverse_calibration/round_five_server|D|规划、不可达/平坦/取整、独立积分；非机制识别|
|22|IA期望事件反校准|ia_reverse_calibration/expansion_two_server|D|年龄条件、多目标局部识别和整数方案|
|23|Panel多状态模型|panel_multistate/expansion_two_server|C|常数Markov、死亡/ALIVE过滤、独立矩阵参照；预测区间未开发|

一般理论缺口继续使用[未完成范围](REMAINING_SCOPE.md)、各契约与[专项复核](NEXT_REVIEW.md)。保留源码，不把近似工作模型改称通用正式方法。

## 4. 首次基线发现的阻塞（历史记录）

|优先级|位置|现象|后果/下一步|
|---|---|---|---|
|P0|`R/study_ui.R:135`|unexpected ')'|app.R显式加载被阻断；先修括号并检查UI结构|
|P0|`R/research_workbench_ui.R:63`|unexpected ')'|条件批量UI不可加载；检查导出面板层级|
|P0|`R/round_five_server.R:13`|unexpected ';'|反向校准等server不可解析；检查参数标签及观察器|
|P0|main与扩展分支|各有9个独有提交|逐文件核对核心修复，不整分支覆盖|
|P0|扩展CI|继承工作流仅workflow_dispatch|需自动解析/两个入口加载，核心CI不覆盖全部扩展|

以上源码问题已在M0修复；补齐je_compare参数帮助、DCO单位标签，并接入自动检查。原表保留首次基线语境，当前验收见[M0报告](V2_0_M0_VALIDATION.md)。修复语法不能代替方法验证。

## 5. 证据范围

本次执行：全体R解析、核心51项、展示13项。历史证据：`validation/core_*.json`、`docs/CORE_VALIDATION.md`、原开发聊天及本机学习资料。未执行：完整入口成功加载、高级方法专项、浏览器/下载重放、Docker或目标环境部署。

记录见[基线JSON](../validation/v2_baseline_2026-10-10.json)，安排见[工作清单](V2_0_WORK_PLAN.md)，总计划见[2.0计划](V2_0_DEVELOPMENT_PLAN.md)。

## 6. M0推进结果

134个R文件全部解析、9项入口加载/注册及12项浏览器初始化/页面/单位检查通过；核心51项及展示13项再次通过。两个入口已恢复构造，七个受影响任务页面实测。远程Linux加载/回归及Windows启动准备均通过；未开展高级方法专项、Docker构建或部署。见[M0报告](V2_0_M0_VALIDATION.md)。

## 7. V2-101推进结果

核心版本2.0.0-dev已接通CSV、XPT v5/v8和SAS7BDAT，可选haven依赖、日期/原编码/筛选校验与冻结配置均落地。137个R文件及9项加载通过，核心64项回归通过；实际下载重放修复了空组别字段的JSON类型。未扩展核心统计模型或发布包。下一项V2-102。
