## 输入方式

选择“参数输入”或“ADTTE 数据”。界面时间单位可选日/周/月，默认月，内部计算单位为日；研究起点日期用于转换日历日期。

### 参数输入

不需要文件。指定事件分布参数、目标事件数、未来入组人数与率、脱落风险和预测窗口。

- 启动前：截点、当前仍随访人数和当前已发生事件数均为 0。
- 进行中：输入当前仍随访人数、已发生事件数和随访年龄。年龄可设为相同值，或从 0 至最大年龄的均匀分布生成。
- 当前队列年龄分布由输入假设确定，不由汇总事件数反推。已有事件数固定；历史具体日期未提供，已达标时不输出推测的历史达标日期。
- 参数模式不进行拟合、Bootstrap 或数据后验更新。治愈比例、混合成分等为输入参数。

指数模型可以输入中位时间或风险率，Weibull可以输入中位时间或尺度eta；对数时间模型可以输入中位时间或位置mu。治愈/混合模型的中位时间输入指分布成分；结果另列总体中位时间。脱落可以输入风险率或指定窗口内概率，后者对应独立脱落时钟。

### ADTTE 数据

支持 CSV、XPT、SAS7BDAT。CSV 变量名区分大小写，使用下列大写名称。先选择一个 `PARAMCD`；存在多条分析记录时用分析标志（如 `ANL01FL=Y`）筛选，筛选后每个 `USUBJID` 只能有一行。

|变量|要求|用途|
|---|---|---|
|`USUBJID`|必填；非空字符|受试者标识|
|`PARAMCD`|必填；字符|选择 OS、PFS 等分析终点|
|`STARTDT`|必填；日期|风险时间起点，可以是随机化日等定义的分析起点|
|`ADT`|必填；日期|事件或末次确认无事件日期|
|`AVAL`|必填；正数、单位按文件 AVAL 选择|核对 `ADT - STARTDT + 日期加项`|
|`CNSR`|必填；非负整数|0 为事件；正数为删失|
|`AVALU`|可选|存在时必须与文件单位选择一致（日/周/月）|
|`EVNTDESC`|可选|显示事件/删失描述，不用于自动分类|
|`ANLxxFL` 等标志|可选|筛选分析记录|
|`TRTP` / `TRTA` 或明确组别变量|合并模式可选；分组模式必须明确选择一列|按所选列独立建模；筛选后不得缺失|

界面时间单位和文件 AVAL 单位独立设置。文件单位支持日（DAY/DAYS/D）、周（WEEK/WEEKS/W）、月（MONTH/MONTHS/M）及对应中文。1周=7日；1月=365.25/12日。AVAL换算为日后核对日期；日期加项仍为0或1日。

日期加项由用户选择 0 或 1。字符日期可用 `YYYY-MM-DD`；任意格式中未标记的数值日期需明确选择 SAS 数值日期（自 1960-01-01 起的整数天数）。XPT/SAS 的日期类型由 haven 读取；不会将数值自动当作 Excel 日期。日期字段不接受缺失、分数日或非UTC零点的时间，不自动截去时分秒。

2.0核心入口通过可选haven读取XPT v5/v8和SAS7BDAT；默认核心安装不要求haven，需读取SAS时运行`Rscript scripts/install_core.R --with-sas`。所有格式变量名区分大小写且必须唯一，SAS变量/值标签保留原编码，不转成标签字符串；不导入单独SAS7BCAT。配置冻结`adtte_source`的格式/读取器版本及`adtte_mapping`的分析映射，重放仍需同一原文件。读取失败显示格式与原因；原文件和路径不写入配置或源码仓库。

日期与换算后的 AVAL 必须一致（容差0.00001日）。例如包含首日的月单位AVAL为 `(ADT - STARTDT + 1) / 30.4375`。内部拟合时间为实际经过日数 `ADT - STARTDT`；事件日仍使用实际 ADT。连续时间拟合要求 `ADT > STARTDT`，同日起止记录暂不处理。研究起点不能晚于任何 STARTDT；所有 ADT 不能晚于截点。

### 删失与未观察区间

`CNSR` 正数本身不说明是否永久退出目标终点随访。必须输入永久退出随访的编码；未列出的正数编码视为仍在随访。编码规则由本次终点定义决定。停止用药若仍继续 OS 随访，不应映射为永久退出 OS 随访。

- 永久退出：作为右删失参与拟合，不再贡献新增可观察事件。
- 仍随访且 ADT=截点：从截点的已知无事件年龄开始预测。
- 仍随访且 ADT<截点：选择“要求确认至截点”时拒绝；选择“从末次确认日补全”时从 ADT 的年龄条件生存抽样，允许 ADT 到截点之间已有未观察事件。截点事件数因此可以大于已记录事件数。

补全采用连续事件与独立脱落模型，不含 PFS 访视间隔、区间删失或评估判定过程。上报延迟作用于新增模拟事件，不能恢复历史报告状态。

### 范围与模板

数据拟合要求 3–5,000 名受试者、至少 2 个已记录事件。数据不足时可使用参数模式。支持合并或已知组别独立建模，均无组内协变量模型。分组拟合每组至少3人和2个事件。上传文件只在本机 Shiny 会话中读取，不写入源码仓库。

模板为合成 OS 数据，`CNSR=1` 表示截点删失，`CNSR=2` 表示永久退出随访；此编码不是跨研究通用的 CDISC 删失原因编码。

变量语义参考 [CDISC 2024 ADaM 说明，ADTTE](https://www.cdisc.org/sites/default/files/2024-04/2024_CDISC-Session%207B%20ADaM_Mitchikou%20Tseng.pdf)。本页规定的是平台输入接口，不是完整 CDISC 合规验证。

## 历史回测的附加输入

每个回测截点需提供当时计划剩余入组人数及恒定入组率，按截点顺序填写；人数为 0–1000 整数，率为当前所选单位下的非负数。过程 Gamma 模式从早期截点数据更新率，计划率仅作记录。当前结构没有历史报告日期，因此仅允许实际发生口径回测，不能评价上报口径。单个最终 ADTTE 文件不能恢复真实历史访视和报告版本。

## 已知组别模式

参数模式支持2–6组，每组输入组名、Nactive、D0、当前年龄、分布及参数、入组计划、脱落率、q和L。ADTTE模式按选择的TRTP、TRTA或其他组别列确定2–6个分析组；筛选后每人唯一且组别不得缺失，全部组须有计划。计划与实际治疗不自动互换，不按名字推断治疗效应。各组当前人数合计≤5000，未来人数上限合计≤1000。D*为全部组事件数之和。

分组历史计划在内部接口中为group、cut、future_n、enroll_rate；每个截点必须覆盖全部组。组名按最近成功预测的配置匹配。cut内部为日，enroll_rate内部为人/日；界面按所选日/周/月换算。

### 扩展模型拟合（v0.6）

Gompertz与Cure-Weibull可在事件模型页选作候选模型，或在分组页为本组指定。仍只接受右删失结构，至少3人、2个事件；这只是入口阈值，不保证治愈尾部可识别。拟合使用有限正观察时间、独立删失假设；不处理左截断或区间删失。平台以数值收敛、梯度、曲率及治愈比例边界检查决定是否保留模型，失败原因会显示，不能将失败作为零事件预测。两种模型支持固定参数及受试者Bootstrap，未增加专用贝叶斯后验。

## IA→Final条件模拟入口

文件采用同一ADTTE字段与日期约定，明确所选PARAMCD、分析标志和已知组别变量。仍随访者必须确认至当前IA DCO；本入口不模拟补全IA之前的缺口。内部时间按ADT-STARTDT，不含首日偏移；首日偏移仅用于核对AVAL。

也可选择设计模拟的一轮/一个截点的连续观察记录，保留连续时间且不读取未观察真值。合成示例固定240人、IA研究时间540日、OS，仅用于试用。汇总人数不构成IA个体历史，不能用于完整KM续推。

Final连续CSV含典型观察字段、模拟序号、截点、统一q情景；不含未观察事件时间。ADTTE同日记录保留并提示不能进入当前文件预测接口。下载的配置JSON及复现R脚本包含IA个体数据，以便复现原风险集。

## 重复试验与设计评价

无需数据文件。给定Control分布及恒定HR、N候选值、简单随机Treatment分配概率、总体入组、两组独立脱落、固定DCO或D*及未达标决策、主检验方向/alpha和重复数。Ⅰ类错误入口固定HR=1。时间选择日/周/月，配置内部参数统一为日。

每轮结果用SCENARIO/BASEID/SIMID/replicate_seed追踪；只有同N/HR不同截点配对。摘要与检验CSV的`*_day`和DCO_DAY为日。取消时保留已完成轮次、原计划数及cancelled状态，未完成轮次不补成不拒绝。

选定轮次连续观察数据、ADTTE及真值可分别导出。观察层不包含未观察事件/脱落时间；真值包含所有计划患者，另行下载。SCENARIO区分情景，SIMID区分重复；样例ADTTE的CUTID与情景编号相同。跨情景/轮次USUBJID可能重复，不直接将合并文件作为单次预测输入。日期向下取整、AVAL含首日，同日起止记录保留。

后台仅在该本机会话临时目录保存配置/分批摘要，结束会话后清理；正式保存使用下载。配置JSON、R脚本与Markdown记录版本、依赖、运行规则及完整/部分状态。R脚本按确定种子重放所需轮次前缀，保留原情景顺序和计划数。


## 历史多次IA入口（v0.18）

ADTTE长表增加平台扩展字段`IASEQ`（1–5原分析整数次序）与`DCO`（该次唯一截点日期），并提供已知两组变量。选择终点及分析标志后，每个IA内`USUBJID`唯一；从分析1至所选M的快照完整且连续，DCO严格递增。每位旧患者的组别、入组日不变，已事件/永久退出者持续保留且状态和日期不回改；新出现者需在前次DCO后入组。仍随访者确认到本次DCO，经过时间需正值。

只读取所选前缀临床内容，不用最新快照倒推历史；后续IASEQ元数据仍须合法。每次累计事件数必须恰好等于原计划ceil(D*×比例)，不接受并列超目标、原时点变更或停止后继续分析。文件AVAL单位和平台日/周/月显示单位分别设置，内部经过时间为日且不含首日偏移。

JSON及R脚本保留所选全部规范化患者前缀、原计划和当前模型配置；逐次CSV以`phase`区分`observed_history`和`simulated_future`。历史CSV中的DCO_DAY为日。合成模板为`examples/adtte_ia_history.csv`，不包含真实研究患者资料。推导、操作和限制见工作手册第25章。

## v0.19 组序贯停止后的推断记录

`sequential_final.csv`以保存的原设计和选定有效观察前缀为计算对象。支持不设无效停止的双侧，以及单侧获益原非约束性Z无效规则实际执行的终止记录。继续中的分析保留`available=FALSE`和未知调整结果；不得以0替代。

- `adjusted_p`、`median_unbiased_hr`、`adjusted_hr_lower/upper`分别为阶段排序p、中位无偏HR近似及双侧调整区间；HR为Treatment/Control。
- `upper_tail_null/lower_tail_null`为零假设获益方向与反方向尾概率，合计为1；双侧p为两倍较小尾概率，单侧p为上尾。
- `p_agrees_original_decision`为按保存alpha比较调整p后，是否与原拒绝决策一致。原`action`仍是原方案的决策；该字段不修改决策。
- `ordering`为有方向的counter-clockwise排序；`engine`记录原生rpact或边界概率反演。`root_probability_error`为新反演的最大尾概率残差（不超过1e−7），原生实现留空；`interval_tail_alpha`为每个置信尾部的alpha。

推断JSON保留原完整设计及观察前缀、所选次序、最小配置、平台/rpact版本；无效设计额外保留原K−1个无效界值。脚本需用同版本源码及rpact运行，不需要潜在患者真值。观察记录须连续且来自同一情景/轮次，累计事件恰等于原目标，原方向、上下界、信息比例、alpha消耗及决策逐项核对。上传历史入口当前尚未接此调整推断。


## v0.20 beta设计快照（待复核）

设计研究配置增加`gs_futility="beta"`、`gs_beta`、`gs_beta_spending`、`gs_design_hr`；HSD使用`gs_beta_gamma`。界面输入方式记为`gs_beta_input`，计算统一保存beta。真实模拟HR、CP未来HR和参照设计HR分别保留。

beta计划额外保存`binding_futility`、`beta_target`、`beta_spending`、`beta_spent`、`design_hr`、`standardized_final_drift`、`reference_required_information`、`reference_required_events`、`reference_events_ceiling`和`implied_design_hr`。逐次分析CSV增加约束性标志和当前累计beta消耗；Final无效界为负无穷，Final不拒绝单独记录，最终beta包含该概率。

推断JSON增加`repeated_available`、`repeated_note`和`review_status`。beta分支重复p/区间CSV保留未知值；终止阶段排序接原边界积分，新增分支标为`pending_v0.20_review`。保存beta计划恢复运行时按配置重建原界及参照字段，不允许更改边界后继续声称原设计。

本轮没有运行新分支检查、数值/覆盖验证或下载复现。上传实际IA及历史多次IA暂拒绝beta模式，不默认回退到非约束性Z。


## v0.21 独立历史IA调整推断接口（待复核）

第七需求入口上传含`USUBJID, PARAMCD, STARTDT, ADT, AVAL, CNSR, IASEQ, DCO`及指定组别变量的历史长表。IASEQ/DCO为平台扩展字段，含义与v0.18历史续推接口相同。每次完整快照、患者跨快照规则及严格确认到DCO保持沿用；所选前缀之外仅IASEQ/终点/标志等选择所需结构信息可用于建立选择项，之后日期/组别/结局均排除。没有未来模型、潜在事件时间或重复模拟数据。

调用`run_history_inference(snapshots,cfg,metadata)`，输入规范化快照列表及原设计配置`target,timing,sided,alpha,spending,futility,futility_z,treatment_fraction,control,display_unit`。每个快照为`list(look,cut,data)`，个体data列为`id,entry,time,obs_day,status,event,group`；时间均为日。原方案2–5次，选定历史前缀1–5次且不得超过原K。每次历史事件数恰为ceil(D*×原比例)；超目标/计划修订拒绝计算。

返回`inference,path,diagnostics,plan,config,snapshots,metadata,control,treatment,created_at,version,status,source,information_assumption`。`source="observed_history"`、`status="pending_review"`；共同引擎内部兼容SCENARIO/SIMID只取1，临床CSV/JSON展示数据移除这两项，不能据此将实际患者分析称为模拟结果。`inference$review_status="pending_v0.21_clinical_adapter_review"`。

完整下载schema为`event_pred.history_inference.v1`。JSON/RDS只含所选规范化前缀与原设计/结果，不含原上传文件或未来患者结局。JSON null可同时表示无穷边界或未知数值，尚不支持通用JSON自动恢复；RDS保留精确R类型和无穷值，R脚本dput保留数值精度。患者标识仍存在于快照、RDS/JSON及脚本；不进行自动匿名化。脚本从观察快照重算各次Z后调用共同推断引擎，并比对保存计划；复现尚未验证。

历史v0.17–v0.19所述“上传历史尚未接入调整推断”为当时范围；v0.21独立入口新增该适配。约束性beta、重估设计、历史迟报/裁定修订及计划变更仍未接入。只更新代码/内容，检查见NEXT_REVIEW.md。


## v0.22 PFS/OS联合生存生成接口（待复核）

配置schema为`event_pred.joint_survival.v1`。`run_joint_simulation(cfg)`接收参数化设计，不接受真实多状态记录。配置含`transitions`的01/02/12模型及各自活动输入基准、groups的name/weight/dropout_rate/命名multipliers、`post_clock=reset|forward`、两组的treatment_fraction及入组/共同截点、固定随访时点、N/reps/seed、研究起点和显示单位。引擎固定为日；rng_kind固定Mersenne-Twister/Inversion/Rejection，调用后恢复原算法及状态。

返回`config,trial_status,truth,observed,summary,fixed,curves,states,intervals,cuts,created_at,version,status,schema`，`status=pending_review`。每轮要么完成整个生成及共同截点描述，要么保留带原SIMID/派生种子的失败状态；汇总分母明确，失败不是0事件。真值计划患者包含未入组者；观察资料只包含对应DCO前有限入组者。

- `truth`独立导出潜在01/02时钟、实际progression/death、post_progression_duration、pfs/os/dropout年龄及研究日，未发生或永不入组可为Inf。进展路径原直接死亡时钟取消，不能从latent_direct_death_clock_day读取实际死亡日期。
- `observed`为两终点长表，`PARAMCD=PFS|OS`，共享SIMID/CUTID/DCO_DAY/USUBJID/group/entry_day；`time_day,obs_day,event,status,event_cause`只包含截至DCO且退出前已观察的终点记录。PFS原因progression/death，OS原因为death，删失为censored。
- `states`给最后观察研究日、最后已知状态0/1/2及DCO状态；退出后的state_at_dco为unknown_after_dropout，不能拿潜在真实状态补齐。
- `intervals`给from_state/to_state、start/stop个体年龄及研究日、event、transition和transition_clock_start/stop。行政/退出删失to_state为空；1→2时钟依reset/forward给坐标，最多两行/患者/截点。
- `cuts`给同一轮共同DCO下PFS与OS事件数及预定触发终点、D*、达标/窗口状态。固定DCO无事件目标，因此target相关字段为空；事件驱动只读取所选触发终点。
- `summary/fixed/curves`带PARAMCD，单终点KM描述；NR或超过观察支持保留未知数值。单轮KM区间与重复试验条件中位数分位数分别解释。

连续CSV均以日导出；ADTTE按研究日期向下取整、AVAL含首日，AVALU随提交显示单位；每患者每截点两行，EVNTDESC保留PROGRESSION/DEATH或退出/截点。同日起止不移动或删除，当前预测入口不接受这些零时间记录。完整RDS包含真值，配置JSON不含患者数据；复现R脚本保存精确配置并从起点重新生成，要求对应源码及survival版本，尚未实际重放。

首批不提供实际IA状态条件预测、联合功效/多重性、转移模型拟合或确认性推断。新增模块、观察/真值隔离、全部公式及导出未验证，见NEXT_REVIEW.md。

## v0.23 设计情景表与导出快照（待复核）

批量入口无需患者数据。可直接输入列表，或上传仅含`label,n,hr,time_scale,enroll_scale,dropout_scale,cut_value`的CSV；名称不作模糊匹配，非label列必须数值，重复参数行拒绝。CSV时间单位单独声明；目标事件整数不转换。Control和其他共同配置由界面提供，不从CSV隐藏读取。

轮次CSV保存SCENARIO/SIMID、派生种子、真实抽样HR/时间倍数、截点与正式决策标志、分析/生成失败原因；汇总保留全部计划情景，未运行轮次仍计入未知。概率有效分母V与请求分母B分别报告。资源均值与Cox覆盖仅在对应有效子集内计算。

RDS保存冻结配置及轮次统计，不存每轮全部患者真值。所选已生成轮次的观察CSV/ADTTE与潜在真值分别用原配置/种子重新构建；观察文件不添加潜在未来结局列。ADTTE沿用单终点USUBJID/PARAMCD/TRTP/STARTDT/ADT/AVAL/CNSR和日期取整规则。重建与实际导出均未验证。

配置JSON供记录，尚无通用自动导入恢复。复现R脚本嵌入原配置和已处理的SCENARIO/SIMID键，保存取消快照后只重放这些键，其余轮次仍未知。当前不提供跨会话任务持久化；关闭会话前自行下载快照。


## v0.24 方法记录与NPH情景（待复核）

NPH情景表需严格八列：原七列加profile，profile匹配本次命名定义，hr为所有段共同乘的幅度。命名定义由界面文本提供并规范化为日切点与HR列表，切点不随Control时间倍数缩放。PH仍七列。patient输入仍不需要。

批量schema升级event_pred.batch_research.v2，RDS新增method_rows/method_overview/method_pairs。每方法记录原SCENARIO/SIMID和同一派生种子、主分析标志、有效/执行/决策状态、p/Z/统一获益Z、估计/SE/普通95%区间/逐轮真值/零假设状态和失败原因。Cox估计单位logHR，RMST日，生存率差概率；score方法估计字段NA。NPH Cox真值、偏倚、覆盖保持NA。

方法统计区分请求/决策有效/估计有效/真值有效/覆盖有效分母；配对表仅两种决策均已知的轮次计算配对均值与SD/根号V，全部请求差值界使用每个方法已知/未知状态。未达完整规则且关闭不拒绝为已知0，不受名义描述p改变。所有方法共用同一观察数据和DCO，不混用独立情景比较的方差。

实际CSV/RDS/JSON/报告/复现脚本和所选患者重建均未验证；新增方法记录不是实际已运行研究证据。


## v0.25 组序贯快照schema v3（待复核）

PH七列情景的cut_value为原Final D*。配置新增design_mode及按D*命名的batch_gs_plans；原计划包含取整目标/信息、效力/无效界及alpha/beta消耗。RDS/R脚本保留无限边界；JSON null不能解释成0，暂无自动JSON恢复。

快照新增looks、gs_plans、stops、resources。逐分析仅保留实际计算前缀，停止后无未来主分析；窗口未达下一目标不补检验。组序贯方法整体p_value为NA，stage_nominal_p仅阶段描述；固定参照method=fixed_final，p为预定一次检验值。两策略共用同轮潜在轨迹，观察CSV/ADTTE仍只为主GS停止截点。

CP CSV/JSON独立保存所选IA的观察Z/信息/日、用户未来HR/无效界假设和原配置/版本/依赖。不读取未来患者结局；当前单选继续IA。路径/资源导出及部分键重放尚未执行；本机预览未更新。

## v0.26 批量研究库、配置JSON和汇总

批量结果schema为`event_pred.batch_research.v4`：保留config/scenarios/rows/method_rows/looks及汇总，新增run_id/run_title/parent_id/operation/configuration_hash/source_hash；continuation_operation、initial_completed、resumed_from_run_id记录执行来源。主轮次键SCENARIO/SIMID唯一，包含失败已处理记录；方法表的同键/method唯一，续跑不得删除或替换失败。统计量时间字段仍为日。研究库首次初始快照尚无处理轮次。

配置JSON schema `event_pred.batch_config.v1`含version、configuration_hash、summary、exact_config。exact_config递归节点type为null/list/data.frame/double/integer/logical/character，names可空；data.frame另有row_names；double数据为小端IEEE-754十六进制并声明length，其他原子量为列表（null表示对应类型的缺失值）。JSON自身不含可执行R表达式。最大文件5MB；旧版readable-config导出不属于本导入契约。摘要用于一致性标识，不是密码学签名。

本机目录`data/private/batch-runs/<32位十六进制工作区>/<研究ID>`包含config.rds、meta.rds、checkpoints及运行时lease。RDS只供平台自建目录内部读取，未提供任意RDS上传。快照先写后发布指针，保留最新三修订；正常关闭保存最近发布记录，强制中断可能丢失末次持久快照之后的进度。研究记录不进入源码开发包或源码SHA清单。

并列汇总包含研究元信息及各自主分析/方法/资源表，不去重合并父子轮次，不计算跨研究差值/检验。共享种子表报告已处理seed交集数量，既不证明患者配对也不证明无交集就独立。各表原始时间量为日，source_display_unit保留原显示单位。归档状态只影响列表展示。

并列汇总另可导出`event_pred.batch_collection_configs.v1`配置集合，其中configurations按研究ID命名，每项为单研究无损配置文档；集合不能作为单研究配置文件直接导入。

## v0.27 联合终点研究与参考分布

第十入口无个体资料上传要求。情景表严格基础七列`label,n,q01,q02,q12,enroll_scale,dropout_scale`，固定DCO加`dco`，PFS/OS目标分支分别加`target_pfs`/`target_os`，both/first加两者。最多60条，不允许完全相同参数重复；固定DCO的文件单位独立声明。列表输入效应定义采用名称和三转移q。

配置使用`event_pred.batch_config.v1`类型树，`research_family=joint_endpoints`，包含joint_base、analyses、原声明规则/附加规则、成功指标、所需权重/顺序和情景。joint_base仅为转移/过程模型的单轮参数原型，其N=10、reps=1及占位固定截点不参与正式研究；各情景的N/共同截点由联合运行器决定。第八入口描述性联合模拟JSON与本格式不互换。

研究快照沿用batch_research.v4，新增policy_rows/policy_overview/policy_pairs/endpoint_correlations。主键仍SCENARIO/SIMID；终点method_rows的method/ PARAMCD为PFS或OS，analysis_method为实际方法。decision字段是原始终点名义策略，不代替声明规则。policy_rows的同键/policy唯一，保留正式检验标志、调整p及缺失p补全界、PFS/OS/any/both/success/false_claim。row$decision_*只对应预定主规则与主指标。

未知原p不直接丢弃或认证为1，声明用0/1补全得到部分确定结果；生成失败整轮未知。false_claim只使用已识别充分真零条件，其他可能真零状态未知；不得将全部情景的原始拒绝比例统称FWER。普通Cox/差值区间不调整多重性，也不以转移q作终点HR真值。

观察表/双终点ADTTE/状态/转移区间在共同Final截取，潜在真值单独下载；结果RDS不包含全部轮次患者轨迹。时间字段为日，概率无量纲，Cox估计logHR；续跑需每已处理键的两个终点及所有预定规则记录完整。

边际参考表包含SCENARIO/PARAMCD/time_day、两组生存率/差值、两组RMST/差值和note；JSON保存对应冻结配置、情景、参考显示单位、时间与版本。不含退出/入组/DCO或患者资料，不作为配置导入文件。并列研究汇总新增research_family和联合规则表，不合并单/双终点估计目标。

## v0.28 独立队列重估批量契约（待复核）

情景CSV严格13列：`label,n1,n2,d1,d_plan,d_max,hr_assumed,cp_target,cp_min,true_hr,time_scale,enroll_scale,dropout_scale`，不含SCENARIO；输入完全重复参数拒绝。所有列无时间量，窗口/率/Control模型在共同配置指定。模板为examples/adaptive_batch_scenarios.csv。列表事件计划采用名称 | 八个设计值，再组合四个情景轴；真实HR=1须显式提供。

无损JSON使用event_pred.batch_config.v1及research_family=independent_cohort_adaptation；patient_base保存共同患者参数，各计划在运行时赋予N1/N2/D1/Dplan/Dmax/HRassumed/CP。它不是实际IA数据配置，不能导入患者预测入口。method为original/bounded/promising，主方法单独预定；effect_mode=ph、sided=benefit、cut_rule=two_cohort_events。

快照沿用event_pred.batch_research.v4，row主键SCENARIO/SIMID、method_rows为同键/method唯一、looks为同键/method/stage唯一。每已处理键包含所有预定规则，生成失败也保留未知决策；成功生成轮次每规则都有阶段1路径，执行新队列者另有阶段2路径。续跑不能缺失/增加已处理方法或路径，失败不重试。

row$decision_*仅是主规则的拒绝。method_overview含reject/stage1_hit/stage2_hit/increased/cp_target_met五指标，各自已知分母；method_pairs为比较减主规则。stops分别标记action与rule_reason。resources时间为日，关闭分位包含未达标者，资源分母是生成成功轮次。policy_overview和GS计划在该家族为空，避免将逆正态组合当组序贯设计。

观察样例各规则共享患者但截点不同，按METHOD过滤；COHORT区分独立队列。队列2entry/obs/DCO转为研究日起点坐标，time_day保留风险年龄。真值另外导出，stage_entry_day/stage_event_day保留阶段本地坐标；队列2研究坐标加实际IA时间，可能包含计划中但未观察/未入组患者。

样例ADTTE包含SCENARIO/SIMID/METHOD/COHORT/USUBJID/PARAMCD/TRTP/STARTDT/ADT/AVAL/AVALU/CNSR/ANL01FL及ENGINE_ENTRY_DAY/ENGINE_OBS_DAY/ENGINE_TIME_DAY。AVAL使用取整日期差、+0日；CNSR为0事件、2永久脱落、1行政删失。先按METHOD选择，再按COHORT作阶段分析，不合并重复规则记录或忽略队列后当作本组合检验。

IA规则参考JSON保存所选情景、Z列表、冻结研究配置、表、版本及时间；不是配置导入文件。CSV不含模拟患者/真值；编辑参考输入后不改旧参考。源码包排除私有研究记录。本轮契约/重放尚未验证。

## v0.29 实际IA条件批量契约（待复核）

无损配置沿用event_pred.batch_config.v1，research_family=actual_ia_conditional、mode=conditional；branch为existing_patients或new_cohort。payload包含冻结data、engine、branch、frozen_at。data使用规范字段id,entry,time,obs_day,status,event,group；时间均研究日起点的连续日，time=obs_day−entry，不含首日加项。输入来源沿用原IA入口ADTTE/历史快照契约，历史只保留至所选IA的数据及原路径。该配置含实际患者记录，不能当无患者的设计情景文件传播。

附加情景严格列名label,control_event_q,treatment_event_q,control_enroll_q,treatment_enroll_q,control_dropout_q,treatment_dropout_q；模板examples/conditional_batch_scenarios.csv。最多59条，自动生成六倍数1的输入基准；表头可表示只有基准。无时间单位列，倍数无量纲，不改未来计划人数/原设计。新队列采用总体入组，两个入组倍数须相同；一次入组均1。

研究快照沿用event_pred.batch_research.v4并保留prepared：IA、配置、拟合/后验模型、过程后验、prepare seed和payload hash。续跑沿用已准备模型，不重新读取后来IA或抽取新的MCMC链。row主键SCENARIO/SIMID；method_rows同键/method唯一；旧患者logrank或新队列adaptive/original。replicate_seed为通用研究键种子；latent_seed=同SIMID的基准SCENARIO1种子，用于跨情景共同参数和轨迹随机数。失败仍保留键与未知决策，不自动重抽。

looks保留实际未来分析路径；draw_rows记录该轮抽取的基准模型/过程参数。overview、method_overview、method_pairs、scenario_pairs、resources、stops及ia_overview/model_overview分别导出；拒绝、目标达标和资源分母不能混用。scenario_pairs为附加情景减输入基准，同SIMID配对；未处理、失败或未知保留完整请求补全界。准备诊断不是验证结论。

样例观察与潜在真值分开导出，新队列按METHOD/COHORT选择，不把两个决策方案重复患者合并分析。真值stage_entry_day/stage_event_day保留新队列本地时间，研究坐标加真实IA时间；观察表已使用研究坐标。ADTTE为日、日期取整差+0，CNSR0事件/2永久退出/1行政删失，ENGINE_*保留连续日。配置/报告/复现脚本和本机库可能含IA记录，源码包排除data/private。

## v0.29 协变量、分层、中心与脆弱性契约（待复核）

research_family=heterogeneous_survival、mode=fixed_truth、effect_mode=conditional_ph_heterogeneous。仅设计参数或平台配置JSON；未接收实际异质患者资料拟合。层表严格stratum,weight,hazard_q,hr，模板examples/heterogeneity_strata.csv；情景CSV严格label,n,hr_scale,center_sd,subject_variance，模板examples/heterogeneity_scenarios.csv。数值均无量纲（N为人数），窗口/分布/率在共同日制配置指定。

SCENARIO/SIMID为轮次键；method_rows的method为logrank/stratified_logrank/adjusted_cox。同轮三个方法共享生成患者，跨情景不声称共同轨迹配对。普通/分层log-rank估计字段为NA；调整Coxestimate为logHR、se和普通95%区间。estimand_status区分common_conditional_logHR与working_Cox_coefficient；recovery表只用前者可识别有效估计计算偏差/RMSE/覆盖，不把潜在乘数或边际HR当作Cox真值。

观察样例除基础生存字段外含ARM_TRT、XBIN、XNORM、STRATUM、CENTER；生成真值另含U_CENTER、U_SUBJECT、conditional_hr、hazard_multiplier，不传入观察分析。draw_rows单独记录该轮中心脆弱性，不作为观察协变量。资源时间均日，固定DCO/目标窗口末关闭分位含未达标者。分层与中心为空风险集/事件不足、Cox收敛或方差无效均标记分析未知，不改为不拒绝；明确预定窗口关闭不拒绝除外。

ADTTE追加XBIN/XNORM/STRATUM/CENTER，整数日期差+0与ENGINE连续日分开。结果RDS与研究库不保存全部轮次患者，可从选定成功键显式恢复。两家族均接原后台、续跑/追加、研究库和配置集合；本轮恢复及重放契约未验证。

## v0.30 beta原设计和重复推断契约（未验证）

原IA条件/历史规范数据不变，原设计增加beta、消耗、gamma、设计HR参照及原分配概率。保存计划含binding_futility/beta_target/beta_spending/beta_spent及参照信息元数据；计算前必须与同构造器重建的原边界一致。原分析次序、事件目标及历史停止限制保持。

重复表新增interval_empty、repeated_engine、band_scale、band_full_plan_crossing、original_beta_decision、repeated_matches_original_test。binding重复方法为full_plan_shape_band_prefix_intersection；后一个标志恒FALSE，表示与原beta拒绝集合不同。区间为空时上下界NA，不交换；阶段排序停止结果另表，未来观察不进入前缀推断。原同版本脚本包含原beta配置，新增实现及重放未验证。

## v0.30 共享患者适应研究契约（未验证）

research_family=shared_patient_adaptation；mode为fixed_truth或conditional，design_mode=adaptive_e_mixture；test_basis=known_hazard或cohort_riskset；effect_mode分别known_baseline_ph/exact_entry_cohort_ph。配置沿用event_pred.batch_config.v1类型树，原检验网格/权重、区间网格/权重及模型/事件规则均冻结。条件payload保留data、cut、文件名和precision，含原患者标识与观察记录，不可当作无患者设计配置传播。

情景严格五列label,n,hr,enroll_scale,dropout_scale，模板examples/shared_adaptation_scenarios.csv。设计n总N，条件n未来新人数；原IA人数另计。仅在从起点模式，HR≥1标记benefit_composite_null；条件模式始终conditional_given_IA，不将条件拒绝率当作无条件错误率。

实际IA常规字段USUBJID/PARAMCD/STARTDT/ADT/AVAL/CNSR及组别。精确时间扩展为ENGINE_ENTRY_DAY/ENGINE_OBS_DAY/ENGINE_TIME_DAY；数值连续日，obs−entry=time，与取整STARTDT/ADT一致；AVAL按整数日期差+所选0/1首日偏移。相同日期但连续时间>0的精确记录允许日期差0。队列风险集分支必须有扩展字段且不能伪造精度；已知风险分支也优先使用，若仅日期则precision=date_times_treated_as_exact，不自动取得实际区间化观察的连续模型结论。仍随访确认至IA，历史事件/退出不回改。

快照沿用event_pred.batch_research.v4。row主键SCENARIO/SIMID；method_rows同键/method，method original/bounded/promising；looks同键/method/stage=1,2，IA为planning_IA_no_test，Final注明是否达标/正式检验。续跑要求已生成轮次每规则的两路径完整，失败不重抽；追加只增加B。方法配对复用患者，不声称跨情景配对。

结果保留IA_log_e、conditional_error_bound、cp_proxy_selected、选定事件目标、e_value/log_e/e_p，以及统计信息。known_hazard有Treatment事件计数与baseline_exposure；cohort_riskset的暴露为NA，另有informative_events。Formal拒绝只在达所选目标后按E≥1/alpha；CP代理不作为正式检验。cs_lower/cs_upper和confidence_engine是另一个参考混合的置信序列，hr_mle不是Cox系数或中位无偏估计。恢复汇总含interval_n/coverage、finite_mle_n及mean_finite_hr_mle，无穷MLE不静默算作有限估计分母。

所选样例观察按METHOD分开，潜在真值单独。cohort_riskset样例另可导出SCENARIO/SIMID/METHOD/entry_day/event_day/R_CONTROL/R_TREATMENT/X_TREATMENT，全部从该METHOD的Final观察记录构造；无双组风险的行保留但不贡献信息。精确共同入组日不做近似分组，同队列并列事件或事件/永久退出同时而缺少先后信息时拒绝该精确分支，不作Breslow替代。

当前观察推断为event_pred.shared_observed.v1，exact_result采用类型树保留Inf/NA及精确double，含当前患者记录、配置和统计/结果；不能作为批量配置JSON导入。导出CSV、完整JSON、匹配源码脚本；仅由实际已观察记录计算，不生成未来。批量JSON/RDS/报告/脚本另走研究库流程。所有新契约与实际下载/恢复/重放未验证。

## v0.35 IA期望反校准

无文件时年龄表严格AGE,N，每个唯一当前年龄及正整数人数，当前已知事件D0另给。有实际记录时专用CSV严格USUBJID,ENTRY,AGE,STATUS；时间单位独立声明、归一连续日，STATUS active/event/dropout，active确认到IA。ADTTE ENGINE_ENTRY_DAY/TIME_DAY/OBS_DAY固定连续日，单终点/CNSR0/1/2，可明确选TRTP，未来结局不得输入。现有风险集固定，不重新估计基准模型或真实HR。

联合目标严格DCO,target,weight,tolerance，自由度界parameter,lower,upper。页面DCO/退出率按显示单位，配置规范为DCO_DAY及每日报险。配置schema event_pred.ia_reverse.v1，摘要/类型树保存真实double，可能含患者资料；它不是batch_config且不提供研究库续跑。

## v0.35 panel状态记录

严格七列USUBJID,GROUP,ENTRY,AGE,STATE,KIND,EXIT；同患者多行，基线AGE0/STATE0，组/入组固定、年龄递增。中间EXITfollowup，末条active/dropout/event；STATE0/1/2/ALIVE，exact_death只为末条STATE2/EXITevent。panel死亡为区间状态概率，exact_death为密度。STATE1后不可回0，死亡后没有记录；active末条确认到IA。最多20000观察/3000患者/8组，每组至少5患者（不保证识别）。模板data/examples/panel_markov_months.csv。

参数模式提供GROUP,lambda01,lambda02,lambda12及GROUP,n0,n1,n2；不用文件。常数Markov强度、可忽略访视/退出；未知转移日期积分处理，ALIVE保持模型过滤组成，不填中点或伪精确ADTTE。IA未知进展模型期望单列。未来表GROUP,weight,dropout_rate和有限均匀入组仅用于期望计数。

配置schema event_pred.panel_model.v1，可能含患者记录。输出current_states/逐观察filter_records/三率/信息诊断/联合参考与所选需求期望表，连续日和每日报险导出；结果RDS/报告/匹配源码脚本独立保存，没有接batch研究库、患者Bootstrap、预测区间/日期分布或真实隐状态选择联合似然。全部契约与真实下载/重放尚未验证。
