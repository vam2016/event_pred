# 第五轮数据、设计与输出契约

## 多臂家族

- `multiarm_independent_design`，页面prefix ma_；设计final/selected/two_in_one。
- 组表严格 `arm,weight,dropout_rate`，Control首行及1–5试验臂；原表固定家族，内部退出率每日。
- 单终点情景 `label,hr_<arm>`；2-in-1另加 `prog_<arm>`；HR/prog0.05–5，每组计划至少2，情景最多24。
- N1/N2为各阶段总计划N，20–3000；每情景B20–3000。总体入组Poisson率正，各阶段重新开始；共同固定日历截点≤3650日。
- 2-in-1进展/状态无关死亡指数；PFS=min(进展,死亡)，OS=死亡。死亡倍数定义OS真值，无PFS确认性声明。
- logrank正常数值为正态近似；landmark_exact为完整固定年龄生存Fisher单侧，退出必须0，未完成全部计划年龄随访整项p=1。
- selected/two_in_one根据预定筛选p阈值、最多k、p/臂名排序选臂；新Stage2患者和新Control，不共享Stage1后续事件。权重固定，未选最终p1；组合任一阶段p=1则最终1。
- 最初家族Bonferroni/Holm单侧alpha；主成功有效臂声明，FWER零效应/有害臂声明，部分零效应同时评价。
- `rows`逐轮主指标，`method_rows`资源/停止，`looks`阶段p/筛选p，`draw_rows`逐臂原/组合/调整p与选择/声明，`arm_overview`逐臂全请求概率。
- 样例观察与潜在真值分开；COHORT1/2；2-in-1样例可同时包含Stage1 PFS和OS及Stage2 OS，PARAMCD和连续日ENGINE字段保留。

## 搜索家族

- `finite_design_search`，页面prefix ds_，嵌套冻结多臂base；仅覆盖该家族，不自动搜索全部旧入口。
- 页面候选CSV七列 `label,n1,n2,stage1,max,landmark,w1`，时间当前显示单位；配置规范为连续日字段stage1_day/max_day/landmark_age。
- 不活动候选字段规范化；规范后重复拒绝。候选1–20、真值≤12；含全HR=1及有效臂备择；每任务B20–2000，最多10万任务，预计患者轮次≤1000万。
- 搜索固定候选×真值，确认独立任务键仅生成冻结选择；未选确认structural_skip不是模拟失败。任务完成数包括结构跳过，需同时查看generated。
- 功效未知作失败/FWER未知作成功；同时Clopper-Pearson单侧界，搜索尾(1-confidence)/(2*C*S)，确认尾(1-confidence)/(2*S)。
- 全部搜索处理后最小预定N+时间成本、再N/ID；没有合格候选不强选；确认不能回看改选择。
- `candidate_overview,selection_record,overview,confirmation_overview`分别保存资格/冻结hash/搜索/确认，完整行供重放。可取消/同版本续跑；禁追加B。

## 反向校准

- 独立 `event_pred.reverse_config.v1`；不是批量家族。只反求q/N/DCO一个变量，或潜在生存率反求q。
- 初始N0风险年龄0；有限未来N均匀入组0–A；独立指数退出。期望临床事件，不处理已知IA存活队列、报告积压或检验功效。
- 预定界支持目标才求根；不放宽界；未来人数返回连续解、ceil及重算期望。生存率需求只读取模型/年龄/目标/界。
- 输出overview/trace/curve/config/solved、源码/依赖/版本；CSV连续日、JSON冻结参数、RDS/报告/复现脚本。残差不是验证通过。

所有新代码/界面/公式和真正导出均未验证。详见手册50–54章及NEXT_REVIEW。

## v0.34增补

本文件保留v0.33交付约定。v0.34补入联合Cox协方差/Dunnett型及完整闭合交集、独立阶段D1/D2事件目标/候选搜索、可选同时Bonferroni与组合Wald近似区间；详见EXPANSION_V034_CONTRACT和手册55/56/58章。共享患者无缝2-in-1、闭合拒绝相容区间及连续平台仍未完成。新Dunnett分支为渐近生存近似，未验证。
