# v0.32观察/区间/柔性/盲态输入契约（未验证）

## 区间单终点快照

USUBJID,PARAMCD,ENTRY,L,R,TYPE,EXIT；按组拟合另需TRTP。单次选择PFS/OS，一人一行；exact上下界相等且正、left为0至正R、interval严格L<R、right为有限非负L至空/Inf。事件前三类EXIT=event，右删失active/dropout。事件R或末次确认L不晚于IA；TRTP在合并/盲态模式中删除，不作为拟合输入。文件时间单位独立选择。

精确ADTTE连续日ENGINE_ENTRY_DAY/ENGINE_TIME_DAY；区间ADTTE另带ENGINE_L_DAY/ENGINE_R_DAY/CNSTYPE。区间导出AVAL保存检测上界或末次确认的日历时长，首日+1；不能把区间AVAL当成确切事件。保留区间元数据导入柔性入口，不将其直接送入旧精确事件拟合入口。

模型假设：观察/退出可忽略或仅作为明确工作模型。中央迟报/状态依赖退出的字段合法不等于可忽略。尚未提供隐状态多状态panel似然、未知退出风险拟合或迟报选择似然修正。

## 已指定PH盲态混合

分配比例pi与外部HR固定，Control基准可指数/Weibull/广义Gamma/log风险样条。实际TRTP覆盖成Combined，其他未要求字段丢弃。数据拟合只能拟合基准参数，不能估计真实治疗效应。幸存者组成按生存概率条件更新；参考列treatment_probability_given_survival是模型概率，不是实际患者揭盲标签。

拟合/参考不需要未来B和过程。预测strict要求active确认到IA；complete明确按模型补全间隙，输出已知/估计未确认/未来新事件，不能称作中央已报告事件。目标是累计临床模型事件目标，不是发布目标。

## 观察研究六列情景

label,n,visit_scale,drop0_scale,drop1_scale,lag_scale。真实PFS/OS三转移相同患者，访视漏访/抖动独立，退出依赖已检测进展，不依赖隐匿真实状态。OS临床连续随访，PFS按评估界定区间。clinical_day不晚于DCO且report_day不晚于DCO才进入中央层；临床层可以知道待报事件，但导出不暴露未来实际发布时间。

仅完整临床实时OS层或零延迟/零批次中央层支持已知机制IPCW固定时点加权均值；不是加权KM/Cox，不纠正未知报告缺失。主可估计比例不是功效，生存率偏差/RMSE真值只在评价层。

## 已知临床积压五列

USUBJID,PARAMCD,CLINICAL_DAY,STATE,SENT_DAY。每人每终点唯一；STATE reported/unsent/sent；临床日≤IA，reported/sent需已知送出日临床日至IA之间，unsent留空。sent仅固定正批次且IA时还未发布。没有输入未来实际报告时点。剩余送出条件于L>已等待年龄，sent按已知送出计算未来批次；零延迟不允许unsent。

只预测已知临床清单，不识别未知积压，不自动拼接未来未知生存事件。不含裁定修订、同患者重复事件、回溯反转或报告容量模型。

## 保存与导出

后台家族flexible_interval_prediction及observation_process共用本机库，配置含资料，无损typed JSON；同引擎/依赖/配置续跑，追加仅增B。拟合-only配置/结果/报告/R脚本在结果页独立导出。候选失败、Bootstrap失败、未运行轮次不补零；参考及分位数标明有效分母。held-out记录按完整患者分折，实际数值/往返/脚本重放尚未执行。

## v0.34增补

本文件保留v0.32交付的观察/资料契约。此前“RP/I-spline未实现”的状态由v0.34限定实现更新：柔性入口已写入全域单调RP log累计风险及正系数I-spline+正线性风险，详见EXPANSION_V034_CONTRACT和手册57章。自由盲态HR、完整后验BMA、隐藏状态或迟报选择联合拟合等仍未接入；所有新增实现尚未验证。
