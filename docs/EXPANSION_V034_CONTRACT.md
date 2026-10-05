# v0.34未完成清单扩展契约

2026-10-05。代码和内容交付，全部待验证。沿用21入口；不新增公网服务、预览进程或远程同步。

## 多臂与独立阶段

- `test=cox_wald`：共同年龄基准PH Cox同时拟合Control及全部在场臂，Efron并列、模型协方差；`test=logrank/landmark_exact`沿用原分支。
- `multiplicity=dunnett`：联合Cox单次Final的单步最大正态尾；不用于选择两阶段。
- `multiplicity=closed_dunnett`：原始所有非空交集的局部最大正态尾，独立阶段交集p组合或Stage2局部p，逐臂max交集调整p。
- Dunnett分支均为大样本近似，不是原始连续正态均值t检验，不具有已验证的有限样本保证。PH、局部渐近/条件有效性、事件停时稳定信息量及估计协方差需要后续复核与模拟。
- 联合Cox警告/非收敛/非法协方差整轮记失败；信息不足预定p=1。奇异相关矩阵不修复；`dunnett_steps`整数32–512，默认128。
- 原Bonferroni/Holm及精确完整固定年龄Fisher分支不改家族。未选择臂不能声明，2-in-1的PFS只筛选，Stage1确认仍OS。

## 事件目标

`cut_rule=fixed/target`，默认fixed。target的`event_d1/event_d2`为1–100000整数。一次Final忽略D2；独立两阶段Stage1计筛选终点，Stage2计新患者的确认终点。日历Stage1/Final参数在target模式为上限，包含研究起点之前没有患者的条件。

实际Stage1截点达到D1即启动新队列；上限未达或筛选空则不启动Stage2。Stage2仅新队列Control+选择臂，D2不累计Stage1后续事件。每个阶段上限未达时正式p=1；同一DCO时刻所有事件纳入，实际计数可超过D*。事件模式不支持完整landmark精确分支。

搜索页面CSV原七列外必需event_d1,event_d2；typed JSON使用`stage1_day,max_day,landmark_age`连续日列。 fixed模式页面把事件目标规范化为0；Final把D2规范化为1。候选、搜索资格、选择hash、独立确认和不能追加B等仍沿用v0.33。成本为预定计划N+日历上限成本，不按结果选平均实际停止成本。

## 区间分支

仅联合Cox可选`interval_enabled`，默认FALSE；`interval_alpha`0.0001–0.2，默认0.05。单阶段为Bonferroni Cox Wald，两阶段组合为固定w的组合Wald参数反演，原家族m不变。未选择或无信息臂全空间(0,Inf)。点估计不宣称无偏；`compatible_with_closed_rejection=FALSE`。闭合拒绝相容的区间和选择后无偏估计未接入。

`draw_rows.record_kind`分别为arm_claim、closed_intersection、joint_covariance。逐臂声明汇总只读取arm_claim；交集、协方差、逐臂覆盖/同轮全部原臂覆盖另表导出。缺失/失败仍按全部请求B记录未知界。

## RP/I-spline模型

柔性入口新增`rp_hazard,ispline_hazard`，参数模式或实际精确/左/区间/右资料，合并/已知组别或固定HR盲态拟合及未来预测。格式沿用OBSERVATION_FLEXIBLE_CONTRACT，风险年龄左截断及访视/迟报选择联合似然未接入。

RP：0–6内部风险年龄切点、正下边界`rp_lower`、大于切点的上边界`tail`及正参考时间`time_scale`。参数顺序为截距、log年龄线性项、每内部切点一个非线性项，长度K+2。自然限制三次基额外除以上下log边界跨度平方；外部软件系数不能直接复制。全域导数严格正；拟合采用线性约束/切平面并在各片段顶点与尾部判定可行性，不接受只在网格单调的解。

I-spline：原年龄阶4的B/M基和积分I，共K+4正权重；另有在所有年龄生效的正线性风险率。内部对象params为log(lambda)、log(alpha_1)...，长度K+5；页面输入为原始正值。边界之后I常数、风险为lambda；不产生隐含治愈质量。分片四点Gauss-Legendre积分，拟合正参数变换和有限盒，不做惩罚/零系数/自动结点。

时间转换同步切点/边界/参考尺度；RP系数与I权重无量纲不变，I的lambda按风险率反向变换。单模型/AIC/K折stacking及患者Bootstrap沿用重拟合路径；固定外部盲态HR不自由估计，完整后验BMA仍未完成。

## 交付状态

引擎/worker/replay源码清单和hash包含flexible_splines、multiarm_multiplicity；后台依赖记录Dunnett的mvtnorm版本。同版本同源码/依赖才可续跑，旧结果可查看但不混入新版轮次。

手册55–58章详细说明推导/参数/操作/输出和限制。仅离线内容构建与源码打包，不运行新增R解析/加载、模型拟合/模拟、方法/数值/结果检查、浏览器/公式显示QA或实际下载重放。NEXT_REVIEW保留全部待验证项；REMAINING_SCOPE保留仍未开发的一般扩展。
