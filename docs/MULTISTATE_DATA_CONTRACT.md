# v0.31精确多状态输入与导出契约（未验证）

状态CSV：USUBJID,GROUP,ENTRY,TIME,STATE,EXIT,PROG。一人一行；GROUP为Control/Treatment；ENTRY为研究入组时间，TIME为入组至末次观察时长，PROG为入组至已观察进展时长；三时间列使用独立选择的文件单位。STATE0未进展、1已进展存活、2死亡；EXIT为active/dropout/event（死亡）。STATE0的PROG空，STATE1必填，STATE2进展后死亡填PROG，否则空。active末次观察须为IA；死亡或永久退出保持。死亡/进展并列路径尚不支持。精确路径不等于访视状态或区间删失。

双终点ADTTE：USUBJID,PARAMCD,TRTP,CNSR,ENGINE_ENTRY_DAY,ENGINE_OBS_DAY,ENGINE_TIME_DAY；PARAMCD为PFS/OS，每人各一条；CNSR0事件/1仍随访/2永久退出。ENGINE连续日，始终日；不能用日期或四舍五入AVAL重建。两终点共有观察过程；未发生PFS事件时截点/退出一致；死亡必须也是PFS事件。参见手册44.1完整限制。

多状态预测：按组各转移拟合或参数指定；参数转移Treatment乘其预定q，实际拟合风险不再乘q。指数零事件保留零率，未暴露转移不可估计；PWE每段须暴露，Weibull每组每转移至少2事件。12前向时钟拟合扣H(进展年龄)。Bootstrap整患者路径组内重抽。

联合似然：实际IA仍使用上述七列状态CSV，但基准风险与混合必须外部预定；不自动从实际IA拟合移入检验。02/12同一已知正指数风险；一般三转移OS正式检验未支持。

新家族ms/je双终点ADTTE导出以DAYS保存日历AVAL（沿用模拟ADTTE的首日+1），精确ENGINE保持连续年龄且不含首日+1。输入以ENGINE权威，AVAL不参与精确路径似然。METHOD保留所选原/重估规则；导入多规则样例须先过滤单一METHOD，再保证每人每终点唯一。观察仅截点内，未来真值另表，状态样例只含IA前资料。

配置为原无损typed JSON契约；包含患者资料，部署/外发不在本轮范围。持久续跑必须同版本、源代码、依赖、配置和原键；追加仅增B。所有实现与往返尚未验证。

## v0.35 panel独立入口增补

原精确状态/转移时长契约保持。Panel序列使用独立七列状态契约和常数Markov联合似然，不通过伪造PROG转成原精确路径模型。0/1/2或ALIVE、panel/末条exact_death、followup/末条active/dropout/event含义见EXPANSION_V035_CONTRACT与手册61–62章。该入口返回拟合/状态/参考或条件期望，不提供原精确路径预测的区间/日期分布，也不生成伪精确患者ADTTE。全部待验证。
