# 批量设计参数模板（开发稿）

batch_fixed_months.csv用于固定DCO，文件时间单位选月。batch_event_minimum.csv用于事件目标或事件+最短研究时间，cut_value是事件数。共同Control分布、入组、退出、最大窗口/最短时间、检验和重复数仍由界面填写。数字只说明结构，不作为研究设计建议；这两份表没有运行或验证。


v0.24：batch_nph_months.csv用于固定DCO、文件单位月，需选择固定真值NPH并保留默认H0/delay/waning/cross定义。hr=1表示原定义的幅度1；正式无效应仍按实际各段HR判断。按需选择主/附加方法，FH参数及tau另填。示例未运行、未验证。


v0.25：batch_sequential_events.csv用于PH组序贯，cut_value为原Final D*，窗口/原设计另由界面填写；可以选择同轮固定参照。该结构示例未运行、未验证。
