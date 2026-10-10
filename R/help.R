# One source for help beside every user-editable parameter.
parameter_help <- c(
  time_unit = "定义输入和显示的经过时间单位。日=1日，周=7日，月=365.25/12日；默认月。切换时转换已填写时间、速率和先验。日历日期保持不变；月不是可变长度的日历月。",
  input_mode = "参数输入直接使用给定生存分布，不需文件；ADTTE 数据用于拟合和数据后验。无个体记录时选参数输入；有明确终点及删失定义的记录时选 ADTTE。",
  origin = "统一日历坐标的研究起点，不是所有患者共同的风险起点。建议用首例入组日或方案指定的研究起点；不得晚于任何 STARTDT。",
  cut = "IA / 当前 DCO：自研究起点至本次数据截点的经过时间。非负；启动前预测填0。进行中按真实 DCO 与研究起点之差换算；当前随访时间不得超过该值。",
  active_n = "IA 时尚无事件且仍可继续获取目标终点的人数 Nactive；不含已事件者或永久退出者。输入非负整数；与 D0 合计不超过5000。建议从当前风险集汇总，不用总入组人数替代。",
  known_n = "当前已记录目标事件数 D0，未来模拟保持固定。输入非负整数；与 Nactive 合计不超过5000。按当前终点及数据截点计数；只有汇总人数时无法恢复历史事件日期。",
  age_mode = "指定当前风险集的已无事件随访年龄分布。相同值用于统一随访年龄假设；均匀分布用于仅知道最大年龄的近似情景。建议有个体年龄时使用 ADTTE。均匀模式实际从最大值的0.1%到最大值抽样以避免零时间。",
  duration = "患者在 IA 时已无事件随访时间 a；均匀模式为最大年龄。当前人数>0时必须>0且不超过当前 DCO。建议依据 STARTDT 到末次确认日的经过时间；历史讨论中8个月可作演算示例。",
  file = "上传 ADTTE CSV、XPT 或 SAS7BDAT。必需变量为 USUBJID、PARAMCD、STARTDT、ADT、AVAL、CNSR。建议先用合成模板核对格式；筛选后每位受试者只能有一条记录。",
  paramcd = "选择一个目标终点，例如 PFS 或 OS。来自文件的 PARAMCD。建议与方案事件目标 D* 对应；不同终点不能在同一拟合中混合。",
  aval_unit = "文件 AVAL 的单位，独立于平台显示单位。默认日；若 AVALU 存在，需与选择一致。月固定按365.25/12日换算；建议按 ADTTE 元数据选择，保留足够小数以通过日期核对。",
  date_encoding = "未解码的字符日期使用 YYYY-MM-DD；未标记的数值日期需明确选择自1960-01-01起的 SAS 数值日。按文件定义选择，不按标签猜测编码。XPT/SAS 已解码日期直接使用；STARTDT/ADT 不自动截去时分秒，不将 Excel 日期序号当作 SAS 日期。",
  offset = "AVAL 换算为日后所用的日期加项：0或1。若分析定义为 ADT−STARTDT+1，选1；否则按定义选0。内部风险时间始终用实际经过日数 ADT−STARTDT。",
  dropout_codes = "明确表示永久退出目标终点随访的正整数 CNSR 编码，可逗号分隔或留空。CNSR=0始终为事件。建议查分析规格逐项填写；停药但继续 OS 随访者不属于 OS 永久退出。模板的2只是示例。",
  analysis_flag = "筛选唯一分析记录的标志变量，例如 ANL01FL。每个 USUBJID 在所选 PARAMCD 下必须唯一。建议有多条分析记录时选择规格指定的标志；已唯一时可不筛选。",
  flag_value = "分析标志变量的保留值，区分大小写。建议按 ADTTE 规格填写，如Y。未选择标志变量时不使用此值。",
  gap_mode = "仍随访者的 ADT 早于当前 DCO 时的处理。若已确认无事件至 DCO，用严格模式并提供更新记录；若只有末次确认信息，可选择补全，模拟未观察区间内事件与脱落。补全不包含访视判定过程。",
  design_model = "选择给定参数的事件时间分布。指数风险恒定；Weibull风险单调；PWE为分段常数；其余分布和混合见方法页。建议依据历史曲线及终点定义设定，并比较风险情景。",
  median = "中位生存时间 mPFS / mOS，满足 S(m)=0.5；必须>0。治愈模型中指未治愈成分，混合模型中指第一成分。建议用可比研究或方案假设；12个月是历史讨论的演算值，不是通用推荐。",
  shape = "Weibull形状 k，必须>0；k=1风险恒定，k>1递增，k<1递减。建议有历史数据时估计；无数据可比较0.7、1、1.5等情景。它不是 survreg 的 scale（后者=1/k）。",
  scale = "对数时间分布的尺度 sigma，必须>0，无时间单位。Log-normal中为log(T)的标准差；Log-logistic中为Logistic尺度。建议根据历史生存分布估计，不能与Weibull尺度eta互换。",
  g_rate = "Gompertz初始风险 b=h(0)，必须>0，单位为1/当前时间单位。建议依据早期事件密度估计；它是瞬时风险率，不是每期事件概率。",
  g_shape = "Gompertz风险随时间改变的系数 g，h(t)=b exp(gt)，可正、零或负。单位为1/当前时间单位。建议以g=0作恒定风险参照；g<0有永久生存质量，需确认是否符合假设。",
  cure = "治愈成分比例 pc，0≤pc<1；本平台指目标终点长期不发生的分布成分，并非临床治愈判定。建议依据长期随访依据设定，缺乏依据时从0开始并作敏感性分析。",
  median2 = "双成分Weibull第二成分中位生存时间，必须>0。建议按第二风险亚群的历史分布或情景假设填写；不要将混合总体中位数直接代入。",
  shape2 = "第二成分Weibull形状 k2，必须>0，k2=1为恒定风险。建议有第二成分数据时估计；没有时比较不同形状情景。",
  mix = "第一成分比例 pi，0<pi<1；第二成分比例为1−pi。建议按总体中的亚群占比设定。已有无事件随访信息会改变条件成分概率；这里不拟合盲态治疗组。",
  parameter_cuts = "PWE风险分段边界，按自患者风险起点的随访年龄输入，正数、严格递增、逗号分隔。历史讨论用3、6、12个月。建议按访视/风险变化依据预先指定；不是日历截点。",
  parameter_rates = "各PWE段的风险率 lambda_j，非负，数量=切点数+1，单位为1/当前时间单位。建议用各段事件数/受试者时间或方案假设；零率表示该段不累积风险，零尾部可使目标不达标。",
  methods = "选择用ADTTE右删失似然拟合的模型，可多选，包括Gompertz和Cure-Weibull；曲率、梯度或治愈边界检查失败时不保留。Gamma事件后验限指数/PWE；Weibull MCMC限Weibull。建议先检查观察区间拟合与尾部假设；AIC不验证远期外推。",
  cuts = "数据拟合PWE切点，正数、严格递增，按随访年龄输入。建议以3、6、12个月为演算起点，按实际数据调整，使每段都有暴露；无暴露区间会拒绝拟合。",
  tail_rate = "最长观察随访时间之后的KM指数尾部风险 lambda_tail，必须>0。单位为1/当前时间单位。建议依据末期事件/暴露或外部随访依据设定；KM自身不能估计该外推率。",
  uncertainty = "固定参数仅模拟未来随机性；Bootstrap重新拟合受试者重抽样数据；Gamma/MCMC从参数后验抽样。建议数据模式比较固定参数与包含参数不确定性的结果；参数模式使用给定参数。",
  prior_shape = "事件率Gamma先验shape alpha>0，无量纲；先验均值=alpha/beta，变异系数=1/sqrt(alpha)。建议同时指定有依据的均值及先验暴露强度；默认0.5仅为演示。PWE各段使用同一组超参数。",
  prior_rate = "事件率Gamma先验rate beta>0，单位为受试者×当前时间单位。后验rate=beta+总暴露。建议根据先验均值 lambda0 设 beta=alpha/lambda0，并检验先验敏感性；不能当作风险率输入。",
  log_eta_mean = "log(Weibull尺度eta)的正态先验均值，eta按当前时间单位数值表示。建议由m0和k0计算log[m0/(log2)^(1/k0)]；不要直接输入mPFS或log(mPFS)。切换单位会平移该均值。",
  log_eta_sd = "log(eta)先验标准差>0，无量纲。建议用可接受的尺度范围反推；均值±1.96倍标准差经指数变换给出约95%的eta范围。默认1表示范围较宽，需结合研究检查。",
  log_shape_mean = "log(k)正态先验均值，可为任意有限数。建议以log(1)=0作恒定风险参照，或用历史形状k0的log值；不随时间单位改变。",
  log_shape_sd = "log(k)先验标准差>0，无量纲。建议用k的合理范围反推并检查尾部外推。默认0.75为演示；标准差越大，形状的先验范围越宽。",
  mcmc_chains = "独立MCMC链数，整数2–4。建议4链用于检查收敛；各链从后验模式附近分散起点启动。R-hat和ESS合格后才进行预测。",
  mcmc_warmup = "每链预热迭代数，整数200–10000；仅用于调节提议，不作为后验draw。建议先1000；链迹或诊断不足时增加并重新运行。",
  mcmc_draws = "每链保留的后验迭代数，整数200–10000。建议先3000；要求R-hat≤1.01且bulk/tail ESS≥400。相关draw使有效样本数小于保留次数，未通过时需增加或检查模型。",
  ensemble = "按原数据AIC权重混合不同模型的完整预测轨迹。建议在参数模型可比时查看；不包含KM。它不是后验模型概率，也不通过平均各模型区间端点得到区间。",
  future_n = "从当前DCO之后计划继续入组的人数上限，整数0–1000。建议填剩余计划人数；已完成入组填0。实际预测窗口内可入组少于该上限。",
  enroll_mode = "未来入组按恒定或分段Poisson强度模拟。建议有明确阶段计划时用分段；所有切点从当前DCO开始计时，达到计划人数后停止。过程Gamma后验只适用于恒定率。",
  enroll_rate = "恒定未来入组率 r，单位为人/当前时间单位，≥0。建议用近期每期实际入组量或计划；15人/月为演示。0表示没有新增入组。不是入组概率。",
  enroll_cuts = "分段入组强度的切点，从当前DCO起算，正数严格递增。建议依据中心启动或阶段计划设置，如3、6个月；不与患者随访年龄切点混用。",
  enroll_rates = "各阶段入组率，≥0，单位为人/当前时间单位，数量=切点数+1。建议用各阶段计划入组量/阶段长度；最后一段为0表示之后停止入组。",
  dropout_rate = "独立永久退出目标终点随访的风险 mu，≥0，单位1/当前时间单位。建议用永久退出数/总受试者时间；无永久脱落假设填0。一单位脱落概率p应换算为mu=−log(1−p)，不要直接填百分数。",
  multiplier = "未来事件风险调整系数q>0：q=1沿用模型，0.7表示相对于外推风险降低30%，1.3表示提高30%。建议以1为基准并设置有依据的敏感性范围；不是治疗组间HR。治愈模型中调整总体条件风险。",
  clock = "按实际发生日或模拟上报日计数。建议与事件目标的操作定义保持一致；上报模式仅给新增模拟事件加固定延迟，已有已记录事件保持固定。",
  lag = "新增模拟事件的固定上报延迟L≥0，以当前时间单位输入。建议用历史发生到入库的典型延迟，或0作无延迟情景。当前版本不模拟延迟分布或恢复历史报告积压。",
  process_uncertainty = "数据模式的未来入组/脱落率处理。固定使用输入率；Gamma后验从指定历史窗口的入组数及已观察脱落/暴露更新。建议窗口有完整入组记录且机制近似稳定时才选后验。",
  recruit_start = "历史入组率观测窗口起点，自研究起点起算，≥0。建议选入组机制相对稳定且记录完整的窗口；与终点/标志筛选后的数据范围一致。",
  recruit_end = "历史入组窗口终点，大于起点且不得晚于当前DCO。建议用窗口实际终点；未完整记录窗口内入组者时，不能用筛选后人数代表全部入组。",
  enroll_prior_shape = "入组率Gamma先验shape alpha_r>0，无量纲。建议与rate一起设定：均值=alpha_r/beta_r，人/当前时间单位。默认1为演示；较大的shape在相同均值下表示更强先验。",
  enroll_prior_rate = "入组Gamma先验rate beta_r>0，单位为当前时间单位。后验beta_r+窗口长度。建议用先验入组率r0设beta_r=alpha_r/r0；该参数是时间，不是每期入组人数。",
  drop_prior_shape = "永久脱落率Gamma先验shape alpha_mu>0，无量纲。建议结合脱落先验均值及暴露设置；默认0.5为演示，不是默认脱落比例。",
  drop_prior_rate = "永久脱落Gamma先验rate beta_mu>0，单位为受试者×当前时间单位。建议用mu0设beta_mu=alpha_mu/mu0；后验加总暴露，切换单位时同比换算。",
  target = "Final DCO触发的累计目标事件数D*，正整数。建议按方案分析触发目标填写；目标超过D0+Nactive+计划入组人数时无法达到。达到事件目标的预测日不等于数据库锁定日。",
  horizon = "从当前DCO之后观察的预测时间窗口，换算后1–3650日。建议覆盖计划分析日期并留出合理范围；未在窗口内达标的轮次仍保留，不能仅汇总成功轮次。",
  sims = "未来试验轨迹模拟次数B，整数50–2000。建议开发检查用300，比较时可增加至2000。单模型独立模拟概率约0.5时，B=300的MC标准误约2.9个百分点；AIC混合需计入两层抽样误差，查看输出MCSE。MCMC模式的MCSE条件于当前后验样本库。",
  seed = "随机数种子，0至2147483647的整数。建议同一分析使用固定值并随配置记录；重复同一配置可重现，相同种子不保证不同模型轨迹逐患者配对。",
  backtest_cuts = "历史DCO，自研究起点起算，1–6个正数且早于当前数据截点。建议选择当时已有足够事件的截点。需另填各截点当时的入组计划。只回测实际发生事件；最终文件不能恢复当时访视/报告版本。",
  backtest_future_n = "每个历史DCO当时计划继续入组的人数上限，与回测截点按顺序一一对应，整数0–1000。建议查当时方案或计划；例如两个截点分别剩余100、30人，填100,30。不能用当前剩余人数替代，也不从最终样本量反推。",
  backtest_enroll_rates = "每个历史DCO当时设定的未来恒定入组率，与截点一一对应，非负，人/当前时间单位。建议查当时计划，如15,10；有剩余入组时固定率需>0。过程Gamma模式用早期数据更新率，此输入仅记录计划，未参与率抽样。历史分段计划尚不支持。",
  sensitivity_values = "1–8个正数的风险情景列表。合并模式直接使用这些q；分组模式将各组基准q乘以该值。建议包含1，并依据研究选择如0.75、1、1.25；其余参数使用最近成功运行配置。",
  lab_model = "条件生存演算使用的模型，来自最近一次成功预测。建议选与当前预测对应的模型；更改输入参数后需重新运行，模型才会更新。",
  age = "已确认无事件随访的年龄a≥0，自该患者风险起点起算。建议输入实际随访年龄，历史讨论用8个月。模型必须在该年龄仍有正生存概率；它不是研究DCO。",
  lab_horizon = "从已随访年龄a之后继续观察的剩余时间U窗口，换算后1–3650日。建议覆盖拟考察的剩余随访；图示为事件条件生存，不包含脱落或入组。",
  lab_multiplier = "条件生存演算的未来风险调整q>0。建议先用1核对S(a+u)/S(a)，再比较风险变化；它只影响本页演算，不改变已保存的试验预测。"
)
parameter_help <- c(parameter_help,
 exp_input="指数分布的等价输入：中位时间m或恒定风险lambda。m=log(2)/lambda；仅一个输入作为本次依据。风险率不是固定周期内的事件概率。建议用方案中位数或明确的风险率估计。",
 exp_rate="指数恒定风险lambda>0，单位为1/当前时间单位。m=log(2)/lambda，例如中位12月对应0.05776/月。不要把12个月累计事件比例直接当作lambda。",
 weibull_input="Weibull可输入中位时间m或尺度eta，并同时指定形状k。eta=m/[log(2)]^(1/k)。治愈模型这里的m/eta对应未治愈成分，双成分模型对应第一成分。",
 eta="Weibull时间尺度eta>0，单位随日/周/月选择。S(eta)=exp(-1)，不是中位时间；中位=m=eta[log(2)]^(1/k)。建议依据同一形状k进行换算。",
 log_input="对数正态/对数Logistic可输入中位时间m或对数时间位置mu；m=exp(mu)。mu会随所选单位变化，sigma保持不变。建议用中位时间以便直接解释。",
 log_mu="mu是log(T/所选时间单位)的位置，有限实数；中位时间为exp(mu)个所选单位。换单位时mu加上单位换算的对数，不按普通时间倍乘；不是风险率。",
 drop_input="独立指数永久脱落可输入风险mu，或指定时间窗口内的脱落概率p。mu=-log(1-p)/窗口。该概率定义于独立潜在脱落时钟，不等于事件竞争后已观察脱落比例。",
 drop_prob="独立潜在脱落时钟在指定窗口内发生的概率，范围[0,1)。例如每月2%且窗口1月，对应mu=-log(0.98)/月。需来自符合独立指数假设的数据或情景。",
 drop_period="脱落概率对应的经过时间窗口，正数，单位随日/周/月切换。建议明确如1月或12月；改变窗口而保持概率会改变脱落风险。")
parameter_help <- c(parameter_help,
  analysis_mode = "合并模式忽略组别，拟合总体事件分布；已知组别模式在每组内独立拟合或设置分布，再逐轮合并事件日期。开放研究可选已知组别；需要真实组别信息，不能从盲态结果推断。",
  group_column = "选择本次分析使用的组别变量，推荐按研究分析定义选择TRTP（计划治疗）或TRTA（实际治疗）。筛选后每人一行、组别不能缺失，需2–6组。不会在TRTP和TRTA间自动替换，也不自动处理交叉治疗。",
  group_count = "无数据参数模式的组数，整数2–6。通常两组研究填2；每组分别设置当前人数、年龄、生存分布和未来计划。当前总人数≤5000，未来入组总人数≤1000；目标D*为各组事件数合计。",
  name = "本次分析组名，必须非空且各组唯一。建议填写方案中的治疗组名称，如对照组、试验组；ADTTE模式组名直接来自所选变量的值，不由输入比例推断。",
  fit_method = "数据模式可使用事件模型页的公共候选列表，或为本组指定一个分布。两组可选不同分布；固定组内模型时直接联合预测。Gamma仅支持指数/PWE，MCMC仅支持Weibull；本组数据至少3人、2个事件。请依据终点过程、拟合诊断与尾部假设选择。",
  posterior_model = "选择需要查看后验诊断的组别/Weibull模型，来自最近成功运行。每组独立采样并各自满足R-hat及ESS阈值；共同先验超参数不表示各组共享一个后验参数。")
for (i in 1:6) {
  ids <- c("exp_input","exp_rate","weibull_input","eta","log_input","log_mu","drop_input","drop_prob","drop_period","fit_method","name","active_n","known_n","age_mode","duration","design_model","median","shape","scale","cure","median2","shape2","mix","g_rate","g_shape","parameter_cuts","parameter_rates","cuts","tail_rate","future_n","enroll_mode","enroll_rate","enroll_cuts","enroll_rates","dropout_rate","multiplier","lag","backtest_future_n","backtest_enroll_rates")
  parameter_help[paste0("g",i,"_",ids)] <- paste0("本组参数。",parameter_help[ids])
}
parameter_label <- function(id, label) {
  if (is.null(label)) return(NULL)
  if (!id %in% names(parameter_help)) stop(paste("缺少参数说明：", id))
  text <- parameter_help[[id]]
  if (is.null(text) || is.na(text) || !nzchar(text)) stop(paste("缺少参数说明：", id))
  tagList(label, bslib::popover(
    tags$button(type = "button", class = "parameter-help", "?", `aria-label` = paste("参数说明", id)),
    tags$p(text), title = "含义与输入建议", placement = "auto", options = list(trigger = "click")))
}
help_input <- function(fun) {
  force(fun)
  function(inputId, label, ...) fun(inputId, parameter_label(inputId, label), ...)
}
for (fn in c("numericInput", "textInput", "textAreaInput", "selectInput", "dateInput", "radioButtons", "checkboxInput", "checkboxGroupInput", "fileInput")) {
  assign(fn, help_input(getExportedValue("shiny", fn)))
}
