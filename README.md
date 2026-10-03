# EVENT / PRED

面向 OS/PFS 事件驱动临床试验的 **R Shiny 事件数与目标事件日期预测平台**。中文界面，交互曲线，完整方法推导。

当前版本 **v0.1.0，研究原型**。已有可运行计算功能；盲态混合、治愈、完整贝叶斯、多状态等专门方法在方法库和开发方案中明确标为规划中。

## 启动

在仓库目录内运行：

```r
source("scripts/install.R")
shiny::runApp(".", host = "127.0.0.1", port = 3838)
```

命令行：

```sh
Rscript scripts/install.R
Rscript scripts/check.R
Rscript -e 'shiny::runApp(".", host="127.0.0.1", port=3838)'
```

浏览器打开 `http://127.0.0.1:3838`。默认提供合成队列，可直接运行；上传 CSV 后配置截点，字段要求见 [数据格式](docs/DATA_CONTRACT.md)。

## 已实现

- 指数、Weibull、PWE、Log-normal、Log-logistic、KM + 指定指数尾部。
- 当前患者条件生存模拟，保留既有事件并排除永久脱落患者的未来事件。
- 未来恒定率 Poisson 入组、独立指数脱落、未来风险倍数和固定上报延迟情景。
- 固定参数预测、受试者 bootstrap、指数/PWE Gamma 共轭后验。
- AIC 加权预测混合，权重不解释为贝叶斯后验模型概率。
- 事件轨迹及逐点 95% 预测区间、目标日期分布、预测窗口内达标概率。
- 条件生存实验室、拟合与外推对比、数据校验、CSV/配置/摘要导出。

尚未实现：分组/协变量、日期字段映射、截点前上报积压、完整联合贝叶斯、中心入组、治愈/盲态混合/多状态、启动前规划和历史回测。详细范围见 [开发方案](docs/DEVELOPMENT_PLAN.md) 与 [方法推导](docs/METHODS.md)。

## 实现与验证

`R/models.R` 为事件模型接口，`R/forecast.R` 为预测引擎，`app.R` 为 Shiny 界面。`scripts/check.R` 检查条件分布、分段边界、脱落处理、目标事件时点、概率分母、共轭抽样、可重复性和 Shiny 服务逻辑；它不等同于正式区间覆盖率或外部验证。

所有时间以天计算，日期只用于显示。PWE 切点处事件归于切点结束的区间。未在窗口内达标的模拟轮次保留，不从分母删除。默认 Gamma 先验与 KM 尾部都是用户可调假设，不能自动适用于所有疾病。输入大于首版计算资源限制会明确报错。

MathJax 公式渲染需要浏览器能加载 Shiny 的 MathJax 资源；计算不依赖外部数据服务。正式部署请进一步锁定依赖版本并完成验证矩阵。

## 部署

源码托管在 GitHub；GitHub Pages 不能提供 R 服务端。可部署到 shinyapps.io、Posit Connect 或 Shiny Server。本仓库提供 `Dockerfile`，可在安装 Docker 的环境中执行：

```sh
docker build -t event-pred .
docker run --rm -p 3838:3838 event-pred
```

容器会把端口公开到运行机器上；真实数据部署应根据实际环境配置访问控制。此次未部署在线服务，Docker 配置尚需目标环境构建验证。

## 材料与方法来源

产品范围来自既有事件数预测与条件生存讨论，说明文档重新撰写并链接相关原始研究。原 Markdown 笔记附件尚未取得，不声称已读取其字节内容。仓库包含公开方法说明、代码和合成示例，不包含原聊天导出或真实受试者数据。
