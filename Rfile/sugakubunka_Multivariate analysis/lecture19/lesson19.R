install.packages("semPlot")
install.packages("lavaan")

R.version
library(lavaan)
library(semPlot)


dat <- read.csv("./data/skill_stats.csv", fileEncoding = "utf-8")


cov(dat)

# ?    回帰
# ??   相関
# =?   右辺の後ろにある潜在変数を想定

# モデルの作成
model <- "
  skill =? 1*r + python
  stats =? 1*basic + advance
  stats ? skill
"

# 分析タスク
result <- sem(model = model, data = dat)
summary(result)

# 可視化
semPaths(result, "model", "est", sizeMan = 5, edge.label.cex = 1.0)

# goodness of fit index：適合度指標 -> １に近いほどよい
fitmeasures(result, fit.measures = "gfi")