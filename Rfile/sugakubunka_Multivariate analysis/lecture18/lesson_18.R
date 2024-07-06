R.version
.libPaths()

install.packages("")
install.packages("semPlot", dependencies=TRUE)
install.packages("lavaan")
library(semPlot)
library(lavaan)
library(psych)

remove.packages("semPlot")

dat <- read.csv("./data/seiseki.csv", fileEncoding = "utf-8")
str(dat)
describe(dat)
# 標本の分散共分散行列
cov(dat)

# モデルの指定 = パス図をコードにしていく
model <- "初級統計学~~基礎からのR
          続初級統計学~~0*数理統計学
          続初級統計学~初級統計学+基礎からのR
          数理統計学~初級統計学+基礎からのR"

result <- sem(model = model, dat = dat)
summary(result)

par(family = "ヒラギノ角ゴシック W3")
semPaths(result, "model", "est", sizeMan = 5, edge.label.cex = 1.0)

# 決定係数
# 続初級統計学の分散を「他の変数から説明されている」と「誤差」に分ける

# 続初級統計学の誤差の分散：85.023
# 続初級統計学の分散：97.60552

1-85.023/97.60552
# 0.128912 -> 誤差がまだまだ多くの情報を持っている可能性が高い。
# 変数を増やす？構造モデルを変更する？いろいろ考えてみる


