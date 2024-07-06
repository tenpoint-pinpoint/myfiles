
# 種子の数のデータをダウンロード
load("data.Rdata")
data
str(data)
summary(data)
table(data)

hist(data, breaks = seq(-0.5,9.5,1)) # -0.5~9.5までを1間隔で分割する

#分散
var(data)
#標準偏差
sd(data)


#ポアソン分布を描画する
y <- 0:30
prob <- dpois(y, lambda = 3.56)
plot(y, prob, type = "b", lty = 2)
plot(0:30, dpois(0:30, lambda = 6), type = "b", lty = 2)
plot(0:30, dpois(0:30, lambda = 0), type = "b", lty = 2)


# 第３章
dat <- read.csv("./poisson/data3a.csv")
str(dat)
summary(dat)

# 描画
# fをfactor型に変更しておく
dat$f <- as.factor(dat$f)
plot(dat$x, dat$y, pch = c(21, 19)[dat$f]) # pchは点の形を変える
legend("topleft", legend = c("肥料あり", "肥料なし"), pch = c(21,19))
# 種子数と体サイズには線形の関係がありそう、肥料のあるなしは関係なさそう

plot(dat$f, dat$y)