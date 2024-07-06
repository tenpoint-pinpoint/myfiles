

dat <- read.csv("./binomial/data4a.csv")
dat$f <- as.factor(dat$f)

summary(dat)

plot(dat$x, dat$y, pch = c(21,19)[dat$f])
legend("topleft", legend = c("C", "T"), pch = c(21,19))

# ロジットリンクで解析
logitlink <- function(q) log(q/(1-q))
q <- seq(0,1,0.1)
plot(q, logitlink(q), type = "l")

# ロジット関数の様子
logistic <- function(z) 1/(1+exp(-z))
z <- seq(-6,6,0.1)
plot(z, logistic(z), type ="l")

# glm関数でロジスティック回帰（link=ロジット）
fit.logistic <- glm(cbind(y,N-y)?x+f, data = dat, family = binomial(link = "logit"))
# cbind(https://webbeginner.hatenablog.com/entry/2015/02/06/132256参照)
fit.logistic
summary(fit.logistic)

# glm関数でロジスティック回帰（link=プロビット）
fit_probit <- glm(cbind(y,N-y)?x+f, data = dat, family = binomial(link = "probit"))
summary(fit_probit)


# ロジットモデルで作った予測と実際の観測値の比較


xx <- seq(min(dat$x),max(dat$x),length = 100)
ff.C <- as.factor(rep(c("C"), times = c(100))) 
ff.T <- as.factor(rep(c("T"), times = c(100))) 

NN <- dat$N

plot.data.C <- data.frame(x = xx,f =ff.C)
plot.data.T <- data.frame(x = xx,f =ff.T)


# 生存確率の予測値を計算
# ”response”の指定なしの場合はリンク関数の値が出力されて、指定ありの場合はリンク関数の逆関数の値が出力される
pred_c <- predict(fit.logistic, newdata = plot.data.C, type = "response")
pred_c
# 上記に観測数nをかけると、各データの生存数の期待値になる
plot(xx, NN*predict(fit.logistic,newdata = plot.data.C, type = "response"), type = "l", xlim = c(min(dat$x),max(dat$x)), ylim = c(0,8))

# 肥料やってない方と期待値を重ねてプロット
plot(xx, NN*predict(fit.logistic,newdata = plot.data.C, type = "response"), type = "l", xlim = c(min(dat$x),max(dat$x)), ylim = c(0,8))
par(new=T)
plot(dat[dat$f == "C",]$x, dat[dat$f == "C",]$y, xlim = c(min(dat$x),max(dat$x)),ylim = c(0,8), pch = 21)

# 肥料やってる方と期待値を重ねてプロット
plot(xx, NN*predict(fit.logistic,newdata = plot.data.T, type = "response"), type = "l", xlim = c(min(dat$x),max(dat$x)), ylim = c(0,8))
par(new=T)
plot(dat[dat$f == "T",]$x, dat[dat$f == "T",]$y, xlim = c(min(dat$x),max(dat$x)),ylim = c(0,8), pch = 21)


library(MASS)
stepAIC(fit.logistic)
#### 結果 #######################################
#         Df Deviance    AIC
# <none>      123.03     272.21  -> x+fモデル
# - f     1   217.17     364.35  -> xのみモデル
# - x     1   490.58     637.76  -> fのみモデル
#################################################


# 第7章GLMMモデル
# install.packages(("glmmML"))
library(glmmML)

d <- read.csv("./glmm/data.csv")
head(d)
plot(d$x, d$y)
hist(d$y, breaks = seq(-0.5, 8.5, 1))
# データフレームに条件式をいれることでその条件にあうレコードだけ抜きだせる
var(d[d$x == 4,]$y)

# glmmMLでは二項分布とポアソン分布のみ使える
# デフォルトのリンク関数はロジット関数と対数関数
fit <- glmmML(cbind(y,N-y)?x, data = d, family = binomial, cluster = id)
# cluster = 個体を識別する番号（集合として考えるグループに番号をつける）今回は100種のid

fit
#### 結果 #####################################################################################
#              coef   se(coef)    z  Pr(>|z|)
# (Intercept) -4.190   0.8777 -4.774  1.81e-06
# x            1.005   0.2075  4.843  1.28e-06
# Scale parameter in mixing distribution:  2.408 gaussian   -> 個体差の分散（rの分散）
# Std. Error:                              0.2202 
# LR p-value for H_0: sigma = 0:  2.136e-55 
# Residual deviance: 269.4 on 97 degrees of freedom  -> 逸脱度（小さいほど最大対数尤度が大きい）
# AIC: 275.4 
###############################################################################################


exp(-1)

