data
length(data)
#度数分布を得る
table(data)
#seq(-0.5,9.5,1)は{-0.5,0.5,1.5 ~ 9.5}という数列を生成する。これをbreakに与えると、その区間の度数を縦軸にしたhistoglamになる
hist(data, breaks = seq(-0.5,9.5,1))

# ポアソン分布との対応関係をみる
# 平均3.56のポアソン分布を生成する
# dxxxxで確率分布の関数を利用。poisはポアソン分布。
# (y, lambda = 3.56)とすると、平均3.56のポアソン分布に従う場合のyの範囲の数値が出現する確率を得られる
y <- 0:9
prob <- dpois(y, lambda = 3.56)
plot(y, prob, type = "b", lty = 3)  # typeは点の形式、ltyは線の形式

hist(data, breaks = seq(-0.5,9.5,1))
par(new=T)  # TRUEでグラフの上書き
plot(y, prob, type = "b", lty = 3)
# 最大尤度点は傾きが０になるので、偏微分して０になるパラメータを探す

# 対数尤度を得る関数を作成している
logL <- function(m)sum(dpois(data,m,log=TRUE))
lambda <- seq(2,5,0.1)
# 対数尤度をプロットするためlogLの結果を縦軸にしている
plot(lambda, sapply(lambda, logL), type="l")


# 擬似乱数の作り方
# ポアソン分布
rpois(50, 3.5) # λ=3.5のポアソン分布から50個乱数を発生させる

