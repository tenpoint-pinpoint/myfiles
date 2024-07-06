#ディレクトリはkubobook_2012のbinomialにする.

d <- read.csv('data4a.csv')
d


#数値要約

summary(d)


#fがcharacter型になっている場合, factor型にする.

d$f = as.factor(d$f)
summary(d)


#xとyの散布図. fの値ごとに色分け.

plot(d$x, d$y, pch = c(21,19)[d$f])
legend("topleft", legend = c("C", "T"), pch = c(21,19))


#これから使うロジスティック関数とロジットリンク関数を観察.
#logistic関数の引数と値の範囲が重要

logistic <- function(z) 1/(1+exp(-z))
z <- seq(-6,6,0.1)
plot(z, logistic(z), type ="l")


#ロジットリンク関数はロジスティック関数をzについて解いたもの.

logitlink <- function(q) log(q/(1-q))
q <- seq(0,1,0.1)
plot(q,logitlink(q),type="l")


#ロジスティック回帰もポアソン回帰と同様に, glm関数で当てはめ（＝パラメータの最尤推定）ができる.
#ただし、応答変数の書き方が特別なので注意
#cbind(y,N-y)はyとN-yを横に並べる操作.100行2列データを内部で生成する.

fit.logistic.x_and_f <- glm(cbind(y,N-y)~x+f, data = d, family = binomial)

summary(fit)



#平均生存種子数の予測値をfの値ごとにプロットしてみる.

xx <- seq(min(d$x),max(d$x),length = 100) #xの値を100個用意
ff.C <- as.factor(rep(c("C"), times = c(100))) #100個のC
ff.T <- as.factor(rep(c("T"), times = c(100))) #100個のT

NN <- d$N #観測種子数.

plot.data.C <- data.frame(x = xx,f =ff.C)
plot.data.T <- data.frame(x = xx,f =ff.T)

#predict関数でplot.data.Cに対する生存確率の予測値を呼び出す.
#それにNNをかけると, 各データの生存種子数の期待値となる.（二項分布の期待値）

plot(xx, NN*predict(fit.logistic,newdata = plot.data.C, type = "response"), type = "l", xlim = c(min(d$x),max(d$x)), ylim = c(0,8))


#散布図と重ね合わせてみる. par(new=T)で重ねられるが, xlimとylimでxとyの描画範囲を揃えないとバラバラになる.
#d[d$f =="C",]でd$fがCである行を抽出.pch=19は白丸で点を描くオプション.

par(new=T)
plot(d[d$f == "C",]$x, d[d$f == "C",]$y, xlim = c(min(d$x),max(d$x)),ylim = c(0,8), pch = 21)



#観察に満足したらpar(new=F)を実行することを推奨.
#（parは永久の変更の命令.ただ、newについてはやらなくてもいい？)

par(new=F)


#f = Tの方も見てみる.

plot(xx, NN*predict(fit.logistic,newdata = plot.data.T, type = "response"), type = "l", xlim = c(min(d$x),max(d$x)), ylim = c(0,8))
par(new=T)
plot(d[d$f == "T",]$x, d[d$f == "T",]$y, xlim = c(min(d$x),max(d$x)),ylim = c(0,8), pch = 19)
legend("topleft", legend = c("f = C", "f = T"), pch = c(21,19))

par(new=F)


#両方見てみる.

plot(xx, NN*predict(fit.logistic,newdata = plot.data.C, type = "response"), type = "l", xlim = c(min(d$x),max(d$x)), ylim = c(0,8), col="blue")
par(new = T)
plot(xx, NN*predict(fit.logistic,newdata = plot.data.T, type = "response"), type = "l", xlim = c(min(d$x),max(d$x)), ylim = c(0,8), col="red")
par(new = T)
plot(d$x, d$y, pch = c(21,19)[d$f])
legend("topleft", legend = c("C", "T"), pch = c(21,19))

par(new = F)


#xモデル, fモデル, x+fモデルのAICの比較
#MASSpackageが未インストールならpackageメニューまたはtoolsメニューからインストールする.

library(MASS)
stepAIC(fit.logistic)


---------おまけ----------

#リンク関数は以下のように明示的に変えられる.

fit.logit <- glm(cbind(y,N-y)~x+f, data = d, family = binomial(link = "logit"))

fit.probit <- glm(cbind(y,N-y)~x+f, data = d, family = binomial(link = "probit"))


#プロビット関数は正規分布の分布関数の逆関数。これも0～1を-∞～+∞に対応させるので二項分布に使える。
#他の関数も使えることがあるが、確率分布のパラメーターの範囲を超えることがある。