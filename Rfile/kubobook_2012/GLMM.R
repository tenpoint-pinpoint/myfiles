#glmmMLパッケージのインストール（1回だけでOK）

install.packages("glmmML")


#ディレクトリはkubobook_2012のglmmに

d <- read.csv("data.csv")


#数値要約

summary(d)


#葉の数xと生存種子数yの散布図.

plot(d$x, d$y)


#生存種子数のヒストグラム

hist(d$y, breaks = seq(-0.5,8.5,1))


#葉の数が4のときの生存種子数のヒストグラム

hist(d[d$x == 4,]$y, breaks = seq(-0.5,8.5,1))


#data.frameに[条件式,]でその条件式を満たす行を出せる.
#条件式の後の,を忘れない

d4 <- d[d$x == 4,]
c(mean(d4$y),var(d4$y)) #過分散が確認される



#GLMMのパラメータ推定

library(glmmML)

#二項分布とポアソン分布ではglmmMLが使える.
#それぞれデフォルトのリンク関数はロジット関数と対数関数
#cluserで個体を識別する番号（個体差が独立する番号）を指定.今回はID列の個体番号



#分布：二項分布、リンク関数：ロジット関数

fit <- glmmML(cbind(y,N-y)~x, data = d, family = binomial, cluster = id)


#下記でも同じ。リンク関数を明示的に指定

fit <- glmmML(cbind(y,N-y)~x, data = d, family = binomial(link = "logit"), cluster = id)


#分布：二項分布、リンク関数：cloglog関数（これも変数と関数の値の関係が0～1と-∞～+∞の関係になるので、リンク関数として使える）
#現状だとlogitかcloglogのみ実装

fit.cloglog <- glmmML(cbind(y,N-y)~x, data = d, family = binomial(link = "cloglog"), cluster = id)


#summaryしてみる。教科書とバージョンが違うため結果が微妙に異なる可能性あり。

summary(fit)


-----おまけ-----

#正規分布やガンマ分布ではlme4パッケージのglmer()が使える.
#これらの分布では過分散は起きないが、標本が擬似反復の場合はGLMMが必要（詳しくは７章後半）


#100個体からそれぞれ10回疑似反復の（同じ個体から複数データを取る）デモデータを作成。
#基本はr+分布名で乱数作成


#0～5の一様分布から乱数を生成（練習時間とか？）
#unifは連続値で出る、minとmaxの値は出ないことに注意。
#floorで離散値っぽくしている。

xx = rep(floor(runif(n=100,min=0,max=6)),each = 10)


#個体差として期待値0、標準偏差5の正規乱数

rr = rep(rnorm(n=100,mean=0,sd=5),each = 10)


#応答変数としてyを作る.今回は0～100の連続値
#正体は標準偏差20の正規分布のGLMMモデル：期待値＝20+12x+r
#例えばxは練習時間、yは反射神経テストのスコアのような状況？


#入れ物として1000個の-1を生成

yy = numeric(1000) - 1


#リンク関数は恒等で、期待値＝20+12x+r

for (i in 1:1000){
	while (yy[i] < 0 || yy[i] > 100) {
	yy[i] = rnorm(n=1,mean=20+12*xx[i]+rr[i],sd=20)
	}
}

#個体番号idも用意

d <- data.frame(x = xx,y = yy, id = rep(1:100, each = 10))


#yのヒストグラムを見てみる。
#連続なのでdensityを使って確率密度関数のカーネル推定も行う。

dens <- density(d$y)
hist(d$y,probability=T,xlim=range(dens$x),ylim = range(dens$y))
lines(dens,lwd=3)

#install.packages("lme4")してから（一回やってたらOK）
library(lme4)


#まずは正規回帰のGLMM（すなわち線形混合モデル＝LMM）
#正規回帰の場合はlmerを使う必要がある.

#glmmMLのcluster=idはこっちでは+(1|id)とする

#Random effects id Std.devが個体差の分散の推定値
#Fixed Effectが線形予測子の係数

lmer(y~x+(1|id),data=d)


#ガンマ分布によるGLMM

#リンク関数＝log
glmer(y~x+(1|id),data=d, family = Gamma(link = "log"))

#リンク関数＝1/m（ガンマ関数の正準リンク関数）
glmer(y~x+(1|id),data=d, family = Gamma(link = "inverse"))