# fisherの線形判別分析
# Rにデフォルトで入っているiris（アヤメ）のデータを使う

data(iris)

# 必要なデータだけを抜き出す
# 今回はversicolorとvirginicaを線形判別したい
dat <- iris[iris$Species %in% c('versicolor', "virginica"), #-------------> どこの行を抜くか（TRUEの箇所だけ残す）
            c("Petal.Length", "Petal.Width", "Species")] #----------------> どこの列を抜くか
dat$Species <- factor(dat$Species,levels = c("versicolor","virginica")) #-> speciesからsetosaを取り除く

# 上の処理が何をしているかわかりやすく
iris$Species %in% c('versicolor', "virginica") #---------------> boolianが返ってくる
factor(dat$Species,levels = c("versicolor","virginica")) #-----> factor型のラベルを明示的に選択

summary(dat)

# 可視化すると線形で判別できそうかどうかがだいたいあたりをつけられる
plot(dat, col=dat$Species)  
# Petalが大きいのがvirginica
# 線形で分類できそうに思える



#線形判別分析の実行
library(MASS)

# 判別分析は、lda関数で線形判別分析の実行
result <- lda(Species ~ ., data = dat)
result$scaling
#------結果------------------------------#
#                    LD1
# Petal.Length 0.8712821
# Petal.Width  2.9247035
#----------------------------------------#

# 判別得点の計算
sum(result$scaling * dat[1, c("Petal.Length", "Petal.Width")])# 目的変数だけ取り除く

# 予測のための修正
# speciesで分けて、それぞれ平均を算出
group_mean <- aggregate(.~Species, dat, mean)
# 説明変数だけを行列にする
group_center <- as.matrix(group_mean[, c("Petal.Length", "Petal.Width")]) %*% result$scaling
# ２つの判別得点の平均を算出しておく
mean(group_center)


# 【最終的な判別得点】
# 判別得点からそれぞれの平均をひき、０を中心として判別得点と比較できるようにする
sum(result$scaling * dat[1, c("Petal.Length", "Petal.Width")]) - mean(group_center)



# 上記式をlda関数で実施、全てのサンプルに対して判別得点を付与する
pred <- predict(result, dat[,c("Petal.Length", "Petal.Width")])
pred

pred$class  #予測ラベル
dat$Species #正解ラベル

#正誤チェック
pred$class == dat$Species #------> trueは予測と実際が同じもの
sum(pred$class == dat$Species)#--> trueは1、falseは0として計算されるので、trueの数がわかる

#正解率の計算　二式は同じ意味
sum(pred$class == dat$Species) / 100
mean(pred$class == dat$Species)


# 群間分散と群内分散を計算してみる---------------------------------------------------

# 群間分散の行列S_Bの計算
type_mean <- aggregate(.~Species, dat, mean)
type_mean <- type_mean[,c("Petal.Length", "Petal.Width")] # spiceiesのカラムを削除
type_mean

diff <- as.numeric(type_mean[1,]-type_mean[2,]) #群平均の引き算
diff

dbg <- diff %*% t(diff)
dbg


# 群内分散の計算
ver_v <- cov(dat[dat$Species=="versicolor",c("Petal.Length", "Petal.Width")])
vir_v <- cov(dat[dat$Species=="virginica",c("Petal.Length", "Petal.Width")])

dwg <- 1/(100-2)*((50-1)*ver_v+(50-1)*vir_v)
dwg

# w1、w2を求める
# SW^(-1)SBの固有ベクトル
eigen(solve(dwg) %*% dbg) # solveは逆行列を求める関数



# 非線形判別分析
library(MASS)

result <- qda(Species~Petal.Length + Petal.Width, data = dat)
result

#予測をしてみる
pred <- predict(result, dat[1:10, c("Petal.Length", "Petal.Width")])
pred
