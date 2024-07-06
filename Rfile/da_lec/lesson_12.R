data(iris)

dat <- iris[iris$Species %in% c('versicolor', "virginica"),#-----> どこの行を抜くか（TRUEの箇所だけ残す）
            c("Petal.Length", "Petal.Width", "Species")]#--------> どこの列を抜くか

iris$Species %in% c('versicolor', "virginica")#------------------> boolianが返ってくる
str(dat)#--------------------------------------------------------> setosaが残っている
dat$Species <- factor(dat$Species,levels = c("versicolor","virginica")) 
str(dat)
summary(dat)

# 可視化してみる
plot(dat, col=dat$Species)  # 可視化すると線形で判別できそうかどうかがだいたいあたりをつけられる！！
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
group_center <- as.matrix(group_mean[, c("Petal.Length", "Petal.Width")]) %*% result$scaling
mean(group_center)


# 【最終的な判別得点】判別得点からそれぞれの平均をひき、０を中心として判別得点と比較できるようにする
sum(result$scaling * dat[1, c("Petal.Length", "Petal.Width")]) - mean(group_center)



# 上記式をlda関数で実施
pred <- predict(result, dat[,c("Petal.Length", "Petal.Width")])
pred

pred$class  #予測ラベル
dat$Species #正解ラベル

#正誤チェック
pred$class == dat$Species
sum(pred$class == dat$Species)

#正解率の計算
sum(pred$class == dat$Species) / 100


# 群間分散の行列S_Bの計算
group_mean <- aggregate(.~Species, dat, mean)
group_mean <- group_mean[,c("Petal.Length", "Petal.Width")] # spiceiesのカラムを削除
group_mean

x_diff <- as.numeric(group_mean[1,]-group_mean[2,]) #群平均の引き算

sb <- x_diff %*% t(x_diff)
sb


# 群内分散の計算
Sm <- cov(dat[dat$Species=="versicolor",c("Petal.Length", "Petal.Width")])
Sp <- cov(dat[dat$Species=="virginica",c("Petal.Length", "Petal.Width")])

SW <- 1/(100-2)*((50-1)*Sm+(50-1)*Sp)
SW

# w1、w2を求める
# SW^(-1)SBの固有ベクトル
eigen(solve(SW) %*% sb) # solveは逆行列を求める関数



# 非線形判別分析
library(MASS)

result <- qda(Species~Petal.Length + Petal.Width, data = dat)
result

#予測をしてみる
pred <- predict(result, dat[1:10, c("Petal.Length", "Petal.Width")])
pred
