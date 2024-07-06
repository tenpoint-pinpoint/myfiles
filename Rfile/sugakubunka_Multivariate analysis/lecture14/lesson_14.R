data(iris)

dat <- iris[iris$Species %in% c('versicolor', "virginica"),#-----> どこの行を抜くか（TRUEの箇所だけ残す）
            c("Petal.Length", "Petal.Width", "Species")]#--------> どこの列を抜くか

iris$Species %in% c('versicolor', "virginica")#------------------> boolianが返ってくる
str(dat)#--------------------------------------------------------> setosaが残っている
dat$Species <- factor(dat$Species,levels = c("versicolor","virginica")) 



# 非線形判別分析_qda関数の中身


# versicolorを取り出す
dat_versi <- dat[dat$Species=="versicolor", c("Petal.Length", "Petal.Width")]
# 平均ベクトル
xbar_versi <- apply(dat_versi, MARGIN = 2, FUN = mean)
# 共分散行列
cov_versi <- cov(dat_versi)

# virginicaを取り出す
dat_virgi <- dat[dat$Species=="virginica", c("Petal.Length", "Petal.Width")]
# 平均ベクトル
xbar_virgi <- apply(dat_virgi, MARGIN = 2, FUN = mean)
# 共分散行列
cov_virgi <- cov(dat_virgi)



# １つデータをとってくる
x <- dat[1, c("Petal.Length", "Petal.Width")]

# マハラノビス距離を計算する関数
d_versi <- mahalanobis(x=x, center = xbar_versi, cov = cov_versi)
d_virgi <- mahalanobis(x=x, center = xbar_virgi, cov = cov_virgi)

-d_versi + d_virgi
# マイナスのでvirgicnicaと判定



# 線形判別分析との関連
# 線形判別分析はMahalanobis距離による判別分析の特殊な場合と考えることができる
# 線形判別分析では２つの群の分散共分散行列が等しいことを仮定している
# 分散の推定をpoolした分散（=群内分散）で行って、Mahalanobis距離の計算をする
# つまり線形判別分析がもつ仮定を採用してMahalanobis距離を計算している

# この結果は線形判別分析の結果と一致する

# マイナスの方（versicolor）
Sm <- cov(dat[dat$Species=="versicolor", c("Petal.Length", "Petal.Width")])
# プラスの方（virginica）
Sp <- cov(dat[dat$Species=="virginica", c("Petal.Length", "Petal.Width")])

# poolした共分散行列
SW <- 1/(100-2)*((50-1)*Sm + (50-1)*Sp)

#判別分析
coef <- t(xbar_versi - xbar_virgi) %*% solve(SW)
coef  # これはlda関数による係数に比例している　： 比例している = 判別境界は変わらない
