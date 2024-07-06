dat <- read.csv("./data/dairy_products.csv",
                fileEncoding = "utf-8",
                row.names = "country")

head(dat)
str(dat)
summary(dat)

plot(dat)

# まず標準化
# scaleはデータフレームを正規化する関数。平均を０分散を１にする（＝標準化）
dat_scaleed <- scale(dat) 

# 標準化したデータをkmeanに与える
result <- kmeans(x = dat_scaleed, # xにデータセットを入れる
                 centers = 3,     # 何クラスタに分けたいか（基準となる中心の数）
                 iter.max = 100)  # 繰り返しの計算を何回やるか

# 各クラスタを解釈するヒント
result$centers  #各クラスタの平均値
result$cluster  #各データのクラスタを表示
plot(dat, col=result$cluster) #クラスターで色分け



# 乱数を作ってクラスタリングを理解する

library(MASS)

# 多変量正規分布に従う乱数を取得する
# 平均ベクトルと分散共分散行列を与える
# diag(c(1,1))で分散１で無相関の分散共分散行列

cluster_1 <- mvrnorm(n=100, 
                     mu=c(2, 2),
                     Sigma = diag(c(1, 1))                     )

cluster_2 <- mvrnorm(n=100, 
                     mu=c(-2, -2),
                     Sigma = diag(c(1, 1)))

X <- rbind(cluster_1,  cluster_2)

dat <- data.frame(x1 = X[,1], x2 = X[,2])
dat

plot(dat)


#----初期値を決める------------------------------------------------#

# 各データ点に対してランダムに１か２のクラスターを付与するためのベクトルを作る
y <- sample(x = c(1, 2),   # 1か２をとる
            size = 200,    # 200回抽出 = ランダムに200のデータ点に１か２を割付
            replace = TRUE)# 復元抽出
y

plot(dat, col=y)

nrow(dat[y==1,])
nrow(dat[y==2,])

#-- 赤チームの平均、黒チームの平均（＝セントロイド）を計算する-----#
center_1 <- colMeans(dat[y==1,]) # datのうちyが1の行だけ抜き出し、列方向に平均をとる
center_2 <- colMeans(dat[y==2,]) # datのうちyが2の行だけ抜き出し、列方向に平均をとる

center_1
center_2


#--------------- 各点からcenter_1までの距離を計算する------------------------------#

# sweep -- 各データから、最後の引数を引く、marginは引く方向を指定している(1:縦、2:横)
sweep(data.frame(A=c(1,2,3), B=c(10,3,20)),
      MARGIN = 1, c(5, 3, 1))
#---結果-------------------------------------#
#     A   B
# 1  -4   5
# 2  -3  -2
# 3  -2  15
#--------------------------------------------#

# 各点からcenter_1までの距離を算出
dist_1 <- rowSums(sweep(dat, 
                        MARGIN = 2, 
                        center_1)^2) # 平均を引いて、２乗している

# 各点からcenter_2までの距離を算出
dist_2 <- rowSums(sweep(dat, 
                        MARGIN = 2, 
                        center_2)^2) # 平均を引いて、２乗している

head(dist_1)
head(dist_2)
head(y)

# boolianに数字を足すと、数字になる。TRUEは1、FALSEは0
(dist_1 > dist_2) + 1

# 条件を元に1か2かを再振り分けしたい
# 1*1+2*0 = 1　もしくは　1*0+2*1 = 2　の計算結果を代入している
new_cluster_id <- 1*(dist_1 < dist_2) + 2*(dist_1 > dist_2)
plot(dat, col=new_cluster_id)


# 初期値は完全にランダムだとうまくいかない場合がある
cluster1 <- mvrnorm(n=100, mu=c(6, 10), Sigma=diag(c(2,2)))
cluster2 <- mvrnorm(n=100, mu=c(13, 10), Sigma=diag(c(2,2)))
cluster3 <- mvrnorm(n=100, mu=c(10, -10), Sigma=diag(c(2,2)))
cluster4 <- mvrnorm(n=100, mu=c(-10, -10), Sigma=diag(c(2,2)))
cluster5 <- mvrnorm(n=50, mu=c(-10, 10), Sigma=diag(c(1,1)))

X <- rbind(cluster1, cluster2, cluster3, cluster4, cluster5)

dat <- data.frame(x1=X[,1], x2=X[,2])
plot(dat)

result_1 <- kmeans(x=dat, centers = 5, iter.max = 100)
plot(dat, col=result_1$cluster)


## k-means++の実装　　　　　##後で実装してみる##
## 初期値を決める関数
init_kmeanspp <- function(x, centers)#------------------------------->  Xはデータ、centers = kの値（重心の数）
  {
  x <- as.matrix(x)#-------------------------------------------------> Xを行列に変換
  dist_centers <- matrix(Inf, nrow = nrow(x), ncol = centers-1)#-----> centerを計算する空箱を作っている（行数はdatの行数、列は残りの重心の数）
  center_pts <- sample(x=1:nrow(x), size=1)#-------------------------> 1~Xの行数（=Xの行数）からランダムに1つめの重心をとっている

  for(i in 1:(centers-1))#-------------------------------------------> 既に1つめの重心は決めているのでcenters-1となる
    {
# 重心からの距離で選択される確率を計算 -> 次の重心をランダムに選ぶ -> その重心からの距離で選ばれる確率を計算-> 
# これまで計算した重心からの距離でそれぞれ一番近い（＝確率の低い）値を選ぶ。 -> それによって次の重心をランダムに選択する 
    dist_centers[,i] <- rowSums(sweep(x, 2, x[center_pts[i],])^2)#---> 1つめの重心からの距離の２乗を足し、該当の列に格納する
    min_dist <- apply(dist_centers, MARGIN = 1, FUN = min)#----------> 行に対して最小の値を取得＝最も確率が低いもの
    prob <- min_dist/sum(min_dist)#----------------------------------> 1つのmin_distが選ばれる確率を計算する
    center_pts[i+1] <- sample(x=1:nrow(x), size = 1, prob = prob)#---> ２つめの列に選ばれる確率を格納する
    }
    return(center_pts)
  }

datmat <- as.matrix(dat)
dist_centers <- matrix(Inf, nrow = nrow(datmat), ncol = 4)
dist_centers
center_pts_te <- sample(x=1:nrow(dat), size = 1)
center_pts_te

dist_centers[,1] <- rowSums(sweep(datmat, 2, datmat[center_pts_te[1],])^2)
dist_centers[,2] <- rowSums(sweep(datmat, 2, datmat[center_pts_te[1],])^4)
head(dist_centers)
head(apply(dist_centers, MARGIN = 1, FUN = min))

min_te <- apply(dist_centers, MARGIN = 1, FUN = min)
head(min_te)
head(min_te/sum(min_te))

head(datmat)
head(sweep(datmat, 2, datmat[center_pts_te[1],])^2)

datmat[center_pts_te[1],]
center_pts_te[1]
center_pts_te

#---選ばれた純真の確認----------------------------------------------------------------
initial_centers <- init_kmeanspp(x = dat, center = 5)
plot(dat, xlim=c(-15, 15), ylim=c(-15, 15))
par(new = TRUE)
plot(dat[initial_centers,], xlim = c(-15, 15), ylim = c(-15, 15), col="red", pch=19)
#-------------------------------------------------------------------------------------

#---k-mean++で選ばれた初期値でk-mean--------------------------------------------------
result <- kmeans(dat, centers = dat[initial_centers,], iter.max = 100)
plot(dat, col = result$cluster)
#-------------------------------------------------------------------------------------


# 正規化の必要あり　： 距離依存であるため
# 値が大きい（or小さい）列があると、結果に影響しやすい



te_dist_centers <- matrix(Inf, nrow = 5, ncol = 5)
te_center_pts <- sample(x=1:5, size=1)

te_dist_centers[,1]
te_dist_centers[te_center_pts,]

te_center_pts

rowSums(sweep(x, 1, x[te_center_pts[2],])^2)

te_dist_centers[,1] <- rowSums(sweep(x, 2, x[te_center_pts[2],])^2)
te_dist_centers

u <- rowSums(matrix(1,2,3))
u
matrix(2,2,2)

sweep(matrix(1:4,2,2), 2, c(2,1))
matrix(1:4,2,2)



(1<2)+1
