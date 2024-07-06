library(MASS)

# 初期値がランダムだとうまくいかないことも
cluster1 <- mvrnorm(n=100, mu=c(6, 10), Sigma=diag(c(2,2)))
cluster2 <- mvrnorm(n=100, mu=c(13, 10), Sigma=diag(c(2,2)))
cluster3 <- mvrnorm(n=100, mu=c(5, -10), Sigma=diag(c(2,2)))
cluster4 <- mvrnorm(n=100, mu=c(-10, -10), Sigma=diag(c(2,2)))
cluster5 <- mvrnorm(n=50, mu=c(-10, 10), Sigma=diag(c(1,1)))

X <- rbind(cluster1, cluster2, cluster3, cluster4, cluster5)

dat <- data.frame(x1=X[,1], x2=X[,2])
plot(dat)

result_1 <- kmeans(x=dat, centers = 5, iter.max = 100)
plot(dat, col=result_1$cluster)




## k-means++の実装　　　　　
## 初期値を決める関数
init_kmeanspp <- function(x, centers)#------------------------------->  Xはデータ、centers = kの値（重心の数）
{
  x <- as.matrix(x)#-------------------------------------------------> Xを行列にする
  dist_centers <- matrix(Inf, nrow = nrow(x), ncol = centers-1)#-----> centerを計算するハコを作っている（行数はdatの行数、列は残りの重心の数）
  center_pts <- sample(x=1:nrow(x), size=1)#-------------------------> 1~Xの行数（=Xの行数）からランダムに1つめの重心をとっている
  
  for(i in 1:(centers-1))#-------------------------------------------> 既に1つめの重心は決めているのでcenters-1ととしておく
  {
    dist_centers[,i] <- rowSums(sweep(x, 2, x[center_pts[i],])^2)#---> 1つめの重心からの距離の２乗を足し、該当の列に格納する
    min_dist <- apply(dist_centers, MARGIN = 1, FUN = min)#----------> 行に対して最小の値を取得＝最も確率が低いもの
    prob <- min_dist/sum(min_dist)#----------------------------------> 1つのmin_distが選ばれる確率を計算する
    center_pts[i+1] <- sample(x=1:nrow(x), size = 1, prob = prob)#---> ２つめの列に選ばれる確率を格納する
  }
  return(center_pts)
}


#---選ばれた重心の確認----------------------------------------------------------------
initial_centers <- init_kmeanspp(x = dat, center = 5)
plot(dat, xlim=c(-15, 15), ylim=c(-15, 15))
par(new = TRUE)
plot(dat[initial_centers,], xlim = c(-15, 15), ylim = c(-15, 15), col="red", pch=19)
#-------------------------------------------------------------------------------------

#---k-mean++で選ばれた初期値でk-mean--------------------------------------------------
result <- kmeans(dat, centers = dat[initial_centers,], iter.max = 100)
plot(dat, col = result$cluster)

