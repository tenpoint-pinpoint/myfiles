library(MASS)

# 初期値がラン�?�?�?と�?まくいかな�?こと�?
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




## k-means++の実�?�?�?�?�?�?
## 初期値を決める関数
init_kmeanspp <- function(x, centers)#------------------------------->  Xは�?ータ、centers = kの値?���?��?の数?�?
{
  x <- as.matrix(x)#-------------------------------------------------> Xを行�?�にする
  dist_centers <- matrix(Inf, nrow = nrow(x), ncol = centers-1)#-----> centerを計算するハコを作って�?る（行数はdatの行数、�?��?�残りの重�?の数?�?
  center_pts <- sample(x=1:nrow(x), size=1)#-------------------------> 1~Xの行数?�?=Xの行数?��からラン�?�?に1つめ�?�重�?をとって�?�?
  
  for(i in 1:(centers-1))#-------------------------------------------> 既に1つめ�?�重�?は決めて�?る�?�でcenters-1ととしておく
  {
    dist_centers[,i] <- rowSums(sweep(x, 2, x[center_pts[i],])^2)#---> 1つめ�?�重�?からの距離の?��乗を足し�?�該当�?�列に格納す�?
    min_dist <- apply(dist_centers, MARGIN = 1, FUN = min)#----------> 行に対して�?小�?�値を取得＝最も確�?が低いも�?�
    prob <- min_dist/sum(min_dist)#----------------------------------> 1つのmin_distが選ばれる確�?を計算す�?
    center_pts[i+1] <- sample(x=1:nrow(x), size = 1, prob = prob)#---> ?��つめ�?�列に選ばれる確�?を�?�納す�?
  }
  return(center_pts)
}


#---選ばれた重�?の確�?----------------------------------------------------------------
initial_centers <- init_kmeanspp(x = dat, center = 5)
plot(dat, xlim=c(-15, 15), ylim=c(-15, 15))
par(new = TRUE)
plot(dat[initial_centers,], xlim = c(-15, 15), ylim = c(-15, 15), col="red", pch=19)
#-------------------------------------------------------------------------------------

#---k-mean++で選ばれた初期値でk-mean--------------------------------------------------
result <- kmeans(dat, centers = dat[initial_centers,], iter.max = 100)
plot(dat, col = result$cluster)

