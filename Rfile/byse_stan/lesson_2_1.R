library(tidyverse)
matrix_1 <- matrix(data = 1:100, nrow = 4, ncol = 25)
matrix_2 <- matrix(data = 1:100, nrow = 4, ncol = 25, byrow = FALSE)
matrix_1
matrix_2

matrix_1[1,2]
matrix_1[,2]

# 時系列データの作り方
ts <- data.frame(data = 1:24) %>% 
  ts(.
     , start = c(2021,1) # 開始月を設定
     , frequency = 12)   # 1年におけるデータの数  
ts
# 開始地点やデータ数を変えてみる
ts_1 <- data.frame(data = 1:36) %>% 
  ts(.
     , start = c(2021,2) # 開始月を設定
     , frequency = 4)   # 1年におけるデータの数  
ts_1

# 乱数の生成
rnorm(n = 1,mean = 0,sd = 1)
# 乱数の固定
set.seed(1)
rnorm(n = 1,mean = 0,sd = 1)
rnorm(n = 1,mean = 0,sd = 1)

for(i in 1:3){
  x <- i*3
  print(x)
}

# stanでよく使うforループ
result_vec_1 <- c(0,0,0)
set.seed(1)
for (i in 1:3){
  result_vec_1[i] <- rnorm(n = 1,mean = 0,sd = 1)
}
result_vec_1

# 平均値を変化させながらベクトル化
result_vec_2 <- c(0,0,0)
mean <- c(0,10,-5)
set.seed(1)
for (i in 1:3){
  result_vec_2[i] <- rnorm(n = 1,mean = mean[i] ,sd = 1)
}
result_vec_2



