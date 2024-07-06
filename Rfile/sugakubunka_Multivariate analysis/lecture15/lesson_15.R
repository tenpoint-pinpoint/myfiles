# ランダムフォレストを実行するためのパッケージ
# install.packages("ranger")  #--> dependencies = TRUEでrangerを動かすために必要なライブラリをインストールする
library(ranger)

# データの作成
dat <- read.csv("./data/split_demo.csv", fileEncoding = "utf-8")
dat$label <- as.factor(dat$label)
str(dat)


plot(dat$x, dat$y, col=dat$label)

#ランダムフォレストの実行
result <- ranger(formula = label~x+y,
                 data = dat,
                 mtry = 1,#------------------> バギングに追加した１工夫の計算
                 max.depth = 2, #------------> 決定木１つ１つのパラメータ
                 min.node.size = 20, #-------> ノードを分割する最小の値
                 write.forest = TRUE, #------> 後で予測を行う場合にはTRUEにする。
                 importance = "impurity", #--> 変数貢献度を計算する指標。ここでは不純度を下げる＝貢献
                 classification = TRUE) #----> 回帰か分類かを指定する。
result


# 決定木分析の不安定性
library(MASS)
library(ggplot2)

# 乱数によるデータ作成を行う関数

sample_demo <- function(){
  A1 <- mvrnorm(n=25, mu=c(1,1), Sigma = 0.25*diag(2))  # -->共分散行列を作成してる
  A2 <- mvrnorm(n=25, mu=c(-1,-1), Sigma = 0.25*diag(2)) 
  A3 <- mvrnorm(n=25, mu=c(-1,1), Sigma = 0.25*diag(2)) 
  B1 <- mvrnorm(n=25, mu=c(1,-1), Sigma = 0.25*diag(2)) 
  
  X <- rbind(A1, A2, A3, B1)
  label <- rep(c(1,2), c(75, 25))#----------------------------> 25個づつサンプルデータを作っている、最後の１つだけラベルを変えたい
  dat <- data.frame(x=X[,1], y=X[,2], label=label)
  return(dat)
}
dat <- sample_demo()
plot(dat$x, dat$y, col=dat$label)


# 決定木でやってみる
library(rpart)
library(partykit)

# 以下の繰り返すと毎回違う結果が出てくる --> 全く異なる見た目の木が作られる事がある、ノイズが要因
dat <- sample_demo()
dat$label <- as.factor(dat$label)

result <- rpart(formula = label~x+y,
                data = dat,
                method = "class",
                control = rpart.control(maxdepth = 3, minsplit = 10, minbucket = 3))
plot(as.party(result))
# ただし、この性質があるからこそ決定木をたくさん作る事が解決方法だという事が見えてくる
# たくさん作ると、作られる決定木に多様性が生まれる事が期待できる
# 最終的に多数決がしたい。せっかく多数決するなら多様性があることはメリットになる
# 弱学習器を決定木で作るのはこれが理由！
# 一時流行った：今は勾配ブースティングという手法がブーム


# ランダムフォレストの別パッケージ
install.packages("randomForest")
library(randomForest)

dat <- sample_demo()
dat$label <- as.factor(dat$label)
randomForest(label~x+y, data=dat, ntrees=500, mtry=1)



# ブートストラップをやってみる
demo <- data.frame(x = 2*(1:5), y = 3*(1:5))
demo

idx_sample <- sample(x = 1:5, 
                     size = 5, #---------> データのサイズに合わせる
                     replace = TRUE) #---> 復元抽出
demo_bootstrap <- demo[idx_sample, ]
demo_bootstrap
