# サポートベクトルマシン
# 回帰も分類もできる、でもだいたい分類で学ぶ
# というわけで分類を学ぶ

train <- read.csv("./data/xor_demo_train.csv", 
                  fileEncoding = "utf-8")

head(train)
# yを識別子にするためにas.factorで論理演算子にしておく
train$y <- as.factor(train$y)
str(train)

plot(train$x1, train$x2, col=train$y)
# 大雑把には黒：右上＆左下、赤：左上＆右下。乱数の影響で混じっている（ノイズあり）


# 前処理をしておく必要がある
# SVM（サポートベクトルマシン）は大きなスケールの変数があると、その変数によって予測を決めてしまう傾向がある
# 事前に正規化・標準化を行う
# min-max正規化を行う
# 最小値が０、最大値が１になるように正規化を行う

summary(train)
# まず最大値と最小値を抜き出す
x1_min <- min(train$x1)
x1_max <- max(train$x1)
x2_min <- min(train$x2)
x2_max <- max(train$x2)

# ここで計算したmaxとminはずっと使う
# 手元にあるデータから予測する計算をすべき
# つまりminやmaxの値を変更することは、予測の方法を変えていることになる
# 例えば新しいデータが１つしかなかったら、minもmaxも取得できない

# 以下の計算処理を行う
x1 <- (train$x1 - x1_min) / (x1_max - x1_min)
x2 <- (train$x2 - x2_min) / (x2_max - x2_min)

train_scaled <- data.frame(x1 = x1, x2 = x2, y = train$y)
summary(train_scaled)


# SVMの実行
# install.packages("kernlab")
library(kernlab)

# ハイパーパラメータ：分析者が指定する、計算に必要がパラメータ。

result <- ksvm(x = y ~ x1 + x2,
               data = train_scaled,#-------> 正規化したデータ
               type = "C-svc",#------------> 分類の方法を指定している 
               C = 1.0,#-------------------> 決定境界の複雑さが変化するハイパーパラメータ
               kernel = "rbfdot",#---------> 決定境界の形に関わる。カーネルの種類を指定。
               kpar = list(sigma = 1.0)#---> カーネルに付随するパラメータ。rbfdotのハイパーパラメータ
               )
result # これで計算完了

# 結果の確認

# １、散布図上に点を稠密に並べる
# ２、

# メッシュグリッドを作成する：
xx1 <- seq(-3, 3, 0.05)
xx2 <- seq(-3, 3, 0.05)

data.frame(x1=xx1, x2=xx2) #------------> これでは不十分
meshgrid <- expand.grid(xx1, xx2) #-----> こちらだと必要なデータになる

names(meshgrid) <- c("x1", "x2")  #-----> 変数に名前をつけた
head(meshgrid)

# meshgridも正規化
x1 <- (meshgrid$x1 - x1_min) / (x1_max - x1_min)
x2 <- (meshgrid$x2 - x2_min) / (x2_max - x2_min)

meshgrid_scaled <- data.frame(x1=x1, x2=x2)
head(meshgrid_scaled)

# 各店に対して、予測を行う
pred <- predict(result, 
                meshgrid_scaled, 
                type = "decision")#---->type = "response"だと、これは予測ラベルを返す


# スコアの等高線を書く関数：予測した値が同じ点を結んで等高線を書く
contour(xx1, xx2, array(pred, dim = c(length(xx1), length(xx2))))
       


######################################################################################################
######################################################################################################
# もう一方のデータで計算
test <- read.csv("./data/xor_demo_test.csv", 
                  fileEncoding = "utf-8")

head(test)
# yを識別子にするためにas.factorで論理演算子にしておく
test$y <- as.factor(test$y)
str(test)

plot(test$x1, test$x2, col=test$y)
# 大雑把には黒：右上＆左下、赤：左上＆右下。乱数の影響で混じっている（ノイズあり）

# 以下の計算処理を行う
x1 <- (test$x1 - x1_min) / (x1_max - x1_min)
x2 <- (test$x2 - x2_min) / (x2_max - x2_min)

test_scaled <- data.frame(x1 = x1, x2 = x2, y = test$y)
summary(test_scaled)

pred_test <- predict(result, test_scaled, type = "response")
pred_test

# トレーニングデータで予測したもので、テストデータを予測し、正解と比較してみる
# この正解率が高いと予測に使えそうな感じがする
table(test$y, pred_test) #----> テストデータは元のデータをトレーニングデータとテストデータに分けたもの
##
#pred_test
#     1  2
# 1  38 12  
# 2   7 43

# トレーニングデータでも確認してみる。これが高いだけでは意味がない
pred_train <- predict(result, train_scaled, type = "response")
table(train$y, pred_train)