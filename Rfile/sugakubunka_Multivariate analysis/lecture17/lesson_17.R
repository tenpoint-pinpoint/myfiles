# install.packages("devtools")
# devtools::install_github("bips-hb/neuralnet")
library(neuralnet)

train <- read.csv("./data/xor_demo_train.csv", fileEncoding = "utf-8")
train$y <- as.factor(train$y)

head(x=train, n=5)
plot(train$x1, train$x2, col=train$y)
# 線形な手法では分類無理そう,,,,,

nnet <- neuralnet(formula = y ~ x1 + x2,
                  data = train,
                  # アーキテクチャの設計
                  hidden = 5,
                  linear.output = FALSE, # act.fctとoutput.act.fctを使用するため
                  act.fct = "relu",
                  output.act.fct = "logistic",
                  # 学習アルゴリズムの設計
                  algorithm = "backprop",
                  err.fct = "ce",          # 誤差の設定、二乗誤差ならsse
                  stepmax = 1e+04,
                  threshold = 0.5,
                  learningrate = 0.0005)

plot(nnet)


# メッシュグリッドの作成
xx1 <- seq(-3, 3, 0.05)
xx2 <- seq(-3, 3, 0.05)
meshgrid <- expand.grid(xx1, xx2)
names(meshgrid) <- c("x1", "x2")
head(meshgrid)

# 決定関数の計算
pred <- predict(nnet, meshgrid)
contour(xx1, xx2, array(pred, dim = c(length(xx1), length(xx2))))


######################################################################
# 予測制度の確認、過剰適合、過少適合

test <- read.csv("./data/xor_demo_test.csv", fileEncoding = "utf-8")
test$y <- as.factor(test$y)

pred_test_proda <- predict(nnet, test)
head(pred_test_proda)

# テストデータで混同行列
pred_test_proda <- predict(nnet, test)
pred_test <- apply(pred_test_proda, 1, which.max)
table(test$y, pred_test)

# 訓練データで混同行列
pred_train_proda <- predict(nnet, train)
pred_train <- apply(pred_train_proda, 1, which.max)
table(train$y, pred_train)
