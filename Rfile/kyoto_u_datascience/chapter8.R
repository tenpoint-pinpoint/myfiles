########## Chapter 8 ##########

# CSVファイルの読み込み
# data_ch8-1.csvを選択
dat <- read.csv(file.choose(), header = TRUE, row.names = 1)
# 欠損値（NA）の有無を確認
subset(dat, complete.cases(dat) == FALSE)
# 中間（1列目）と期末（2列目）の成績を抽出し，NAを含む行を削除
dat.2 <- na.omit(dat[, 1 : 2])
# 中間と期末の成績を抽出し，NAを含む行を削除したデータの冒頭を確認
head(dat.2)

# Mahalanobisの距離
d <- mahalanobis(dat.2, apply(dat.2, 2, mean), cov(dat.2))
# データの行数と列数の計算
n <- nrow(dat.2)
v <- ncol(dat.2)
# 外れ値の検出
out <- n * (n - v) / ((n ^ 2 - 1) * v) * d > qf(0.9, n, v)
# 散布図に外れ値を表示
plot(dat.2, pch = ifelse(out, 16, 21), xlim = c(0, 100), ylim = c(0, 100), xlab = "中間試験", ylab = "期末試験")
# 外れ値の行番号とラベルを表示
text(dat.2[out, ] - 3, labels = paste(which(out), ":", rownames(dat.2)[out]))
# 外れ値の除外
dat.3 <- dat.2[-which(out == TRUE), ]

# 単回帰モデル（結果変数 ~ 予測変数）
model.1 <- lm(end ~ mid, data = dat.3)
summary(model.1)

# 回帰直線の可視化
plot(dat.3, xlim = c(0, 100), ylim = c(0, 100), xlab = "中間試験", ylab = "期末試験")
abline(model.1)

##標準化解をみたい
dat.3z <- scale(dat.3)
dat.3z <- data.frame(dat.3z)
attach(dat.3z)

model.lz <- lm(end~mid, data = dat.3z)
summary(model.lz)
cor(end, mid) #標準化すると相関係数と一致する

# 傾きと切片の95%信頼区間
confint(model.1, level = 0.95)
# 新しいデータ（0点から100点まで1点刻み）を用意
new <- data.frame(mid = seq(0, 100, 1))
# 95%信頼区間の可視化
confidence <- predict(model.1, newdata = new, interval = 'confidence', level = 0.95)
lines(new$mid, confidence[, 2], lty = 3)
lines(new$mid, confidence[, 3], lty = 3)

# 中間試験が60点の場合の期末試験の点数の予測
pred <- predict(model.1, newdata = new, interval = 'prediction', level = 0.95)
pred[60, ]
# 予測区間の可視化
plot(dat.3, xlim = c(0, 100), ylim = c(0, 100), xlab = "中間試験", ylab = "期末試験")
abline(model.1)
lines(new$mid, pred[, 2], lty = 2)
lines(new$mid, pred[, 3], lty = 2)

# 残差の等分散性と正規性のプロット
par(mfcol = c(1, 2))
plot(model.1, which = c(1, 2))

# 分析データの準備
dat.4 <- na.omit(dat)
# パッケージのインストール（初回のみ）
install.packages("pequod", dependencies = TRUE)
# パッケージの読み込み
library("pequod")

# 外れ値の除外
d.2 <- mahalanobis(dat.4, apply(dat.4, 2, mean), cov(dat.4))
n.2 <- nrow(dat.4)
v.2 <- ncol(dat.4)
out.2 <- n.2 * (n.2 - v.2) / ((n.2 ^ 2 - 1) * v.2) * d.2 > qf(0.9, n.2, v.2)
dat.4[out.2, ]
dat.5 <- dat.4[-which(out.2 == TRUE), ]

# 重回帰モデル
model.2 <- lmres(end ~ mid + quiz, data = dat.5)
summary(model.2)

# 回帰平面の作成
install.packages("scatterplot3d", dependencies = TRUE)
library(scatterplot3d)
plot3d <- scatterplot3d(dat.4$mid, dat.4$quiz, dat.4$end, type = "n", 
                        xlim = c(0, 100), ylim = c(0, 50), zlim = c(0, 100), angle = 25,
                        xlab = "?中間試験", ylab = "?期末試験", zlab = "?小テスト")
plot3d$plane3d(lm(end ~ mid + quiz, data = dat.4),
               lty.box = "solid", col = "grey", draw_polygon = TRUE, draw_lines = TRUE, 
               polygon_args = list(col = rgb(0.97, 0.97, 0.97, 0.97)))
orig <- plot3d$xyz.convert(dat.4$mid, dat.4$quiz, dat.4$end)
plane <- plot3d$xyz.convert(dat.4$mid, dat.4$quiz, fitted(lm(end ~ mid + quiz, data = dat.4)))
i.negpos <- 1 + (resid(lm(end ~ mid + quiz, data = dat.4)) > 0)
segments(orig$x, orig$y, plane$x, plane$y,
         lty = c(2, 1)[i.negpos])
wh1 <- resid(lm(end ~ mid + quiz, data = dat.4)) > 0
wh2 <- resid(lm(end ~ mid + quiz, data = dat.4)) < 0
segments(orig$x[wh1], orig$y[wh1], plane$x[wh1], plane$y[wh1], lty = 1)
plot3d$points3d(dat.4$mid[wh1], dat.4$quiz[wh1], dat.4$end[wh1], pch = 19)
plot3d$points3d(dat.4$mid[wh2], dat.4$quiz[wh2], dat.4$end[wh2], pch = 21)

# 交互作用の検討
model.3 <- lmres(end ~ mid + quiz + mid : quiz, centered = c("mid", "quiz"), data = dat.5)
summary(model.3)

# 単純傾斜の検定
eff.1 <- simpleSlope(model.3, pred = "mid", mod1 = "quiz")
summary(eff.1)
eff.2 <- simpleSlope(model.3, pred = "quiz", mod1 = "mid")
summary(eff.2)

# 単純傾斜のプロット
PlotSlope(eff.1)
PlotSlope(eff.2)

# 検定によるモデル選択
anova(model.1, model.2$Stepfin, model.3$Stepfin)

# 情報量基準によるモデル選択
AIC(model.1, model.2$Stepfin, model.3$Stepfin)

# 分析データの読み込み
# data_ch8-2.csvを選択
dat <- read.csv(file.choose(), row.names = 1, header = TRUE)
# 読み込んだデータの冒頭を確認
head(dat)
# 散布図の作成と回帰直線の描画
plot(dat$Hour, dat$TOEIC)
linear.model <- lm(TOEIC ~ Hour, data = dat)
abline(linear.model)

# 個々の学習者の情報を表示した散布図
plot(dat$Hour, dat$TOEIC, pch = dat$School)

# 学校ごとに回帰分析
linear.model.2 <- lm(TOEIC[1 : 40] ~ Hour[1 : 40], data = dat)
linear.model.3 <- lm(TOEIC[41 : 80] ~ Hour[41 : 80], data = dat)
linear.model.4 <- lm(TOEIC[81 : 120] ~ Hour[81 : 120], data = dat)
# 回帰直線の表示
abline(linear.model.2, lty = 1)
abline(linear.model.3, lty = 2)
abline(linear.model.4, lty = 3)

# パッケージのインストール（初回のみ）
install.packages("ICC", dependencies = TRUE)
# パッケージの読み込み
library("ICC")
# 級内相関
ICCest(TOEIC, Hour, data = dat)

# パッケージのインストール（初回のみ）
install.packages("lme4", dependencies = TRUE)
# パッケージの読み込み
library("lme4")
# マルチレベル分析
multilevel.model <- lmer(TOEIC ~ Hour + (Hour | School), data = dat)
# マルチレベル分析の結果を確認
summary(multilevel.model)

# マルチレベル分析から得られた係数
fixef(multilevel.model)
# 通常の回帰分析から得られた係数
linear.model$coefficients
# 散布図上で比較
plot(dat$Hour, dat$TOEIC, pch = dat$School)
abline(fixef(multilevel.model), lty = 1)
abline(linear.model$coefficients, lty = 2)

# 勉強時間の影響を除いた学校間の点数比較
multilevel.model.2 <- lmer(TOEIC ~ Hour + (1 | School), data = dat)
summary(multilevel.model.2)