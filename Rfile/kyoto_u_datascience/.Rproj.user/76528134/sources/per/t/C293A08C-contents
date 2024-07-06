######### Chapter 9 ##########

# CSVファイルの読み込み
# data_ch9-1.csvを選択
dat <- read.csv(file.choose(), header = TRUE)
# psychパッケージの読み込み
library("psych")
# 記述統計の確認 (1列目のstudentの列は分析に含めない)
describe(dat[, -1])

# KMOによるサンプリングの適切性指標を確認
KMO(dat[, -1])

# Bartlettの球面性検定
cortest.bartlett(cor(dat[, -1]), n = nrow(dat))

# 観測変数間の相関係数を確認する
pairs.panels(dat[, -1], lm = TRUE, density = FALSE)

# 因子数の決定（1）
# 固有値の計算
r.eigen <- eigen(cor(dat[, -1]))
print(r.eigen$values, digit = 2)

# スクリープロットの作成
plot(r.eigen$values, xlim = c(1, 9),  xaxp = c(1, 9, 8), type = "b", xlab = "因子の番号", ylab = "固有値")
abline(h = 1)

# 因子数の決定（2）
# MAP基準の確認
VSS(dat[, -1], n = nrow(dat), fm = "ml")

# 因子数の決定（3）
# 平行分析
fa.parallel(cor(dat[, -1]), fm = "ml", n.obs = nrow(dat), n.iter = 100)

# 最尤法・オブリミン回転による因子分析
r.fa <- fa(dat[, -1], nfactors = 3, fm = "ml", rotate = "oblimin")
print(r.fa, sort = TRUE, digits = 2)

# lavaanパッケージの読み込み
library("lavaan")
# 3因子構造のモデルを記述（''を忘れやすいので注意）
model.1 <- '
  LV.1 =~ item8 + item9 + item4
  LV.2 =~ item3 + item7
  LV.3 =~ item2 + item1 + item6 '
# 確認的因子分析
fit.1 <- cfa(model.1, data = dat, estimator = "ML")
summary(fit.1, fit.measures = TRUE, standardized = TRUE)

# 適合度指標の検討
fitMeasures(fit.1)

# 2因子構造のモデルを記述
model.2 <- '
  LV.1 =~ item8 + item9 + item4
  LV.2 =~ item2 + item1 + item6 '
# 確認的因子分析
fit.2 <- cfa(model.2, data = dat, estimator = "ML")
summary(fit.2, fit.measures = TRUE, standardized = TRUE)

# model1とmodel2の比較
anova(fit.1, fit.2)

# 因子得点の計算
lavPredict(fit.2)

# 尺度得点の計算
LV.1 <- rowMeans(dat[, c(9, 10, 5)])
LV.2 <- rowMeans(dat[, c(3, 2, 7)])
cbind(LV.1, LV.2)

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch9-2.csvを選択
dat.A <- read.csv(file.choose(), header = TRUE)
# data_ch9-3.csvを選択
dat.B <- read.csv(file.choose(), header = TRUE)

# パッケージのインストール（初回のみ）
install.packages("ltm", dependencies = TRUE)
# パッケージの読み込み
library("ltm") 
# 1パラメター（Rasch）モデルで分析
test.A <- rasch(dat.A[, -1])
test.B <- rasch(dat.B[, -1])

# 2つの図を1つにまとめて表示する設定
par(mfrow = c(1, 2))
# 項目特性曲線の作成
plot(test.A, type = "ICC", items = c(23, 24, 29))
# 項目情報量曲線の作成
plot(test.A, type = "IIC", items = c(23, 24, 29))

# 能力推定値の計算
theta.A <- factor.scores.rasch(test.A, resp.pattern = dat.A[, -1])
theta.B <- factor.scores.rasch(test.B, resp.pattern = dat.B[, -1])
A.theta <- theta.A$score.dat$z1
B.theta <- theta.B$score.dat$z1
# 計算結果の冒頭を確認
head(A.theta)
head(B.theta)

# 図9.6
raw <- rowSums(dat.A[, -1])
cbind(raw, A.theta)
par(ps = 20, mai = c(1, 1, 1, 1), mfrow = c(1, 2))
hist(A.theta, main = "", xlab = "能力推定値", ylab = "度数")
abline(v = mean(A.theta), lty = 2)
plot(raw, A.theta, pch = 1, xlab = "素点", ylab = "能力推定値", cex = 1.5)
abline(lm(A.theta ~ raw), col = "black", lty = 2)

# 共通項目の難易度の平均値
(dffcltA.mean <- mean(test.A$coefficients[1:20, 1]))
(dffcltB.mean <- mean(test.B$coefficients[1:20, 1]))
# 変換式の作成
Intercept <- dffcltA.mean - dffcltB.mean
# テストB受験者の能力推定値をテストAに合わせる処理
B.adjusted <- B.theta + Intercept
# 等化前と等化後のテストＢ受験者の能力推定値の変化を箱ひげ図で可視化
boxplot(A.theta, B.theta, B.adjusted, names = c("テストA", "テストB（等化前）", "テストB（等化後）"), main = NA, xlab = NA, ylab = "能力推定値", col = "grey")

# CSVファイルの読み込み
# data_ch9-3.csvを選択
dat.C <- read.csv(file.choose(), header = TRUE)

# パッケージのインストール（初回のみ）
install.packages("mirt", dependencies = TRUE)
# パッケージの読み込み
library("mirt")
# 段階反応モデルで分析
test.C <- mirt(data = dat.C[, -1], model = 1, itemtype = "graded")
# 能力推定値の計算
C.theta <- fscores(test.C)
C.theta
# 項目特性曲線の作成
plot(test.C, type = 'trace', which.items = c(6, 10))



