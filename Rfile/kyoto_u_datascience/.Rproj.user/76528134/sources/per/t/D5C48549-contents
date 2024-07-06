########## Chapter 6 ##########

# サンプルサイズ，平均値，標準偏差を指定してデータを作成する関数
gendat <- function(n, mu = 0, sigma = 1)
{
  x <- rnorm(n)
  return((x - mean(x)) / sd(x) * sigma + mu)
}
# データセットAの作成（サンプルサイズ，平均値，標準偏差の順）
a <- gendat(50, 30.00, 10.00)
b <- gendat(50, 32.00, 10.00)
# データセットBの作成（サンプルサイズ，平均値，標準偏差の順）
x <- gendat(500, 30.00, 10.00)
y <- gendat(500, 32.00, 10.00)

# t検定（データセットA）
t.test(a, b)
# t検定（データセットB）
t.test(x, y)

# 描画のためのデータを用意（因子型に変換）
score.A <- c(a, b)
group.A <- factor(c(rep("1 (n = 50)", length(a)), rep("2 (n = 50)", length(b)))) 
score.B <- c(x, y)
group.B <- factor(c(rep("1 (n = 500)", length(x)), rep("2 (n = 500)", length(y))))
# グラフを2つ並べて表示する設定
par(mfrow = c(1, 2))
# パッケージの読み込み
library("beeswarm")
# 箱ひげ図と蜂群図の描画
boxplot(score.A ~ group.A, ylim = c(0, 70), main = "データセットA", xlab = "指導法", ylab = "score")
beeswarm(score.A ~ group.A, ylim = c(0, 70), pch = 16, cex = 0.5, add = TRUE)
boxplot(score.B ~ group.B, ylim = c(0, 70), main = "データセットB", xlab = "指導法", ylab = "score")
beeswarm(score.B ~ group.B, ylim = c(0, 70), pch = 16, cex = 0.5, add = TRUE)

# パッケージのインストール（初回のみ）
install.packages("compute.es", dependencies = TRUE)
# パッケージの読み込み
library("compute.es")
# データセットAの効果量算出
mes(mean(a), mean(b), sd(a), sd(b), n.1 = 50, n.2 = 50)
# データセットBの効果量算出
mes(mean(x), mean(y), sd(x), sd(y), n.1 = 500, n.2 = 500)

# 第4章の対応のないt検定のデータから効果量を計算
mes(60.24, 72.27, 15.81, 16.11, 33, 37)

# 第4章の対応のあるt検定のデータから効果量を計算
# 対応なしの場合のdの計算
res <- mes(67.33, 74.43, 9.66, 8.98, 30, 30)

# 式（6.2）を使用
# 事前テストと事後テストの相関係数は0.6487101
# res[, "d"]は効果量d
d.val <- res[, "d"] / sqrt(2 * (1 - 0.6487101))
# 95%信頼区間の計算
library("psych")
d.ci(d.val, n1 = 30)

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch6-1.csvを選択
dat <- read.csv(file.choose(), header = TRUE)
# パッケージのインストール（初回のみ）
install.packages("coin", dependencies = TRUE)
# パッケージの読み込み
library("coin")
# Mann?WhitneyのU検定
res <- wilcox_test(dat$score ~ factor(dat$class), distribution = "exact")
res
# res@statistic@teststatisticは，上の結果のZ値と同じ
r <- abs(res@statistic@teststatistic) / sqrt(length(dat$score))
r
# 効果量rの95%信頼区間の算出
library("psych")
r.con(r, length(dat$score), p = .95, twotailed = TRUE)

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch6-2.csvを選択
dat.2 <- read.csv(file.choose(), header = TRUE)
# Wilcoxonの符号付順位和検定
# パッケージのインストール（初回のみ）
install.packages("exactRankTests", dependencies = TRUE)
# パッケージの読み込み
library("exactRankTests")
res.2 <- wilcox.exact(dat.2$pre, dat.2$post, paired = TRUE)
res.2
# 効果量rの計算
z <- qnorm(1 - (res.2$p.value / 2))
r <- z / sqrt(length(dat.2$pre) * 2)
r
# 効果量rの95%信頼区間の算出
r.con(r, length(dat.2$pre * 2), p =.95, twotailed = TRUE)

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch6-3.csvを選択
dat.3 <- read.csv(file.choose(), header = TRUE)
# Kruskal-Wallis検定
res.3 <- kruskal.test(dat.3$score ~ factor(dat.3$class))
res.3
# 効果量rの計算
z.2 <- qnorm(1 - (res.3$p.value / 2))
r.2 <- abs(z.2) / sqrt(nrow(dat.3))
r.2
# 効果量rの95%信頼区間の算出
r.con(r.2, nrow(dat.3), p =.95, twotailed = TRUE)
# 多重比較
# 引数p.adjで"bonferroni"を指定すれば，Bonferroniの方法
pairwise.wilcox.test(dat.3[, 2], dat.3[, 1], p.adj = "holm", exact = FALSE, correct = FALSE)

#演習レッスン_カイ二乗検定
z<- c(9,40,11,17,23)
names(z) <- c("大反対","反対","中立","賛成","大賛成")
result <-chisq.test(z)
result

z2 <- matrix(c(3,12,12,3,9,16,8,17),
             ncol = 2, nrow = 4, byrow = T)
rownames(z2) <- c("国語","算数","社会","理科")
colnames(z2) <- c("男児","女児")


z2

result2 <-chisq.test(z2)
result2

#標準化残差
result$residuals

#調整済み残差
result$stdres

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch6-4.csvを選択
dat.4 <- read.csv(file.choose(), header = TRUE)
# Freedman検定（データを行列に変換）
res.4 <- friedman.test(as.matrix(dat.4))
res.4
# 効果量rの計算
z.3 <- qnorm(1 - (res.4$p.value / 2))
r.3 <- abs(z.3) / sqrt(nrow(dat.4))
r.3
# 効果量rの95%信頼区間の算出
r.con(r.3, nrow(dat.4), p =.95, twotailed = TRUE)
# 型の変更
dat.5 <- stack(dat.4) 
x <- dat.5[, 1]
y <- dat.5[, 2]
# 多重比較（引数pairedでTRUEを指定）
pairwise.wilcox.test(x, y, p.adj = "holm", exact = FALSE, paired = TRUE, correct = FALSE)

