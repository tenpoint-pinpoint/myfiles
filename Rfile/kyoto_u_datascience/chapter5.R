########## Chapter 5 ##########
#分散分析

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch5_1.csvを選択
dat <- read.csv(file.choose(), header = TRUE)
# 読み込んだデータの冒頭の確認
head(dat)
# 行数と列数の確認
dim(dat)
# クラス別の学習者数
table(dat$class)
# クラスごとの記述統計量の計算
library("psych")
describeBy(dat$score, dat$class)
# 等分散性の検定
library("car")
leveneTest(dat$score, dat$class, center = mean)

# パッケージの読み込み
library("beeswarm")
# 蜂群図の描画
boxplot(dat$score ~ dat$class, col = "grey", ylim = c(0, 100), main = "3クラスの比較", xlab = "class", ylab = "score")
beeswarm(dat$score ~ dat$class, ylim = c(0, 100), pch = 16, add = TRUE)

# 繰り返しのない一元配置分散分析
# ANOVA君のソースを読み込んでから実行
source("anovakun_485.txt")# これは都度読み込まなければならない
anovakun(dat[, -1], "As", 3, holm = TRUE, eta = TRUE)

# eta^2 = 2789/35160 (興味のある変動の不偏分散／全体の不偏分散)


# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch5_2.csvを選択
dat.2 <- read.csv(file.choose(), header = TRUE)
# 読み込んだデータの冒頭の確認
head(dat.2)
# 行数と列数の確認
dim(dat.2)
# クラス別の学習者数
table(dat.2$class)

# 二元配置分散分析
anovakun(dat.2[, -1], "AsB", 2, 3, auto = TRUE, holm = TRUE, eta = TRUE)
#autoは反復測定データ

# スタック形式に変更
x <- stack(dat.2[, 3 : 5])
# データフレームの作成
y <- data.frame(dat.2$class, x)
# 因子の型に変更
y$dat.2.class <- factor(y$dat.2.class)
# 水準の順序を指定
y$ind <- factor(y$ind, levels = c("pre", "post", "delayed"))
# データフレームの列名を変更
names(y) <- c("class", "score", "test")
# 交互作用確認プロット
interaction.plot(y$test, y$class, y$score, type = "b", pch = c(1, 2), xlab = "Test", ylab = "Score", trace.label = "Class")
