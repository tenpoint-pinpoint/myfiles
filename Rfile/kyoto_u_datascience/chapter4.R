dat.2 <- read.csv("data_ch2.csv", header = TRUE)

dat.2 <- read.csv(file.choose(), header = TRUE)
head(dat.2)

#演習２回目

dat4 <- read.csv(file.choose(), header = TRUE)

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch4_1.csvを選択
dat4 <- read.csv(file.choose(), header = TRUE)
# 読み込んだデータの冒頭の確認
head(dat4)
# 行数と列数の確認
dim(dat4)
# 読み込んだ4列のデータのうちNAのある行を削除
dat4 <- na.omit(dat4)
# NAのある行を削除したデータの冒頭を確認
head(dat.2)
# NAのある行を削除したデータの行数と列数の確認
dim(dat4)
# クラス別の学習者数
table(dat4$class)
# クラス別の男女の学習者数
table(dat4$class, dat4$sex)

# パッケージの読み込み
library("lattice")
library("beeswarm")
install.packages("beeswarm")
# クラス別の得点分布の比較（ヒストグラム）
histogram(? score | class, data = dat.2)
# クラス別の得点分布の比較（箱ひげ図と蜂群図）
boxplot(dat.2$score ? dat.2$class, ylim = c(0, 100), main = "Result of the Exam", xlab = "class", ylab = "score")
beeswarm(dat.2$score ? dat.2$class, ylim = c(0, 100), pch = 16, add = TRUE)

# パッケージの読み込み
library("psych")
# クラスごとの記述統計量の計算
describeBy(dat4$score, dat4$class)

# 等分散性の検定
# パッケージのインストール（初回のみ）
install.packages("car", dependencies = TRUE)
# パッケージの読み込み
library("car")
leveneTest(dat4$score, dat4$class, center = mean)

# 独立したt検定 ※こちらを推奨
t.test(dat4$score ? dat4$class, var.equal=T)

# ウェルチのt検定
t.test(dat4$score ? dat4$class)

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch4_2.csvを選択
dat.3 <- read.csv(file.choose(), header = TRUE)
# 読み込んだデータの冒頭の確認
head(dat.3)

# 行数と列数の確認
dim(dat.3)
# preとpostの列に欠損値がないか確認（TRUEがあれば，欠損値がある）
table(is.na(dat.3[3 : 4]))
# 男女の学習者数
table(dat.3$sex)

# 事前・事後テストの結果を重ねたヒストグラム（col = rgbで半透明の指定）
hist(dat.3$pre, col = rgb(1, 0, 0, 0.5), xlim = c(30, 100), ylim = c(0, 15), main = "Overlapping Histogram", xlab = "score")
hist(dat.3$post, col = rgb(0, 0, 1, 0.5), add = TRUE)
# 箱ひげ図に個人の得点分布を重ねた蜂群図
score <- c(dat.3$pre, dat.3$post)
group <- factor(c(rep("pre", 30), rep("post", 30)), levels = c("pre","post"))
boxplot(score ? group, ylim = c(0, 100), main = "Result of the Pre-Post Test", xlab = "test", ylab = "score")
beeswarm(score ? group, ylim = c(0, 100), pch = 16, add = TRUE)

# 記述統計量の確認　
describe(dat.3[, 3 : 4])　#３列目と４列目の基本統計量を示す

# 対応のあるt検定
t.test(dat.3$pre, dat.3$post, paired = TRUE)　#p値が小さく差がある

# p値の指数表示を回避
options(scipen = 10)
2.813e-05
# 事前テストと事後テストの相関係数
cor(dat.3$pre, dat.3$post)

# パッケージのインストール（初回のみ）
install.packages("latticeExtra", dependencies = TRUE)
# パッケージの読み込み
library("lattice")
library("latticeExtra")
# 個別推移図（スパゲティ・プロット）
df <- data.frame(score, group)
df$indiv <- factor(c(rep(1 : nrow(dat.3)), rep(1 : nrow(dat.3))))
each <- xyplot(score ? group, group = indiv, type = c("l"), data = df, xlab = "test", ylab = "score")
all_mean <- c(mean(dat.3$pre), mean(dat.3$post))
fact <- factor(c("pre", "post"), levels = c("pre","post"))
all <- xyplot(all_mean ? fact, col = "black", lwd = 5, type = c("l"), data = df)
each + as.layer(all, axes = NULL)

# 横軸に事前テスト，縦軸に事後テストをプロットした散布図
plot(dat.3$pre, dat.3$post, las = 1, pch = 16, xlab = "pretest", ylab = "posttest", main = NA, xlim = c(0, 100), ylim = c(0, 100))
lines(par()$usr[1 : 2], par()$usr[3 : 4], lty = 3)





