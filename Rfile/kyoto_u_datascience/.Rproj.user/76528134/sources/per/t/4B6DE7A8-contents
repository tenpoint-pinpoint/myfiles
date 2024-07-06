########## Chapter 10 ##########

# パッケージのインストール（初回のみ）
install.packages("semPlot", dependencies = TRUE)
# パッケージの読み込み
library("lavaan")
library("semPlot")

# CSVファイルの読み込み（data_ch10-1.csvを選択）
dat <- read.csv(file.choose(),fileEncoding = "shift-jis", header = TRUE)

# パス解析（逐次モデルの指定）
model.RM <- '
  興味関心 ~ 学生対応 + 説明
  理解度 ~ 説明 + 興味関心 '

# 分析と結果の表示
fit.RM <- sem(model.RM, data = dat, estimator = "ML")
summary(fit.RM, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# パス図の作成
semPaths(fit.RM, what = "stand", style = "lisrel", layout = "tree", rotation = 2, nCharNodes = 0, nCharEdges = 0, fade = FALSE, edge.width = 0.2, label.scale = FALSE, label.cex = 1.2, theme = 'gray', asize = 6.0, node.width = 2.0, curve = 2.0)

# CSVファイルの読み込み（data_ch10-2.csvを選択）
dat.2 <- read.csv(file.choose(),fileEncoding = "shift-jis", header = TRUE)

# MIMICモデルの指定
model.MIMIC <- '
  指導技術 =~ 進み具合 + 興味関心 + 理解度
  指導技術 ~ 学生対応 + 話し方 + 説明 '
# 分析と結果の表示
fit.MIMIC <- sem(model.MIMIC, data = dat.2, estimator = "ML")
summary(fit.MIMIC, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# パス図の作成
semPaths(fit.MIMIC, what = "stand", style = "lisrel", layout = "tree", rotation = 2, nCharNodes = 0, nCharEdges = 0, fade = FALSE, optimizeLatRes = TRUE, edge.width = 0.2, label.scale = FALSE, label.cex = 1.2, theme = 'gray', asize = 6.0, node.width = 1.5)

# CSVファイルの読み込み（data_ch10-3.csvを選択）
dat.3 <- read.csv(file.choose(), fileEncoding = "shift-jis", header = TRUE)

# 多重指標モデルの指定
model.MIC <- '
  授業満足度 =~ item8 + item9 + item4
  指導技術 =~ item2 + item1 + item6
  授業満足度 ~ 指導技術 '
# 分析と結果の表示
fit.MIC <- sem(model.MIC, data = dat.3, estimator = "ML")
summary(fit.MIC, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# パス図の作成
semPaths(fit.MIC, what = "stand", style = "lisrel", layout = "tree", rotation = 2, nCharNodes = 0, nCharEdges = 0,  fade = FALSE, optimizeLatRes = TRUE, edge.width = 0.2, label.scale = FALSE, label.cex = 1.2, theme = 'gray', asize = 6.0, node.width = 1.5)

# CSVファイルの読み込み（data_ch10-4.csvを選択）
dat.4 <- read.csv(file.choose(),fileEncoding = "shift-jis", header = TRUE)

# 交差遅延モデル
model.CLM <- '
  単語力2 ~ 単語力1 + 多読量1
  多読量2 ~ 単語力1 + 多読量1 '
# 分析と結果の表示
fit.CLM <- sem(model.CLM, data = dat.4, estimator = "ML")
summary(fit.CLM, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# パス図の作成
semPaths(fit.CLM, what = "stand", style = "lisrel", layout = "tree2", rotation = 2, nCharNodes = 0, nCharEdges = 0, fade = FALSE, edge.width = 0.2, edge.label.position = 0.7, label.scale = FALSE, label.cex = 1.2, theme = 'gray', asize = 6.0,  node.width = 2.0)

# CSVファイルの読み込み（data_ch10-5.csvを選択）
dat.5 <- read.csv(file.choose(), fileEncoding = "shift-jis", header = TRUE)

# 潜在成長曲線モデルの指定
model.LGM <- '
  切片 =~ 1 * 一学期 + 1 * 二学期 + 1 * 三学期 
  傾き =~ 0 * 一学期 + 1 * 二学期 + 2 * 三学期 '

# 分析と結果の表示
fit.LGM <- growth(model.LGM, data = dat.5, estimator = "ML")
summary(fit.LGM, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# パス図の作成
semPaths(fit.LGM, title = FALSE, whatLabels = "est", style = "lisrel", layout = "tree", rotation = 1, nCharNodes = 0, nCharEdges = 0, fade = FALSE, optimizeLatRes = TRUE, edge.width = 0.2, edge.color = "black", label.scale = FALSE, label.cex = 1.2, theme = 'gray', asize = 6.0, node.width = 1.0)

# モデルの修正指標
modificationindices(fit.RM)

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch10-6.csvを選択
dat <- read.csv(file.choose(), header = TRUE)
# 未受験者（NA）を削除
dat <- na.omit(dat)
# 素点を偏差値に換算
dat <- scale(dat[, -1]) * 10 + 50
# 読み込んだデータの冒頭の確認
head(dat)

# 関数の定義（コードは自動で読み込まれます）
source("C:/.../filename.txt")

# 最適なランクを探索
for(i in 2 : 6){
  print(i)
  r.LRT <- LRA(dat, i)
  AIC(r.LRT)
}

# 分析結果の確認
r.LRT <- LRA(dat, 5)
summary(r.LRT)

# ランクメンバーシッププロファイルの確認
r.LRT$rank
# ランクメンバーシッププロファイルの可視化
par(mfrow = c(1, 2))
plot(r.LRT$res[1, ], type = "b", xlab = "Rank")
plot(r.LRT$res[9, ], type = "b", xlab = "Rank")

########## Chapter 11 ##########

# CSVファイルの読み込み（ヘッダーと列名がある場合）
# data_ch11-1.csvを選択
dat <- read.csv(file.choose(), header = TRUE, row.names = 1)
# 読み込んだデータの冒頭の確認
head(dat)
# 行数と列数の確認
dim(dat)

# 各科目の平均点（＝列平均）を計算
colMeans(dat)
# 各学習者の平均点（＝行平均）を計算
rowMeans(dat)

# ユークリッド距離の計算
dat.d <- dist(dat)
# ユークリッド距離の計算結果の確認
dat.d

# ウォード法によるクラスターの作成
dat.hc <- hclust(dat.d, method = "ward.D2")
dat.hc

# 樹形図による可視化
plot(dat.hc)

# 樹形図における個々のデータを一番下にそろえて配置
plot(dat.hc, hang = -1)

# 個々のデータが含まれるクラスターの確認（クラスター数が2の場合）
cutree(dat.hc, k = 2)

# 樹形図による可視化
plot(dat.hc, hang = -1)
# 樹形図におけるクラスターを囲む四角形を表示（クラスター数が2の場合）
rect.hclust(dat.hc, k = 2)

# 分析データの形式を確認
class(dat)
# 分析データを行列に変換
dat.2 <- as.matrix(dat)
# 分析データの形式を再確認
class(dat.2)
# ヒートマップつきの樹形図の作成（ユークリッド距離，ウォード法）
heatmap(dat.2, cexCol = 0.5, hclustfun = function(x){hclust(x, method = "ward.D2")})

# 乱数の種を固定
set.seed(1)
# k-means法（クラスター数は2）
dat.km <- kmeans(dat, center = 2)

# k-means法によるクラスタリング結果の確認
dat.km$cluster

# パッケージの読み込み
library("cluster")
# k-means法によるクラスタリング結果の可視化
clusplot(dat, dat.km$cluster)

# k-means法によるクラスタリング結果の可視化（個々の学習者のIDを表示）
clusplot(dat, dat.km$cluster, labels = 2)

# Gap統計量の計算
set.seed(1)
dat.Gap <- clusGap(dat, kmeans, K.max = 5)
# 計算されたGap統計量の確認
dat.Gap

# Gap統計量とクラスター数の関係の可視化
plot(dat.Gap)

# CSVファイルの読み込み（ヘッダーがある場合）
# data_ch11-2.csvを選択
dat <- read.csv(file.choose(), header = TRUE)
# 読み込んだデータの冒頭の確認
head(dat)
# 読み込んだデータ（全体）の概要の確認
summary(dat)

# テストの合格者数と不合格者数を確認
table(dat$Grade)

# パッケージの読み込み
library("rpart")
# 決定木分析（Gradeを結果変数，それ以外を予測変数として指定）
rpart.result <- rpart(Grade ~ ., data = dat)
# 分析結果の確認
rpart.result

# パッケージのインストール（初回のみ）
install.packages("partykit", dependencies = TRUE)
# パッケージの読み込み
library("partykit")
# 決定木の可視化
plot(as.party(rpart.result))