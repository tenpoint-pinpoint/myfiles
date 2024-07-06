########## Chapter 7 ##########

# CSVファイルの読み込み
# data_ch7-1.csvを選択
dat <- read.csv(file.choose(), header = TRUE) 
# 1～6列目のうちNAのある行を削除
dat.2 <- na.omit(dat[, 1 : 6])
# NAのある行を削除したデータの冒頭を確認
head(dat.2)
# midとend（5～6列目）の記述統計
summary(dat.2[, 5 : 6])

# 中間試験と期末試験のPearsonの積率相関係数
cor(dat.2$mid, dat.2$end, method = "pearson")

# 相関係数の有意確率（無相関検定）や95%信頼区間の計算
cor.test(dat.2$mid, dat.2$end, method = "pearson")

# 対角線ありの散布図の作成
plot(dat.2$mid, dat.2$end, 
     # ドットの大きさを変更
     cex = 1.2,
     # x軸・y軸のラベルと範囲を変更，タイトルを表示
     xlab = "中間テスト得点", xlim = c(0, 100),
     ylab = "期末テスト得点", ylim = c(0, 100))
# 散布図に切片a = 0，傾きb = 1の直線を表示
abline(a = 0, b = 1, lty = 1)

# パッケージの読み込み
library("psych")
# 中間試験（5列目）と期末試験（6列目）の散布図を作成
pairs.panels(dat.2[, 5 : 6], 
             # 回帰直線（黒色）とその95%信頼区間を表示
             lm = TRUE, ci = TRUE, col = "black",
             # ヒストグラムの色を変更
             hist.col = "grey",
             # ドットのスタイルを変更
             pch = 21,
             # 有意な相関係数にアスタリスクを付ける
             stars = TRUE,
             # 平滑線（smooth）と相関円（ellipses）を描写しない
             smooth = FALSE, ellipses = FALSE)


# 学科F01だけで相関分析
cor(dat.2$mid[dat.2$faculty == "F01"], dat.2$end[dat.2$faculty == "F01"], method = "pearson")

# パッケージの読み込み
library("lattice")
# 中間試験と期末試験の散布図を学科別にプロット（実行結果は省略）
xyplot(mid ~ end | faculty, data = dat.2,
       # 中間の点数が80点以上か否かで色分け，異なるマーカーを使用
       groups = mid >= 80, col = c("grey20", "grey40"),
       pch = c(1, 16), cex = c(1.0, 1.0),
       # 中間試験と期末試験の平均点ラインを描写
       abline = list(h = mean(dat.2$mid),v = mean(dat.2$end),
                     col = "grey"))
# F01~F09の順番を並び替えることも可能
dat.2$faculty <- factor(dat.2$faculty, 
                        levels = c("F07", "F08", "F09", "F04", "F05", "F06",
                                   "F01", "F02", "F03"))
xyplot(mid ~ end | faculty, data = dat.2,  
       groups = mid >= 80, col = c("grey20", "grey40"),
       pch = c(1, 16), cex = c(1.0, 1.0),
       abline = list(h = mean(dat.2$mid),v = mean(dat.2$end),
                     col = "grey"))
dat.3
# 7～9列目のうちNAのある行を削除
dat.3 <- na.omit(dat[, 7 : 9])
# Spearmanの相関係数を計算
cor(dat.3, method = "spearman")

# 3変数以上の組み合わせから相関係数の有意確率と95%信頼区間を計算
res <- corr.test(dat.3, method = "spearman")
print(res, short = FALSE)

# 散布図
plot(dat.3)

# クロス集計表の作成
ques <- xtabs( ~ manzoku + rikai, data = dat.3)
ques
# パッケージの読み込み
library("gplots")
# 気球グラフのプロット
balloonplot(ques)











# CSVファイルの読み込み
# data_ch7-2.csvを選択
dat.4 <- read.csv(file.choose(), header = TRUE) 
# 読み込んだデータの冒頭の確認
head(dat.4)

# 1列目を除いたデータからCronbachのα係数を計算
alpha(dat.4[, -1])

# パッケージの読み込み
library("RMeCab") 
# 分析データの読み込み
# data_ch7-3.csvを選択
dat.5 <- read.csv(file.choose(), header = FALSE)
# 形態素解析
RMC <- RMeCabDF(dat.5)
# 100名分の自由回答を結合
RMC.2 <- unlist(RMC)
# 結合したデータの冒頭を確認
head(RMC.2)

# 形態素解析の結果をデータフレームに変換
RMC.3 <- data.frame(RMC.2, names(RMC.2))
# 列名を編集
colnames(RMC.3) <- c("Word", "POS")
# データフレームの冒頭を確認
head(RMC.3)
# データフレームから名詞の行だけを抽出
noun <- RMC.3[RMC.3$POS == "名詞", ]
# 抽出した結果（冒頭の3つ）を確認
head(noun, 3)
# 名詞の頻度を集計
noun.list <- table(noun[, 1])
# 頻度の高い順に並び替え
noun.list.2 <- sort(noun.list, decreasing = TRUE)
# 並び替え結果をデータフレームに変換
noun.list.3 <- data.frame(noun.list.2)
# 列名を編集
colnames(noun.list.3) <- c("Word", "Freq")
# 頻度上位の名詞（冒頭の3つ）を確認
head(noun.list.3, 3)

# KWICコンコーダンスを作成する関数の定義
kwic.conc <- function(vector, word, span){
  word.vector <- vector  # 分析対象とするベクトルを指定
  word.positions <- which(word.vector == word)  # 分析対象とする単語の出現位置を特定
  context <- span  # 単語の文脈を左右何語ずつ表示するかを指定
  # 用例を検索
  for(i in seq(word.positions)) { 
    if(word.positions[i] == 1) {
      before <- NULL
    } else {
      start <- word.positions[i] - context
      start <- max(start, 1)
      before <- word.vector[start : (word.positions[i] - 1)]
    }
    end <- word.positions[i] + context
    after <- word.vector[(word.positions[i] + 1) : end]
    after[is.na(after)] <- ""
    keyword <- word.vector[word.positions[i]]
    # 用例を表示
    cat("--------------------", i, "--------------------", "\n")
    cat(before, "[", keyword, "]", after, "\n")
  }
}

# 「教室」の用例検索
kwic.conc(RMC.2, "教室", 5)

result$residuals

