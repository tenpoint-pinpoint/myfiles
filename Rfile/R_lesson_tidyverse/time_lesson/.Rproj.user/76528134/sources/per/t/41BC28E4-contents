library(tidyverse)
library(dplyr) ## データ処理パッケージ
library(ggplot2) ## 汎用的な可視化パッケージ
library(forecast) ## 時系列分析のためのパッケージ
library(plotly) ## インタラクティブな図を作るためのパッケージ

ts.plot(Nile)　##ナイルのデータをひとまずプロット

ts.plot(cbind(Nile, 1.5*Nile, 0.5*Nile), ## cbind関数で括ることで複数の時系列を表示できる（横に列を結合する関数）
        lty = c("solid","dashed", "dashed"), ## 線の型を設定
        col = c("gray", "green", "blue")## 線の色を設定
        )

Nile_df <- data_frame(Year = time(Nile), Flow = Nile)　##時系列データをデータフレームに変換、ggplotはデータフレーム型式の方がわかりやすい
g <- ggplot(data = Nile_df, aes(x = Year)) + 　## plotするデータを決める
  geom_line(aes(y = Flow))　　##折れ線グラフをマッピングする
ggplotly(g)       ##ggplotyでインタラクティブなグラフに！



##自分でやってみた##



ts.plot(ldeaths, mdeaths, fdeaths,gpars = list(xlab = "年", ylab = "死亡数"))

death_df <- data_frame(Year = time(ldeaths), ldeaths = ldeaths, mdeaths = mdeaths, fdeaths = fdeaths ) 
ddf <- ggplot(data = death_df, aes(x = Year, y = deaths)) +
  geom_line(aes(y = ldeaths, color = "black")) +
  geom_line(aes(y = mdeaths, color = "red")) +
  geom_line(aes(y = fdeaths, color = "blue"))
ggplotly(ddf)　　# 冗長出しうまく色変えできない。データフレームがtidyでないんだと気付く


death_df <- data_frame(Year = time(ldeaths), ldeaths = ldeaths, mdeaths = mdeaths, fdeaths = fdeaths )  %>% #異なるファイルを統合
  pivot_longer(col = -Year, names_to = "type", values_to = "deaths")
　　　# tidyなデータに変換　col：列を指定,names_to:新たに作る属性列の名前,valuus_to:新たに作る数値列の名前
ddf <- ggplot(data = death_df, aes(x = Year, y = deaths)) +　# プロット
  geom_line(mapping = aes(color = type)) # 折れ線グラフのレイアウト調整
ggplotly(ddf)
