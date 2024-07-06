library("tidyverse")
library("MatchIt")
library("broom")

### データ生成 #################################################################
## 1849年・エリアのコレラ部死者
## サザークアンドヴォクソールカンパニーのみ
sv1849 <- c(283,157,192,249,259,226,352,97,111,8,235,92)
## Lambeth Company＆Southwark and Vauxhall Company
lsv1849 <- c(256,267,312,257,318,446,143,193,243,215,544,187,153,81,113,176)

## 1849年・エリアのコレラ部死者
## サザークアンドヴォクソールカンパニーのみ
sv1854 <- c(371,161,148,362,244,237,282,59,171,9,240,174)
## Lambeth Company＆Southwark and Vauxhall Company
lsv1854 <- c(113,174,270,93,210,388,92,58,117,49,193,303,142,48,165,132)

## 会社毎にまとめる  vetorにする
sv_death <- c(sv1849,sv1854)
lsv_death <- c(lsv1849,lsv1854)

## どのデータがどのエリアかを認識するためのラベル
sv_area <- paste0("sv_",c(1:length(sv1849),1:length(sv1854)))
lsv_area <- paste0("lsv_",c(1:length(lsv1849),1:length(lsv1854)))
# paste  … ()内の要素を文字列に変換し、繋げて出力する関数
# paste0 … pasteと同じ役割だが、区切りなしで出力する →　paste( , seq="" ) == paste0()

## どのデータが何年のものかを認識するためのラベル
sv_year <- c(rep("1849",length(sv1849)),rep("1854",length(sv1854)))
lsv_year <- c(rep("1849",length(lsv1849)),rep("1854",length(lsv1854)))
# rep  … rep(a:b, length = c )で、aからbまで１づつ増加する数列を。長さcになるまで反復して生成

## データフレーム化
sv <- data.frame(area = sv_area,
                 year = sv_year,
                 death = sv_death,
                 LSV = "0",
                 company = "Southwark & Vauxhall")

lsv <- data.frame(area = lsv_area,
                  year = lsv_year,
                  death = lsv_death,
                  LSV = "1",
                  company = "Lambetg/Southwark & Vauxhall")

## 地域・年代別のデータセットを作成
JS_df <- rbind(sv, lsv) %>% 
  mutate(LSV = if_else(company == "Lambetg/Southwark & Vauxhall", 1, 0))

# rbind … データフレームを結合する。多分だけどbind_row()の方が便利かも。
#         rbindはカラム名と列数が完全一致している場合のみ有効
#         bind_rowは結合するデータフレーム同士を参照して存在しないデータはNAで返してくれる
#　　　　ってかなんでデータフレーム作成時に作ってるカラムなのに再生成しているのか…

## 会社別のデータセットを作成
data1 <- 
  JS_df %>% 
  group_by(company,LSV,year) %>% 
  summarise(death = sum(death))

### データ生成 完#################################################################

## 集計による推定
result <- data1 %>% 
  mutate(year = paste("year", year, sep = "_")) %>% 
  pivot_wider(names_from = year, values_from = death) %>%   
  mutate(gap = year_1854 - year_1849,
         gap_rate = year_1854/year_1849 -1)

result_log <- data1 %>% 
  mutate(year = paste("year", year, sep = "_"), death = log(death)) %>% 
  pivot_wider(names_from = year, values_from = death) %>%
  mutate(gap = year_1854 - year_1849)

## 集計による推定(log)
JS_grp_summary_log <- JS_sum %>% 
  mutate(year = paste("year", year, sep = "_"), death = log(death)) %>% 
  pivot_wider(names_from = year, values_from = death) %>%   # spreadよりpivot_widerの方が可読性が良い
  mutate(gap = year_1854 - year_1849)

