library(tidyverse)
library(readxl)
library(skimr)

# ------------------------------------------------------------------------------------------
# 練習問題
# ------------------------------------------------------------------------------------------
# データ読み込み
rfm <- read_csv("/Users/yoshikawahiroshi/Documents/R_lesson_tidyverse/r_lesson_4th/RFM.csv")

# データの整形
rfm <- rfm %>% mutate(Sex = if_else(Sex == "M", "男性", "女性"),
                      DM = if_else(DM == "yes", "反応", "無反応"),
                      C.Age = case_when(Age >= 50 ~ "50代",
                                        Age >= 40 ~ "40代",
                                        Age >= 30 ~ "30代",
                                        Age >= 20 ~ "20代"))



# (1) Rのヒストグラムを作成する
rfm %>%
  ggplot(aes(x = R)) +
  geom_histogram(binwidth = 20)

# (2) Fのヒストグラムを作成する
ggplot(data = rfm, mapping = aes(x = F)) +
  geom_histogram(bins = 15)

# (3) Mのヒストグラムを作成する
ggplot(data = rfm, aes(x = M)) +
  geom_histogram(bins = 15)


# (4) 男女別のRのヒストグラムを作成し、並べて表示
ggplot(rfm, aes(x = R)) +
  geom_histogram() +
  facet_grid(Sex ~ .)

# (5) 男女別にFのヒストグラムを作成し、並べて表示する
rfm %>% ggplot(aes(x = F)) +
  geom_histogram(binwidth = 5) +
  facet_grid(Sex ~ .) 

# (6) 男女別にMのヒストグラムを作成し、並べて表示する
ggplot(rfm, aes(x = F)) +
  geom_histogram() +
  facet_grid(Sex ~ .)

# (7) DMで層別化し、Rのヒストグラムを性別ごとに作成し、並べて表示する
rfm %>% group_by(DM) %>%
  ggplot(aes(x = R)) +
  geom_histogram() +
  facet_grid(Sex ~ .) +
  theme_gray(base_family = "HiraKakuPro-W3")

# (8) DMで層別化し、Rのヒストグラムを性別ごとに作成し、並べて表示する


# (9) DMで層別化し、Rのヒストグラムを性別ごとに作成し、並べて表示する


# (10) DMで層別化し、RとFの散布図を作成する
rfm %>% ggplot(aes(x = R, y = F, color = DM)) +
  geom_point()

rfm %>% ggplot(aes(x = R, y = F, color = DM)) +
  geom_point() 

# (11) DMで層別化し、RとMの散布図を作成する
rfm %>% ggplot(aes(x = R, y = M, color = DM)) +
  geom_point()


# (12) DMで層別化し、FとMの散布図を作成する
rfm %>% ggplot(aes(x = F, y = M, color = DM)) +
  geom_point()

# (13) DMごとのAgeの平均値を棒グラフで表示する
ggplot(data = rfm,mapping = aes(x = DM, y = Age))+
geom_bar(stat = "summary", fun.y = "mean")

rfm %>% ggplot(aes(x = DM, y = Age)) +
  stat_summary(geom = "bar", fun.y = "mean") 


# (14) DMごとのRの平均値を棒グラフで表示する
ggplot(data = rfm, mapping =  aes(x = DM, y = R)) +
  geom_bar(stat = "summary", fun.y = "mean") +
theme_gray(base_family = "HiraKakuPro-W3")

# (15) DMごとのFの平均値を棒グラフで表示する
ggplot(data = rfm, mapping =  aes(x = DM, y = F)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  theme_gray(base_family = "HiraKakuPro-W3")

# (16) DMで層別化したAgeの平均値をSexごとに棒グラフで表示する
ggplot(data = rfm, mapping = aes(x = Sex, y = Age, fill = DM)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") + 
  theme_gray(base_family = "HiraKakuPro-W3")


rfm %>% ggplot(aes(x = Sex, y = R, fill = DM)) +
  stat_summary(geom = "bar", fun.y = "mean", position = "dodge") 

# (17) DMで層別化したRの平均値をSexごとに棒グラフで表示する


# (18) DMで層別化したFの平均値をSexごとに棒グラフで表示する


# (19) DMで層別化したMの平均値をSexごとに棒グラフで表示する












# ------------------------------------------------------------------------------------------
# 解答例
# ------------------------------------------------------------------------------------------
# データ読み込み
rfm <- read_csv("RFM.csv")

# データの整形
rfm <- rfm %>% mutate(Sex = if_else(Sex == 1, "男性", "女性"),
                      DM = if_else(DM == "yes", "反応", "無反応"),
                      C.Age = case_when(Age >= 50 ~ "50代",
                                        Age >= 40 ~ "40代",
                                        Age >= 30 ~ "30代",
                                        Age >= 20 ~ "20代"))


# (1) Rのヒストグラムを作成する
rfm %>% ggplot(aes(x = R)) +
  geom_histogram(binwidth = 20)

# (2) Fのヒストグラムを作成する
rfm %>% ggplot(aes(x = F)) +
  geom_histogram(binwidth = 5)

# (3) Mのヒストグラムを作成する
rfm %>% ggplot(aes(x = M)) +
  geom_histogram(bins = 20)

# (4) 男女別のRのヒストグラムを作成し、並べて表示
rfm %>% ggplot(aes(x = R)) +
  geom_histogram(binwidth = 20) +
  facet_grid(Sex ~ .) 

# (5) 男女別にFのヒストグラムを作成し、並べて表示する
rfm %>% ggplot(aes(x = F)) +
  geom_histogram(binwidth = 5) +
  facet_grid(Sex ~ .) 

# (6) 男女別にMのヒストグラムを作成し、並べて表示する
rfm %>% ggplot(aes(x = M)) +
  geom_histogram(bins = 20) +
  facet_grid(Sex ~ .) 

# (7) DMで層別化し、Rのヒストグラムを性別ごとに作成し、並べて表示する
rfm %>% ggplot(aes(x = R, fill = DM)) +
  geom_histogram(bins = 20, position = "identity", alpha = 0.4) +
  facet_grid(Sex ~ .) 

# (8) DMで層別化し、Rのヒストグラムを性別ごとに作成し、並べて表示する
rfm %>% ggplot(aes(x = F, fill = DM)) +
  geom_histogram(bins = 20, position = "identity", alpha = 0.4) +
  facet_grid(Sex ~ .) 

# (9) DMで層別化し、Rのヒストグラムを性別ごとに作成し、並べて表示する
rfm %>% ggplot(aes(x = M, fill = DM)) +
  geom_histogram(bins = 20, position = "identity", alpha = 0.4) +
  facet_grid(Sex ~ .) 

# (10) DMで層別化し、RとFの散布図を作成する
rfm %>% ggplot(aes(x = R, y = F, color = DM)) +
  geom_point() 

# (11) DMで層別化し、RとMの散布図を作成する
rfm %>% ggplot(aes(x = R, y = M, color = DM)) +
  geom_point() 

# (12) DMで層別化し、FとMの散布図を作成する
rfm %>% ggplot(aes(x = F, y = M, color = DM)) +
  geom_point() 

# (13) DMごとのAgeの平均値を棒グラフで表示する
rfm %>% ggplot(aes(x = DM, y = Age)) +
  stat_summary(geom = "bar", fun.y = "mean") 

# (14) DMごとのRの平均値を棒グラフで表示する
rfm %>% ggplot(aes(x = DM, y = R)) +
  stat_summary(geom = "bar", fun.y = "mean") 

# (15) DMごとのFの平均値を棒グラフで表示する
rfm %>% ggplot(aes(x = DM, y = F)) +
  stat_summary(geom = "bar", fun.y = "mean") 

# (16) DMで層別化したAgeの平均値をSexごとに棒グラフで表示する
rfm %>% ggplot(aes(x = Sex, y = Age, fill = DM)) +
  stat_summary(geom = "bar", fun.y = "mean", position = "dodg") 

# (17) DMで層別化したRの平均値をSexごとに棒グラフで表示する
rfm %>% ggplot(aes(x = Sex, y = R, fill = DM)) +
  stat_summary(geom = "bar", fun.y = "mean", position = "dodge") 

# (18) DMで層別化したFの平均値をSexごとに棒グラフで表示する
rfm %>% ggplot(aes(x = Sex, y = F, fill = DM)) +
  stat_summary(geom = "bar", fun.y = "mean", position = "dodge") 

# (19) DMで層別化したMの平均値をSexごとに棒グラフで表示する
rfm %>% ggplot(aes(x = Sex, y = M, fill = DM)) +
  stat_summary(geom = "bar", fun.y = "mean", position = "dodge") 
