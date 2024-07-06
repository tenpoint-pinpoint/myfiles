library(tidyverse)
library(readxl)
library(skimr)

# ------------------------------------------------------------------------------------------
# 練習問題
# ------------------------------------------------------------------------------------------
# データ読み込み
rfm <- read_csv("/Users/yoshikawahiroshi/Documents/R_lesson_tidyverse/R_lesson_3rd/RFM.csv")



# (1) 変数Sexの値を変換し、rfmに上書き。1->男性、0->女性

         
# (2) 変数DMの値を変換し、rfmに上書き。1->反応、0->無反応


# (3) Age,R,F,Mの平均値をSexの値ごとに求める
rfm %>% group_by(Sex) %>%
  summarise_at(vars(Age,R,F,M),mean)

rfm %>% group_by(Sex) %>%
  summarise_at(c("Age","R","F","M"),mean)

# (4) Age,R,F,Mの平均値をDMの値ごとに求める
rfm %>% group_by(DM) %>%
  summarise_at(vars(Age,R,F,M),mean)

rfm %>% group_by(DM) %>%
  summarise_at(c("Age","R","F","M"),mean)

# (5) SexごとにDM反応者数の集計をする(クロス集計表の作成)
rfm %>% group_by(Sex,DM) %>%
  tally() %>%
  spread(key = DM, value = n)

# (6) Ageの値をカテゴリー化した変数を作成し(20代、30代、40代、50代)、その変数とDMのクロス集計表を作成する
rfm %>% 
  mutate(Ages = case_when(Age < 30 ~ "20代",
                          Age < 40 ~ "30代",
                          Age < 50 ~ "40代",
                          Age < 60 ~ "50代")) %>%
  group_by(Ages,DM) %>%
  summarise(n =n()) %>%
  spread(key = DM, value = n)

rfm %>% mutate(C.Age = case_when(
  Age >= 50 ~ "50代",
  Age >= 40 ~ "40代",
  Age >= 30 ~ "30代",
  Age >= 20 ~ "20代")) %>% 
  count(C.Age, DM) %>% 
  spread(key = DM, value = n)

rfm %>% mutate(C.Age = case_when(
  Age >= 50 ~ "50代",
  Age >= 40 ~ "40代",
  Age >= 30 ~ "30代",
  Age >= 20 ~ "20代")) %>% 
  group_by(C.Age, DM) %>% 
  summarise(n =n()) %>% 
  spread(key = DM, value = n)












# ------------------------------------------------------------------------------------------
# 解答例
# ------------------------------------------------------------------------------------------
# データ読み込み
rfm <- read_csv("RFM.csv")
rfm

# (1) 変数Sexの値を変換し、rfmに上書き。1->男性、0->女性
(rfm <- rfm %>% mutate(Sex = if_else(Sex == 1, "男性", "女性")))

# (2) 変数DMの値を変換し、rfmに上書き。1->反応、0->無反応
(rfm <- rfm %>% mutate(DM = if_else(DM == 1, "反応", "無反応" )))

# (3) Age,R,F,Mの平均値をSexの値ごとに求める
rfm %>% group_by(Sex) %>% 
  summarise_at(vars(Age, R, F, M), mean)

# (4) Age,R,F,Mの平均値をDMの値ごとに求める
rfm %>% group_by(DM) %>% 
  summarise_at(vars(Age, R, F, M), mean)

# (5) SexごとにDM反応者数の集計をする(クロス集計表の作成)
rfm %>% count(Sex, DM) %>% 
  spread(key = DM, value = n)

rfm %>% group_by(Sex, DM) %>% 
  summarise(n = n()) %>% 
  spread(key = DM, value = n)

# (6) Ageの値をカテゴリー化した変数を作成し(20代、30代、40代、50代)、その変数とDMのクロス集計表を作成する
rfm %>% mutate(C.Age = case_when(
  Age >= 50 ~ "50代",
  Age >= 40 ~ "40代",
  Age >= 30 ~ "30代",
  Age >= 20 ~ "20代")) %>% 
  count(C.Age, DM) %>% 
  spread(key = DM, value = n)

rfm %>% mutate(C.Age = case_when(
  Age >= 50 ~ "50代",
  Age >= 40 ~ "40代",
  Age >= 30 ~ "30代",
  Age >= 20 ~ "20代")) %>% 
  group_by(C.Age, DM) %>% 
  summarise(n =n()) %>% 
  spread(key = DM, value = n)
