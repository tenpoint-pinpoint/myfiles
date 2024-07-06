library(tidyverse)
library(skimr)

# ------------------------------------------------------------------------------------------
# 練習問題
# ------------------------------------------------------------------------------------------
# データ読み込み
titanic <- read_csv("/Users/yoshikawahiroshi/Documents/R_lesson_tidyverse/R_lesson_3rd/titanic.csv")


# (1) PclassとSurvivedをfactor型に、その他のchr型をfactor型に変換し、titanicに上書き。
titanic %>%
  mutate_at(vars(Pclass, Survived), as.factor) %>%
  mutate_if(is.character, as.factor)
  
# (2) Name、Cabin、Ticketを削除し、titanicに上書き。
titanic %>%
  select(-Name,-Cabin,-Ticket)

# (3) 欠損値をリストワイズ削除し、titanicに上書き。
titanic %>% 
  na.omit() %>%
  skim()

# (4) PassengerId以外の数値型の変数の平均値を求める
titanic %>%
  select(-"PassengerId") %>%
  summarise_if(is.numeric, mean)


# (5) PassengerId以外の数値型の変数の平均値をPclassの値ごとに求める(Pclassでグループ化)
titanic %>% group_by(Pclass) %>%
  select(-"PassengerId") %>%
  summarise_if(is.numeric, mean)

# (6) PassengerId以外の数値型の変数の平均値をSexの値ごとに求める(Sexでグループ化)
titanic %>% group_by(Sex) %>%
  select(-"PassengerId") %>%
  summarise_if(is.numeric, mean)


# (7) Pclassごとに死亡者数の集計する(クロス集計の作成)
titanic %>% group_by(Survived,Pclass) %>%
  tally() %>%
  spread(key = Survived, value = n)

# (8) PclassとSexごとに死亡者数の集計(クロス集計の作成)
titanic %>% group_by(Survived,Pclass, Sex) %>%
  tally() %>%
  spread(key = Survived, value = n)


# ------------------------------------------------------------------------------------------
# 解答例
# ------------------------------------------------------------------------------------------
# データ読み込み
titanic <- read_csv("titanic.csv")
titanic %>% skim()

# (1) PclassとSurvivedをfactor型に、その他のchr型をfactor型に変換し、titanicに上書き。
(titanic <- titanic %>% 
    mutate_at(c("Pclass", "Survived"), as.factor) %>% 
    mutate_if(is.character, as.factor))
titanic %>% skim()


# (2) Name、Cabin、Ticketを削除し、titanicに上書き。
(titanic <- titanic %>% select(-Name, -Cabin, -Ticket))


# (3) 欠損値をリストワイズ削除し、titanicに上書き。
(titanic <- titanic %>% na.omit())
titanic %>% skim()


# (4) PassengerId以外の数値型の変数の平均値を求める
titanic %>% 
    select(-"PassengerId") %>% 
    summarize_if(is.numeric,mean)


# (5) PassengerId以外の数値型の変数の平均値をPclassの値ごとに求める(Pclassでグループ化)
titanic %>% 
    select(-PassengerId) %>% 
    group_by(Pclass) %>% 
    summarise_if(is.numeric, mean)


# (6) PassengerId以外の数値型の変数の平均値をSexの値ごとに求める(Sexでグループ化)
titanic %>% 
    select(-PassengerId) %>% 
    group_by(Sex) %>% 
    summarise_if(is.numeric, mean)

# (7) Pclassごとに死亡者数の集計する(クロス集計表の作成)
titanic %>% 
    group_by(Pclass, Survived) %>% 
    tally() %>% 
    spread(key = Survived, value = n)

titanic %>% 
  group_by(Pclass, Survived) %>% 
  summarise(n = n()) %>% 
  spread(key = Survived, value = n)

titanic %>% count(Pclass, Survived) %>% 
  spread(key = Survived, value = n)


# (8) PclassとSexごとに死亡者数の集計する(クロス集計表の作成)
titanic %>% group_by(Pclass, Sex, Survived) %>% 
  tally() %>% 
  spread(key = Survived, value = n)

titanic %>% 
    group_by(Pclass, Sex, Survived) %>% 
    summarise(n = n()) %>% 
    spread(key = Survived, value = n)

titanic %>% count(Pclass, Sex, Survived) %>% 
  spread(key = Survived, value = n)

