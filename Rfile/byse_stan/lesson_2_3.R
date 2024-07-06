library(tidyverse)

fish <- read.csv(file.choose())

# ヒストグラム
ggplot(data = fish, mapping = aes(x = length)) +
  geom_histogram(alpha=0.5, bins=20)+
  labs(titele = "ヒストグラム")

# カーネル密度推定
ggplot(data = fish, mapping = aes(x = length))+
  geom_density(size = 1.5)+
  labs(title = "カーネル密度推定")

# グラフを重ね合わせる
ggplot(data = fish, mapping = aes(x = length, y = ..density..))+ #..density..でy軸をヒストグラムの面積が１になるよう調整
  geom_histogram(alpha = 0.3, bins = 20)+
  geom_density(size = 1.5)+ # sizeは曲線の太さ
  labs(title = "重ね合せ")

# グラフの一覧表示
library(gridExtra)

p_hist <- ggplot(data = fish, mapping = aes(x=length))+
  geom_histogram(alpha=0.5, bins=20)+
  labs(title = "ヒストグラム")

p_density <- ggplot(data = fish, mapping = aes(x=length))+
  geom_density(size=1.0, fill="blue", alpha = 0.3)+
  labs(title = "カーネル密度")

grid.arrange(p_hist, p_density, ncol = 2)

# 箱ひげ図
iris <- iris
p_box <- ggplot(data = iris, mapping = aes(x = Species, y = Petal.Length))+
  geom_boxplot()+
  labs(title = "箱ひげ")

p_vio <- ggplot(data = iris, mapping = aes(x = Species, y = Petal.Length))+
  geom_violin()+
  labs(title = "バイオリン")

grid.arrange(p_box, p_vio, ncol = 2)


# 散布図
ggplot(data = iris, mapping = aes(x = Petal.Width, y = Petal.Length, color = Species))+
  geom_point()


# 折れ線グラフ
nile_df <- data.frame(
  year = 1871:1970
  ,nile = as.numeric(nile))
ggplot(data = nile_df, mapping = aes(x = year, y = nile))+
  geom_line()+
  labs(title = "nile_flow")
