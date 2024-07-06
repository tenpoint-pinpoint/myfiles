library(tidyverse)

fish <-  read.csv(file.choose())

head(fish)

hist(fish$length)

# ヒストグラムを滑らかにしたものがカーネル密度推定
kernel_density <- density(fish$length)
plot(kernel_density)

# バンド幅を変えてカーネル密度推定
# バンド幅は各標本が影響を及ぼす範囲を指定するもの。大きいほど滑らかになる
kernel_density_quarter <- density(fish$length, adjust = 0.25)
kernel_density_quadruple <- density(fish$length, adjust = 4)

plot(kernel_density
     , lwd = 2
     , xlab = ""
     , ylim = c(0, 0.26)
     , main = "バンド幅を変える")
lines(kernel_density_quarter, col = 2)
lines(kernel_density_quadruple, col = 4)
legend("topleft" # 凡例の位置
       ,col = c(1,2,4) # 線の色
       , lwd = 1 # 線の太さ
       , bty = "n" # 凡例の囲い線を消す
       , legend = c("標準", "1/4", "*4")) # 凡例の名称

# 基本統計量
suuretsu <- 0:1000
mean(suuretsu)
length(suuretsu)
median(suuretsu)
quantile(suuretsu, probs = c(0.25,0.5,0.75))

# 相関係数
birds <- read.csv(file.choose())
head(birds)

cor(birds$body_length, birds$feather_length)

# 自己共分散
nile <- Nile
head(nile)
acf(nile
    ,type = "covariance"
    ,plot = F
    ,lag.max = 5)
# 自己相関
acf(nile,plot = F,lag.max = 5) # 5時点前までの自己相関
# コレログラム
acf(nile)
