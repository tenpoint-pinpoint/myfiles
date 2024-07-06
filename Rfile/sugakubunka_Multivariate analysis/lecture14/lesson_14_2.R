# 決定木分析
# install.packages("rpart")
# install.packages("partykit")
library(rpart)
library(package)

# データの読み込み
dat <- read.csv("./data/split_demo.csv", fileEncoding = "utf-8")
head(dat)
str(dat)
dat$label <- as.factor(dat$label)
summary(dat)
plot(dat$x, dat$y, col=dat$label)
# 分けることによってこの図の赤と黒の割合が偏るようにしたい

result <- rpart(formula = label ~ x + y,
                data = dat,
                method = "class", # classは分類してくださいメソッド。回帰の場合はanova
                control = rpart.control(maxdepth = 2,
                                        minsplit = 20,
                                        minbucket = 5))

result
#------結果------------------------------------------------------#
# n= 100 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node

# 1) root 100 25 1 (0.75000000 0.25000000)  
#   2) x< 0.3325615 55  1 1 (0.98181818 0.01818182) *
#   3) x>=0.3325615 45 21 2 (0.46666667 0.53333333)    
#    6) y>=0.3488362 21  0 1 (1.00000000 0.00000000) *
#    7) y< 0.3488362 24  0 2 (0.00000000 1.00000000) *
#----------------------------------------------------------------#

# 文字だとわかりにくいので描画してみる
# 判別分析などと違い、一目でどっちに分類されるかわかる。新しいデータに対しても分類可能！
plot(as.party(result))


# 何をやっているのか？
# 変数重要度：各変数が分類にどの程度貢献しているか
result$variable.importance
#------結果------------------------------------------------------#
# y        x 
# 23.56768 16.33636     ----->  yの方が貢献している
#----------------------------------------------------------------#