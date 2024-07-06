# install.packages('psych')
library(psych)

dat <- read.csv('./data/ramen.csv', fileEncoding = 'cp932', row.names = '店名')
dat

# 無回転の因子分析 _ 最尤法
result_none <- fa(dat, 
                  nfactors = 2,#------>因子分析は先に因子の数を決めておかないと計算できない
                　fm = 'ml',#--------->推定方法。'ml'は最尤法。'ols'で最小二乗法
                　rotate = 'none')#--->回転の方法。'none'は無回転

print(result_none$loadings, #--------->因子負荷量
      cutoff = 0)

#無回転解は解釈しやすいとは限らない
#解釈しやすい = 0から近いか遠いかのメリハリがついている状態
#これを数学的に表現したのがサーストンの単純構造の条件

result_none$communalities#------------>共通性の計算。１に近いほどデータを説明できている度合い


# バリマックス回転の因子分析
result_varimax <- fa(dat, 
                  nfactors = 2,#------>因子分析は先に因子の数を決めておかないと計算できない
                  fm = 'ml',#--------->推定方法。'ml'は最尤法
                  rotate = 'varimax')#--->回転の方法。'none'は無回転
print(result_varimax$loadings, #--------->因子負荷量
      cutoff = 0)#------------------------->相対的に小さい数値を消す。=0なら全て表示

matrix(result_varimax$Phi, ncol = 2)

#プロマックスを順番に
#バリマックス解をxに代入
x <- result_varimax$loadings

Q <- x * abs(x)^3 #absは絶対値。これは３乗でも４乗でも符号を変えないように操作

print(x, cutoff = 0)
print(Q, cutoff = 0)

#これは１次変換ではないので線形回帰する
#q=Q[,1]、q=Q[,2]でそれぞれ以下の計算をする
u <- data.frame(f1_load=x[,1], f2_load=x[,2], q = Q[,1])
lm(q~0 + f1_load + f2_load, data=u)

#一気にやる方法
U <- lm.fit(x,Q)$coefficients
U

#xUとすると因子得点の標準偏差が１にならない
#調整する
d <- diag(solve(t(U) %*% U)) #------------->tは転置、solveは
d 

#プロマックス解を出す
x %*% U %*% diag(sqrt(d))



# プロマックス回転の因子分析
# mtcarsで

dat <- mtcars
library(corrplot)
corrplot(cor(dat))

result_promax <-  fa(dat, 
                     nfactors = 2,#-------->因子分析は先に因子の数を決めておかないと計算できない
                     fm = 'ml',#----------->推定方法。'ml'は最尤法
                     rotate = 'Promax')#--->回転の方法。'none'は無回転

result_promax <- fa.sort(result)
print(result_promax$loadings) #------------>因子負荷量、サーストンの単純構造があると解釈しやすい

result_promax$communalities #---->　共通性

#因子間相関
result_promax$Phi











#--------------------------
# Loadings:
#           ML1    ML2   
# 麺             0.654
# 具      0.296       
# スープ  0.816       

# ML1は具とスープの美味しさ
# ML2は麺の美味しさ

# 因子間の相関行列を見てみると
matrix(result_promax$Phi, ncol = 2)

#因子得点
result_promax$scores #------->平均0分散1になるようになっている

#可視化
plot(result_promax$scores, 
     type='n')#-------------------->点を出さない
text(result_promax$scores, 
     labels = rownames(dat),#------>点ではなく店名を出す
     cex = 0.7)#------------------->文字の大きさ





     