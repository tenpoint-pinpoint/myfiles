install.packages('psych') #--------> パッケージインストール
library(psych) #-------------------> このライブラリのfa関数で因子分析を実行可能

dat <- read.csv('./da_lec/fa/sitedata.csv',header=T, fileEncoding = 'utf-8', row.names = 'id')
str(dat)

# 無回転の因子分析 _ 最尤法
result_n <- fa(dat,
               nfactors = 2,#------> 因子分析は先に因子の数を決めておく
               fm = 'ml',#---------> 推定方法、'ml'は最尤法。'ols'で最小二乗法。デフォルトは'minres'で最小残差法
               rotate = 'none')#---> 回転の方法、'none'は無回転

print(result_n$loadings, #---------> 因子負荷量
      cutoff = 0) #----------------> 相対的に小さい数値を表示しない。=0なら全て表示

# 無回転解は解釈しやすいとは限らない
# 解釈しやすい = 0から近いか遠いかのメリハリがついている状態
# これを数学的に表現したのがサーストンの単純構造の条件（調べてみてください）

# 共通性の計算。１に近いほどデータを説明できている度合い
result_n$communalities



# バリマックス回転の因子分析
result_varimax <- fa(dat, 
                     nfactors = 2,#---------> 因子分析は先に因子の数を決めておかないと計算できない
                     fm = 'ml',#------------> 推定方法。'ml'は最尤法
                     rotate = 'varimax')#---> 回転の方法

print(result_varimax$loadings, #------------> 因子負荷量
      cutoff = 0)#--------------------------> 相対的に小さい数値を消す。=0なら全て表示
# 結果 -----------------------------------------------------------------------------------------------
#                  ML1   ML2
# SS loadings    1.367 0.627   因子寄与、因子負荷の2乗和.因子が説明できる観測変数の分散の大きさ
# Proportion Var 0.456 0.209   因子寄与率、因子寄与を項目数で割った値
# Cumulative Var 0.456 0.665   累積寄与率
# ----------------------------------------------------------------------------------------------------

# プロマックス回転の因子分析
result_promax <- fa(dat,
                    nfactors = 2,#---------> 因子分析は先に因子の数を決めておかないと計算できない
                    fm = 'ml',#------------> 推定方法。'ml'は最尤法
                    rotate = 'Promax')#---> 回転の方法

print(result_promax$loadings, #------------> 因子負荷量
      cutoff = 0.1)#--------------------------> 相対的に小さい数値を消す。=0なら全て表示


# 因子間相関
matrix(result_promax$Phi, ncol = 2)

# バリマックスで因子間相関をみようとすると、そもそも計算していないので出力されません
matrix(result_varimax$Phi, ncol = 2)


# 因子得点を求める
result_promax$scores


# 因子得点を散布図に
par(family = "ヒラギノ角ゴシック W3") # macの人はうまくいかない時これを実行してから散布図を出力してください
plot(result_promax$scores, 
     type='n')#--------------------> 点を出さない
text(result_promax$scores, 
     labels = rownames(dat),#------> 点ではなくidを出す
     cex = 0.7)#-------------------> 文字の大きさ

# 緩い負の相関が見られる
# 使いやすさ、デザイン、価格という３つの要素を２つの因子で解釈した時、どんな因子名が妥当そうでしょうか？



# プロマックス解をバリマックス解から導出----------------------------------#
x <- result_varimax$loadings

# ４乗した行列
Q <- x * abs(x)^3 # 符号は元のまま保つ

# Qを見るとメリハリがよりついたように見える
print(x, cutoff = 0)
print(Q, cutoff = 0)

# 一次変換を表現する行列を計算、q = Q[,2]も同様に計算する
u <- data.frame(fa1 = x[,1], fa2 = x[,2], q = Q[,1])
lm(q ~ 0 + fa1 + fa2, data = u)
# 上記式を一気にやる方法
U <- lm.fit(x, Q)$coefficients
U

# xUのままだと因子得点の標準偏差が1にならないので調整する行列を作る　※詳細は数学的に難解なので省略
d <- diag(solve(t(U) %*% U)) # diagは行列生成、solveは逆行列、tは転置

# これがプロマックス解
x %*% U %*% diag(sqrt(d))
# fa関数で出した解と比較してみる
print(result_promax$loadings, cutoff = 0)

#-------------------------------------------------------------------------#
# 改めてプロマックス回転の結果を見てみる

# print(result_promax, cutoff = 0.1)
# Factor Analysis using method =  ml
# Call: fa(r = dat, nfactors = 2, rotate = "Promax", fm = "ml")

# Standardized loadings (pattern matrix) based upon correlation matrix
#          ML1   ML2    h2     u2 com
# use    -0.05 -0.14 0.014 0.9858 1.3
# design -0.54  0.56 0.991 0.0092 2.0
# price   0.98 -0.03 0.991 0.0088 1.0
# --->ここでh2は共通性、u2は独自性。２つの因子からuseはほとんど説明できない

# ML1  ML2
# SS loadings           1.45 0.54
# Proportion Var        0.48 0.18
# Cumulative Var        0.48 0.67
# Proportion Explained  0.73 0.27
# Cumulative Proportion 0.73 1.00

# With factor correlations of 
# ML1   ML2
# ML1  1.00 -0.61
# ML2 -0.61  1.00

# Mean item complexity =  1.4
# Test of the hypothesis that 2 factors are sufficient.

# The degrees of freedom for the null model are  3  and the objective function was  1.64 0.1 with Chi Square of  11.78
# The degrees of freedom for the model are -2  and the objective function was  0 
# 0.1
# The root mean square of the residuals (RMSR) is  0 
# The df corrected root mean square of the residuals is  NA 
# 0.1
# The harmonic number of observations is  10 with the empirical chi square  0  with prob <  NA 
# 0.1The total number of observations was  10  with Likelihood Chi Square =  0  with prob <  NA 
# 0.1
# Tucker Lewis Index of factoring reliability =  1.455
# Fit based upon off diagonal values = 1
# Measures of factor score adequacy             
# ML1  ML2
# Correlation of (regression) scores with factors   1.00 0.98
# Multiple R square of scores with factors          0.99 0.96
# Minimum correlation of possible factor scores     0.98 0.93


