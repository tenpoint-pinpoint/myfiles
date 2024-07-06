z2 <- matrix(c(3,12,12,3,9,16,8,17),
             ncol = 2, nrow = 4, byrow = T)
rownames(z2) <- c("国語","算数","社会","理科")
colnames(z2) <- c("男","女")

z2
#カイ二乗検定
result01 <- chisq.test(z2)
result01

#カイ二乗検定を行うと、residuals1というパラメータが生成されている
#これが標準化残差、これは近似的に正規分布に従う
#残差とは、　観測値　ー　期待値
#これを標準偏差で割ったものが標準化残差
result01$residuals
#こっちは調整済み残差
#標準化残差とその分散を用いて標準化変換を行い、
#近似的に平均０、標準偏差１の標準正規分布に従うように調整したもの
#有意水準５％の検定の場合には、この値が1.96よりも大きいと特徴的な箇所とみなせる
result01$stdres
#p値の計算
pnorm(abs(result01$stdres), lower.tail = FALSE) *2

#全体として差があったのでライアンの方法で多重比較
source("http://aoki2.si.gunma-u.ac.jp/R/src/p_multi_comp.R", encoding="euc-jp")
p.multi.comp(c(15,15,25,25),c(3,12,9,8), method="ryan")

