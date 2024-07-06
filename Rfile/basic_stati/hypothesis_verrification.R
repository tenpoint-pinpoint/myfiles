#仮説検定


 #母平均の検定
  #標本のp観測値をxに代入
x <- c(13,16,10,17)
  #t検定　※１標本の母平均のt検定
t.test(x, mu=8, alternative="greater")
    #mu : 任意の母平均値
    #alternative = 指定なし  : 両側検定
    #alternative = "greater" : 上側検定
    #alternative = "less"    : 下側検定

  #検定統計量の実現値
   #t値の算出
t <- (mean(x)-8)/sqrt(var(x)/4)
t
   #p値の算出
p <- 1-pt(t,3)
p

 #母分散の検定
  #標本の観測値をxに代入
x <- c(21,17,19,14,16,15)

  #検定統計量の実現値 : t値
chisq <- (6-1)*var(x)/10
chisq
  #p値
pchisq(chisq,5)  #カイ二乗分布の関数(母分散検定の関数は用意されていない)


#カイ二乗検定
 #適合度検定：実測度数が特定の分布に適合（一致）するかどうかの検定
 #帰無仮説は「実測度数は理論度数分布と一致する」
 #適合度検定は片側検定
   #観測度数をoに代入
o <- c(13,7,8,14,7,11)
   #帰無仮説の確率をpに代入
prob <- c(1/6,1/6,1/6,1/6,1/6,1/6)
   #適合度の検定
chisq.test(o, p=prob)
   #期待度数
e <- sum(o)/6
   #検定統計量の実現値
chisq <- sum((o-e)^2/e)
chisq
   #p値
1-pchisq(chisq,5)

 #独立性検定:２つ以上の分類基準間に関連があるかどうか
 #帰無仮説は「独立である（関連がない）」
 #独立性検定は片側検定
#観測度数をoに代入
o <- matrix(c(42,28,19,11,16,24,23,37), nrow=2)
#独立性の検定
chisq.test(o)

 #関数を使わずに順番に計算
   #標本の大きさをnに代入
n <- sum(o)
n
   #行の周辺度数:o.row、列の周辺度数:o.colに代入
o.row <- rowSums(o)
o.col <- colSums(o)
o.row
o.col
   #期待度数を求める : 対応する行・列の周辺度数の積を標本の大きさで割る
e <- matrix(NA, nrow=2, ncol=4)
e <- matrix(NA,2,4)
e[1,1] <- o.row[1]*o.col[1] / n
e[1,2] <- o.row[1]*o.col[2] / n
e[1,3] <- o.row[1]*o.col[3] / n
e[1,4] <- o.row[1]*o.col[4] / n
e[2,1] <- o.row[2]*o.col[1] / n
e[2,2] <- o.row[2]*o.col[2] / n
e[2,3] <- o.row[2]*o.col[3] / n
e[2,4] <- o.row[2]*o.col[4] / n
e
   #検定統計量の実現値
chisq <- sum((o-e)^2/e)
chisq
   #p値
1-pchisq(chisq, 3)  #自由度3で算出



  #t検定  ※対応のない２標本のt検定 
   #母分散が等しいと仮定(studentのt検定)
a <- c(10,12,9,15,8,2)
b <- c(20,18,14,23,16)
t.ans1 <- t.test(a,b,var=T)
t.ans1
   #母分散が等しくないと仮定(ウェルチのt検定)
t.ans2 <- t.test(a,b,var=F)
t.ans2
   #ノンパラメトリック（ウィルコクソン順位和検定)
t.ans3 <- wilcox.test(a,b)
t.ans3

  #t検定　※対応のある２標本のt検定
c <- c(10,11,6,3,19,14)
d <- c(16,15,8,4,21,12)
t.ans4 <- t.test(c,d,paired=T)
t.ans4
  #ノンパラメトリック（ウィルコクソン検定)
t.ans5 <- wilcox.test(c,d)
t.ans5
