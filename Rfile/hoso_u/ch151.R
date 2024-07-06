library(RMeCab)
a1 <- docMatrix("C15",pos=c("連体詞","副詞"))
a2 <- docMatrix("C15",pos=c("連体詞","副詞"),2)
a3 <- a1[row.names(a1)!="[[LESS-THAN-1]]",]
a4 <- a3[row.names(a3)!="[[TOTAL-TOKENS]]",]
write.table(a4,"hindo.txt")
c1 <- t(a4)
c2 <- dist(c1)
c3 <- cmdscale(c2,eig=T)
c4 <- c3$points
c5 <- kmeans(c4,3)
c6 <- c5$cluster
plot(c4,xlim=c(-25,30),ylim=c(-12,16))
text(c4,row.names(c1),col=c6,pos=3)