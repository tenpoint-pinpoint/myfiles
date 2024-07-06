a1 <- round( runif(100)*100,0 )
summary(a1)
a2 <- cut(a1,breaks=c(0,40,101),right=F,label=c("U40","O40"))
table(a2)