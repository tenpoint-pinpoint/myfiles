pc1 <- read.csv("ch054.csv",header=T,row.names=1)
pc1[,2] <- factor(pc1[,2],levels=
c("まったくそう思わない","あまりそう思わない",
"ややそう思う","とてもそう思う") )
pc1[,3] <- cut(pc1[,1],c(0,30,40,50,60,70,100),right=F,
labels=c("30歳未満","30代","40代","50代","60代","70歳以上"))
pie( table(pc1[,3]),clockwise=T)
pc2 <- table(pc1[,2],pc1[,3])
barplot(pc2,col=rainbow(4),las=2
        ,legend= rownames(pc2),
        args.legend=list(x=3.5,y=140))
          