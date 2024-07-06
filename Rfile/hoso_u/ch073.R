h1 <- read.table("ch071.dat",h=T,row.names=1)
h2 <- lm(weight~height,data=h1)
plot(h1,pch=16)
abline(h2)
