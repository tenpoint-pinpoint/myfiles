km1 <- read.table("ch112.dat",h=T,row.names=1) 
km2 <- kmeans(km1,2)
plot(km1,pch=km2$cluster)
points(km2$center,pch=8)
