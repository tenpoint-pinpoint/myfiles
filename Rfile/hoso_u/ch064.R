ite <- 10000
sc <- numeric(ite)
for(i in 1:ite){
  x<- runif(500)
  y<-ifelse(x<0.4,1,ifelse(x<0.7,2,ifelse(x<0.9,3,4)))
  z1 <- table(y)
  z2 <- c(40,30,20,10)*5
  z3 <- (z1-z2)^2 /z2
  sc[i] <- sum(z3)
}
hist(sc,ylim=c(0,0.35),breaks="Scott",freq=F)
curve(dchisq(x,3),add=T)