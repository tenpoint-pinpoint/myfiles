ite <- 10000
trial <- 10000
p <- 0.3
hist( (rbinom(ite,trial,p)-trial*p)/sqrt( trial*p*(1-p) ), breaks="scott" ,freq=F,xlim=c(-4,4),ylim=c(0,0.5))
curve(dnorm(x),add=T)