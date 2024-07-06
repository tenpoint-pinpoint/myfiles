hist(
  rnorm(100000,0,1),
  breaks = "scott",
  freq = F,
  xlim = c(-4, 4),
  ylim = c(0, 0.5)
)
curve(dnorm(x), add = T)
