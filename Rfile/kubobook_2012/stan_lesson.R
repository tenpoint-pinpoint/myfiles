#pkgbuild::has_build_tools(debug = TRUE)
#install.packages('rstan', repos='https://cloud.r-project.org/', dependencies=TRUE)

# ˆÈ‰º‚Í‹N“®‚Ì‚½‚Ñ‚ÉÀs‚·‚é
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit <- stan(file = "./schools.stan", data = schools_dat)
fit
