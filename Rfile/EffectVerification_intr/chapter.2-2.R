install.packages("devtools")
devtools::install_github("itamarcaspi/experimentdatar")

library("experimentdatar")
library("broom")
library("tidyverse")
data(vouchers)
vouchers

#回帰式の再現

#文字列の準備
formula_x_base <- "VOUCH0"
formula_x_covariate <- "SVY + HSVISIT + AGE + STRATA1 + STRATA2 + STRATA3 + STRATA4 + STRATA5 + STRATA6 + STRATAMS + 
                        D1993 + D1995 + D1997 + DMONTH1 + DMONTH2 + DMONTH3 + DMONTH4 + DMONTH5 + DMONTH6 + DMONTH7 + 
                        DMONTH8 + DMONTH9 + DMONTH10 + DMONTH11 + DMONTH12 + SEX2"
formula_y <- c("TOTSCYRS","INSCHL","PRSCH_C","USNGSCH","PRSCHA_1","FINISH6","FINISH7","FINISH8","REPT6","REPT","NREPT",
               "MARRIED","HASCHILD","HOURSUM","WORKING3")

#formula_yに対して共変量を含まない回帰式の作成
base_reg_formula <- paste(formula_y, "?", formula_x_base)
names(base_reg_formula) <- paste(formula_y, "base", sep = "_")
base_reg_formula
#formula_yに対して共変量を含む回帰式の作成
covariate_reg_formula <- paste(formula_y, "?", formula_x_base, "+", formula_x_covariate)
names(covariate_reg_formula) <- paste(formula_y, "covariate", sep = "_")
covariate_reg_formula
#モデル式のベクトルを作成
table3_formula <- c(base_reg_formula, covariate_reg_formula) 

seq_len(3) %>% enframe("col","sol")