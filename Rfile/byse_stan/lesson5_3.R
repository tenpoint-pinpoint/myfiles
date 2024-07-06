library(rstan)
library(bayesplot)
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())
source("plotSSM.R", encoding = "utf-8")

sales_df_all <- read.csv(file.choose())
sales_df_all$date <- as.POSIXct(sales_df_all$date)
head(sales_df_all)


### ローカルレベルモデルの予測実行
dat_list <- list(
  T = nrow(sales_df_all),
  y = sales_df_all$sales,
  pred_term = 20         # 20時点先まで予測してみる
) 
local_level_pred <- stan(
  file = "5_3_local_level_pred.stan",
  data = dat_list,
  seed = 1
)
# 収束の確認 -> ひとまず問題はなさそう
mcmc_rhat(rhat(local_level_pred))

# 予測のための箱を用意
date_plot <- seq( #seqは連番作成の関数
  from = as.POSIXct("2010-01-01"), # 2020/1/1から
  by = "days",                     # １日毎に
  len = 120                        # 120個(=120日分)
)
date_plot
# 乱数の取り出し
mcmc_sample_pred <- rstan::extract(local_level_pred)
# 予測の図示
plotSSM(
  mcmc_sample = mcmc_sample_pred,
  time_vec = date_plot,
  state_name = "mu_pred",
  graph_title = "predict",
  y_label = "sales"
)
# 過程誤差の期待値は0、これは予測期間全てにおいて同じ
# よって、データが得られた最後の時点の状態推定値が次の予測値になる
# グラフでは予測期間が平坦な値をとり、かつ95%信用区間が広がり続ける


### 欠損データの取り扱い
sales_df_NA <- read.csv(file.choose())
sales_df_NA$date <- as.POSIXct(sales_df_NA$date)
head(sales_df_NA)

# 欠損行を削除する
sales_df_NAomit <- na.omit(sales_df_NA)
# 欠損していた行数をカウント
nrow(sales_df_NA) - nrow(sales_df_NAomit)
# 欠損を判定する
!is.na(sales_df_NA$sales) # naじゃなければTRUEとしているので、FALSEが欠損データ
# データがある行番号を取得
which(!is.na(sales_df_NA$sales)) # which関数hじゃTRUEとなるレコードのindexを取得する

# ローカルレベルモデルでの補間の実行
dat_list_in <- list(
  T = nrow(sales_df_NA),
  len_obs = nrow(sales_df_NAomit),
  y = sales_df_NAomit$sales,
  obs_no = which(!is.na(sales_df_NA$sales))
) 

local_level_in <- stan(
  file = "5_3_local_level_interpolation.stan",
  data = dat_list_in,
  seed = 1,
  iter= 6000 # 収束をよくするため繰り返しを増やした
)

# 生成された乱数取り出し
mcmc_sample_in <- rstan::extract(local_level_in) 
plotSSM(
  mcmc_sample = mcmc_sample_in,
  time_vec = sales_df_all$date,
  obs_vec = sales_df_all$sales,
  state_name = "mu",
  graph_title = "hokan",
  y_label =  "sales"
)
# ここで表示されている95%信用区間は状態の95%信用区間
# 観測値の95％信用区間は以下で生成
local_level_pred <- stan(
  file = "5_3_pred.stan",
  data = dat_list_in,
  seed = 1,
  iter= 6000 # 収束をよくするため繰り返しを増やした
)
sample_pred <- rstan::extract(local_level_pred, permuted = F)

plotSSM(
  mcmc_sample = sample_pred,
  time_vec = sales_df_all$date,
  obs_vec = sales_df_all$sales,
  state_name = "y_pred",
  graph_title = "y_pred",
  y_label =  "sales"
)
# 復習のため推定したパラメタのチェック
print(local_level_pred, pars = c("s_v", "s_w"))        # 特定のパラメタ取得
print(local_level_pred, probs = c(0.025, 0.5, 0.975))  # 95%ベイズ信用区間と予測区間の算出


