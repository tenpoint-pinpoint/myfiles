#### ローカルレベルモデル
library(rstan)
library(bayesplot)
library(ggfortify) # 時系列データの可視化ライブラリ
library(gridExtra) # グラフの一覧表示
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## ホワイトノイズとランダムウォークの比較
set.seed(1)
# ホワイトノイズ
wn <- rnorm(n = 100, mean = 0, sd = 1)
# ランダムウォーク
rw <- cumsum(wn) # 累積和をとる関数
# 可視化
wn_1 <- autoplot(ts(wn), main = "writenoise")
rm_1 <- autoplot(ts(rw), main = "randomwork")
grid.arrange(wn_1, rm_1)

## 複数のwnとrwを作ってみる
wn_mat <- matrix(nrow = 100, ncol = 20)
rw_mat <- matrix(nrow = 100, ncol = 20)

set.seed(1)
for(i in 1:20){
  wn <- rnorm(n = 100, mean = 0, sd = 1)
  wn_mat[,i] <- wn
  rw_mat[,i] <- cumsum(wn)
}
# facets = Fで複数時系列でもグラフわけないようにできる
wn_2 <- autoplot(ts(wn_mat), facets = F, main = "writenoise")+
  theme(legend.position = "none")
rw_2 <- autoplot(ts(rw_mat), facets = F, main = "randomwork")+
  theme(legend.position = "none")
grid.arrange(wn_2, rw_2)


# ローカルレベルモデルは状態がランダムウィークし観測がホワイトノイズ
# ランダムウォークで状態（=パラメタ）が決まり、ホワイトノイズで観測値が決まる
# このことから階層ベイズモデルで記述することができる。以下stanで実装

sales_df <-read.csv(file.choose())
sales_df$date <- as.POSIXct(sales_df$date) # dateを計算しやすいように
head(sales_df)
dat_list <- list(
  y = sales_df$sales,
  T = nrow(sales_df)
)

local_level_stan <- stan(
  file = "local_level.stan",
  data = dat_list,
  seed = 1
)
# 収束の確認 -> 問題なさそう
mcmc_rhat(rhat(local_level_stan))

print(local_level_stan,
      pars = c("s_w", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975)
)
# 出力結果 mean se_mean    sd    2.5%     50%   97.5% n_eff Rhat
#  s_w     1.30    0.02  0.30    0.85    1.26    1.98   277 1.01
#  s_v     2.87    0.01  0.26    2.38    2.86    3.42  1926 1.00
#  lp__ -226.09    1.16 17.50 -260.91 -225.60 -193.26   227 1.01

# 推定された状態の図示
mcmc_sample <- rstan::extract(local_level_stan)
# 1時点目の95%ベイズ信用区間と中央値を取得
quantile(mcmc_sample[["mu"]][,1],probs = c(0.025, 0.5, 0.975))
# 上記をapply関数を用いて100時点分取得
result_df <- data.frame(t( # tは転置の関数
  apply(
    X = mcmc_sample[["mu"]], # 実行対象のデータ
    MARGIN = 2,              # 列を対象にループ
    FUN = quantile,          # 実行対象となる関数
    probs = c(0.025, 0.5, 0.975) # 上記関数への引数
  ))
)

# 図示データ作成のための調整
# 95%信用区間なので2.5%点:lwr、50%点:fit、97.5%点:uprとしている
colnames(result_df) <- c("lwr", "fit", "upr") # 列名変更
result_df$time <- sales_df$date               # 時間軸の追加
result_df$obs  <- sales_df$sales              # 観測値の追加
head(result_df)
# 図示
ggplot(data = result_df, mapping = aes(x = time, y = obs))+
  labs(title = "local_level_modeling")+
  ylab("sales")+
  geom_point(alpha = 0.6, size = 0.9)+
  geom_line(aes(y = fit), size = 1.2, color = "red")+
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3)+
  scale_x_datetime(date_labels = "%Y-%m")


plotSSM(mcmc_sample = mcmc_sample,
        time_vec = sales_df$date,
        obs_vec = sales_df$sales,
        state_name = "mu",
        graph_title = "estimate_local_level",
        y_label = "sales"
        )
