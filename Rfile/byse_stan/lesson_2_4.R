install.packages("rstan", repos="https://cloud.r-project.org/", dependencies=TRUE)
install.packages("tidyverse")
install.packages("ggfortify")
install.packages("bayesplot")
library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE) 
# TRUEにしてMCMCを行うと、[.rds]拡張子のRDSファイルが生成される。２回目以降コンパイル不要になる
options(mc.cores = parallel::detectCores())
# 計算を並列化して計算処理速度UP

file_beer_sales_1 <- read.csv(file.choose())
head(file_beer_sales_1)

# MCMCの実行にはデータをリスト形式にしておく必要がある
# stanのdataブロックにはサンプルサイズと売上を宣言しているのでこれをリスト化

N <- nrow(file_beer_sales_1)# サンプルサイズは行数を数えると得られる

dat_list <- list(sales = file_beer_sales_1$sales, N = N)
dat_list

# MCMCの実行
mcmc_result <- stan(
  file = "lesson_2_4stan.stan"  # 使用するstanファイルの指定
  ,data = dat_list              # 使用するデータ
  ,seed = 1                     # 乱数の種を指定して再現できるよにする
  ,chains = 4                   # 同時に走らせるMCMCの数
  ,iter = 2000                  # 乱数生成の繰り返し回数（１チェーンあたり）
  ,warmup = 1000                # バーンイン期間：初期値依存性の緩和
  ,thin = 1                     # 間引き数：自己相関の緩和のため得られた乱数を間引く->1なら間引きなし
)
mcmc_result  # 2.5?97.5の幅がベイズ信用区間
###  結果  ########################################################################################
#         mean se_mean   sd    2.5%     25%     50%     75%   97.5% n_eff Rhat
# mu     102.22    0.03 1.83   98.61  100.97  102.23  103.45  105.79  3283    1  -> 母平均
# sigma   18.19    0.02 1.27   15.93   17.30   18.12   18.99   20.98  2758    1  -> 母標準偏差
# lp__  -336.43    0.02 0.96 -338.94 -336.81 -336.13 -335.74 -335.48  1905    1  -> 対数確率密度
###################################################################################################

# mcmc収束の確認
# トレースプロット：バーンイン期間なし
traceplot(mcmc_result)
# トレースプロット：バーンイン期間あり
traceplot(mcmc_result, inc_warmup = TRUE)



# lesson_2_5:MCMC結果の評価

# mcmcサンプルの抽出
# mcmcの結果はstanfitというクラスに格納されている。これをそのまま扱うのはやや面倒。extractで抽出
mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)
class(mcmc_sample) # 配列型になっている
dim(mcmc_sample)   # 繰り返し数、Chain、推定されたパラメタが格納されている

# 各名称をみておく
dimnames(mcmc_sample)

# １回目のchainで得られた最初のMCMCサンプルにおける母平均を抽出　※バーンイン後
mcmc_sample[1, "chain:1", "mu"]
# １回目のchainで得られた全てのMCMCサンプルにおける母平均を抽出　※バーンイン後
chain_1 <- mcmc_sample[, "chain:1", "mu"]
length(chain_1)

# mcmc代表値の計算
mu_mcmc_vec <- as.vector(mcmc_sample[,,"mu"])

# 事後中央値
median(mu_mcmc_vec)
# 事後期待値
mean(mu_mcmc_vec)
#  95%ベイズ信頼区間
quantile(mu_mcmc_vec, probs = c(0.025, 0.975))

#トレースプロットの描画
library(ggfortify)

autoplot(ts(mcmc_sample[,,"mu"]) # タイムシリーズの折れ線グラフなのでts化
         ,facets = F             # 4つのchainを１つのグラフで描画 
         ,ylab = "mu"       
         ,main = "traceplot")

# 事後分布の可視化
# MCMCサンプルをまとめてカーネル密度推定するとパラメータの事後分布になる
mu_df <- data.frame(
  mu_mcmc_sample = mu_mcmc_vec
)

ggplot(data = mu_df, mapping = aes(x = mu_mcmc_sample))+
  geom_density(size = 1.0)

#bayesplotを使って簡潔に
library(bayesplot)

# ヒストグラムの描画
mcmc_hist(mcmc_sample, pars = c("mu", "sigma"))
# カーネル密度推定の描画
mcmc_dens(mcmc_sample, pars = c("mu", "sigma"))
# トレースプロット
mcmc_trace(mcmc_sample, pars = c("mu", "sigma"))
# トレースプロットと事後分布をまとめて描画
mcmc_combo(mcmc_sample, pars =  c("mu", "sigma"))


# 事後分布の範囲の確認
mcmc_intervals(
  mcmc_sample, pars = c("mu", "sigma")
  ,prob = 0.8         # 太い線の範囲の累積分布関数
  ,prob_outer =  0.95 # 細い線の範囲の累積分布関数
)

# 密度を一緒に描画
mcmc_areas(
  mcmc_sample, pars = c("mu", "sigma")
  ,prob = 0.6
  ,prob_outer = 0.99
)

# mcmcmサンプルのコレログラム:自己相関を評価する
mcmc_acf_bar(mcmc_sample, pars = c("mu", "sigma"), )

# 事後予測チェック
# 動物の観測数データをポアソン分布と正規分布で予測してみる
animal_num <- read_csv(file.choose())
head(animal_num)

# stanに渡すデータの準備
sample_size <- nrow(animal_num)
dat_list <- list(animal_num = animal_num$animal_num, N = sample_size)

# MCMCの実行
mcmc_normal <- stan(
  file = "2_5_1_normal-dist.stan"
  ,data = dat_list
  ,seed = 1
)
mcmc_poisson <- stan(
  file = "2_5_1_poisson-dist.stan"
  ,data = dat_list
  ,seed = 1
)

#事後予測値のmcmcサンプルを取得
y_rep_normal <- rstan::extract(mcmc_normal)$pred
y_rep_poisson <- rstan::extract(mcmc_poisson)$pred

dim(y_rep_poisson)

# パッケージを使わない場合の描画
hist(animal_num$animal_num)
hist(y_rep_normal[1,])
hist(y_rep_poisson[1,])

# パッケージを使う場合の描画
ppc_hist(y = animal_num$animal_num, yrep = y_rep_normal[1:5,])
ppc_hist(y = animal_num$animal_num, yrep = y_rep_poisson[1:5,])



