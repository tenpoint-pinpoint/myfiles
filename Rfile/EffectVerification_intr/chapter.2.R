#バイアスのあるデータでの
biased_reg <- lm(data = biased_data, formula = spend ? treatment + history)
summary(biased_reg)

#回帰分析の結果のうち、Coefficientsだけを表示する
library("broom")
biased_reg_coef <- tidy(biased_reg) #tidyで回帰分析の結果をデータフレームにしてくれる

#効果検証のための回帰分析は介入変数以外のパラメータや有意性に興味はないので気にしない
#共変量の追加でバイアスの除去

rct_reg <- lm(data = male_df,formula = spend ? treatment)
rct_reg_coef <- summary(rct_reg) %>% tidy()

nonrct_reg <- lm(data = biased_data,formula = spend ? treatment)
nonrct_reg_coef <- summary(nonrct_reg) %>% tidy()

#バイアスのあるデータで重回帰
nonrct_mreg <- lm(data = biased_data,formula = spend ? treatment + recency + channel + history)
nonrct_mreg_coefg <- tidy(nonrct_mreg)
nonrct_mreg_coefg

#脱落変数バイアス（OVB）の確認
#モデル式のベクトルを用意
formula_vec <- c(spend ? treatment + recency + channel, #モデルA
                 spend ? treatment + recency + channel + history, #モデルB
                 history ? treatment + recency + channel) #モデルC
#formulaに名前をつける
names(formula_vec) <- paste("reg",LETTERS[1:3],sep = "_")

#データをデータフレームに変換する
models <- formula_vec %>%
  enframe(name = "model_index", value = "formula")
#mapを利用して回帰分析
#mapは.xで受け取ったデータの一つ一つに対し.fで入力した関数を実行する
df_models <- models %>%
  mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))
#モデルの結果を整形
df_results <- df_models %>%
  mutate(formula = as.character(formula)) %>%
  select(formula, model_index, lm_result) %>%
  unnest(cols = c(lm_result))  
#知りたいパラメータは
#「α1-β1（β1＝selectionbiaseを発生させる変数が説明変数として入っている時の介入変数の回帰係数
#        (α1=selectionbiaseを発生させる変数が説明変数として入っていない時の介入変数の回帰係数）」と
#「γ1β4（γ1=selectionbiaseを発生させている変数のモデル挿入時における介入変数の回帰係数）
#       (β4=selectionbiaseを発生させている変数のモデル挿入時の回帰係数」の２つ

#モデルA,B,Cからtreatmentのパラメータを抜き出す（α1、β1、γ1）
treatment_coef <- df_results %>%
  filter(term == "treatment") %>%
  pull(estimate)
#モデルBからhistoryのパラメータを抜き出す（β4）
history_coef <- df_results %>%
  filter(model_index =="reg_B",
         term == "history") %>%
  pull(estimate)
df_results
#OVBの確認
OVB <- history_coef*treatment_coef[3] # γ1β4
coef_gap <- treatment_coef[1]-treatment_coef[2] #α1-β1
OVB 
coef_gap



