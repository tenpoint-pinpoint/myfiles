plotSSM <- function(mcmc_sample,           # mcmcサンプル
                    time_vec,              # 時間軸のベクトル(POSIXct型)
                    obs_vec = NULL,        # 観測値のベクトル
                    state_name,            # 図示する状態の変数名
                    graph_title,           # グラフタイトル
                    y_label,               # y軸のラベル
                    date_labels = "%Y-%m"  # 日付の書式
){
  # 全ての時点の状態における95％区間と中央値を取得
  result_df <- data.frame(t(apply(
    X = mcmc_sample[[state_name]],
    MARGIN = 2,
    FUN = quantile,
    probs = c(0.025, 0.5, 0.975)
  )))
  # 列名の変更
  colnames(result_df) <- c("lwr","fit","upr")
  # 時間軸の追加
  result_df$time <- time_vec
  # 観測値の追加
  if(!is.null(obs_vec)){
    result_df$obs <- obs_vec
  }
  # 図示
  graph <-ggplot(data = result_df, mapping = aes(x = time))+
    labs(title = graph_title)+
    ylab(y_label)+
    geom_line(aes(y = fit), size = 1.2)+
    geom_ribbon (aes(ymin = lwr, ymax = upr), alpha = 0.3)+
    scale_x_datetime(date_labels = date_labels)
  # 観測値をグラフに追加
  if(!is.null(obs_vec)){
    graph <- graph + geom_point(alpha = 0.6,size = 0.9,
                                data = result_df, aes(x = time, y = obs))
  }
  return(graph)
}
