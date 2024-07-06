library(tidyverse)

# ---------------------------------------------------------------------------------------------------------------------------------------------
# データの読み込み
# ---------------------------------------------------------------------------------------------------------------------------------------------
iris <- iris %>% as_tibble()
iris
# ---------------------------------------------------------------------------------------------------------------------------------------------
# ヒストグラムの作成
# ---------------------------------------------------------------------------------------------------------------------------------------------
# 1つの変数(Sepal.Length)のヒストグラムを作成(ビンの数を指定)
iris %>% ggplot(aes(x = Sepal.Length)) +                                # ggplot(data, aes(x = 変数名)) でキャンバスの用意
  geom_histogram(bins = 10)                                             # geom_histgram(bins = α) でビンの数がαヒストグラムを作成

# 1つの変数のヒストグラムを作成(ビンの幅を指定)
iris %>% ggplot(aes(x = Sepal.Length)) +                                # ggplot(data, aes(x = 変数名)) でキャンバスの用意
  geom_histogram(binwidth = 0.5)                                        # geom_histgram(binwidth = α) でビンの幅がαのヒストグラムを作成

# レイアウトの指定
iris %>% ggplot(aes(x = Sepal.Length)) +                                
  geom_histogram(bins = 10, 
                 color = "black",                                       # color = "色" で外渕の色を指定
                 fill = "white")                                        # fill = "色" で塗りつぶし色を指定
# 色見本
# http://www.okadajp.org/RWiki/?%E8%89%B2%E8%A6%8B%E6%9C%AC


# 1つの変数のヒストグラムを作成(密度プロットを重ねる)
iris %>% ggplot(aes(x = Sepal.Length, y = ..density..)) +               # ggplot(data, aes(x = 変数名, y = ..density..))でy軸が確率密度のキャンパスを作成
  geom_histogram(binwidth = 0.5, fill = "white", color = "black") +     # fill = "色" で塗りつぶし色を指定。color = "色" で枠の色の指定
  geom_density(fill = "black", alpha = 0.3) +                           # geom_density() で密度曲線を作成。alpha = α で透過度を指定
  xlim(3,9)                                                             # xlim(α, β) でx軸の下限値がα、上限値がβの範囲を表示

# データの一部分を使ってヒストグラムを作成
iris %>% filter(Species == "setosa") %>%                                # filter()でデータを絞ってヒストグラムを作成
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5)


# facet機能------------------------------------------------------------------------------------------------------------------------------------
# facet_grid()で条件毎に別々のグラフを作成
iris %>% ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5) +
  facet_grid(. ~ Species)                                               # facet_grid(.~ 変数名) で、変数名でグループ分けしたグラフを横並びに表示

# facet_grid()で条件毎に別々のグラフを作成
iris %>% ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5) +
  facet_grid(Species ~ .)                                               # facet_grid(変数名 ~ .) で、変数名でグループ分けしたグラフを縦並びに表示

# facet_wrap()で条件毎に別々のグラフを作成
iris %>% ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(. ~ Species, nrow = 2)                                     # facet_wrap(.~ 変数名, nrow = α) で、行数がαとなるように表示


# facet_grid()とfacet_wrapの違いは2つの変数で層別化したグラフを作成する際に出る → facet_grig()は行列配置、facet_wrap()は入れ子構造
# Sepal.Width3という変数を作成。(Sepal.Widthが3より大きければ"over3", そうでなければ"under3"をとる変数)
iris.facet <- iris %>% mutate(Sepal.Width3 = if_else(Sepal.Width > 3, "over3", "under3"))
iris.facet

# SpeciesとSepal.width3で層別化したグラフをfacet_grid()を用いて作成
iris.facet %>% ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5) +
  facet_grid(Sepal.Width3 ~ Species)                                    # facet_grid(変数名1 ~ 変数名2) で、変数名1を行に、変数名2を列に配置

# SpeciesとSepal.width3で層別化したグラフをfacet_grid()を用いて作成
iris.facet %>% ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5) +
  facet_grid(Species ~ Sepal.Width3)                                    # 上のグラフの行と列を入れ替え


# SpeciesとSepal.width3で層別化したグラフをfacet_wrap()を用いて作成
iris.facet %>% ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(Species ~ Sepal.Width3)                                    # facet_wrap()を使うと変数の配置が入れ子構造となる

# SpeciesとSepal.width3で層別化したグラフをfacet_wrap()を用いて作成
iris.facet %>% ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(Sepal.Width3 ~ Species)                                    # 上のグラフの入れ子構造を反転


# 層別化------------------------------------------------------------------------------------------------------------------------------------------
# 層別ヒストグラムを作成(Speciesで層別化)
iris  %>% ggplot(aes(x = Petal.Length, fill = Species)) +               # fill = 変数名で変数名ごとにグラフを作成
  geom_histogram(bins = 10,
                 position = "identity",                                 # position = "identity" でグラフを重ねて描く
                 alpha = 0.4)                                           # alpha = α で透過度をαに設定。(0 < α < 1)

# 層別ヒストグラムを作成(Sepal.Width3で層別化)
iris.facet %>% ggplot(aes(x = Petal.Length, fill = Sepal.Width3)) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.4)



# ------------------------------------------------------------------------------------------------------------------------------------------------
# 散布図の作成
# ------------------------------------------------------------------------------------------------------------------------------------------------
# 散布図の作成(Sepal.LengthとSepal.Width)
iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +                            # aes(x = 変数名1, y = 変数名2) でx軸に変数名1をy軸に変数名2を配置
geom_point(size = 4, color = "blue", alpha = 0.4)                                  # sizeで点の大きさを、colorで色を、alphaで透過度を指定

# 回帰直線の追加
iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(size = 4, color = "blue") +
  geom_smooth(method = "lm")                                                         # geom_smooth(method = "lm") で回帰直線を追加


# 層別散布図の作成--------------------------------------------------------------------------------------------------------------------------------
# 質的変数で層別化(色で分ける)
iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +           # color = 質的変数で、変数の水準ごとに点の色が変わる
  geom_point(size = 4, alpha = 0.7) 

# 質的変数で層別化(形で分ける)
iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width, shape = Species)) +           # shape = 質的変数で、変数の水準ごとに点の形が変わる
  geom_point(size = 4,color = "blue", alpha = 0.7) 

# 量的変数で層別化(色で分ける)
iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +      # color = 量的変数で、変数の値ごとに点の色が変わる
  geom_point(size = 4)                                              

# facetを用いて複数の散布図を作成
iris.facet %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width), color = Species) +
  geom_point(size = 1,color = "red") +
  facet_wrap(. ~ Species, nrow = 3)

# チートシート
# https://www.rstudio.com/wp-content/uploads/2016/10/ggplot2-cheatsheet-2.0-ja.pdf

ggplot(data = iris, mapping = aes(x = Sepal.Length,y = Sepal.Width))

# 散布図行列--------------------------------------------------------------------------------------------------------------------------------------
# 散布図行列の作成
library(GGally)
iris %>% ggpairs(aes_string(color = "Species", alpha = 0.5))              # ggpairs(aes_string(color = "α", alpha = 0.5)) でαで層別化した散布図行列を作成

# 変数を絞って散布図行列を作成
iris %>% select(-Sepal.Length) %>% 
  ggpairs(aes_string(color = "Species", alpha = 0.5))                     # 変数を絞って散布図行列を作成

# タイトルの作成----------------------------------------------------------------------------------------------------------------------------------
iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(title = "タイトル",                                                # labs()でタイトルを指定する
       subtitle = "サブタイトル",
       caption = "キャプション",
       x = "x軸",
       y = "y軸",
       color = "カラー") +
  theme_gray (base_family = "HiraKakuPro-W3")                             # 文字化けを防ぐ



# ------------------------------------------------------------------------------------------------------------------------------------------------
# 箱ひげ図の作成
# ------------------------------------------------------------------------------------------------------------------------------------------------
# 箱ひげ図の作成
iris %>% ggplot(aes(x = Species, y = Sepal.Length)) +                                # aes(x = 質的変数, y = 量的変数) と指定
  geom_boxplot()                                                                     # geom_boxplot() で箱ひげ図を作成

# 箱ひげ図にデータ点を重ねる
iris %>% ggplot(aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  geom_jitter()                                                                      # geom_jitter() で箱ひげ図に実際のデータの分布を重ねる



# ------------------------------------------------------------------------------------------------------------------------------------------------
# バイオリンプロットの作成
# ------------------------------------------------------------------------------------------------------------------------------------------------
# バイオリンプロットの作成
iris %>% ggplot(aes(x = Species, y = Sepal.Length)) +                                # aes(x = 質的変数, y = 量的変数) と指定
  geom_violin(scale = "area")                                                        # geom_violin() でバイオリンプロットの作成

# バイオリンプロットの作成(面積一定)
iris.facet %>% ggplot(aes(x = Sepal.Width3, y = Sepal.Length)) +
  geom_violin(scale = "area")                                                        # scale = "area" でデータ個数によらず各プロットの面積が同じとなる

# バイオリンプロットの作成(面積をデータ数に依存させる)
iris.facet %>% ggplot(aes(x = Sepal.Width3, y = Sepal.Length)) +
  geom_violin(scale = "count")                                                       # scale = "count" でデータ個数に依存して各プロットの面積が変わる



# ------------------------------------------------------------------------------------------------------------------------------------------------
# 棒グラフの作成
# ------------------------------------------------------------------------------------------------------------------------------------------------

# statの指定--------------------------------------------------------------------------------------------------------------------------------------
# Sepal.Lengthの平均値をSpeciesごとに棒グラフで表示(mean1)
iris %>% group_by(Species) %>%                                                       # Speciesでグループ化
  summarise_if(is.numeric, mean) %>%                                                 # summarise_if()で数値型の変数の平均値を計算
  ggplot(aes(x = Species, y = Sepal.Length)) +                                       # x軸に種類を、y軸にSepal.Lengthを指定
  geom_bar(stat = "identity")                                                   #→デフォルトでstat="count"となっており、これでは個数を表示してしまう

# Sepal.Lengthの標準偏差をSpeciesごとに棒グラフで表示(sd1)
iris %>% group_by(Species) %>%                                                       # Speciesでグループ化
  summarise_if(is.numeric, sd) %>%                                                   # summarise_if()で数値型の変数の平均値を計算
  ggplot(aes(x = Species, y = Sepal.Length)) +                                       # x軸に種類を、y軸にSepal.Lengthを指定
  geom_bar(stat = "identity") +                                                      # stat = "identity" を指定してグラフ作成。
  labs(y = "Sepal.Length_sd")+                                                       # stat = "identity" を指定してグラフ作成。
  labs(y = "Sepal.Length_mean")    


# Sepal.Lengthの平均値をSpeciesごとに棒グラフで表示(mean2)
iris %>% ggplot(aes(x = Species, y = Sepal.Length)) +                                # geom_bar(stat = "summary", fun.y = "mean") で(mean1)と同じことができる  
  geom_bar(stat = "summary", fun.y = "mean") +                                       # stat = "summary"でデータを要約することを指定し、
  labs(y = "Sepal.Length_mean")                                                      # fun.y = "mean"でy軸の変数にmean()を適用することを指定

# Sepal.Lengthの標準偏差をSpeciesごとに棒グラフで表示(sd2)
iris %>% ggplot(aes(x = Species, y = Sepal.Length)) +                                # geom_bar(stat = "summary", fun.y = "sd") で(sd1)と同じことができる
  geom_bar(stat = "summary", fun.y = "sd") +                                         # stat = "summary"でデータを要約することを指定し、
  labs(y = "Sepal.Length_sd")                                                        # fun.y = "mean"でy軸の変数にsd()を適用することを指定


# Sepal.Lengthの平均値をSpeciesごとに棒グラフで表示(mean3)
iris %>% ggplot(aes(x = Species, y = Sepal.Length)) +                                # stat_summary(geom = "bar", fun.y = "mean") で(mean1)と同じことができる
  stat_summary(geom = "bar", fun.y = "mean") +                                       # stat_summary でデータを要約したレイヤーを重ねることを指定し、
  labs(y = "Sepal.Length_mean")                                                      # geom = "bar" で要約値を棒グラフで表示することを指定

# Sepal.Lengthの平均値をSpeciesごとに棒グラフで表示(sd3)
iris %>% ggplot(aes(x = Species, y = Sepal.Length)) +                                # stat_summary(geom = "bar", fun.y = "sd") で(sd1)と同じことができる
  stat_summary(geom = "bar", fun.y = "sd") +                                         # stat_summary でデータを要約したレイヤーを重ねることを指定し、
  labs(y = "Sepal.Length_sd")                                                        # geom = "bar" で要約値を棒グラフで表示することを指定


# positionの指定--------------------------------------------------------------------------------------------------------------------------------------
# Sepal.Width3で層別化したSepal.Lengthの平均値をSpeciesごとに棒グラフで表示(重ねる)
iris.facet %>% ggplot(aes(x = Species, y = Sepal.Length, fill = Sepal.Width3)) +     # fill = Sepal.Width3 で層別化
  stat_summary(geom = "bar", fun.y = "mean", position = position_identity())         # position = position_identity() で層別化グラフを重ねて表示
                                                                                     # position = "identity" の略記でも可

# Sepal.Width3で層別化したSepal.Lengthの平均値をSpeciesごとに棒グラフで表示(積み上げ)
iris.facet %>% ggplot(aes(x = Species, y = Sepal.Length, fill = Sepal.Width3)) +     # fill = Sepal.Width3 で層別化
  stat_summary(geom = "bar", fun.y = "mean", position = position_stack())            # position = position_stack() で層別化グラフを積み上げて表示
                                                                                     # position = "stack" の略記でも可

# Sepal.Width3で層別化したSepal.Lengthの平均値をSpeciesごとに棒グラフで表示(横に配置)
iris.facet %>% ggplot(aes(x = Species, y = Sepal.Length, fill = Sepal.Width3)) +     # fill = Sepal.Width3 で層別化
  stat_summary(geom = "bar", fun.y = "mean", position = position_dodge())            # position = position_dodge() で層別化グラフを横に表示
                                                                                     # position = "dodge" の略記でも可

# Sepal.Width3で層別化したSepal.Lengthの平均値をSpeciesごとに棒グラフで表示(100%積み上げ)
iris.facet %>% ggplot(aes(x = Species, y = Sepal.Length, fill = Sepal.Width3)) +     # fill = Sepal.Width3 で層別化
  stat_summary(geom = "bar", fun.y = "mean", position = position_fill())             # position = position_fill() で層別化グラフを100%積み上げで表示
                                                                                     # position = "fill" の略記でも可

# 個数の棒グラフ------------------------------------------------------------------------------------------------------------------------------
# Sepal.Width3で層別化したSpeciesの個数を棒グラフで表示
iris.facet %>% ggplot(aes(x = Species, fill = Sepal.Width3)) +                       # y軸はデフォルトで個数となっているので、y軸の指定はなし
  geom_bar(position = "dodge")                                                       # statはデフォルトで"count"となっているのでこちらも指定なし












