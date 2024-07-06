library(tidyverse)
library(skimr)
library(mlbench)                # このパッケージの関数は直接使わないが、データ"BreastCancer"を使うために読み込む

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# データの整理
# -----------------------------------------------------------------------------------------------------------------------------------------------------
# データの読み込み(BreastCancer)
data("BreastCancer")
BC <- BreastCancer
BC <- as_tibble(BC)             # tibble型に変換
head(BreastCancer)
BC

# データの確認
BC
BC  %>% skim()                  # skim()によるデータの確認


# データの読み込み(iris)
iris <- iris
iris <- as_tibble(iris)         # tibble型に変換

# データの確認
iris
iris  %>% skim()                # skim()によるデータの確認


# データの作成
set.seed(0)
test <- tibble(x1 = rnorm(10,4,3), x2 = rnorm(10,30,5), x3 = rnorm(10, 50, 10))
test
test %>% skim()


# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 列の抽出
# -----------------------------------------------------------------------------------------------------------------------------------------------------
# select()の使い方
BC %>% select(Id, Cell.size)                        # select(data, 列名, 列名, ...)で選択した列を抽出
BC %>% select(1, 3)                                 # 列名は列番号でも良い。今回は1列目と3列目を抽出
BC %>% select(Id:Cell.size)                         # :を用いると指定した２つの列の間にある全ての変数を抽出できる
BC %>% select(1:3)                                  # 列番号でも同じことができる

BC %>% select(-Id)                                  # 列名に-列名を指定すれば、指定した列を除外できる
BC %>% select(-1)                                   # 列番号でも同じ
BC %>% select(-Id, -Cell.size)                      # -列名, -列名, ... で複数列を除外できる
BC %>% select(-c(Id, Cell.size))                    # -c(列名, 列名, ...)でもOK

# ヘルパ関数の利用
BC %>% select(starts_with("M"))                     # starts_with()で前方一致している列を抽出
BC %>% select(ends_with("s"))                       # ends_with()で後方一致している列を抽出
BC %>% select(contains("Cell"))                     # contains()で部分一致している列を抽出
BC %>% select(everything())                         # everything()ですべての列を抽出

# everything()の使い方
BC %>% select(Cell.size, everything())              # 順番の入れ替え

# 名前の変更
BC %>% select(x1 = Id, x2 = Cell.size)              # select(data, 新しい列名 = 古い列名, ...)で列を抽出しつつ、新しい列名をつけることができる
BC %>% select(x = contains("Cell"), everything())   # 対象列が複数ある場合は自動的に連番を振ってくれる
BC %>% select(x = 1:5, everything())

# rename()の使い方
BC %>% rename(x1 = Id, x2 = Cell.size)              # rename(data, 新しい列名 = 古い列名, ...)で列名の変更を行う。指定しない列はそのまま残る



# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 列の変換
# -----------------------------------------------------------------------------------------------------------------------------------------------------
iris
# mutate()の使い方
iris %>% mutate(x = Sepal.Length*2)                                         # mutate(data, 列名1 = コード1, ...)でコードに従って新しい列名を追加
iris %>% mutate(Sepal.Length*2)                                             # 列名を指定しなければ自動的に列名がつく
iris %>% mutate(Sepal.Length = Sepal.Length*2)                              # 列名を既存の列名にすれば、上書きとなる

iris %>% mutate(x = if_else(Sepal.Length >= 5, "5以上", "5未満"))           # if_else(条件, 真の時の値, 偽の時の値)

iris %>% mutate(                                                            # case_when(条件1 ~ 真の時の値, 条件2 ~ 真の時の値, ...)で複数条件を指定できる
    x = case_when(                                                          
        Sepal.Length >= 6 ~ "6以上",
        Sepal.Length >= 5 ~ "5以上6未満",
        TRUE ~ "5未満"                                                      # TRUEはそれ以外を指定
))

# transmute()の使い方
iris %>% transmute(x = Sepal.Length*2, Species)                             # transmute(data, 列名1 = コード1, ...)でコードに従って作成した列のみを残す。指定されなかった列は捨てられる


# 行番号を追加
iris %>% rownames_to_column("Id")                                           # rownames_to_column("行番号名")で行番号をデータに追加できる。ただし行番号はchr型


# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 集計
# -----------------------------------------------------------------------------------------------------------------------------------------------------
# summarise()の使い方
iris %>% summarise(n = n(),                                                 # summarise(列名1 = コード1, 列名2 = コード2, ...)でコードに従って集計を行う
                   mean = mean(Sepal.Length),                  
                   sd = sd(Sepal.Length))

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# グループ化
# -----------------------------------------------------------------------------------------------------------------------------------------------------
# group_by()の使い方
iris %>% group_by(Species)                                                         # group_by(グループ化変数1, グループ化変数2, ...)で指定した変数ごとにグループ化されて処理がされる

iris %>%                                                                           # summarise()を用いて集計すると、Speaciesのグループごとに集計がされる
    group_by(Species) %>%
    summarise(n = n(),
              SL.mean = mean(Sepal.Length),
              SL.sd = sd(Sepal.Length))


iris %>%
    group_by(Sepalength.5 = if_else(Sepal.Length >= 5, "5以上", "5未満")) %>%      # if_else()を用いてグループ化した後集計
    summarise(n = n(),
              SL.mean = mean(Sepal.Length),
              SL.sd = sd(Sepal.Length))


iris %>%                                                                           # case_when()を用いてグループ化した後集計
    group_by(Sepal.Length.C = case_when(
        Sepal.Length > 5.1 ~ "5.1以上",
        Sepal.Length > 4.7 ~ "4,7以上5.1未満",
        TRUE ~ "4.7未満")) %>% 
            summarise(n = n(),
                      SL.mean = mean(Sepal.Length),
                      SL.sd = sd(Sepal.Length))

iris %>%                                                                           # 複数の変数でグループ化した後集計
    group_by(Species, SL.5 = if_else(
        Sepal.Length >= 5, "5以上","5未満"
    )) %>% 
    summarise(n = n(),
              mean = mean(Sepal.Length),
              sd = sd(Sepal.Length))

# グループ化の解除
iris %>% group_by(Species) -> iris1
iris %>% group_by(Species) %>% ungroup()                                           # ungroup()でグループ化の解除



# -----------------------------------------------------------------------------------------------------------------------------------------------------
# tidyデータ
# -----------------------------------------------------------------------------------------------------------------------------------------------------
# tidyデータへの変換(全ての列)
(test.tidy <- test %>% gather(key = key, value = value))                           # gather(key = 任意の変数名, value = 任意の変数名, ...) でtidyデータの作成

# tidyデータへの変換(一部の列)                                                     
(test.tidy12 <- test %>% gather(key = key, value = value, x1, x2))                 # 変数1、変数2をtidy化
                                                                                   # -> 変数1と変数2の名前を水準に持つ変数(key)を一つ作成し、変数1と変数2の値を水準に持つ変数(value)を作成

(test.tidy3 <- test %>% gather(key = key, value = value, -x1, -x2))                # -変数名 で指定した変数名以外をtidy化



# tidyデータから元のデータに変換(test.tidy12)
test.tidy12 %>% spread(key = key, value = value)                                    # spread(key = 変数1, value = 変数2) で元のデータに戻す
                                                                                    # -> 変数1で指定した列の水準名を変数名に持つ列を新たに作り、変数2で指定した列の値をその新たな列に配置
# tidyデータから元のデータに変換(test.tidy3)
test.tidy3 %>% spread(key = key, value = value)

iris
# 変数の型について
iris %>% gather(key = key, value = value)

# 上のコマンドではデータのtidy化はできるが下記の警告がでる。
# attributes are not identical across measure variables; they will be dropped
# これはtidy化を行う際に選択した変数の型が同じでないことを警告している。今回はSpeciesの型(fct)とその他の変数(dbl)の型が異なっているのが原因。
# 今回はできあがった変数の型はchr型となっている。下のようにSpeciesをtidy化しなければ警告はでない。
iris %>% gather(key = key, value = value, -Species)


#プライマリーキーについて
iris %>% gather(key = key, value = value, -Species)

# 上のコマンドではデータのtidy化はあまりお勧めしない。なぜならこのtidyデータは行を区別するキー(ID)がないため、元に戻せないからである。
# この行を区別する変数をプライマリーキーという。
# 上のtest.tidy12データは変数x3をtidy化しておらず、変数3の値に同じ値がないため、x3がプライマリーキーとして働いている。(test.tidy3も同様)
# しかしtest.tidyはプライマリーキーがないため、元に戻せない。
# 試しにtest.tidyをspreadしてみると、エラーが出る
test.tidy %>% spread(key = key, value = value)               　　　　　　　　　　　　

# tidy化する際プライマリーキーがないのであれば、変数IDを新たに追加し、IDを除いてtidy化すれば良い。
iris %>% rownames_to_column("ID") %>%                                                # rownames_to_column("ID") で行番号を追加
  gather(key = key, value = value, -ID, -Species)                                    

# 元に戻してみる
iris %>% rownames_to_column("ID") %>% 
  gather(key = key, value = value, -ID, -Species) %>% 
  spread(key = key, value = value)　　　　　　　　　　　　　　　　　　　　　　　　　

# 細かいことだが、上の方法ではspreadした際に変数の順番が変わってしまう。
# gatherする際に、factor_key = TRUE、と指定すればこれを防げる。
iris %>% rownames_to_column("ID") %>% 
  gather(key = key, value = value, -ID, -Species, factor_key = TRUE) %>%             # factor_key = TRUE で変数のtidyデータを元に戻した時の変数の順番が変わることを防ぐ
  spread(key = key, value = value)





# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 複数列への処理
# -----------------------------------------------------------------------------------------------------------------------------------------------------
# irisデータの数値データ全てに1を足す処理を考える
iris %>% mutate(Sepal.Length = Sepal.Length + 1,
                Sepal.Width = Sepal.Width + 1,
                Petal.Length = Petal.Length + 1,
                Petal.Width = Petal.Width +1)

iris

# 上記のコードはあまり綺麗ではないので２つの方法で解決する


# (1) tidyデータでの処理
# 数値データ全てに10を足す
iris %>% rownames_to_column("ID") %>% 
  gather(key = key, value = value, -ID, -Species, factor_key = TRUE) %>% 
  mutate(value = value + 10) %>%                                                           
  spread(key = key, value = value)                                                # tidyデータに変換して、value列に1を足す処理。その後spread()



# (2) scoped functionを用いる
# mutate(),summarise(),group_by()には複数列に同じ処理を行うscoped版が存在する
# scoped functionの種類は次の３つ。_all, _if, _at

# mutate_all()の使い方
test %>% mutate_all(round, 0)                                           # mutate_all(data, 関数, 関数に渡す引数1, ...) 全ての値を少数第２位で四捨五入
test %>% mutate_all(mean)                                               # 全ての値を平均値で置き換える

# mutate_if()の使い方
test %>% mutate_if(is.numeric, round, 0)                                # mutate_if(data, 条件, 関数, 関数に渡す引数1, ...) 数値型の列全てに、roundを適用する
iris %>% mutate_if(is.numeric, round, 0)                                # 数値型の列全てに、roundを適用する

# mutate_at()の使い方
test %>% mutate_at(c("x1", "x2"), round, 0)                             # mutate_at(data, 対象の列, 関数, 関数に渡す引数1, ...) 指定した名前(x1とx2)の列全てにroundを適用する
iris %>% mutate_at(c("Sepal.Length","Petal.Length"), round,0)           # 指定した名前(Sepal.LengthとPetal.Length)の列全てにroundを適用する
iris %>% mutate_at(vars(ends_with("Length")), round, 0)                 # vars()でくくればselect()の表記が使える
iris %>% mutate_at(vars(-Species), as.factor)                           # Species列以外をfactorに変換

# summarize_all()の使い方
test %>% summarise_all(mean)                                            # summarise_all(data, 関数, 関数に渡す引数1, ...) 全ての変数の平均を求める
test %>% summarise_all(sd)                                              # 全ての変数のsdを求める

# summarise_if()の使い方
iris %>% summarise_if(is.numeric, mean)                                 # summarise_if(data, 条件, 関数, 関数に渡す引数1, ...) 数値型の変数の平均を求める
test %>% summarise_if(is.numeric, sd)                                   # 数値型の変数のsdを求める

# summarise_at()の使い方
iris %>% summarise_at(c("Sepal.Length", "Petal.Length"), mean)          # summarise_at(data, 対象の列, 関数, 関数に渡す引数, ...) 指定した名前(Sepal.LengthとPetal.Length)の列の平均を求める
iris %>% summarise_at(vars(contains("Sepal")), mean)                    # 指定した名前(Sepal.LengthとPetal.Length)の列の平均を求める



# -----------------------------------------------------------------------------------------------------------------------------------------------------
# クロス集計表の作成
# -----------------------------------------------------------------------------------------------------------------------------------------------------
# データの用意
iris.cros <- iris %>% mutate(SL4.8 = if_else(Sepal.Length >= 4.8, "4.8以上", "4.8未満"))    # Sepal.Lengthが5以上かそうでないかを表す変数を作成

# SpeciesとSL4.8のクロス集計表を作成(tallyを用いて)
iris.cros %>% group_by(Species, SL4.8) %>%                              # group_by()でSpeciesとSL4.8でグループ化
    tally() %>%                                                         # tally()で集計
    spread(key = SL4.8, value = n) %>%                                  # spread()で広げる
    mutate_at(vars(-Species), list(~ifelse(is.na(.), 0, .)))             # 欠損値を0で埋める

# SpeciesとSL4.8のクロス集計表を作成(summariseを用いて)
iris.cros %>% group_by(Species, SL4.8) %>%                              # group_by()でSpeciesとSL4.8でグループ化
    summarise(n = n()) %>%                                              # summarise()とn()で個数をカウント
    spread(key = SL4.8, value = n) %>%                                  # spread()で広げる
    mutate_at(vars(-Species), funs(ifelse(is.na(.), 0, .)))             # 欠損値を0で埋める

# SpeciesとSL4.8のクロス集計表を作成(countを用いて)
iris.cros %>% count(Species, SL4.8) %>%                                 # count(data, 列1, 列2)で列1でグループを作り、列２ごとに集計する
    spread(key = SL4.8, value = n) %>%                                  # spread()で広げる
    mutate_at(vars(-Species), funs(ifelse(is.na(.), 0, .)))             # 欠損値を0で埋める

# SpeciesとSL4.8のクロス集計表を作成し、セルにSepal.Widthの平均値を表示
iris.cros %>% group_by(Species, SL4.8) %>% 
  summarise(mean = mean(Sepal.Width)) %>%                               # summariseを用いて集計すれば、集計の関数を変更することでクロス集計表に個数以外のものを表示できる。
  spread(key = SL4.8, value = mean) %>% 
  mutate_at(vars(-Species), funs(ifelse(is.na(.), 0, .)))

# SpeciesとSL4.8のクロス集計表を作成し、背うにSepal.Widthの合計値を表示
iris.cros %>% group_by(Species, SL4.8) %>% 
  summarise(sum = sum(Sepal.Width)) %>%                                 # 集計関数をsum() で集計
  spread(key = SL4.8, value = sum) %>% 
  mutate_at(vars(-Species), funs(ifelse(is.na(.), 0, .)))



# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 行の絞り込み
# -----------------------------------------------------------------------------------------------------------------------------------------------------
# filter()の使い方
iris %>% filter(Species == "setosa")                                    # filter(data, 条件)で条件に合う行のみを抽出
iris %>% filter(Sepal.Length > 5)                                       # Sepal.Lengthが5より大きいデータを抽出
iris %>% filter(Species == "setosa", Sepal.Length > 5)                  # filter(data, 条件1, 条件2, ...)で複数条件を同時に満たすデータを抽出
iris %>% filter(Species == "setosa"| Sepal.Length > 5)                  # filter(data, 条件1| 条件2| ...)で複数条件のうち少なくとも1つを満たすデータを抽出
iris %>% filter(!(Species == "setosa"| Sepal.Length > 5))               # !で論理値を反転させることができる。この場合、setosaでもなくSepal.Length > 5でもないデータを抽出

# 「Aでない、かつBでない」と「(AまたはB)でない」は数学的に同じこと。
# つまり上のコードは
iris %>% filter(!(Species == "setosa"), !(Sepal.Length > 5))
# でもよい。またちなみに、「Aでない、またはBでない」と「(AかつB)でない」は数学的に同じこと。



