# データの読み込み
dat <- read.csv(file = "./data/salary.csv",fileEncoding = "utf-8") # ./でworking directory

head(dat,5)
tail(dat)
str(dat)  # データフレームの概要をみることができる
# サンプルサイズ100、次元が5

## 給与を他の４変数で予測する →　線形回帰
# step.1　…　線形性を確認する
# step.2　…　lm関数で実行する


# step.1　…　線形性を確認する : 散布図に作成
plot(dat)
boxplot(salary~license, data = dat)

# step.2　…　lm関数で実行する : 回帰係数の推定
result <- lm(formula = salary ~ year+work+absent+license, data = dat)
result <- lm(formula = salary ~ . ,data = dat)
summary(result)

