# データの読み込み
usedata <- read.csv(
  #csvファイルならURL突っ込んでも持ってこれるよ
  "http://r-marketing.r-forge.r-project.org/data/rintro-chapter4.csv",
  stringsAsFactors = TRUE)
head(usedata, n = 5) # 先頭5行だけ見る

# データ規模
dim(usedata) # dimentionの略
colnames(usedata) # columns names

# sum
# 変数名は具体的であればあるほどよい
online_spend_sum <- sum(usedata$online.spend)
store_spend_sum <- sum(usedata$store.spend)
cat( # 
  paste0(
    "EC購入金額合計: ", online_spend_sum, "\n",
    "店舗購入金額合計: ", store_spend_sum 
  )
)

# 要約
summary(usedata)

# 1つの変数をよく見る
par(mfrow = c(1,2))
hist(usedata$store.spend, breaks = 30, main = "店舗支払い")
hist(usedata$online.spend, breaks = 30, main = "EC支払い")

# %/%は割り算の結果の整数部分を返す演算子。
usedata$ages <- paste0((usedata$age %/% 10) * 10, "代")

# 実は$だけじゃなくてこういう形でも変数を表記できる
# pandasっぽくてわかりやすいこともある
# ただし$とはデータの型が違うので注意。
head(usedata[, c("age", "ages")])

# グループ別集計
# byの中に複数の変数を入れてもOK
online_spend_by_ages <- aggregate(usedata$online.spend,
                                  by = list(usedata$ages), sum)
print(online_spend_by_ages)
sum(online_spend_by_ages[, 2])

# 2変数の関連を見る
par(mfrow = c(1,2))
plot(x = usedata$age, y = usedata$store.spend, main = "年齢×店舗支払い")
plot(x = usedata$age, y = usedata$online.spend, main = "年齢×EC支払い")

# クロス表
cross_table <- table(usedata$ages, usedata$email)
# roundで丸めができる(四捨五入ではないので注意)。
# applyはちょっと難しいができると楽しい。
# apply(計算対象, {行なら1,列なら2}, 計算方法)
cross_table_p_col <- round(apply(cross_table, 2, function(x){x/sum(x)}), 2)
cross_table_p_row <- round(t(apply(cross_table, 1, function(x){x/sum(x)})), 2)

cbind(cross_table, cross_table_p_col, cross_table_p_row)

# 相関
# 相関係数にはいくつかあるけど指定できる
print(cor(usedata$age, usedata$credit.score))
cor.test(usedata$age, usedata$credit.score)
# 相関行列
usecols <- colnames(usedata[, c(2, 3, 5:10)])
cor(usedata[, usecols])

# 重回帰
linear_model <- lm(
  online.visits ~ age + email + credit.score + sat.selection,
  data = usedata)

summary(linear_model)$coefficients