
# df <- read.csv(url("https://www.stopcovid19.jp/data/covid19japan.csv"))
usedata <- read.csv(
  # csvファイルならURL突っ込んでも持ってこれるよ
  "http://r-marketing.r-forge.r-project.org/data/rintro-chapter4.csv",
  stringsAsFactors = TRUE)
head(usedata, n = 5) # 先頭5行だけ見る
cor.test(usedata$distance.to.store, usedata$store.spend)

usedata$ages <- usedata$age %/% 10 * 10

aggregate(usedata$cust.id, by = list(usedata$email, usedata$ages), length)

usedata[,sapply(usedata, is.numeric)]

func_list <- list(
  func_min = function(x){min(x, na.rm = TRUE)},
  func_1qu = function(x){quantile(x, 0.25, na.rm = TRUE)},
  func_med = function(x){median(x, na.rm = TRUE)},
  func_ave = function(x){mean(x, na.rm = TRUE)},
  func_3qu = function(x){quantile(x, 0.75, na.rm = TRUE)},
  func_max = function(x){max(x, na.rm = TRUE)},
  func_nas = function(x){sum(is.na(x))}
)

apply(usedata[,sapply(usedata,is.numeric)], 2, func_list$func_min)
make_summary <- function(df){
  # 数値型に絞る
  tmp_df <- df[, sapply(df, is.numeric)] # apply系は応用編。
  idx <- c("Min", "1st_Qu", "Median", "Mean", "3rd_Qu", "Max", "NAs")
  out <- data.frame(
    row.names = idx,
    matrix(0, length(idx), ncol(tmp_df))
  )
  colnames(out) <- colnames(tmp_df)
  for(i in 1:length(idx)){
    tmp_func <- func_list[[i]]
    out[i, ] <- apply(tmp_df, 2, function(x){tmp_func(x)})
  }
  return(out)
}

make_summary(usedata)

idx <- c("Min", "1st_Qu", "Median", "Mean", "3rd_Qu", "Max", "NAs")
out <- data.frame(
  row.names = idx,
  matrix(0, length(idx), ncol(usedata))
)

