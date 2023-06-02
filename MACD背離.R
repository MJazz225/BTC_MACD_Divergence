###########################################################################
#
#
#    MACD背離
#
#
#
############################################################################
rm(list=ls())

########################################### 測BTC1小時資料 #######################################################

setwd("D:\\Users\\marshaw.tan\\Desktop\\TradingData")

rawdata <-  read.csv("Binance_BTCUSDT_1h.csv", sep = ",", stringsAsFactors = F, encoding = "UTF-8", header = F)
columns <- rawdata[2,]
rawdata <- rawdata[c(nrow(rawdata):3),]
colnames(rawdata) <- columns
BTC_1h_price <- rawdata[,c(2,4,5,6,7)]
rm(rawdata)
########################################### 技術指標設定 #######################################################
#MACD parameter
fast_period = 13
slow_period = 34
signal_period = 9

n = 13 # for ATR
# 定义MACD函数
calculate_macd <- function(data, fast_period = 13, slow_period = 34, signal_period = 9) {#data要改
  # 计算快线（快速移动平均线）和慢线（慢速移动平均线）
  macd_line <- EMA(data, n = fast_period) - EMA(data, n = slow_period)
  
  # 计算信号线（移动平均线）
  signal_line <- EMA(macd_line, n = signal_period)
  
  # 计算MACD柱状图（MACD线和信号线之间的差异）
  macd_histogram <- macd_line - signal_line
  
  # 创建一个包含计算结果的数据框
  macd_data <- data.frame(macd = macd_line, signal = signal_line, histogram = macd_histogram)
  
  # 返回MACD数据框
  return(macd_data)
}

# 调用MACD函数计算MACD指标
macd_data <- calculate_macd(BTC_1h_price$Close)#data要改
BTC_1h_price <- cbind(BTC_1h_price, macd_data$histogram)
BTC_1h_price <- na.omit(BTC_1h_price)
colnames(BTC_1h_price) <- c("Date", "Open", "High", "Low", "Close", "Histogram")

# 定义ATR函数
calculate_atr <- function(data, n) {
  # 计算每个周期的真实范围（True Range）
  true_range <- pmax(High(data) - Low(data), abs(High(data) - lag(Cl(data))), abs(Low(data) - lag(Cl(data))))
  
  # 计算平均真实范围（Average True Range）
  atr <- SMA(true_range, n)
  
  # 返回ATR指标
  return(atr)
}

########################################### 定義背離 #######################################################
#畫背離線 
# 定义函数：判断顶部背离
f_top_fractal <- function(src) {
  src[5] < src[3] & src[4] < src[3] & src[3] > src[2] & src[3] > src[1]
}

# 定义函数：判断底部背离
f_bot_fractal <- function(src) {
  src[5] > src[3] & src[4] > src[3] & src[3] < src[2] & src[3] < src[1]
}

# 定义函数：判断背离类型
f_fractalize <- function(src) {
  if (f_top_fractal(src)) {
    return(1)
  } else if (f_bot_fractal(src)) {
    return(-1)
  } else {
    return(0)
  }
}

###################################testing 測試成功!
#  定义函数检查背离
check_divergence <- function(prices, macd_histogram) {
  divergences <- character(length(prices))
  
  for (i in 3:length(prices)) {
    if (!is.na(macd_histogram[i]) && !is.na(macd_histogram[i-1]) && !is.na(prices[i]) && !is.na(prices[i-1])) {
      if (macd_histogram[i] > 0 && macd_histogram[i-1] > 0 && macd_histogram[i] < macd_histogram[i-1] && prices[i] > prices[i-1]) {
        divergences[i] <- 1
      } else if (macd_histogram[i] < 0 && macd_histogram[i-1] < 0 && macd_histogram[i] > macd_histogram[i-1] && prices[i] < prices[i-1]) {
        divergences[i] <- -1
      }
    }
  }
  
  return(divergences)
}

# 检测背离
divergence <- as.data.frame(check_divergence(BTC_1h_price$Close, BTC_1h_price$Histogram)) 

# 输出结果
print(divergence)
colnames(divergence) <- "Divergence"
BTC_1h_price <- cbind(BTC_1h_price, divergence)

library(quantmod)
prices <- BTC_1h_price$Close
hourly_time <- BTC_1h_price$Date

plot(BTC_1h_price$Close, BTC_1h_price$Date)


