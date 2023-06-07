###########################################################################
#
#
#    MACD背離
#
#
#
############################################################################
rm(list=ls())

if(!require(TTR))install.packages("TTR")

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

########################################### ATR #######################################################
subset1 <- BTC_1h_price

# 定义ATR函数
if(!require(TTR))install.packages("TTR")

subset1$High <- as.numeric(subset1$High)
subset1$Low <- as.numeric(subset1$Low)
subset1$Close <- as.numeric(subset1$Close)
atr <- ATR(subset1[,c("High","Low","Close")], n = 13)

BTC_1h_price <- cbind(BTC_1h_price, atr[,2])
colnames(BTC_1h_price) <- c("Date", "Open", "High", "Low", "Close", "Histogram", "ATR")

write.csv(BTC_1h_price, file = "BTC_1h_price.csv")


###########################################################################
#
#
#    MACD背離 part 2 start from here
#
#
#
############################################################################

BTC_1h_price <- read.csv("BTC_1h_price.csv", sep = ",", stringsAsFactors = F, encoding = "UTF-8", header = F)
columns <- BTC_1h_price[1,]
colnames(BTC_1h_price) <- columns
BTC_1h_price <- BTC_1h_price[c(2:nrow(BTC_1h_price)),c(2:8)]

rm(columns)
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
BTC_1h_price1 <- cbind(BTC_1h_price, divergence)

library(quantmod)
prices <- BTC_1h_price1$Close
hourly_time <- BTC_1h_price1$Date

subset2 <- BTC_1h_price1[c(1:500),c(1,5,6)]
write.csv(subset2, file = "BTC_1hour_price.csv")

########################################### 回測交易 #######################################################

#做多策略
BTC_1h_price2 <- na.omit(BTC_1h_price1)
#BTC_2022to2023 <- BTC_1h_price2[38169:nrow(BTC_1h_price2),]
#BTC_1h_price2 <- BTC_2022to2023

BTC_1h_price2$Open <- as.numeric(BTC_1h_price2$Open)
BTC_1h_price2$High <- as.numeric(BTC_1h_price2$High)
BTC_1h_price2$Low <- as.numeric(BTC_1h_price2$Low)
BTC_1h_price2$Close <- as.numeric(BTC_1h_price2$Close)
BTC_1h_price2$Histogram <- as.numeric(BTC_1h_price2$Histogram)
BTC_1h_price2$ATR <- as.numeric(BTC_1h_price2$ATR)

data_trading <-  NULL

buy_profit <- 0
buy_position <- 0
tem_loss <- 0
tem_profit <- 0
buy_trade_count <- 0
buy_win_trade <- 0

for (i in 1:nrow(BTC_1h_price2)) {
  
  cat(i, "/", nrow(BTC_1h_price2), "\n") 
  
  if(BTC_1h_price2[i,"Divergence"] == 1){
    buy_position <- buy_position + 1
    buyin <- BTC_1h_price2[i,"Close"]
    stop_loss <- buyin - BTC_1h_price2[i,"ATR"]
    stop_profit <- buyin + BTC_1h_price2[i,"ATR"]
    if(BTC_1h_price2[i,"Low"] < stop_loss){ #最低價小過止損, 表示止損在價格區間內
      if(buy_position > 0){
        tem_loss <- stop_loss - buyin
      }
    }  
    if(BTC_1h_price2[i,"High"] > stop_profit){
      if (buy_position > 0) {
      tem_profit <- stop_profit - buyin #最高價大過止盈, 表示止盈在價格區間內
      }
    }
    tem <- buy_profit
    buy_profit <- buy_profit + tem_loss + tem_profit
    buy_position <- buy_position - 1
    buy_trade_count <- buy_trade_count + 1
    if (tem > buy_profit) {
      buy_win_trade <- buy_win_trade + 1
    }

  }
  tem1 <- cbind(BTC_1h_price2[i,c("Date", "Close", "ATR", "Divergence")], buy_profit)
  data_trading <- rbind(data_trading, tem1)

}
data_trading1 <- data_trading[!duplicated(data_trading$buy_profit),]


# 做空策略
sell_profit <- 0
sell_position <- 0
tem_loss <- 0
tem_profit <- 0
sell_trade_count <- 0
sell_win_trade <- 0
data_sell_profit <- NULL

for (i in 1:nrow(BTC_1h_price2)) {
  
  cat(i, "/", nrow(BTC_1h_price2), "\n") # 有49460筆資料, 先存檔
  
  if(BTC_1h_price2[i,"Divergence"] == -1){
    sell_position <- sell_position + 1
    sell <- BTC_1h_price2[i,"Close"] #sell是做空部位
    stop_loss <- sell + BTC_1h_price2[i,"ATR"]
    stop_profit <- sell - BTC_1h_price2[i,"ATR"]
    if(BTC_1h_price2[i,"Low"] < stop_profit){ #最低價小過止損, 表示止損在價格區間內
      if(sell_position > 0){
        tem_profit <- sell - stop_profit
      }
    }  
    if(BTC_1h_price2[i,"High"] > stop_loss){
      if (sell_position > 0) {
        tem_loss <- stop_profit - sell #最高價大過止盈, 表示止盈在價格區間內
      }
    }
    tem <- sell_profit
    sell_profit <- sell_profit + tem_loss + tem_profit
    sell_position <- sell_position - 1
    sell_trade_count <- sell_trade_count + 1
    if (tem > sell_profit) {
      sell_win_trade <- sell_win_trade + 1
    }
    tem1 <- cbind(BTC_1h_price2[i,"Date"], sell_profit, sell)
    data_sell_profit <- rbind(data_sell_profit, tem1)
  }
  
  
}

buy_win_percent <- buy_win_trade/buy_trade_count
sell_win_percent <- sell_win_trade/sell_trade_count

buy_win_percent
sell_win_percent
buy_profit
sell_profit

######都做完了, 接下來要做表格, 進出場時間點, 包括buy and sell, ATR, profit, loss



