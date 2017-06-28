setwd("/Users/renco/GitHub/Renco_Quant_Trading")
# load("A_list.RData")

###Detect all tickers that have corresponding stocks in Shanghai Market
require(quantmod)
require(lubridate)

real_stocks <- rep(0,604000)
market.name <- c("Shenzhen","Shanghai","Zhongxiaoban")
for (market in market.name) {
  if (market == "Shenzhen") {
    stocks_code <- seq(000001,000999)
    print(market)
  } else if (market == "Shanghai") {
    stocks_code <- seq(600001,603999)
    print(market)
  } else if (market == "Zhongxiaoban") {
    stocks_code <- seq(002001,002853)
    print(market)
  }

  #real_stocks <- rep(0,length.out = length(stocks_code)) #use 1 to denote the stock exists
  start_date <- today() - 30
  
  for (code in stocks_code) {
    if (code %% 1000 == 0) {print("One Chapter")} #monitor progress
    symbol <- "" ##Initiate
    symbol = paste(sprintf("%06d", code),
                   ifelse(code <= 600000, "SZ","SS")
                   ,sep = ".")
    
    tryCatch({getSymbols(symbol,from = start_date)
      real_stocks[code] <- 1
      rm(symbol)},  
    warning = function(msg) {
      print(paste("Caught warning message:", msg))
    },
    error = function(msg) {
      print(paste("Caught an error:", msg))
      return(NA)
    }
    )
  } #for code 
} #for market


all.stock_code <- seq(1,604000)
All_list <- data.frame(all.stock_code,real_stocks)
All_list <- All_list[which(All_list["real_stocks"] == 1),]
All_list <- All_list["all.stock_code"]


##Got Shenzhen Market
save(All_list,file = "All_list.RData")

