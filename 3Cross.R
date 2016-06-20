###Turtle Trading Detector
require(quantmod)


setwd("/Users/renco/GitHub/Renco_Quant_Trading")
stock_list <- load("All_list.RData")
stocks_code <- as.vector(as.matrix(get(stock_list))) #for looping to saving time
TCbreak_out <- rep(0,length.out=length(stocks_code)) #for vector
start_date <- "2016-01-20"
end_date <- "2016-06-17"

for(code in stocks_code){
  #if(code %% 1000 ==0) {print (code)} #monitor progress
  symbol <- "" ##Initiate
  symbol <- paste(sprintf("%06d", code),
                  ifelse(code <= 1000, "SZ","SS"),
                  sep=".") #SZ stocks has number smaller than 1000
  #getSymbols(symbol,from=start_date) #Fecthing data
  tryCatch({getSymbols(symbol,from=start_date,to=end_date)
    #atr <- lag(ATR(get(symbol)))
    #ajusted for split and dividends 
    #get(symbol) <- adjustOHLC(get(symbol),use.Adjusted=TRUE)
    macd <-MACD(Cl(adjustOHLC(get(symbol),use.Adjusted=TRUE)))
    ma5 <- SMA(Cl(adjustOHLC(get(symbol),use.Adjusted=TRUE)),n=5) #100 days simple moving average
    ma10 <- SMA(Cl(adjustOHLC(get(symbol),use.Adjusted=TRUE)),n=10) #10 days simple moving average
    Vma5 <- SMA(Vo(adjustOHLC(get(symbol),use.Adjusted=TRUE)),n=5) #100 days simple moving average
    Vma10 <- SMA(Vo(adjustOHLC(get(symbol),use.Adjusted=TRUE)),n=10) #10 days simple moving average
    if(last(macd)$macd > last(macd)$signal & 
       last(lag(macd))$macd < last(lag(macd))$signal &
       last(ma5) > last(ma10) &
      last(lag(ma5)) < last(lag(ma10)) &
       last(Vma5) > last(Vma10) &
      last(lag(Vma5)) < last(lag(Vma10)))
    {TCbreak_out[which(stocks_code == code)] <- 1
    } else {removeSymbols(symbol) #drop uninteresting data
    }
  },  
  warning=function(msg) {
    print(paste("Caught warning message:", msg))
  },
  error=function(msg) {
    print(paste("Caught fatal message:", msg))
    return(NA)
  }
  ) #tryCatch
} #for 

##Delete data that doesn't matter 

TCsignal <- data.frame(cbind(stocks_code,TCbreak_out))
TCsignal <- TCsignal[which(TCsignal["TCbreak_out"]==1),]

# save(potential,
#      file=paste(paste("potential",Sys.Date(),sep="_"),"RData",sep="."))

##vectorize the computation
##add code to memo stocks that no longer exist 

##Vol Skew
# for(dt in unique(df_SPX$dtExpiry)) {
#   dt.2 <- as.Date(dt, origin='1970-01-01') 
#   tmp = subset(SPX.IV, dtExpiry == dt.2)
#   call.skew <- subset(tmp, Type == "C", select = c(Strike, IVMid))
#   put.skew <- subset(tmp, Type == "P", select = c(Strike, IVMid))
#   #plot(call.skew$Strike,call.skew$IVMid, col='green',type='l')#,ylim=c(0,0.4))
#   plot(put.skew$Strike,put.skew$IVMid, col='red',type='l')
# }
