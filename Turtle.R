###Turtle Trading Detector
require(quantmod)


setwd("/Users/renco/GitHub/Renco_Quant_Trading")
stock_list <- load("All_list.RData")
stocks_code <- as.vector(as.matrix(get(stock_list))) #for looping to saving time
break_out <- rep(0,length.out=length(stocks_code)) #for vector
# break_out <- rep(0,length.out=dim(stocks_code)[1]) #for data.frame
start_date <- "2016-01-01"

for(code in stocks_code){
  #if(code %% 1000 ==0) {print (code)} #monitor progress
  symbol <- "" ##Initiate
  symbol <- paste(sprintf("%06d", code),
                  ifelse(code <= 1000, "SZ","SS"),
                  sep=".") #SZ stocks has number smaller than 1000
  #getSymbols(symbol,from=start_date) #Fecthing data
  tryCatch({getSymbols(symbol,from=start_date)
            #atr <- lag(ATR(get(symbol)))
            #ajusted for split and dividends 
            #get(symbol) <- adjustOHLC(get(symbol),use.Adjusted=TRUE)
            dc <- lag(DonchianChannel(cbind(Hi(adjustOHLC(get(symbol),use.Adjusted=TRUE)),
                                      Lo(adjustOHLC(get(symbol),use.Adjusted=TRUE)))),
                                      n=20)
            if(last(Cl(last(adjustOHLC(get(symbol),use.Adjusted=TRUE)))) > last(dc)$high)
            #if(last(Cl(last(adjustOHLC(get(symbol),use.Adjusted=TRUE)))) < last(dc)$low)
            {break_out[which(stocks_code == code)] <- 1
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

potential <- data.frame(cbind(stocks_code,break_out))
##Get all the tickers of stocks that have Donchian Break_Out
potential <- potential[which(potential["break_out"]==1),]

####Add code to save potential

save(potential,
     file=paste(paste("potential",Sys.Date(),sep="_"),"RData",sep="."))

##vectorize the computation
##add code to memo stocks that no longer exist 



