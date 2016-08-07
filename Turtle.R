###Turtle Trading Detector
require(quantmod)


setwd("/Users/renco/GitHub/Renco_Quant_Trading")
stock_list <- load("All_list.RData")
stocks_code <- as.vector(as.matrix(get(stock_list))) #for looping to saving time
break_out <- rep(0,length.out=length(stocks_code)) #for vector
# break_out <- rep(0,length.out=dim(stocks_code)[1]) #for data.frame
start_date <- "2016-03-20"
end_date <- "2016-08-02"

for(code in stocks_code){
  #if(code %% 1000 ==0) {print (code)} #monitor progress
  symbol <- "" ##Initiate
  symbol <- paste(sprintf("%06d", code),
                  ifelse(code <= 1000, "SZ","SS"),
                  sep=".") #SZ stocks has number smaller than 1000
  #getSymbols(symbol,from=start_date) #Fecthing data
  tryCatch({getSymbols(symbol,from=start_date,to = end_date)
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

# save(potential,
#      file=paste(paste("potential",Sys.Date(),sep="_"),"RData",sep="."))

##vectorize the computation
##add code to memo stocks that no longer exist 

####################################################################

##MA break-out to filter out false Turtle break out
require(quantmod)

#load(paste("potential",Sys.Date(),sep="_"))
#use potential from Turtle
load(paste(paste("potential",Sys.Date(),sep="_"),"RData",sep="."))
potential <- as.vector(as.matrix(potential[,1]))
double_break_out <- rep(0,length.out=length(potential)) 
order_size <- rep(0,length.out=length(potential)) 
last_price <- rep(0,length.out=length(potential)) 
range_size <- rep(0,length.out=length(potential)) #atr
stop_loss <- rep(0,length.out=length(potential)) 
scale_up <- rep(0,length.out=length(potential)) #price to add
scale_up2 <- rep(0,length.out=length(potential)) 
start_date <- "2015-09-01"

for(code in potential){
  symbol <- "" ##Initiate
  symbol <- paste(sprintf("%06d", code),
                  ifelse(code <= 1000, "SZ","SS"),
                  sep=".")
  tryCatch({ getSymbols(symbol,from=start_date,to=end_date)
    ma100 <- SMA(Cl(adjustOHLC(get(symbol),use.Adjusted=TRUE)),n=100) #100 days simple moving average
    ma10 <- SMA(Cl(adjustOHLC(get(symbol),use.Adjusted=TRUE)),n=10) #10 days simple moving average
    if(last(ma10) > last(ma100))
    {double_break_out[which(potential == code)] <- 1
    order_size[which(potential == code)] <- 500/last(ATR(HLC(get(symbol)))$atr)
    range_size[which(potential == code)] <- last(ATR(HLC(get(symbol)))$atr)
    last_price[which(potential == code)] <- last(Cl(adjustOHLC(get(symbol),use.Adjusted=TRUE)))
    stop_loss[which(potential == code)] <- last(Cl(adjustOHLC(get(symbol),use.Adjusted=TRUE))) - range_size[which(potential == code)]
    scale_up[which(potential == code)] <- last(Cl(adjustOHLC(get(symbol),use.Adjusted=TRUE))) + 0.5 * range_size[which(potential == code)]
    scale_up2[which(potential == code)] <- last(Cl(adjustOHLC(get(symbol),use.Adjusted=TRUE))) + range_size[which(potential == code)]
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

high_prob <- data.frame(cbind(potential,double_break_out
                              ,order_size,last_price,range_size,
                              stop_loss,scale_up,scale_up2))
##Get all the tickers of stocks that have Double Break_Out
high_prob <- high_prob[which(high_prob["double_break_out"]==1),]
#drop the double break out column 
high_prob <-high_prob[-2]

# save(high_prob,
#      file=paste(paste("high_prob",Sys.Date(),sep="_"),"RData",sep="."))
# 

####################################################################
as.vector(high_prob[,1])