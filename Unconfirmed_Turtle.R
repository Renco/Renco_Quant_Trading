#Potential Turtle Size

load(paste(paste("potential",Sys.Date(),sep="_"),"RData",sep="."))
potential <- as.vector(as.matrix(potential[,1]))
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
  tryCatch({ getSymbols(symbol,from=start_date)
    order_size[which(potential == code)] <- 500/last(ATR(HLC(adjustOHLC(get(symbol),use.Adjusted=TRUE)))$atr)
    range_size[which(potential == code)] <- last(ATR(HLC(get(symbol)))$atr)
    last_price[which(potential == code)] <- last(Cl(get(symbol)))
    stop_loss[which(potential == code)] <- last(Cl(get(symbol))) - range_size[which(potential == code)]
    scale_up[which(potential == code)] <- last(Cl(get(symbol))) + 0.5 * range_size[which(potential == code)]
    scale_up2[which(potential == code)] <- last(Cl(get(symbol))) + range_size[which(potential == code)]
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

turtle <- data.frame(cbind(potential,order_size,
                              last_price,range_size,
                              stop_loss,scale_up,scale_up2))

#adjust price