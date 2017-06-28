###Momentum Trading Detector
require(quantmod)
require(xts)
require(data.table)
require(lubridate)

setwd("/Users/renco/GitHub/Renco_Quant_Trading")
start_date <- today() - 30
end_date <- today()


# Should we trade? --------------------------------------------------------


#Is market in distress?
getSymbols("000001.SS", from = start_date,
           to = end_date)
sh <- dailyReturn(get("000001.SS"))
sh <- data.table(coredata(sh), index(sh))
names(sh) <- c("d.ret", "date")
sh <- sh[d.ret !=  0 ]
sh.ret <- prod(1 + head(tail(sh$d.ret,8),7))
if (sh.ret < 1) {
  print("No Trading ")
}else{
  print("Good to go")
}
print(tail(index(get("000001.SS")),1))
print(sh.ret)


# Momentum Strat ----------------------------------------------------------
stock_list <- load("All_list.RData")
stocks_code <- as.vector(as.matrix(get(stock_list))) #for looping to saving time
mom.df <- data.frame(matrix(rep(NA, 2 * length(stocks_code)), ncol = 2))
names(mom.df) <- c("symbol","cum.ret")


code_count <- 0 
for (code in stocks_code) {
  code_count <- code_count + 1
  
  symbol <- "" ##Initiate
  symbol <- paste(sprintf("%06d", code),
                  ifelse(code <= 600000, "SZ","SS"),
                  sep = ".") #SZ stocks has number smaller than 1000
  #getSymbols(symbol,from=start_date) #Fecthing data
  tryCatch({getSymbols(symbol,from = start_date,to = end_date)
    #atr <- lag(ATR(get(symbol)))
    #ajusted for split and dividends 
    
    #get(symbol) <- adjustOHLC(get(symbol),use.Adjusted=TRUE)
    ret.ts <- dailyReturn(get(symbol))
    ret.ts <- data.table(coredata(ret.ts),index(ret.ts))
    names(ret.ts) <- c("d.ret","date")
    ret.ts <- ret.ts[d.ret != 0] #take out daily not trading
    if (dim(ret.ts)[1] < 7) {
      removeSymbols(symbol) #house keeping 
      next
    }
    #last 7 days skippping the most recent day 
    temp.cum.ret <- prod(1 + head(tail(ret.ts$d.ret,8),7)) 
    mom.df[code_count,"symbol"] <- symbol
    mom.df[code_count,"cum.ret"] <- temp.cum.ret
    removeSymbols(symbol)  #house keeping 
  },  
  warning = function(msg) {
    print(paste("Caught warning message:", msg))
  },
  error = function(msg) {
    print(paste("Caught fatal message:", msg))
    return(NA)
  }
  ) #tryCatch
} #for 

mom.dt <- data.table(mom.df) 
mom.dt <- mom.dt[order(-cum.ret)]
mom.dt <- mom.dt[is.na(cum.ret) == FALSE]
cat("Top 20 Winners:\n\n")
print(head(mom.dt,20))
port <- head(mom.dt, 20)


# optimal portfolio  ------------------------------------------------------
N = 5
exclude <- c('002591.SZ','002510.SZ','603861.SZ',
             '000002.SZ','000921.SZ','002761.SZ',
             '603861.SZ')

num_mom_stock <- 0
while (num_mom_stock < 5) {
  
  port <- port[!symbol %in% exclude]
  holding <- data.frame(matrix(rep(NA, 4 * N),ncol = 4))
  names(holding) <- c("symbol", "weight","vol","cum.ret")
  holding["symbol"] <- port[1:N,symbol]
  
  del_row = c()
  for (i in 1:N) {
    symbol <- holding[i,"symbol"]
    getSymbols(symbol, from = today() - 126, to = today())
    ts <- dailyReturn(Ad(get(symbol)))
    holding[i,"weight"] <- mean(ts) / var(ts)
    if (holding[i,'weight'] < 0 ) {
      exclude <- c(exclude, symbol)
      print(paste(symbol, "has negative return -- Kill it"))
      del_row <- c(del_row, i) 
    }
    holding[i,"vol"] <- sd(ts)
    holding[i,"cum.ret"] <- mom.dt[i,cum.ret]
    #removeSymbols(symbol)
  }
  if(!is.null(del_row)){
    holding <- holding[-del_row, ]
  }
  num_mom_stock <- dim(holding)[1]
}

holding["weight"] <- holding["weight"] / sum(holding["weight"]) * 100 
#print(holding)

p.vol <- sqrt(sum(holding["vol"]^2 * holding["weight"] / 100))
if (p.vol >= 0.05) {
  print("portfolio's volatility is too large.")
  ratio = p.vol / 0.05
  holding["weight"] <- holding["weight"] / ratio  
}

p.value = 107538.58 - 47530.00
holding["value"] = p.value * holding["weight"] / 100
print(holding)