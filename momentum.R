###Momentum Trading Detector
require(quantmod)
require(xts)
require(data.table)
require(lubridate)
require(PerformanceAnalytics)

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
while (num_mom_stock < N) {
  
  port <- port[!symbol %in% exclude]
  holding <- data.frame(matrix(rep(NA, 4 * N),ncol = 4))
  names(holding) <- c("symbol", "weight","vol","cum.ret")
  holding["symbol"] <- port[1:N,symbol]
  
  del_row = c()
  for (i in 1:N) {
    symbol <- holding[i,"symbol"]
    getSymbols(symbol, from = today() - 126, to = today())
    ts <- dailyReturn(Ad(get(symbol)))
    ts <- ts[!is.na(ts)] #remove NA values
    holding[i,"weight"] <- mean(ts) / var(ts)
    if (holding[i,'weight'] < 0 ) {
      exclude <- c(exclude, symbol)
      print(paste(symbol, "has negative return -- Kill it"))
      del_row <- c(del_row, i) 
    }
    holding[i,"vol"] <- sd(ts, na.rm = TRUE)
    holding[i,"cum.ret"] <- mom.dt[i,cum.ret]
  }
  if(!is.null(del_row)){
    holding <- holding[-del_row, ]
  }
  num_mom_stock <- dim(holding)[1]
}

last.price = c() #used to calculate "shou"
for (symbol in unlist(holding['symbol']) ) {
  last.price = c(last.price, tail(Ad(get(symbol)), 1))
  #removeSymbols(symbol)                
}

holding["weight"] <- holding["weight"] / sum(holding["weight"]) * 100 

p.vol <- sqrt(sum(holding["vol"]^2 * holding["weight"] / 100))
if (p.vol >= 0.05) {
  print("portfolio's volatility is too large.")
  ratio = p.vol / 0.05
  holding["weight"] <- holding["weight"] / ratio  
}


#mao tai 
mt.symbol <- "600519.SS"
mt.holding <- 100
getSymbols(mt.symbol, from = today() - 126,
           to = today())
mt.price <- tail(Ad(get(mt.symbol)),1)
mt.value <- mt.price * mt.holding

#risk managment
#trim addtional holding to make sure the daily VaR is less than
# 1% of total asset value 

risk.VaR <- Inf
p.value = 107538.58 - 47530.00 #value for momentum investments
tolerance <- 0.01 * (mt.value + p.value)
risk.downsize_ratio <- 1
while(abs(risk.VaR) > tolerance){
  
  p.value <- p.value * risk.downsize_ratio 
  risk.xts <- dailyReturn(Ad(get(mt.symbol)))
  for (symbol in unlist(holding['symbol'])){
    risk.xts <- merge(risk.xts, dailyReturn(Ad(get(symbol))))  
  }
  names(risk.xts) <- c(mt.symbol,unlist(holding['symbol']))
  mt.weight <- as.numeric(coredata(mt.value /  (mt.value + p.value)))
  risk.weight <- c(mt.weight, unlist(holding["weight"]) * (1 - mt.weight) * 1/100)
  stopifnot(abs(sum(risk.weight) - 1) < 1e-6)
  
  risk.ret <- xts(coredata(risk.xts) %*% as.matrix(risk.weight, col = 1),
                  order.by = index(get(mt.symbol)))
  risk.VaR <- VaR(risk.ret, p = 0.95, method = "modified") * (p.value + mt.value)
  risk.downsize_ratio <- abs( as.numeric(tolerance / risk.VaR))
  if (risk.downsize_ratio < 1){
    print(risk.downsize_ratio)
    print("Downsize")
    cat("\n")
  }
  if (abs(risk.downsize_ratio - 1) < 0.01){
    break
  }
}

chart.CumReturns(risk.ret)

holding["value"] = p.value * holding["weight"] / 100
holding['shou'] = round(holding['value'] / (100 * last.price), 
                        digits = 0)
print(holding) 


#clean up
removeSymbols(mt.symbol)
lapply(unlist(holding['symbol']), removeSymbols)



