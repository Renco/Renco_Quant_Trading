require(quantmod)
require(magrittr)
require(data.table)

symbol <- "002110"
market <- "SZ"
symbol <- paste(symbol, market, sep = ".")

getSymbols(symbol, from = '2008-03-31',
           to = "2018-03-31")

ohld <- get(symbol)
stock.m.ret <- ohld %>% monthlyReturn() 

getSymbols("^SSEC", from = '2007-01-01 fund.ticker <- head(x[,nasdaq],1)',
           to = "2017-02-28")
mkt <- get("SSEC")
mkt.m.ret <- mkt %>% monthlyReturn()


reg.data <- merge(stock.m.ret, mkt.m.ret)
reg.dt <- data.table(index(reg.data), coredata(reg.data))
names(reg.dt) <- c("month","stock","mkt")
reg.dt <- reg.dt[!is.na(stock) & !is.na(mkt)]
reg.dt <- reg.dt[stock != 0] #ting pai 


fit <- lm(stock ~ mkt, data = reg.dt)
summary(fit)
beta <-  coef(fit)['mkt']

rf <- 4 / 100 #guokai hang bond rate
equity.premium <- 6.85 / 100 #Adaradon 
(cost.equity <- rf + beta * equity.premium)
