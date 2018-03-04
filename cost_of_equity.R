require(quantmod)
require(magrittr)
require(data.table)

symbol <- "002110"
market <- "SZ"
symbol <- paste(symbol, market, sep = ".")

getSymbols(symbol, from = '2007-01-01',
           to = "2017-12-31")

ohld <- get(symbol)
stock.m.ret <- ohld %>% monthlyReturn() 

getSymbols("^SSEC", from = '2007-01-01',
           to = "2017-12-31")
mkt <- get("SSEC")
mkt.m.ret <- mkt %>% monthlyReturn()


reg.data <- merge(stock.m.ret, mkt.m.ret)
reg.dt <- data.table(index(reg.data), coredata(reg.data))
names(reg.dt) <- c("month","stock","mkt")
reg.dt <- reg.dt[!is.na(stock) & !is.na(mkt)]
reg.dt <- reg.dt[stock != 0] #ting pai 


fit <- lm(stock ~ mkt, data = reg.dt)
summary(fit)

m.pred <- predict(fit, newdata = data.frame(mkt = 1.1^(1/12) - 1) ) 
(1 + m.pred)^12


mean(stock.m.ret) 
