renco.risk.management <- function(){
require(quantmod)
require(PerformanceAnalytics) #Do VaR
require(magrittr)

holding.tickers <- c("600419.SS","000613.SZ",
                     "600506.SS","600333.SS",
                     "000036.SZ","002673.SZ",
                     "600272.SS","000833.SZ")
getSymbols(holding.tickers, from = "2015-09-01")
position <- c(25,400,300,300,400,200,100,1000)

portfolio <- length(position) %>% rep(1,.)
for (i in 1:length(portfolio)){
  portfolio[i] <- position[i] * last(Cl(get(holding.tickers[i])))
}


VaRdata <- get(holding.tickers[1]) %>% dailyReturn()

for (ticker in holding.tickers[-1]){
  x <- get(ticker) %>% dailyReturn() 
  VaRdata <- merge(VaRdata,x)
}


s <- VaR(VaRdata, p = 0.8, method = "modified",
         portfolio_method = "component",
         weights = portfolio / sum(portfolio))
# risk.exposure <- as.vector (-s$contribution * portfolio )
# print(risk.exposure)
total.exposure <- sum(portfolio) * (-s$MVaR)
print(total.exposure)
risk.contribution <- as.vector(s$pct_contrib_MVaR*100) 
names(risk.contribution) <- holding.tickers
#print(risk.contribution)
pie(s$pct_contrib_MVaR,
    labels = paste(holding.tickers," ",round(risk.contribution,2),"%"),
    col=rainbow(length(portfolio)))

rbind(round(risk.contribution > 100 * portfolio / sum(portfolio)),
     risk.contribution - 100 * portfolio / sum(portfolio))


# risk.exposure <- as.vector (s * portfolio )
# names(risk.exposure) <- holding.tickers
# print(risk.exposure)
# total.exposure <- sum(risk.exposure) 
# print(total.exposure)
#fit <- armaFit(~arma(1,1),data=x)

# layout(matrix(seq(1:length(portfolio)),ncol=2,byrow=TRUE))
# apply(VaRdata,2,chart.VaRSensitivity)
# layout(matrix(1))
}


renco.risk.management()