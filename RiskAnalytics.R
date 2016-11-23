#function for risk management
renco.risk.management <- function(){

require(quantmod)
require(PerformanceAnalytics) #Do VaR
require(magrittr)

holding.tickers <- c("000002.SZ"
                    )
#get data
getSymbols(holding.tickers, from = "2015-09-01")
position <- c(400)

portfolio <- length(position) %>% rep(1,.)
for (i in 1:length(portfolio)){
  #calculate the value of position
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
total.exposure <- data.frame(sum(portfolio) * (-s$MVaR))
names(total.exposure) <- "Total Exposure"
print(total.exposure)
risk.contribution <- as.vector(s$pct_contrib_MVaR*100) 
names(risk.contribution) <- holding.tickers
#print(risk.contribution)

###draw a pie graph for risk attribution
pie(s$pct_contrib_MVaR,
    labels = paste(holding.tickers," ",round(risk.contribution,2),"%"),
    col=rainbow(length(portfolio)))

risk.summary <- rbind(round(risk.contribution > 100 * portfolio / sum(portfolio)),
     risk.contribution - 100 * portfolio / sum(portfolio)) %>% t() 
risk.summary <- data.frame(risk.summary)

names(risk.summary) <- c("Risk Contribution","Active Risk")

print(risk.summary)
#Renco defines active risk as the risk contribution in excess of 
#portfolio weight

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