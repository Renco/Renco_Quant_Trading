#function for risk management
holding.tickers <- c("000002.SZ",
                     "600628.SS",
                     "000623.SZ",
                     "600519.SS",
                     "601336.SS",
                     "600383.SS",
                     "000911.SZ"
                     )
position <- c(400,300,300,100,200,400,0)
start.date <- "2016-07-05"
past.high <- c(28.25,16.18,36.99,318,48.56,14.6,24.35)

renco.risk.management <- function(holding.tickers,position,start.date){

require(quantmod)
require(PerformanceAnalytics) #Do VaR
require(magrittr)
require(ggplot2)
require(reshape2)

#get data
getSymbols(holding.tickers, from = start.date)



#################################
##housekeeping
################################
#number of tickers 
num <- length(holding.tickers)
#ticker names
tickerNum <- character()
for (i in 1:num){
  tickerNum <- c(tickerNum,strsplit(holding.tickers[i],"[.]")[[1]][1])
}
#common data period for stocks 
common.date <- get(holding.tickers) %>% index()
#market return
getSymbols("000001.ss",from = start.date)
#dates that index value is available
index.date <- index(`000001.SS`)
#dates that beta estimation is available
beta.date <- as.Date(intersect(common.date,index.date))
# VaR management ----------------------------------------------------------


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


s <- VaR(VaRdata, p = 0.90, method = "modified",
         portfolio_method = "component",
         weights = portfolio / sum(portfolio))
# risk.exposure <- as.vector (-s$contribution * portfolio )
# print(risk.exposure)
total.exposure <- data.frame(sum(portfolio) * (-s$MVaR))
names(total.exposure) <- "Total Exposure"
print(total.exposure)
risk.contribution <- as.vector(s$pct_contrib_MVaR*100) 
names(risk.contribution) <- tickerNum
#print(risk.contribution)

###draw a pie graph for risk attribution
pie(s$pct_contrib_MVaR,
    labels = paste(holding.tickers," ",round(risk.contribution,2),"%"),
    col=rainbow(length(portfolio)))

risk.summary <- rbind(
  round(risk.contribution > 100 * portfolio / sum(portfolio)),
  s$pct_contrib_MVaR * 100,
  100 * portfolio / sum(portfolio),
  risk.contribution - 100 * portfolio / sum(portfolio)) %>% t() 
risk.summary <- data.frame(risk.summary)

names(risk.summary) <- c("Addition?","RiskShare","PortfolioShare","ActiveRisk")

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
portfolio <- data.frame(t(portfolio))
names(portfolio) <- holding.tickers
print("Port Values")
print(portfolio)
print(paste("total portfolio value",sum(portfolio)))



# Beta Management--------------------------------------------------------------------

BetaData <- data.frame(get(holding.tickers[1]) %>% dailyReturn())

for(i in 2:num){
  BetaData <- data.frame(BetaData,
                         get(holding.tickers[i]) %>% dailyReturn() )
}
names(BetaData) <- tickerNum

#show a graph of return series
beta.df <- data.frame()
for(i in 1:num){
  temp.data <- BetaData[,i]
  temp.len <- length(temp.data)
  temp.data <- data.frame(common.date,temp.data, 
                          rep(names(BetaData)[i],temp.len))
  beta.df <- rbind(beta.df,temp.data)
}
names(beta.df) <- c("date","return","ticker")
#candy subsetting by dates
beta.df <- beta.df[beta.df$date %in% beta.date,] 
#add market data 
temp.data <- get("000001.SS") %>% dailyReturn()
temp.len <- length(temp.data)
temp.data <- data.frame(beta.date,temp.data, 
                        rep("Shanghai",temp.len))
names(temp.data) <- c("date","return","ticker")
beta.df <- rbind(beta.df,temp.data)

print("Daily Vols are")
stock.vol <- apply(BetaData,2,sd) * 100
print(stock.vol)
#momentum open
momentum.open <- past.high * (1  - 1/100 * 3 * stock.vol)
print("momentum open are")
print(momentum.open)

#a graph for return series
p1 <- ggplot(beta.df,aes(x=date,y=return,colour = ticker)) + geom_line() +
  facet_wrap(~ ticker,ncol=2) + geom_smooth()
print(p1)

beta.fit <- list()
beta.summary <- data.frame()
beta.vec <- numeric()
market.ret <- get("000001.SS") %>% dailyReturn()
for(i in 1:num){
  #######consider adding shibor rate for correction
  stock.ret <- subset(beta.df,ticker == tickerNum[i],select = return)
  stock.ret <- cbind(beta.date, stock.ret)
  stock.ret <- xts(stock.ret$return,order.by = stock.ret$beta.date)
  temp.fit <- lm(stock.ret ~ market.ret )
  beta.fit[[i]] <- temp.fit 
  beta.vec <- c(beta.vec,temp.fit$coefficients[2])
}
names(beta.vec) <- tickerNum
print("Beta Estimate")
print(beta.vec)
total.beta <- sum(beta.vec * risk.summary$PortfolioShare /100 )
print(paste("The total Beta exposure is: ",total.beta))

CAPM.df <- data.frame(as.numeric(tail(BetaData)[5,]),beta.vec)
names(CAPM.df) <- c("ex_return","beta")
capm.plot <- ggplot(CAPM.df, aes(x =beta,y=ex_return,
                                 label = tickerNum)) + 
  geom_smooth(formula = y ~ x, method =lm) + geom_point() + 
  geom_text(vjust = -0.3) + labs(title = "Security Market Line")
print(capm.plot)



return(beta.fit)
}

# 
 beta.fitt <- renco.risk.management(holding.tickers,position,start.date) #VAR risk
# 
# for(i in 1:length(beta.fit)){
#   print(holding.tickers[i])
#   print(summary(beta.fit[[i]]))
#   acf(beta.fitt[[i]]$residuals)
# }




