require(quantmod)
require(fArma)
require(fGarch)
require(magrittr)

ticker <- "600419.SS"
getSymbols(ticker, from = "2015-01-01")

subset(get(ticker), paste(ticker,".Volume",sep="")>0)

#<- as.vector( (dailyReturn(Cl(subset(get(ticker),Volume >0))))
                  
Adata <- which(get(ticker)[,5]>0) %>% get(ticker)[.,] %>% Cl() %>% 
  dailyReturn() %>%  as.vector() 

                    
par(mfrow = c(2, 1), cex = 0.7)
acf(Adata)
pacf(Adata)
par(mfrow = c(1, 1))

fit <- armaFit(~ arma(1,1), data = Adata)
print(fit)

##ARMA analytics
par(mfrow = c(2, 2), cex = 0.7)
summary(fit, which = "all")
# Forecast 5 Steps Ahead:
par(mfrow = c(1, 1))
predict(fit, 5)
# Graph of fitness
plot(Adata,col="red",type='l')
lines(fitted(fit))


gfit <- garchFit(~garch(1,1),data= Adata,trace=FALSE)
summary(garchfit)
#plot(garchfit,which="all")
###
plot(Adata,col="red",type='l')
lines(fitted(gfit))
predict(gfit)
