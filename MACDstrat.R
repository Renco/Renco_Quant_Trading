##Technical Analysis of Renco 

code = 000613
Is_Indice = 0
location = c("SS","SZ")
loc_code = 2
start_date = "2014-09-01"



if(typeof(code) != "character"){ 
  #Indices and US stocks are characters
  symbol = paste(sprintf("%06d", code),
                 ifelse(code <= 6000, "SZ","SS"),
                 sep=".")
} else if(Is_Indice == 1){
  symbol = paste("^",code,sep="") 
} else {
  symbol = code
}


getSymbols(symbol,from=start_date) #Fecthing data

# #####Chart
# if(Is_Indice !=1){
#   chartSeries(adjustOHLC(get(symbol),use.Adjusted=TRUE),
#               theme="white",name=symbol)
# } else {
#   chartSeries(get(code),theme="white",name=symbol)
# }
# ###TA to use
# #addADX(n=14,maType="EMA",wilder=TRUE)
# addMACD()
# addBBands()
# #addATR()
# #addSAR()
# ##Donchian Chanles
# Has_Donchian = 0
# if(Has_Donchian == 1){
#   dc <- lag(DonchianChannel(cbind(Hi(get(symbol)), Lo(get(symbol)))))
#   addTA(dc$high, on=1, col='purple')
#   addTA(dc$low, on=1, col='green4')
# }
# ##Moving Averages
# ##Note I haven't work on colors of MA yet 
# Has_MA = 1
# if(Has_MA == 1)
# {
#   addSMA(n=10,on=1,with.col = Cl, overlay = TRUE, col = "red")
#   addSMA(n=100,on=1,with.col = Cl, overlay = TRUE, col = "cyan")
# }
# 

##Take out zero volume days 


# require(quantomd)
# 
# symbol <- "000001.SS"
# getSymbols("000001.SS")



##################MACD
if(Is_Indice !=1){
SSmacd <- MACD(Cl(get(symbol)))
} else{
  SSmacd <- MACD(Cl(get(code)))
}


###################Turtle
dc <- lag(DonchianChannel(
            cbind(Hi(adjustOHLC(get(symbol),use.Adjusted=TRUE)),
            Lo(adjustOHLC(get(symbol),use.Adjusted=TRUE)))),
          n=20)


#plot(SSmacd$macd - SSmacd$signal)
holding.choice <- (SSmacd$macd > SSmacd$signal)
#turtle.choice <- (last(Cl(last(adjustOHLC(get(symbol),use.Adjusted=TRUE))))
                 > last(dc)$high)

if(Is_Indice !=1){
  xx <- get(symbol)
} else{
  xx <- get(code)
}

cumulative <- 1 + dailyReturn(Cl(xx)) * 
                ifelse(is.na(holding.choice),0,holding.choice)
# turtle <- 1 + dailyReturn(Cl(xx)) * 
#   ifelse(is.na(turtle.choice),0,holding.choice)
cumulative.return <- cumprod(cumulative)
#turtle.return <- cumprod(turtle)
SS.return <- cumprod(1 + dailyReturn(xx))
plot(as.vector(cumulative.return),col="red",type='l',
     ylim=c(min(SS.return),max(cumulative.return)))
lines(as.vector(SS.return))
#lines(as.vector(turtle.return),col="blue")