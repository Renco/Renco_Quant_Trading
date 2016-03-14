###Get the dynamic stop loss line for current holding
holding <- c('600988','600237','000008','600425','000596')
holding <- paste(holding, ifelse(as.integer(holding) < 1000, "SZ","SS"),sep=".")

getSymbols(holding,from="2016-01-01") #fetch data
cut_loss <- rep(0,length.out = length(holding))

for (i in 1:length(holding)){
  cut_loss[i] <- last(Cl(get(holding[i])))  - last(ATR(get(holding[i])))$atr
}

print(data.frame(holding,cut_loss))