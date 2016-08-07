###Get the dynamic stop loss line for current holding
holding <- c("000766","600419","000839")
holding <- paste(holding, ifelse(as.integer(holding) < 60000, "SZ","SS"),sep=".")

getSymbols(holding,from="2016-03-01",to="2016-07-18") #fetch data
cut_loss <- rep(0,length.out = length(holding))
scale.up <- rep(0,length.out = length(holding))

for (i in 1:length(holding)){
  cut_loss[i] <- last(Cl(get(holding[i])))  - last(ATR(get(holding[i])))$atr
  scale.up[i] <- last(Cl(get(holding[i])))  + last(ATR(get(holding[i])))$atr
}

print(data.frame(holding,cut_loss,scale.up,order.size = 1000/(scale.up - cut_loss)))
