###back testing volatility timing 
require(quantmod)
require(xts)
require(data.table)
require(lubridate)
require(PerformanceAnalytics)
require(magrittr)
require(ggplot2)
require(ggthemes)
require(dplyr)
require(reshape2)

setwd("/Users/renco/GitHub/Renco_Quant_Trading")

start_date <- as.Date("2000-01-01")
end_date <- today()

# get market data ---------------------------------------------------------

#Is market in distress?
getSymbols("000001.SS", from = start_date,
           to = end_date)
sh <- dailyReturn(get("000001.SS"))
sh <- data.table(coredata(sh), index(sh))
names(sh) <- c("d.ret", "date")
sh <- sh[d.ret !=  0 ]

#a plot of daily volatility 
plot(sh$date, sh$d.ret^2, type = "l", col="blue")

#get the thirty day volatility window 
data <- sh
data[,"back.vol" := rollapply(d.ret, width = 30, FUN = sd,fill = NA, align = "right")]
#lag one day so that vol is known before market openning 
data[,"back.vol" := shift(back.vol, n = 1L)]
data <- data[!is.na(back.vol)]

cor(data$d.ret, data$back.vol, use = "complete.obs")
ggplot(data) + geom_line(aes(x=date, y = back.vol), col = "red") +
  geom_line(aes(x=date, y = (d.ret)), col = "blue", alpha = 0.5) + 
  theme_few()

#create vol quintile 
data[,"vol.bin" := cut(back.vol, breaks = 5, include.lowest = TRUE,
                       labels = seq(1,5))]

#summarize data 
sum.tab <- data %>% group_by(vol.bin) %>%
  summarise(avg.ret = mean(d.ret),
            avg.vol = mean(back.vol),
            sr = avg.ret / avg.vol)

sum.tab <- melt(sum.tab, id.vars = "vol.bin")
ggplot(sum.tab) + geom_bar(aes(x=vol.bin, y=value, fill = variable),
                           stat = "identity") +
  facet_grid(.~variable, scales = "free_y")


#reduce exposure when the vol is in the hightest bin
data[,"position" := ifelse(vol.bin == 5 | vol.bin == 4, 0.5, 1)]
rf <- 0.0025 / 225
data[,"strat.ret" := cumprod(1 + d.ret * position + rf * (1 - position))]

ggplot(data) + geom_line(aes(x=date, y = strat.ret), col = "red") +
  geom_line(aes(x=date, y = cumprod(1+d.ret)), col = "blue", alpha = 0.5) + 
  theme_few()

ggplot(data) + geom_bar(aes(x=date, y = vol.bin), stat = "identity",col = "blue",
                        alpha = 0.5) 

# ggplot(data, aes(x=d.ret, y = stat(density), col = vol.bin)) +
#   geom_freqpoly(binwidth = 0.005 * 4)
# 
# ggplot(data, aes(x=d.ret, y = stat(density), fill = vol.bin)) +
#   geom_histogram(bins = 60, alpha = 0.5, position = "identity", col="black")

ggplot(data, aes(x=d.ret, y = stat(density), fill = vol.bin)) +
  geom_density(alpha = 0.3, position = "identity", col="black")

# which vol bin are we in now? --------------------------------------------


