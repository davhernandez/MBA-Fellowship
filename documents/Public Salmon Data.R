#setup -------------------------------
library(readxl)
library(dplyr)
library(lubridate)

#DKK Exchange rate ---------------------------------------------------------

#Faroe Islands data is by month, so you just have to trim off the day and average over the months
dkk_exchange_rate <- read.csv("data/Salmon public data/DKKExchangeRate.csv")
colnames(dkk_exchange_rate) <- c("Date", "Rate")

#NOK Exchange rate -----------------------------------------------------------

nok_exchange_rate <- read.csv("data/Salmon public data/NOKExchangeRate.csv")
colnames(nok_exchange_rate) <- c("Date", "Rate")
nok_exchange_rate$Date <- as.Date(nok_exchange_rate$Date, "%d-%b-%y")
#Norway exchange rate is by week, so you have to fragment by weeks
nok_exchange_rate$Date <- cut(nok_exchange_rate$Date, breaks = "week", start.on.monday = TRUE)

#average over each week
nok_exchange_rate <-  nok_exchange_rate %>%
  group_by(Date) %>%
  summarise(Rate = mean(Rate)) %>%
  mutate(Week = isoweek(Date), #get the week number
         Year = as.numeric(format(as.Date(.$Date), "%Y"))) #convert `week` from factor to date, then convert that to a numeric

#Norway weekly prices ----------------------------------------------------------

norway_prices <- read_xls("data/Salmon public data/NorwayPricesWeek.xls")
colnames(norway_prices) <- norway_prices[8,] #use row 8 to grab column names
norway_prices <- norway_prices[-(1:8),1:9] #keep only the columns of interest
norway_prices <- as.data.frame(apply(norway_prices, 2, as.numeric))
#convert to USD
dummy <- left_join(norway_prices, nok_exchange_rate[,c("Year","Week","Rate")], by = c("Year", "Week"))


#!!!!!!!!for some reason, it is cutting off three entries when it does the apply!!!!!!!!
#!!!!!!!!find out what is going wrong!!!!!!!!!
#trying this out by row
as.data.frame(do.call(rbind,
        apply(norway_prices, 1,  function(x)(x[3:9] / nok_exchange_rate$Rate[which(nok_exchange_rate$Year == x["Year"] & nok_exchange_rate$Week == x["Week"])]))
))
