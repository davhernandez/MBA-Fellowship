#setup -------------------------------
library(readxl)
library(dplyr)
library(lubridate)

#DKK Exchange rate ---------------------------------------------------------

#Faroe Islands data is by month, so you just have to trim off the day and average over the months
dkk_exchange_rate <- read.csv("data/Salmon public data/DKKExchangeRate.csv")
colnames(dkk_exchange_rate) <- c("Date", "Rate")

#mutate to get a month and a year
dkk_exchange_rate$Date <- as.Date(dkk_exchange_rate$Date, "%d-%b-%y")
dkk_exchange_rate <- dkk_exchange_rate[-1,] %>%
  mutate(Month = as.numeric(month(.$Date)),
         Year = as.numeric(year(.$Date))) %>%
  group_by(Month, Year) %>%
  summarise(Rate = mean(Rate))

#Faroe Island Monthly Exports to US ------------------------------------------------

#Monthly price and weight of all salmon products exported to the US

faroe_US_aggregate <- read_xlsx("data/Salmon public data/Faroe_exports_to_US.xlsx")

colnames(faroe_US_aggregate) <- c("1","2","Date", "Price", "Weight")
#select rows and columns to keep
faroe_US_aggregate <- faroe_US_aggregate %>%
  select(Date, Price, Weight) %>%
  filter(!is.na(.$Date)) %>%
  mutate(Price = as.numeric(Price),
         Weight = as.numeric(Weight))

#find and replace `-` with `0`
faroe_US_aggregate[is.na(faroe_US_aggregate)] <- 0

#split date into a year and month column
faroe_US_aggregate$Year <- apply(faroe_US_aggregate, 1, function(x) as.numeric(substr(x["Date"], 1,4))) #Year
faroe_US_aggregate$Month <- apply(faroe_US_aggregate, 1, function(x) as.numeric(substr(x["Date"], 6,7))) #Month


#!!!!!!!!!!!the problem is that x["Price"] is changing the value to a character instead of just keeping it as a numeric. I could just wrap it in as.numeric, but that seems like a poor workaround !!!!!!!!!!!!!!

#convert to USD
dummy <- apply(faroe_US_aggregate,
      1,
      function(x) (x["Price"] / dkk_exchange_rate$Rate[which(dkk_exchange_rate$Month == x["Month"] & dkk_exchange_rate$Year == x["Year"])]))
#match month and year to each other

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
         Year = isoyear(Date)) #get year according to ISO 8601 week calendar

  #NOTE: `isoyear` returns the value as a decimal numer rather than an integer number (which is what as.numeric would do). This is why the `apply` function used below for the 


#Norway weekly prices ----------------------------------------------------------

norway_prices <- read_xls("data/Salmon public data/NorwayPricesWeek.xls")
colnames(norway_prices) <- norway_prices[8,] #use row 8 to grab column names
norway_prices <- norway_prices[-(1:8),1:9] #keep only the columns of interest
norway_prices <- as.data.frame(apply(norway_prices, 2, as.numeric))

#convert to USD
norway_prices[n,2:9] <- as.data.frame( #make it into a dataframe
  t( #transpose the output b/c for some reason it is a matrix
    apply(norway_prices, 1,  function(x)(x[3:9] / nok_exchange_rate$Rate[which(nok_exchange_rate$Year == x["Year"] & nok_exchange_rate$Week == x["Week"])])) #matches the exchange rate for the week and year of the row, then divides all of the values by the rate
  )
)
