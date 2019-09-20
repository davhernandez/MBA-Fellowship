#setup ------------------------------------------------------------------
library(readxl)
library(dplyr)
library(lubridate)
library(aTSA)

#DKK Exchange rate --------------------------------------------------------------------------

#Faroe Islands data is by month, so you just have to trim off the day and average over the months
dkk_exchange_rate <- read.csv("data/Salmon public data/Exchange Rates/DKKExchangeRate.csv")
colnames(dkk_exchange_rate) <- c("Date", "Rate")

#mutate to get a month and a year
dkk_exchange_rate$Date <- as.Date(dkk_exchange_rate$Date, "%d-%b-%y")
dkk_exchange_rate <- dkk_exchange_rate[-1,] %>%
  mutate(Month = as.numeric(month(.$Date)),
         Year = as.numeric(year(.$Date))) %>%
  group_by(Month, Year) %>%
  summarise(Rate = mean(Rate))

#Faroe Island Monthly Exports to US ----------------------------------------------------------------------------

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

#convert to USD
faroe_US_aggregate$Price <- apply(faroe_US_aggregate[,-1],
      1,
      function(x) (x["Price"] / dkk_exchange_rate$Rate[which(dkk_exchange_rate$Month == x["Month"] & dkk_exchange_rate$Year == x["Year"])]))

#Faroe Islands Monthly Exports to All Countries -----------------------------------------------------------

#Monthly Price and weight of all salmon products exported to all countries
  #ie the total exports of salmon from Faroe Islands

faroe_worldwide_aggregate <- read_xlsx("data/Salmon public data/Faroe_all_exports.xlsx")

colnames(faroe_worldwide_aggregate) <- c("1","2","Date", "Price", "Weight")
#select rows and columns to keep
faroe_worldwide_aggregate <- faroe_worldwide_aggregate %>%
  select(Date, Price, Weight) %>%
  filter(!is.na(.$Date)) %>%
  mutate(Price = as.numeric(Price),
         Weight = as.numeric(Weight))

#find and replace `-` with `0`
faroe_worldwide_aggregate[is.na(faroe_worldwide_aggregate)] <- 0

#split date into a year and month column
faroe_worldwide_aggregate$Year <- apply(faroe_worldwide_aggregate, 1, function(x) as.numeric(substr(x["Date"], 1,4))) #Year
faroe_worldwide_aggregate$Month <- apply(faroe_worldwide_aggregate, 1, function(x) as.numeric(substr(x["Date"], 6,7))) #Month

#convert to USD
faroe_worldwide_aggregate$Price <- apply(faroe_worldwide_aggregate[,-1],
                                  1,
                                  function(x) (x["Price"] / dkk_exchange_rate$Rate[which(dkk_exchange_rate$Month == x["Month"] & dkk_exchange_rate$Year == x["Year"])]))

# Faroe Islands Monthly Exports by type -----------------------------------------------------------------

#Breakdown of product type exported from the Faroe Islands


#Import Weights
faroe_product_type_weight <- t(read_xlsx("data/Salmon public data/Faroe_tons_months.xlsx"))
colnames(faroe_product_type_weight) <- faroe_product_type_weight[2,]
faroe_product_type_weight <- as.data.frame(faroe_product_type_weight[-c(1,2),2:8])

faroe_product_type_weight <- faroe_product_type_weight %>%
  mutate(Month = match(.$Month, month.abb), #change month to numeric
         type = "weight") #new column for data type
faroe_product_type_weight[,1:7] <- as.data.frame(apply(faroe_product_type_weight[,1:7], 2, as.numeric)) #convert factors to numeric
faroe_product_type_weight[is.na(faroe_product_type_weight)] <- 0 #remove NAs


#Import Price data
faroe_product_type_price <- t(read_xlsx("data/Salmon public data/Faroe_price_months.xlsx"))
colnames(faroe_product_type_price) <- faroe_product_type_price[2,]
faroe_product_type_price <- as.data.frame(faroe_product_type_price[-c(1,2),2:8]) #remains as a matrix so that you can use apply

#apply is anoying me so I decided to do it with more lines of code
faroe_product_type_price <- faroe_product_type_price %>%
  mutate(Month = match(.$Month, month.abb),
         type = "price")
faroe_product_type_price[,1:7] <- as.data.frame(apply(faroe_product_type_price[,1:7], 2, as.numeric)) #convert factors to numeric
faroe_product_type_price[is.na(faroe_product_type_price)] <- 0 #remove NAs

faroe_product_type_price <- left_join(faroe_product_type_price, dkk_exchange_rate, by = c("Month", "Year"))

faroe_product_type_price[,3:6] <- apply(faroe_product_type_price[,c(3:6)], 2, function(x) x/faroe_product_type_price$Rate) #convert price to USD


#combine both dataframes with rbind
faroe_product_type <- rbind(faroe_product_type_weight, faroe_product_type_price[,-9])
rm(list = "faroe_product_type_price", "faroe_product_type_weight")

#NOK Exchange rate ------------------------------------------------------------------------------

nok_exchange_rate <- read.csv("data/Salmon public data/Exchange Rates/NOKExchangeRate.csv")
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

  #NOTE: `isoyear` returns the value as a decimal number rather than an integer number (which is what as.numeric would do). This is why the `apply` function used below for the conversion has trouble


#Norway weekly prices ----------------------------------------------------------------------------------

norway_prices <- read_xls("data/Salmon public data/NorwayPricesWeek.xls") #covers 1995, week 1 to 2013, week 13
colnames(norway_prices) <- norway_prices[8,] #use row 8 to grab column names
norway_prices <- norway_prices[-(1:8),1:9] #keep only the columns of interest
norway_prices <- as.data.frame(apply(norway_prices, 2, as.numeric))

nasdaq_prices <- read_xls("data/Salmon public data/NASDAQsalmonIndexHistory.xls") #covers 2013, week 14 to 2019, week 22
colnames(nasdaq_prices) <- nasdaq_prices[5,] #use row 5 to grab column names
nasdaq_prices <- nasdaq_prices[-(1:5), 1:11] #keep only columns of interest
nasdaq_prices <- apply(nasdaq_prices, 2, as.numeric)
nasdaq_prices <- as.data.frame(cbind(nasdaq_prices, apply(nasdaq_prices[,9:11], 1, mean, na.rm = TRUE))) #turn 7-9kg columns into a mean which represents 7+ category
nasdaq_prices <- nasdaq_prices[,-(9:11)]
colnames(nasdaq_prices)[9] <- "7+"

#combine into a single data frame
norway_prices <- rbind(norway_prices, nasdaq_prices)
rm(list = "nasdaq_prices")

#convert to USD
norway_prices[,3:9] <- as.data.frame( #make it into a dataframe
  t( #transpose the output b/c it is a matrix
    apply(norway_prices, 1,  function(x)(x[3:9] / nok_exchange_rate$Rate[which(nok_exchange_rate$Year == x["Year"] & nok_exchange_rate$Week == x["Week"])])) #matches the exchange rate for the week and year of the row, then divides all of the values by the rate
  )
)

#Norway stationary test ------------------------------------------------------------------
for(i in 3:9){
  print(paste(i, "################################################################################", ""))
  adf.test(norway_prices[,i], nlag = 23)
}

#CLP Exchange rate --------------------------------------

clp_exchange_rate <- read.csv("data/Salmon public data/Exchange Rates/CLPExchangeRate.csv")
colnames(clp_exchange_rate) <- c("Date", "Rate")
clp_exchange_rate$Date <- as.Date(clp_exchange_rate$Date, "%d-%b-%y")
#Chilean exchange rate is by week, so you have to fragment by weeks
clp_exchange_rate$Date <- cut(clp_exchange_rate$Date, breaks = "week", start.on.monday = TRUE)

#average over each week
clp_exchange_rate <-  clp_exchange_rate %>%
  group_by(Date) %>%
  summarise(Rate = mean(Rate)) %>%
  mutate(Week = isoweek(Date), #get the week number
         Year = isoyear(Date)) #get year according to ISO 8601 week calendar


#Chile Monthly Imports to US ---------------------------------------------------------------------------------

#Monthly imports of various salmon products to the US
  #based on US Census data

chile_product_type <- read_xlsx("data/Salmon public data/Chile_salmon_imports_month.xlsx")

chile_product_type$`Product Name` <- chile_product_type$`Product Name` %>%
  gsub("SALMON ATLANTIC,DANUBE FRESH", "Danube Atlantic Salmon, Fresh", x = .) %>%
  gsub("SALMON ATLANTIC,DANUBE FROZEN", "Danube Atlantic Salmon, Frozen", .) %>%
  gsub("SALMON ATLANTIC,DANUBE STEAKS FRESH", "Danube Atlantic Salmon, Fresh Steaks", .) %>%
  gsub("SALMON ATLANTIC FRESH FARMED", "Atlantic Salmon, Fresh Farmed", .) %>%
  gsub("SALMON ATLANTIC FRESH WILD", "Atlantic Salmon, Fresh Wild", .) %>%
  gsub("SALMON ATLANTIC FRESH", "Atlantic Salmon, Fresh", .) %>%
  gsub("SALMON ATLANTIC FILLET FRESH FARMED", "Atlantic Salmon, Fresh Fillet Farmed",.) %>%
  gsub("SALMON ATLANTIC FILLET FRESH WILD", "Atlantic Salmon, Fresh Fillet Wild",.) %>%
  gsub("SALMON ATLANTIC FILLET FRESH", "Atlantic Salmon, Fresh Fillet Unclassified",.) %>%
  gsub("SALMON ATLANTIC FILLET FROZEN", "Atlantic Salmon, Frozen Fillet Unclassified",.) %>%
  gsub("SALMON ATLANTIC MEAT FRESH FARMED", "Atlantic Salmon, Fresh Meat Farmed",.) %>%
  gsub("SALMON ATLANTIC MEAT FRESH WILD", "Atlantic Salmon, Fresh Meat Wild",.)
