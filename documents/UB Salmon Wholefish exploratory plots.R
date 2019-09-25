# UB Salmon Wholefish exploratory plots
#plottting the UB salmon data for high, low, & average

#setup ---------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(stringr) #for str_detect
library(purrr) #trying to use `map` for better for loops
#https://speakerdeck.com/jennybc/row-oriented-workflows-in-r-with-the-tidyverse?slide=29

#import data ---------------------------
setwd("~/Desktop/Grad School/github/MBA Fellowship")

wholefish_average <- read.csv("data/UB Salmon data/Comtell Salmon Whole average.csv")
#wholefish_all <- read.csv("data/UB Salmon data/Comtell Salmon wholefishs high low average.csv")

wholefish_average <- wholefish_average[-1,] #remove extra row with data types
wholefish_average <- wholefish_average[-(1132:1134),] #remove summary rows from the bottom

colnames(wholefish_average)[1] <- "Date" #rename first column
wholefish_average$Date <- as.Date(wholefish_average$Date, format = "%m/%d/%Y") #reformat the date column to be a date

#rename columns
colnames(wholefish_average)[2:34] <- c("Northeast, 6-8 lb", "Northeast, 8-10 lb", "Northeast, 10-12 lb", "Northeast, 12-14 lb", "Northeast, 14-16 lb", "Northeast, 16-18 lb",
                                       "West Coast Seattle, 6-8 lb", "West Coast Seattle, 8-10 lb", "West Coast Seattle, 10-12 lb", "West Coast Seattle, 12-14 lb", "West Coast Seattle, 14-16 lb", "West Coast Seattle, 16-18 lb",
                                       "West Coast LA, 6-8 lb", "West Coast LA, 8-10 lb", "West Coast LA, 10-12 lb", "West Coast LA, 12-14 lb", "West Coast LA, 14-16 lb", "West Coast LA, 16-18 lb",
                                       "Norway Container Load, 5-6 kg", "Norway Container Load, 6-7 kg", "Norway Container Load, 7-8 kg", "Norway Container Load, 8-9 kg",
                                       "Scotland Container Load, 6-7 kg", "Scotland Container Load, 7-8 kg", "Scotland Container Load, 8-9 kg",
                                       "Faroe Island Container Load, 6-7 kg", "Faroe Island Container Load, 7-8 kg",
                                       "Canada, 4-6 lb", "Canada, 6-8 lb", "Canada, 8-10 lb", "Canada, 10-12 lb", "Canada, 12-14 lb", "Canada, 14-16 lb"
)
                                       
wholefish_average[,2:34] <- apply(wholefish_average[,2:34], 2, function(x) as.numeric(as.character(x))) #convert factors to numeric

#change data to long form
wholefish_average <- melt(wholefish_average, id.vars = "Date",
                       measure.vars = 2:34,
                       value.name = "Average$/lb",
                       variable.name = "source",
                       na.rm = FALSE) #leaving NAs preserves the gaps when plotting

#all data, single plot --------------------------------------------------------
ggplot(wholefish_average, aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average price of wholefish")

#overview of all wholefish sources ---------------------------------------------------------------
#breaks all the data out into different countries, then makes a composite graph

wholecomp1 <- wholefish_average[grep("Northeast", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average Northeast wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

wholecomp2 <- wholefish_average[grep("Seattle", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average West Coast Seattle wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

wholecomp3 <- wholefish_average[grep("LA", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average West Coast LA wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

wholecomp4 <- wholefish_average[grep("Norway", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average Norway wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

wholecomp5 <- wholefish_average[grep("Scotland", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average Scotland wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

wholecomp6 <- wholefish_average[grep("Faroe", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average Faroe Island wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

wholecomp7 <- wholefish_average[grep("Canada", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average Canada wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

grid.arrange(wholecomp1, wholecomp2, wholecomp3, wholecomp7, wholecomp4, wholecomp5, wholecomp6) #build composite plot

rm(list = ls(pattern = "wholecomp")) #remove standalone plots from environment

#comparing similar size fish ---------------------------------------------
#grouping data based on the size bin of the fish

#6-8lb
lb68 <- wholefish_average[grep("6-8 lb", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 6-8 lb wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

#8-10lb
lb810 <- wholefish_average[grep("8-10 lb", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 8-10 lb wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

#10-12lb
lb1012 <- wholefish_average[grep("10-12 lb", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 10-12 lb wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

#12-14lb
lb1214 <- wholefish_average[grep("12-14 lb", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 12-14 lb wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

#14-16lb
lb1416 <- wholefish_average[grep("14-16 lb", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 14-16 lb wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

#16-18lb
lb1618 <- wholefish_average[grep("16-18 lb", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 16-18 lb wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

grid.arrange(lb68, lb810, lb1012, lb1214, lb1416, lb1618)

#5-6kg
kg56 <- wholefish_average[grep("5-6 kg", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 5-6 kg wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

#6-7 kg
kg67 <- wholefish_average[grep("6-7 kg", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 6-7 kg wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

#7-8 kg
kg78 <- wholefish_average[grep("7-8 kg", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 7-8 kg wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

#8-9 kg
kg89 <- wholefish_average[grep("8-9 kg", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 8-9 kg wholefish") +
  ylim(c(0.5,6)) +
  scale_x_date(breaks = "2 year", date_labels = "%Y")

grid.arrange(kg56, kg67, kg78, kg89)

#Deviation from mean --------------------------------------------

#Some notes on the function:
#It calculates the means for each Date for which there is data available
#for any data which doesn't have data, I dropped them by match the dates in `means` back to the dates in the data

deviation <- function(data_source, grouping = NA, title = NA){
  #subset the data with ifelse
  ifelse(is.na(grouping), print(":)"),
        data_source <- data_source[grep(grouping, data_source$source),]
         )
  #find the mean for each time point
  means <- aggregate(`Average$/lb` ~ Date, data = data_source, FUN = mean)
  #drop dates with no data
  data_source <- data_source[which(data_source$Date %in% means$Date),]
  
  #calculate deviation from mean
  data_source$dev_mean <- apply(data_source, 1, function(x)

    #convert `data_source` back to numeric, then find the row in `means` with matching date and subtract the mean price from the price in that row of `data_source`
    as.numeric(x["Average$/lb"]) - means$`Average$/lb`[which(means$Date == x["Date"])]
  )

  #plot
  ggplot(data_source, aes(x = Date, y = dev_mean, color = source)) +
    geom_point(stat = "identity") +
    geom_smooth() +
    ggtitle(title)
}


#matrix for subsetting and generating plot titles
app_wholefish <- matrix(c("6-8 lb", "8-10 lb", "10-12 lb", "12-14 lb", "14-16 lb", "16-18 lb", "6-7 kg", "7-8 kg", "8-9 kg",
                          "Wholefish 6-8 lb", "Wholefish 8-10 lb", "Wholefish 10-12 lb", "Wholefish 12-14 lb", "Wholefish 14-16 lb", "Wholefish 16-18 lb", "Wholefish 6-7 kg", "Wholefish 7-8 kg", "Wholefish 8-9 kg"),
                        nrow = 9,
                        ncol = 2,
                        dimnames = list(c(),
                                        c("group", "plot_title")))
  
#run it
apply(app_wholefish, 1, function(x) deviation(data_source = wholefish_average, grouping = x["group"], title = x["plot_title"]))

#Exchange rates --------------------------------

#import all exchange rates
CAD <- readRDS("data/Salmon public data/Exchange Rates/Cleaned/Canada_ER")
CLP <- readRDS("data/Salmon public data/Exchange Rates/Cleaned/Chile_ER")
DKK <- readRDS("data/Salmon public data/Exchange Rates/Cleaned/Faroe_ER")
NOK <- readRDS("data/Salmon public data/Exchange Rates/Cleaned/Norway_ER")

#set starting point as 0
CAD$Rate <- CAD$Rate - CAD$Rate[1]
CLP$Rate <- CLP$Rate - CLP$Rate[1]
DKK$Rate <- DKK$Rate - DKK$Rate[1]
NOK$Rate <- NOK$Rate - NOK$Rate[1]

#add column to define the currency type
CAD$country <- "Canada"
CLP$country <- "Chile"
DKK$country <- "Faroe Islands"
NOK$country <- "Norway"

#plot
rbind(CAD, CLP, DKK, NOK) %>%
  ggplot(aes(x = Date, y = Rate, color = country)) +
  geom_line(stat = "identity")

#Local currency ------------------------------------------------

#convert to local currency
  #subset based on location
exchange_rate_conversion <- function(data_source, string, exchange_rate){
  data_source %>%
    filter(str_detect(source, string)) %>% #partial string matching
    mutate(local_currency = .$`Average$/lb` * exchange_rate$Rate[match(.$Date, exchange_rate$Date)])  #multiply USD by exchange rate
}

exchange_rate_conversion(wholefish_average, "Northeast|West Coast|Seattle", CAD) %>%
  group_by(Date) %>% View
  summarise(local_currency = mean(local_currency)) %>% View
  mutate(local_currency = .$local_currency - .$local_currency[which.min(.$Date)]) %>% View()

#set time point 1 as a zero value
  #find earliest time point in the dataset
  #subtract the value of the earliest point from the entire row

#plot

#sandbox ---------------------
dummy_means <- deviation(wholefish_average, "16-18 lb")

deviation(wholefish_average, "16-18 lb", title = "test 16-18 lb") %>% View

str(dummy)
unlist(dummy)

str(data.matrix(dummy$dev_mean))


wholefish_average[grep("16-18 lb", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_point(stat = "identity")
rownames(wholefish_average) <- c()
rownames(wholefish_average)

wholefish_average[grep("16-18 lb", wholefish_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_point(stat = "identity")

