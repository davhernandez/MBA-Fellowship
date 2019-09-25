#UB Salmon Fillets exploratory plots
#plottting the UB salmon data for high, low, & average

#setup ---------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(lubridate)

#import data ---------------------------
setwd("~/Desktop/Grad School/github/MBA Fellowship")

fillet_average <- read.csv("data/UB Salmon data/Comtell Salmon Fillets average.csv")
fillet_all <- read.csv("data/UB Salmon data/Comtell Salmon Fillets high low average.csv")

fillet_average <- fillet_average[-1,] #remove extra row with data types
fillet_average <- fillet_average[-(976:978),] #remove summary rows from the bottom

colnames(fillet_average)[1] <- "Date" #rename first column
fillet_average$Date <- as.Date(fillet_average$Date, format = "%m/%d/%Y") #reformat the date column to be a date

colnames(fillet_average)[2:16] <- c("Chile, Fresh, 2-3 lb", "Chile, Fresh, 3-4 lb", "Chile, Fresh, 4-5 lb", #rename columns
                                    "NE Europe, 2-3 lb", "NE Europe, 3-4 lb",
                                    "NE Northeast, 2-3 lb", "NE Northeast, 3-4 lb", "NE Northeast, 4-5 lb",
                                    "LA West Coast, 2-3 lb", "LA West Coast, 3-4 lb", "LA West Coast, 4-5 lb",
                                    "Chile, Frozen, 1-2 lb", "Chile, Frozen, 2-3 lb", "Chile, Frozen, 3-4 lb", "Chile, Frozen, 4-5 lb")
fillet_average[,2:16] <- apply(fillet_average[,2:16], 2, function(x) as.numeric(as.character(x))) #convert factors to numeric

#change data to long form
fillet_average <- melt(fillet_average, id.vars = "Date",
                       measure.vars = 2:16,
                       value.name = "Average$/lb",
                       variable.name = "source",
                       na.rm = FALSE) #leaving NAs preserves the gaps when plotting

#all data, single plot --------------------------------------------------------
ggplot(fillet_average, aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average price of fillets")

#overview of all fillet sources ---------------------------------------------------------------
#breaks all the data out into different countries, then makes a composite graph

comp1 <- fillet_average[grep("Chile", fillet_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average Chile fillets") +
  ylim(c(0.5,8.5)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y")

comp2 <- fillet_average[grep("Europe", fillet_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average NE Europe fillets") +
  ylim(c(0.5,8.5)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y")

comp3 <- fillet_average[grep("Northeast", fillet_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average NE Northeast fillets") +
  ylim(c(0.5,8.5)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y")

comp4 <- fillet_average[grep("West Coast", fillet_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average LA West Coast fillets") +
  ylim(c(0.5,8.5)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y")

grid.arrange(comp1, comp2, comp3, comp4) #build composite plot

rm(list = ls(pattern = "comp")) #remove standalone plots from environment

#comparing similar size fish ---------------------------------------------
#grouping data based on the size bin of the fish

#1-2lb
lb12 <- fillet_average[grep("1-2 lb", fillet_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 1-2 lb fillets") +
  ylim(c(0.5,8.5)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y")

lb23 <- fillet_average[grep("2-3 lb", fillet_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 2-3 lb fillets") +
  ylim(c(0.5,8.5)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y")

lb34 <- fillet_average[grep("3-4 lb", fillet_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 3-4 lb fillets") +
  ylim(c(0.5,8.5)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y")

lb45 <- fillet_average[grep("4-5 lb", fillet_average$source),] %>%
  ggplot(aes(x = Date, y = `Average$/lb`, color = source)) +
  geom_line(stat = "identity") +
  ggtitle("Weekly average 4-5 lb fillets") +
  ylim(c(0.5,8.5)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y")

grid.arrange(lb12, lb23, lb34, lb45)

rm(lb12, lb23, lb34, lb45)

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

app_fillet <- matrix(c("1-2 lb", "2-3 lb", "3-4 lb", "4-5 lb",
                       "Fillet 1-2 lb", "Fillet 2-3 lb", "Fillet 3-4 lb", "Fillet 4-5 lb"),
                     nrow = 4,
                     ncol = 2,
                     dimnames = list(c(),
                                     c("group", "plot_title"))
                     )
apply(app_fillet, 1, function(x) deviation(data_source = fillet_average, grouping = x["group"], title = x["plot_title"]))

#Exchange rates --------------------------------------------


#sandbox --------------------------------------

#adding a column for isoweek and isoyear. These were added to the exchange rate dfs. they will be used to match up the rows when combining things
fillet_average %>%
  mutate(Week = isoweek(Date)) %>%
  mutate(Year = isoyear(Date))