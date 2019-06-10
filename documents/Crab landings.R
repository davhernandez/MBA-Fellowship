#setup -------------------------------------------
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(lubridate)

setwd("~/Desktop/Grad School/github/MBA Fellowship")

#clean landings receipts -----------------------------------------

#landing receipts
landings_receipts_raw <- read_excel("data/CDFW_MonthlyCrabLandingReceipts&Pounds_11-1980 to 07-2018.xlsx", sheet = 1)

#add column names
colnames(landings_receipts_raw) <- c("Year", "Month", "Cresent_City", "Trinidad", "Eureka", "Fort_Bragg", "Bodega_Bay", "San_Francisco", "Halfmoon_Bay","Monterey", "Morro_Bay")
#remove the first two rows
landings_receipts_raw <- landings_receipts_raw[-c(1:2),]
#remove the NAs
landings_receipts_raw <- landings_receipts_raw[which(!is.na(landings_receipts_raw$Cresent_City)),]
#adding a statewide sum
  landings_receipts_raw[,3:11] <- as.numeric(unlist(landings_receipts_raw[,3:11])) #redfeine class as numeric
  landings_receipts_raw <- mutate(landings_receipts_raw, Statewide = rowSums(landings_receipts_raw[,3:11])) #sum rows
#make a date row
landings_receipts_raw <- landings_receipts_raw %>%
                         mutate(Date = as.Date(paste(landings_receipts_raw$Year, landings_receipts_raw$Month, "1", sep = ""), format = "%Y%b%d"))

#melt to tidydata format
landings_receipts_raw <- melt(landings_receipts_raw,
              id.vars = "Date",
              measure.vars = c("Cresent_City", "Trinidad", "Eureka", "Fort_Bragg", "Bodega_Bay", "San_Francisco", "Halfmoon_Bay","Monterey", "Morro_Bay", "Statewide"),
              value.name = "receipts",
              variable.name = "Location")

#cleaning landed weight ----------------------------------------

#landed weight by month and year
pounds_landed_raw <- read_excel("data/CDFW_MonthlyCrabLandingReceipts&Pounds_11-1980 to 07-2018.xlsx", sheet = 2)

#add column names
colnames(pounds_landed_raw) <- c("Year", "Month", "Cresent_City", "Trinidad", "Eureka", "Fort_Bragg", "Bodega_Bay", "San_Francisco", "Halfmoon_Bay","Monterey", "Morro_Bay")
#remove the first two rows
pounds_landed_raw <- pounds_landed_raw[-c(1:2),]
#remove the NAs
pounds_landed_raw <- pounds_landed_raw[which(!is.na(pounds_landed_raw$Cresent_City)),]
#adding a statewide sum
  pounds_landed_raw[,3:11] <- as.numeric(unlist(pounds_landed_raw[,3:11])) #make class numeric
  pounds_landed_raw <- mutate(pounds_landed_raw, Statewide = rowSums(pounds_landed_raw[,3:11])) #sum rows
#make a date row
pounds_landed_raw <- pounds_landed_raw %>%
  mutate(Date = as.Date(paste(pounds_landed_raw$Year, pounds_landed_raw$Month, "1", sep = ""), format = "%Y%b%d"))
#melt to tidyverse
pounds_landed_raw <- melt(pounds_landed_raw, id.vars = "Date",
                                             measure.vars = c("Cresent_City", "Trinidad", "Eureka", "Fort_Bragg", "Bodega_Bay", "San_Francisco", "Halfmoon_Bay","Monterey", "Morro_Bay", "Statewide"),
                                             value.name = "lbs",
                                             variable.name = "Location")

# clean yearly summary data ------------------------------------------

#landings receipts and weight per year by region
yearly_summaries_raw <- read_excel("data/CDFW_MonthlyCrabLandingReceipts&Pounds_11-1980 to 07-2018.xlsx", sheet = 3)

yearly_summaries_raw[,5] <- NULL #remove empty column
colnames(yearly_summaries_raw) <- c("Year", "Northern", "Central", "Statewide", "Year", "Northern", "Central", "Statewide") #rename columns

yearly_summaries_raw <- yearly_summaries_raw[-1,] #delete first row

subset_r <- melt(yearly_summaries_raw[1:4],id.vars = c("Year"), #subset the receipt data
                                            measure.vars = c("Northern", "Central", "Statewide"),
                                            variable.name = "Region",
                                            value.name = "receipts")

subset_lbs <- melt(yearly_summaries_raw[5:8],id.vars = c("Year"), #subset the lbs data
                   measure.vars = c("Northern", "Central", "Statewide"),
                   variable.name = "Region",
                    value.name = "lbs")
yearly_summaries_raw <- merge(subset_r, subset_lbs, by = c("Year", "Region")) #merge subsets to get rows of unique year and region combinations that have receipts and landings in the same row
rm(list = "subset_r", "subset_lbs")

#correcting classes
yearly_summaries_raw$receipts <- as.numeric(yearly_summaries_raw$receipts) #make values numeric
yearly_summaries_raw$lbs <- as.numeric(yearly_summaries_raw$lbs) #make values numeric
yearly_summaries_raw$Year <- as.numeric(substr(yearly_summaries_raw$Year, start = 1, stop = 4)) #grab season start year

#proportion of from each region
  #figure out what proportion of the data is coming from each region for a year
yearly_summaries_raw$receipt_proportion <- NA
yearly_summaries_raw$lbs_proportion <- NA
for(i in 1:nrow(yearly_summaries_raw)){
    #grabs the value for the receipts, then finds the value for that year from the `Statewide` region
    yearly_summaries_raw$receipt_proportion[i] <- yearly_summaries_raw$receipts[i] / yearly_summaries_raw$receipts[which(yearly_summaries_raw$Year == as.character(yearly_summaries_raw$Year[i]) & yearly_summaries_raw$Region == "Statewide")]
    #does the same for lbs
    yearly_summaries_raw$lbs_proportion[i] <- yearly_summaries_raw$lbs[i] / yearly_summaries_raw$lbs[which(yearly_summaries_raw$Year == as.character(yearly_summaries_raw$Year[i]) & yearly_summaries_raw$Region == "Statewide")]
}

# effort calculations ------------------------------------

#merge landings weight and landings reciepts
fishing_effort <- merge(pounds_landed_raw, landings_receipts_raw, by = c("Date", "Location"))
#calculate effort
for(i in 1:nrow(fishing_effort)){
  if(fishing_effort$lbs[i] == 0 & fishing_effort$receipts[i] ==0){
    fishing_effort$Effort[i] <- 0 #if no fishing occured, fill in with `0`
  }  else if(fishing_effort$lbs[i] > 0 & fishing_effort$receipts[i] == 0) { #if landings, but no receipts just `NA``
    fishing_effort$Effort[i] <- NA
    } else {
    fishing_effort$Effort[i] <- fishing_effort$lbs[i] / fishing_effort$receipts[i]
  }
}

fishing_effort <- na.omit(fishing_effort) #remove NA entries

#plot wd ----------------------------------------------
#set wd so that you can save the plots to douments folder
#setwd("~/Desktop/Grad School/github/MBA Fellowship/plots/Dungeness")

# regional plots ---------------------------------------

#seasonal landing lbs by region
yearly_summaries_raw %>%
  mutate(lbs = lbs/1000000) %>%
  {
    ggplot(. , aes(x = Year, y = lbs, colour = Region)) +
      geom_point() +
      geom_line(aes(group = Region)) +
      ylab("Season landings (millions of lbs)") +
      ggtitle("Seasonal Landings by Region") +
      theme_classic()
  }
#ggsave("Season Landings by Region.pdf")

#proportion of yearly landings contributed by each region
yearly_summaries_raw %>%
  filter(Region %in% c("Northern", "Central")) %>%
  {
    ggplot(. , aes(x = Year, y = lbs_proportion, fill = Region)) +
      geom_bar(stat = "identity", position = "fill") +
      theme_classic() +
      ggtitle("Proportion of Season Landings from Each Region")
  }
#ggsave("Proportion of Season Landings by Region.pdf")

#Seasonal receipts by region
yearly_summaries_raw %>%
    ggplot(aes(x = Year, y = receipts, colour = Region)) +
      geom_point() +
      geom_line(aes(group = Region)) +
      ylab("Season Landings Receipts") +
      ggtitle("Season Landing Receipts by Region") +
      theme_classic()

#ggsave("Season Landing Receipts by Region.pdf")

#Proportion of receipts by region
yearly_summaries_raw %>%
  filter(Region %in% c("Northern", "Central")) %>%
  {
    ggplot(. , aes(x = Year, y = receipt_proportion, fill = Region)) +
      geom_bar(stat = "identity", position = "fill") +
      ylab("Proportion of Statewide Landings Receipts") +
      theme_classic() +
      ggtitle("Proportion of Season Landing Receipts by Region")
  }

#ggsave("Proportion of Season Landing Receipts by Region.pdf")

#landings receipts plots ---------------------------------------------------------

#landings reciepts by location
ggplot(landings_receipts_raw, aes(x = Date, y = receipts, colour = Location)) +
  geom_point() +
  geom_line()

#landings reciepts per location per year
landings_receipts_raw %>%
  mutate(Year = as.numeric(format(landings_receipts_raw$Date, "%Y"))) %>%
  group_by(Location, Year) %>%
  summarise(receipts = sum(receipts)) %>%
  {
    ggplot(. , aes(x = Year, y = receipts, colour = Location)) +
      geom_point() +
      geom_line() +
      ggtitle("Landing Receipts per Port") +
      theme_classic()
  }

#ggsave("Landings Receipts by Port & Statewide.pdf")

#landings reciepts per location per year smoothed
landings_receipts_raw %>%
  mutate(Year = as.numeric(format(landings_receipts_raw$Date, "%Y"))) %>%
  group_by(Location, Year) %>%
  summarise(receipts = sum(receipts)) %>%
  {
    ggplot(. , aes(x = Year, y = receipts, colour = Location)) +
      geom_point() +
      geom_smooth() +
      theme_classic() +
      ggtitle("Landings Receipts per Year by Port")
  }

#ggsave("Landings Receipts by Port & Statewide Smoothed.pdf")

#landings reciepts per location per year smoothed w/o statewide
landings_receipts_raw %>%
  mutate(Year = as.numeric(format(landings_receipts_raw$Date, "%Y"))) %>%
  filter(Location != "Statewide") %>%
  group_by(Location, Year) %>%
  summarise(receipts = sum(receipts)) %>%
  {
    ggplot(. , aes(x = Year, y = receipts, colour = Location)) +
      geom_point() +
      geom_smooth() +
      theme_classic() +
      ggtitle("Landings Receipts per Year by Port")
  }

#ggsave("Landings Receipts by Port Smoothed.pdf")


#landings reciepts per month by location w/ Statewide
landings_receipts_raw %>%
  mutate(Month = month(Date)) %>%
  group_by(Location, Month) %>%
  summarise(receipts = sum(receipts)) %>%
  {
    ggplot(. , aes(x = Month, y = receipts, fill = Location)) +
      geom_bar(stat= "identity") +
      scale_x_discrete(limits = month.abb) +
      ggtitle("Landings Receipts by Month") +
      theme_classic()
  }

#ggsave("Landing Receipts by Month.pdf")

#landings reciepts per month by location facet grid
landings_receipts_raw %>%
  mutate(Month = month(Date)) %>%
  #filter(Location != "Statewide") %>%
  group_by(Location, Month) %>%
  summarise(receipts = sum(receipts)) %>%
  {
    ggplot(. , aes(x = Month, y = receipts, fill = Location)) +
      geom_bar(stat= "identity") +
      scale_x_discrete(limits = month.abb) +
      ggtitle("Landings Receipts by Month (floating y-axis)") +
      theme_classic() +
      facet_grid(vars(Location), scales = "free")
  }

#ggsave("Landings Receipts by Month Gridded floating axis.pdf")

#landings reciepts per month by location facet grid with same y-axis
landings_receipts_raw %>%
  mutate(Month = month(Date)) %>%
  filter(Location != "Statewide") %>%
  group_by(Location, Month) %>%
  summarise(receipts = sum(receipts)) %>%
  {
    ggplot(. , aes(x = Month, y = receipts, fill = Location)) +
      geom_bar(stat= "identity") +
      scale_x_discrete(limits = month.abb) +
      ggtitle("Landings Receipts by Month (locked y-axis)") +
      theme_classic() +
      facet_grid(vars(Location))
  }

#ggsave("Landings Receipts by Month Gridded locked axis.pdf")

landings_receipts_raw %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  group_by(Year) %>%
  summarise(Receipts = sum(receipts)) %>%
  ggplot(aes(x = Year, y = Receipts)) +
  geom_point(stat = "identity") +
  geom_line() +
  ggtitle("Statewide Landings Receipts") +
  theme_classic()


#landing weight plots ---------------------------------------------------------

#weight statewide
pounds_landed_raw %>%
  filter(Location == "Statewide") %>%
{
ggplot(., aes(x = Date, y = lbs)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  ggtitle("Landing Weight per Month Statewide")
  }

#gsave("Landed lbs per Month Statewide.pdf")

#weight by location
pounds_landed_raw %>%
  filter(Location != "Statewide") %>%
  {
    ggplot(., aes(x = Date, y = lbs, colour = Location)) +
      geom_point() +
      geom_line() +
      theme_classic() +
      ggtitle("Landing Weight per Month by Location")
  }

#ggsave("Landed lbs per Month by Port.pdf")

#weight per year statewide
pounds_landed_raw %>%
  mutate(Year = as.numeric(format(pounds_landed_raw$Date, "%Y"))) %>%
  filter(Location == "Statewide") %>%
  group_by(Year) %>%
  summarise(lbs = sum(lbs)) %>%
  {
    ggplot(. , aes(x = Year, y = lbs)) +
      geom_point() +
      geom_line() +
      ggtitle("Yearly Landings Statewide") +
      theme_classic()
  }

#ggsave("Yearly Landings Statewide.pdf")

#Yearly landings statewide smoothed
pounds_landed_raw %>%
  mutate(Year = as.numeric(format(pounds_landed_raw$Date, "%Y"))) %>%
  filter(Location == "Statewide") %>%
  group_by(Year) %>%
  summarise(lbs = sum(lbs)) %>%
  {
    ggplot(. , aes(x = Year, y = lbs)) +
      geom_point() +
      geom_smooth() +
      ggtitle("Yearly Landings Statewide") +
      theme_classic()
  }

#Yearly landings by location
pounds_landed_raw %>%
  mutate(Year = as.numeric(format(pounds_landed_raw$Date, "%Y"))) %>%
  filter(Location != "Statewide") %>%
  group_by(Location, Year) %>%
  summarise(lbs = sum(lbs)) %>%
  {
    ggplot(. , aes(x = Year, y = lbs, colour = Location)) +
      geom_point() +
      geom_line() +
      ggtitle("Yearly Landings by Port") +
      theme_classic()
  }

#ggsave("Yearly Landings by Port.pdf")

#Yearly landings by location smoothed
pounds_landed_raw %>%
  mutate(Year = as.numeric(format(pounds_landed_raw$Date, "%Y"))) %>%
  filter(Location != "Statewide") %>%
  group_by(Location, Year) %>%
  summarise(lbs = sum(lbs)) %>%
  {
    ggplot(. , aes(x = Year, y = lbs, colour = Location)) +
      geom_point() +
      geom_smooth(se = FALSE) + #SE overlaps a ton
      ggtitle("Yearly Landings by Port") +
      theme_classic()
  }

#ggsave("Yearly Landings by Port Smoothed.pdf")

#landings per month Statewide
pounds_landed_raw %>%
  mutate(Month = month(Date)) %>%
  group_by(Location, Month) %>%
  filter(Location == "Statewide") %>%
  summarise(lbs = sum(lbs)) %>%
  {
    ggplot(. , aes(x = Month, y = lbs)) +
      geom_bar(stat= "identity") +
      scale_x_discrete(limits = month.abb) +
      ggtitle("Landings per Month Statewide") +
      theme_classic()
  }

#ggsave("Landed lbs per Month Statewide.pdf")

#landings per month by location facet grid
landings_receipts_raw %>%
  mutate(Month = month(Date)) %>%
  filter(Location != "Statewide") %>%
  group_by(Location, Month) %>%
  summarise(receipts = sum(receipts)) %>%
  {
    ggplot(. , aes(x = Month, y = receipts, fill = Location)) +
      geom_bar(stat= "identity") +
      scale_x_discrete(limits = month.abb) +
      ggtitle("Landings per Month by Port (floating y-axis)") +
      theme_classic() +
      facet_grid(vars(Location), scales = "free")
  }

#ggsave("Landed lbs per Month by Port floating axis.pdf")

#landings per month by location facet grid
landings_receipts_raw %>%
  mutate(Month = month(Date)) %>%
  filter(Location != "Statewide") %>%
  group_by(Location, Month) %>%
  summarise(receipts = sum(receipts)) %>%
  {
    ggplot(. , aes(x = Month, y = receipts, fill = Location)) +
      geom_bar(stat= "identity") +
      scale_x_discrete(limits = month.abb) +
      ggtitle("Landings per Month by Port (locked y-axis)") +
      theme_classic() +
      facet_grid(vars(Location))
  }

#ggsave("Landed lbs per Month by Port locked axis.pdf")

#per unit effort plots ---------------------------------------------------------

#landings per unit effort statewide
fishing_effort %>%
  filter(Location == "Statewide") %>%
  ggplot(aes(x = Date, y = Effort)) +
  geom_point() +
  geom_line() +
  ggtitle("Landings per Receipt Statewide") +
  theme_classic()

#landings per unit effort statewide trimmed
fishing_effort %>%
  filter(Location == "Statewide", Effort <= 100000) %>%
  ggplot(aes(x = Date, y = Effort)) +
  geom_point() +
  geom_line() +
  ggtitle("Landings per Receipt Statewide (Outliers Trimmed)") +
  theme_classic()

#ggsave("Effort landings Statewide.pdf")

# landings per receipt by location
fishing_effort %>%
  filter(Location != "Statewide") %>%
  group_by(Location, Date) %>%
  summarise(Effort = sum(Effort)) %>%
  ggplot(aes(x = Date, y = Effort, color = Location)) +
  geom_point() +
  geom_line() +
  ggtitle("Landings per Receipt by Port") +
  theme_classic() +
  facet_wrap(vars(Location), scale = "free")

# landings per receipt by location (outliers trimmed)
fishing_effort %>%
  filter(Location != "Statewide", Effort < 100000) %>%
  group_by(Location, Date) %>%
  summarise(Effort = sum(Effort)) %>%
  ggplot(aes(x = Date, y = Effort, color = Location)) +
  geom_point() +
  geom_line() +
  ggtitle("Landings per Receipt by Port (Outliers Trimmed)") +
  theme_classic() +
  facet_wrap(vars(Location), scale = "free")

#ggsave("Effort Landings by Port.pdf")

# landings per receipt by month
fishing_effort %>%
  filter(Location == "Statewide") %>%
  mutate(Month = month(Date)) %>%
  group_by(Month) %>%
  summarise(Effort = sum(Effort)) %>%
  ggplot(aes(x = Month, y = Effort)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = month.abb) +
  ggtitle("Landings per Receipt Statewide by Month") +
  theme_classic()

#ggsave("Effort Landings by Month Satewide.pdf")

# landings per receipt per month by port
fishing_effort %>%
  filter(Location != "Statewide") %>%
  mutate(Month = month(Date)) %>%
  group_by(Month, Location) %>%
  summarise(Effort = sum(Effort)) %>%
  ggplot(aes(x = Month, y = Effort, fill = Location)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = month.abb) +
  ggtitle("Landings per Receipt Statewide by Month & Port") +
  facet_grid(vars(Location), scale = "free") +
  theme_classic()

#ggsave("Effort Landings by Month per Port.pdf")

#save dfs as R objects -----------------------------------------

#saveRDS(fishing_effort, "data/Crab Cleaned/Effort by Port")
#saveRDS(landings_receipts_raw, "data/Crab Cleaned/Receipts by Port")
#saveRDS(pounds_landed_raw, "data/Crab Cleaned/Landings Weight by Port")
#saveRDS(yearly_summaries_raw, "data/Crab Cleaned/Regional Data")
