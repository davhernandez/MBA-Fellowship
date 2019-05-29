library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(tmap)
library(stringr)
library(ggplot2)
library(dplyr)
library(readxl)
library(gridExtra)

setwd(dir = "~/Desktop/Grad school/github/MBA Fellowship")

#otter counts -------------------------------------------
#grab all of the individual Range_counts.xls files and make a big dataframe
otter_counts <- data.frame()
for(i in 1985:2014){
  if(i == 2011){
    #skip 2011 b/c the data doesn't exist
  } else {
    otter_counts <- rbind(otter_counts, read_xls(path = paste("data/Otter Data/Spring",i,"_SeaOtterCensus/Range_counts.xls", sep = ""), sheet = 1))
  }
}

#summarize population size by year
otter_counts <- otter_counts %>%
  group_by(Year) %>%
  summarize("Otter Population Size" = sum(Count))

#Crab data ----------------------------------------------------------

#otter data is 1985-2014
#proportional change in crab landings

#import port data
port_landings <- readRDS("data/Crab Cleaned/Landings Weight by Port")
#select monterey and 1984-2017
port_landings$Date <- as.numeric(format(port_landings$Date, "%Y"))
port_landings <- port_landings %>%
  filter(Location == c("Monterey", "Morro_Bay"), Date >= 1984, Date <= 2017)%>%
  group_by(Date) %>%
  summarise(lbs = sum(lbs)) %>%
  mutate("Proportional Change" = NA)
#loop to calculate the amount landed (this year - last year) / last year
for(i in 2:nrow(port_landings)){
  port_landings$`Proportional Change`[i] <- (port_landings$lbs[i] - port_landings$lbs[(i-1)])/port_landings$lbs[(i-1)]
}

#merge by year
proportional <- merge(y = port_landings, x = otter_counts, by.y = "Date", by.x = "Year")

ggplot(proportional, aes(x = `Otter Population Size`, y = `Proportional Change`)) +
  geom_point() +
  theme_classic() +
  xlab("Otter Population") +
  ylab("Proportional Change in Landings") +
  ggtitle("Otter Population vs Yearly Change in Landings")

#add regional data into the mix
regional <- readRDS("data/Crab Cleaned/Regional Data")
central <- regional %>%
  filter(Region == "Central", Year >= 1984, Year <= 2017) %>%
  group_by(Year, Region) %>%
  summarise(lbs = sum(lbs)) %>%
  mutate("Proportional Change" = NA)
statewide <- regional %>%
  filter(Region == "Statewide", Year >= 1984, Year <= 2017) %>%
  group_by(Year, Region) %>%
  summarise(lbs = sum(lbs)) %>%
  mutate("Proportional Change" = NA)
for(i in 2:nrow(central)){
  central$`Proportional Change`[i] <- (central$lbs[i] - central$lbs[(i-1)])/central$lbs[(i-1)]
}
for(i in 2:nrow(statewide)){
  statewide$`Proportional Change`[i] <- (statewide$lbs[i] - statewide$lbs[(i-1)])/statewide$lbs[(i-1)]
}

port_landings$Region <- rep("Monterey", times = nrow(port_landings))
port_landings <- port_landings[,c(1,4,2,3)] #reorder columns to match regional dfs
colnames(port_landings)[1] <- "Year"
overall_proportional <- bind_rows(central, statewide, port_landings) #`bind_rows` instead of `rbind` because of tbls


ggplot(overall_proportional, aes(x = Year, y = `Proportional Change`, color = Region)) +
  geom_line() +
  theme_classic() +
  ylab("Proportional Change in Landings") +
  ggtitle("Regional Change in Yearly Landings")

# Otter population vs crab landings
merge(otter_counts, port_landings) %>%
  mutate(tons = lbs * 0.0004535924) %>%
  ggplot(aes(x = `Otter Population Size`, y = tons)) +
  geom_point() +
  xlab("Otter Population") +
  ylab("Landings (metric tons)") +
  ggtitle("Otter Population vs Monterey Area Landings") +
  theme_classic()

#plot landings in metric ton for Monterey area and SF
readRDS("data/Crab Cleaned/Landings Weight by Port") %>%
  mutate(Date = as.numeric(format(.$Date, "%Y"))) %>%
  #filter(Location %in% c("Monterey", "Morro_Bay", "Halfmoon_Bay", "San_Francisco", "Bodega_Bay")) %>%
  group_by(Location, Date) %>%
  summarise(lbs = sum(lbs)) %>%
  mutate(tons = lbs * 0.0004535924) %>% #convert to metric tons
  ggplot(aes(x = Date, y = tons, color = Location)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(vars(Location), scales = "free", labeller = as_labeller(c("Monterey" = "Monterey",
                                                                       "Morro_Bay" = "Morro Bay",
                                                                       "Halfmoon_Bay" = "Halfmoon Bay",
                                                                       "San_Francisco" = "San Francisco",
                                                                       "Bodega_Bay" = "Bodega Bay",
                                                                       "Fort_Bragg" = "Fort Bragg",
                                                                       "Eureka" = "Eureka",
                                                                       "Trinidad" = "Trinidad",
                                                                       "Cresent_City" = "Cresent City"))) +
  ylab("Landings (metric tons)") +
  xlab("Year") +
  ggtitle("Yearly Dungeness Crab Landings") +
  theme_bw()

#plot landings in metric ton as box plots
readRDS("data/Crab Cleaned/Landings Weight by Port") %>%
  mutate(Date = as.numeric(format(.$Date, "%Y"))) %>%
  filter(lbs > 0) %>%
  mutate(tons = lbs * 0.0004535924) %>% #convert to metric tons
  ggplot(aes(x = Date, y = tons, color = Location, group = Date)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(vars(Location), scales = "free") + #labeller = as_labeller(c("Monterey" = "Monterey",
  #"Morro_Bay" = "Morro Bay",
  #"Halfmoon_Bay" = "Halfmoon Bay",
  #"San_Francisco" = "San Francisco",
  #"Bodega_Bay" = "Bodega Bay"))) +
  ylab("Landings (metric tons)") +
  xlab("Year") +
  ggtitle("Yearly Dungeness Crab Landings") +
  theme_bw()

#effort (lbs per ticket)

readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Location %in% c("Monterey", "Morro_Bay", "Halfmoon_Bay", "San_Francisco", "Bodega_Bay"), Year >= 1984, Year <= 2017) %>%
  group_by(Location, Year) %>%
  summarise(tons = sum(Effort) * 0.0004535924) %>% #metric tons per receipt
  filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = tons, color = Location)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(vars(Location), scales = "free", labeller = as_labeller(c("Monterey" = "Monterey",
                                                                       "Morro_Bay" = "Morro Bay",
                                                                       "Halfmoon_Bay" = "Halfmoon Bay",
                                                                       "San_Francisco" = "San Francisco",
                                                                       "Bodega_Bay" = "Bodega Bay"))) +
  ylab("Landings per Receipt (metric tons)") +
  xlab("Year") +
  ggtitle("Yearly Landings per Receipt (Outliers Trimmed)") +
  theme_bw()

readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Location %in% c("Monterey", "Morro_Bay", "Halfmoon_Bay", "San_Francisco", "Bodega_Bay"), Year >= 1984, Year <= 2017) %>%
  group_by(Location, Year) %>%
  summarise(receipts = sum(receipts)) %>% #metric tons per receipt
  ggplot(aes(x = Year, y = receipts, color = Location)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(vars(Location), scales = "free", labeller = as_labeller(c("Monterey" = "Monterey",
                                                                       "Morro_Bay" = "Morro Bay",
                                                                       "Halfmoon_Bay" = "Halfmoon Bay",
                                                                       "San_Francisco" = "San Francisco",
                                                                       "Bodega_Bay" = "Bodega Bay"))) +
  ylab("Landings Receipts") +
  xlab("Year") +
  ggtitle("Yearly Landings Receipts") +
  theme_bw()

#publication plots ----------------------------------------------------------------

#Pub plot 1: CPUE regression ---------------------------------------------------

#regression for CPUE over time
readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Year >= 1984, Year <= 2017, Location != "Statewide") %>%
  group_by(Location, Year) %>%
  summarise(CPUE = sum(Effort) * 0.0004535924) %>% #metric tons per receipt
  filter(Year != 1997) %>% #remove the outlier year
  ggplot(aes(x = Year, y = CPUE, color = Location)) +
  geom_smooth(se = FALSE) +
  theme_classic() +
  ggtitle("CPUE by port (tons per landing receipt)")
  
  #MAYBE not useful
  #regression of just the southern ports
  #readRDS("data/Crab Cleaned/Effort by Port") %>%
    #mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
    #filter(Location %in% c("Monterey", "Morro_Bay", "Halfmoon_Bay", "San_Francisco", "Bodega_Bay"), Year >= 1984, Year <= 2017) %>%
    #group_by(Location, Year) %>%
    #summarise(CPUE = sum(Effort) * 0.0004535924) %>% #metric tons per receipt
    #filter(Year != 1997) %>% #remove the outlier year
    #ggplot(aes(x = Year, y = CPUE, color = Location)) +
    #geom_smooth(se = FALSE) +
    #theme_classic() +
    #ggtitle("CPUE by port (tons per landing receipt)")

#Pub plot 2: CPUE by port ------------------------------------------------------------------------
#Raw CPUE by port
  #using color palette viridis(9)
    #manually enter the color for each plot
    # "#440154FF" "#472D7BFF" "#3B528BFF" "#2C728EFF" "#21908CFF" "#27AD81FF" "#5DC863FF" "#AADC32FF" "#FDE725FF"

p1 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y")), CPUE = Effort * 0.0004535924) %>%
  filter(Year >= 1984, Year <= 2017, Location == "Cresent_City") %>%
  #filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = CPUE, group = Year)) +
  geom_boxplot(outlier.shape = NA, fill = "#1F968BFF") +
  coord_cartesian(ylim = c(0,3.5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(Location), labeller = as_labeller(c("Cresent_City" = "Cresent City")))

p2 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y")), CPUE = Effort * 0.0004535924) %>%
  filter(Year >= 1984, Year <= 2017, Location == "Trinidad") %>%
  #filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = CPUE, group = Year)) +
  geom_boxplot(outlier.shape = NA, fill = "#20A387FF") +
  coord_cartesian(ylim = c(0,3.5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(Location))

p3 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y")), CPUE = Effort * 0.0004535924) %>%
  filter(Year >= 1984, Year <= 2017, Location == "Eureka") %>%
  #filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = CPUE, group = Year)) +
  geom_boxplot(outlier.shape = NA, fill = "#29AF7FFF") +
  coord_cartesian(ylim = c(0,3.5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(Location))

p4 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y")), CPUE = Effort * 0.0004535924) %>%
  filter(Year >= 1984, Year <= 2017, Location == "Fort_Bragg") %>%
  #filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = CPUE, group = Year)) +
  geom_boxplot(outlier.shape = NA, fill = "#3CBB75FF") +
  coord_cartesian(ylim = c(0,3.5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(Location), labeller = as_labeller(c("Fort_Bragg" = "Fort Bragg")))

p5 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y")), CPUE = Effort * 0.0004535924) %>%
  filter(Year >= 1984, Year <= 2017, Location == "Bodega_Bay") %>%
  #filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = CPUE, group = Year)) +
  geom_boxplot(outlier.shape = NA, fill = "#55C667FF") +
  coord_cartesian(ylim = c(0,3.5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(Location), labeller = as_labeller(c("Bodega_Bay" = "Bodega Bay")))

p6 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y")), CPUE = Effort * 0.0004535924) %>%
  filter(Year >= 1984, Year <= 2017, Location == "San_Francisco") %>%
  #filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = CPUE, group = Year)) +
  geom_boxplot(outlier.shape = NA, fill = "#73D055FF") +
  coord_cartesian(ylim = c(0,3.5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(Location), labeller = as_labeller(c("San_Francisco" = "San Francisco")))

p7 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y")), CPUE = Effort * 0.0004535924) %>%
  filter(Year >= 1984, Year <= 2017, Location == "Halfmoon_Bay") %>%
  #filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = CPUE, group = Year)) +
  geom_boxplot(outlier.shape = NA, fill = "#95D840FF") +
  coord_cartesian(ylim = c(0,3.5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(Location), labeller = as_labeller(c("Halfmoon_Bay" = "Halfmoon Bay")))

p8 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y")), CPUE = Effort * 0.0004535924) %>%
  filter(Year >= 1984, Year <= 2017, Location == "Monterey") %>%
  #filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = CPUE, group = Year)) +
  geom_boxplot(outlier.shape = NA, fill = "#B8DE29FF") +
  coord_cartesian(ylim = c(0,3.5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(Location))

p9 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y")), CPUE = Effort * 0.0004535924) %>%
  filter(Year >= 1984, Year <= 2017, Location == "Morro_Bay") %>%
  #filter(Year != 1997) %>%
  ggplot(aes(x = Year, y = CPUE, group = Year)) +
  geom_boxplot(outlier.shape = NA, fill = "#DCE319FF") +
  coord_cartesian(ylim = c(0,3.5)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  facet_grid(vars(Location), labeller = as_labeller(c("Morro_Bay" = "Morro Bay")))

#combine all the plots into one massive plot
grid.arrange(arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,p9,
             ncol = 1,
             left = "Metric Tons per Landing Receipt",
             bottom = "Year",
             top = "CPUE")
)





# Pub plot 3: CPUE vs otter pop ---------------------------------------------

#CPUE at Halfmoon Bay, Monterey & Morro Bay vs. Otter population within 100km of the ports
#match the colors with the colors from Pub plot 2

#this plot is done in `Pub plot 3.R`` since the buffering process is a long script

#Pub plot 4: Timeline of mgmt changes ---------------------------------------------
