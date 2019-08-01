#setup -------------------------------------------------------------
library(stringr)
library(ggplot2)
library(dplyr)
library(readxl)
library(gridExtra)
library(lemon)
library(ggpubr)
library(aTSA)

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
#select monterey and 1985-2017
port_landings$Date <- as.numeric(format(port_landings$Date, "%Y"))
port_landings <- port_landings %>%
  filter(Location == c("Monterey", "Morro_Bay"), Date >= 1985, Date <= 2017)%>%
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
  filter(Region == "Central", Year >= 1985, Year <= 2017) %>%
  group_by(Year, Region) %>%
  summarise(lbs = sum(lbs)) %>%
  mutate("Proportional Change" = NA)
statewide <- regional %>%
  filter(Region == "Statewide", Year >= 1985, Year <= 2017) %>%
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
  filter(Location %in% c("Monterey", "Morro_Bay", "Halfmoon_Bay", "San_Francisco", "Bodega_Bay"), Year >= 1985, Year <= 2017) %>%
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
  filter(Location %in% c("Monterey", "Morro_Bay", "Halfmoon_Bay", "San_Francisco", "Bodega_Bay"), Year >= 1985, Year <= 2017) %>%
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

# Figure 1 CPUE by Port ---------------------------------------------
#CPUE summed across years

#theme that Kyle likes
theme_mine <- function () { 
  theme_classic()+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          #axis.ticks = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
          axis.ticks.length=unit(-0.1, "cm"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5)) }

### define custom boxplot by mean/ sd/ max
#sd.box <- function(d) {
#  return(data.frame(ymin=min(d), ymax=max(d), upper=mean(d)+sd(d), lower=mean(d)-sd(d), middle=mean(d)))
#}

box_smoothed <- function(place, fill_color){
  readRDS("data/Crab Cleaned/Effort by Port") %>%
    mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
    filter(Year >= 1985, Year <= 2017, Location == place, Effort < 40000) %>%
    group_by(Year) %>%
    summarise(CPUE = sum(Effort * 0.0004535924)) %>%
  ggplot(aes(x = Year, y = CPUE)) +
    #geom_boxplot(aes(x = Year, y = CPUE, group = Year),outlier.shape = NA, fill = "#fc8d62") +
    geom_point( color = "black", size = 1) +
    geom_smooth(aes(x = Year, y = CPUE), color = fill_color, fill = fill_color, alpha = 0.5, size = 1)+
    scale_x_continuous(expand = c(0,0), breaks = c(1985,1990, 1995, 2000, 2005, 2010, 2015))+
    theme_mine() 
    #coord_cartesian(ylim = c(0,5))
}

#with otters
p1 <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Year >= 1985, Year <= 2017, Location == "Morro_Bay", Effort < 40000) %>%
  group_by(Year) %>%
  summarise(CPUE = sum(Effort * 0.0004535924)) %>%
  ggplot( aes(x = Year, y = CPUE)) +
  #geom_boxplot(aes(x = Year, y = CPUE, group = Year),outlier.shape = NA, fill = "#fc8d62") +
  geom_point(color = "black", size = 1) +
  geom_smooth(aes(x = Year, y = CPUE), color = "#0aa1ff", fill = "#0aa1ff", alpha = 0.5, size = 1)+
  scale_x_continuous(expand = c(0,0), breaks = c(1985,1990, 1995, 2000, 2005, 2010, 2015))+
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
        #axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),)


p2 <- box_smoothed("Monterey", "#0aa1ff")
p3 <- box_smoothed("Halfmoon_Bay", "#0aa1ff")
#without otters
p4 <- box_smoothed("San_Francisco", "#d4000b")
p5 <- box_smoothed("Bodega_Bay", "#d4000b")
p6 <- box_smoothed("Fort_Bragg", "#d4000b")
p7 <- box_smoothed("Eureka", "#d4000b")
p8 <- box_smoothed("Trinidad", "#d4000b")
#last one needs to be different because it has the ticks
p9 <- box_smoothed("Cresent_City", "#d4000b")

pub2_composite <- ggarrange(p9,p8,p7,p6,p5,p4,p3,p2, p1,
                       ncol=1, nrow = 9,
                       align="v")

grid.arrange(arrangeGrob(pub2_composite,
             ncol = 1,
             left = "metric tons per offload reciept",
             bottom = "year")
)

#Figure 1 r --------------------------------------------------------------------

readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Year >= 1985, Year <= 2017, Effort < 60000) %>% #consider modifying the year range
  group_by(Location, Year) %>%
  summarise(ln_CPUE = log(sum(Effort))) %>% #natural log of CPUE
  mutate(r = ln_CPUE - lag(ln_CPUE, n = 1)) %>% #year to year change in CPUE
  group_by(Location) %>%
  summarise(mu = mean(r, na.rm = TRUE), sigma = sd(r, na.rm = TRUE)) %>% View #calculate mean and sd for each location

#Figure 1 normalized --------------------------------------------------------

#for each port, set 1985 to 0
# normalize it to 1
#save it with port name, year, and effort

#within year, find mean
#set loess curve to that mean
#what if the start point isn't 0 after loess?
#extract values at each year
#subtract those values from each port's value

#but then the limits aren't -1,1
#you would have to re-normalize

#have to normalize the ports outside of the function
fig1_normal <- bind_rows(lapply(c("Trinidad", "Cresent_City", "Eureka", "Fort_Bragg", "Bodega_Bay", "San_Francisco", "Halfmoon_Bay", "Monterey", "Morro_Bay"), function(port_name) readRDS("data/Crab Cleaned/Effort by Port") %>%
    mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
    filter(Year >= 1985, Year <= 2017, Effort < 40000, Location == port_name) %>%
    group_by(Location, Year) %>%
    summarise(CPUE = sum(Effort * 0.0004535924)) %>%
    mutate(normalized_CPUE = (CPUE - CPUE[which(Year == 1985)])) %>% #set 1985 as 0 point
      mutate(normalized_CPUE = normalized_CPUE/max(abs(normalized_CPUE))) #normalized deviations to 1
  ))

#subtract out the statewide average
fig1_normal <- merge(fig1_normal, fig1_normal %>%
        group_by(Year) %>%
        summarise(fishing_success = mean(normalized_CPUE)), by = "Year") %>%
  mutate(normal = normalized_CPUE - fishing_success)

#for all ports
  #set the zero point of the whole plot as the mean for 1985
  #normalize to -1,1
#find mean line from there
#Plot above and below the mean line

#create a new data object that gets rid of the outlier data points

normalized <- function(place, fill_color, axes = FALSE){
  
  #plot the data
  if(axes == TRUE){ #for the bottom plot with axis labels
    fig1_normal %>%
      filter(Location == place) %>%
      ggplot(aes(x = Year, y = normal)) +
      geom_line(color = "black", size = 1) + #plot line
      geom_line(aes(x = 1985:2017, y = 0)) + #flat line @ 0
      #geom_smooth(aes(x = Year, y = normal), color = fill_color, fill = fill_color, alpha = 0.5, size = 1) + #loess curve with specific fill color
      scale_x_continuous(expand = c(0,0), breaks = c(1985,1990, 1995, 2000, 2005, 2010, 2015)) +
      scale_y_continuous(breaks = c(-0.5, 0, 0.50), limits = c(-0.7,0.7)) +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(margin = margin( 0.2, unit = "cm")),
            #axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
            axis.ticks.length=unit(-0.1, "cm"),
            panel.border = element_rect(colour = "black", fill=NA, size=.5),)
  } else {
    fig1_normal %>%
      filter(Location == place) %>%
    ggplot(aes(x = Year, y = normal)) +
    geom_line(color = "black", size = 1) + #plot line
    geom_line(aes(x = 1985:2017, y = 0)) + #flat line @ 0
    #geom_smooth(aes(x = Year, y = normal), color = fill_color, fill = fill_color, alpha = 0.5, size = 1) + #loess curve with specific fill color
    scale_x_continuous(expand = c(0,0), breaks = c(1985,1990, 1995, 2000, 2005, 2010, 2015)) +
    scale_y_continuous(breaks = c(-0.5, 0, 0.50), limits = c(-0.7,0.7)) +
    theme_mine() 
  }
}

n1 <- normalized("Morro_Bay", "#0aa1ff", TRUE)
n2 <- normalized("Monterey", "#0aa1ff")
n3 <- normalized("Halfmoon_Bay", "#0aa1ff")
#without otters
n4 <- normalized("San_Francisco", "#d4000b")
n5 <- normalized("Bodega_Bay", "#d4000b")
n6 <- normalized("Fort_Bragg", "#d4000b")
n7 <- normalized("Eureka", "#d4000b")
n8 <- normalized("Trinidad", "#d4000b")
n9 <- normalized("Cresent_City", "#d4000b")

grid.arrange(arrangeGrob(ggarrange(n9,n8,n7,n6,n5,n4,n3,n2,n1,
                                   ncol=1, nrow = 9,
                                   align="v"),
                         ncol = 1,
                         left = "residual normalized fishing success",
                         bottom = "year")
)


# Pub plot 3: CPUE vs otter pop ---------------------------------------------

#CPUE at Halfmoon Bay, Monterey & Morro Bay vs. Otter population within 100km of the ports
#match the colors with the colors from Pub plot 2

#this plot is done in `Pub plot 3.R`` since the buffering process is a long script

#Pub plot 4: Timeline of mgmt changes ---------------------------------------------

#Pub plot 5: Inherent growth rate ----------------------------------------------
growth_rate <-function(site_name){
  scratch <- readRDS("data/Crab Cleaned/Effort by Port") %>%
    mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  #filter the site
    filter(Location == site_name) %>%
    group_by(Year, Location) %>%
    summarise(CPUE = log(sum(Effort))) %>% #careful about this. Kyle asked for a log transform. see pg 117 of notebook
    mutate(r = NA)
    for(i in 2:nrow(scratch)){
      scratch$r[i] <- (scratch$CPUE[i] - scratch$CPUE[i-1])/scratch$CPUE[i-1] #year-to-year change in pandings
    }
  return(scratch)
}

growth_plot <- function(input, hex){
  ggplot(data = input, aes(x = Year, y = r, group = Year)) +
    geom_point(color = hex) +
    theme_mine() #+
    #coord_cartesian(ylim = c(-6.5,6.5))
}
#intrinsic change in landings for each port
plot_list_absent <- lapply(list("Trinidad", "Cresent_City", "Eureka", "Fort_Bragg", "Bodega_Bay", "San_Francisco"),
                           function(x) growth_rate(site_name = x) %>% growth_plot(., hex = "#fc8d62"))

plot_list_present <- lapply(list("Halfmoon_Bay", "Monterey"),
                            function(x) growth_rate(x) %>% growth_plot(., hex = "#8da0cb"))
#the last plot in the series has to have different axes to display the year ticks at the bottom
plot_list_present[[3]] <- growth_rate("Morro_Bay") %>%
                          ggplot(aes(x = Year, y = r, group = Year)) +
                            geom_point(color = "#8da0cb") +
                            #coord_cartesian(ylim = c(-6.5,6.5)) +
                            theme_classic() +
                            theme(axis.title.y = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
                                  axis.text.x = element_text(margin = margin(c(0.2,1), unit = "cm")),
                                  axis.ticks.length=unit(-0.1, "cm"),
                                  panel.border = element_rect(colour = "black", fill=NA, size=.5))

#paste all plots together
pub5_composite <- ggarrange(plot_list_absent[[1]], plot_list_absent[[2]], plot_list_absent[[3]], plot_list_absent[[4]], plot_list_absent[[5]], plot_list_absent[[6]], plot_list_present[[1]], plot_list_present[[2]], plot_list_present[[3]],
          ncol = 1,
          nrow = 9,
          align = "hv")

grid.arrange(arrangeGrob(pub5_composite, #calling a list object requires explicitly calling `grob =`
                         ncol = 1,
                         top = "Change in CPUE",
                         left= "r",
                         bottom = "Year"))
#r MANOVA ---------------------------------------------------------
#calculate MANOVA for intrinsic growth rate

r_anova <- as.data.frame(do.call(rbind, lapply(c("Trinidad", "Cresent_City", "Eureka", "Fort_Bragg", "Bodega_Bay", "San_Francisco", "Halfmoon_Bay", "Monterey", "Morro_Bay"), function(x) growth_rate(site_name = x))))

summary(aov(r ~ Location, r_anova))

# Figure 2 Proportial CPUE ----------------------------------------------------------

#Sum CPUE by year and port
proportioned_CPUE <- ungroup(readRDS("data/Crab Cleaned/Effort by Port")) %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Location != "Statewide", Effort < 40000) %>%
  group_by(Year, Location) %>%
  summarize(Effort = sum(Effort)) %>%
  mutate(presence = ifelse(Location %in% c("Halfmoon_Bay", "Monterey", "Morro_Bay"), "Otters Present", "Otters Absent"))
  
#divide a single port's CPUE by the total, statewide CPUE
proportioned_CPUE$prop_CPUE <- apply(proportioned_CPUE,1, function(x)  as.numeric(x["Effort"])/sum(proportioned_CPUE$Effort[which(proportioned_CPUE$Year == x["Year"])]))

proportioned_CPUE %>%
  group_by(Year, presence) %>%
  summarise(prop_CPUE = sum(prop_CPUE)) %>%
ggplot(aes(x = Year, y = prop_CPUE, fill = presence)) +
  geom_bar(stat = "identity", position = "stack", width = 1, color = "black") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = c(0.25,0.50,0.75,1)) +
  scale_fill_manual(values = c("Otters Present" = "#0aa1ff","Otters Absent" = "#fc8d62")) +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=.3),
    legend.title=element_blank()) +
  ylab("proportion of CPUE")

#regression line version
proportioned_CPUE %>%
  group_by(Year, presence) %>%
  summarise(prop_CPUE = sum(prop_CPUE)) %>%
  filter(presence == "Otters Present") %>%
  ggplot(aes(x = Year, y = prop_CPUE, fill = presence)) +
  geom_point(stat = "identity") +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = c(0.25,0.50,0.75,1)) +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=.3),
    legend.title=element_blank()) +
  ylab("proportion of fishing success")

proportioned_CPUE %>%
  group_by(Year, presence) %>%
  summarise(prop_CPUE = sum(prop_CPUE)) %>%
  filter(presence == "Otters Present") %>%
  summary(lm(prop_CPUE ~ Year, .))
# sandbox ------------------------------------------------------


fitted_CPUE <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Year >= 1985, Year <= 2017, Effort < 60000) %>%
  group_by(Location, Year) %>%
  summarise(CPUE = sum(Effort * 0.0004535924)) %>%
  group_by(Year) %>%
  summarise(CPUE2 = mean(CPUE)) %>% #find the mean CPUE within a year
  loess(CPUE2 ~ Year, data = .) #fit loess curve. Default smoothing of 75%

fitted_CPUE <- cbind(fitted_CPUE$fitted, c(1985:2017)) #add a column of years to match the data later
colnames(fitted_CPUE) <- c("fitted", "Year")

#plot the data
  port_data <- readRDS("data/Crab Cleaned/Effort by Port") %>%
    mutate(Year = as.numeric(format(.$Date, "%Y"))) %>% #convert dates to Year
    filter(Year >= 1985, Year <= 2017, Effort < 60000, Location == "San_Francisco") %>% #cut out outliers, pick relevant years, keep only the names port location
    group_by(Year) %>%
    summarise(CPUE = sum(Effort * 0.0004535924)) %>% #sum over a year and convert to metric tons
    merge(., fitted_CPUE, by = "Year") %>%
    mutate(normalize_CPUE = (CPUE - fitted)) %>%
    mutate(normalize_CPUE = (normalize_CPUE - normalize_CPUE[which(Year == 1985)])) %>%
    mutate(normalize_CPUE = normalize_CPUE/max(normalize_CPUE))
  