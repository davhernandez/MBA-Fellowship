#code based on `Buffer round Moss Landings.R`
  #trimmed the fat just to get the plots
  #for more detail and explanation about the code, go to the original document

library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(tmap)
library(stringr)
library(ggplot2)
library(dplyr)
library(readxl)

setwd(dir = "~/Desktop/Grad school/github/MBA Fellowship")

#Sea otter count data/shapefiles --------------------------------------------------

#raw data was imported, cleaned, and saved in `Otter Census data cleaning.R`
#loading the cleaned data file
SeaOtter_df <- st_read(dsn = "data/Otter Data/Cleaned Data", layer = "Otter_census_cleaned")

#assigning EPSG code because st_write didn't save that
st_crs(SeaOtter_df) <- 3310

#Otters in each ATOS_ID (raw counts) -----------------------------------------------------------
#import range counts.xls speadsheet for all years
#grab all of the individual Range_counts.xls files and make a big dataframe
otter_individuals <- data.frame()
for(i in 1985:2014){
  if(i == 2011){
    #skip 2011 b/c the data doesn't exist
  } else {
    otter_individuals <- rbind(otter_individuals, read_xls(path = paste("data/Otter Data/Spring",i,"_SeaOtterCensus/Range_counts.xls", sep = ""), sheet = 1))
  }
}

#change ATOS column to ATOS_ID
colnames(otter_individuals)[2] <- "ATOS_ID"


#buffer around fishing ports --------------------------------------------------------------------
#determine what level consitiutes no sea otters/backgroun level of sea otters
#talk to Teri and ask which ports we consider to have sea otter populations

#buffering function
port_buffering <- function(ports, ocean, distance){
  port_geometry <- st_geometry(ports) #grabs the geometry of the port points
  port_buffer <- st_buffer(port_geometry, dist = distance)#set a buffer around the port that of how far they might go
  
  within_buffer <- st_contains(port_buffer, ocean) #grab all the rows that are contained within the buffer
  subset_list <- list() #empty list to fill
  #loops over the list entries from 'within_buffer', subsetting the data based on row number.
  #Each entry in the resulting list corresponds to the areas within a port's buffer
  for(listentry in 1:length(within_buffer)){
    subset_list[[listentry]] <- ocean[unlist(within_buffer[[listentry]]),]
  }
  return(subset_list)
}


#100km around Monterey  ----------------------------------------------------------------------------------
#import or manually enter the gps coordinates for all of the fishing ports
monterey_bay_GPS <- data.frame(rbind(c(-121.79, 36.81))) #using Elkhorn Slough/Moss Landings as the actual GPS point because they are likely rolled into the same area. Moss Landings is more of a commerical fishing port anyways
colnames(monterey_bay_GPS) <- c("lon", "lat") #name the columns of the df so that 'st_as_sf' can use column names
monterey_bay_st <- st_as_sf(monterey_bay_GPS, coords = c("lon", "lat")) #make the df into a point object with multiple entries

#morro_bay_st <- st_geometry(morro_bay_st) #assigns a geometry to the point df
st_crs(monterey_bay_st) <- 4326 #EPSG for WSG84, used by Google Earth
monterey_bay_st <- st_transform(monterey_bay_st, 3310) #assign crs to

buffered_monterey <- do.call(rbind.data.frame,
                                              port_buffering(monterey_bay_st, SeaOtter_df,
                                                             distance = 100 * 1000))

buffered_monterey <- otter_individuals[which(otter_individuals$ATOS_ID %in% unique(buffered_monterey$ATOS_ID)),] %>%
  group_by(Year) %>%
  summarise(Otters = sum(Count))

readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Year <= 2014, Year >= 1985, Location == "Monterey") %>%
  group_by(Year) %>%
  summarise(CPUE = mean(Effort) * 0.0004535924) %>%
  merge(.,buffered_monterey, by = "Year") %>%
  ggplot(aes(x = Otters, y = CPUE)) +
  geom_point(color = "#B8DE29FF") +
  ggtitle("Otters within 100km of Elkhorn Slough") +
  ylab("Metric Tons per Landing Receipt") +
  theme_classic()

#in case Kyle asks for a boxplot version
#readRDS("data/Crab Cleaned/Effort by Port") %>%
  #mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  #filter(Year <= 2014, Year >= 1985, Location == "Monterey") %>%
  #mutate(CPUE = Effort * 0.0004535924) %>%
  #merge(.,buffered_monterey, by = "Year") %>%
  #ggplot(aes(x = Otters, y = CPUE, group = Year)) +
  #geom_boxplot(color = "#B8DE29FF") +
  #ggtitle("Otters within 100km of Elkhorn Slough") +
  #ylab("Metric Tons per Landing Receipt") +
  #theme_bw()

#100km around Morro Bay -------------------------------------------------

morro_bay_GPS <- data.frame(rbind(c(-120.87, 35.40))) #Morro Bay GPS
colnames(morro_bay_GPS) <- c("lon", "lat") #name the columns of the df so that 'st_as_sf' can use column names
morro_bay_st <- st_as_sf(morro_bay_GPS, coords = c("lon", "lat")) #make the df into a point object with multiple entries

st_crs(morro_bay_st) <- 4326 #EPSG for WSG84, used by Google Earth
morro_bay_st <- st_transform(morro_bay_st, 3310) #assign crs to 

buffered_morro <- do.call(rbind.data.frame,
                          port_buffering(morro_bay_st, SeaOtter_df,
                                         distance = (100 * 1000)))

buffered_morro <-otter_individuals[which(otter_individuals$ATOS_ID %in% unique(buffered_morro$ATOS_ID)),] %>%
  group_by(Year) %>%
  summarise(Otters = sum(Count))

readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Year <= 2014, Year >= 1985, Location == "Morro_Bay") %>%
  group_by(Year) %>%
  summarise(CPUE = mean(Effort) * 0.0004535924) %>%
  merge(.,buffered_morro, by = "Year") %>%
  ggplot(aes(x = Otters, y = CPUE)) +
  geom_point(color = "#DCE319FF") +
  ggtitle("Otters within 100km of Morro Bay") +
  ylab("Metric Tons per Landing Receipt") +
  theme_classic()

#100km around Halfmoon Bay -----------------------------------------------------

halfmoon_bay_GPS <- data.frame(rbind(c(-122.47, 37.49))) #Morro Bay GPS
colnames(halfmoon_bay_GPS) <- c("lon", "lat") #name the columns of the df so that 'st_as_sf' can use column names
halfmoon_bay_st <- st_as_sf(halfmoon_bay_GPS, coords = c("lon", "lat")) #make the df into a point object with multiple entries

st_crs(halfmoon_bay_st) <- 4326 #EPSG for WSG84, used by Google Earth
halfmoon_bay_st <- st_transform(halfmoon_bay_st, 3310) #assign crs to 

buffered_halfmoon <- do.call(rbind.data.frame,
                             port_buffering(halfmoon_bay_st, SeaOtter_df,
                                            distance = (100 * 1000)))

buffered_halfmoon <-otter_individuals[which(otter_individuals$ATOS_ID %in% unique(buffered_halfmoon$ATOS_ID)),] %>%
  group_by(Year) %>%
  summarise(Otters = sum(Count))

readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  filter(Year <= 2014, Year >= 1985, Location == "Halfmoon_Bay") %>%
  group_by(Year) %>%
  summarise(CPUE = mean(Effort) * 0.0004535924) %>%
  merge(.,buffered_halfmoon, by = "Year") %>%
  ggplot(aes(x = Otters, y = CPUE)) +
  geom_point(color = "#95D840FF") +
  ggtitle("Otters within 100km of Elkhorn Slough") +
  ylab("Metric Tons per Landing Receipt") +
  theme_classic()
