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
#raw data was imported, cleaned, and saved in `Otter Census data cleaning.R`
#loading the cleaned data file
SeaOtter_df <- st_read(dsn = "data/Otter Data/Cleaned Data", layer = "Otter_census_cleaned")

#assigning EPSG code because st_write didn't save that
st_crs(SeaOtter_df) <- 3310

#Port GPS points ----------------------------------------------------------------------------------
#import or manually enter the gps coordinates for all of the fishing ports
monterey_bay_GPS <- data.frame(rbind(c(-121.79, 36.81))) #using Elkhorn Slough/Moss Landings as the actual GPS point because they are likely rolled into the same area. Moss Landings is more of a commerical fishing port anyways
colnames(monterey_bay_GPS) <- c("lon", "lat") #name the columns of the df so that 'st_as_sf' can use column names
monterey_bay_st <- st_as_sf(monterey_bay_GPS, coords = c("lon", "lat")) #make the df into a point object with multiple entries

#morro_bay_st <- st_geometry(morro_bay_st) #assigns a geometry to the point df
st_crs(monterey_bay_st) <- 4326 #EPSG for WSG84, used by Google Earth
monterey_bay_st <- st_transform(monterey_bay_st, 3310) #assign crs to 


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

#!!!!!!!!!!!!!!how far away do we think the fishermen will go to fish?!!!!!!!!!!!!!!!!

#do it to all ports and spit out a list.
#Each entry is a port containing all sea otter info for all years
#units for projection are in meters, so `distance =` units are meters
surrounding_port <- port_buffering(ports = monterey_bay_st, ocean = SeaOtter_df, distance = 100 * 1000)

#add a column with the port name
#port names
port_names <- c("Monterey Bay") #using Elkhorn Slough/Moss Landings as the actual GPS point because they are likely rolled into the same area. Moss Landings is more of a commerical fishing port anyways
for(indexentry in 1:length(port_names)){
  surrounding_port[[indexentry]]$port <- port_names[indexentry]
}

rm(list = "indexentry")

#Organizing port over time -------------------------------------------------------------

surrounding_port_df <- do.call(rbind.data.frame,surrounding_port) #make the list into a dataframe by rowbinding each list entry together
rm(list = "surrounding_port") #remove the sf object to save memory

#aggregate by port and year. Find the average density of otters in the surrounding water
port_density_aggregate <- surrounding_port_df %>%
  group_by(port, Year) %>%
  summarise(density = sum(dens_sm))

ggplot(port_density_aggregate, aes(x = Year, y = density, fill = port)) +
  geom_line() +
  theme_classic() +
  ggtitle("Density of Otters Around Moss Landing (5000m buffer)")

ggplot() +
  geom_sf(data = st_union(surrounding_port_df), aes(fill = ZONE)) +
  theme_bw()

#This will plot the geometry layers and give a fill
  #Takes a very long time to run if you don't subset it to [1:100]
#ggplot() + 
#  geom_sf(data = surrounding_port_df[1:100,], aes(fill = factor(ZONE)), lwd = 0) +
#  scale_fill_viridis_d(option = "magma", begin = 0.5)

#Otters 50km from Elkhorn Slough --------------------------------------------------

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
#Add the count column to the surrounding_port_df
  #Unfortunately, there are some cells that don't have a raw otter count, so it enters `NA`
  #looks like coverage drops off 2015-2018
surrounding_port_df <- left_join(surrounding_port_df, otter_individuals, by = c("Year", "ATOS_ID"))

#group_by ATOS_ID
surrounding_port_df %>%
  group_by(ATOS_ID) %>%
  summarise(mean_otters = mean(Count, na.rm = TRUE)) %>% #mean over the years while removing the NAs
  filter(ATOS_ID != 321) %>% #remove the polygon which is in Elkhorn Slough
  ggplot(aes(fill = mean_otters)) +
  geom_sf(lwd = 0) + #remove the border line
  scale_fill_viridis_c(option = "magma", begin = 0.1, name = "Otters") +
  theme_bw() +
  ggtitle("Mean Number of Otters within 50km of Elkhorn Slough (1985-2018)")

#The whole range -------------------------------------------------------

whole_range <- port_buffering(monterey_bay_st, SeaOtter_df, distance = (1000 * 1000))
everything <- do.call(rbind.data.frame, whole_range)
rm(list = "whole_range")

everything <- left_join(everything, otter_individuals, by = c("Year", "ATOS_ID")) %>%
  group_by(ATOS_ID) %>%
  summarise(mean_otters = mean(Count, na.rm = TRUE)) %>%
  filter(ATOS_ID != 321) #remove the polygon which is in Elkhorn Slough

ggplot(everything, aes(fill = mean_otters)) +
  geom_sf(lwd = 0) + #remove the border line
  scale_fill_viridis_c(option = "magma", begin = 0.1, name = "Otters") +
  theme_bw() +
  ggtitle("Mean Number of Otters (1985-2018)")

rm(list = "everything") #try to save memory

#100km around Morro Bay -------------------------------------------------

morro_bay_GPS <- data.frame(rbind(c(-120.87, 35.40))) #Morro Bay GPS
colnames(morro_bay_GPS) <- c("lon", "lat") #name the columns of the df so that 'st_as_sf' can use column names
morro_bay_st <- st_as_sf(morro_bay_GPS, coords = c("lon", "lat")) #make the df into a point object with multiple entries

st_crs(morro_bay_st) <- 4326 #EPSG for WSG84, used by Google Earth
morro_bay_st <- st_transform(morro_bay_st, 3310) #assign crs to 

buffered_morro <- do.call(rbind.data.frame,
                          port_buffering(morro_bay_st, SeaOtter_df,
                                         distance = (100 * 1000)))

buffered_morro <-left_join(buffered_morro, otter_individuals, by = c("Year", "ATOS_ID")) %>%
  group_by(ATOS_ID) %>%
  summarise(mean_otters = mean(Count, na.rm = TRUE))

ggplot() +
  geom_sf(data = buffered_morro, aes(fill = mean_otters),lwd = 0) + #remove the border line
  geom_sf(data = morro_bay_st, col = 'red', size = 20) +
  scale_fill_viridis_c(option = "magma",begin = 0.1, name = "Otters") +
  #scale_shape_discrete(name = NULL, values = c("A" =  "red")) +
  theme_bw() +
  ggtitle("Mean Number of Otters within 50km of Morro Bay (1985-2018)")

#100km around Halfmoon Bay -----------------------------------------------------

halfmoon_bay_GPS <- data.frame(rbind(c(-122.47, 37.49))) #Morro Bay GPS
colnames(halfmoon_bay_GPS) <- c("lon", "lat") #name the columns of the df so that 'st_as_sf' can use column names
halfmoon_bay_st <- st_as_sf(halfmoon_bay_GPS, coords = c("lon", "lat")) #make the df into a point object with multiple entries

st_crs(halfmoon_bay_st) <- 4326 #EPSG for WSG84, used by Google Earth
halfmoon_bay_st <- st_transform(halfmoon_bay_st, 3310) #assign crs to 

buffered_halfmoon <- do.call(rbind.data.frame,
                          port_buffering(halfmoon_bay_st, SeaOtter_df,
                                         distance = (100 * 1000)))

buffered_halfmoon <-left_join(buffered_halfmoon, otter_individuals, by = c("Year", "ATOS_ID")) %>%
  group_by(ATOS_ID) %>%
  summarise(mean_otters = mean(Count, na.rm = TRUE))

ggplot() +
  geom_sf(data = buffered_halfmoon, aes(fill = mean_otters),lwd = 0) + #remove the border line
  #geom_sf(data = halfmoon_bay_st, aes(fill = "white"), size = 20) +
  scale_fill_viridis_c(option = "magma",begin = 0.1, name = "Otters") +
  #scale_shape_discrete(name = NULL, values = c("A" =  "red")) +
  theme_bw() +
  ggtitle("Mean Number of Otters within 50km of Halfmoon Bay (1985-2018)")

#100km around Monterey -----------------------------------

