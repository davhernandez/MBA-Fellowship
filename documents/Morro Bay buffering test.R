library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(tmap)
library(stringr)
library(ggplot2)
library(dplyr)

setwd(dir = "~/Desktop/Grad school/github/MBA Fellowship")
#raw data was imported, cleaned, and saved in `Otter Census data cleaning.R`
#loading the cleaned data file
SeaOtter_df <- st_read(dsn = "data/Otter Data/Cleaned Data", layer = "Otter_census_cleaned")

#assigning EPSG code because st_write didn't save that
st_crs(SeaOtter_df) <- 3310

#Port GPS points ----------------------------------------------------------------------------------
#import or manually enter the gps coordinates for all of the fishing ports
morro_bay_GPS <- data.frame(rbind(c(-120.866953, 35.363525), c(-120.87, 35.40)))
colnames(morro_bay_GPS) <- c("lon", "lat") #name the columns of the df so that 'st_as_sf' can use column names
morro_bay_st <- st_as_sf(morro_bay_GPS, coords = c("lon", "lat")) #make the df into a point object with multiple entries

#morro_bay_st <- st_geometry(morro_bay_st) #assigns a geometry to the point df
st_crs(morro_bay_st) <- 4326 #EPSG for WSG84, used y Google Earth
morro_bay_st <- st_transform(morro_bay_st, 3310) #assign crs to 


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
surrounding_port <- port_buffering(ports = morro_bay_st, ocean = SeaOtter_df, distance = 5000)

#add a column with the port name
#port names
port_names <- c("Morro Bay", "Morro2")
for(indexentry in 1:length(port_names)){
  surrounding_port[[indexentry]]$port <- port_names[indexentry]
}

rm(list = "indexentry")

#Organizing port over time -------------------------------------------------------------

#make the list into a dataframe by rowbinding each list entry together
surrounding_port_df <- do.call(rbind.data.frame,surrounding_port)

#aggregate by port and year. Find the average density of otters in the surrounding water
port_density_aggregate <- surrounding_port_df %>%
                    group_by(port, Year) %>%
                    summarise(density = mean(dens_sm))

ggplot(port_density_aggregate, aes(x = Year, y = density, fill = port)) +
  geom_line()
#analysis ----------------------------------------------------------------------------------------

#come up with baseline fishing amounts for the entire year
crab_data %>%
  group_by(year, month) %>%
  summarise(catch = mean(landings))
#plot residuals from the baseline for each port
#residuals vs average otter density in the otter ports
#track difference in difference between ports with and without otters

#sandbox ------------------------------

dummy_geometry <- st_geometry(morro_bay_st)
dummy_buffer <- st_buffer(dummy_geometry, dist = 5000)
plot(dummy_buffer, border = "red")
plot(surrounding_port$geometry, add = TRUE)

dummy_contains <- st_contains(dummy_buffer, SeaOtter_df)
dummy_subset_list <- list()
for(listentry in 1:length(dummy_contains)){
  dummy_subset_list[[listentry]] <- SeaOtter_df[unlist(dummy_contains[[listentry]]),]
  print(listentry)
}


rm(list = "dummy_geometry", "dummy_buffer", "dummy_contains", "dummy_subset_list")
