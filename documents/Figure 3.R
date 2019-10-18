#setup ---------------------------------------------
#reworking the code from `Pub plot 3.R`
#last minute hail mary and I didn't want to delete all of that code so here is a new script
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
library(gridExtra)
library(ggpubr)

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

#st objects ----------------------------------------------------------------------------
#Half Moon Bay
halfmoon_bay_GPS <- data.frame(rbind(c(-122.47, 37.49))) #Half Moon Bay GPS
colnames(halfmoon_bay_GPS) <- c("lon", "lat") #name the columns of the df so that 'st_as_sf' can use column names
halfmoon_bay_st <- st_as_sf(halfmoon_bay_GPS, coords = c("lon", "lat")) #make the df into a point object with multiple entries
st_crs(halfmoon_bay_st) <- 4326 #EPSG for WSG84, used by Google Earth
halfmoon_bay_st <- st_transform(halfmoon_bay_st, 3310) #assign crs to

#Monterey
monterey_bay_GPS <- data.frame(rbind(c(-121.80, 36.60))) #Monterey GPS
colnames(monterey_bay_GPS) <- c("lon", "lat")
monterey_bay_st <- st_as_sf(monterey_bay_GPS, coords = c("lon", "lat"))
st_crs(monterey_bay_st) <- 4326
monterey_bay_st <- st_transform(monterey_bay_st, 3310)

#Morro Bay
morro_bay_GPS <- data.frame(rbind(c(-120.87, 35.40))) #Morro Bay GPS
colnames(morro_bay_GPS) <- c("lon", "lat")
morro_bay_st <- st_as_sf(morro_bay_GPS, coords = c("lon", "lat"))
st_crs(morro_bay_st) <- 4326
morro_bay_st <- st_transform(morro_bay_st, 3310)

#plot function----------------------------------------------------------------------------------
regression_plots <- function(range){
  shore_distance <- function(port, distance){
    #find the closes grid cell to the port
    grid_cell <- SeaOtter_df$ATOS_ID[which.min(st_distance(port, SeaOtter_df))]
    #add and subtract from he grid cell value based on the distance
    cell_range <- c((grid_cell-(distance*2)):(grid_cell+(distance*2))) #2*distance on either side because grid cells are 500m wide
    otter_individuals %>%
      filter(ATOS_ID %in% cell_range) %>%
      group_by(Year) %>%
      summarise(Otters = sum(Count))
  }
#make the plots
  plotting <- function(site, port_st, distance){
    readRDS("data/Crab Cleaned/Effort by Port") %>%
    mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
    dplyr::filter(Year <= 2014, Year >= 1985, Location == site, Effort < 40000) %>%
    group_by(Year) %>%
    summarise(CPUE = mean(Effort) * 0.0004535924) %>%
    mutate(Location = site) %>%
    merge(.,shore_distance(port_st, distance), by = "Year") %>% #call the `shore_distance` function
    ggplot(aes(x = Otters, y = CPUE)) +
    geom_smooth(method = 'lm', color = "#0aa1ff", fill = "#9dd9ff", alpha = 1) +
    geom_point(color = "#0aa1ff", size = 2.5) +
    theme_classic() +
      theme(#axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(c(0.2,1), unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
      #coord_cartesian(xlim = x_scale, ylim = c(0, 0.5)) #use for stable axis
      coord_cartesian(ylim = c(0, 0.5)) #use for floating x-axis
  }

  #call functions to make plots
  output <- list()
  output[[1]] <-plotting("Halfmoon_Bay", halfmoon_bay_st, range)
  output[[2]] <- plotting("Monterey", monterey_bay_st,range)
  output[[3]] <- plotting("Morro_Bay", morro_bay_st, range)
  return(output)
}

#plotting------------------------------------------------------------------------------
fig_3 <- regression_plots(range = 100)
ggarrange(fig_3[[1]], fig_3[[2]], fig_3[[3]],
          ncol = 1, nrow = 3,
          align = "hv")

#Supplimental 2
supplimental <- lapply(c(20,50,100,150,200), regression_plots)


#build composite plot
ggarrange(supplimental[[1]][[1]], supplimental[[2]][[1]], supplimental[[3]][[1]], supplimental[[4]][[1]], supplimental[[5]][[1]],
          supplimental[[1]][[2]], supplimental[[2]][[2]], supplimental[[3]][[2]], supplimental[[4]][[2]], supplimental[[5]][[2]],
          supplimental[[1]][[3]], supplimental[[2]][[3]], supplimental[[3]][[3]], supplimental[[4]][[3]], supplimental[[5]][[3]],
          ncol = 5, nrow = 3,
          align = "hv") #aling both `h` and `v`

#sandbox --------------------------------------------------------

dummy_function <- function(port, distance){
  #find the closes grid cell to the port
  grid_cell <- SeaOtter_df$ATOS_ID[which.min(st_distance(port, SeaOtter_df))]
  #add and subtract from he grid cell value based on the distance
  cell_range <- c((grid_cell-(distance*2)):(grid_cell+(distance*2))) #2*distance on either side because grid cells are 500m wide
  otter_individuals %>%
    filter(ATOS_ID %in% cell_range) %>%
    #ggplot(., aes(fill = dens_sm)) +
    #geom_sf()
    group_by(Year) %>%
    summarise(Otters = sum(Count))
}

dummy_function(monterey_bay_st, 20) %>% View 


#getting the regression for the plots
regression_significance <- function(port, range, site){
  shore_distance <- function(port, distance){
    #find the closes grid cell to the port
    grid_cell <- SeaOtter_df$ATOS_ID[which.min(st_distance(port, SeaOtter_df))]
    #add and subtract from he grid cell value based on the distance
    cell_range <- c((grid_cell-(distance*2)):(grid_cell+(distance*2))) #2*distance on either side because grid cells are 500m wide
    otter_individuals %>%
      filter(ATOS_ID %in% cell_range) %>%
      group_by(Year) %>%
      summarise(Otters = sum(Count))
  }
  #make the plots
    readRDS("data/Crab Cleaned/Effort by Port") %>%
      mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
      dplyr::filter(Year <= 2014, Year >= 1985, Location == site, Effort < 40000) %>%
      group_by(Year) %>%
      summarise(CPUE = mean(Effort) * 0.0004535924) %>%
      mutate(Location = site) %>%
      merge(.,shore_distance(port, range), by = "Year") #call the `shore_distance` function
}

#results from regression test for significance can be found on pg 155 of the Orange Sea Grant notebook
test <- regression_significance(halfmoon_bay_st, 200, "Halfmoon_Bay") #%>%
  summary(lm(Otters ~ CPUE,test))
