#setup ---------------------------------------------
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

#buffer around fishing ports --------------------------------------------------------------------


buffer_plots_function <- function(buffer_range, axes = FALSE, x_scale = c(0,3000)){
  
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
  
  
  #apply buffer to each st object
  buffered_halfmoon <- do.call(rbind.data.frame,
                               port_buffering(halfmoon_bay_st, SeaOtter_df,
                                              distance = (buffer_range * 1000)))
  #pull out only ATOS_IDs that are in the buffer zone
  buffered_halfmoon <-otter_individuals[which(otter_individuals$ATOS_ID %in% unique(buffered_halfmoon$ATOS_ID)),] %>%
    group_by(Year) %>%
    summarise(Otters = sum(Count))
  
  buffered_monterey <- do.call(rbind.data.frame,
                               port_buffering(monterey_bay_st, SeaOtter_df,
                                              distance = buffer_range * 1000))
  buffered_monterey <- otter_individuals[which(otter_individuals$ATOS_ID %in% unique(buffered_monterey$ATOS_ID)),] %>%
    group_by(Year) %>%
    summarise(Otters = sum(Count))

  buffered_morro <- do.call(rbind.data.frame,
                            port_buffering(morro_bay_st, SeaOtter_df,
                                           distance = (buffer_range * 1000)))
  buffered_morro <-otter_individuals[which(otter_individuals$ATOS_ID %in% unique(buffered_morro$ATOS_ID)),] %>%
    group_by(Year) %>%
    summarise(Otters = sum(Count))
  
  #make the plots
  default_plot <- function(site){
    readRDS("data/Crab Cleaned/Effort by Port") %>%
      mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
      dplyr::filter(Year <= 2014, Year >= 1985, Location == site, Effort < 40000) %>%
      group_by(Year) %>%
      summarise(CPUE = mean(Effort) * 0.0004535924) %>%
      mutate(Location = site) %>% #!!!!!!!ummmm hard coded Monterey?!?!?!
      merge(.,buffered_monterey, by = "Year") %>% #hard coded again!!!!!!!
      ggplot(aes(x = Otters, y = CPUE)) +
      geom_smooth(method = 'lm', color = "#0aa1ff", fill = "#9dd9ff", alpha = 1) +
      geom_point(color = "#0aa1ff", size = 2.5) +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.length=unit(-0.1, "cm"),
            panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
      #coord_cartesian(xlim = x_scale, ylim = c(0, 0.5)) #use for stable axis
    coord_cartesian(ylim = c(0, 0.5)) #use for floating x-axis
  }
  
  #make the plots
  if(axes == FALSE){ #normal plots
    output  <- lapply(c("Halfmoon_Bay", "Monterey", "Morro_Bay"), default_plot)
  } else if(axes == TRUE){
  #for the single plot that needs axis labels
    #plots Half Moon Bay and Monterey as normal
    output  <- lapply(c("Halfmoon_Bay", "Monterey"), default_plot)
    #plot with axis labels
    output[[3]] <-     readRDS("data/Crab Cleaned/Effort by Port") %>%
      mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
      dplyr::filter(Year <= 2014, Year >= 1985, Location == "Morro_Bay", Effort < 40000) %>%
      group_by(Year) %>%
      summarise(CPUE = mean(Effort) * 0.0004535924) %>%
      mutate(Location = "Morro_Bay") %>%
      merge(.,buffered_morro, by = "Year") %>%
      ggplot(aes(x = Otters, y = CPUE)) +
      geom_smooth(method = 'lm', color = "#0aa1ff", fill = "#9dd9ff", alpha = 1) +
      geom_point(color = "#0aa1ff", size = 2.5) +
      ylab("metric tons per offload receipt") +
      xlab("otter population size") +
      theme_classic() +
      theme(#axis.title.x = element_blank(),
            #axis.title.y = element_blank(),
            axis.text.x = element_text(margin = margin(c(0.2,1), unit = "cm")),
            axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
            axis.ticks.length=unit(-0.1, "cm"),
            panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
      #coord_cartesian(xlim = x_scale, ylim = c(0, 0.5)) #use for stable axis
    coord_cartesian(ylim = c(0, 0.5)) #use for floating x-axis
    
    
  }
return(output)
}

#plots -------------------------------------------------------------------------

#Figure 3
fig_3 <- buffer_plots_function(buffer_range = 100, axes = TRUE, x_scale = c(940,1790))
ggarrange(fig_3[[1]], fig_3[[2]], fig_3[[3]],
          ncol = 1, nrow = 3,
          align = "hv")

#Supplimental 1
buffers_axis <- buffer_plots_function(buffer_range = 20, axes = TRUE)
buffers <- lapply(c(50,100,150,200), buffer_plots_function, axes = TRUE)


#build composite plot
ggarrange(buffers_axis[[1]], buffers[[1]][[1]], buffers[[2]][[1]], buffers[[3]][[1]], buffers[[4]][[1]],
          buffers_axis[[2]], buffers[[1]][[2]], buffers[[2]][[2]], buffers[[3]][[2]], buffers[[4]][[2]],
          buffers_axis[[3]], buffers[[1]][[3]], buffers[[2]][[3]], buffers[[3]][[3]], buffers[[4]][[3]],
          ncol = 5, nrow = 3,
          align = "hv") #aling both `h` and `v`



#sandbox --------------------------------

buffer_plots_function <- function(buffer_range, axes = FALSE, x_scale = c(0,3000)){
  
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
  
  
  #apply buffer to each st object
  buffered_halfmoon <- do.call(rbind.data.frame,
                               port_buffering(halfmoon_bay_st, SeaOtter_df,
                                              distance = (buffer_range * 1000)))
  #pull out only ATOS_IDs that are in the buffer zone
  buffered_halfmoon <-otter_individuals[which(otter_individuals$ATOS_ID %in% unique(buffered_halfmoon$ATOS_ID)),] %>%
    group_by(Year) %>%
    summarise(Otters = sum(Count))
  
  buffered_monterey <- do.call(rbind.data.frame,
                               port_buffering(monterey_bay_st, SeaOtter_df,
                                              distance = buffer_range * 1000))
  buffered_monterey <- otter_individuals[which(otter_individuals$ATOS_ID %in% unique(buffered_monterey$ATOS_ID)),] %>%
    group_by(Year) %>%
    summarise(Otters = sum(Count))
  
  buffered_morro <- do.call(rbind.data.frame,
                            port_buffering(morro_bay_st, SeaOtter_df,
                                           distance = (buffer_range * 1000)))
  buffered_morro <-otter_individuals[which(otter_individuals$ATOS_ID %in% unique(buffered_morro$ATOS_ID)),] %>%
    group_by(Year) %>%
    summarise(Otters = sum(Count))
  
  #make the plots
  output <- list()
    #Monterey plot
    output[[1]] <- readRDS("data/Crab Cleaned/Effort by Port") %>%
      mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
      dplyr::filter(Year <= 2014, Year >= 1985, Location == "Monterey", Effort < 40000) %>%
      group_by(Year) %>%
      summarise(CPUE = mean(Effort) * 0.0004535924) %>%
      mutate(Location = "Monterey") %>%
      merge(.,buffered_monterey, by = "Year") %>%
      ggplot(aes(x = Otters, y = CPUE)) +
      geom_smooth(method = 'lm', color = "#0aa1ff", fill = "#9dd9ff", alpha = 1) +
      geom_point(color = "#0aa1ff", size = 2.5) +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(margin = margin(c(0.2,1), unit = "cm")),
            axis.ticks.length=unit(-0.1, "cm"),
            panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
      #coord_cartesian(xlim = x_scale, ylim = c(0, 0.5)) #use for stable axis
      coord_cartesian(ylim = c(0, 0.5)) #use for floating x-axis

  #Halfmoon Bay plot
output[[2]] <- readRDS("data/Crab Cleaned/Effort by Port") %>%
  mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
  dplyr::filter(Year <= 2014, Year >= 1985, Location == "Halfmoon_Bay", Effort < 40000) %>%
  group_by(Year) %>%
  summarise(CPUE = mean(Effort) * 0.0004535924) %>%
  mutate(Location = "Halfmoon_Bay") %>%
  merge(.,buffered_halfmoon, by = "Year") %>%
  ggplot(aes(x = Otters, y = CPUE)) +
  geom_smooth(method = 'lm', color = "#0aa1ff", fill = "#9dd9ff", alpha = 1) +
  geom_point(color = "#0aa1ff", size = 2.5) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin = margin(c(0.2,1), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  #coord_cartesian(xlim = x_scale, ylim = c(0, 0.5)) #use for stable axis
  coord_cartesian(ylim = c(0, 0.5)) #use for floating x-axis
  
  #make the plots
  if(axes == FALSE){ #normal plots
    output[[3]] <- readRDS("data/Crab Cleaned/Effort by Port") %>%
      mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
      dplyr::filter(Year <= 2014, Year >= 1985, Location == "Morro_Bay", Effort < 40000) %>%
      group_by(Year) %>%
      summarise(CPUE = mean(Effort) * 0.0004535924) %>%
      mutate(Location = "Morro_Bay") %>%
      merge(.,buffered_morro, by = "Year") %>%
      ggplot(aes(x = Otters, y = CPUE)) +
      geom_smooth(method = 'lm', color = "#0aa1ff", fill = "#9dd9ff", alpha = 1) +
      geom_point(color = "#0aa1ff", size = 2.5) +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            #axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.length=unit(-0.1, "cm"),
            panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
      #coord_cartesian(xlim = x_scale, ylim = c(0, 0.5)) #use for stable axis
      coord_cartesian(ylim = c(0, 0.5)) #use for floating x-axis
  } else if(axes == TRUE){
    #for the single plot that needs axis labels
    #plot with axis labels
    output[[3]] <-     readRDS("data/Crab Cleaned/Effort by Port") %>%
      mutate(Year = as.numeric(format(.$Date, "%Y"))) %>%
      dplyr::filter(Year <= 2014, Year >= 1985, Location == "Morro_Bay", Effort < 40000) %>%
      group_by(Year) %>%
      summarise(CPUE = mean(Effort) * 0.0004535924) %>%
      mutate(Location = "Morro_Bay") %>%
      merge(.,buffered_morro, by = "Year") %>%
      ggplot(aes(x = Otters, y = CPUE)) +
      geom_smooth(method = 'lm', color = "#0aa1ff", fill = "#9dd9ff", alpha = 1) +
      geom_point(color = "#0aa1ff", size = 2.5) +
      ylab("metric tons per offload receipt") +
      xlab("otter population size") +
      theme_classic() +
      theme(#axis.title.x = element_blank(),
        #axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(c(0.2,1), unit = "cm")),
        axis.text.y = element_text(margin = margin(c(1, 0.2), unit = "cm")),
        axis.ticks.length=unit(-0.1, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
      #coord_cartesian(xlim = x_scale, ylim = c(0, 0.5)) #use for stable axis
      coord_cartesian(ylim = c(0, 0.5)) #use for floating x-axis
    
    
  }
  return(output)
}

