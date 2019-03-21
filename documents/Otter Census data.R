library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(tmap)
library(readxl) #package to read Excel's xls files
library(leaflet)
library(stringr)
library(ggplot2)
library(mapview)

setwd(dir = "~/Desktop/Grad school/github/MBA Fellowship")

# create a list of all files in the directory
list_filenames<-list.files(path = "data", pattern="SeaOtterCensus") # use the pattern argument to define a common pattern for import files with regex


load_shapefiles <- function(file_name){
  partial_year <- str_sub(file_name, start = 9, end = 10)   #grabs the last two digits of the year for the current file
  #sf:::as_Spatial(
    st_read(
      dsn = sprintf("data/%s", file_name),
      layer = sprintf("Census_sum_%s", partial_year),
      quiet = TRUE) #keeps it from printing file info in the console
    #) #sf:::as_Spatial will make the object an S4 SpatialPolygonDataFrame like readOGR would. Otherwise the output is an S3 polygon
}

list_data <- lapply(list_filenames, function(x) load_shapefiles(x)) #import data


names(list_data)<-list_filenames # add the names of your data to the list

# now you can index one of your tables like this
#list.data$Spring1985_SeaOtterCensus

#port GPS -----------------
port_GPS <- data.frame(longitude = -120.8499924, latitude = 35.3659445) #input GPS points

port_points <- st_as_sf(x = port_GPS, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") #convert GPS to projected geometry
  
mapview(port_points)

tm_shape(list_data$Spring1985_SeaOtterCensus) +
  tm_borders(alpha = 0.4) +
  tm_fill(col = "dens_sm", palette = 'YlGnBu', style = 'sd') +
tm_shape(port_points) +
  tm_dots(col = "red", size = 1)

#buffer around GPS --------------------------------

boop <- gBuffer(list_data$Spring1985_SeaOtterCensus, width = 20, byid= TRUE)
plot(boop)

port_spatial <- SpatialPoints(port_GPS, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
port_spatial <- spTransform(port_spatial, CRSobj = "+proj=longlat +datum=WGS84 +no_defs")
gBuffer(port_spatial)
