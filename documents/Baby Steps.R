library(sp)
library(rgeos)
library(rgdal)
library(tmap)
library(readxl) #package to read Excel's xls files
library(leaflet)

setwd(dir = "~/Desktop/Grad school/github/MBA Fellowship")

#Import data --------------
#the stupid data has multiple sheets, but only has data on sheet 1
test_data <- read_excel(path = "data/Spring1985_SeaOtterCensus/Census_sum.xls", sheet = 1)

test_map <- readOGR(dsn = "data/Spring1985_SeaOtterCensus", layer = "Census_sum_85")
proj4string(test_map) <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

test_merge <- merge(test_map, test_data, by.x = "POLY_ID", by.y = "POLY_ID")
plot(test_merge)

tm_shape(test_map) +
  tm_borders(alpha = 0.2) +
  tm_fill(col = "dens_sm", palette = 'YlGnBu', style = 'sd') #need to figure out which style works best for visualizing this data

#trying to add a gps point to the map
#-121.7868953, 36.8044003 for moss landing

dot <- data.frame(NA, nrow = 1, ncol = 2)
dot[1,1] <- -121.7868953
dot[1,2] <- 36.8044003
coordinates(dot)<- c(-121.7868953, 36.8044003)

polygons(dot, "ID")
SpatialPolygonsDataFrame(dot, match.ID = TRUE)

tm_shape(test_map) +
  tm_borders(alpha = 0.2) +
  tm_fill(col = "dens_sm", palette = 'YlGnBu', style = 'sd') 
  tm_shape(c(-121.7868953, 36.8044003)) +
  tm_dots(col = "Red")

  
#how to get coordinates into gps form ---------------------
  #convert CRS to the standard gps output
  converting_coords <- spTransform(test_map,CRS("+init=epsg:4326"))
  #extract coordinates from the shapefile
  head(coordinates(converting_coords))
  
  
# test interactive map -------------------
#set to interactive mode
tmap_mode("view")
#set to default polt mode
  tmap_mode("plot")
  

#thinking about how to analyze the data
#identify the mean distance that fishermen from a port will travel
    #this is easy with logbooks, but hard for crab
#Get the coordinates of the port
  #Grab the distance both north and south of the port coordinates as a subset
#plot mean otter density for that range
  #show this as a time series
#on the same plot, show a line of fish landings for that port
  