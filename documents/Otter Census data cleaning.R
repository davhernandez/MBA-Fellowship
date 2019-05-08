library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(readxl) #package to read Excel's xls files
library(stringr)
library(ggplot2)
library(dplyr)

setwd(dir = "~/Desktop/Grad school/github/MBA Fellowship")

# create a list of all files in the directory
list_filenames<-list.files(path = "data/Otter Data", pattern="SeaOtterCensus") # use the pattern argument to define a common pattern for import files with regex


load_shapefiles <- function(file_name){
  partial_year <- str_sub(file_name, start = 9, end = 10)   #grabs the last two digits of the year for the current file
  #sf:::as_Spatial(
  st_read(
    dsn = sprintf("data/Otter Data/%s", file_name),
    layer = sprintf("Census_sum_%s", partial_year),
    quiet = TRUE) #keeps it from printing file info in the console
  #) #sf:::as_Spatial will make the object an S4 SpatialPolygonDataFrame like readOGR would. Otherwise the output is an S3 polygon
}

list_data <- lapply(list_filenames, function(x) load_shapefiles(x)) #import data

names(list_data)<-list_filenames # add the names of your data to the list
# now you can index one of your tables like this
#list.data$Spring1985_SeaOtterCensus

#for some reason, 2018 had missing or misnamed columns. This section is about fixing those inconsistencies
#rename `id` column to `POLY_ID` so that it matches the other years
colnames(list_data$Spring2018_SeaOtterCensus)[colnames(list_data$Spring2018_SeaOtterCensus) == "id"] <- "POLY_ID"
#add a Year column populated with '2018;
list_data$Spring2018_SeaOtterCensus$Year <- rep(x = 2018, times = nrow(list_data$Spring2018_SeaOtterCensus))
#add a Sect_ID column. Right now, I don't know if this infor is important or how to populate this column
list_data$Spring2018_SeaOtterCensus$Sect_ID <- 0

#rbind all parts of the list_data into a dataframe. Basically, unlisting it into a dataframe
SeaOtter_df <- do.call(rbind.data.frame, list_data)

#make sure that ATOS_ID matches the first 3 number of HAB_ID
#determine which rows have a mismatch
mismatched <- which(SeaOtter_df$ATOS_ID != 
                      substr(SeaOtter_df$HAB_ID, start = 1, stop = (nchar(as.character(SeaOtter_df$HAB_ID))-1)))
#all mismatched rows have '1286' instead of '126'
SeaOtter_df$ATOS_ID[mismatched] <- 126

#Loop figures out the min and max ATOS_ID for each Sect_ID, then finds the rows from 2018 that fall within that ATOS_ID range and inputs the Sect_ID value
for(i in 1:14){
  #identify which ATOS values the region are
  min <- min(SeaOtter_df$ATOS_ID[which(SeaOtter_df$Sect_ID == i)])
  max <- max(SeaOtter_df$ATOS_ID[which(SeaOtter_df$Sect_ID == i)])
  #input the value of i into SECT_ID of year 2018
  SeaOtter_df$Sect_ID[which(SeaOtter_df$ATOS_ID >= min &
                              SeaOtter_df$ATOS_ID <= max &
                              SeaOtter_df$Year == 2018)] <- i
}

#find CRS ------------------------------
#determine the CRS of our data
#notice that it has a proj4string but no EPSG code
st_crs(SeaOtter_df)

#let's look up the EPSG code
#filter all EPSG codes for ones that have '+proj=aea'
#make_EPSG() is a list of every EPSG code
aea_proj4s <-make_EPSG() %>%
  filter(str_detect(prj4, "aea"))
#find the entry that matches our proj4string
aea_proj4s %>%
  filter(prj4 == otter_crs[2])

#our EPSG code is 3310
#now assign it to the data
st_crs(SeaOtter_df) <- 3310
#note that assigning CRS does not reproject the data, it just assigns the code.
#reassigning the CRS in this way will overwrite the EPSG and proj4 codes
st_crs(SeaOtter_df)

#save data -----------------------------------
st_write(SeaOtter_df, "data/Otter Data/Cleaned Data/Otter_census_cleaned.shp")
