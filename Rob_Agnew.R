library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)
library(biogeo)

### Read in the GPS points which are store as latitude longitude




GPS_Pts_Rob_Agnew <-   read_excel("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/Regional winery data/Raw_data/Rob Agnew data/Updated Rob Agnew All Marlborough Sav Blanc _Yield_data for GYA Project mct.xlsx",
                         "Final_Dataset_Marlborough_Sauvi",  range = "A1:I1117"   )


str(GPS_Pts_Rob_Agnew)
GPS_Pts_Rob_Agnew <- select(GPS_Pts_Rob_Agnew,
                            vineyard =phenology.vineyard.name,
                            winery,
                            latitude,
                            longitude)  # new name = old name
#remove the missing data 
GPS_Pts_Rob_Agnew <- filter(GPS_Pts_Rob_Agnew, latitude != "NA")
str(GPS_Pts_Rob_Agnew)
############ From whitehaven

#Latitude Longitude
#make 3 new clms to reformat latitude 
GPS_Pts_Rob_Agnew$Latitude_1 <- sub("[^0-9]", "_", GPS_Pts_Rob_Agnew$latitude)
GPS_Pts_Rob_Agnew$Latitude_2 <- sub("[']", "_", GPS_Pts_Rob_Agnew$Latitude_1) 
GPS_Pts_Rob_Agnew$Latitude_3 <- sub('\"', " ", GPS_Pts_Rob_Agnew$Latitude_2, fixed = TRUE ) 

GPS_Pts_Rob_Agnew <- separate(GPS_Pts_Rob_Agnew, 
                              Latitude_3, 
                              into = c("Lat_dd", "Lat_mm", "Lat_ss"), 
                              sep = "\\_", remove = FALSE)


GPS_Pts_Rob_Agnew <- separate(GPS_Pts_Rob_Agnew, 
                              Lat_ss, into = c("Lat_ss", "Lat_L" ), 
                              sep = " ", remove = FALSE)

#make new clm double not charcaters
GPS_Pts_Rob_Agnew$Lat_dd <- as.double(GPS_Pts_Rob_Agnew$Lat_dd)
GPS_Pts_Rob_Agnew$Lat_mm <- as.double(GPS_Pts_Rob_Agnew$Lat_mm)
GPS_Pts_Rob_Agnew$Lat_ss <- as.double(GPS_Pts_Rob_Agnew$Lat_ss)

#convert from DMS to DD and create a new clm name
GPS_Pts_Rob_Agnew$Lat_DD <- dms2dd(GPS_Pts_Rob_Agnew$Lat_dd,
                                   GPS_Pts_Rob_Agnew$Lat_mm,
                                   GPS_Pts_Rob_Agnew$Lat_ss, 
                                   GPS_Pts_Rob_Agnew$Lat_L)
glimpse(GPS_Pts_Rob_Agnew)



# Longitude
#make 3 new clms to reformat latitude 
GPS_Pts_Rob_Agnew$Longitude_1 <- sub("[^0-9]", "_", GPS_Pts_Rob_Agnew$longitude)
GPS_Pts_Rob_Agnew$Longitude_2 <- sub("[']", "_", GPS_Pts_Rob_Agnew$Longitude_1) 
GPS_Pts_Rob_Agnew$Longitude_3 <- sub('\"', " ", GPS_Pts_Rob_Agnew$Longitude_2, fixed = TRUE ) 

GPS_Pts_Rob_Agnew <- separate(GPS_Pts_Rob_Agnew, 
                              Longitude_3, 
                              into = c("Long_dd", "Long_mm", "Long_ss"), 
                              sep = "\\_", remove = FALSE)


GPS_Pts_Rob_Agnew <- separate(GPS_Pts_Rob_Agnew, 
                              Long_ss, into = c("Long_ss", "Long_L" ), 
                              sep = " ", remove = FALSE)

#make new clm double not charcaters
GPS_Pts_Rob_Agnew$Long_dd <- as.double(GPS_Pts_Rob_Agnew$Long_dd)
GPS_Pts_Rob_Agnew$Long_mm <- as.double(GPS_Pts_Rob_Agnew$Long_mm)
GPS_Pts_Rob_Agnew$Long_ss <- as.double(GPS_Pts_Rob_Agnew$Long_ss)

#convert from DMS to DD and create a new clm name
GPS_Pts_Rob_Agnew$Long_DD <- dms2dd(GPS_Pts_Rob_Agnew$Long_dd,
                                    GPS_Pts_Rob_Agnew$Long_mm,
                                    GPS_Pts_Rob_Agnew$Long_ss, 
                                    GPS_Pts_Rob_Agnew$Long_L)
glimpse(GPS_Pts_Rob_Agnew)


GPS_Pts_Rob_Agnew <-  select(GPS_Pts_Rob_Agnew,
                             vineyard,
                             winery ,
                             latitude,
                             longitude,
                             Lat_DD,
                             Long_DD)



glimpse(GPS_Pts_Rob_Agnew)
write_csv(GPS_Pts_Rob_Agnew, "V:/Marlborough regional/working_jaxs/GPS_Pts_Rob_Agnew_check.csv")
