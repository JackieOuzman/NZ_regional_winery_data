
#install.packages("sp")
#install.packages("biogeo")
install.packages("rgdal")
install.packages("sf")


##Mess about getting 

#https://spatialreference.org/ref/epsg/2193/
mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(white_haven_GPS_DD)

test<- select(white_haven_GPS_DD, Lat_DD, Long_DD)
glimpse(test)
#proj4string(test) <- wgs84CRS   # assume input lat and longs are WGS84
coordinates(test) <- ~Long_DD+Lat_DD
proj4string(test) <- wgs84CRS   # assume input lat and longs are WGS84
test1 <- spTransform(test, mapCRS)

glimpse(test1)

#write_csv(test1, "check_out_projec.csv")
st_write(test1, "out.csv", layer_options = "coords")
library(sf)
st_write(test1, "test.shp")
st_write_db(test1,"out.csv", layer)

###REAL DATA

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(sp)
library(biogeo)
library(stringr)
library(rgdal)
library(sf)







white_haven_GPS <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Harvest  Mapping Data Whitehaven.xlsx", 
                              sheet = "All - Data",
                              col_types = c("text", "text", "text", 
                                            "text", "text", "text", "text", "text", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "text", "text", "text", 
                                            "text", "date", "numeric", "numeric", 
                                            "text", "numeric", "numeric", "numeric", 
                                            "date", "text", "numeric", "numeric", 
                                            "numeric", "date", "text", "numeric", 
                                            "numeric", "numeric", "text", "date", 
                                            "text", "numeric", "numeric", "numeric", 
                                            "date", "text", "numeric", "numeric", 
                                            "numeric", "date", "text", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric"))

white_haven_GPS_1 <-  select(white_haven_GPS,Vineyard,
                             Block,
                             Latitude,
                             Longitude,
                             Variety,
                             Row_spacing =`Row Spacing` ,
                             Vine_spacing =`Vine Spacing`)
glimpse(white_haven_GPS_1)
#Latitude Longitude
#make 3 new clms to reformat latitude 
white_haven_GPS_1$Latitude_1 <- sub("[^0-9]", "_", white_haven_GPS_1$Latitude)
white_haven_GPS_1$Latitude_2 <- sub("[']", "_", white_haven_GPS_1$Latitude_1) 
white_haven_GPS_1$Latitude_3 <- sub('\"', " ", white_haven_GPS_1$Latitude_2, fixed = TRUE ) 

white_haven_GPS_1 <- separate(white_haven_GPS_1, 
                                         Latitude_3, 
                                         into = c("Lat_dd", "Lat_mm", "Lat_ss"), 
                                         sep = "\\_", remove = FALSE)


white_haven_GPS_1 <- separate(white_haven_GPS_1, 
                              Lat_ss, into = c("Lat_ss", "Lat_L" ), 
                              sep = " ", remove = FALSE)

#make new clm double not charcaters
white_haven_GPS_1$Lat_dd <- as.double(white_haven_GPS_1$Lat_dd)
white_haven_GPS_1$Lat_mm <- as.double(white_haven_GPS_1$Lat_mm)
white_haven_GPS_1$Lat_ss <- as.double(white_haven_GPS_1$Lat_ss)

#convert from DMS to DD and create a new clm name
white_haven_GPS_1$Lat_DD <- dms2dd(white_haven_GPS_1$Lat_dd,
                                   white_haven_GPS_1$Lat_mm,
                                   white_haven_GPS_1$Lat_ss, 
                                   white_haven_GPS_1$Lat_L)
glimpse(white_haven_GPS_1)



# Longitude
#make 3 new clms to reformat latitude 
white_haven_GPS_1$Longitude_1 <- sub("[^0-9]", "_", white_haven_GPS_1$Longitude)
white_haven_GPS_1$Longitude_2 <- sub("[']", "_", white_haven_GPS_1$Longitude_1) 
white_haven_GPS_1$Longitude_3 <- sub('\"', " ", white_haven_GPS_1$Longitude_2, fixed = TRUE ) 

white_haven_GPS_1 <- separate(white_haven_GPS_1, 
                              Longitude_3, 
                              into = c("Long_dd", "Long_mm", "Long_ss"), 
                              sep = "\\_", remove = FALSE)


white_haven_GPS_1 <- separate(white_haven_GPS_1, 
                              Long_ss, into = c("Long_ss", "Long_L" ), 
                              sep = " ", remove = FALSE)

#make new clm double not charcaters
white_haven_GPS_1$Long_dd <- as.double(white_haven_GPS_1$Long_dd)
white_haven_GPS_1$Long_mm <- as.double(white_haven_GPS_1$Long_mm)
white_haven_GPS_1$Long_ss <- as.double(white_haven_GPS_1$Long_ss)

#convert from DMS to DD and create a new clm name
white_haven_GPS_1$Long_DD <- dms2dd(white_haven_GPS_1$Long_dd,
                                   white_haven_GPS_1$Long_mm,
                                   white_haven_GPS_1$Long_ss, 
                                   white_haven_GPS_1$Long_L)
glimpse(white_haven_GPS_1)


white_haven_GPS_DD <-  select(white_haven_GPS_1,Vineyard,
                             Block,
                             Latitude,
                             Longitude,
                             Lat_DD,
                             Long_DD,
                             Variety,
                             Row_spacing  ,
                             Vine_spacing)



glimpse(white_haven_GPS_DD)

#Now I have my data in decimal degrees I want to convert it into GDA
### ASK DAVID 

write_csv(white_haven_GPS_DD, "white_haven_GPS_DD.csv")
#I need to work on this converting to GDA in R

white_haven_GPS_GDA.csv <- read_csv(file = "white_haven_GPS_GDA.csv" )
          
          
white_haven <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Harvest  Mapping Data Whitehaven.xlsx", 
                              sheet = "All - Data")
#Create a ID Code with vineyard abbrevation, block variety and year


white_haven <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Harvest  Mapping Data Whitehaven.xlsx", 
                                              sheet = "All - Data",
                                              col_types = c("text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "text", "text", "text", 
                                                            "text", "date", "numeric", "numeric", 
                                                            "text", "numeric", "numeric", "numeric", 
                                                            "date", "text", "numeric", "numeric", 
                                                            "numeric", "date", "text", "numeric", 
                                                            "numeric", "numeric", "text", "date", 
                                                            "text", "numeric", "numeric", "numeric", 
                                                            "date", "text", "numeric", "numeric", 
                                                            "numeric", "date", "text", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric"))



###2019
white_haven_2019 <- mutate(white_haven,
                      vineyard_abb = tolower(substr(white_haven$Vineyard, 1, 3)),
                      block_abb1 = gsub(" ", "", white_haven$Block),
                      block_abb2 = gsub("block", "", block_abb1, ignore.case = TRUE),
                      block_abb = tolower(substr(block_abb2, 1, 5)),
                      ID = paste0(vineyard_abb, "_", block_abb, "_", tolower(Variety)),
                      year = 2019,
                      ID_yr = paste0(ID, "_", year),
                      julian = as.numeric(format(`V2019 Harvest Date`, "%j")),
                      bunch_numb_m = NA)
glimpse(white_haven_2019)

white_haven_2019_1 <- select(white_haven_2019,
                           ID,
                           ID_yr ,
                           Vineyard,
                           Block,
                           Variety,
                           harvest_date = `V2019 Harvest Date`,
                           julian ,
                           yield_t_ha =   `V2019 t/ha`,
                           yield_kg_m =   `V2019 kg/m`,
                           brix =         `V2019 Brix`,
                           bunch_weight = `V2019 Bunch Weight Harvest`,
                           berry_weight = `V2019 Berry Weight Harvest`,
                           bunch_numb_m,   #to be cal
                           pruning_style = `V2019 Cane #`, #check this
                           row_width =`Row Spacing`,
                           vine_spacing = `Vine Spacing`)
glimpse(white_haven_2019_1)


#some brix have 0 values that need to be replaced with NA 
#some brix have multiple readings eg 22.4/22.5 this needs to be averaged
#split brix clm into multiples
white_haven_2019_1 <- separate(white_haven_2019_1, 
                               brix, into = c("brix_a", "brix_b" ), 
                               sep = "/", remove = FALSE)

#Change these new clm into numbers not characters 
white_haven_2019_1$brix_a <- as.double(white_haven_2019_1$brix_a)
white_haven_2019_1$brix_b <- as.double(white_haven_2019_1$brix_b)
#create a new clm for average
white_haven_2019_1$brix_av <- rowMeans(select(white_haven_2019_1, brix_a, brix_b), na.rm = TRUE)
#change the NaN to NA and report as number not character
white_haven_2019_1$brix_av <- as.double(gsub("NaN", "NA", white_haven_2019_1$brix_av)) 
#rename the 0 value to NA
white_haven_2019_1$brix_av[white_haven_2019_1$brix_av == 0] <- NA

glimpse(white_haven_2019_1)





###2018
glimpse(white_haven)

#2018 is missing some data that 2019 has

white_haven_2018 <- mutate(white_haven,
                           vineyard_abb = tolower(substr(white_haven$Vineyard, 1, 3)),
                           block_abb1 = gsub(" ", "", white_haven$Block),
                           block_abb2 = gsub("block", "", block_abb1, ignore.case = TRUE),
                           block_abb = tolower(substr(block_abb2, 1, 5)),
                           ID = paste0(vineyard_abb, "_", block_abb, "_", tolower(Variety)),
                           year = 2019,
                           ID_yr = paste0(ID, "_", year),
                           harvest_date =  `V2018 Harvest Date`,
                           julian = as.numeric(format(harvest_date, "%j")),
                           bunch_numb_m = NA,
                           bunch_weight = NA,
                           berry_weight = NA)
glimpse(white_haven_2018)

white_haven_2018_1 <- select(white_haven_2018,
                             ID,
                             ID_yr ,
                             Vineyard,
                             Block,
                             Variety,
                             harvest_date,
                             julian ,
                             yield_t_ha =   `V2018 t/ha`,
                             yield_kg_m =   `V2018 kg/m`,
                             brix =         `V2018 Brix`,
                             bunch_weight , #no data
                             berry_weight , #no data
                             bunch_numb_m,  # no data
                             pruning_style = `V2018 Cane #`, #check this
                             row_width =`Row Spacing`,
                             vine_spacing = `Vine Spacing`)

glimpse(white_haven_2018_1)

#some brix have 0 values that need to be replaced with NA 
#some brix have multiple readings eg 22.4,22.5 this needs to be averaged
#split brix clm into multiples
white_haven_2018_1 <- separate(white_haven_2018_1, 
                               brix, into = c("brix_a", "brix_b" ), 
                               sep = ",", remove = FALSE)

#Change these new clm into numbers not characters 
white_haven_2018_1$brix_a <- as.double(white_haven_2018_1$brix_a)
white_haven_2018_1$brix_b <- as.double(white_haven_2018_1$brix_b)
#create a new clm for average
white_haven_2018_1$brix_av <- rowMeans(select(white_haven_2018_1, brix_a, brix_b), na.rm = TRUE)
#change the NaN to NA and report as number not character
white_haven_2018_1$brix_av <- as.double(gsub("NaN", "NA", white_haven_2018_1$brix_av)) 
#rename the 0 value to NA
white_haven_2018_1$brix_av[white_haven_2018_1$brix_av == 0] <- NA

glimpse(white_haven_2018_1)
