
#install.packages("sp")
#install.packages("biogeo")
install.packages("rgdal")
install.packages("sf")


##Mess about getting 

#https://spatialreference.org/ref/epsg/2193/
#mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
#wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

#glimpse(white_haven_GPS_DD)

#test<- select(white_haven_GPS_DD, Vineyard, Lat_DD, Long_DD)
#glimpse(test)
#proj4string(test) <- wgs84CRS   # assume input lat and longs are WGS84
#coordinates(test) <- ~Long_DD+Lat_DD
#proj4string(test) <- wgs84CRS   # assume input lat and longs are WGS84
#test1 <- spTransform(test, mapCRS)

#glimpse(test1)

#coordinates(test1) #this pulls out the coordinates from my spatial point data frame
#but I want to keep the ID column - in this case the $Vineyards ? not sure how to do this??

#test1.df = as.data.frame(test1) #this has the new coordinates projected !YES!!
#head(test1.df)



################  TRY THIS  Example #################
#make a dummy x and y
#xc = round(runif(10), 2)
#head(xc)
#yc = round(runif(10), 2)
#xy = cbind(xc, yc)
#head(xy)
#convert to spatial points
#xy.sp = SpatialPoints(xy)
#xy.sp #this is different to the demo
#xy.cc = coordinates(xy.sp)
#xy.cc
#xy.sp[1:2] #this is different to the demo
#xy.df = as.data.frame(xy.sp)
#class(xy.df)
#summary(xy.sp)
#head(xy.sp)
#plot(xy.sp, pch=2)
#bbox(xy.sp)
#xy.df = as.data.frame(xy.sp)
#xy.df
##build points with atttributes


#df = data.frame(z1 = round(5+rnorm(10), 2), z2 = 20:29)
#df
#xy.spdf = SpatialPointsDataFrame(xy.sp, df)
#xy.spdf #not the same as the demo?? why
#xy.spdf[1:2, ]
#xy.df = as.data.frame(xy.spdf)



###REAL DATA

#install.packages("sp")
#install.packages("biogeo")
install.packages("rgdal")
install.packages("sf")


library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(sp)
library(biogeo)
library(stringr)
library(rgdal)
library(sf)


######################################################################################################################
################                         Make DF with GPS coodinates                                 #################
######################################################################################################################


################  Bring in data file   and reformat coodinates clms      ################ ###############     #################


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


############### Data still need a ID clms ##########

white_haven_GPS_DD1 <- mutate(white_haven_GPS_DD,
                           vineyard_abb = tolower(substr(white_haven_GPS_DD$Vineyard, 1, 10)),
                           block_abb1 = gsub(" ", "", white_haven_GPS_DD$Block),
                           block_abb2 = gsub("block", "", block_abb1, ignore.case = TRUE),
                           block_abb = tolower(substr(block_abb2, 1, 20)),
                           ID = paste0(vineyard_abb, "_", block_abb, "_", tolower(Variety)),
                           Lat_DD,
                           Long_DD)

#str_length("Bullpaddock z1")

#Now I have my data in decimal degrees I want to convert it into GDA



mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(white_haven_GPS_DD1)

white_haven_GPS_DD1<- select(white_haven_GPS_DD1, ID, Lat_DD, Long_DD)
glimpse(white_haven_GPS_DD1)
#proj4string(test) <- wgs84CRS   # assume input lat and longs are WGS84
coordinates(white_haven_GPS_DD1) <- ~Long_DD+Lat_DD
proj4string(white_haven_GPS_DD1) <- wgs84CRS   # assume input lat and longs are WGS84
white_haven_GPS_DD <- spTransform(white_haven_GPS_DD1, mapCRS)

glimpse(white_haven_GPS_DD)

#coordinates(white_haven_GPS_DD1) #this pulls out the coordinates from my spatial point data frame
#but I want to keep the ID column - in this case the $Vineyards ? not sure how to do this??

white_haven_GPS_DD1_df = as.data.frame(white_haven_GPS_DD) #this has the new coordinates projected !YES!!
glimpse(white_haven_GPS_DD1_df)



#write_csv(white_haven_GPS_DD1_df, "white_haven_GPS_DD1_df.csv") #I have written it to csv and pulled into arcmap to do a check - mostly looks ok.




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



######################################################################################################################
################                         Make DF 2019                                                #################
######################################################################################################################


white_haven_2019 <- mutate(white_haven,
                      vineyard_abb = tolower(substr(white_haven$Vineyard, 1, 10)),
                      block_abb1 = gsub(" ", "", white_haven$Block),
                      block_abb2 = gsub("block", "", block_abb1, ignore.case = TRUE),
                      block_abb = tolower(substr(block_abb2, 1, 20)),
                      ID = paste0(vineyard_abb, "_", block_abb, "_", tolower(Variety)),
                      year = 2019,
                      ID_yr = paste0(ID, "_", year),
                      julian = as.numeric(format(`V2019 Harvest Date`, "%j")),
                      bunch_numb_m = NA)
glimpse(white_haven_2019)

white_haven_2019_1 <- select(white_haven_2019,
                           ID,
                           ID_yr ,
                           year,
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
                               brix, into = c("brix_a", "brix_b", "brix_c", "brix_d" ), 
                               sep = "([\\,\\/])", remove = FALSE)


#Change these new clm into numbers not characters 
white_haven_2019_1$brix_a <- as.double(white_haven_2019_1$brix_a)
white_haven_2019_1$brix_b <- as.double(white_haven_2019_1$brix_b)
white_haven_2019_1$brix_c <- as.double(white_haven_2019_1$brix_c)
white_haven_2019_1$brix_d <- as.double(white_haven_2019_1$brix_d)
#create a new clm for average
white_haven_2019_1$brix_av <- rowMeans(select(white_haven_2019_1, brix_a, brix_b, brix_c, brix_d), na.rm = TRUE)


#change the NaN to NA and report as number not character
white_haven_2019_1$brix_av <- as.double(gsub("NaN", "NA", white_haven_2019_1$brix_av)) 
#rename the 0 value to NA
white_haven_2019_1$brix_av[white_haven_2019_1$brix_av == 0] <- NA

glimpse(white_haven_2019_1)

####add in the GPS coords####
glimpse(white_haven_2019_1)
glimpse(white_haven_GPS_DD1_df) #something wrong here??



white_haven_2019_GPS <- left_join(white_haven_2019_1,white_haven_GPS_DD1_df )
glimpse(white_haven_2019_GPS)
#white_haven_2019_GPS_anti <- anti_join(white_haven_2019_1,white_haven_GPS_DD1_df )
#print(white_haven_2019_GPS_anti)
#print(white_haven_2019_GPS)

######################################################################################################################
################                         Make DF 2018                                                #################
######################################################################################################################
glimpse(white_haven)

#2018 is missing some data that 2019 has

white_haven_2018 <- mutate(white_haven,
                           vineyard_abb = tolower(substr(white_haven$Vineyard, 1, 10)),
                           block_abb1 = gsub(" ", "", white_haven$Block),
                           block_abb2 = gsub("block", "", block_abb1, ignore.case = TRUE),
                           block_abb = tolower(substr(block_abb2, 1, 20)),
                           ID = paste0(vineyard_abb, "_", block_abb, "_", tolower(Variety)),
                           year = 2018,
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
                             year,
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
                               brix, into = c("brix_a", "brix_b", "brix_c", "brix_d" ), 
                               sep = "([\\,\\/])", remove = FALSE)


#Change these new clm into numbers not characters 
white_haven_2018_1$brix_a <- as.double(white_haven_2018_1$brix_a)
white_haven_2018_1$brix_b <- as.double(white_haven_2018_1$brix_b)
white_haven_2018_1$brix_c <- as.double(white_haven_2018_1$brix_c)
white_haven_2018_1$brix_d <- as.double(white_haven_2018_1$brix_d)
#create a new clm for average
white_haven_2018_1$brix_av <- rowMeans(select(white_haven_2018_1, brix_a, brix_b, brix_c, brix_d), na.rm = TRUE)

#change the NaN to NA and report as number not character
white_haven_2018_1$brix_av <- as.double(gsub("NaN", "NA", white_haven_2018_1$brix_av)) 
#rename the 0 value to NA
white_haven_2018_1$brix_av[white_haven_2018_1$brix_av == 0] <- NA

glimpse(white_haven_2018_1)

####add in the GPS coords####
glimpse(white_haven_2018_1)
glimpse(white_haven_GPS_DD1_df)

white_haven_2018_GPS <- left_join(white_haven_2018_1,white_haven_GPS_DD1_df )

######################################################################################################################
################                         Make DF 2017                                                #################
######################################################################################################################
white_haven_2017 <- mutate(white_haven,
                           vineyard_abb = tolower(substr(white_haven$Vineyard, 1, 10)),
                           block_abb1 = gsub(" ", "", white_haven$Block),
                           block_abb2 = gsub("block", "", block_abb1, ignore.case = TRUE),
                           block_abb = tolower(substr(block_abb2, 1, 20)),
                           ID = paste0(vineyard_abb, "_", block_abb, "_", tolower(Variety)),
                           year = 2017,
                           ID_yr = paste0(ID, "_", year),
                           harvest_date =  `V2017 Harvest Date`,
                           julian = as.numeric(format(harvest_date, "%j")),
                           bunch_numb_m = NA,
                           bunch_weight = NA,
                           berry_weight = NA)
glimpse(white_haven_2017)

white_haven_2017_1 <- select(white_haven_2017,
                             ID,
                             ID_yr ,
                             year,
                             Vineyard,
                             Block,
                             Variety,
                             harvest_date,
                             julian ,
                             yield_t_ha =   `V2017 t/ha`,
                             yield_kg_m =   `V2017 kg/m`,
                             brix =         `V2017 Brix`,
                             bunch_weight , #no data
                             berry_weight , #no data
                             bunch_numb_m,  # no data
                             pruning_style = `V2017 Cane #`, #check this
                             row_width =`Row Spacing`,
                             vine_spacing = `Vine Spacing`)

glimpse(white_haven_2017_1)

#some brix have 0 values that need to be replaced with NA 
#some brix have multiple readings eg 22.4,22.5 this needs to be averaged
#split brix clm into multiples
white_haven_2017_1 <- separate(white_haven_2017_1, 
                               brix, into = c("brix_a", "brix_b", "brix_c", "brix_d" ), 
                               sep = "([\\,\\/])", remove = FALSE)


#Change these new clm into numbers not characters 
white_haven_2017_1$brix_a <- as.double(white_haven_2017_1$brix_a)
white_haven_2017_1$brix_b <- as.double(white_haven_2017_1$brix_b)
white_haven_2017_1$brix_c <- as.double(white_haven_2017_1$brix_c)
white_haven_2017_1$brix_d <- as.double(white_haven_2017_1$brix_d)
#create a new clm for average
white_haven_2017_1$brix_av <- rowMeans(select(white_haven_2017_1, brix_a, brix_b, brix_c, brix_d), na.rm = TRUE)

#change the NaN to NA and report as number not character
white_haven_2017_1$brix_av <- as.double(gsub("NaN", "NA", white_haven_2017_1$brix_av)) 
#rename the 0 value to NA
white_haven_2017_1$brix_av[white_haven_2017_1$brix_av == 0] <- NA

glimpse(white_haven_2017_1)

####add in the GPS coords####
glimpse(white_haven_2017_1)
glimpse(white_haven_GPS_DD1_df)

white_haven_2017_GPS <- left_join(white_haven_2017_1,white_haven_GPS_DD1_df )


######################################################################################################################
################                         Make DF 2016                                                #################
######################################################################################################################
white_haven_2016 <- mutate(white_haven,
                           vineyard_abb = tolower(substr(white_haven$Vineyard, 1, 10)),
                           block_abb1 = gsub(" ", "", white_haven$Block),
                           block_abb2 = gsub("block", "", block_abb1, ignore.case = TRUE),
                           block_abb = tolower(substr(block_abb2, 1, 20)),
                           ID = paste0(vineyard_abb, "_", block_abb, "_", tolower(Variety)),
                           year = 2016,
                           ID_yr = paste0(ID, "_", year),
                           harvest_date =  `V2016 Harvest Date`,
                           julian = as.numeric(format(harvest_date, "%j")),
                           bunch_numb_m = NA,
                           bunch_weight = NA,
                           berry_weight = NA,
                           pruning_style = NA)
glimpse(white_haven_2016)

white_haven_2016_1 <- select(white_haven_2016,
                             ID,
                             ID_yr ,
                             year,
                             Vineyard,
                             Block,
                             Variety,
                             harvest_date,
                             julian ,
                             yield_t_ha =   `V2016 t/ha`,
                             yield_kg_m =   `V2016 kg/m`,
                             brix =         `V2016 Brix`,
                             bunch_weight , #no data
                             berry_weight , #no data
                             bunch_numb_m,  # no data
                             pruning_style, #no data
                             row_width =`Row Spacing`,
                             vine_spacing = `Vine Spacing`)
                             #`V2016 Thinning Plan` not used

glimpse(white_haven_2016_1)

#some brix have 0 values that need to be replaced with NA 
#some brix have multiple readings eg 22.4/22.5 eg 22.4,22.5this needs to be averaged
#split brix clm into multiples
white_haven_2016_1 <- separate(white_haven_2016_1, 
                               brix, into = c("brix_a", "brix_b", "brix_c", "brix_d" ), 
                               sep = "([\\,\\/])", remove = FALSE)

#Change these new clm into numbers not characters 
white_haven_2016_1$brix_a <- as.double(white_haven_2016_1$brix_a)
white_haven_2016_1$brix_b <- as.double(white_haven_2016_1$brix_b)
white_haven_2016_1$brix_c <- as.double(white_haven_2016_1$brix_c)
white_haven_2016_1$brix_d <- as.double(white_haven_2016_1$brix_d)
#create a new clm for average
white_haven_2016_1$brix_av <- rowMeans(select(white_haven_2016_1, brix_a, brix_b, brix_c, brix_d), na.rm = TRUE)
#change the NaN to NA and report as number not character
white_haven_2016_1$brix_av <- as.double(gsub("NaN", "NA", white_haven_2016_1$brix_av)) 
#rename the 0 value to NA
white_haven_2016_1$brix_av[white_haven_2016_1$brix_av == 0] <- NA

glimpse(white_haven_2016_1)

####add in the GPS coords####
glimpse(white_haven_2016_1)
glimpse(white_haven_GPS_DD1_df)

white_haven_2016_GPS <- left_join(white_haven_2016_1,white_haven_GPS_DD1_df )


######################################################################################################################
################                         Make DF 2015                                                #################
######################################################################################################################
white_haven_2015 <- mutate(white_haven,
                           vineyard_abb = tolower(substr(white_haven$Vineyard, 1, 10)),
                           block_abb1 = gsub(" ", "", white_haven$Block),
                           block_abb2 = gsub("block", "", block_abb1, ignore.case = TRUE),
                           block_abb = tolower(substr(block_abb2, 1, 20)),
                           ID = paste0(vineyard_abb, "_", block_abb, "_", tolower(Variety)),
                           year = 2015,
                           ID_yr = paste0(ID, "_", year),
                           harvest_date =  `V2015 Harvest Date`,
                           julian = as.numeric(format(harvest_date, "%j")),
                           bunch_numb_m = NA,
                           bunch_weight = NA,
                           berry_weight = NA,
                           pruning_style = NA)
glimpse(white_haven_2015)

white_haven_2015_1 <- select(white_haven_2015,
                             ID,
                             ID_yr ,
                             year,
                             Vineyard,
                             Block,
                             Variety,
                             harvest_date,
                             julian ,
                             yield_t_ha =   `V2015 t/ha`,
                             yield_kg_m =   `V2015 kg/m`,
                             brix =         `V2015 Brix`,
                             bunch_weight , #no data
                             berry_weight , #no data
                             bunch_numb_m,  # no data
                             pruning_style, #no data
                             row_width =`Row Spacing`,
                             vine_spacing = `Vine Spacing`)


glimpse(white_haven_2015_1)

#some brix have 0 values that need to be replaced with NA 
#some brix have multiple readings eg 22.4/22.5 this needs to be averaged
#split brix clm into multiples
white_haven_2015_1 <- separate(white_haven_2015_1, 
                               brix, into = c("brix_a", "brix_b", "brix_c", "brix_d" ), 
                               sep = "([\\,\\/])", remove = FALSE)


#Change these new clm into numbers not characters 
white_haven_2015_1$brix_a <- as.double(white_haven_2015_1$brix_a)
white_haven_2015_1$brix_b <- as.double(white_haven_2015_1$brix_b)
white_haven_2015_1$brix_c <- as.double(white_haven_2015_1$brix_c)
white_haven_2015_1$brix_d <- as.double(white_haven_2015_1$brix_d)
#create a new clm for average
white_haven_2015_1$brix_av <- rowMeans(select(white_haven_2015_1, brix_a, brix_b, brix_c, brix_d), na.rm = TRUE)


#change the NaN to NA and report as number not character
white_haven_2015_1$brix_av <- as.double(gsub("NaN", "NA", white_haven_2015_1$brix_av)) 
#rename the 0 value to NA
white_haven_2015_1$brix_av[white_haven_2015_1$brix_av == 0] <- NA

glimpse(white_haven_2015_1)

####add in the GPS coords####
glimpse(white_haven_2015_1)
glimpse(white_haven_GPS_DD1_df)

white_haven_2015_GPS <- left_join(white_haven_2015_1,white_haven_GPS_DD1_df )


######################################################################################################################
################                         Make DF 2014                                                #################
######################################################################################################################
white_haven_2014 <- mutate(white_haven,
                           vineyard_abb = tolower(substr(white_haven$Vineyard, 1, 10)),
                           block_abb1 = gsub(" ", "", white_haven$Block),
                           block_abb2 = gsub("block", "", block_abb1, ignore.case = TRUE),
                           block_abb = tolower(substr(block_abb2, 1, 20)),
                           ID = paste0(vineyard_abb, "_", block_abb, "_", tolower(Variety)),
                           year = 2014,
                           ID_yr = paste0(ID, "_", year),
                           harvest_date =  `V2014 Harvest Date`,
                           julian = as.numeric(format(harvest_date, "%j")),
                           bunch_numb_m = NA,
                           bunch_weight = NA,
                           berry_weight = NA,
                           pruning_style = NA)
glimpse(white_haven_2014)

white_haven_2014_1 <- select(white_haven_2014,
                             ID,
                             ID_yr ,
                             year,
                             Vineyard,
                             Block,
                             Variety,
                             harvest_date,
                             julian ,
                             yield_t_ha =   `V2014 t/ha`,
                             yield_kg_m =   `V2014 kg/m`,
                             brix =         `V2014 Brix`  ,
                             bunch_weight , #no data
                             berry_weight , #no data
                             bunch_numb_m,  # no data
                             pruning_style, #no data
                             row_width =`Row Spacing`,
                             vine_spacing = `Vine Spacing`)


glimpse(white_haven_2014_1)

#some brix have 0 values that need to be replaced with NA 
#some brix have multiple readings eg 22.4/22.5 this needs to be averaged
#split brix clm into multiples
white_haven_2014_1 <- separate(white_haven_2014_1, 
                               brix, into = c("brix_a", "brix_b", "brix_c", "brix_d" ), 
                               sep = "([\\,\\/])", remove = FALSE)


#Change these new clm into numbers not characters 
white_haven_2014_1$brix_a <- as.double(white_haven_2014_1$brix_a)
white_haven_2014_1$brix_b <- as.double(white_haven_2014_1$brix_b)
white_haven_2014_1$brix_c <- as.double(white_haven_2014_1$brix_c)
white_haven_2014_1$brix_d <- as.double(white_haven_2014_1$brix_d)
#create a new clm for average
white_haven_2014_1$brix_av <- rowMeans(select(white_haven_2014_1, brix_a, brix_b, brix_c, brix_d), na.rm = TRUE)
#change the NaN to NA and report as number not character
white_haven_2014_1$brix_av <- as.double(gsub("NaN", "NA", white_haven_2014_1$brix_av)) 
#rename the 0 value to NA
white_haven_2014_1$brix_av[white_haven_2014_1$brix_av == 0] <- NA

glimpse(white_haven_2014_1)

####add in the GPS coords####
glimpse(white_haven_2014_1)
glimpse(white_haven_GPS_DD1_df)

white_haven_2014_GPS <- left_join(white_haven_2014_1,white_haven_GPS_DD1_df )


######################################################################################################################
################                         Make DF 2019 -2014                                                #################
######################################################################################################################

glimpse(white_haven_2014_GPS)
glimpse(white_haven_2015_GPS)
glimpse(white_haven_2016_GPS)
glimpse(white_haven_2017_GPS)
glimpse(white_haven_2018_GPS)
glimpse(white_haven_2019_GPS)

white_haven_2019to2014_GPS <- rbind(white_haven_2014_GPS,
                                    white_haven_2015_GPS,
                                    white_haven_2016_GPS,
                                    white_haven_2017_GPS,
                                    white_haven_2018_GPS,
                                    white_haven_2019_GPS)

glimpse(white_haven_2019to2014_GPS)

white_haven_2019to2014_GPS <- select(white_haven_2019to2014_GPS,
                                     ID,
                                     ID_yr,
                                     year,
                                     Vineyard,
                                     Block,
                                     Variety,
                                     harvest_date,
                                     julian,
                                     yield_t_ha,
                                     yield_kg_m,
                                     bunch_weight,
                                     berry_weight,
                                     bunch_numb_m,
                                     pruning_style,
                                     row_width,
                                     vine_spacing,
                                     brix = brix_av,
                                     y = Lat_DD,
                                     x = Long_DD)

white_haven_2019to2014_GPS$na_count <- apply(is.na(white_haven_2019to2014_GPS), 1, sum)

write_csv(white_haven_2019to2014_GPS, "white_haven_2019to2014_GPS.csv")
