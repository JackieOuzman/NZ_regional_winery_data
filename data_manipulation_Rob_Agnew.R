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




GPS_Pts_Rob_Agnew <-   read_excel("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/Regional winery data/Raw_data/Rob Agnew data/Updated Rob Agnew All Marlborough Sav Blanc _Yield_data for GYA Project mct_v2.xlsx",
                         "Final_Dataset_Marlborough_Sauvi",  range = "A1:I1142"   )


str(GPS_Pts_Rob_Agnew)
GPS_Pts_Rob_Agnew <- select(GPS_Pts_Rob_Agnew,
                            vineyard =phenology.vineyard.name,
                            winery,
                            latitude,
                            longitude
                             )  # new name = old name
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
print(GPS_Pts_Rob_Agnew$Long_DD)



glimpse(GPS_Pts_Rob_Agnew)

mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(GPS_Pts_Rob_Agnew)

GPS_Pts_Rob_Agnew_DD1<- select(GPS_Pts_Rob_Agnew, vineyard , winery, Lat_DD, Long_DD)
glimpse(GPS_Pts_Rob_Agnew_DD1)
#proj4string(test) <- wgs84CRS   # assume input lat and longs are WGS84
coordinates(GPS_Pts_Rob_Agnew_DD1) <- ~Long_DD+Lat_DD
proj4string(GPS_Pts_Rob_Agnew_DD1) <- wgs84CRS   # assume input lat and longs are WGS84
GPS_Pts_Rob_Agnew_DD <- spTransform(GPS_Pts_Rob_Agnew_DD1, mapCRS)

glimpse(GPS_Pts_Rob_Agnew_DD)
GPS_Pts_Rob_Agnew_DD_df = as.data.frame(GPS_Pts_Rob_Agnew_DD) #this has the new coordinates projected !YES!!
glimpse(GPS_Pts_Rob_Agnew_DD_df)

#write_csv(GPS_Pts_Rob_Agnew_DD_df, "V:/Marlborough regional/working_jaxs/GPS_Pts_Rob_Agnew_check.csv")


rm(list = c( "wgs84CRS", "mapCRS", "GPS_Pts_Rob_Agnew", "GPS_Pts_Rob_Agnew_DD1"))
#########################################################################################################

Yld_Rob_Agnew <-   read_excel("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/Regional winery data/Raw_data/Rob Agnew data/Updated Rob Agnew All Marlborough Sav Blanc _Yield_data for GYA Project mct_v2.xlsx",
                                  "Final_Dataset_Marlborough_Sauvi"   )
str(Yld_Rob_Agnew)
Yld_Rob_Agnew <- select(
  Yld_Rob_Agnew,
  company = winery,
  vineyard = phenology.vineyard.name,
  variety = variety.name,
  year,
  yield_t_ha = `standard.yield T/ha. With density 2315 vines/ha`,
  bunch_weight = `calculated.good.bunch.weight.g`,
  berry_weight = `mean.berry.weight.harvest.sample.g`,
  vineyard = phenology.vineyard.name,
  bunch_per_vine = calculated.bunches.per.vine,
  month,
  day
)

###make harvest date from day month year
#as.Date(paste(dt$mon,dt$day,dt$year,sep="-"),format = "%m-%d-%Y")
Yld_Rob_Agnew$date <- with(Yld_Rob_Agnew, ymd(sprintf('%04d%02d%02d', year, month, day)))
str(Yld_Rob_Agnew$date)



##If in the missing company data with the values above.
Yld_Rob_Agnew <- fill(Yld_Rob_Agnew,company )
## add in row and vine spacing from Rob email 13/1/2019
unique(Yld_Rob_Agnew$company)
Yld_Rob_Agnew <- mutate(Yld_Rob_Agnew, 
                        row_width = case_when(
                    company == "Oyster Bay/ Delegat" ~ 2.5,
                    company != "Oyster Bay/ Delegat" ~ 2.4),
                    
                    vine_spacing = 1.8
)                       
                        
str(Yld_Rob_Agnew)
Yld_Rob_Agnew <- mutate(Yld_Rob_Agnew,yield_t_ha = as.double(yield_t_ha))
Yld_Rob_Agnew <- mutate(Yld_Rob_Agnew,bunch_weight = as.double(bunch_weight))
Yld_Rob_Agnew <- mutate(Yld_Rob_Agnew,berry_weight = as.double(berry_weight))
Yld_Rob_Agnew <- mutate(Yld_Rob_Agnew,bunch_per_vine = as.double(bunch_per_vine))

str(Yld_Rob_Agnew)

Yld_Rob_Agnew <- mutate(
  Yld_Rob_Agnew,       
ID_yr = paste0(vineyard, "_", date, "_", year), 
ID_harvest_date = paste0(vineyard, "_", date), 
yield_kg_m = (yield_t_ha * 1000) / (10000/row_width), #check this cal
bunch_numb_m = bunch_per_vine / vine_spacing,
brix = NA,
pruning_style = NA,
harvest_date = date,
julian = as.numeric(format(harvest_date, "%j")),
variety = "Sauvignon_blanc")

unique(Yld_Rob_Agnew$year)
Yld_Rob_Agnew <- filter(Yld_Rob_Agnew, year >0)

###ADD in GPS
str(Yld_Rob_Agnew)
str(GPS_Pts_Rob_Agnew_DD_df)

Yld_GPS_Rob_Agnew <- full_join(Yld_Rob_Agnew, GPS_Pts_Rob_Agnew_DD_df)

###################### seems that we have multiple entries for the same site.
## I think we need to average sites with the same harvest dates.
#df %>% group_by(grp) %>% summarise_all(funs(mean))

str(Yld_GPS_Rob_Agnew)
dim(Yld_GPS_Rob_Agnew)
Yld_GPS_Rob_Agnew <- group_by(Yld_GPS_Rob_Agnew, ID_harvest_date, vineyard, variety, winery, company, ID_yr ) %>% 
  summarise_all(funs(mean), na.rm = TRUE) 

Yld_GPS_Rob_Agnew  <- ungroup(Yld_GPS_Rob_Agnew)

### note that there are multiple sites which are alreday in database.
str(Yld_GPS_Rob_Agnew)
unique(Yld_GPS_Rob_Agnew$vineyard)

# Booker - #already got MV9_SBLK - some missing some data for berry weight all years but not 2016 2017. Bunchnumb_m all yrs
# McKean
# OYB
# Rarangi #already got NRARSBC 
# Rowley
# Seaview #already got M10SBLF missing berry wt and bunch numb m 
# Seddon # not GPS pts
# Squire #already got MV7_SBLD missing berry wt (got 2011 and 2017) and bunch numb m
# Tohu
# Villa #already got MMBWSB missing harvest date 2018 missing yld for all, brix 2018, bunch wt 2018,17,15,13. Missing berry wt for all except 2016 and 2012. missing all bunch _m


#safe to use...
Yld_GPS_Rob_Agnew <- filter(
  Yld_GPS_Rob_Agnew,
  vineyard == "McKean" |
    vineyard == "OYB" |
    vineyard == "Rowley" |
    vineyard == "Tohu"
)
                 
 
  
 
 
 
 
### Select clm I want
str(Yld_GPS_Rob_Agnew)
Yld_GPS_Rob_Agnew <- select(Yld_GPS_Rob_Agnew,
company,
ID_yr,
variety,
x_coord = Long_DD,
y_coord = Lat_DD,
year,
harvest_date,
julian,
yield_t_ha,
yield_kg_m,
brix,
bunch_weight,
berry_weight,
bunch_numb_m,
pruning_style,
row_width,
vine_spacing)

dim(Yld_GPS_Rob_Agnew)
Yld_GPS_Rob_Agnew$na_count <- apply(is.na(Yld_GPS_Rob_Agnew), 1, sum)

dim(Yld_GPS_Rob_Agnew)

##########################################################################


#how many entries with and without GPS for all years
dim(Yld_GPS_Rob_Agnew)#29
unique(Yld_GPS_Rob_Agnew$variety)
str(Yld_GPS_Rob_Agnew)


dim(Yld_GPS_Rob_Agnew)#29

#how many entries with GPS for all years



Yld_GPS_Rob_Agnew_GPS_SAB <- filter(Yld_GPS_Rob_Agnew,  x_coord       >0)
dim(Yld_GPS_Rob_Agnew_GPS_SAB) # Seddon is missing coordinates from Pernod Ricard
#how many site are SAU?
unique(Yld_GPS_Rob_Agnew_GPS_SAB$variety)



####################################################################################################
############################################################################# 
########################    File to use   ####################################

write_csv(Yld_GPS_Rob_Agnew_GPS_SAB, "V:/Marlborough regional/working_jaxs/July2020/Yld_GPS_Rob_Agnew_GPS_SAB_select_sites.csv")
############################################################################## 



#Revised  set 21/0/2021
names(Yld_GPS_Rob_Agnew_GPS_SAB)

#just need to make a block 
Yld_GPS_Rob_Agnew_GPS_SAB <- Yld_GPS_Rob_Agnew_GPS_SAB %>% separate(ID_yr, c("Block"), sep = "_", remove = FALSE)

#1. How many sites?
#for each year
Yld_GPS_Rob_Agnew_GPS_SAB %>%
  group_by(year) %>%
  summarise(count = n_distinct(Block))
#overall for the data set from 2014-2019 how many blocks do we have?
Yld_GPS_Rob_Agnew_GPS_SAB %>%
  summarise(count = n_distinct(Block))

#2. For harvest date how many sites per year?
names(Forrest_08_2020_df)

Yld_GPS_Rob_Agnew_GPS_SAB %>%
  group_by(year) %>%
  summarise(mean_julian_days = mean(julian, na.rm = TRUE),
            min_julian_days = min(julian, na.rm = TRUE),
            max_julian_days = max(julian, na.rm = TRUE),
            sum_na = sum(!is.na(julian)))

#3. For yield kg/m  how many sites per year

Yld_GPS_Rob_Agnew_GPS_SAB %>%
  group_by(year) %>%
  summarise(mean_yield_kg_m = mean(yield_kg_m, na.rm = TRUE),
            min_yield_kg_m = min(yield_kg_m, na.rm = TRUE),
            max_yield_kg_m = max(yield_kg_m, na.rm = TRUE),
            sum_na = sum(!is.na(yield_kg_m)))
