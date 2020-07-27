library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)
library(stringr)
library(tidyr)

Weta_2011_2020 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Mark_Allen/Weta Estate Yield Comparison 2011 onwards with coords Rob.xlsx"    
                                      , sheet = "Sheet1", skip = 0,
                             col_types = c("text", "text", "text", 
                                           "text", "text", "text", "text", "text", 
                                           "date", "text", "text", "text", "date", 
                                           "text", "text", "text", "date", "text", 
                                           "text", "text", "date", "text", "text", 
                                           "text", "date", "text", "text", "text", 
                                           "date", "text", "text", "text", "date", 
                                           "text", "text", "text", "date", "text", 
                                           "text", "text", "date", "text", "text", 
                                           "text", "date"))
names(Weta_2011_2020)




###################################################################################################################
### GPS data only
GPS_only <- dplyr::select(Weta_2011_2020,"...1", "...2", "...3", "...4", "...5"
                          )
GPS_only <- fill(GPS_only, "...2", "...3", "...4", "...5", .direction = "down")
heading_GPS_only  <-  GPS_only %>% slice(3)
#remove rows 1 and 2
GPS_only <-  GPS_only %>% slice(4:15) #slice(4:n())

#assign names to heading
colnames(GPS_only) <- heading_GPS_only
names(GPS_only)
#fix up first heading
names(GPS_only)[names(GPS_only) == "Sau Blanc"] <- "Blocks"

str(GPS_only)
#change data type
GPS_only$LAT <- as.double(GPS_only$LAT)
GPS_only$LON <- as.double(GPS_only$LON)
GPS_only$`Row spacing` <- as.double(GPS_only$`Row spacing`)
GPS_only$`Vine spacing` <- as.double(GPS_only$`Vine spacing`)

#remove the df I don't need anymore
rm(heading_GPS_only)







### Yield Data for each all year

yld_data <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Mark_Allen/Weta Estate Yield Comparison 2011 onwards with coords Rob.xlsx"    
                             , sheet = "Sheet1", skip = 0,
                             col_types = c("text", "text", "text", 
                                           "text", "text", 
                                           "numeric", "numeric", "numeric", "date", 
                                           "numeric", "numeric", "numeric", "date", 
                                           "numeric", "numeric", "numeric", "date", 
                                           "numeric", "numeric", "numeric", "date", 
                                           "numeric", "numeric", "numeric", "date", 
                                           "numeric", "numeric", "numeric", "date", 
                                           "numeric", "numeric", "numeric", "date", 
                                           "numeric", "numeric", "numeric", "date", 
                                           "numeric", "numeric", "numeric", "date", 
                                           "numeric", "numeric", "numeric", "date"))




names(yld_data)
yld_data <- dplyr::select(yld_data,"...1","...6": "...45")


#remove rows 1 and 2
yld_data <-  yld_data %>% slice(1:15) #slice(4:n())


#### for each year
# 2011
yld_data_2011 <- dplyr::select(yld_data,"...1","...6": "...9") %>% 
  mutate(year = "2011")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
names(yld_data_2011)
yld_data_2011 <- yld_data_2011 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...6",
    "Harvest_t/ha" = "...7",
    "Bunch_Wghts" =  "...8",
    "Harvest_Date" = "...9"
  )

yld_data_2011 <-  yld_data_2011 %>% slice(4:n()) 

### 2012
yld_data_2012 <- dplyr::select(yld_data,"...1","...10": "...13") %>% 
  mutate(year = "2012")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
yld_data_2012 <- yld_data_2012 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...10",
    "Harvest_t/ha" = "...11",
    "Bunch_Wghts" =  "...12",
    "Harvest_Date" = "...13"
  )

yld_data_2012 <-  yld_data_2012 %>% slice(4:n()) 


### 2013
yld_data_2013 <- dplyr::select(yld_data,"...1","...14": "...17") %>% 
  mutate(year = "2013")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
yld_data_2013 <- yld_data_2013 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...14",
    "Harvest_t/ha" = "...15",
    "Bunch_Wghts" =  "...16",
    "Harvest_Date" = "...17"
  )
yld_data_2013 <-  yld_data_2013 %>% slice(4:n()) 

### 2014
yld_data_2014 <- dplyr::select(yld_data,"...1","...18": "...21") %>% 
  mutate(year = "2014")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
yld_data_2014 <- yld_data_2014 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...18",
    "Harvest_t/ha" = "...19",
    "Bunch_Wghts" =  "...20",
    "Harvest_Date" = "...21"
  )
yld_data_2014 <-  yld_data_2014 %>% slice(4:n()) 

### 2015
yld_data_2015 <- dplyr::select(yld_data,"...1","...22": "...25") %>% 
  mutate(year = "2015")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
yld_data_2015 <- yld_data_2015 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...22",
    "Harvest_t/ha" = "...23",
    "Bunch_Wghts" =  "...24",
    "Harvest_Date" = "...25"
  )
yld_data_2015 <-  yld_data_2015 %>% slice(4:n()) 

### 2016
yld_data_2016 <- dplyr::select(yld_data,"...1","...26": "...29") %>% 
  mutate(year = "2016")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
yld_data_2016 <- yld_data_2016 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...26",
    "Harvest_t/ha" = "...27",
    "Bunch_Wghts" =  "...28",
    "Harvest_Date" = "...29"
  )
yld_data_2016 <-  yld_data_2016 %>% slice(4:n()) 

### 2017
yld_data_2017 <- dplyr::select(yld_data,"...1","...30": "...33") %>% 
  mutate(year = "2017")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
yld_data_2017 <- yld_data_2017 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...30",
    "Harvest_t/ha" = "...31",
    "Bunch_Wghts" =  "...32",
    "Harvest_Date" = "...33"
  )
yld_data_2017 <-  yld_data_2017 %>% slice(4:n()) 

### 2018
yld_data_2018 <- dplyr::select(yld_data,"...1","...34": "...37") %>% 
  mutate(year = "2018")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
yld_data_2018 <- yld_data_2018 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...34",
    "Harvest_t/ha" = "...35",
    "Bunch_Wghts" =  "...36",
    "Harvest_Date" = "...37"
  )
yld_data_2018 <-  yld_data_2018 %>% slice(4:n()) 

### 2019
yld_data_2019 <- dplyr::select(yld_data,"...1","...38": "...41") %>% 
  mutate(year = "2019")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
yld_data_2019 <- yld_data_2019 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...38",
    "Harvest_t/ha" = "...39",
    "Bunch_Wghts" =  "...40",
    "Harvest_Date" = "...41"
  )
yld_data_2019 <-  yld_data_2019 %>% slice(4:n()) 

### 2020
yld_data_2020 <- dplyr::select(yld_data,"...1","...42": "...45") %>% 
  mutate(year = "2020")
#Assign the names to clm and make a row called year
#rename(new variable name = existing variable name)
yld_data_2020 <- yld_data_2020 %>%
  rename(
    Blocks = "...1",
    "Floret_Count" = "...42",
    "Harvest_t/ha" = "...43",
    "Bunch_Wghts" =  "...44",
    "Harvest_Date" = "...45"
  )
yld_data_2020 <-  yld_data_2020 %>% slice(4:n()) 


####This makes all the years of data tidy so we can join them.
yld_data_tidy <- bind_rows(yld_data_2011,
                           yld_data_2012,
                           yld_data_2013,
                           yld_data_2014,
                           yld_data_2015,
                           yld_data_2016,
                           yld_data_2017,
                           yld_data_2018,
                           yld_data_2019,
                           yld_data_2020
                           )
### Add in the GPS points

yld_data_tidy_GPS <- full_join(yld_data_tidy, GPS_only)
rm(yld_data_2011,
   yld_data_2012,
   yld_data_2013,
   yld_data_2014,
   yld_data_2015,
   yld_data_2016,
   yld_data_2017,
   yld_data_2018,
   yld_data_2019,
   yld_data_2020,
   yld_data,
   GPS_only,
   Weta_2011_2020
   )

#################################################################################################################
## This is the tidy data for Weta a starting point for extra  calulations

names(yld_data_tidy_GPS)

weta_yld_data <- dplyr::select(
  yld_data_tidy_GPS,
  Blocks,
  row_width =  "Row spacing" ,
  vine_spacing = "Vine spacing" ,
  POINT_X = "LAT",
  POINT_Y = "LON",
  yield_t_ha = "Harvest_t/ha",
  harvest_date = Harvest_Date,
  bunch_weight = "Bunch_Wghts",
  year
)


#### Add in the calulations
names(weta_yld_data)
weta_yld_data <- mutate(
  weta_yld_data,
  harvest_date ,
  julian = as.numeric(format(harvest_date, "%j")),
  m_ha_vine = 10000 / row_width,
  yield_kg_m = (yield_t_ha * 1000) / m_ha_vine,
  bunch_weight,
  berry_weight = NA,
  bunch_numb_m = NA,
  bunches_per_vine = NA,
  pruning_style = NA,
  brix = NA,
  meter_row_per_ha = 10000 / row_width,
  yld_per_m_row_kg = (yield_t_ha * 1000) / 10000 /
    row_width,
  bunch_m = (yld_per_m_row_kg * 1000) / bunch_weight,
  variety = "SB",
  company = "Weta",
  x_coord = POINT_X,
  y_coord = POINT_Y ,
  ID_yr = paste0("Weta_", Blocks, "_", year))
  

names(weta_yld_data)
weta_yld_data <- select(
  weta_yld_data,
    company,
    ID_yr,
    year, 
    variety,
    x_coord ,
    y_coord  ,
    year,
    harvest_date,
    julian,
    bunch_weight,
    yield_t_ha,
    yield_kg_m,
    brix, #missing
    bunch_m,
    pruning_style,
    row_width ,
    vine_spacing,
    Block = "Blocks"
  )
 
names(weta_yld_data)

mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(weta_yld_data)

weta_yld_data_DD1<- select(weta_yld_data, ID_yr , x_coord,  y_coord )
glimpse(weta_yld_data_DD1)
#proj4string(test) <- wgs84CRS   # assume input lat and longs are WGS84
coordinates(weta_yld_data_DD1) <- ~y_coord+x_coord
proj4string(weta_yld_data_DD1) <- wgs84CRS   # assume input lat and longs are WGS84
weta_yld_data_DD <- spTransform(weta_yld_data_DD1, mapCRS)

glimpse(weta_yld_data_DD)
weta_yld_data_DD_df = as.data.frame(weta_yld_data_DD) #this has the new coordinates projected !YES!!
glimpse(weta_yld_data_DD_df)

##put in back into the data..
names(weta_yld_data)
#drop the old long and lats
weta_yld_data <- dplyr::select(weta_yld_data,
                               -"x_coord" ,- "y_coord" )

weta_yld_data <- full_join(weta_yld_data , weta_yld_data_DD_df)
#I think the x and y got mixed up
weta_yld_data <- rename(weta_yld_data, y_coord_new=x_coord,
       x_coord_new=y_coord)
names(weta_yld_data)
weta_yld_data <- rename(weta_yld_data, 
                        y_coord=y_coord_new,
                        x_coord=x_coord_new)

write_csv(weta_yld_data, "V:/Marlborough regional/working_jaxs/July2020/weta_yld_data.csv")
