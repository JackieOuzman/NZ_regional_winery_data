
#1. work still to do - this is so messy!
# add the 2018 and 2019 yield data once this is added I should have a list of vineyard location
#2. revist my googling and change the names to what is in my short list.

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

####################################################################################################################
######   Bring in the coodinated that I have got from the pdf maps and street address     ##########################



babich_coordinates <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Babich/google_earth_location/Babich_locations_with_v_spacing_r_width CSIRO Amended 050820.xlsx",
                                sheet =  "Babich_locations_with_v_spacing")

names(babich_coordinates)

babich_coordinates <- filter(babich_coordinates,
                             !is.na(POINT_X...2))



babich_coordinates <- select(babich_coordinates,
                             "ID",
                             "POINT_X...2",
                             "POINT_Y...3",
                             "row_spacing",
                             "vine_spacing",
                             "block" ,
                             "Variety" ,
                            "vineyard code" )


babich_coordinates <- rename(babich_coordinates, 
                             "POINT_X" = "POINT_X...2",
                             "POINT_Y" ="POINT_Y...3",
                             "variety" ="Variety" ,
                             "vineyard_code" = "vineyard code" )


### change the coordinates to from long and lats to projected data.
# I have my data in decimal degrees I want to convert it into GDA
mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

names(babich_coordinates)
coordinates(babich_coordinates) <- ~POINT_X+POINT_Y
proj4string(babich_coordinates) <- wgs84CRS   # assume input lat and longs are WGS84
babich_coordinates1 <- spTransform(babich_coordinates, mapCRS)

glimpse(babich_coordinates1)
babich_coordinates1_df = as.data.frame(babich_coordinates1) #this has the new coordinates projected !YES!!
glimpse(babich_coordinates1_df)

rm(Babich_2015, Babich_2015_SB, babich_coordinates, babich_coordinates1, mapCRS, wgs84CRS)

############################################################################################################
########      Bring in the yield data for multiple years      ############################################

Babich_2014_17 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Babich/revised_data_05082020/Historical Harvest Details V14-V17 CSIRO 050820.xlsx",
                             skip = 2)
                             
names(Babich_2014_17)



Babich_2014_17 <- fill(Babich_2014_17, "Corporate Blocks", .direction = "down")

Babich_2014_17 <- rename(
  Babich_2014_17,
  "vineyard" = "Corporate Blocks",
  "notes" = "...2"   ,
  "blocks" =  "...4"  ,
  "ha" =  "...5"  ,
  "harvest_date_14" = "vintage 14"  ,
  "tonnes_14" = "...7",
  "brix_14" = "...8" ,
  "harvest_date_15" = "vintage 15"  ,
  "tonnes_15" = "...10" ,
  "brix_15" = "...11",
  "harvest_date_16" = "vintage 16",
  "tonnes_16" = "...13" ,
  "brix_16" = "...14" ,
  "harvest_date_17" = "vintage 17"   ,
  "tonnes_17" = "...16"  ,
  "brix_17" = "...17"
)


Babich_2014_17 <- filter(Babich_2014_17,
                             !is.na(blocks))
Babich_2014_17 <- dplyr::select(Babich_2014_17, - "...3"  )
names(Babich_2014_17)
## the vineyard name is not super sensible all the time but the blocks show some promise!

#I think this should be at the start just sussing out what do do
names(Babich_2014_17)


Babich_2014_17<- separate(Babich_2014_17, blocks, 
                into = c("company_grower_code","variety","vineyard_code","block_code","temp5", "temp6", "temp7",
                         "temp8","temp9","temp10","temp11","temp12"), 
                remove = FALSE)

unique(Babich_2014_17$variety)
Babich_2014_17 <- filter(Babich_2014_17, variety == "SAB")

# add a clm with more details about what I grower company is...
Babich_2014_17 <- mutate(Babich_2014_17, 
               grower_name = case_when(
                 company_grower_code == "BA" ~ "babich",
                 company_grower_code == "JC" ~ "james_cameron",
                 company_grower_code == "AC" ~ "angus_cameron",
                 company_grower_code == "RG" ~ "richard_gifford",
                 company_grower_code == "MG" ~ "murray_game",
                 company_grower_code == "MW" ~ "matakana_wines",
                 company_grower_code == "SL" ~ "steven_la_plante",
                 company_grower_code == "MP" ~ "martin_pattie",
                 company_grower_code == "CB" ~ "cable_bay",
                 company_grower_code == "JL" ~ "john_leslie",
                 TRUE ~ company_grower_code))

Babich_2014_17 <- mutate(Babich_2014_17, 
               vineyard_code = case_when(
                 vineyard_code == "CS" ~ "cvv",
                 vineyard_code == "EV" ~ "ecv",
                 vineyard_code == "HW" ~ "hw",
                 vineyard_code == "SR" ~ "sr",
                 vineyard_code == "WD" ~ "wdr",
                 vineyard_code == "TB" ~ "tbv",
                 TRUE ~ tolower(vineyard_code)))

Babich_2014_17 <- mutate(Babich_2014_17, 
                block_code = case_when(
                  block_code == "PEAR" ~ "pear_tree",
                  block_code == "TOI" ~ "toi_toi",
                  block_code == "5" ~ "5_eyes",
                  TRUE ~ block_code))

Babich_2014_17 <- mutate(Babich_2014_17, 
                         block_code = case_when(
                  temp5 == "MAIN" &  block_code == "GULLY" ~ "gully_main",
                  temp5 == "WEST"&  block_code == "GULLY"  ~ "gully_west",
                  temp5 == "WEST"&  block_code == "EAST"  ~ "east_west",
                  TRUE ~ tolower(block_code)))

names(Babich_2014_17)

###################################################################################################

#create a df for each year with the same column names and then join togther
Babich_2014 <- dplyr::select(
  Babich_2014_17,
  "vineyard" ,
  "notes"   ,
  "blocks"   ,
  "ha" ,
  "harvest_date" = "harvest_date_14",
  "tonnes" = "tonnes_14",
  "brix" = "brix_14",
  "grower_name",
  "variety",
  "vineyard_code",
  "block_code"
) %>%
  mutate(year = 2014)

Babich_2015 <- dplyr::select(
  Babich_2014_17,
  "vineyard" ,
  "notes"   ,
  "blocks"   ,
  "ha" ,
  "harvest_date" = "harvest_date_15",
  "tonnes" = "tonnes_15",
  "brix" = "brix_15",
  "grower_name",
  "variety",
  "vineyard_code",
  "block_code"
) %>%
  mutate(year = 2015)

Babich_2016 <- dplyr::select(
  Babich_2014_17,
  "vineyard" ,
  "notes"   ,
  "blocks"   ,
  "ha" ,
  "harvest_date" = "harvest_date_16",
  "tonnes" = "tonnes_16",
  "brix" = "brix_16",
  "grower_name",
  "variety",
  "vineyard_code",
  "block_code"
) %>%
  mutate(year = 2016)

Babich_2017 <- dplyr::select(
  Babich_2014_17,
  "vineyard" ,
  "notes"   ,
  "blocks"   ,
  "ha" ,
  "harvest_date" = "harvest_date_17",
  "tonnes" = "tonnes_17",
  "brix" = "brix_17",
  "grower_name",
  "variety",
  "vineyard_code",
  "block_code"
) %>%
  mutate(year = 2017)

Babich_2014_2017_yld_info <- bind_rows(Babich_2014,
                                       Babich_2015,
                                       Babich_2016,
                                       Babich_2017)

rm(Babich_2014,
   Babich_2015,
   Babich_2016,
   Babich_2017,
   Babich_2014_17)




#####################################################################################################################







