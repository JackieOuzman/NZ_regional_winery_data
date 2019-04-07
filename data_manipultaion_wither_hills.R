library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

wither_hills_GPS_temp <- read_excel("C:/Users/ouz001/NZ_work/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                             sheet = "Wither Hills NZ2000")
glimpse(wither_hills_GPS_temp)
wither_hills_GPS <- wither_hills_GPS_temp %>% 
                    select(ID_temp = Name ,
                    x_coord = POINT_X,
                    y_coord = POINT_Y) %>% 
                    mutate(ID_temp = gsub( "-", "_", ID_temp))
glimpse(wither_hills_GPS)
wither_hills_block_info <- read_excel("C:/Users/ouz001/NZ_work/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                                    sheet = "locations" , skip = 3)
glimpse(wither_hills_block_info)
wither_hills_block_info <- wither_hills_block_info %>% 
                            select(
                            ID_temp = X__1, 
                            variety,
                            row_spacing = `row spacing (m)`) %>% 
                            mutate(ID_temp = gsub( "-", "_", ID_temp))
glimpse(wither_hills_block_info)
###Join GPS and block data

wither_hills_GPS_block_info <- left_join(wither_hills_GPS,wither_hills_block_info, by = "ID_temp")
glimpse(wither_hills_GPS_block_info)

#### measures
wither_hills_harvest_details <- read_excel("C:/Users/ouz001/NZ_work/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                                    sheet = "15,16,17,18 ", skip = 3)
glimpse(wither_hills_harvest_details)
#Create an ID clm
wither_hills_harvest_details <- wither_hills_harvest_details %>% 
         mutate(vineyard_lower =str_to_lower(Vineyard, locale = "en"),
         block_lower =str_to_lower(Block, locale = "en"),
         Variety_lower =str_to_lower(Variety, locale = "en"),
         year = gsub( "V|v", "20", wither_hills_harvest_details_test$Vintage),
         ID_temp1 = gsub( " ", "_", wither_hills_harvest_details_test$block_lower),
         ID_temp = paste0(ID_temp1,"_", Variety_lower),
         ID_yr = paste0(ID_temp,"_", year))  
glimpse(wither_hills_harvest_details)

#cals and selected clm - need to double check cals
wither_hills_harvest_details1 <- wither_hills_harvest_details %>% 
  select(ID_yr,ID_temp, year,
         Variety = Variety_lower,
         year,
         brix = `Harvest brix` ,
         harvest_date = Harvest, #something wrong here the input file has this problem
         yield_t_ha = `Actual T/Ha`,
         metres_row_ha =`metres row/ha`,
         vine_spacing = `Vine Spacing` ,
         pruning_style = `Pruning style`, 
         row_width = `Row Width` ,
         bunch_numb_per_vine = `Ave Pre-Harvest Bunch #`,
         bunch_weight = `Ave Pre-Harvest Bunch weight`,
         berry_weight = `Ave Pre-Harvest Berry weight`) %>% 
  mutate(pruning_style = as.double(gsub( " cane", "", pruning_style)),
         yield_kg_m = (yield_t_ha * 1000) / (10000/row_width), #check this cal
         bunch_numb_m = bunch_numb_per_vine / vine_spacing , #check this cal
         bunch_mass_g = 1000 * yield_kg_m /bunch_numb_m, #check this cal
         berry_bunch = bunch_weight / berry_weight,
         berry_wt = bunch_mass_g / berry_bunch)
glimpse(wither_hills_harvest_details1)

#### join the GPS files to the harvest data files
glimpse(wither_hills_GPS_block_info)
glimpse(wither_hills_harvest_details1)
wither_hills_GPS_block_info_harvest <- left_join(wither_hills_GPS_block_info, wither_hills_harvest_details1,
                                                 by= "ID_temp")
glimpse(wither_hills_GPS_block_info_harvest)
