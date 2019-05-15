library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)

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

glimpse(wither_hills_GPS)
glimpse(wither_hills_block_info)

wither_hills_GPS_block_info <- full_join(wither_hills_GPS,wither_hills_block_info, by = "ID_temp")
wither_hills_GPS_block_info_anti_join1 <- anti_join(wither_hills_GPS,wither_hills_block_info, by = "ID_temp")
wither_hills_GPS_block_info_anti_join2 <- anti_join(wither_hills_block_info,wither_hills_GPS, by = "ID_temp")
glimpse(wither_hills_GPS_block_info)

#### measures
wither_hills_harvest_details <- read_excel("C:/Users/ouz001/NZ_work/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                                    sheet = "15,16,17,18 ", skip = 3)
glimpse(wither_hills_harvest_details)
#Create an ID clm
wither_hills_harvest_details_step1 <- wither_hills_harvest_details %>% 
         mutate(vineyard_lower =str_to_lower(Vineyard, locale = "en"),
         block_lower =str_to_lower(Block, locale = "en"),
         Variety_lower =str_to_lower(Variety, locale = "en"),
         year = gsub( "V|v", "20", wither_hills_harvest_details$Vintage))
glimpse(wither_hills_harvest_details_step1)

wither_hills_harvest_details_step2 <- wither_hills_harvest_details_step1 %>% 
         mutate(ID_temp1 = gsub( " ", "_", wither_hills_harvest_details_step1$block_lower),
         ID_temp = paste0(ID_temp1,"_", Variety_lower),
         ID_yr = paste0(ID_temp,"_", year))  
glimpse(wither_hills_harvest_details_step2)



####Note that there is something wrong with the imput date clm we have lots of wrong dates
# replace date error with NA

wither_hills_harvest_details <- wither_hills_harvest_details_step2 %>% 
    mutate(
      harvest_date1 = ifelse(Harvest < 1980, NA , Harvest))

wither_hills_harvest_details$harvest_date1 <- as_datetime(wither_hills_harvest_details$harvest_date1)
glimpse(wither_hills_harvest_details)




#cals and selected clm - need to double check cals
wither_hills_harvest_details <- wither_hills_harvest_details %>% 
  select(ID_yr,ID_temp, 
         #Variety = Variety_lower,
         year,
         brix = `Harvest brix` , #no data in the input file
         harvest_date = harvest_date1, #something wrong here the input file has this problem
         yield_t_ha = `Actual T/Ha`,
         #metres_row_ha =`metres row/ha`, there is an error in the input data file dont use this clm
         vine_spacing = `Vine Spacing` ,
         row_width = `Row Width` ,
         pruning_style = `Pruning style`, 
         bunch_numb_per_vine = `Ave Pre-Harvest Bunch #`,
         bunch_weight = `Ave Pre-Harvest Bunch weight`,
         berry_weight = `Ave Pre-Harvest Berry weight`) %>% 
  mutate(pruning_style = as.double(gsub( " cane", "", pruning_style)),
         yield_kg_m = (yield_t_ha * 1000) / (10000/row_width), #check this cal
         bunch_numb_m = bunch_numb_per_vine / vine_spacing , #check this cal
         bunch_mass_g = 1000 * yield_kg_m /bunch_numb_m, #check this cal
         berry_bunch = bunch_weight / berry_weight,
         berry_wt = bunch_mass_g / berry_bunch,
         julian = as.numeric(format(harvest_date, "%j")))
glimpse(wither_hills_harvest_details)

wither_hills_harvest_details1_wrong_date <- wither_hills_harvest_details %>% 
  filter(harvest_date < 1980)
write_csv(wither_hills_harvest_details1_wrong_date, "wither_hills_harvest_details1_wrong_date.csv")

wither_hills_harvest_details1_no_yld <- wither_hills_harvest_details %>% 
  filter(yield_t_ha <= 0)
write_csv(wither_hills_harvest_details1_no_yld, "wither_hills_harvest_details1_no_yld.csv")
wither_hills_harvest_details1_no_prune_style<- wither_hills_harvest_details %>% 
  filter( is.na(pruning_style))
wither_hills_harvest_details1_bunch_numb<- wither_hills_harvest_details %>% 
  filter( is.na(bunch_numb_per_vine) | bunch_numb_per_vine == 0)
wither_hills_harvest_details1_bunch_wt<- wither_hills_harvest_details %>% 
  filter( is.na(bunch_weight) | bunch_weight == 0)
wither_hills_harvest_details1_berry_wt<- wither_hills_harvest_details %>% 
  filter( is.na(berry_weight) | berry_weight == 0)


#### join the GPS files to the harvest data files
glimpse(wither_hills_GPS_block_info)
glimpse(wither_hills_harvest_details)
wither_hills_GPS_block_info_harvest <- full_join(wither_hills_GPS_block_info, wither_hills_harvest_details,
                                                 by= "ID_temp") %>% 
  mutate(company = "Wither_Hills")
glimpse(wither_hills_GPS_block_info_harvest)

wither_hills_GPS_block_info_harvest <- wither_hills_GPS_block_info_harvest %>% 
  select(company, ID_temp, ID_yr, variety, x_coord, y_coord,
         year,harvest_date, julian,yield_t_ha,yield_kg_m,
         brix,bunch_weight, berry_weight,
         bunch_numb_m,
         row_width, vine_spacing,
         #bunch_mass_g, berry_bunch, berry_wt,
         pruning_style)
glimpse(wither_hills_GPS_block_info_harvest)





wither_hills_GPS_block_info_harvest$na_count <- apply(is.na(wither_hills_GPS_block_info_harvest), 1, sum)

glimpse(wither_hills_GPS_block_info_harvest)
dim(wither_hills_GPS_block_info_harvest)
write_csv(wither_hills_GPS_block_info_harvest, "wither_hills_april_2019.csv")
