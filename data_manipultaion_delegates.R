library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)

delegates_GPS1 <- read_excel("V:/Marlborough regional/working_jaxs/Delegat For MRC Project RGVB rev.xlsx", 
                                 sheet = "Use this Delegat4 NZ2000")
glimpse(delegates_GPS1)
delegates_GPS <- delegates_GPS1 %>% 
  select(ID_temp = Name ,
         x_coord = POINT_X,
         y_coord = POINT_Y) %>% 
  mutate(ID_temp = gsub( "-", "_", ID_temp),
         ID_temp =str_to_lower(ID_temp, locale = "en")) 

glimpse(delegates_GPS)


####Bring in the sub_block info#####
delegates_sub_block1 <- read_excel("V:/Marlborough regional/working_jaxs/Delegat For MRC Project RGVB rev.xlsx", 
                                   sheet = "Sub Block info")
glimpse(delegates_sub_block1)

delegates_sub_block <- delegates_sub_block1 %>% 
  mutate(ID_temp = str_to_lower(`Sub-Block Name`, locale = "en"),
         ID_temp = gsub( "-", "_", ID_temp)) %>% 
  select(ID_temp,
         row_spacing_m = `Row Spacing (m) (Block)`,
         Vine_Spacing_m = `Vine Spacing (m) (Block)`)
glimpse(delegates_sub_block)

### Join to sub block and GPS data based onID_temp####

glimpse(delegates_sub_block)
glimpse(delegates_GPS)
delegates_GPS_sub_block <- full_join(delegates_GPS, delegates_sub_block, by= "ID_temp")

delegates_GPS_sub_block_anti_join1 <- anti_join(delegates_GPS, delegates_sub_block, by= "ID_temp")
delegates_GPS_sub_block_anti_join2 <- anti_join(delegates_sub_block, delegates_GPS, by= "ID_temp")

glimpse(delegates_GPS_sub_block) # change the row_spacing_m to row_width 



####Bring in the yield info#####
delegates_yld_data1 <- read_excel("V:/Marlborough regional/working_jaxs/Delegat For MRC Project RGVB rev.xlsx", 
                             sheet = "Yield Info")
glimpse(delegates_yld_data1)

####    Harvest data    ######
delegates_yld_data_harvest1 <- delegates_yld_data1 %>% 
  filter(`Yield Type` == "Actual Yield") 
glimpse(delegates_yld_data_harvest1)

delegates_yld_data_harvest <- delegates_yld_data_harvest1 %>% 
  mutate(ID_temp = str_to_lower(`Sub-Block`, locale = "en"),
         ID_temp = gsub( "-", "_", ID_temp),
         ID_yr = paste0(ID_temp,"_", Vintage),
         yr = Vintage,
         brix = NA,
         variety = Variety) %>% 
  select(ID_temp,
         ID_yr,
         variety,
         yr,
         brix,
         harvest_date = `Analysis Date`,
         trellis_type = `Trellis Type`,
         pruning_style = `Pruning Method`, #can this be revised with what I used for pernod
         yield_t_ha = `Yield (Tonnes/Hectare)`) %>% 
         #berry_weight_g = `Average Pre-Harvest Berry Weight (g)`,
         #bunch_weight_g = `Average Pre-Harvest Bunch Weight (g)`) %>% 
  mutate(julian = as.numeric(format(harvest_date, "%j")))
glimpse(delegates_yld_data_harvest) #has yield tha here

####    Pre - Harvest data    ######  
delegates_yld_data_pre_harvest <- delegates_yld_data1 %>% 
    filter(`Yield Type` == "Pre-Harvest Yield") 
  
  
delegates_yld_data_pre_harvest <- delegates_yld_data_pre_harvest %>% 
    mutate(ID_temp = str_to_lower(`Sub-Block`, locale = "en"),
           ID_temp = gsub( "-", "_", ID_temp),
           ID_yr = paste0(ID_temp,"_", Vintage),
           yr = Vintage) %>% 
    select(ID_yr,
           berry_weight = `Average Pre-Harvest Berry Weight (g)`,
           bunch_weight = `Average Pre-Harvest Bunch Weight (g)`)
glimpse(delegates_yld_data_pre_harvest)

#join pre harvest and yield data togther

delegates_yld_data1 <- left_join(delegates_yld_data_harvest, delegates_yld_data_pre_harvest, by= "ID_yr")
delegates_yld_data <- full_join(delegates_yld_data_harvest, delegates_yld_data_pre_harvest, by= "ID_yr")
#check <- full_join(delegates_yld_data_harvest, delegates_yld_data_pre_harvest, by= "ID_yr")
#glimpse(check)
glimpse(delegates_yld_data)
glimpse(delegates_yld_data1)

###### Join more data ####

glimpse(delegates_GPS_sub_block)
glimpse(delegates_yld_data)

delegates_GPS_sub_block_yld <- left_join(delegates_yld_data,delegates_GPS_sub_block, by= "ID_temp" )
glimpse(delegates_GPS_sub_block_yld)

delegates_GPS_sub_block_yld_anti_join1 <- anti_join(delegates_yld_data,delegates_GPS_sub_block, by= "ID_temp" )
delegates_GPS_sub_block_yld_anti_join2 <- anti_join(delegates_GPS_sub_block,delegates_yld_data, by= "ID_temp" )



#### ADD Some extra data clms - need to chcek this #####
glimpse(delegates_GPS_sub_block_yld)

delegates_GPS_sub_block_yld <- delegates_GPS_sub_block_yld %>% 
  mutate(yield_kg_m 	= ( yield_t_ha * 1000) / (10000/row_spacing_m), # is row spacing and row width the same thing?
         #bunch_numb_m = bunch_numb_per_vine / Vine_Spacing_m,
         bunch_numb_m = NA,
         #bunch_mass_g 		= 1000 * yield_kg_m /bunch_numb_m,
         bunch_mass_g 		= NA,
         berry_bunch 		= bunch_weight / berry_weight,
         berry_wt = NA,
         company = "Delegates",
         year = yr,
         row_width  = row_spacing_m,
         vine_spacing =Vine_Spacing_m)
glimpse(delegates_GPS_sub_block_yld)

delegates_april_2019 <- delegates_GPS_sub_block_yld %>% 
select(company, ID_temp, ID_yr, variety, x_coord, y_coord,
       year, harvest_date, julian,yield_t_ha,yield_kg_m,
       brix,bunch_weight, berry_weight,
       bunch_numb_m, 
       #bunch_mass_g, berry_bunch, berry_wt,
       pruning_style,
       row_width,
       vine_spacing)
glimpse(delegates_april_2019)
delegates_april_2019$na_count <- apply(is.na(delegates_april_2019), 1, sum)
glimpse(delegates_april_2019)
write_csv(delegates_april_2019, "delegates_april_2019.csv")
