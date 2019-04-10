library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)

delegates_GPS1 <- read_excel("V:/Marlborough regional/working_jaxs/Delegat For MRC Project RGVB rev.xlsx", 
                                 sheet = "Use this Delegat4 NZ2000")
glimpse(delegates_GPS)
delegates_GPS <- delegates_GPS1 %>% 
  select(ID_temp = Name ,
         x_coord = POINT_X,
         y_coord = POINT_Y) %>% 
  mutate(ID_temp = gsub( "-", "_", ID_temp),
         ID_temp =str_to_lower(ID_temp, locale = "en")) 

glimpse(delegates_GPS)


delegates_data1 <- read_excel("V:/Marlborough regional/working_jaxs/Delegat For MRC Project RGVB rev.xlsx", 
                             sheet = "Yield Info")
glimpse(delegates_data1)
delegates_data <- delegates_data1 %>% 
  mutate(ID_temp = str_to_lower(`Sub-Block`, locale = "en"),
         ID_temp = gsub( "-", "_", ID_temp),
         ID_yr = paste0(ID_temp,"_", Vintage),
         yr = Vintage) %>% 
  select(ID_temp,
         ID_yr,
         Variety,
         yr,
         harvest_date = `Analysis Date`,
         trellis_type = `Trellis Type`,
         pruning_method = `Pruning Method`,
         yield_t_ha = `Yield (Tonnes/Hectare)`,
         berry_weight_g = `Average Pre-Harvest Berry Weight (g)`,
         bunch_weight_g = `Average Pre-Harvest Bunch Weight (g)`)
         
         
glimpse(delegates_data)   
  