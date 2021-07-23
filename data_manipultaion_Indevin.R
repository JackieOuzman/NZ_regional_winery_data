


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
######   Bring in the coodinated that I have got from ROB - I have used certain blocks to reflect the whole vineyard see note in excel file for more details     ##########################

Indevin_all <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Indevin/Historical Harvested T per Ha - COMPANY V14-V20.xlsx",
                                        col_types = c("text", "text", "text", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "date", "numeric", "date", 
                                  "numeric", "date", "numeric", "date", 
                                  "numeric", "date", "numeric", "date", 
                                  "numeric", "date"))


 #### Make a ID number of entries
names(Indevin_all)
Indevin_all <- Indevin_all %>%           # Applying row_number function
  mutate(ID = row_number())
######################################################################################################################

# slipt the data into block info and yld data.

Indevin_block <- Indevin_all %>% 
 dplyr::select(ID, Variety,
               Subregion,
               `Latitude, Longitude`,
               `Row Spacing`,
               `Vine Spacing`,
               `Vines/Ha` )
## split the `Latitude, Longitude` into two clm
Indevin_block <- Indevin_block %>% 
  separate(`Latitude, Longitude`, c('Latitude', 'Longitude'), sep = ",")

str(Indevin_block)

Indevin_block$Latitude <- as.double(Indevin_block$Latitude)
Indevin_block$Longitude <- as.double(Indevin_block$Longitude)


######################################################################################################################
################                         change the projection of the data                              #################
######################################################################################################################
#Now I have my data in decimal degrees I want to convert it into GDA

mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(Indevin_block) # seems to be missing a few values
#Forrest_08_2020 <-drop_na(Latitude)

Indevin_block<- Indevin_block %>% 
  filter(!is.na(Latitude))

coordinates(Indevin_block) <- ~Longitude + Latitude

proj4string(Indevin_block) <- wgs84CRS   # assume input lat and longs are WGS84
Indevin_block <- spTransform(Indevin_block, mapCRS)

glimpse(Indevin_block)

Indevin_block_df = as.data.frame(Indevin_block) #this has the new coordinates projected !YES!!
glimpse(Indevin_block_df)

rm("mapCRS", "wgs84CRS", "Forrest_08_2020")  

#############################################################################################
## the yield data per year now.
names(Indevin_all)
Indevin_yld <- Indevin_all %>% 
  dplyr::select(ID, 
                V14_yld_t_ha =`V14 T/Ha`,
                #V14_date =    `V14 Harvest Date`,
                
                V15_yld_t_ha =`V15 T/Ha`,
                #V15_date =    `V15 Harvest Date`,
                
                V16_yld_t_ha =`V16 T/Ha`,
                #V16_date =    `V16 Harvest Date`,
                
                V17_yld_t_ha =`V17 T/Ha`,
                #V17_date =    `V17 Harvest Date`,
                
                V18_yld_t_ha =`V18 T/Ha`,
                #V18_date =    `V18 Harvest Date`,
                
                V19_yld_t_ha = `V19 T/Ha`,
                #V19_date =    `V19 Harvest Date`,
                
                V20_yld_t_ha =`V20 T/Ha`)
                #V20_date =    `V20 Harvest Date`)   
                
 Indevin_date <- Indevin_all %>% 
                  dplyr::select(ID, 
                                #V14_yld_t_ha =`V14 T/Ha`,
                                V14_date =    `V14 Harvest Date`,
                                
                                #V15_yld_t_ha =`V15 T/Ha`,
                                V15_date =    `V15 Harvest Date`,
                                
                                #V16_yld_t_ha =`V16 T/Ha`,
                                V16_date =    `V16 Harvest Date`,
                                
                                #V17_yld_t_ha =`V17 T/Ha`,
                                V17_date =    `V17 Harvest Date`,
                                
                                #V18_yld_t_ha =`V18 T/Ha`,
                                V18_date =    `V18 Harvest Date`,
                                
                                #V19_yld_t_ha = `V19 T/Ha`,
                                V19_date =    `V19 Harvest Date`,
                                
                                #V20_yld_t_ha =`V20 T/Ha`,
                                V20_date =    `V20 Harvest Date`)   




## turn wide data into narrow format.
 Indevin_date_narrow <- Indevin_date %>% 
   pivot_longer(
     cols = starts_with("V"),
     names_to = "year",
     values_to = "harvest_date"
   )


 #now take o
 Indevin_date_narrow$year <- Indevin_date_narrow$year %>% stringr::str_remove(pattern = "_date")
 Indevin_date_narrow$year <- Indevin_date_narrow$year %>% stringr::str_remove(pattern = "V")
 Indevin_date_narrow$year <- as.double(Indevin_date_narrow$year)
  Indevin_date_narrow <- Indevin_date_narrow %>%  
   mutate(year = year +2000,
          ID_yr = paste0(ID, "_",year ))
 View(Indevin_date_narrow) 
  
  ## turn wide yld data into narrow format.
  Indevin_yld_narrow <- Indevin_yld %>% 
    pivot_longer(
      cols = starts_with("V"),
      names_to = "year",
      values_to = "yield_t_ha"
    )
  
  
  #now take out the yld
  Indevin_yld_narrow$year <- Indevin_yld_narrow$year %>% stringr::str_remove(pattern = "_yld_t_ha")
  Indevin_yld_narrow$year <- Indevin_yld_narrow$year %>% stringr::str_remove(pattern = "V")
  Indevin_yld_narrow$year <- as.double(Indevin_yld_narrow$year)
  Indevin_yld_narrow <- Indevin_yld_narrow %>%  
    mutate(year = year +2000,
           ID_yr = paste0(ID, "_",year ))

  #############################################################################################
 ## join the two datasets togther in the wide format
  
  Indevin_date_narrow
  Indevin_yld_narrow
  
  Indevin_date_yld_narrow <- left_join(Indevin_yld_narrow,Indevin_date_narrow)
  
  
  
  #############################################################################################
  ## join the narrow datasets togther with the block info
  
  Indevin_date_yld_narrow
  Indevin_block_df
  
  Indevin <- left_join(Indevin_date_yld_narrow,Indevin_block_df)
  View(Indevin)
  
  
  #############################################################################################
#Keep only the SB Sauvignon Blanc

unique(Indevin$Variety)

  Indevin <- Indevin %>% 
  filter(Variety == "Sauvignon Blanc")


### I think that it
names(Indevin)

Indevin <- Indevin %>% 
  rename(#year = Harvest.year,
         variety = Variety,
         x_coord = Longitude,
         y_coord = Latitude ,
         row_width = Row.Spacing ,
         vine_spacing = Vine.Spacing,
         #Block = Paste0(Subregion, "_" , ID)
         )
#####################################################################################################################

str(Indevin)  

#Forrest_08_2020_df$harvest_date_1 <- lubridate::dmy(Forrest_08_2020_df$harvest_date)

Indevin <- mutate(
  Indevin,
  harvest_date  ,
  julian = as.numeric(format(harvest_date, "%j")),
  m_ha_vine = 10000 / row_width,
  yield_kg_m = (yield_t_ha * 1000) / m_ha_vine,
  bunch_weight = NA,
  berry_per_bunch = NA ,
  bunches_per_vine = NA,
  pruning_style = NA,
  brix = NA,
  meter_row_per_ha = 10000 / row_width,
  yld_per_m_row_kg = (yield_t_ha * 1000) / 10000 /
    row_width,
  bunch_m = NA,
  vine_spacing = NA,
  Block = paste0(Subregion, "_" , ID)
)
names(Indevin)

Indevin <- Indevin %>% 
  dplyr::select(
  year, 
  variety ,
  x_coord ,
  y_coord ,
  harvest_date,
  julian,
  bunch_weight,
  yield_t_ha,
  yield_kg_m,
  brix, 
  bunch_m,
  pruning_style,
  row_width ,
  vine_spacing,
  Block 
)

# add missing clms
Indevin <- mutate(Indevin,berry_weight = NA,
                           bunch_numb_m = NA,
                           na_count = NA,
                           company = "indevin",
              ID_yr = paste0(Block, "_", year))



write.csv(Indevin,
          "V:/Marlborough regional/working_jaxs/July2020/Indevin_02_07_2021.csv")
   
write.csv(Indevin,
          "C:/Users/ouz001/working_from_home/NZ_regional_winery_data/Indevin_02_07_2021.csv")



########################################################################################################################

#Revised Indevin 21/0/2021
names(Indevin)

#just need to make a block 


# Giesen_2020_spatial_yld_upadted2020_vs2 <- Giesen_2020_spatial_yld_upadted2020_vs2 %>% separate(ID_yr, c("Block"), sep = "_", remove = FALSE)

#1. How many sites?
#for each year
Indevin %>%
  group_by(year) %>%
  summarise(count = n_distinct(Block))
#overall for the data set from 2014-2019 how many blocks do we have?
Indevin %>%
  summarise(count = n_distinct(Block))

#2. For harvest date how many sites per year?

Indevin %>%
  group_by(year) %>%
  summarise(mean_julian_days = mean(julian, na.rm = TRUE),
            min_julian_days = min(julian, na.rm = TRUE),
            max_julian_days = max(julian, na.rm = TRUE),
            sum_na = sum(!is.na(julian)))

#3. For yield kg/m  how many sites per year

Indevin %>%
  group_by(year) %>%
  summarise(mean_yield_kg_m = mean(yield_kg_m, na.rm = TRUE),
            min_yield_kg_m = min(yield_kg_m, na.rm = TRUE),
            max_yield_kg_m = max(yield_kg_m, na.rm = TRUE),
            sum_na = sum(!is.na(yield_kg_m)))
