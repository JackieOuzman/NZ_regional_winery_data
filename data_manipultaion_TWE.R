


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

TWE_coordinates <- read_csv("V:/Marlborough regional/Regional winery data/Raw_data/TWE/TWE_JAX.csv")

names(TWE_coordinates)
str(TWE_coordinates)

#####################################################################################################################

## the year in this file now in the year of planting - so change this.
TWE_coordinates <- TWE_coordinates %>% 
  rename(Year_planting = Year)

## make a clm for the year of harvest
#subset data for just yld
yld <- TWE_coordinates %>%
  dplyr::select(X,Y,NAME, 
  Yld_2014,
  Yld_2015,
  Yld_2016,
  Yld_2017,
  Yld_2018,Yld_2019 )


names(yld)
yld_narrow <- yld %>% 
  pivot_longer(cols = starts_with("Yld"),
               names_to = "year",
               values_to = "yield_t_ha")
               
#now take out the yld
yld_narrow$year <- yld_narrow$year %>% stringr::str_remove(pattern = ".*_")


#subset data for just harvest Date
names(TWE_coordinates)
harvest_date <- TWE_coordinates %>%
  dplyr::select(X,Y,NAME, 
                Harvest_date_2014,
                Harvest_date_2015,
                Harvest_date_2016,
                Harvest_date_2017,
                Harvest_date_2018,Harvest_date_2019 )


names(harvest_date)
harevst_date_narrow <- harvest_date %>% 
  pivot_longer(cols = starts_with("Harvest_date"),
               names_to = "year",
               values_to = "Harvest_date")

#now take out the yld
harevst_date_narrow$year <- harevst_date_narrow$year %>% stringr::str_remove(pattern = ".*_")

## join the narrow yld and narrow harvest date togther.
# make a join_clm

harevst_date_narrow <- harevst_date_narrow %>% 
  mutate(Join_ID = paste0(NAME, "_", year))
yld_narrow <- yld_narrow %>% 
  mutate(Join_ID = paste0(NAME, "_", year))

yld_and_date <- left_join(yld_narrow, harevst_date_narrow)


## now add the rest of the data.
names(TWE_coordinates)
TWE <- TWE_coordinates %>% 
  dplyr::select(NAME,GROWER,VINEYARD_1,VARIETY, vine_spacing = `Vine Space`,row_spacing =`Row Space`)  


TWE <- left_join(yld_and_date, TWE)

names(TWE)
str(TWE)
test <- TWE



TWE$Harvest_date <- lubridate::dmy(TWE$Harvest_date)

TWE <- mutate(
  TWE,
  harvest_date  = Harvest_date,
  julian = as.numeric(format(harvest_date, "%j")),
  m_ha_vine = 10000 / row_spacing,
  yield_kg_m = (yield_t_ha * 1000) / m_ha_vine,
  bunch_weight = NA,
  berry_per_bunch = NA ,
  bunches_per_vine = NA,
  pruning_style = NA,
  brix = NA,
  meter_row_per_ha = 10000 / row_spacing,
  yld_per_m_row_kg = (yield_t_ha * 1000) / 10000 /
    row_spacing,
  bunch_m = NA
)
names(TWE)

TWE <- TWE %>% 
  dplyr::select(
  year, 
  variety = VARIETY,
  x_coord = X,
  y_coord = Y ,
  harvest_date,
  julian,
  bunch_weight,
  yield_t_ha,
  yield_kg_m,
  brix, 
  bunch_m,
  pruning_style,
  row_width = row_spacing ,
  vine_spacing,
  Block = NAME 
)

# add missing clms
TWE <- mutate(TWE,berry_weight = NA,
                           bunch_numb_m = NA,
                           na_count = NA,
                           company = "TWE",
              ID_yr = paste0(Block, "_", year))



write.csv(TWE,
          "V:/Marlborough regional/working_jaxs/July2020/TWE_30_06_2021.csv")
   
write.csv(TWE,
          "C:/Users/ouz001/working_from_home/NZ_regional_winery_data/TWE_30_06_2021.csv")
