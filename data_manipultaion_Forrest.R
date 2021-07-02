


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

Forrest_08 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                    sheet = "2008",
                    col_types = c("text", 
                                  "text", "text", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "date"))
str(Forrest_08)
Forrest_12 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                         sheet =    "2012",
                         col_types = c("text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date"))
Forrest_13 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                         sheet =    "2013",
                         col_types = c("text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date"))
Forrest_14 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                         sheet =    "2014",
                         col_types = c("text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date"))
Forrest_15 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                         sheet =    "2015",
                         col_types = c("text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date"))
Forrest_16 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                         sheet =    "2016",
                         col_types = c("text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date"))
Forrest_17 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                         sheet =    "2017",
                         col_types = c("text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date"))
Forrest_18 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                         sheet =    "2018",
                         col_types = c("text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date"))
Forrest_19 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                         sheet =    "2019",
                         col_types = c("text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date"))
Forrest_20 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Forrest/Forrest Cropping History for Mike Trought.xlsx",
                         sheet =    "2020",
                         col_types = c("text", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "date"))

## join them all togther

Forrest_08_2020 <- rbind(Forrest_08,
                         Forrest_12,
                         Forrest_13,
                         Forrest_14,
                         Forrest_15,
                         Forrest_16,
                         Forrest_17,
                         Forrest_18,
                         Forrest_19,
                         Forrest_20
                         )
### fill in the missing  data for sublock clm
names(Forrest_08_2020)

Forrest_08_2020 <- Forrest_08_2020 %>%
  fill(Subblock, .direction = "down")


#### remove the df I no longer need
rm(list=ls()[ls()!= c("Forrest_08_2020")])


######################################################################################################################
################                         change the projection of the data                              #################
######################################################################################################################
#Now I have my data in decimal degrees I want to convert it into GDA

mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(Forrest_08_2020) # seems to be missing a few values
#Forrest_08_2020 <-drop_na(Latitude)

Forrest_08_2020<- Forrest_08_2020 %>% 
  filter(!is.na(Latitude))

coordinates(Forrest_08_2020) <- ~Longitude + Latitude

proj4string(Forrest_08_2020) <- wgs84CRS   # assume input lat and longs are WGS84
Forrest_08_2020 <- spTransform(Forrest_08_2020, mapCRS)

glimpse(Forrest_08_2020)

Forrest_08_2020_df = as.data.frame(Forrest_08_2020) #this has the new coordinates projected !YES!!
glimpse(Forrest_08_2020_df)

rm("mapCRS", "wgs84CRS", "Forrest_08_2020")  


#############################################################################################
#Keep only the SB Sauvignon Blanc

unique(Forrest_08_2020_df$Variety)

Forrest_08_2020_df <- Forrest_08_2020_df %>% 
  filter(Variety == "Sauvignon Blanc")


### I think that it
names(Forrest_08_2020_df)

Forrest_08_2020_df <- Forrest_08_2020_df %>% 
  rename(year = Harvest.year,
         variety = Variety,
         x_coord = Longitude,
         y_coord = Latitude ,
         harvest_date = harvest.date,
         
         bunch_weight = Pre.Harvest.Average.Bunch.Weight..g.,
         yield_t_ha = Harvested.T.Ha,
         #yield_kg_m,
         #brix, 
         #bunch_m,
         #pruning_style,
         row_width = In.row.spacing..m. ,
         #vine_spacing,
         Block = Subblock 
         )

#####################################################################################################################

str(Forrest_08_2020_df)  

#Forrest_08_2020_df$harvest_date_1 <- lubridate::dmy(Forrest_08_2020_df$harvest_date)

Forrest_08_2020_df <- mutate(
  Forrest_08_2020_df,
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
  vine_spacing = NA
)
names(Forrest_08_2020_df)

Forrest_08_2020_df <- Forrest_08_2020_df %>% 
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
Forrest_08_2020_df <- mutate(Forrest_08_2020_df,berry_weight = NA,
                           bunch_numb_m = NA,
                           na_count = NA,
                           company = "TWE",
              ID_yr = paste0(Block, "_", year))



write.csv(Forrest_08_2020_df,
          "V:/Marlborough regional/working_jaxs/July2020/Forrest_08_2020.csv")
   
write.csv(Forrest_08_2020_df,
          "C:/Users/ouz001/working_from_home/NZ_regional_winery_data/Forrest_08_2020.csv")
