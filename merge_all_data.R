library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)
#V:/Marlborough regional/working_jaxs/pernod_ricard1_sau.csv
#V:/Marlborough regional/working_jaxs/white_haven_2019to2014_all_sav.csv
#V:/Marlborough regional/working_jaxs/wither_hills_GPS_block_info_harvest_sau.csv
#V:/Marlborough regional/working_jaxs/Villia_maria_2017_2012_all_sau.csv
#V:/Marlborough regional/working_jaxs/delegates_april_2019_sau.csv


delegates_april_2019 <-  read_csv( "V:/Marlborough regional/working_jaxs/delegates_april_2019_sau.csv")
pernod_ricard_april_2019 <- read_csv( "V:/Marlborough regional/working_jaxs/pernod_ricard1_sau.csv")
Villia_maria_april_2019<- read_csv( "V:/Marlborough regional/working_jaxs/Villia_maria_2017_2012_all_sau.csv")
white_haven_april_2019 <- read_csv("V:/Marlborough regional/working_jaxs/white_haven_2019to2014_all_sav.csv")
wither_hills_april_2019 <-  read_csv("V:/Marlborough regional/working_jaxs/wither_hills_GPS_block_info_harvest_sau.csv")
####################################################################################################################
####################################    DELEGATES     ##############################################################
####################################################################################################################
glimpse(delegates_april_2019) #19
#need to drop ID_temp
delegates_april_2019 <- select(delegates_april_2019, - ID_temp)
####################################################################################################################
####################################    Pern          ##############################################################
####################################################################################################################
glimpse(pernod_ricard_april_2019) #20
#need to drop ID and number_canes
pernod_ricard_april_2019 <- select(pernod_ricard_april_2019, -  ID, -number_canes)
####################################################################################################################
####################################    Villia Maria    ############################################################
####################################################################################################################
glimpse(Villia_maria_april_2019) #18
Villia_maria_april_2019 <- Villia_maria_april_2019 %>% 
  mutate(ID_yr = paste0(ID,"_", year))
Villia_maria_april_2019 <- select(Villia_maria_april_2019, -  ID)
####################################################################################################################
####################################    wither_hills   ############################################################
####################################################################################################################
glimpse(wither_hills_april_2019) #18
#fix up iD so I have year and ID clm
wither_hills_april_2019 <- wither_hills_april_2019 %>% 
  mutate(ID_yr = paste0(ID,"_", year))
wither_hills_april_2019 <- select(wither_hills_april_2019, -  ID)
####################################################################################################################
####################################    white_haven     ############################################################
####################################################################################################################
glimpse(white_haven_april_2019) #21
white_haven_april_2019 <- select(white_haven_april_2019, -Vineyard, -Block, -ID)

glimpse(wither_hills_april_2019)

####################################################################################################################
####################################    merge one step at a time   #################################################
####################################################################################################################

delegate_pern <- rbind(delegates_april_2019,
                      pernod_ricard_april_2019)

glimpse(delegate_pern)
glimpse(Villia_maria_april_2019)
delegate_pern_villia <- rbind(delegate_pern,
                              Villia_maria_april_2019)

delegate_pern_villia_white_haven <- rbind(delegate_pern_villia,
                                          white_haven_april_2019)

del_pern_vill_white_wither <- rbind(delegate_pern_villia_white_haven,
                                    wither_hills_april_2019)


write_csv(del_pern_vill_white_wither, "V:/Marlborough regional/working_jaxs/del_pern_vill_white_wither.csv")

 
