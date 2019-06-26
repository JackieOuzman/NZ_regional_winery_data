library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)
#V:/Marlborough regional/working_jaxs/pernod_ricard1_sau.csv
#V:/Marlborough regional/working_jaxs/white_haven_2019to2014_all_sav.csv
#V:/Marlborough regional/working_jaxs/wither_hills_GPS_block_info_harvest_sau.csv
#V:/Marlborough regional/working_jaxs/Villia_maria_2017_2012_all_sau.csv
#V:/Marlborough regional/working_jaxs/delegates_april_2019_sau.csv



pernod_ricard_april_2019 <- read_csv( "V:/Marlborough regional/working_jaxs/pernod_ricard1_sau.csv")
white_haven_april_2019 <- read_csv("V:/Marlborough regional/working_jaxs/white_haven_2019to2014_all_sav.csv")
wither_hills_april_2019 <-  read_csv("V:/Marlborough regional/working_jaxs/wither_hills_GPS_block_info_harvest_sau.csv")
Villia_maria_april_2019<- read_csv( "V:/Marlborough regional/working_jaxs/Villia_maria_2017_2012_all_sau.csv")
delegates_april_2019 <-  read_csv( "V:/Marlborough regional/working_jaxs/delegates_april_2019_sau.csv")






glimpse(delegates_april_2019) #19
glimpse(wither_hills_april_2019) #18
glimpse(Villia_maria_april_2019) #18
glimpse(pernod_ricard_april_2019) #20


delegate_wither_villia_pern <- rbind(delegates_april_2019,
                                     wither_hills_april_2019,
                                     Villia_maria_april_2019,
                                     pernod_ricard_april_2019)
write_csv(delegate_wither_villia_pern, "delegate_wither_villia_pern.csv")
write_csv(delegate_wither_villia_pern, "delegate_wither_villia_pern2.csv")
 
