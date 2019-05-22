library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)
install.packages("sp")
library(sp)

white_haven_GPS <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Harvest  Mapping Data Whitehaven.xlsx", 
                             sheet = "All - Data")

glimpse(white_haven_GPS)
white_haven_GPS_1 <-  select(white_haven_GPS,Vineyard,
                             Block,
                             Latitude,
                             Longitude,
                             Variety,
                             Row_spacing =`Row Spacing` ,
                             Vine_spacing =`Vine Spacing`)
glimpse(white_haven_GPS_1)


as.numeric(char2dms(white_haven_GPS_1$Latitude))
as.numeric(char2dms("41°29'33.04S"))
as.numeric(char2dms("32d14'23\"N"))
