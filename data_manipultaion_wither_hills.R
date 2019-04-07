library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

wither_hills_GPS_temp <- read_excel("C:/Users/ouz001/NZ_work/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                             sheet = "Wither Hills NZ2000")
glimpse(wither_hills_GPS_temp)
wither_hills_GPS <- select(wither_hills_GPS_temp,
                    ID_temp = Name ,
                    x_temp = POINT_X,
                    y_temp = POINT_Y)
glimpse(wither_hills_GPS)
wither_hills_block_info <- read_excel("C:/Users/ouz001/NZ_work/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                                    sheet = "locations" , skip = 3)
glimpse(wither_hills_block_info)
wither_hills_block_info <- wither_hills_block_info %>% 
                            select(
                            ID_temp = X__1, 
                            variety,
                            row_spacing = `row spacing (m)`)

###Join GPS and block data

wither_hills_GPS_block_info <- left_join(wither_hills_GPS,wither_hills_block_info, by = "ID_temp")
glimpse(wither_hills_GPS_block_info)
