library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)


##### Bring in the spatial data
GPS_Pts_cloudy_bay <-   read_excel("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/Regional winery data/Raw_data/Cloudy_bay/Cloudy Bay Sauvignon Harvest data for GYA prokect.xlsx",
                                  "Sheet1" )
str(GPS_Pts_cloudy_bay)

GPS_Pts_cloudy_bay <- select(GPS_Pts_cloudy_bay,
                             Vineyards,
                             coods = `Vineyard Co-ordinates`,
                             Code,
                             blk_name =`Block Name`,
                             row_spacing = `Row spacing`,
                             vine_spacing = `Vine Spacing`)
#seperate the coods clm
GPS_Pts_cloudy_bay <- GPS_Pts_cloudy_bay %>% 
  separate( coods, into = c("lats","long"), sep = ",")
#remove the missing data
GPS_Pts_cloudy_bay <- filter(GPS_Pts_cloudy_bay, lats != "NA")
## the correct coods


mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(GPS_Pts_cloudy_bay)
GPS_Pts_cloudy_bay$lats <- as.double(GPS_Pts_cloudy_bay$lats)
GPS_Pts_cloudy_bay$long <- as.double(GPS_Pts_cloudy_bay$long)

#proj4string(test) <- wgs84CRS   # assume input lat and longs are WGS84
coordinates(GPS_Pts_cloudy_bay) <- ~long+lats
proj4string(GPS_Pts_cloudy_bay) <- wgs84CRS   # assume input lat and longs are WGS84
GPS_Pts_cloudy_bay <- spTransform(GPS_Pts_cloudy_bay, mapCRS)

glimpse(GPS_Pts_cloudy_bay)
GPS_Pts_cloudy_bay_df = as.data.frame(GPS_Pts_cloudy_bay) #this has the new coordinates projected !YES!!
glimpse(GPS_Pts_cloudy_bay_df)

write_csv(GPS_Pts_cloudy_bay_df, "V:/Marlborough regional/working_jaxs/GPS_Pts_cloudy_bay_df_check.csv") #looks ok




############### yld data
yld_cloudy_bay <-   read_excel("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/Regional winery data/Raw_data/Cloudy_bay/Cloudy Bay Sauvignon Harvest data for GYA prokect.xlsx",
                                   "Sheet1" )

str(yld_cloudy_bay)

yld_cloudy_bay_2004 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date = `Harvest date 2004`,
                              yield_t_ha = `2004 t/ha`
                              )
yld_cloudy_bay_2004 <- filter(yld_cloudy_bay_2004, harvest_date != "NA") %>% 
  mutate(year = 2004)

#some dates are entered in as & and - 

yld_cloudy_bay_2004 <- separate(yld_cloudy_bay_2004, 
                                harvest_date, 
                              into = c("day_range", "month"), 
                              sep = "/", remove = FALSE)

test <- separate(yld_cloudy_bay_2004, 
                                day_range, 
                                into = c("day"), 
                                sep = "([\\-\\&\\:])", remove = FALSE)

#make it a date again


test$date <- as.Date(paste(test$day,test$month,test$year,sep="-"),format = "%d-%m-%Y")


### if I have na in dates fill with harvest date case when??
test1 <- mutate(test, date1 = case_when(
  date != "NA" ~ "test"
))



