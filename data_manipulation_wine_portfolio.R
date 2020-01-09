library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)


###########################################################################################
#### GPS points ################################################################
#########################################################################################

#mapCRS <- CRS("+init=epsg:2120")     # 2120 = NZGD2000 / Marlborough 2000 
#wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

###############################################################################################
shapefile_WP =  st_read("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/Kuranui_NZ2000.shp")
shapefile_WP_KML <- st_read("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/Kuranui blocks.kml")
###############################################################################################
#plot(shapefile_WP_KML)
#I can't get the coodinates correct - would be good to make this work!
# shapefile_WP_KML <- st_transform(shapefile_WP_KML, 4326)
# shapefile_WP_KML <- st_transform(shapefile_WP_KML, 2120)

str(shapefile_WP)
geom <- st_geometry(shapefile_WP)
centroid <- st_centroid(geom)
#this makes a df with the x and y coodinates but no name
centroid_df <- data.frame(matrix(unlist(centroid), nrow=length(centroid), byrow=T))

#pull out the name clm from the shapefile that we used first
name <- shapefile_WP_KML$Name

centroid_df <- mutate(centroid_df, 
                      #OBJECTID = Object_ID,
                      POINT_X = X1,
                      POINT_Y = X2,
                      Block_name = name)

centroid_df <- select(centroid_df,
                      Block_name,
                      POINT_X,
                      POINT_Y)

write_csv(centroid_df, "V:/Marlborough regional/working_jaxs/Wine_port_centroid_df_check.csv")

###########################################################################################
#### Yld data     ################################################################
#########################################################################################


Yld_data_2014 <-   read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/Kuranui Harvest data 2014-18.xlsx",
                 "2014" ,  col_names = FALSE,  range = "A5:s30" )

header1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/Kuranui Harvest data 2014-18.xlsx",
                       "2014" ,  col_names = FALSE,  range = "A1:s1" )



names(Yld_data_2014) <- header1
#clm 7 is the block id

colnames(Yld_data_2014)[7] <- "BLK_CODE"
colnames(Yld_data_2014)[17] <- "Brix"
str(Yld_data_2014)



Yld_data_20141 <- select(Yld_data_2014,
                        "BLK_CODE") 

# ,
                        `2014 Actual t/ha` = Yield_t_ha,
                        `Av. Bunch Weight at Harvest (g)` = Bunch_wt,
                        `Av .Berry Weight at Harvest (g)` = Berry_wt,
                        `2014 Harvest Dates` = Harvest_day,
                        Brix)
                        
                        

