library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)

## this file has boundries ID from row number on a pdf map and cal the ha for each subblock
Giesen_shapefile =  st_read("V:/Marlborough regional/Regional winery data/Raw_data/Giesen/Working_blk_bound/Giesen_Bay_blocks.shp")
#some subblock have the same name so these should be aggreated before taking the centriod

Giesen_shapefile_agg <- aggregate(Giesen_shapefile, list(Giesen_shapefile$Block ), function(x) x[1])

geom <- st_geometry(Giesen_shapefile_agg)
centroid <- st_centroid(geom)
centroid_df <- data.frame(matrix(unlist(centroid), nrow=length(centroid), byrow=T))


Giesen_shapefile$Block <- as.character(Giesen_shapefile$Block)
Block_ID <- c(unique(Giesen_shapefile$Block)) 


centroid_df <- mutate(centroid_df, Block = Block_ID,
                      POINT_X = X1,
                      POINT_Y = X2
                      )
shapefile_df <- st_drop_geometry(Giesen_shapefile)
str(shapefile_df)
str(centroid_df)

Giesen_site_centroid <- full_join(shapefile_df,centroid_df )
str(Giesen_site_centroid)


Giesen_site_centroid <- select(Giesen_site_centroid,
                                    Block,
                                    #ha= AREA_GEO,this is wrong now
                               POINT_X,
                               POINT_Y
                                    )
### what is the sum of the ha for the block with the same names?

Giesen_shapefile_sum_ha <- group_by(Giesen_shapefile, Block) %>% 
                                    summarise(ha_sum = mean(AREA_GEO))  
#remove the geometry
Giesen_shapefile_sum_ha <- st_set_geometry(Giesen_shapefile_sum_ha, NULL)

#join these data set togther again
str(Giesen_shapefile_sum_ha)
str(Giesen_site_centroid)

centroid_Giesen_shapefile_agg_ha <- left_join(Giesen_site_centroid, Giesen_shapefile_sum_ha)
str(centroid_Giesen_shapefile_agg_ha)
centroid_Giesen_shapefile_agg_ha <- select(centroid_Giesen_shapefile_agg_ha,block_ID = Block,  ha_sum, POINT_X, POINT_Y )


st_write(centroid_Giesen_shapefile, "centroid_Giesen_shapefile.csv", layer_options = "GEOMETRY=AS_XY")                               

Giesen_2020 <-  read_excel( "V:/Marlborough regional/Regional winery data/Raw_data/Giesen/Giesen Bay Block Rob 2017-19.xlsx",
                            sheet = "Year_block totals")
str(Giesen_2020)
#select clms
Giesen_2020 <- select(
  Giesen_2020,
  variety = Variety ,
  Block,
  year = Year,
  harvest_date = Date,
  yield_t_total = `Sum Nett (t)`,
  brix = `Brix (deg)`,
  row_width = `Row space`,
  vine_spacing = `Vine space`,
  Block
)

unique(Giesen_2020$Block)
#Try and get block names to match
Giesen_2020<- mutate(Giesen_2020, block_ID = case_when(
  Block == "SBGBAY59+" ~ "SBGBAY59+",
  Block == "SBGBAY59+a" ~ "SBGBAY59+",
  Block == "SB BAY59+ CONTROL" ~ "NA",
  Block == "SB BAY59+ TREATMENT" ~ "NA",
  Block == "SBGBAY 201-235" ~ "NA", #dont have block info for this site could be SBGBAY300'
  Block == "SBGBAY125" ~ "SBGBAY125",
  Block == "SBGBAY125a" ~ "SBGBAY125",
  Block == "SBGBAY160" ~ "SBGBAY160",
  Block == "SBGBAY160a" ~ "SBGBAY160",
  Block == "SBGBAY200" ~ "SBGBAY200",
  Block == "SBGBAY200a" ~ "SBGBAY200",
  Block == "SBGBAY236-270 CONTROLlow alc control" ~ "NA",
  Block == "SBGBAY270" ~ "NA",
  Block == "SBGBAY270a" ~ "NA",
  Block == "SBGBAY495" ~ "NA",
  Block == "SBGBAY495a" ~ "NA",
  Block == "SBGBAY531" ~ "SBGBAY531",
  Block == "SBGBAY531a" ~ "SBGBAY531",
  Block == "SBGBAY670" ~ "SBGBAY670",
  Block == "SBGBAY670a" ~ "SBGBAY670",
  Block == "SBGBAY740" ~ "SBGBAY740",
  Block == "SBGBAY740a" ~ "SBGBAY740",
  Block == "SBGBAY795" ~ "SBGBAY795",
  Block == "SBGBAY795a" ~ "SBGBAY795",
  Block == "SBGBAY850" ~ "SBGBAY850",
  Block == "SBGBAY850a" ~ "SBGBAY850",
  Block == "SBGBAY927" ~ "SBGBAY850",
  Block == "SBGBAY927a" ~ "SBGBAY927"
))
str(Giesen_2020) #38
str(centroid_Giesen_shapefile_agg_ha) #13

#add in spatial data matching the block ID 
Giesen_2020_spatial_yld <- left_join(Giesen_2020,centroid_Giesen_shapefile_agg_ha)
str(Giesen_2020_spatial_yld)



Giesen_2020_spatial_yld <- mutate(Giesen_2020_spatial_yld,
                      company = "Giesen",
                      ID_yr = paste0(block_ID, "_", year),
                      x_coord = POINT_X,
                      y_coord = POINT_Y,
                      julian = as.numeric(format(harvest_date, "%j")),
                      yield_t_ha = yield_t_total/ ha_sum, #ha is from spatial data
                      m_ha_vine = 1000/ row_width,
                      yield_kg_m = (yield_t_ha *1000)/m_ha_vine,
                      bunch_weight = NA,
                      berry_weight = NA,
                      bunch_numb_m = NA,
                      pruning_style = NA,
                      na_count = NA
                      
)
Giesen_2020_spatial_yld <- select(Giesen_2020_spatial_yld,
company,
ID_yr,
variety,
x_coord,
y_coord,
year,
harvest_date,
julian,
yield_t_ha,
yield_kg_m,
brix,
bunch_weight,
berry_weight,
bunch_numb_m,
pruning_style,
row_width,
vine_spacing,
na_count)

Giesen_2020_spatial_yld <- filter(Giesen_2020_spatial_yld, x_coord > 0)

dim(Giesen_2020_spatial_yld)
#merge of Rob and my work

write_csv(Giesen_2020_spatial_yld, "V:/Marlborough regional/Regional winery data/Raw_data/Giesen/Giesen_2020_spatial_yld.csv")






check <- left_join(Giesen_2020,centroid_Giesen_shapefile_agg_ha)

check <- mutate(check,
                                  company = "Giesen",
                                  ID_yr = paste0(block_ID, "_", year),
                                  x_coord = POINT_X,
                                  y_coord = POINT_Y,
                                  julian = as.numeric(format(harvest_date, "%j")),
                                  yield_t_ha = yield_t_total/ ha_sum, #ha is from spatial data
                                  m_ha_vine = 1000/ row_width,
                                  yield_kg_m = (yield_t_ha *1000)/m_ha_vine,
                                  bunch_weight = NA,
                                  berry_weight = NA,
                                  bunch_numb_m = NA,
                                  pruning_style = NA,
                                  na_count = NA
                                  
)
