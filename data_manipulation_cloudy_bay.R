library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)


##### Bring in the spatial data as supplied

GPS_Pts_cloudy_bay_shapefile =  st_read("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/Regional winery data/Raw_data/Cloudy_bay/CBV_Marster_GDA.shp")
str(GPS_Pts_cloudy_bay_shapefile)
GPS_Pts_cloudy_bay_shapefile <- select(GPS_Pts_cloudy_bay_shapefile,
                                       Name,
                                      Owner,
                                      Vineyard,
                                      Variety,
                                      geometry,
                                      Row_spacin,
                                      Vine_spaci
                                      )

unique(
  GPS_Pts_cloudy_bay_shapefile$Variety)

GPS_Pts_cloudy_bay_shapefile_SB <- filter(GPS_Pts_cloudy_bay_shapefile,
                                          Variety == "SB")

#some subblock have the same name so these should be aggreated before taking the centriod
str(GPS_Pts_cloudy_bay_shapefile_SB)
GPS_Pts_cloudy_bay_shapefile_SB_agg <- aggregate(GPS_Pts_cloudy_bay_shapefile_SB, 
                                              list(GPS_Pts_cloudy_bay_shapefile_SB$Name ), function(x) x[1])

#########################################################################################
str(GPS_Pts_cloudy_bay_shapefile_SB_agg)
geom <- st_geometry(GPS_Pts_cloudy_bay_shapefile_SB_agg)
centroid <- st_centroid(geom)
centroid_df <- data.frame(matrix(unlist(centroid), nrow=length(centroid), byrow=T))


GPS_Pts_cloudy_bay_shapefile_SB_agg$Name <- as.character(GPS_Pts_cloudy_bay_shapefile_SB_agg$Name)
Block_ID <- c(unique(GPS_Pts_cloudy_bay_shapefile_SB_agg$Name)) 

dim(centroid_df)
dim(GPS_Pts_cloudy_bay_shapefile_SB_agg)

centroid_df <- mutate(centroid_df, Name = Block_ID,
                      POINT_X = X1,
                      POINT_Y = X2
)
shapefile_df <- st_drop_geometry(GPS_Pts_cloudy_bay_shapefile_SB_agg)
str(shapefile_df)
str(centroid_df) #block

cloudyBay_site_centroid <- full_join(shapefile_df,centroid_df )
str(cloudyBay_site_centroid)


cloudyBay_site_centroid <- select(cloudyBay_site_centroid,
                               Name,
                               Row_spacin,
                               Vine_spaci,
                               POINT_X,
                               POINT_Y
)



rm(
  list = c(
    "centroid",
    "centroid_df",
    "geom",
    "GPS_Pts_cloudy_bay_shapefile",
    "GPS_Pts_cloudy_bay_shapefile_SB",
    "GPS_Pts_cloudy_bay_shapefile_SB_agg",
    "Block_ID",
    "shapefile_df"))
    


############### yld data
yld_cloudy_bay <-   read_excel("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/Regional winery data/Raw_data/Cloudy_bay/Cloudy Bay Sauvignon Harvest data for GYA prokect.xlsx",
                                   "Sheet1" )
str(yld_cloudy_bay)
yld_cloudy_bay_2004 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2004`,
                              yield_t_ha = `2004 t/ha`
                              )
yld_cloudy_bay_2004 <- filter(yld_cloudy_bay_2004, harvest_date1 != "NA") %>% 
  mutate(year = 2004)
################################################################################
yld_cloudy_bay_2005 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2005`,
                              yield_t_ha = `2005 t/ha`
)
yld_cloudy_bay_2005 <- filter(yld_cloudy_bay_2005, harvest_date1 != "NA") %>% 
  mutate(year = 2005)
################################################################################
yld_cloudy_bay_2006 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2006`,
                              yield_t_ha = `2006 t/ha`
)
yld_cloudy_bay_2006 <- filter(yld_cloudy_bay_2006, harvest_date1 != "NA") %>% 
  mutate(year = 2006)
################################################################################
yld_cloudy_bay_2007 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2007`,
                              yield_t_ha = `2007 t/ha`
)
yld_cloudy_bay_2007 <- filter(yld_cloudy_bay_2007, harvest_date1 != "NA") %>% 
  mutate(year = 2007)
################################################################################
yld_cloudy_bay_2008 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2008`,
                              yield_t_ha = `2008 t/ha`
)
yld_cloudy_bay_2008 <- filter(yld_cloudy_bay_2008, harvest_date1 != "NA") %>% 
  mutate(year = 2008)
################################################################################
yld_cloudy_bay_2009 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2009`,
                              yield_t_ha = `2009 t/ha`
)
yld_cloudy_bay_2009 <- filter(yld_cloudy_bay_2009, harvest_date1 != "NA") %>% 
  mutate(year = 2009)
################################################################################
yld_cloudy_bay_2010 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2010`,
                              yield_t_ha = `2010 t/ha`
)
yld_cloudy_bay_2010 <- filter(yld_cloudy_bay_2010, harvest_date1 != "NA") %>% 
  mutate(year = 2010)
################################################################################
yld_cloudy_bay_2011 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2011`,
                              yield_t_ha = `2011 t/ha`
)
yld_cloudy_bay_2011 <- filter(yld_cloudy_bay_2011, harvest_date1 != "NA") %>% 
  mutate(year = 2011)
################################################################################
yld_cloudy_bay_2012 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2012`,
                              yield_t_ha = `2012 t/ha`
)
yld_cloudy_bay_2012 <- filter(yld_cloudy_bay_2012, harvest_date1 != "NA") %>% 
  mutate(year = 2012)
################################################################################
yld_cloudy_bay_2013 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2013`,
                              yield_t_ha = `2013 t/ha`
)
yld_cloudy_bay_2013 <- filter(yld_cloudy_bay_2013, harvest_date1 != "NA") %>% 
  mutate(year = 2013)
################################################################################
yld_cloudy_bay_2014 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2014`,
                              yield_t_ha = `2014 t/ha`
)
yld_cloudy_bay_2014 <- filter(yld_cloudy_bay_2014, harvest_date1 != "NA") %>% 
  mutate(year = 2014)
################################################################################
yld_cloudy_bay_2015 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2015`,
                              yield_t_ha = `2015 t/ha`
)
yld_cloudy_bay_2015 <- filter(yld_cloudy_bay_2015, harvest_date1 != "NA") %>% 
  mutate(year = 2015)
################################################################################
yld_cloudy_bay_2016 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2016`,
                              yield_t_ha = `2016 t/ha`
)
yld_cloudy_bay_2016 <- filter(yld_cloudy_bay_2016, harvest_date1 != "NA") %>% 
  mutate(year = 2016)
################################################################################
yld_cloudy_bay_2017 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2017`,
                              yield_t_ha = `2017 t/ha`
)
yld_cloudy_bay_2017 <- filter(yld_cloudy_bay_2017, harvest_date1 != "NA") %>% 
  mutate(year = 2017)
################################################################################
yld_cloudy_bay_2018 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2018`,
                              yield_t_ha = `2018 t/ha`
)
yld_cloudy_bay_2018 <- filter(yld_cloudy_bay_2018, harvest_date1 != "NA") %>% 
  mutate(year = 2018)
################################################################################
yld_cloudy_bay_2019 <- select(yld_cloudy_bay,
                              Code ,
                              blk_name =`Block Name`,
                              Variety,
                              harvest_date1 = `Harvest date 2019`,
                              yield_t_ha = `2019 t/ha`
)
yld_cloudy_bay_2019 <- filter(yld_cloudy_bay_2019, harvest_date1 != "NA") %>% 
  mutate(year = 2019)

yld_cloudy_bay_2004_19 <- rbind(yld_cloudy_bay_2004,
                                yld_cloudy_bay_2005,
                                yld_cloudy_bay_2006,
                                yld_cloudy_bay_2007,
                                yld_cloudy_bay_2008,
                                yld_cloudy_bay_2009,
                                yld_cloudy_bay_2010,
                                yld_cloudy_bay_2011,
                                yld_cloudy_bay_2012,
                                yld_cloudy_bay_2013,
                                yld_cloudy_bay_2014,
                                yld_cloudy_bay_2015,
                                yld_cloudy_bay_2016,
                                yld_cloudy_bay_2017,
                                yld_cloudy_bay_2018,
                                yld_cloudy_bay_2019)


## remove objects I dont need

rm(
  list = c(
    "yld_cloudy_bay_2004",
    "yld_cloudy_bay_2005",
    "yld_cloudy_bay_2006",
    "yld_cloudy_bay_2007",
    "yld_cloudy_bay_2008",
    "yld_cloudy_bay_2009",
    "yld_cloudy_bay_2010",
    "yld_cloudy_bay_2011",
    "yld_cloudy_bay_2012",
    "yld_cloudy_bay_2013",
    "yld_cloudy_bay_2014",
    "yld_cloudy_bay_2015",
    "yld_cloudy_bay_2016",
    "yld_cloudy_bay_2017",
    "yld_cloudy_bay_2018",
    "yld_cloudy_bay_2019")
  )    


###############    some dates are entered in as & and - also are in different formats      ###########
#######################################################################################################
## Do this step with one big data set ######

#1. deal with the multiple dates seperating & and -
yld_cloudy_bay_2004_19 <- separate(yld_cloudy_bay_2004_19, 
                                harvest_date1, 
                              into = c("day_range", "month"), 
                              sep = "/", remove = FALSE)

yld_cloudy_bay_2004_19 <- separate(yld_cloudy_bay_2004_19, 
                                day_range, 
                                into = c("day"), 
                                sep = "([\\-\\&\\:])", remove = FALSE)

#make it a date again ie make a date clm by using the different clms
yld_cloudy_bay_2004_19$date <- as.Date(paste(yld_cloudy_bay_2004_19$day,
                                             yld_cloudy_bay_2004_19$month,
                                             yld_cloudy_bay_2004_19$year,sep="-"),format = "%d-%m-%Y")


### if I have na in dates fill with harvest date - making a new clm
yld_cloudy_bay_2004_19$date1 <- ifelse(is.na(yld_cloudy_bay_2004_19$date), 
                                       yld_cloudy_bay_2004_19$harvest_date1, NA)
#format this new clm

yld_cloudy_bay_2004_19$date1 <- as.double(yld_cloudy_bay_2004_19$date1)
yld_cloudy_bay_2004_19$date1 <- as.Date(yld_cloudy_bay_2004_19$date1,
                       origin = "1899-12-30") #this is the starting date value in excel
                      
#match the formats 
yld_cloudy_bay_2004_19$date1 <- lubridate::ydm(yld_cloudy_bay_2004_19$date1)

##now both clm the correct format I can try and put them togther
str(yld_cloudy_bay_2004_19)
yld_cloudy_bay_2004_19 <- mutate(yld_cloudy_bay_2004_19,
                              harvest_date = ifelse(is.na(yld_cloudy_bay_2004_19$date), 
                                                    yld_cloudy_bay_2004_19$date1, 
                                                    yld_cloudy_bay_2004_19$date))

yld_cloudy_bay_2004_19$harvest_date <- as.Date(yld_cloudy_bay_2004_19$harvest_date,
                       origin = "1970-01-01") #this is the starting date value in R

str(yld_cloudy_bay_2004_19)
yld_cloudy_bay_2004_19 <- select(yld_cloudy_bay_2004_19,
                              -date,
                              -date1,
                              -day_range,
                              - day,
                              - month)



#####################################################################################################
#### Join the spatial data to the yield data

str(yld_cloudy_bay_2004_19)
str(cloudyBay_site_centroid)



yld_spatial_cloudy_bay_2004_19 <- left_join(yld_cloudy_bay_2004_19,
                                            cloudyBay_site_centroid,
                                            by = c("Code" = "Name"))
str(yld_spatial_cloudy_bay_2004_19)

yld_spatial_cloudy_bay_2004_19 <- select(
  yld_spatial_cloudy_bay_2004_19,
  variety = Variety ,
  Code,
  Block = blk_name,
  year = year,
  harvest_date = harvest_date,
  yield_t_ha = yield_t_ha,
  row_width = Row_spacin,
  vine_spacing = Vine_spaci,
  x_coord = POINT_X,
  y_coord = POINT_Y
  
)
str(yld_spatial_cloudy_bay_2004_19)
yld_spatial_cloudy_bay_2004_19$yield_t_ha <- as.double(yld_spatial_cloudy_bay_2004_19$yield_t_ha)

yld_spatial_cloudy_bay_2004_19 <- mutate(yld_spatial_cloudy_bay_2004_19,
                                  company = "Cloudy_bay",
                                  ID_yr = paste0(Code, "_", year),
                                  julian = as.numeric(format(harvest_date, "%j")),
                                  m_ha_vine = 10000/ row_width,
                                  yield_kg_m = (yield_t_ha *1000)/m_ha_vine,
                                  bunch_weight = NA,
                                  berry_weight = NA,
                                  bunch_numb_m = NA,
                                  pruning_style = NA,
                                  na_count = NA,
                                  brix = NA
                                  
)


yld_spatial_cloudy_bay_2004_19 <- select(yld_spatial_cloudy_bay_2004_19,
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


#remove missing data
yld_spatial_cloudy_bay_2004_19 <- filter(yld_spatial_cloudy_bay_2004_19, x_coord > 0)
str(yld_spatial_cloudy_bay_2004_19)
#remove duplicates
#yld_spatial_cloudy_bay_2004_19 <- distinct(yld_spatial_cloudy_bay_2004_19, ID_yr, .keep_all = TRUE)


getwd()


write_csv(yld_spatial_cloudy_bay_2004_19, "V:/Marlborough regional/working_jaxs/yld_spatial_cloudy_bay_2004_19.csv")
# write_csv(yld_spatial_cloudy_bay_2004_19,
#             "C:/Users/ouz001/working_from_home/NZ_regional_winery_data/yld_spatial_cloudy_bay_2004_19.csv")
          
          
        

