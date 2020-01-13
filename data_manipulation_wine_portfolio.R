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
#### Yld data     ########################################################################
#### as a function             ###########################################################
##########################################################################################


#for 2014-2018
#Harvest yield data
yld_import_function <- function(file_name, file_name2, year_file){
  
Yld_data <-   read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/",file_name, ".xlsx"),
                              year_file ,  col_names = FALSE,  range = "A5:s30" )

header1 <- read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/",file_name, ".xlsx"),
                      year_file ,  col_names = FALSE,  range = "A1:s1" )



names(Yld_data) <- header1
#clm assign names that have meregd excel cells
colnames(Yld_data)[1] <- "VAR"
colnames(Yld_data)[2] <- "GRW"
colnames(Yld_data)[3] <- "BLK"
colnames(Yld_data)[4] <- "Ha"
colnames(Yld_data)[5] <- "CLN"
colnames(Yld_data)[6] <- "Rows"
colnames(Yld_data)[7] <- "BLK_CODE"
colnames(Yld_data)[8] <- "Est_tonnes"
colnames(Yld_data)[9] <- "Actual_tonnes"
colnames(Yld_data)[10] <- "Actual_t_ha"
colnames(Yld_data)[11] <- "Av_Kg_Vine"

colnames(Yld_data)[16] <- "Harvest_date"

colnames(Yld_data)[17] <- "Brix"
colnames(Yld_data)[18] <- "pH"
colnames(Yld_data)[19] <- "TA"
str(Yld_data)

#make sure harvest_date is as a date
#Yld_data$Harvest_date <- as.POSIXct(Yld_data$Harvest_date)


Yld_data <- select(
  Yld_data,
  VAR,
  Block_name = BLK_CODE,
  # new name = old name
  Yield_t_ha = Actual_t_ha,
  Bunch_wt = `Av. Bunch Weight at Harvest (g)`,
  Berry_wt = `Av .Berry Weight at Harvest (g)`,
  Harvest_date ,
  Brix) %>% 
  mutate(
    julian = as.numeric(format(Harvest_date, "%j")),
    year = year_file,
    company = "wine_portfolio")
#remove the summary data rows.
#unique(Yld_data_2014$Block_name)
Yld_data <- filter(Yld_data, Block_name != "NA")

#yield prediction data

#Yld_pred_data <-   read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/Kuranui Yield Prediction 2014-18.xlsx",
  #                            "2014" ,  col_names = FALSE,  range = "A3:L28" )
Yld_pred_data <-   read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/",file_name2, ".xlsx"),
                         year_file ,  col_names = FALSE,  range = "A3:p28" )


Yld_pred_data <- select(Yld_pred_data,
                        Block_name = `...9`,
                        pruning_style = `...12`,
                        Bunches_per_Vine_nov = `...16`)

#remove the summary data rows.

Yld_pred_data <- filter(Yld_pred_data, Block_name != "NA")

#Join the yield data to prune style data
Yld_data_all <- left_join(Yld_data, Yld_pred_data)


#Join the spatial data

Yld_data_GPS <- left_join(Yld_data_all, centroid_df)

}



#for 2019
yld_import_2019_function <- function(file_name, file_name2, year_file){
  
  Yld_data <-   read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/",file_name, ".xlsx"),
                           year_file ,  col_names = FALSE,  range = "A5:s30" )
  
  header1 <- read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/",file_name, ".xlsx"),
                        year_file ,  col_names = FALSE,  range = "A1:s1" )
  
  
  
  names(Yld_data) <- header1
  #clm assign names that have meregd excel cells
  colnames(Yld_data)[1] <- "VAR"
  colnames(Yld_data)[2] <- "GRW"
  colnames(Yld_data)[3] <- "BLK"
  colnames(Yld_data)[4] <- "Ha"
  colnames(Yld_data)[5] <- "CLN"
  colnames(Yld_data)[6] <- "Rows"
  colnames(Yld_data)[7] <- "BLK_CODE"
  colnames(Yld_data)[8] <- "Est_tonnes"
  colnames(Yld_data)[9] <- "Actual_tonnes"
  colnames(Yld_data)[10] <- "Actual_t_ha"
  colnames(Yld_data)[11] <- "Av_Kg_Vine"
  
  colnames(Yld_data)[16] <- "Harvest_date"
  
  colnames(Yld_data)[17] <- "Brix"
  colnames(Yld_data)[18] <- "pH"
  colnames(Yld_data)[19] <- "TA"
  str(Yld_data)
  
  #make sure harvest_date is as a date
  #Yld_data$Harvest_date <- as.POSIXct(Yld_data$Harvest_date)
  
  
  Yld_data <- select(
    Yld_data,
    VAR,
    Block_name = BLK_CODE,
    # new name = old name
    Yield_t_ha = Actual_t_ha,
    Bunch_wt = `Av. Bunch Weight at Harvest (g)`,
    Berry_wt = `Av .Berry Weight at Harvest (g)`,
    Harvest_date ,
    Brix) %>% 
    mutate(
      julian = as.numeric(format(Harvest_date, "%j")),
      year = year_file,
      company = "wine_portfolio")
  #remove the summary data rows.
  #unique(Yld_data_2014$Block_name)
  Yld_data <- filter(Yld_data, Block_name != "NA")
  
  

  Yld_pred_data <-   read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/",file_name2, ".xlsx"),
                                "file_name2" ,  col_names = FALSE,  range = "A3:p28" )
  
  
  Yld_pred_data <- select(Yld_pred_data,
                          Block_name = `...9`,
                          pruning_style = `...12`,
                          Bunches_per_Vine_nov = `...16`)
  
  #remove the summary data rows.
  
  Yld_pred_data <- filter(Yld_pred_data, Block_name != "NA")
  
  #Join the yield data to prune style data
  Yld_data_all <- left_join(Yld_data, Yld_pred_data)
  
  
  #Join the spatial data
  
  Yld_data_GPS <- left_join(Yld_data_all, centroid_df)
  
}

### use the function
yld_2014 <- yld_import_function("Kuranui Harvest data 2014-18", "Kuranui Yield Prediction 2014-18", "2014")
yld_2015 <- yld_import_function("Kuranui Harvest data 2014-18", "Kuranui Yield Prediction 2014-18","2015")
yld_2016 <- yld_import_function("Kuranui Harvest data 2014-18", "Kuranui Yield Prediction 2014-18", "2016")
yld_2017 <- yld_import_function("Kuranui Harvest data 2014-18", "Kuranui Yield Prediction 2014-18", "2017")
yld_2018 <- yld_import_function("Kuranui Harvest data 2014-18", "Kuranui Yield Prediction 2014-18", "2018")
yld_2019 <- yld_import_function("2019 Intake Harvest Data", "V19 Yield Predictions_not_as_object", "2019")


                             

str(yld_2019)



###############################################################################################################
### bind all the years together

yld_GPS <- rbind(yld_2014,
                 yld_2015,
                 yld_2016,
                 yld_2017,
                 yld_2018, 
                 yld_2019)
str(yld_GPS)

################################################################################################################
 unique(yld_GPS$VAR)
# Sauvignon Blanc - 2.2m x 1.8m = 2525 vines/ha (SAB)
# Pinot Noir - 2.2m x 1.25 = 3636 vines/ha (PIN)
# Pinot Gris - 2.2m x 1.5m = 3030 vines/ha (PIG)

# add in vine spacing and row spacing
yld_GPS <- mutate(yld_GPS, 
                  vine_spacing = case_when(
                  VAR == "PIN" ~ 1.25,
                  VAR == "PIG" ~ 1.5,
                  VAR == "SAB" ~ 1.8),
                  row_width = 2.2
                  )
  
  
unique(yld_GPS$vine_spacing)
unique(yld_GPS$row_width)
             

yld_GPS <- mutate(yld_GPS,
               ID_yr = paste0(Block_name, "_", year),
               yield_kg_m = (Yield_t_ha * 1000) / (10000/row_width), #check this cal
               bunch_numb_m = Bunches_per_Vine_nov / vine_spacing,
               ID_yr = paste0(Block_name, "_", year)
               
               )


yld_GPS <- mutate(yld_GPS,
               year = as.double(year))


yld_GPS <- select(yld_GPS,
                           company,
                           ID_yr,
                           variety = VAR,
                           x_coord = POINT_X,
                           y_coord = POINT_Y,
                           year ,
                           harvest_date = Harvest_date ,
                           julian, 
                           yield_t_ha = Yield_t_ha,
                           yield_kg_m ,
                           brix = Brix,
                           bunch_weight = Bunch_wt,
                           berry_weight = Berry_wt,
                           bunch_numb_m ,
                           pruning_style ,
                           row_width ,
                           vine_spacing)

str(yld_GPS)
dim(yld_GPS)


######################################################################################################################
################                         view and summaries DF 2014 -2019                            #################
######################################################################################################################

#how many entries with and without GPS for all years
dim(yld_GPS)#144
yld_GPS_noGPS_SAB <- filter(yld_GPS, variety       == "SAB" )
dim(yld_GPS_noGPS_SAB)#71

#how many entries with GPS for all years
yld_GPS_only <- filter(yld_GPS,  x_coord       >0)
dim(yld_GPS_only) #136
#how many site are SAU?
unique(yld_GPS_only$variety)
#SAB
yld_GPS_only_SAB <- filter(yld_GPS_only, variety == "SAB" )
dim(yld_GPS_only_SAB) #70



glimpse(yld_GPS_only_SAB) #70 records
max(yld_GPS_only_SAB$year) #2019
min(yld_GPS_only_SAB$year) #2014
#how many records are SAU with GPS
count(filter(yld_GPS_only_SAB,   x_coord >0)) #70



#how many records with GPS pts all varieties
glimpse(yld_GPS_only  )#136 records
#how many records with GPS pts all varieties
count(filter(yld_GPS_only,   x_coord >0)) #136

filter(yld_GPS_only,   x_coord >0) %>% 
  ggplot( aes(variety ))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites with GPS coordinates")

#how many sites with GPS pts by Variety by year
filter(yld_GPS_only,  x_coord >0) %>% 
  ggplot( aes(variety ))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)




#create a new variable year_as_factor
yld_GPS_only_SAB$year_factor <- as.factor(yld_GPS_only_SAB$year)

#filter data for Sauvignon Blanc
yld_GPS_only_SAB

#how many sites for Sauvignon Blanc by year
filter(yld_GPS_only_SAB,  x_coord >0) %>% 
  group_by(year) %>% 
  count() # 2014 = 12, 2015 =12, 2016 =12, 2017 =10, 2018 =12, 2019 =12

####################################################################################################

yld_GPS_only_SAB$na_count <- apply(is.na(yld_GPS_only_SAB), 1, sum)

str(yld_GPS_only_SAB)

#how many sites for Sauvignon Blanc have missing data - how much missing data?
filter(yld_GPS_only_SAB,  x_coord >0) %>% 
  ggplot( aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")
#how many sites for Sauvignon Blanc have missing data - missing data grouped together?
filter(yld_GPS_only_SAB,  x_coord >0) %>%
  ggplot( aes(na_count))+
  geom_bar()+
  #xlim(0,10)+
  scale_x_continuous(breaks =  c(0,2,4,6,8,10))+
  facet_wrap(~year_factor)+
  theme_bw()+
  labs(x = "number of na counts per entry",
       y= "Counts of missing data entries NA")

########################################################################################################

#check stuff 


glimpse(yld_GPS_only_SAB)
#julian days
filter(yld_GPS_only_SAB,  x_coord >0) %>%
  ggplot( aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
#yield_t_ha
filter(yld_GPS_only_SAB,  x_coord >0) %>%
  ggplot( aes(year_factor,  yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")
#yield_kg_m
filter(yld_GPS_only_SAB,  x_coord >0) %>%
  ggplot( aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")

#yield_kg_m filter out zeros
filter(yld_GPS_only_SAB,  x_coord >0) %>%
  filter(yield_kg_m != 0) %>% 
  ggplot( aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")


#brix - too many zero
filter(yld_GPS_only_SAB,  x_coord >0) %>%
  ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


#brix - filter out high values
filter(yld_GPS_only_SAB,  x_coord >0) %>%
  filter(brix <40) %>% 
  ggplot( aes(year_factor, brix ))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")



############################################################################## 
########################    File to use   ####################################
yld_GPS_only_SAB <- filter(yld_GPS_only_SAB,  x_coord >0)
str(yld_GPS_only_SAB)

yld_GPS_only_SAB <- select(yld_GPS_only_SAB, -year_factor)
glimpse(yld_GPS_only_SAB)



                                          
                                          
                                          
                           
                           


write_csv(yld_GPS_only_SAB, "V:/Marlborough regional/working_jaxs/Wine_portfolio_yld_GPS_only_SAB.csv")
############################################################################## 