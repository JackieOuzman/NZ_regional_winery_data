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
###############################################################################################
shapefile_WP =  st_read("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/Kuranui_NZ2000.shp")
shapefile_WP_KML <- st_read("V:/Marlborough regional/Regional winery data/Raw_data/Wine Portfolio/Kuranui blocks.kml")
###############################################################################################

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

#write_csv(centroid_df, "V:/Marlborough regional/working_jaxs/Wine_port_centroid_df_check.csv")
rm("centroid", "geom", "shapefile_WP", "shapefile_WP_KML")


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

#Yld_data_GPS <- left_join(Yld_data_all, centroid_df)
Yld_data_GPS <- full_join(Yld_data_all, centroid_df)
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


unique(yld_GPS$variety)

yld_GPS <- dplyr::filter(yld_GPS,
                      variety == "SAB"|
                      is.na(variety) )
#13 sites with no yeild data only have coods
yld_GPS <- dplyr::filter(yld_GPS,
                         variety == "SAB")


yld_GPS <- mutate(yld_GPS,na_count = NA )


############################################################################## 
########################    File to use   ####################################

write_csv(yld_GPS, "V:/Marlborough regional/working_jaxs/July2020/Wine_portfolio_yld_GPS_only_SAB.csv")
############################################################################## 

###########################################################################################################
#Revised  set 26/0/2021
names(yld_GPS)

#just need to make a block 
yld_GPS <- yld_GPS %>% separate(ID_yr, c("Block"), sep = "_", remove = FALSE)

#1. How many sites?
#for each year
yld_GPS %>%
  group_by(year) %>%
  summarise(count = n_distinct(Block))
#overall for the data set from 2014-2019 how many blocks do we have?
yld_GPS %>%
  summarise(count = n_distinct(Block))

#2. For harvest date how many sites per year?
names(yld_GPS)

yld_GPS %>%
  group_by(year) %>%
  summarise(mean_julian_days = mean(julian, na.rm = TRUE),
            min_julian_days = min(julian, na.rm = TRUE),
            max_julian_days = max(julian, na.rm = TRUE),
            sum_na = sum(!is.na(julian)))

#3. For yield kg/m  how many sites per year

yld_GPS %>%
  group_by(year) %>%
  summarise(mean_yield_kg_m = mean(yield_kg_m, na.rm = TRUE),
            min_yield_kg_m = min(yield_kg_m, na.rm = TRUE),
            max_yield_kg_m = max(yield_kg_m, na.rm = TRUE),
            sum_na = sum(!is.na(yield_kg_m)))








#  





