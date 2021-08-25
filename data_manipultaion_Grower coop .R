library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)


##########################################################################################################################
################### 2019 block data ######################################################################################

Grower_coop_V2019 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Grape MASTER V2019.xlsx", 
                                 sheet = "Block Master 2019 ", skip = 2)
str(Grower_coop_V2019)
Grower_coop_V2019_block_info <- dplyr::select(Grower_coop_V2019,
                                         Sub_Region = "Sub Region",
                                         Grower,
                                         Block,
                                         SWNZ_Vineyard_ID = "SWNZ Vineyard ID",
                                         Ha = Hectares,
                                         row_width = "Row Spacing",
                                         vine_Spacing = "Vine Spacing",
                                         vines_ha = "Vines/ha",
                                         "Realignment Number"
                                         )

# remove the no data - eg the summary row at the end.
Grower_coop_V2019_block_info <- filter(Grower_coop_V2019_block_info,
                                       Sub_Region != "NA")
#make a temp ID clm which is the grower and the block

Grower_coop_V2019_block_info <- mutate(Grower_coop_V2019_block_info,
                                       Temp_ID = paste0(Grower, "_", Block))
#just some formatting adding in _
Grower_coop_V2019_block_info$Temp_ID <-str_replace(Grower_coop_V2019_block_info$Temp_ID, ",", "_")
Grower_coop_V2019_block_info$Temp_ID <-str_replace_all(Grower_coop_V2019_block_info$Temp_ID, " ", "_")
Grower_coop_V2019_block_info$Temp_ID <-str_replace_all(Grower_coop_V2019_block_info$Temp_ID, "__", "_")

Grower_coop_V2019_block_info <- mutate(Grower_coop_V2019_block_info,
                                       company = "Grower_coop",
                                       year = 2019,
                                       ID_yr = paste0(Temp_ID, "_", year))


#############################################################################################################
########           harvest date and GPS locations ############################################
############################################################################################################

Grower_coop_V2019_GPS <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Grape MASTER V2019.xlsx", 
                                sheet = "coords")

names(Grower_coop_V2019_GPS) #Realignment Number
names(Grower_coop_V2019_block_info)

#now I can join them....
str(Grower_coop_V2019_block_info) 
str(Grower_coop_V2019_GPS)
Grower_coop_V2019_GPS$`Realignment Number`<- as.numeric(Grower_coop_V2019_GPS$`Realignment Number`)

Grower_coop_V2019 <- full_join(Grower_coop_V2019_block_info, Grower_coop_V2019_GPS, by= "Realignment Number")
#whats missing?

not_joined_Grower_coop_V2019 <- anti_join(Grower_coop_V2019_block_info, Grower_coop_V2019_GPS)
getwd()
#write.csv(not_joined_Grower_coop_V2019, "not_joined_Grower_coop_V2019.csv")

#remove what I dont want..
rm(
   #Grower_coop_V2019,
   Grower_coop_V2019_block_info,
   Grower_coop_V2019_GPS,
   not_joined_Grower_coop_V2019
    )

str(Grower_coop_V2019)
#just the coodinates
str(Grower_coop_V2019_GPS)

Grower_coop_GPS <- select(Grower_coop_V2019,
                          Name,
                          POINT_X,
                          POINT_Y ,
                          Realignment_Number = `Realignment Number`)
   ### change the coordinates to from long and lats to projected data.
   # I have my data in decimal degrees I want to convert it into GDA
mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs


names(Grower_coop_GPS)
Grower_coop_GPS <- filter(Grower_coop_GPS, !is.na(POINT_X))
str(Grower_coop_GPS)
Grower_coop_GPS$POINT_X <- as.double(Grower_coop_GPS$POINT_X)
Grower_coop_GPS$POINT_Y <- as.double(Grower_coop_GPS$POINT_Y)
coordinates(Grower_coop_GPS) <- ~POINT_X+POINT_Y
proj4string(Grower_coop_GPS) <- wgs84CRS   # assume input lat and longs are WGS84
Grower_coop_GPS_1 <- spTransform(Grower_coop_GPS, mapCRS)

glimpse(Grower_coop_GPS_1)
Grower_coop_GPS_1_df = as.data.frame(Grower_coop_GPS_1) #this has the new coordinates projected !YES!!
glimpse(Grower_coop_GPS_1_df)


#so I want to update the coodinates clm in the grower_coop_V2019 and only keep this one.

Grower_coop_V2019 <- select(Grower_coop_V2019,
                                -POINT_X,
                                -POINT_Y)
Grower_coop_V2019 <- rename(Grower_coop_V2019, "Realignment_Number" =`Realignment Number` )

names(Grower_coop_V2019)
names(Grower_coop_GPS_1_df)


Grower_coop_gps_block <- left_join(Grower_coop_V2019, Grower_coop_GPS_1_df, by = "Realignment_Number")
names(Grower_coop_gps_block)                             
Grower_coop_gps_block <- select(Grower_coop_gps_block,
                                Realignment_Number,
                                ID_yr,
                                Ha,
                                row_width,
                                vine_Spacing,
                                Grower ,
                                Block,
                                POINT_X, POINT_Y)

rm(Grower_coop_GPS_1, Grower_coop_GPS, Grower_coop_V2019)
rm(mapCRS, wgs84CRS, Grower_coop_GPS_1_df)


###########################################################################################################
################     add in the 2019 yield data ##########################################################
  

Grower_coop_V2019_yld_data <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Grape MASTER V2019.xlsx", 
                                sheet = "Crop Estimate MASTER 2019", skip = 2)
str(Grower_coop_V2019_yld_data)

Grower_coop_V2019_yld_data <- dplyr::select(Grower_coop_V2019_yld_data,
                                            Sub_Region = "Sub Region",
                                            Grower,
                                            Block,
                                            Ha = Hectares,
                                            vines_ha = "Vines / ha",
                                            bunches_per_vine = "Average bunches / vine (Nov)",
                                            nunb_bunches_collected ="Number bunches collected",
                                            total_weight_bunches = "Total weight of bunches (g)",
                                            total_weight_berry_sample ="Total weight of berry sample (g)" ,
                                            total_weight_berry = "Total weight of berry sample (g)",
                                            no_berry_in_sample = "No. berry's in sample (5 berry's / bunch)",
                                            yield_t_ha = "Harvested T/ha",
                                            "Realignment_Number" = "Realignment number",
                                            berry_weight  = `V2019 Actual Berry wgt (gm)`
                                            
                                             )

Grower_coop_V2019_yld_data <- filter(Grower_coop_V2019_yld_data,
                                       Sub_Region != "NA")
names(Grower_coop_V2019_yld_data)
#make a temp ID clm which is the grower and the block

Grower_coop_V2019_yld_data <- mutate(Grower_coop_V2019_yld_data,
                                       Temp_ID = paste0(Grower, "_", Block))
#just some formatting adding in _
Grower_coop_V2019_yld_data$Temp_ID <-str_replace(Grower_coop_V2019_yld_data$Temp_ID, ",", "_")
Grower_coop_V2019_yld_data$Temp_ID <-str_replace_all(Grower_coop_V2019_yld_data$Temp_ID, " ", "_")
Grower_coop_V2019_yld_data$Temp_ID <-str_replace_all(Grower_coop_V2019_yld_data$Temp_ID, "__", "_")

Grower_coop_V2019_yld_data <- mutate(Grower_coop_V2019_yld_data,
                                       company = "Grower_coop",
                                       year = 2019,
                                       ID_yr = paste0(Temp_ID, "_", year))


str(Grower_coop_V2019_yld_data)
### Bring in the block data 

#str(Grower_coop_V2019_block_info)
str(Grower_coop_gps_block)


names(Grower_coop_V2019_yld_data)
names(Grower_coop_gps_block)

temp_Grower_coop_gps_block <- select(Grower_coop_gps_block,
                                     Realignment_Number,
                                     POINT_X,
                                     POINT_Y,
                                     vine_Spacing,
                                     row_width)

Grower_coop_V2019_join <- full_join(temp_Grower_coop_gps_block, Grower_coop_V2019_yld_data, by = "Realignment_Number")                                                     
rm(Grower_coop_V2019_yld_data, temp_Grower_coop_gps_block)

names(Grower_coop_V2019_join)

#### Add in the calulations
Grower_coop_V2019_join <- mutate(Grower_coop_V2019_join,
                                     harvest_date= NA,
                                     julian = NA, #julian = as.numeric(format(harvest_date, "%j")),
                                     m_ha_vine = 10000/ row_width,
                                     yield_kg_m = (yield_t_ha *1000)/m_ha_vine,
                                     bunch_weight = total_weight_bunches / nunb_bunches_collected, #the data sheet has -4 from this cal ?why
                                     berry_weight_1 = total_weight_berry / no_berry_in_sample,
                                     berry_per_bunch = bunch_weight / berry_weight_1,
                                     bunches_per_vine = NA, #??not sure I can cal this?? help
                                     pruning_style = NA,
                                     brix = NA,
                                     meter_row_per_ha = 10000/row_width,
                                     yld_per_m_row_kg = (yield_t_ha *1000) / 10000/row_width,
                                     bunch_m = (yld_per_m_row_kg * 1000)/ bunch_weight)
                                     



str(Grower_coop_V2019_join)
Grower_coop_V2019 <- select(Grower_coop_V2019_join,
                                             company,
                                             ID_yr, 
                                             #variety,
                                             x_coord = POINT_X,
                                             y_coord = POINT_Y ,
                                             year,
                                             harvest_date, #missing
                                             julian, #missing
                                             yield_t_ha,
                                             yield_kg_m,
                                             brix, #missing
                                             bunch_m,
                                             #pruning_style,
                                             row_width ,
                                             vine_spacing = vine_Spacing,
                                             Realignment_Number,
                                             bunch_weight,
                                             berry_weight 
)




names(Grower_coop_V2019)


##Remove the df I dont need anymore
rm(Grower_coop_V2019_join)
  

#remove the rows with the missing data

Grower_coop_V2019 <- filter(Grower_coop_V2019,
                            !is.na(year))

##############################################################################################################################
############ 2014-2018 data ######################################################################################################


Harvest_data_2014_2017 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Harvest data 2014-2017.xlsx", 
                                     sheet = "2014_2017_codes")

names(Harvest_data_2014_2017)
names(Grower_coop_V2019)

#filter out data so I am keeping sites that have a good match to the 2019 data. clm 

Harvest_data_2014_2017 <- filter(Harvest_data_2014_2017,
                                 match_of_Realignment_and_ha == "y")


#Divide up per year to make data long
Harvest_data_2014 <- dplyr::select(
   Harvest_data_2014_2017,
   "Realignment Number"  = "Realignment Number best match",
   "Grower" ,
   ha = "Hectares",
   yld = "Fruit Produced 2014",
   yield_t_ha = "Tonnes per hectare 2014" ,
   brix = "2014 Brix"
) %>%
   mutate(year = 2014)

Harvest_data_2015 <- dplyr::select(
   Harvest_data_2014_2017,
   "Realignment Number"  = "Realignment Number best match",
   "Grower" ,
   ha = "Hectares",
   yld = "Fruit Produced 2015",
   yield_t_ha = "Tonnes per hectare 2015" ,
   brix = "2015 Brix"
) %>%
   mutate(year = 2015)

Harvest_data_2016 <- dplyr::select(
   Harvest_data_2014_2017,
   "Realignment Number"  = "Realignment Number best match",
   "Grower" ,
   ha = "Hectares",
   yld = "Fruit Produced 2016",
   yield_t_ha = "Tonnes per hectare 2016" ,
   brix = "2016 Brix"
) %>%
   mutate(year = 2016)

Harvest_data_2017 <- dplyr::select(
   Harvest_data_2014_2017,
   "Realignment Number"  = "Realignment Number best match",
   "Grower" ,
   ha = "Hectares",
   yld = "Fruit Produced 2017",
   yield_t_ha = "Tonnes per hectare 2017" ,
   brix = "2017 Brix"
) %>%
   mutate(year = 2017)

#join them all togther again..
 yld_data2014_2017 <- bind_rows(Harvest_data_2014,
                                Harvest_data_2015,
                                Harvest_data_2016,
                                Harvest_data_2017)
 rm("Harvest_data_2014",
    "Harvest_data_2015",
    "Harvest_data_2016",
    "Harvest_data_2017")
 
yld_data2014_2017$yield_t_ha <- replace(yld_data2014_2017$yield_t_ha, 
                                      yld_data2014_2017$yield_t_ha == 0, NA)
yld_data2014_2017$yld <- replace(yld_data2014_2017$yld, 
                                      yld_data2014_2017$yld == 0, NA)

#check to see if the t/ha and ha are a good - match some sites might have an error but
# yield in t/ha looks sensible - so keep it?
 yld_data2014_2017 <- mutate(yld_data2014_2017,
                            check_t_ha = yld /ha)
str(yld_data2014_2017)
yld_data2014_2017 <- mutate(yld_data2014_2017,
                            match_ha = case_when(
                               check_t_ha == yield_t_ha  ~ 1,
                               TRUE ~ 0
                            ))
names(yld_data2014_2017)
site_with_t_ha_error <- filter(yld_data2014_2017, !is.na(yld))
site_with_t_ha_error <- filter(site_with_t_ha_error,
                               match_ha == 0)



### add in the coodinates based on relaingnmnet numbers
str(yld_data2014_2017)
str(Grower_coop_gps_block)
yld_data2014_2017 <- rename(yld_data2014_2017,
                            Realignment_Number =  `Realignment Number`)


yld_data2014_2017$Realignment_Number<- as.numeric(yld_data2014_2017$Realignment_Number)

#yld_data2014_2017 <- full_join(yld_data2014_2017,Grower_coop_gps_block )
yld_data2014_2017 <- full_join(yld_data2014_2017,Grower_coop_gps_block, by = "Realignment_Number")


names(yld_data2014_2017)
yld_data2014_2017 <- dplyr::select(yld_data2014_2017,
                                   -"Ha" , #this is from the 2019 GPS data so I will use the 2014-2017
                                  - "Block", #this is from the 2019 GPS data so I will use the 2014-2017
                                  - "Grower.y", 
                                  -"check_t_ha" ,
                                  - "match_ha" )





#### Add in the calulations
yld_data2014_2017 <- mutate(yld_data2014_2017,
                                 harvest_date= NA,
                                 julian = NA, #julian = as.numeric(format(harvest_date, "%j")),
                                 m_ha_vine = 10000/ row_width,
                                 yield_kg_m = (yield_t_ha *1000)/m_ha_vine,
                                 bunch_weight = NA, 
                                 berry_per_bunch = NA ,
                                 bunches_per_vine = NA, #??not sure I can cal this?? help
                                 pruning_style = NA,
                                 brix ,
                                 meter_row_per_ha = 10000/row_width,
                                 yld_per_m_row_kg = (yield_t_ha *1000) / 10000/row_width,
                                 bunch_m = NA,
                                 berry_weight = NA)


str(yld_data2014_2017)
str(Grower_coop_gps_block)



yld_data2014_2017 <- mutate(yld_data2014_2017,
                            company = "grower_coop",
                            ID_yr = paste0("Realignment_numb_", Realignment_Number, 
                                           "_year_", year ),
                            variety = "SAB")
names(yld_data2014_2017)

yld_data2014_2017 <- select(
   yld_data2014_2017,
   company,
   ID_yr,
   year, 
   variety,
    POINT_X,
    POINT_Y ,
   year,
   harvest_date,
   julian,
   bunch_weight,
   yield_t_ha,
   yield_kg_m,
   brix, 
   bunch_m,
   pruning_style,
   row_width ,
   vine_spacing = vine_Spacing,
   #Block =Grower.x,
  # Block = Name ,
   berry_weight
)

names(yld_data2014_2017)

rm(   "site_with_t_ha_error")

#remove the rows with missing yld data

 
yld_data2014_2017 <- filter(yld_data2014_2017,
                               !is.na(year))

# join the 2019 and 2014-2018 data togther

str(yld_data2014_2017)
str(Grower_coop_V2019) #add in a fw more clms
Grower_coop_V2019 <- mutate(Grower_coop_V2019,
                            company = "grower_coop",
                            ID_yr = paste0("Realignment_numb_", Realignment_Number, "_year_", year ),
                            variety = "SAB",
                            pruning_style = NA,
                            )

names(Grower_coop_V2019)
Grower_coop_V2019 <- select(
   Grower_coop_V2019,
   company,
   ID_yr,
   year, 
   variety,
   POINT_X = x_coord ,
   POINT_Y = y_coord  ,
   year,
   harvest_date,
   julian,
   bunch_weight,
   yield_t_ha,
   yield_kg_m,
   brix, 
   bunch_m,
   pruning_style,
   row_width ,
   vine_spacing,
   berry_weight
   
)
names(Grower_coop_V2019)
names(yld_data2014_2017)

V2019_2014_to_2017 <- bind_rows(Grower_coop_V2019, yld_data2014_2017)

names(V2019_2014_to_2017)

V2019_2014_to_2017 <- filter(V2019_2014_to_2017,
               !is.na(yield_t_ha) | !is.na(brix))





##############################################################################################################################
############ 2018 data ######################################################################################################


Grower_coop_V2018_part1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Member Harvest Summary 2018 01052018.xlsx", 
                                      sheet = "Member Harvest Summary 2018")


#- umm that work what about Date clm that has name

Grower_coop_V2018_part1 <- filter(Grower_coop_V2018_part1,
               !is.na(Date))
#only keep values that are string characters
Grower_coop_V2018_part1 <- Grower_coop_V2018_part1 %>% 
   filter(str_detect(Date, "[:alpha:]"))

Grower_coop_V2018_part1 <- select(Grower_coop_V2018_part1,
               Block ,
               yield_T = `Actual Yield (T)`,
               brix = `Brix (deg)`)

##### Get the dates and average for each block

Grower_coop_V2018_part2 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Member Harvest Summary 2018 01052018.xlsx", 
                                      sheet = "Shelley - TWG Fruit Receival An")

str(Grower_coop_V2018_part2)
Grower_coop_V2018_part2 <- group_by(Grower_coop_V2018_part2, Block) %>% 
   summarise(
      Date_ave = mean(Date )
   )

##### Get the relaingnnet codes to use
Grower_coop_V2018_part3 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Member Harvest Summary 2018 01052018.xlsx", 
                                      sheet = "Member Harvest Summary 2018 Jax")

str(Grower_coop_V2018_part3)
Grower_coop_V2018_part3 <- select(Grower_coop_V2018_part3,
                                  Block ,
                                  realignmnet_number = `realignmnet number to use`,
                                  ha,
                                  row_width =`Row Spacing`,
                                  vine_spacing = `Vine Spacing`
                                  )

 

#keep the ones with values - some I could work out or Jhonny said not to use!
Grower_coop_V2018_part3 <- filter(Grower_coop_V2018_part3,
                                  !is.na(realignmnet_number))



####################################################################################################################
# join the three parts togther

dim(Grower_coop_V2018_part1)
dim(Grower_coop_V2018_part2)
dim(Grower_coop_V2018_part3)

#join part 1 to part 2
Grower_coop_V2018 <- full_join(Grower_coop_V2018_part1, Grower_coop_V2018_part2)
Grower_coop_V2018 <- full_join(Grower_coop_V2018, Grower_coop_V2018_part3)


### double check a few things - why dont I have dates and realignmnet number for some of my sites?
#seems that we dont have harvest dates for some sites, not sure why this is the raw data reflects this.

rm(Grower_coop_V2018_part1,Grower_coop_V2018_part2, Grower_coop_V2018_part3)






#add in the gPS data
 str(Grower_coop_gps_block)
 str(Grower_coop_V2018)
 Grower_coop_V2018 <- rename(Grower_coop_V2018,
                             Realignment_Number = realignmnet_number )
 temp_Grower_coop_gps_block <- select(Grower_coop_gps_block,
                                      Realignment_Number,
                                      POINT_X,
                                      POINT_Y)
 
 Grower_coop_V2018 <- full_join(Grower_coop_V2018, temp_Grower_coop_gps_block)
 Grower_coop_V2018 <- filter(Grower_coop_V2018, !is.na(POINT_X))
 Grower_coop_V2018 <- filter(Grower_coop_V2018, !is.na(Block))
 Grower_coop_V2018 <- select(Grower_coop_V2018, -Block)
 
rm(temp_Grower_coop_gps_block)
### add the caluations and missing data
#first add the correct clm 

Grower_coop_V2018 <- mutate(Grower_coop_V2018,
                            harvest_date = Date_ave,
                            yield_t_ha = yield_T /ha,
                            year = 2018
                            )
#### Add in the calulations
Grower_coop_V2018 <- mutate(Grower_coop_V2018,
                            harvest_date,
                            julian = as.numeric(format(harvest_date, "%j")),
                            m_ha_vine = 10000/ row_width,
                            yield_kg_m = (yield_t_ha *1000)/m_ha_vine,
                            bunch_weight = NA, 
                            berry_per_bunch = NA ,
                            bunches_per_vine = NA, #??not sure I can cal this?? help
                            pruning_style = NA,
                            brix ,
                            meter_row_per_ha = 10000/row_width,
                            yld_per_m_row_kg = (yield_t_ha *1000) / 10000/row_width,
                            bunch_m = NA,
                            berry_weight = NA)
str(Grower_coop_V2018)

Grower_coop_V2018 <- mutate(
   Grower_coop_V2018,
   company = "grower_coop",
   ID_yr = paste0("Realignment_numb_", Realignment_Number, "_year_", year),
   variety = "SAB"
)

Grower_coop_V2018 <- select(
   Grower_coop_V2018,
   company,
   ID_yr,
   year, 
   variety,
   POINT_X,
   POINT_Y ,
   year,
   harvest_date,
   julian,
   bunch_weight,
   yield_t_ha,
   yield_kg_m,
   brix, 
   bunch_m,
   pruning_style,
   row_width ,
   vine_spacing,
   #Block = Name ,
   berry_weight
)


##################################################################################################################
#Join the 2018 data to the rest.

dim(Grower_coop_V2018)
dim(V2019_2014_to_2017)
View(V2019_2014_to_2017)
V2014_to_2019 <- bind_rows(Grower_coop_V2018, V2019_2014_to_2017)
V2014_to_2019 <- V2014_to_2019 %>% 
   mutate( bunch_numb_m = NA,
           x_coord = POINT_X,
           y_coord = POINT_Y ,
           na_count = NA)


V2014_to_2019 <- V2014_to_2019 %>% separate(ID_yr, c("temp", "temp2","Block"), sep = "_", remove = FALSE)

V2014_to_2019 <- V2014_to_2019 %>% dplyr::select(-temp, -temp2)
## add in some guess work for the blocks that have missing row and vine spacing.
names(V2014_to_2019)

V2014_to_2019 <- V2014_to_2019 %>% 
   mutate(row_width = case_when(
      Block == "200" ~ 2.80,
      Block == "201" ~ 2.50,
      Block == "202" ~ 3.0,
      Block == "203" ~ 2.70,
      Block == "204" ~ 2.70,
      Block == "205" ~ 3.0,
      Block == "206" ~ 2.70,
      Block == "208" ~ 2.80,
      TRUE ~ row_width))
V2014_to_2019 <- V2014_to_2019 %>% 
   mutate(vine_spacing = case_when(
      Block == "200" ~ 1.80,
      Block == "201" ~ 1.50,
      Block == "202" ~ 1.8,
      Block == "203" ~ 2,
      Block == "204" ~ 1.8,
      Block == "205" ~ 1.8,
      Block == "206" ~ 1.8,
      Block == "208" ~ 1.8,
      TRUE ~ vine_spacing))   
   
#Now that I have some more info on the row and vine spacing I need to cal the yield kg again   
V2014_to_2019 <- mutate(V2014_to_2019,
                            m_ha_vine = 10000/ row_width,
                            yield_kg_m = (yield_t_ha *1000)/m_ha_vine,
                            meter_row_per_ha = 10000/row_width,
                            yld_per_m_row_kg = (yield_t_ha *1000) / 10000/row_width
                            )


write.csv(V2014_to_2019,
          "V:/Marlborough regional/working_jaxs/July2020/grower_coop_V2014_to_2019.csv")


########################################################################################################################

#Revised Giesen_2020_spatial_yld_upadted2020_vs2 21/0/2021
names(V2014_to_2019)

#just need to make a block 


# Giesen_2020_spatial_yld_upadted2020_vs2 <- Giesen_2020_spatial_yld_upadted2020_vs2 %>% separate(ID_yr, c("Block"), sep = "_", remove = FALSE)

#1. How many sites?
#for each year
V2014_to_2019 %>%
   group_by(year) %>%
   summarise(count = n_distinct(Block))
#overall for the data set from 2014-2019 how many blocks do we have?
V2014_to_2019 %>%
   summarise(count = n_distinct(Block))

#2. For harvest date how many sites per year?

V2014_to_2019 %>%
   group_by(year) %>%
   summarise(mean_julian_days = mean(julian, na.rm = TRUE),
             min_julian_days = min(julian, na.rm = TRUE),
             max_julian_days = max(julian, na.rm = TRUE),
             sum_na = sum(!is.na(julian)))

#3. For yield kg/m  how many sites per year

V2014_to_2019 %>%
   group_by(year) %>%
   summarise(mean_yield_kg_m = mean(yield_kg_m, na.rm = TRUE),
             min_yield_kg_m = min(yield_kg_m, na.rm = TRUE),
             max_yield_kg_m = max(yield_kg_m, na.rm = TRUE),
             sum_na = sum(!is.na(yield_kg_m)))
