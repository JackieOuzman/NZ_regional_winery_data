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

Grower_coop_V2019_GPS <- rename(Grower_coop_V2019_GPS,
                                "Realignment Number" = "Site_numb")

#now I can join them....
names(Grower_coop_V2019_block_info) 
names(Grower_coop_V2019_GPS)
Grower_coop_V2019 <- left_join(Grower_coop_V2019_block_info, Grower_coop_V2019_GPS, by= "Realignment Number")
#whats missing?

not_joined_Grower_coop_V2019 <- anti_join(Grower_coop_V2019_block_info, Grower_coop_V2019_GPS)
getwd()
#write.csv(not_joined_Grower_coop_V2019, "not_joined_Grower_coop_V2019.csv")

#remove what I dont want..
rm(
   #Grower_coop_V2019,
   Grower_coop_V2019_block_info,
   #Grower_coop_V2019_GPS,
   not_joined_Grower_coop_V2019
    )

str(Grower_coop_V2019)
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
                                            "Realignment_number" = "Realignment number"
                                            
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
str(Grower_coop_V2019)
#just keep a few clms 

 Grower_coop_V2019_selection <- dplyr::select(Grower_coop_V2019,
                                                         #ID_yr,
                                                         SWNZ_Vineyard_ID,
                                                         row_width ,
                                                         vine_Spacing ,
                                                         "Realignment_number" = "Realignment Number" ,
                                                         POINT_X,
                                                         POINT_Y
                                                         )


names(Grower_coop_V2019_yld_data)
names(Grower_coop_V2019_selection)


Grower_coop_V2019_join <- full_join(Grower_coop_V2019_selection, Grower_coop_V2019_yld_data)                                                     
                                                     

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
                                     berry_weight_2 = (yield_t_ha/vines_ha/bunches_per_vine/berry_per_bunch)*1000000, #this is in the data sheet - check
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
                                             Realignment_number
)



Grower_coop_V2019


##Remove the df I dont need anymore
rm(#Grower_coop_V2019
   #Grower_coop_V2019_GPS,
   Grower_coop_V2019_join,
   Grower_coop_V2019_selection, 
   Grower_coop_V2019_yld_data)
  


##############################################################################################################################
############ 2018 data ######################################################################################################


Grower_coop_V2018_part1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Member Harvest Summary 2018 01052018.xlsx", 
                                                   sheet = "Shelley - TWG Fruit Receival An")
#remove the missing rows
Grower_coop_V2018_part1 <- filter(Grower_coop_V2018_part1,
                                  Block != "NA")
str(Grower_coop_V2018_part1)
Grower_coop_V2018_part1 <- dplyr::select(Grower_coop_V2018_part1, Vineyard,
                                         Block,
                                         brix = "Brix (deg)",
                                         Date)

Grower_coop_V2018_part1 <- mutate(Grower_coop_V2018_part1,
                                  ID_yr = paste0( Block,  "_2018")) #paste0(Grower, "_", Block))

#average the data per block
Grower_coop_V2018_part1_av <- Grower_coop_V2018_part1 %>%
  group_by(Block) %>% 
           summarise(Date = mean(Date),
                     Brix = mean(brix, na.rm = TRUE))


Grower_coop_V2018_part2 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Member Harvest Summary 2018 01052018.xlsx", 
                                                   sheet = "Member Harvest Summary 2018")
#the date clm has a mix of dates and names make 2 clms and code properly
str(Grower_coop_V2018_part2)
Grower_coop_V2018_part2 <- mutate(Grower_coop_V2018_part2, Date_name = Date)
Grower_coop_V2018_part2 <- Grower_coop_V2018_part2 %>% mutate(id_ref = row_number())                                  
Grower_coop_V2018_part2$Date_name <- str_replace_all(Grower_coop_V2018_part2$Date_name,
                                                              "[:digit:]", "")

Grower_coop_V2018_part2$Date_name <-na_if(Grower_coop_V2018_part2$Date_name, ".")
str(Grower_coop_V2018_part2)
Grower_coop_V2018_part2 <- dplyr::select(Grower_coop_V2018_part2, Vineyard,
                                         Block,
                                         brix = "Brix (deg)",
                                         Date_name, id_ref
                                         )



Grower_coop_V2018_part2_a <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Member Harvest Summary 2018 01052018.xlsx", 
                                                   col_types = c("date", "text", "text", 
                                                                 "text", "numeric", "text", "numeric", 
                                                                 "numeric", "text", "numeric", "numeric", 
                                                                 "numeric", "text", "text", "text", 
                                                                 "text"))
                                        
Grower_coop_V2018_part2_a <- Grower_coop_V2018_part2_a %>% mutate(id_ref = row_number()) 


Grower_coop_V2018_part2_a <- mutate(Grower_coop_V2018_part2_a,
                                  ID_yr = paste0( Block,  "_2018")) #paste0(Grower, "_", Block))
str(Grower_coop_V2018_part2_a)
Grower_coop_V2018_part2_a <- dplyr::select(Grower_coop_V2018_part2_a, ID_yr,
                                           Date, id_ref)
#join the two togther
str(Grower_coop_V2018_part2_a)
str(Grower_coop_V2018_part2)

Grower_coop_V2018_part2_join <- full_join(Grower_coop_V2018_part2_a, Grower_coop_V2018_part2) 


rm(Grower_coop_V2018_part1, Grower_coop_V2018_part1_av, Grower_coop_V2018_part2, Grower_coop_V2018_part2_a)


#final is Grower_coop_V2018_part2_join # 

# but I am having huge problems joining this data

##############################################################################################################################
############ 2014 - 2017  data ######################################################################################################


Harvest_data_2014_2017 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Marlborough_Grape_Growers_Cooperative/Harvest data 2014-2017.xlsx", 
                                     sheet = "2014_2017_codes")

str(Harvest_data_2014_2017)
Harvest_data_2014_2017 <- filter(Harvest_data_2014_2017,Grower != "NA" )
Harvest_data_2014_2017 <- filter(Harvest_data_2014_2017,Hectares != "NA" )

#keep the data that has a good match with the name and ha.
Harvest_data_2014_2017 <- filter(Harvest_data_2014_2017, match_of_Realignment_and_ha != "n")

#split the data into years
Harvest_data_2014 <- dplyr::select(Harvest_data_2014_2017,
                                   Grower ,
                                   Realignment_Number = "Realignment Number best match", 
                                   ha = "Hectares",
                                   yld = "Fruit Produced 2014" ,
                                   yield_t_ha = "Tonnes per hectare 2014",
                                   brix = "2014 Brix") %>% 
  mutate(year = 2014)


Harvest_data_2015 <- dplyr::select(Harvest_data_2014_2017,
                                   Grower ,
                                   Realignment_Number = "Realignment Number best match", 
                                   ha = "Hectares",
                                   yld = "Fruit Produced 2015" ,
                                   yield_t_ha = "Tonnes per hectare 2015",
                                   brix = "2015 Brix") %>% 
  mutate(year = 2015)

Harvest_data_2016 <- dplyr::select(Harvest_data_2014_2017,
                                   Grower ,
                                   Realignment_Number = "Realignment Number best match", 
                                   ha = "Hectares",
                                   yld = "Fruit Produced 2016" ,
                                   yield_t_ha = "Tonnes per hectare 2016",
                                   brix = "2016 Brix") %>% 
  mutate(year = 2016)

Harvest_data_2017 <- dplyr::select(Harvest_data_2014_2017,
                                   Grower ,
                                   Realignment_Number = "Realignment Number best match", 
                                   ha = "Hectares",
                                   yld = "Fruit Produced 2017" ,
                                   yield_t_ha = "Tonnes per hectare 2017",
                                   brix = "2017 Brix") %>% 
  mutate(year = 2017)

str(Harvest_data_2014)
str(Harvest_data_2015)
str(Harvest_data_2016)
str(Harvest_data_2017)
Harvest_data_2017$yld <- as.numeric(Harvest_data_2017$yld)
Harvest_data_2017$yield_t_ha <- as.numeric(Harvest_data_2017$yield_t_ha)




Harvest_data_2014_2017_tidy <- bind_rows(Harvest_data_2014,
                                         Harvest_data_2015,
                                         Harvest_data_2016,
                                         Harvest_data_2017)

## what the go with the names are there any replicates? _ nope looks ok
growers2014 <-unique(Harvest_data_2014$Grower)
growers2015 <-unique(Harvest_data_2015$Grower)
growers2016 <-unique(Harvest_data_2016$Grower)
growers2017 <-unique(Harvest_data_2017$Grower)



###############################################################################################
##### 2019 GPS sites - done via google earth and maps ########################################
str(Harvest_data_2014_2017_tidy)
Harvest_data_2014_2017_tidy$Realignment_Number <- as.numeric(Harvest_data_2014_2017_tidy$Realignment_Number)
str(Grower_coop_V2019)
Grower_coop_V2019_selection <- dplyr::select(Grower_coop_V2019,
                                             #ID_yr,
                                             #SWNZ_Vineyard_ID = "SWNZ Vineyard ID",
                                             row_width ,
                                             vine_spacing ,
                                             "Realignment_Number" = "Realignment_number",
                                             x_coord,
                                             y_coord
)


str(Harvest_data_2014_2017_tidy)
str(Grower_coop_V2019_selection)

Harvest_data_2014_2017_GPS <- left_join(Harvest_data_2014_2017_tidy,Grower_coop_V2019_selection )

rm(Harvest_data_2014_2017_tidy, Grower_coop_V2019_GPS,
   Harvest_data_2014, Harvest_data_2015, Harvest_data_2016, Harvest_data_2017)


#### Add in the other fields
str(Harvest_data_2014_2017_GPS)
#### Add in the calulations
Harvest_data_2014_2017_GPS <- mutate(Harvest_data_2014_2017_GPS,
                                 ID_yr = paste0( Grower,"_", year),
                                 harvest_date= "NA",
                                 bunch_m = "NA",
                                 company = "Grower_coop",
                                 julian = NA, #julian = as.numeric(format(harvest_date, "%j")),
                                 m_ha_vine = 10000/ row_width,
                                 yield_kg_m = (yield_t_ha *1000)/m_ha_vine,
                                 #bunch_weight = total_weight_bunches / nunb_bunches_collected, #the data sheet has -4 from this cal ?why
                                 #berry_weight_1 = total_weight_berry / no_berry_in_sample,
                                 #berry_per_bunch = bunch_weight / berry_weight_1, 
                                 #berry_weight_2 = (yield_t_ha/vines_ha/bunches_per_vine/berry_per_bunch)*1000000, #this is in the data sheet - check
                                 #bunches_per_vine = NA, #??not sure I can cal this?? help
                                 pruning_style = NA,
                                 brix ,
                                 meter_row_per_ha = 10000/row_width,
                                 yld_per_m_row_kg = (yield_t_ha *1000) / 10000/row_width)#,
                                 #bunch_m = (yld_per_m_row_kg * 1000)/ bunch_weight)



str(Harvest_data_2014_2017_GPS)
Harvest_data_2014_2017_GPS <- select(Harvest_data_2014_2017_GPS,
                            company ,
                            ID_yr , 
                            #variety,
                            x_coord ,
                            y_coord  ,
                            year,
                            harvest_date, #missing
                            julian, #missing
                            yield_t_ha,
                            yield_kg_m,
                            brix, #missing
                            bunch_m ,
                            #pruning_style,
                            row_width ,
                            vine_spacing,
                            Realignment_Number
)
###### Now join to the 2019 data 
names(Grower_coop_V2019)
names(Harvest_data_2014_2017_GPS)
Grower_coop_V2019 <- rename(Grower_coop_V2019,
                            "Realignment_Number" = "Realignment_number")
str(Grower_coop_V2019)
str(Harvest_data_2014_2017_GPS)

Grower_coop_V2014_2019ex2018 <- rbind(Grower_coop_V2019, Harvest_data_2014_2017_GPS)
