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
                                             Realignment_number,
                                             bunch_weight
)



Grower_coop_V2019


##Remove the df I dont need anymore
rm(#Grower_coop_V2019
   #Grower_coop_V2019_GPS,
   Grower_coop_V2019_join,
   Grower_coop_V2019_selection, 
   Grower_coop_V2019_yld_data)
  
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
str(Grower_coop_V2019_GPS)

yld_data2014_2017$`Realignment Number`<- as.numeric(yld_data2014_2017$`Realignment Number`)

yld_data2014_2017 <- full_join(yld_data2014_2017,Grower_coop_V2019_GPS )
yld_data2014_2017 <- dplyr::select(yld_data2014_2017,
                                   - "...6",
                                   -"notes",
                                   -"check_t_ha" ,
                                  - "match_ha" )

#I need to add in the row spacing and vine spacing this is in the 2019 data
names(Grower_coop_V2019)
row_vine_spacing <- dplyr::select(Grower_coop_V2019,
                                  "Realignment_number" ,
                                  row_width  ,
                                  vine_spacing)

# add this to the other 2014 -2017 data
str(row_vine_spacing)
str(yld_data2014_2017)
#rename clm 
yld_data2014_2017 <- rename(yld_data2014_2017,
                            Realignment_number = `Realignment Number`)
yld_data2014_2017$Realignment_number <- as.numeric(yld_data2014_2017$Realignment_number)
yld_data2014_2017 <- left_join(yld_data2014_2017, row_vine_spacing )


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
                                 bunch_m = NA)


str(yld_data2014_2017)
str(Grower_coop_V2019_GPS)
Grower_coop_V2019_GPS <- rename(Grower_coop_V2019_GPS,
                            Realignment_number = `Realignment Number`)


yld_data2014_2017 <- mutate(yld_data2014_2017,
                            company = "grower_coop",
                            ID_yr = paste0("Realignment_numb_", Realignment_number, "_year_", year ),
                            variety = "SAB")

yld_data2014_2017 <- select(
   yld_data2014_2017,
   company,
   ID_yr,
   year, 
   variety,
   x_coord = POINT_X,
   y_coord = POINT_Y ,
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
   Block = Name 
)

names(yld_data2014_2017)

rm("row_vine_spacing",
   "site_with_t_ha_error")
rm("Grower_coop_V2019_GPS")

# join the 2019 and 2014-2018 data togther

str(yld_data2014_2017)
str(Grower_coop_V2019) #add in a fw more clms
Grower_coop_V2019 <- mutate(Grower_coop_V2019,
                            company = "grower_coop",
                            ID_yr = paste0("Realignment_numb_", Realignment_number, "_year_", year ),
                            variety = "SAB",
                            pruning_style = NA,
                            )


Grower_coop_V2019 <- select(
   Grower_coop_V2019,
   company,
   ID_yr,
   year, 
   variety,
   x_coord ,
   y_coord  ,
   year,
   harvest_date,
   julian,
   #bunch_weight,
   yield_t_ha,
   yield_kg_m,
   brix, 
   bunch_m,
   #pruning_style,
   row_width ,
   vine_spacing
   
)


V2019_2014_to_2017 <- bind_rows(Grower_coop_V2019, yld_data2014_2017)

write.csv( V2019_2014_to_2017, "grower_coop_V2019_2014_to_2017.csv")

#### STOP HERE I NEED HELP with the 2018 data


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

