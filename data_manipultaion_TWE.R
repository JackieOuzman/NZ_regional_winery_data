
#1. work still to do - this is so messy!
# add the 2018 and 2019 yield data once this is added I should have a list of vineyard location
#2. revist my googling and change the names to what is in my short list.

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)
library(stringr)
library(tidyr)

####################################################################################################################
######   Bring in the coodinated that I have got from the pdf maps and street address     ##########################

babich_coordinates <- read_csv("V:/Marlborough regional/Regional winery data/Raw_data/Babich/google_earth_location/babich_and_babich_growers_14_09_2020GDA.csv")

names(babich_coordinates)

babich_coordinates <- filter(babich_coordinates,
                             !is.na(POINT_X))

babich_coordinates_DF <-data.frame(babich_coordinates)
str(babich_coordinates_DF)


############################################################################################################
########      Bring in the yield data for multiple years 2014 to 2017     ############################################

                 
Babich_2014_17 <-
  read_excel(
    "V:/Marlborough regional/Regional winery data/Raw_data/Babich/revised_data_05082020/Historical Harvest Details V14-V17 CSIRO 050820.xlsx",
    col_types = c(
      "text",
      "text",
      "numeric",
      "text",
      "text",
      "date",
      "text",
      "text",
      "date",
      "text",
      "text",
      "date",
      "text",
      "text",
      "date",
      "text",
      "text"
    ),
    skip = 2
  )

Babich_2014_17 <- fill(Babich_2014_17, "Corporate Blocks", .direction = "down")

Babich_2014_17 <- rename(
  Babich_2014_17,
  "vineyard" = "Corporate Blocks",
  "notes" = "...2"   ,
  "blocks" =  "...4"  ,
  "ha" =  "...5"  ,
  "harvest_date_14" = "vintage 14"  ,
  "tonnes_14" = "...7",
  "brix_14" = "...8" ,
  "harvest_date_15" = "vintage 15"  ,
  "tonnes_15" = "...10" ,
  "brix_15" = "...11",
  "harvest_date_16" = "vintage 16",
  "tonnes_16" = "...13" ,
  "brix_16" = "...14" ,
  "harvest_date_17" = "vintage 17"   ,
  "tonnes_17" = "...16"  ,
  "brix_17" = "...17"
)


Babich_2014_17 <- filter(Babich_2014_17,
                             !is.na(blocks))
Babich_2014_17 <- dplyr::select(Babich_2014_17, - "...3"  )
names(Babich_2014_17)
## the vineyard name is not super sensible all the time but the blocks show some promise!
## Let see what we can pull out of the blocks names



Babich_2014_17<- separate(Babich_2014_17, blocks, 
                into = c("company_grower_code","variety","vineyard_code","block_code","temp5", "temp6", "temp7",
                         "temp8","temp9","temp10","temp11","temp12"), 
                remove = FALSE)
#Just keep the correct variety
unique(Babich_2014_17$variety)
Babich_2014_17 <- filter(Babich_2014_17, variety == "SAB")

# add a clm with more details about what I grower company is...
Babich_2014_17 <- mutate(Babich_2014_17, 
               grower_name = case_when(
                 company_grower_code == "BA" ~ "babich",
                 company_grower_code == "JC" ~ "james_cameron",
                 company_grower_code == "AC" ~ "angus_cameron",
                 company_grower_code == "RG" ~ "richard_gifford",
                 company_grower_code == "MG" ~ "murray_game",
                 company_grower_code == "MW" ~ "matakana_wines",
                 company_grower_code == "SL" ~ "steven_la_plante",
                 company_grower_code == "MP" ~ "martin_pattie",
                 company_grower_code == "CB" ~ "cable_bay",
                 company_grower_code == "JL" ~ "john_leslie",
                 TRUE ~ company_grower_code))

Babich_2014_17 <- mutate(Babich_2014_17, 
               vineyard_code = case_when(
                 vineyard_code == "CS" ~ "cvv",
                 vineyard_code == "EV" ~ "ecv",
                 vineyard_code == "HW" ~ "hw",
                 vineyard_code == "SR" ~ "sr",
                 vineyard_code == "WD" ~ "wdr",
                 vineyard_code == "TB" ~ "tbv",
                 TRUE ~ tolower(vineyard_code)))

Babich_2014_17 <- mutate(Babich_2014_17, 
                block_code = case_when(
                  block_code == "PEAR" ~ "pear_tree",
                  block_code == "TOI" ~ "toi_toi",
                  block_code == "5" ~ "5_eyes",
                  TRUE ~ block_code))

Babich_2014_17 <- mutate(Babich_2014_17, 
                         block_code = case_when(
                  temp5 == "MAIN" &  block_code == "GULLY" ~ "gully_main",
                  temp5 == "WEST"&  block_code == "GULLY"  ~ "gully_west",
                  temp5 == "WEST"&  block_code == "EAST"  ~ "east_west",
                  TRUE ~ tolower(block_code)))

names(Babich_2014_17)

###################################################################################################

#create a df for each year with the same column names and then join togther
Babich_2014 <- dplyr::select(
  Babich_2014_17,
  "vineyard" ,
  "notes"   ,
  "blocks"   ,
  "ha" ,
  "harvest_date" = "harvest_date_14",
  "tonnes" = "tonnes_14",
  "brix" = "brix_14",
  "grower_name",
  "variety",
  "vineyard_code",
  "block_code"
) %>%
  mutate(year = 2014)

Babich_2015 <- dplyr::select(
  Babich_2014_17,
  "vineyard" ,
  "notes"   ,
  "blocks"   ,
  "ha" ,
  "harvest_date" = "harvest_date_15",
  "tonnes" = "tonnes_15",
  "brix" = "brix_15",
  "grower_name",
  "variety",
  "vineyard_code",
  "block_code"
) %>%
  mutate(year = 2015)

Babich_2016 <- dplyr::select(
  Babich_2014_17,
  "vineyard" ,
  "notes"   ,
  "blocks"   ,
  "ha" ,
  "harvest_date" = "harvest_date_16",
  "tonnes" = "tonnes_16",
  "brix" = "brix_16",
  "grower_name",
  "variety",
  "vineyard_code",
  "block_code"
) %>%
  mutate(year = 2016)

Babich_2017 <- dplyr::select(
  Babich_2014_17,
  "vineyard" ,
  "notes"   ,
  "blocks"   ,
  "ha" ,
  "harvest_date" = "harvest_date_17",
  "tonnes" = "tonnes_17",
  "brix" = "brix_17",
  "grower_name",
  "variety",
  "vineyard_code",
  "block_code"
) %>%
  mutate(year = 2017)

Babich_2014_2017_yld_info <- bind_rows(Babich_2014,
                                       Babich_2015,
                                       Babich_2016,
                                       Babich_2017)

rm(Babich_2014,
   Babich_2015,
   Babich_2016,
   Babich_2017,
   Babich_2014_17)


## small changes to get the block names consitant (as they arent compatiable across the data sets)
 names(Babich_2014_2017_yld_info)
Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                   block_code = case_when(
                     vineyard_code == "rk" & block_code == "2a" ~ "2a_2b",
                     vineyard_code == "rk" & block_code == "4" ~ "4_5_6",
                   TRUE ~ block_code))

Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    block_code = case_when(
                                      blocks == "MP SAB TK SAB 15-58|74-114" ~ "15_58|74_114",
                                      blocks == "MP SAB TK SAB 137-170" ~ "137_170",
                                      blocks == "MP SAB TK SAB 115-136|199-210" ~ "115_136|199_210",
                                      TRUE ~ block_code))
#I think the vineyard is coded wrong too!
Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    vineyard = case_when(
                                      vineyard_code == "tk" ~ "Te Kohanga Vineyard",
                                      TRUE ~ vineyard))
#I think the grower name is coded wrong 
Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    grower_name = case_when(
                                      vineyard_code == "kv" ~ "kintyre",
                                      TRUE ~ grower_name))
Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    vineyard = case_when(
                                      vineyard_code == "kv" ~ "Waihopai",
                                      TRUE ~ vineyard))
#mismatch with data supplied and map..? not sure what is corrrect this is for vineyard R 4.75ha vs 5.75ha

# rename stuff
Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    vineyard = case_when(
                                      vineyard_code == "te" ~ "TERRACES FARM TRUST VINEYARD",
                                      TRUE ~ vineyard))
Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    grower_name = case_when(
                                      vineyard_code == "te" ~ "angus_cameron",
                                      TRUE ~ grower_name))
Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    block_code = case_when(
                                      vineyard_code == "te" & blocks == "AC SAB TE 1-58" ~ "1_58",
                                      vineyard_code == "te" & blocks == "AC SAB TE 59-88" ~ "59_88",
                                      TRUE ~ block_code))

Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    block_code = case_when(
                                      vineyard_code == "sp"  ~ "1_58",
                                      TRUE ~ block_code))


Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    block_code = case_when(
                                      vineyard_code == "sr" & blocks == "MP SAB SR SAB 213-394" ~ "top_main_bottom",
                                      TRUE ~ block_code))

Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
                                    vineyard = case_when(
                                      vineyard_code == "sr" & blocks == "MP SAB SR SAB 213-394" ~ "stephens_run",
                                      TRUE ~ vineyard))

#re-order the clms

names(Babich_2014_2017_yld_info)
Babich_2014_2017_yld_info <- dplyr::select(Babich_2014_2017_yld_info,
                                           grower_name,  
                                           vineyard_code,
                                           block_code, 
                                           vineyard,
                                           blocks,
                                           variety,
                                           year,
                                           ha,
                                           harvest_date,
                                           tonnes,
                                           brix)

#there was a few sites with missing data from the 2016 dataset I will fill them in now
#need to revist this i am havng trouble with dates!!!


Babich_2014_2017_yld_info$harvest_date <-  as.Date(strptime(
  Babich_2014_2017_yld_info$harvest_date,
  "%Y-%m-%d")) # convert your character column to POSIXct object

Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
               harvest_date = case_when(
                 blocks == "BA SAB SR 158-207" & year == 2016 ~ as.Date("2016-04-09", format= "%Y-%m-%d"),
                 blocks == "MP SAB SR SAB 213-394" & year == 2016 ~ as.Date("2016-04-16", format= "%Y-%m-%d") ,
                 TRUE ~ harvest_date))
                 
                 
Babich_2014_2017_yld_info <- mutate(Babich_2014_2017_yld_info,
               tonnes  = case_when(
                 blocks == "BA SAB SR 158-207" & year == 2016 ~ "79.8",
                 blocks == "MP SAB SR SAB 213-394" & year == 2016 ~ "91.04",
                 TRUE ~ tonnes ))

                 
              

Babich_2014_2017_yld_info[2,9]

#get the clm into the correct format
Babich_2014_2017_yld_info$ha <- as.numeric(Babich_2014_2017_yld_info$ha)
Babich_2014_2017_yld_info$tonnes <- as.numeric(Babich_2014_2017_yld_info$tonnes)
Babich_2014_2017_yld_info$brix <- as.numeric(Babich_2014_2017_yld_info$brix)


#####################################################################################################################

## Now let bring in the 2018 data 


Babich_2018 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Babich/revised_data_05082020/FORECAST VS HARVEST TONNES BRIX V18 Lowest Ta 310718.xlsx", 
                                                                   sheet = "Maturity Sample v Harvest Res ", 
                                                                   col_types = c("text", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "numeric", "date", "numeric", "numeric", 
                                                                                 "numeric", "numeric", "numeric", 
                                                                                 "text"), skip = 4)
names(Babich_2018)
str(Babich_2018)


Babich_2018 <- dplyr::select(Babich_2018,
                             blocks = "Block Codes",
                             ha = "Area" ,
                             tonnes = "Actual Tonnes",
                             harvest_date = "Harvest Date" ,
                             brix = "Brix...12" )


Babich_2018 <- filter(Babich_2018,
  !is.na(ha))
#split the blocks clm to get more info
Babich_2018<- separate(Babich_2018, blocks, 
                          into = c("company_grower_code","variety","vineyard_code","block_code","temp5", "temp6", "temp7",
                                   "temp8","temp9","temp10","temp11","temp12"), 
                          remove = FALSE)
#Just keep the correct variety
unique(Babich_2018$variety)
Babich_2018 <- filter(Babich_2018, variety == "SAB")

# add a clm with more details about what I grower company is...
Babich_2018 <- mutate(Babich_2018, 
                         grower_name = case_when(
                           company_grower_code == "BA" ~ "babich",
                           company_grower_code == "JC" ~ "james_cameron",
                           company_grower_code == "AC" ~ "angus_cameron",
                           company_grower_code == "RG" ~ "richard_gifford",
                           company_grower_code == "MG" ~ "murray_game",
                           company_grower_code == "MW" ~ "matakana_wines",
                           company_grower_code == "SL" ~ "steven_la_plante",
                           company_grower_code == "MP" ~ "martin_pattie",
                           company_grower_code == "CB" ~ "cable_bay",
                           company_grower_code == "JL" ~ "john_leslie",
                           TRUE ~ company_grower_code))

Babich_2018 <- mutate(Babich_2018, 
                         vineyard_code = case_when(
                           vineyard_code == "CS" ~ "cvv",
                           vineyard_code == "EV" ~ "ecv",
                           vineyard_code == "HW" ~ "hw",
                           vineyard_code == "SR" ~ "sr",
                           vineyard_code == "WD" ~ "wdr",
                           vineyard_code == "TB" ~ "tbv",
                           TRUE ~ tolower(vineyard_code)))

Babich_2018 <- mutate(Babich_2018, 
                         block_code = case_when(
                           block_code == "PEAR" ~ "pear_tree",
                           block_code == "TOI" ~ "toi_toi",
                           block_code == "5" ~ "5_eyes",
                           block_code == "watchtower" ~ "watch_tower",
                           TRUE ~ block_code))

Babich_2018 <- mutate(Babich_2018, 
                         block_code = case_when(
                           temp5 == "MAIN" &  block_code == "GULLY" ~ "gully_main",
                           temp5 == "WEST"&  block_code == "GULLY"  ~ "gully_west",
                           temp5 == "WEST"&  block_code == "EAST"  ~ "east_west",
                           temp5 == "CRT"&  block_code == "DAM"  ~ "dam_crt",
                           temp5 == "DRY"&  block_code == "DAM"  ~ "dam_dry",
                           TRUE ~ tolower(block_code)))

#remove all the numbers ???

### could try ordering the clm and then replacing the first few lines with NA
names(Babich_2018)
Babich_2018 <- arrange(Babich_2018, ( block_code))
#rows 1-10 and 12-14
Babich_2018[1:10,5]
Babich_2018[1:10,5] <- NA
Babich_2018[12:14,5]
Babich_2018[12:14,5] <- NA

names(Babich_2018)
Babich_2018 <- mutate(Babich_2018, year = 2018,
                      vineyard = "NA" )
#re-order the clms
names(Babich_2018)
Babich_2018 <- dplyr::select(Babich_2018,
                                           grower_name,  
                                           vineyard_code,
                                           block_code, 
                                           vineyard,
                                           blocks,
                                           variety,
                                           year,
                                           ha,
                                           harvest_date,
                                           tonnes,
                                           brix)


## the names need more help ###
Babich_2018 <- mutate(Babich_2018, 
                      block_code = case_when(
                        blocks == "AC SAB TE 1-58"  ~ "1_58",
                        blocks == "AC SAB TE 59-88  OXIDATIVE"  ~ "59_88",
                        blocks == "CB SAB R 1 76-99"  ~ "1",
                        blocks == "CB SAB R 2 100-210"  ~ "2a_2b",
                        blocks == "CB SAB R 4 299-327"  ~ "4",
                        blocks == "MP SAB SR 213-236"  ~ "top",
                        blocks == "MP SAB SR 237-299"  ~ "bottom",
                        blocks == "MP SAB SR 300-394"  ~ "main",
                        blocks == "MW SAB KV J 1-250"  ~ "j",
                        blocks == "MW SAB KV K 1-253"  ~ "k",
                        blocks == "	MW SAB KV O 1-28"  ~ "o",
                        blocks == "MW SAB KV P 3-167"  ~ "p",
                        blocks == "MW SAB KV R 14-92"  ~ "r",
                        
                        blocks == "MP SAB TK SAB 15-58|74-114"  ~ "15_58|74_114",
                        blocks == "MP SAB TK SAB 137-170"  ~ "137_170",
                        blocks == "MP SAB TK SAB 115-136|199-210"  ~ "115_136|199_210",
                        
                        blocks == "BA SAB EV 5 EYES 57-132"  ~ "5_eyes",
                        blocks == "BA SAB EV PEAR TREE 1-11 WR"  ~ "pear_tree",
                        blocks == "BA SAB EV PEAR TREE 12-52"  ~ "pear_tree",
                        
                        blocks == "BA SAB EV TOI TOI PART BLK  53-90"  ~ "toi_toi",
                        blocks == "BA SAB EV TOI TOI 53-303"  ~ "toi_toi",
                        blocks == "BA SAB EV WATCHTOWER 95-98 WR"  ~ "watch_tower",
                        blocks == "BA SAB EV WATCHTOWER 1-94"  ~ "watch_tower",
                        
                        
                        TRUE ~ block_code))

Babich_2018 <- mutate(Babich_2018, 
                      vineyard_code = case_when(
                        blocks == "CB SAB R 1 76-99"  ~ "rk",
                        blocks == "CB SAB R 2 100-210"  ~ "rk",
                        blocks == "CB SAB R 4 299-327"  ~ "rk",
                        blocks == "MW SAB KV J 1-250"  ~ "kv",
                        blocks == "MW SAB KV K 1-253"  ~ "kv",
                        blocks == "	MW SAB KV O 1-28"  ~ "kv",
                        blocks == "MW SAB KV P 3-167"  ~ "kv",
                        blocks == "MW SAB KV R 14-92"  ~ "kv",
                        
                        
                        TRUE ~ vineyard_code))

Babich_2018 <- mutate(Babich_2018, 
                      vineyard = case_when(
                        blocks == "CB SAB R 1 76-99"  ~ "rocky_vineyard",
                        blocks == "CB SAB R 2 100-210"  ~ "rocky_vineyard",
                        blocks == "CB SAB R 4 299-327"  ~ "v",
                        blocks == "MP SAB SR 213-236"  ~ "stephens_run",
                        blocks == "MP SAB SR 237-299"  ~ "stephens_run",
                        blocks == "MP SAB SR 300-394"  ~ "stephens_run",
                        blocks == "MW SAB KV J 1-250"  ~ "waihopai",
                        blocks == "MW SAB KV K 1-253"  ~ "waihopai",
                        blocks == "	MW SAB KV O 1-28"  ~ "waihopai",
                        blocks == "MW SAB KV P 3-167"  ~ "waihopai",
                        blocks == "MW SAB KV R 14-92"  ~ "waihopai",
                        blocks == "MW SAB KV O 1-28"  ~ "waihopai",
                        
                        
                        TRUE ~ vineyard))

Babich_2018 <- mutate(Babich_2018, 
                      grower_name = case_when(
                        
                        blocks == "MW SAB KV J 1-250"  ~ "kintyre",
                        blocks == "MW SAB KV K 1-253"  ~ "kintyre",
                        blocks == "	MW SAB KV O 1-28"  ~ "kintyre",
                        blocks == "MW SAB KV P 3-167"  ~ "kintyre",
                        blocks == "MW SAB KV R 14-92"  ~ "kintyre",
                        blocks == "MW SAB KV O 1-28"  ~ "kintyre",
                        
                        
                        TRUE ~ grower_name))


#####################################################################################################################
### Do more work on the 2018 data before merging
Babich_2018$grower_name <-tolower(Babich_2018$grower_name)
Babich_2018$vineyard_code <-tolower(Babich_2018$vineyard_code)
Babich_2018$block_code <-tolower(Babich_2018$block_code)

Babich_2018 <- mutate(Babich_2018,
                      Name = paste0(grower_name, " ", vineyard_code, " ", block_code))

#just bring the last clm at the front

Babich_2018 <- Babich_2018 %>%
  select(Name, everything())


#### some points need avearging
#1. remove the  rows
tbv_b <- filter(Babich_2018, vineyard_code == "tbv" &block_code == "b")
#sum and average where needed...
str(tbv_b)
tbv_b_sum_av <- tbv_b %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
tbv_b_sum_av <- mutate(tbv_b_sum_av,
                           blocks = "av_sum_values" ,
                           vineyard = "tbv"
                           )
tbv_b_sum_av <- ungroup(tbv_b_sum_av)

#2. remove  rows from yld df
Babich_2018 <-filter(Babich_2018,  vineyard_code!= "tbv" | block_code!= "b")
Babich_2018 <- bind_rows(Babich_2018, tbv_b_sum_av)

#### some points need avearging
#1. remove the  rows
tbv_c <- filter(Babich_2018, vineyard_code == "tbv" &block_code == "c")
#sum and average where needed...
str(tbv_c)
tbv_c_sum_av <- tbv_c %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
tbv_c_sum_av <- mutate(tbv_c_sum_av,
                       blocks = "av_sum_values" ,
                       vineyard = "tbv"
)
tbv_c_sum_av <- ungroup(tbv_c_sum_av)

#2. remove  rows from yld df
Babich_2018 <-filter(Babich_2018,  vineyard_code!= "tbv" | block_code!= "c")
Babich_2018 <- bind_rows(Babich_2018, tbv_c_sum_av)



#### some points need avearging
#1. remove the  rows
ecv_duck <- filter(Babich_2018, vineyard_code == "ecv" &block_code == "duck")
#sum and average where needed...
str(ecv_duck)
ecv_duck_sum_av <- ecv_duck %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
ecv_duck_sum_av <- mutate(ecv_duck_sum_av,
                       blocks = "av_sum_values" ,
                       vineyard = "ecv"
)
ecv_duck_sum_av <- ungroup(ecv_duck_sum_av)

#2. remove  rows from yld df
Babich_2018 <-filter(Babich_2018,  vineyard_code!= "ecv" | block_code!= "duck")
Babich_2018 <- bind_rows(Babich_2018, ecv_duck_sum_av)


#### some points need avearging
#1. remove the  rows
ecv_watchtower <- filter(Babich_2018, vineyard_code == "ecv" &block_code == "watch_tower")
#sum and average where needed...
str(ecv_watchtower)
ecv_watchtower_sum_av <- ecv_watchtower %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
ecv_watchtower_sum_av <- mutate(ecv_watchtower_sum_av,
                          blocks = "av_sum_values" ,
                          vineyard = "ecv"
)
ecv_watchtower_sum_av <- ungroup(ecv_watchtower_sum_av)

#2. remove  rows from yld df
Babich_2018 <-filter(Babich_2018,  vineyard_code!= "ecv" | block_code!= "watch_tower")
Babich_2018 <- bind_rows(Babich_2018, ecv_watchtower_sum_av)






#### some points need avearging
#1. remove the  rows
ecv_pear_tree <- filter(Babich_2018, vineyard_code == "ecv" &block_code == "pear_tree")
#sum and average where needed...
str(ecv_pear_tree)
ecv_pear_tree_sum_av <- ecv_pear_tree %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
ecv_pear_tree_sum_av <- mutate(ecv_pear_tree_sum_av,
                                blocks = "av_sum_values" ,
                                vineyard = "ecv"
)
ecv_pear_tree_sum_av <- ungroup(ecv_pear_tree_sum_av)

#2. remove  rows from yld df
Babich_2018 <-filter(Babich_2018,  vineyard_code!= "ecv" | block_code!= "pear_tree")
Babich_2018 <- bind_rows(Babich_2018, ecv_pear_tree_sum_av)






#### some points need avearging
#1. remove the  rows
ecv_toi_toi <- filter(Babich_2018, vineyard_code == "ecv" &block_code == "toi_toi")
#sum and average where needed...
str(ecv_toi_toi)
ecv_toi_toi_sum_av <- ecv_toi_toi %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
ecv_toi_toi_sum_av <- mutate(ecv_toi_toi_sum_av,
                               blocks = "av_sum_values" ,
                               vineyard = "ecv"
)
ecv_toi_toi_sum_av <- ungroup(ecv_toi_toi_sum_av)

#2. remove  rows from yld df
Babich_2018 <-filter(Babich_2018,  vineyard_code!= "ecv" | block_code!= "toi_toi")
Babich_2018 <- bind_rows(Babich_2018, ecv_toi_toi_sum_av)








#### some points need avearging
#1. remove the  rows
ecv_pheasant <- filter(Babich_2018, vineyard_code == "ecv" &block_code == "pheasant")
#sum and average where needed...
str(ecv_toi_toi)
ecv_pheasant_sum_av <- ecv_pheasant %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
ecv_pheasant_sum_av <- mutate(ecv_pheasant_sum_av,
                             blocks = "av_sum_values" ,
                             vineyard = "ecv"
)
ecv_pheasant_sum_av <- ungroup(ecv_pheasant_sum_av)

#2. remove  rows from yld df
Babich_2018 <-filter(Babich_2018,  vineyard_code!= "ecv" | block_code!= "pheasant")
Babich_2018 <- bind_rows(Babich_2018, ecv_pheasant_sum_av)







#### some points need avearging
#1. remove the  rows
cvv_south <- filter(Babich_2018, vineyard_code == "cvv" &block_code == "south")
#sum and average where needed...
str(cvv_south)
cvv_south_sum_av <- cvv_south %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
cvv_south_sum_av <- mutate(cvv_south_sum_av,
                              blocks = "av_sum_values" ,
                              vineyard = "ecv"
)
cvv_south_sum_av <- ungroup(cvv_south_sum_av)

#2. remove  rows from yld df
Babich_2018 <-filter(Babich_2018,  vineyard_code!= "cvv" | block_code!= "south")
Babich_2018 <- bind_rows(Babich_2018, cvv_south_sum_av)



##################################################################################################################
## Now let bring in the 2019 data 


Babich_2019 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Babich/MASTER 2019 Forecast vs Harvest Tonnes Brix.xlsx", 
                                                          sheet = "Maturity Sample v Harvest Res ", 
                                                          col_types = c("text", "text", "text", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "text", "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "date", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "text"), skip = 6)
names(Babich_2019)
Babich_2019 <-dplyr::select(Babich_2019,
                            blocks = "Block Codes" ,
                            "Vineyard",
                            ha = "Area", 
                            tonnes ="Actual Tonnes",
                            yield_t_ha = "actual t/ha" , 
                            variety ="Variety",
                            harvest_date ="Harvest Date",
                            brix = "Brix...17")
# remove rows with no ha
Babich_2019 <- filter(Babich_2019,
                      !is.na(ha))
#split the blocks clm to get more info
Babich_2019<- separate(Babich_2019, blocks, 
                       into = c("company_grower_code","variety","vineyard_code","block_code","temp5", "temp6", "temp7",
                                "temp8","temp9","temp10","temp11","temp12"), 
                       remove = FALSE)
#Just keep the correct variety
unique(Babich_2019$variety)
Babich_2019 <- filter(Babich_2019, variety == "SAB")

# add a clm with more details about what I grower company is...
Babich_2019 <- mutate(Babich_2019, 
                      grower_name = case_when(
                        company_grower_code == "BA" ~ "babich",
                        company_grower_code == "JC" ~ "james_cameron",
                        company_grower_code == "AC" ~ "angus_cameron",
                        company_grower_code == "RG" ~ "richard_gifford",
                        company_grower_code == "MG" ~ "murray_game",
                        company_grower_code == "MW" ~ "matakana_wines",
                        company_grower_code == "SL" ~ "steven_la_plante",
                        company_grower_code == "MP" ~ "martin_pattie",
                        company_grower_code == "CB" ~ "cable_bay",
                        company_grower_code == "JL" ~ "john_leslie",
                        TRUE ~ company_grower_code))

Babich_2019 <- mutate(Babich_2019, 
                      vineyard_code = case_when(
                        vineyard_code == "CS" ~ "cvv",
                        vineyard_code == "EV" ~ "ecv",
                        vineyard_code == "HW" ~ "hw",
                        vineyard_code == "SR" ~ "sr",
                        vineyard_code == "WD" ~ "wdr",
                        vineyard_code == "TB" ~ "tbv",
                        TRUE ~ tolower(vineyard_code)))

Babich_2019 <- mutate(Babich_2019, 
                      block_code = case_when(
                        block_code == "PEAR" ~ "pear_tree",
                        block_code == "TOI" ~ "toi_toi",
                        block_code == "5" ~ "5_eyes",
                        block_code == "watchtower" ~ "watch_tower",
                        
                        TRUE ~ block_code))

Babich_2019 <- mutate(Babich_2019, 
                      block_code = case_when(
                        temp5 == "MAIN" &  block_code == "GULLY" ~ "gully_main",
                        temp5 == "WEST"&  block_code == "GULLY"  ~ "gully_west",
                        temp5 == "WEST"&  block_code == "EAST"  ~ "east_west",
                        temp5 == "CRT"&  block_code == "DAM"  ~ "dam_crt",
                        temp5 == "DRY"&  block_code == "DAM"  ~ "dam_dry",
                        temp5 == "STREAM"&  block_code == "BOUNDARY"  ~ "bounadry_stream",
                        temp5 == "TERRACE"&  block_code == "TOP"  ~ "top_terrace",
                        temp5 == "NUT"&  block_code == "PINE"  ~ "pine_nut",
                        temp5 == "2B"&  block_code == "2A"  ~ "2a_2b",
                        temp5 == "NELSON"&  block_code == "ORGANIC"  ~ "nelson_organic",
                        temp5 == "N"&  block_code == "A"  ~ "a_n_s_c_d",
                        TRUE ~ tolower(block_code)))
#remove all the numbers ???

### could try ordering the clm and then replacing the first few lines with NA
names(Babich_2019)
Babich_2019 <- arrange(Babich_2019, ( block_code))
#rows 1-10 and 12-14
Babich_2019[c(1:8,11,13:14,16:20),5]
Babich_2019[c(1:8,11,13:14,16:20),5] <- NA


names(Babich_2019)
Babich_2019 <- mutate(Babich_2019, year = 2019,
                      vineyard = "NA" )
#re-order the clms
names(Babich_2019)
Babich_2019 <- dplyr::select(Babich_2019,
                             grower_name,  
                             vineyard_code,
                             block_code, 
                             vineyard,
                             blocks,
                             variety,
                             year,
                             ha,
                             harvest_date,
                             tonnes,
                             yield_t_ha,
                             brix)

## the names need more help ###
Babich_2019 <- mutate(Babich_2019, 
                      block_code = case_when(
                        blocks == "BA SAB RK 1 76-99"  ~ "1",
                        blocks == "CB SAB CN 1 r1-47"  ~ "1",
                        blocks == "CB SAB CN 4 r1-33"  ~ "4",
                        blocks == "CB SAB CN 6 r1-21|r22-25" ~ "6",
                        blocks == "MP SAB SR 237-299"~ "bottom",
                        blocks == "MP SAB SR 300-394" ~ "main",
                        blocks == "MP SAB SR 213-236" ~ "top",
                        blocks == "CB SAB RK 3"  ~ "3",
                        blocks == "CB SAB RK 4 (r299-327), 5 & 6"  ~ "4_5_6",
                        blocks == "MP SAB TK SAB 15-58|74-114"  ~ "15_58|74_114",
                        blocks == "MP SAB TK SAB 137-170"  ~ "137_170",
                        blocks == "MP SAB TK SAB 115-136|199-210"  ~ "115_136|199_210",
                        blocks == "BA SAB TE 1-58"  ~ "1_58",
                        blocks == "BA  SAB TE 59-88"  ~ "59_88",
                        blocks == "BA SAB HW MAIN 81-148"  ~ "81-148",
                        blocks == "BA SAB HW MAIN ORG SV 1-80"  ~ "1-80",
                        blocks == "BA  SAB TE 59-88"  ~ "59_88",
                        blocks == "BA SAB TE HAYSHED 89-181"  ~ "hayshed",
                        
                        blocks == "BA SAB EV 5 EYES 57-132"  ~ "5_eyes",
                        blocks == "BA SAB EV PEAR TREE 1-11 WR"  ~ "pear_tree",
                        blocks == "BA SAB EV PEAR TREE 12-52"  ~ "pear_tree",
                        
                        blocks == "BA SAB EV TOI TOI 53-303 (part 2)"  ~ "toi_toi",
                        blocks == "BA SAB EV TOI TOI 53-303 (part 1)"  ~ "toi_toi",
                        blocks == "BA SAB EV WATCHTOWER r1-93"  ~ "watch_tower",
                        blocks == "BA SAB EV WATCHTOWER r94-98 WR"  ~ "watch_tower",
                        
                        
                        TRUE ~ block_code))


Babich_2019 <- mutate(Babich_2019, 
                      vineyard = case_when(
                        blocks == "BA SAB RK 2A & 2B 100-210 (part 1)"  ~ "rocky_vineyard",
                        blocks == "BA SAB RK 2A & 2B 100-210 (part 2)"  ~ "rocky_vineyard",
                        blocks == "CB SAB RK 3"  ~ "rocky_vineyard",
                        blocks == "CB SAB RK 4 (r299-327), 5 & 6"  ~ "rocky_vineyard",
                        blocks == "MP SAB TK SAB 15-58|74-114"  ~ "kohanga",
                        blocks == "BA SAB TE 1-58"  ~ "terrance_farm_trust",
                        
                        
                        TRUE ~ vineyard))

Babich_2019 <- mutate(Babich_2019, 
                      grower_name = case_when(
                        blocks == "BA SAB RK 1 76-99"  ~ "cable_bay",
                        blocks == "BA SAB RK 2A & 2B 100-210 (part 1)"  ~ "cable_bay",
                        blocks == "BA SAB RK 2A & 2B 100-210 (part 2)"  ~ "cable_bay",
                        blocks == "CB SAB RK 3"  ~ "cable_bay",
                        blocks == "CB SAB RK 4 (r299-327), 5 & 6"  ~ "cable_bay",
                        
                        blocks == "WV SAB KV R r14-92"  ~ "kintyre",
                        blocks == "	WV SAB KV K r161-253"  ~ "kintyre",
                        blocks == "WV SAB KV J r1-250 (part 1)"  ~ "kintyre",
                        blocks == "WV SAB KV J r1-250 (part 2)"  ~ "kintyre",
                        blocks == "WV SAB KV K r161-253"  ~ "kintyre",
                        
                        blocks == "BA SAB TE 1-58"  ~ "angus_cameron",
                        blocks == "BA  SAB TE 59-88"  ~ "angus_cameron",
                        blocks == "BA SAB TE HAYSHED 89-181"  ~ "angus_cameron",
                        
                        TRUE ~ grower_name))

#####################################################################################################################
### Do more work on the 2019 data before merging
Babich_2019$grower_name <-tolower(Babich_2019$grower_name)
Babich_2019$vineyard_code <-tolower(Babich_2019$vineyard_code)
Babich_2019$block_code <-tolower(Babich_2019$block_code)

Babich_2019 <- mutate(Babich_2019,
                           Name = paste0(grower_name, " ", vineyard_code, " ", block_code))

#just bring the last clm at the front

Babich_2019 <- Babich_2019 %>%
  select(Name, everything())


#### some points need avearging
#1. remove the  rows
kintyre_j <- filter(Babich_2019, vineyard_code == "kv" &block_code == "j")
#sum and average where needed...
str(kintyre_j)
kintyre_j_sum_av <- kintyre_j %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
kintyre_j_sum_av <- mutate(kintyre_j_sum_av,
                          blocks = "av_sum_values" ,
                          vineyard = "kintyre",
                          yield_t_ha = NA)
kintyre_j_sum_av <- ungroup(kintyre_j_sum_av)

#2. remove  rows from yld df
Babich_2019 <-filter(Babich_2019,  vineyard_code!= "kv" | block_code!= "j")
Babich_2019 <- bind_rows(Babich_2019, kintyre_j_sum_av)

#######################################################################################
#1. remove the  rows
rk_2a_2b <- filter(Babich_2019, vineyard_code == "rk" &block_code == "2a_2b")
#sum and average where needed...
str(rk_2a_2b)
rk_2a_2b_sum_av <- rk_2a_2b %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
rk_2a_2b_sum_av <- mutate(rk_2a_2b_sum_av,
                           blocks = "av_sum_values" ,
                           vineyard = "rocky_vineyard",
                           yield_t_ha = NA)
rk_2a_2b_sum_av <- ungroup(rk_2a_2b_sum_av)

#2. remove  rows from yld df
Babich_2019 <-filter(Babich_2019,  vineyard_code!= "rk" | block_code!= "2a_2b")
Babich_2019 <- bind_rows(Babich_2019, rk_2a_2b_sum_av)

#######################################################################################
#1. remove the  rows
pf_lsb <- filter(Babich_2019, vineyard_code == "pf" &block_code == "lsb")
#sum and average where needed...
str(pf_lsb)
pf_lsb_sum_av <- pf_lsb %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
pf_lsb_sum_av <- mutate(pf_lsb_sum_av,
                          blocks = "av_sum_values" ,
                          vineyard = "park_farm",
                          yield_t_ha = NA)
pf_lsb_sum_av <- ungroup(pf_lsb_sum_av)

#2. remove  rows from yld df
Babich_2019 <-filter(Babich_2019,  vineyard_code!= "pf" | block_code!= "lsb")
Babich_2019 <- bind_rows(Babich_2019, pf_lsb_sum_av)


#######################################################################################
#1. remove the  rows
cvv_north <- filter(Babich_2019, vineyard_code == "cvv" &block_code == "north")
#sum and average where needed...
str(cvv_north)
cvv_north_sum_av <- cvv_north %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
cvv_north_sum_av <- mutate(cvv_north_sum_av,
                        blocks = "av_sum_values" ,
                        vineyard = "cowslip",
                        yield_t_ha = NA)
cvv_north_sum_av <- ungroup(cvv_north_sum_av)

#2. remove  rows from yld df
Babich_2019 <-filter(Babich_2019,  vineyard_code!= "cvv" | block_code!= "north")
Babich_2019 <- bind_rows(Babich_2019, cvv_north_sum_av)

#######################################################################################
#1. remove the  rows
cvv_south <- filter(Babich_2019, vineyard_code == "cvv" &block_code == "south")
#sum and average where needed...
str(cvv_south)
cvv_south_sum_av <- cvv_south %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
cvv_south_sum_av <- mutate(cvv_south_sum_av,
                           blocks = "av_sum_values" ,
                           vineyard = "cowslip",
                           yield_t_ha = NA)
cvv_south_sum_av <- ungroup(cvv_south_sum_av)

#2. remove  rows from yld df
Babich_2019 <-filter(Babich_2019,  vineyard_code!= "cvv" | block_code!= "south")
Babich_2019 <- bind_rows(Babich_2019, cvv_south_sum_av)

#######################################################################################
#1. remove the  rows
ecv_pear_tree <- filter(Babich_2019, vineyard_code == "ecv" &block_code == "pear_tree")
#sum and average where needed...
str(ecv_pear_tree)
ecv_pear_tree_sum_av <- ecv_pear_tree %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
ecv_pear_tree_sum_av <- mutate(ecv_pear_tree_sum_av,
                           blocks = "av_sum_values" ,
                           vineyard = "echelon",
                           yield_t_ha = NA)
ecv_pear_tree_sum_av <- ungroup(ecv_pear_tree_sum_av)

#2. remove  rows from yld df
Babich_2019 <-filter(Babich_2019,  vineyard_code!= "ecv" | block_code!= "pear_tree")
Babich_2019 <- bind_rows(Babich_2019, ecv_pear_tree_sum_av)



#######################################################################################
#1. remove the  rows
ecv_toi_toi <- filter(Babich_2019, vineyard_code == "ecv" &block_code == "toi_toi")
#sum and average where needed...
str(ecv_toi_toi)
ecv_toi_toi_sum_av <- ecv_toi_toi %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
ecv_toi_toi_sum_av <- mutate(ecv_toi_toi_sum_av,
                               blocks = "av_sum_values" ,
                               vineyard = "echelon",
                               yield_t_ha = NA)
ecv_toi_toi_sum_av <- ungroup(ecv_toi_toi_sum_av)

#2. remove  rows from yld df
Babich_2019 <-filter(Babich_2019,  vineyard_code!= "ecv" | block_code!= "toi_toi")
Babich_2019 <- bind_rows(Babich_2019, ecv_toi_toi_sum_av)

#######################################################################################
#1. remove the  rows
ecv_watchtower <- filter(Babich_2019, vineyard_code == "ecv" &block_code == "watch_tower")
#sum and average where needed...
str(ecv_watchtower)
ecv_watchtower_sum_av <- ecv_watchtower %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
ecv_watchtower_sum_av <- mutate(ecv_watchtower_sum_av,
                             blocks = "av_sum_values" ,
                             vineyard = "echelon",
                             yield_t_ha = NA)
ecv_watchtower_sum_av <- ungroup(ecv_watchtower_sum_av)

#2. remove  rows from yld df
Babich_2019 <-filter(Babich_2019,  vineyard_code!= "ecv" | block_code!= "watch_tower")
Babich_2019 <- bind_rows(Babich_2019, ecv_watchtower_sum_av)

#######################################################################################
#1. remove the  rows
cvv_poplar <- filter(Babich_2019, vineyard_code == "cvv" &block_code == "poplar")
#sum and average where needed...
str(cvv_poplar)
cvv_poplar_sum_av <- cvv_poplar %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
cvv_poplar_sum_av <- mutate(cvv_poplar_sum_av,
                                blocks = "av_sum_values" ,
                                vineyard = "echelon",
                                yield_t_ha = NA)
cvv_poplar_sum_av <- ungroup(cvv_poplar_sum_av)

#2. remove  rows from yld df
Babich_2019 <-filter(Babich_2019,  vineyard_code!= "cvv" | block_code!= "poplar")
Babich_2019 <- bind_rows(Babich_2019, cvv_poplar_sum_av)


###################################################################################################################
#### put all the yeild data togther and then work out the blocks?
str(Babich_2014_2017_yld_info)
Babich_2014_2017_yld_info$harvest_date <- as.POSIXct(Babich_2014_2017_yld_info$harvest_date)

str(Babich_2018)
str(Babich_2019)

Babich_2014_2019 <- bind_rows(Babich_2014_2017_yld_info,
                              Babich_2018, Babich_2019)
## make it all lower case 
Babich_2014_2019$blocks <-tolower(Babich_2014_2019$blocks)


getwd()


## lets try and join the GPS and vine spacing data

rm(Babich_2018, Babich_2019, Babich_2014_2017_yld_info)
str(Babich_2014_2019)
Babich_2014_2019 <- mutate(Babich_2014_2019,
                           Name = paste0(grower_name, " ", vineyard_code, " ", block_code))
str(babich_coordinates_DF)


######################################################################################
## I need to fiddle with the names to get it to match for the yld dataset
unique(Babich_2014_2019$blocks)
unique(Babich_2014_2019$Name)


Babich_2014_2019 <- mutate(Babich_2014_2019, Name = case_when(
  blocks == "ba sab cs south 1-110" ~ "babich cvv south 1-110",
  blocks == "ba sab cs south 111-115 wr" ~ "babich cvv south 111-115",
  blocks == "ba sab cs south r107-115 wr" ~ "babich cvv south 111-115",
  blocks == "ba sab cs south r1-106" ~ "babich cvv south 1-110",
  
  blocks == "ba sab sr 158-207" ~ "babich sr 158-207" ,
  blocks == "ba sab sr 208-222" ~ "babich sr 208-222",
  
  blocks == "ba sab ev watchtower 1-94" ~ "babich ecv watch_ tower 1-94" ,
  blocks == "	ba sab ev watchtower 95-98 wr" ~ "babich ecv watch_ tower 95-98",
  
  blocks == "ba sab hw main 81-148" ~ "babich hw main 81-148",
  blocks == "ba sab hw main org sv 1-80" ~ "babich hw main 1 - 80",
  
  blocks == "ba sab ev watchtower 95-98 wr" ~ "babich ecv watch_ tower 95-98",
  blocks == "ba sab ev watchtower r1-93" ~ "babich ecv watch_ tower 1-94",
  blocks == "ba sab ev watchtower r94-98 wr" ~ "babich ecv watch_ tower 95-98",
  
  TRUE ~ Name
))



# can't include the pe data its too young
names(Babich_2014_2019)
Babich_2014_2019 <- filter(Babich_2014_2019, vineyard_code != "pe")

#################################################################################################################

babich_coordinates_DF$Name <- tolower(babich_coordinates_DF$Name)


#Babich_2014_2019 <- full_join(babich_coordinates_DF, Babich_2014_2019)
Babich_2014_2019 <- left_join(Babich_2014_2019 ,babich_coordinates_DF)
# remove the sites that dont have coods
names(Babich_2014_2019)
Babich_2014_2019 <- filter(Babich_2014_2019, !is.na( POINT_X ))


#what babich sites didnt join
test1 <- anti_join(babich_coordinates_DF, Babich_2014_2019)
test2 <- anti_join(Babich_2014_2019 , babich_coordinates_DF)

#Rob wants a list of what was not joined
write.csv(test2,"V:/Marlborough regional/Regional winery data/Raw_data/Babich/google_earth_location/missing_location_13_08_2020/babich_no_coord.csv")

############################################################################################################

#### I need to add in the calulations when possible
# be careful here is the row has t/ha keep it!

#fix up names
Babich_2014_2019 <- rename(Babich_2014_2019,row_width =  row_spacing)
Babich_2014_2019 <- Babich_2014_2019 %>% mutate(company = "babich")
Babich_2014_2019 <- Babich_2014_2019 %>% mutate(ID_yr = paste0(Name, "_", year))
#cal the yield_t_ha_temp for all data
Babich_2014_2019 <- Babich_2014_2019 %>%  mutate(yield_t_ha_temp = tonnes / ha)
str(Babich_2014_2019)
Babich_2014_2019 <- Babich_2014_2019 %>%  mutate( 
  yield_t_ha_temp2 =  case_when(
    is.na(yield_t_ha)~ yield_t_ha_temp,
    TRUE ~ yield_t_ha
  ))

#only want one yield_t/ha clm
names(Babich_2014_2019)
Babich_2014_2019 <- dplyr::select(Babich_2014_2019,
                      -yield_t_ha_temp,
                      -yield_t_ha)
Babich_2014_2019 <- rename(Babich_2014_2019,yield_t_ha =  yield_t_ha_temp2)

Babich_2014_2019 <- mutate(Babich_2014_2019,
                            harvest_date ,
                            julian = as.numeric(format(harvest_date, "%j")),
                            m_ha_vine = 10000/ row_width,
                            yield_kg_m = (yield_t_ha *1000)/m_ha_vine,
                            bunch_weight = NA, 
                            berry_per_bunch = NA ,
                            bunches_per_vine = NA, 
                            pruning_style = NA,
                            brix ,
                            meter_row_per_ha = 10000/row_width,
                            yld_per_m_row_kg = (yield_t_ha *1000) / 10000/row_width,
                            bunch_m = NA)
names(Babich_2014_2019)

Babich_2014_2019 <- select(
  Babich_2014_2019,
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

# add missing clms
Babich_2014_2019 <- mutate(Babich_2014_2019,berry_weight = NA,
                           bunch_numb_m = NA,
                           na_count = NA)



write.csv(Babich_2014_2019,
          "V:/Marlborough regional/working_jaxs/July2020/babich_13_08_2020.csv")
   
