
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

babich_coordinates <- read_csv("V:/Marlborough regional/Regional winery data/Raw_data/Babich/google_earth_location/Babich_locations_v3.csv")


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
str(Babich_2014_2017_yld_info)
test <- mutate(Babich_2014_2017_yld_info,
               harvest_date = case_when(
                 blocks == "BA SAB SR 158-207" & year == 2016 ~ "2016-04-09",
                 blocks == "MP SAB SR SAB 213-394" & year == 2016 ~ "2016-04-16",
                 #blocks == "MP SAB SR SAB 213-394" & year == 2016 ~ "2016-04-16 00:00:00",
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
#### put all the yeild data togther and then work out the blocks?

Babich_2014_2019 <- bind_rows(Babich_2014_2017_yld_info,
                              Babich_2018, Babich_2019)
## make it all lower case 
Babich_2014_2019$blocks <-tolower(Babich_2014_2019$blocks)

getwd()
#write.csv(Babich_2014_2019, "Babich_2014_2019.csv")

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


# ecv yld data needs more work for pear tree I need to sum the yld and av the brix and harvest date
#1. remove the pear tree rows
pear_tree <- filter(Babich_2014_2019, block_code == "pear_tree")


#sum and average where needed...
str(pear_tree)
pear_tree_sum_av <- pear_tree %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
summarise(
ha = sum(ha),
tonnes = sum(tonnes),
harvest_date = mean(harvest_date),
brix = mean(brix)
)
#add in the missing clms
pear_tree_sum_av <- mutate(pear_tree_sum_av,
                           blocks = "av_sum_values" ,
                           vineyard = "ECHELON",
                           yield_t_ha = NA)
pear_tree_sum_av <- ungroup(pear_tree_sum_av)
pear_tree_sum_av <- filter(pear_tree_sum_av, year != 2015)
str(pear_tree_sum_av)

# it seems like there is nothing to sum for 2015 so just keep the one entry
pear_tree_2015 <- filter(pear_tree,year == 2015 & !is.na(tonnes))

#join the data togther 
str(pear_tree_2015)
str(pear_tree_sum_av)

pear_tree <- bind_rows(pear_tree_2015, pear_tree_sum_av)

#2. remove pear tree rows from yld df
Babich_2014_2019 <-filter(Babich_2014_2019, block_code != "pear_tree")
Babich_2014_2019 <- bind_rows(Babich_2014_2019, pear_tree)

# ecv yld data needs more work for pheasent 2018 and 2019? I need to sum the yld and av the brix and harvest date

#1. remove the pheasent rows
pheasant <- filter(Babich_2014_2019, block_code == "pheasant")


#sum and average where needed...
str(pheasant)
pheasant_sum_av <- pheasant %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
pheasant_sum_av <- mutate(pheasant_sum_av,
                           blocks = "av_sum_values" ,
                           vineyard = "ECHELON",
                           yield_t_ha = NA)
pheasant_sum_av <- ungroup(pheasant_sum_av)


#2. remove pheasant rows from yld df
Babich_2014_2019 <-filter(Babich_2014_2019, block_code != "pheasant")
Babich_2014_2019 <- bind_rows(Babich_2014_2019, pheasant_sum_av)



# ecv yld data needs remove the toi toi blocks for 2018 and 2019
names(Babich_2014_2019)
Babich_2014_2019 <- filter(Babich_2014_2019, 
               block_code != "toi_toi" |  year != 2018,
               block_code != "toi_toi" | year != 2019)
                

# ecv yld data needs av and sum for duck blocks for 2018 
#1. remove the pheasent rows
duck_2108 <- filter(Babich_2014_2019, block_code == "duck" & year == 2018)


#sum and average where needed...
str(duck_2108)
duck_sum_av <- duck_2108 %>%
  group_by(year, Name,grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
duck_sum_av <- mutate(duck_sum_av,
                          blocks = "av_sum_values" ,
                          vineyard = "ECHELON",
                      yield_t_ha = NA)
duck_sum_av <- ungroup(duck_sum_av)


#2. remove 2018 duck rows from yld df
Babich_2014_2019 <-filter(Babich_2014_2019, block_code != "duck" | year != 2018)
Babich_2014_2019 <- bind_rows(Babich_2014_2019, duck_sum_av)



# hw yld data needs remove the main  for 2018 
Babich_2014_2019 <- filter(Babich_2014_2019, 
                           block_code != "main" |  year != 2018)

# only keep some of the sr data too hard to match the rows looks like its a new planting

Babich_2014_2019 <- filter(Babich_2014_2019,
               vineyard_code != "sr" | block_code != '1') %>%
        filter (vineyard_code != "sr" | block_code != '87') %>%
        filter (vineyard_code != "sr" | block_code != '98') %>%
        filter (vineyard_code != "sr" | block_code != 'sab') %>% 
        filter (vineyard_code != "sr" | block_code != '300')

# tbv yld data needs averaged but I need to bring in more data 2018 

#1. remove the tbv in 2018 rows
names(Babich_2014_2019)
tbv_b_2108 <- filter(Babich_2014_2019, block_code == "b" & vineyard_code == "tbv" & year == 2018)
#sum and average where needed...
str(tbv_b_2108)
tbv_b_2108_sum_av <- tbv_b_2108 %>%
  group_by(year, Name, grower_name, vineyard_code, block_code, variety) %>% 
  summarise(
    ha = sum(ha),
    tonnes = sum(tonnes),
    harvest_date = mean(harvest_date),
    brix = mean(brix)
  )
#add in the missing clms
tbv_b_2108_sum_av <- mutate(tbv_b_2108_sum_av,
                      blocks = "av_sum_values" ,
                      vineyard = "ECHELON",
                      yield_t_ha = NA)
tbv_b_2108_sum_av <- ungroup(tbv_b_2108_sum_av)


#2. remove 2018 duck rows from yld df

Babich_2014_2019 <-filter(Babich_2014_2019, Name != "babich tbv b" | year != 2018)
Babich_2014_2019 <- bind_rows(Babich_2014_2019, tbv_b_2108_sum_av)


rm (duck_2108, duck_sum_av,
    pear_tree, pear_tree_2015, pear_tree_sum_av,
    pheasant, pheasant_sum_av,
    tbv_b_2108, tbv_b_2108_sum_av
    )

# can't include the pe data its too young
names(Babich_2014_2019)
Babich_2014_2019 <- filter(Babich_2014_2019, vineyard_code != "pe")

#################################################################################################################
Babich_2014_2019 <- full_join(babich_coordinates_DF, Babich_2014_2019)

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

## I am waiting for more coodinates but for now just chuck out the sites we dont know the location
names
Babich_2014_2019 <- Babich_2014_2019 %>% 
  filter(!is.na(x_coord) )

write.csv(Babich_2014_2019,
          "V:/Marlborough regional/working_jaxs/July2020/babich_13_08_2020.csv")
   
