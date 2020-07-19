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

Babich_2015 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Babich/2015 Vintage counts.xlsx", 
                                   sheet = "actual weights", skip = 1)
names(Babich_2015)

#I need to fill the above name for each row for the following clms Variety, Grower and Block

Babich_2015 <- fill(Babich_2015, Variety, Grower,  Block, .direction = "down")

#now only keep the 
unique(Babich_2015$Variety)

Babich_2015_SB <- filter(Babich_2015,
                         Variety == "Sauv Blanc")

# remove the totals and empty rows in the blockclm
unique(Babich_2015$Block)
#change everything to lower case
Babich_2015_SB$Block <- str_to_lower(Babich_2015_SB$Block)

Babich_2015_SB <- Babich_2015_SB %>% 
  filter(!str_detect(Block, "total"))
#remove the rows with no ha - not sure what these were doing.
names(Babich_2015_SB)
Babich_2015_SB <- Babich_2015_SB %>% 
  filter(!is.na(`Area (ha)`))
## the rows I want to keep are                           
Babich_2015_SB <- dplyr:: select(Babich_2015_SB,
                                 variety = Variety,
                                 Grower,
                                 Block,
                                 Area_ha = "Area (ha)",
                                 ave_bunches = "ave bunches",
                                 berry_weight = "est berry weight",
                                 tonnes = "Actual Tonnes",
                                 yield_t_ha = "Actual T/Ha"
                                 )  

### need to get the vine spacing row spacing and location
sites_for_GPS <- unique(Babich_2015_SB$Grower)
#write.csv(sites_for_GPS, "sites_for_GPS.csv")
##########################################################################################################################
##Google earth with pdf maps have given me these sites:

GPS <- read.csv("V:/Marlborough regional/Regional winery data/Raw_data/Babich/google_earth_location/Babich_locations.csv")
#make a clm for grower and one for block
str(GPS)
#1.turn everything into lower case

GPS$Name <- str_to_lower(GPS$Name)
#2. replace spaces with underscores

#oops there is a typo "tetle brook g" should be "tetlebrook g"
GPS$Name <- str_replace(GPS$Name, "tetle brook g", "tetlebrook g")
GPS <- separate(GPS, Name, into = c("Grower", "Block"), sep = " ", remove = FALSE)

names(Babich_2015_SB)
names(GPS)
#fix up the names so they match
Babich_2015_SB$Grower <- str_to_lower(Babich_2015_SB$Grower)
Babich_2015_SB$Block <- str_to_lower(Babich_2015_SB$Block)
#GPS names to fix
GPS$Grower <- str_replace(GPS$Grower, "tetlebrook", "tettly brook")
GPS$Grower <- str_replace(GPS$Grower, "tetleybrook", "tettly brook")
GPS$Grower <- str_replace(GPS$Grower, "headwater", "headwaters")
GPS$Block <- str_replace(GPS$Block, "pear_tree", "pear tree")
GPS$Block <- str_replace(GPS$Block, "toi_toi", "toi toi")
GPS$Block <- str_replace(GPS$Block, "toi_toi", "toi toi")
GPS$Block <- str_replace(GPS$Block, "5_eyes", "5 eyes")
GPS$Block <- str_replace(GPS$Block, "watch_", "watch tower")
GPS$Block <- str_replace(GPS$Block, "fext", "fx") #check this is correct?
GPS$Block <- str_replace(GPS$Block, "main1-80", "main") #check this is correct?
GPS$Block <- str_replace(GPS$Block, "main81-148", "organic") #check this is correct?
GPS$Block <- str_replace(GPS$Block, "208-222", "sr 208-222") #check this is correct?

#write.csv(GPS, "GPS_temp.csv")
#getwd()

Vineyard_details <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Babich/vineyard_details_from_maps.xlsx")

GPS <-  full_join(GPS,Vineyard_details )

#lets see what we can join...
Babich_2015_SB_GPS <- full_join(Babich_2015_SB, GPS)
#####################################################################################################################################

Babich_2016 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Babich/2016 Yield counts and actuals.xlsx", 
                          sheet = "estimates with thinning ", skip = 1)
names(Babich_2016)

#I need to fill the above name for each row for the following clms Variety, Grower and Block

Babich_2016 <- fill(Babich_2016, Variety, Grower,  Block, .direction = "down")

#now only keep the 
unique(Babich_2016$Variety)

Babich_2016_SB <- filter(Babich_2016,
                         Variety == "Sauv Blanc")

# remove the totals and empty rows in the blockclm
unique(Babich_2016_SB$Block)
#change everything to lower case
Babich_2016_SB$Block <- str_to_lower(Babich_2016_SB$Block)

Babich_2016_SB <- Babich_2016_SB %>% 
  filter(!str_detect(Block, "total"))
#remove the rows with no ha - not sure what these were doing.
names(Babich_2016_SB)
Babich_2016_SB <- Babich_2016_SB %>% 
  filter(!is.na(`Area (ha)`))
## the rows I want to keep are                           
Babich_2016_SB <- dplyr:: select(Babich_2016_SB,
                                 variety = Variety,
                                 Grower,
                                 Block,
                                 Area_ha = "Area (ha)",
                                 ave_bunches = "ave bunches",
                                 berry_weight = "est berry weight",
                                 tonnes = "Actual",
                                 yield_t_ha = "Actual T/Ha"
)  

Babich_2016_SB$Grower <- str_to_lower(Babich_2016_SB$Grower)
Babich_2016_SB$Block <- str_to_lower(Babich_2016_SB$Block)

Babich_2016_SB_GPS <- full_join(Babich_2016_SB, GPS)


#####################################################################################################################################

Babich_2017 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Babich/2016 Yield counts and actuals.xlsx", 
                          sheet = "estimates with thinning ", skip = 1)

#stuck here I am not sure what to with the 2017 and 2018 most of it is forcast data??
