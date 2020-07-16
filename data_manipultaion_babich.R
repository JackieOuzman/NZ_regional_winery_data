library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)

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
write.csv(sites_for_GPS, "sites_for_GPS.csv")
