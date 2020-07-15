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
test <- Babich_2015_SB %>% 
  filter(str_detect(Block, "Total"))
                         # Block != "Total Actual"|
                         # Block != "Total grower" |
                         # Block != "total"|
                         # Block != "Total Babich")
               
               mtcars %>% 
                 filter(str_detect(rowname, "Merc"))   
               
               
