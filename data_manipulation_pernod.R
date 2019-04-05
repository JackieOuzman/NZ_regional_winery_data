library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

####           GPS POINTS for blocks ##########
perno_GPS_temp <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod Ricard Trought BX BchWT 2008 -2018 Co Marl.xlsx", 
                                 sheet = "Block Location reference")
glimpse(perno_GPS_temp)
perno_GPS <- select(perno_GPS_temp,
                    ID_temp = `VendorBlock Key` ,
                    x_temp = `NZGD2000 Centroid Easting`,
                    y_temp = `NZGD2000 Centroid Nrthing`,
                    Vnd,
                    Block,
                    Yr,
                    Variety) %>% 
  mutate(ID = paste0(Vnd,"_", Block),
         year = as.double(paste0(Yr+2000)))
glimpse(perno_GPS)
#make 2 files one with GPS pts and one without
perno_GPS_no_coord <- filter(perno_GPS, is.na(x_temp))
perno_GPS_coord <- filter(perno_GPS, !is.na(y_temp))
###Create a unquie list of blocks ###
perno_GPS_distinct_no_coords <- distinct(perno_GPS_no_coord, ID, .keep_all = TRUE)
perno_GPS_distinct <- distinct(perno_GPS_coord, ID, .keep_all = TRUE)

glimpse(perno_GPS_distinct_no_coords)
#summary of blocks with no coords
variety_no_coords <- perno_GPS_distinct_no_coords %>% 
                      group_by(Variety) %>% 
                      summarise(number_blocks_no_coords =n())
#summary of blocks with with coords
variety_coords <- perno_GPS_distinct %>% 
  group_by(Variety) %>% 
  summarise(number_blocks_with_coords =n())

###### Summary of blocks with and without coords ########
Summary_blocks_coods <- full_join(variety_no_coords, variety_coords)
                      
                      
######### perno_GPS_distinct ########
glimpse(perno_GPS_distinct)
##### Fix up the decimal place problem with the x and y ######






perno_GPS_distinct <- perno_GPS_distinct %>% 
  mutate(x_length = str_length(x_temp) ,
         y_length = str_length(y_temp))
glimpse(perno_GPS_distinct)



perno_GPS_distinct <- perno_GPS_distinct %>% 
  mutate(x_coord = ifelse(x_length == 8, x_temp/10,
                   ifelse(x_length == 9, x_temp/100,
                   ifelse(x_length == 10, x_temp/1000, 
                   ifelse(x_length == 11, x_temp/10000,0)))),
         
         y_coord = ifelse(y_length == 8, y_temp/10,
                   ifelse(y_length == 9, y_temp/100,
                   ifelse(y_length == 10, y_temp/1000, 
                   ifelse(y_length == 11, y_temp/10000,0)))))  

#change the coordinates for these                        

#ID	          		x_coord	       	 	y_coord
#M18_SBAA	    	1665252.97		  5403846.94
#MV8_SBLAK	  	1664300.02005 	5405949.42973
#MV7_CHDC	    	1674120.96		  5407776.31
#M21_PNNE	    	1692719.51		  5389699.26
#MV7_CHDD     	1674066.57  		5407761.64 
#M14_SBLH    		1681656.44  		5405858.30 
#M16_SBLI     	1666891.79  		5406747.31 

perno_GPS_distinct <- mutate(perno_GPS_distinct,
                             x_coord =  case_when(
                               ID == "M18_SBAA" ~ 1665252.97,
                               ID == "MV8_SBLAK" ~ 1664300.02005,
                               ID == "MV7_CHDC" ~ 1674120.96,
                               ID == "M21_PNNE" ~ 1692719.51,
                               ID == "MV7_CHDD" ~ 1674066.57,
                               ID == "M14_SBLH" ~ 1681656.44,
                               ID == "M16_SBLI" ~ 1666891.79, 
                               TRUE ~ x_coord),
                             y_coord =  case_when(
                               ID == "M18_SBAA" ~ 5403846.94,
                               ID == "MV8_SBLAK" ~ 5405949.42973,
                               ID == "MV7_CHDC" ~ 5407776.31,
                               ID == "M21_PNNE" ~ 5389699.26,
                               ID == "MV7_CHDD" ~ 5407761.64,
                               ID == "M14_SBLH" ~ 5405858.30,
                               ID == "M16_SBLI" ~ 5406747.31, 
                               TRUE ~ y_coord))


 

write_csv(perno_GPS_distinct, path = "V:/Marlborough regional/working_jaxs/perno_GPS_test1.csv" )


#This is working now all the coordinates are in the corrcet spot
                          
