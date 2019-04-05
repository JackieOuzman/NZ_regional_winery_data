library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

####           GPS POINTS for blocks ##########
perno_GPS <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod Ricard Trought BX BchWT 2008 -2018 Co Marl.xlsx", 
                                 sheet = "Block Location reference")
glimpse(perno_GPS)



perno_GPS <- select(perno_GPS,
                    ID_temp = `VendorBlock Key` ,
                    x_temp = `NZGD2000 Centroid Easting`,
                    y_temp = `NZGD2000 Centroid Nrthing`,
                    Vnd,
                    Block,
                    Yr) %>% 
  mutate(ID = paste0(Vnd,"_", Block),
         year = as.double(paste0(Yr+2000)))
glimpse(perno_GPS)

#ID	          		x_temp	       	 	y_temp
#M18_SBAA	    	1665252.97		  5403846.94
#MV8_SBLAK	  	1664300.02005 	5405949.42973
#MV7_CHDC	    	1674120.96		  5407776.31
#M21_PNNE	    	1692719.51		  5389699.26
#MV7_CHDD     	1674066.57  		5407761.64 
#M14_SBLH    		1681656.44  		5405858.30 
#M16_SBLI     	1666891.79  		5406747.31 


