library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)
#V:/Marlborough regional/working_jaxs/pernod_ricard1_sau.csv
#V:/Marlborough regional/working_jaxs/white_haven_2019to2014_all_sav.csv
#V:/Marlborough regional/working_jaxs/wither_hills_GPS_block_info_harvest_sau.csv
#V:/Marlborough regional/working_jaxs/Villia_maria_2017_2012_all_sau.csv
#V:/Marlborough regional/working_jaxs/delegates_april_2019_sau.csv


delegates_april_2019 <-  read_csv( "V:/Marlborough regional/working_jaxs/delegates_april_2019_sau.csv")
pernod_ricard_april_2019 <- read_csv( "V:/Marlborough regional/working_jaxs/pernod_ricard1_sau.csv")
Villia_maria_april_2019<- read_csv( "V:/Marlborough regional/working_jaxs/Villia_maria_2017_2012_all_sau.csv")
white_haven_april_2019 <- read_csv("V:/Marlborough regional/working_jaxs/white_haven_2019to2014_all_sav.csv")
wither_hills_april_2019 <-  read_csv("V:/Marlborough regional/working_jaxs/wither_hills_GPS_block_info_harvest_sau.csv")
####################################################################################################################
####################################    DELEGATES     ##############################################################
####################################################################################################################
glimpse(delegates_april_2019) #19
#need to drop ID_temp
delegates_april_2019 <- select(delegates_april_2019, - ID_temp)
####################################################################################################################
####################################    Pern          ##############################################################
####################################################################################################################
glimpse(pernod_ricard_april_2019) #20
#need to drop ID and number_canes
pernod_ricard_april_2019 <- select(pernod_ricard_april_2019, -  ID, -number_canes)
####################################################################################################################
####################################    Villia Maria    ############################################################
####################################################################################################################
glimpse(Villia_maria_april_2019) #18
Villia_maria_april_2019 <- Villia_maria_april_2019 %>% 
  mutate(ID_yr = paste0(ID,"_", year))
Villia_maria_april_2019 <- select(Villia_maria_april_2019, -  ID)
####################################################################################################################
####################################    wither_hills   ############################################################
####################################################################################################################
glimpse(wither_hills_april_2019) #18
#fix up iD so I have year and ID clm
wither_hills_april_2019 <- wither_hills_april_2019 %>% 
  mutate(ID_yr = paste0(ID,"_", year))
wither_hills_april_2019 <- select(wither_hills_april_2019, -  ID)
####################################################################################################################
####################################    white_haven     ############################################################
####################################################################################################################
glimpse(white_haven_april_2019) #21
white_haven_april_2019 <- select(white_haven_april_2019, -Vineyard, -Block, -ID)

glimpse(wither_hills_april_2019)

####################################################################################################################
####################################    merge one step at a time   #################################################
####################################################################################################################

delegate_pern <- rbind(delegates_april_2019,
                      pernod_ricard_april_2019)

glimpse(delegate_pern)
glimpse(Villia_maria_april_2019)
delegate_pern_villia <- rbind(delegate_pern,
                              Villia_maria_april_2019)

delegate_pern_villia_white_haven <- rbind(delegate_pern_villia,
                                          white_haven_april_2019)

del_pern_vill_white_wither <- rbind(delegate_pern_villia_white_haven,
                                    wither_hills_april_2019)
####do na count again ####
glimpse(del_pern_vill_white_wither)
del_pern_vill_white_wither <- select(del_pern_vill_white_wither, -na_count)

del_pern_vill_white_wither$na_count <- apply(is.na(del_pern_vill_white_wither), 1, sum)
######   Recode variety column so that it is all the same
group_by(del_pern_vill_white_wither, variety) %>% 
  count()


del_pern_vill_white_wither <- mutate(del_pern_vill_white_wither,
                                     variety =  case_when(
                                       variety == "SAUV" ~ "Sauvignon Blanc",
                                       variety == "sb" ~ "Sauvignon Blanc",
                                       variety == "SB" ~ "Sauvignon Blanc",
                                       TRUE ~ variety))

group_by(del_pern_vill_white_wither, company) %>% 
  count()

del_pern_vill_white_wither <- mutate(del_pern_vill_white_wither,
                                     company =  case_when(
                                       company == "Delegates" ~ "Delegat",
                                       company == "pernod_ricard" ~ "Pernod Ricard",
                                       company == "Villa Maria" ~ "Villa Maria",
                                       company == "whitehaven" ~ "Whitehaven",
                                       company == "Wither_Hills" ~ "Wither Hills",
                                       TRUE ~ company))

write_csv(del_pern_vill_white_wither, "V:/Marlborough regional/working_jaxs/del_pern_vill_white_wither.csv")

 


##################################################################################################################
######################      Display data                   #####################################################
################################################################################################################

dim(del_pern_vill_white_wither)
#how many site?

glimpse(del_pern_vill_white_wither) #4400 records
summary(del_pern_vill_white_wither)#2006 -2019


#how many sites by company by year
ggplot(del_pern_vill_white_wither, aes(company))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#how many sites by company by year with coods
filter(del_pern_vill_white_wither,x_coord > 0) %>% 
ggplot( aes(company))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#create a new variable year_as_factor
del_pern_vill_white_wither$year_factor <- as.factor(del_pern_vill_white_wither$year)


ggplot(del_pern_vill_white_wither, aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")

#julian days
ggplot(del_pern_vill_white_wither, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
##### only display greater than 20 
filter(del_pern_vill_white_wither,julian > 20) %>% 
  ggplot( aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")

filter(del_pern_vill_white_wither,julian > 20) %>% 
  ggplot( aes(year_factor, julian, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")+
  facet_wrap(.~ company)

#yield_t_ha
ggplot(del_pern_vill_white_wither, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")

#yield_t_ha
filter(del_pern_vill_white_wither,yield_t_ha > 0) %>% 
ggplot( aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")

#yield_t_ha
filter(del_pern_vill_white_wither,yield_t_ha > 0) %>% 
  ggplot( aes(year_factor, yield_t_ha, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")+
  facet_wrap(.~ company)
