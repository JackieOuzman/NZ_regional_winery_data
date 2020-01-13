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

#sites added in 2020 jaxs

Rob_agnew_2020 <-  read_csv( "V:/Marlborough regional/working_jaxs/Yld_GPS_Rob_Agnew_GPS_SAB_select_sites.csv")
wine_portfolio_2020 <-  read_csv( "V:/Marlborough regional/working_jaxs/Wine_portfolio_yld_GPS_only_SAB.csv")
constellation_2020 <-  read_csv( "V:/Marlborough regional/working_jaxs/constellation_2017_2019_all_sau.csv")


#sites added in 2020 Rob Not run yet

yealands_2020 <-  read_excel( "V:/Marlborough regional/Regional winery data/Raw_data/Yealands/Updated Yealands.xlsx",
                              sheet = "Updated other blocks All years")
str(yealands_2020)
yealands_2020 <- select(yealands_2020,
                        x_coord = POINT_X,
                        y_coord = POINT_Y ,
                        vineyard = VINEYARD,
                        year = Year,                                       
                        harvest_date = `Harvest date`,
                        yield_t_ha = `Actual Harvested  T/ha`, #check with ROB - ok to use
                        yield_kg_m = `Yield (kg/m)`,
                        bunch_weight = `Calculated Bunch mass (g)`, #check with ROB - ok to use
                        berry_weight = `Calculated Berry wt (g)`,
                        bunch_per_vine = `Bunches Removed pre Veraison, post estimate`,
                        row_width = `row spacing (m)`,
                        vine_spacing = `In-row spacing (m)`)
                    
yealands_2020 <- mutate(yealands_2020,                      
company = "Yealands",
ID_yr = paste0(vineyard, "_", year), 
variety = "Sauvignon Blanc",
julian = as.numeric(format(harvest_date, "%j")),
brix = NA,
bunch_numb_m = bunch_per_vine / vine_spacing,
pruning_style = NA)



str(yealands_2020)
#######Up to here need to cal the vine spacing
yealands_seaview_2020 <-  read_excel( "V:/Marlborough regional/Regional winery data/Raw_data/Yealands/Updated Yealands.xlsx",
                              sheet = "Updated Seaview all years")
str(yealands_seaview_2020)
yealands_seaview_2020 <- select(yealands_seaview_2020,
                        x_coord = POINT_X,
                        y_coord = POINT_Y ,
                        #vineyard = VINEYARD,
                        year = Year,                                       
                        harvest_date = `Harvest Date`,
                        yield_t_ha = `Actual Harvested  T/ha`, #check with ROB - ok to use
                        yield_kg_m = `Yield (kg/m)`,
                        bunch_weight = `Calculated Bunch mass (g)`, #check with ROB - ok to use
                        berry_weight = `Calculated Berry wt (g)`,
                        bunch_per_vine = `Bunches Removed pre Veraison, post estimate`,
                        row_width = `row spacing (m)`,
                        #vine_spacing = `In-row spacing (m)`)

yealands_2020 <- mutate(yealands_2020,                      
                        company = "Yealands",
                        vineyard = "seaview",
                        ID_yr = paste0(vineyard, "_", year), 
                        variety = "Sauvignon Blanc",
                        julian = as.numeric(format(harvest_date, "%j")),
                        brix = NA,
                        bunch_numb_m = bunch_per_vine / vine_spacing,
                        pruning_style = NA)






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
####################################    constellation     ############################################################
####################################################################################################################
glimpse(constellation_2020) #18


####################################    wine_portfolio_2020     ############################################################
####################################################################################################################
glimpse(wine_portfolio_2020) #18

####################################    Rob_agnew_2020     ############################################################
####################################################################################################################
glimpse(Rob_agnew_2020)#18

####################################################################################################################
####################################    merge one step at a time   #################################################
####################################################################################################################
###site added in 2019
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

####################################################################################################################
###site added in 2020 jaxs

del_pern_vill_white_wither_con <- rbind(del_pern_vill_white_wither,
                                        constellation_2020)
del_pern_vill_white_wither_con_win_port <- rbind(del_pern_vill_white_wither_con, wine_portfolio_2020)
del_pern_vill_white_wither_con_win_port_RA <- rbind(del_pern_vill_white_wither_con_win_port, Rob_agnew_2020)
glimpse(del_pern_vill_white_wither_con_win_port_RA)


####do na count again ####
glimpse(del_pern_vill_white_wither_con_win_port_RA)
del_pern_vill_white_wither_con_win_port_RA <- select(del_pern_vill_white_wither_con_win_port_RA, -na_count)

del_pern_vill_white_wither_con_win_port_RA$na_count <- apply(is.na(del_pern_vill_white_wither_con_win_port_RA), 1, sum)
######   Recode variety column so that it is all the same
group_by(del_pern_vill_white_wither_con_win_port_RA, variety) %>% 
  count()


del_pern_vill_white_wither_con_win_port_RA <- mutate(del_pern_vill_white_wither_con_win_port_RA,
                                     variety =  case_when(
                                       variety == "SAUV" ~ "Sauvignon Blanc",
                                       variety == "sb" ~ "Sauvignon Blanc",
                                       variety == "SB" ~ "Sauvignon Blanc",
                                       variety == "SAB" ~ "Sauvignon Blanc",
                                       variety == "Sauvignon_blanc" ~ "Sauvignon Blanc",
                                       TRUE ~ variety))
group_by(del_pern_vill_white_wither_con_win_port_RA, variety) %>% 
  count()
group_by(del_pern_vill_white_wither_con_win_port_RA, company) %>% 
  count()

del_pern_vill_white_wither_con_win_port_RA <- mutate(del_pern_vill_white_wither_con_win_port_RA,
                                     company =  case_when(
                                       company == "Delegates" ~ "Delegat",
                                       company == "pernod_ricard" ~ "Pernod Ricard",
                                       company == "Villa Maria" ~ "Villa Maria",
                                       company == "whitehaven" ~ "Whitehaven",
                                       company == "Wither_Hills" ~ "Wither Hills",
                                       TRUE ~ company))

write_csv(del_pern_vill_white_wither_con_win_port_RA, "V:/Marlborough regional/working_jaxs/del_pern_vill_white_wither_con_win_port_RA.csv")

 
del_pern_vill_white_wither_con_win_port_RA <- mutate(del_pern_vill_white_wither_con_win_port_RA,year = as.double(year))

##################################################################################################################
######################      Display data                   #####################################################
################################################################################################################

dim(del_pern_vill_white_wither_con_win_port_RA)
#how many site?
str(del_pern_vill_white_wither_con_win_port_RA)
glimpse(del_pern_vill_white_wither_con_win_port_RA) #5,023 records


#how many sites by company by year
ggplot(del_pern_vill_white_wither_con_win_port_RA, aes(company))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#how many sites by company by year with coods
test <- filter(del_pern_vill_white_wither_con_win_port_RA, year_factor != "NA")
glimpse(test)
summary(del_pern_vill_white_wither_con_win_port_RA)


filter(del_pern_vill_white_wither_con_win_port_RA,x_coord > 0) %>% 
  filter( year != "NA") %>% 
   ggplot( aes(company))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=0))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

###Create a new column which changes company name
unique(del_pern_vill_white_wither_con_win_port_RA$company)

del_pern_vill_white_wither_con_win_port_RA <- mutate(del_pern_vill_white_wither_con_win_port_RA,
                                     company_a =  case_when(
                                       company == "Delegat" ~ "a",
                                       company == "Pernod Ricard" ~ "b",
                                       company == "Villa Maria" ~ "c",
                                       company == "Whitehaven" ~ "d",
                                       company == "Wither Hills" ~ "e",
                                       company == "constellation" ~ "f",
                                       company == "wine_portfolio" ~ "g",
                                       company == "Matua" ~ "h",
                                       company == "Oyster Bay/ Delegat" ~ "i",
                                       company == "Marlborough Research" ~ "j"
                                       #company == "Matua" ~ "k",
                                       TRUE ~ company))



#create a new variable year_as_factor
del_pern_vill_white_wither_con_win_port_RA$year_factor <- as.factor(del_pern_vill_white_wither_con_win_port_RA$year)


ggplot(del_pern_vill_white_wither_con_win_port_RA, aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")

#julian days
ggplot(del_pern_vill_white_wither_con_win_port_RA, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
##### only display greater than 20 
filter(del_pern_vill_white_wither_con_win_port_RA,julian > 20) %>% 
  ggplot( aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")

filter(del_pern_vill_white_wither_con_win_port_RA,julian > 20) %>% 
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
ggplot(del_pern_vill_white_wither_con_win_port_RA, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")

#yield_t_ha
filter(del_pern_vill_white_wither_con_win_port_RA,yield_t_ha > 0) %>% 
ggplot( aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")

#yield_t_ha
filter(del_pern_vill_white_wither_con_win_port_RA,yield_t_ha > 0) %>% 
  ggplot( aes(year_factor, yield_t_ha, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")+
  facet_wrap(.~ company)
