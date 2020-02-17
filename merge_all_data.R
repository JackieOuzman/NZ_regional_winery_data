library(dplyr)
library(ggplot2)
library(tidyverse)
#install.packages("cowplot")
library(cowplot)

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
cloudyBay_2020 <- read_csv("V:/Marlborough regional/working_jaxs/yld_spatial_cloudy_bay_2004_19.csv")
str(cloudyBay_2020)
cloudyBay_2020$harvest_date <- as.Date(cloudyBay_2020$harvest_date,
                                             origin = "1970-01-01") #this is the starting date value in R


#sites added in 2020 Rob 

yealands_2020 <-  read_csv( "V:/Marlborough regional/Regional winery data/Raw_data/Yealands/Updated_Yealands_jax.csv")
dim(yealands_2020)
yealands_2020$harvest_date <- as.Date(yealands_2020$harvest_date,
                                             origin = "1970-01-01") #this is the starting date value in R


############# Rob Giesen


Giesen_2020 <-  read_csv("V:/Marlborough regional/Regional winery data/Raw_data/Giesen/Giesen_2020_spatial_yld.csv")
dim(Giesen_2020)
####################################################################################################################
####################################    DELEGATES     ##############################################################
####################################################################################################################
glimpse(delegates_april_2019) #19
#need to drop ID_temp
delegates_april_2019 <- select(delegates_april_2019, - ID_temp)
###what is the date format?
str(delegates_april_2019)
delegates_april_2019$harvest_date <- as.Date(delegates_april_2019$harvest_date,
                                               origin = "1970-01-01") #this is the starting date value in R
delegates_april_2019$bunch_weight[delegates_april_2019$bunch_weight == 0] <- NA
delegates_april_2019$julian[delegates_april_2019$julian == 1] <- NA

####################################################################################################################
####################################    Pern          ##############################################################
####################################################################################################################
glimpse(pernod_ricard_april_2019) #20
#need to drop ID and number_canes
pernod_ricard_april_2019 <- select(pernod_ricard_april_2019, -  ID, -number_canes)
str(pernod_ricard_april_2019)
pernod_ricard_april_2019$harvest_date <- as.Date(pernod_ricard_april_2019$harvest_date,
                                             origin = "1970-01-01") #this is the starting date value in R

###Extra stuff lat Jan 2020
#note for "Pernod Ricard" we have lots of zero values for brix and yield kg/m and brix
pernod_ricard_april_2019$brix[pernod_ricard_april_2019$brix == 0] <- NA
pernod_ricard_april_2019$yield_kg_m[pernod_ricard_april_2019$yield_kg_m == 0] <- NA

####################################################################################################################
####################################    Villia Maria    ############################################################
####################################################################################################################
glimpse(Villia_maria_april_2019) #18
Villia_maria_april_2019 <- Villia_maria_april_2019 %>% 
  mutate(ID_yr = paste0(ID,"_", year))
Villia_maria_april_2019 <- select(Villia_maria_april_2019, -  ID)
str(Villia_maria_april_2019)
Villia_maria_april_2019$harvest_date <- as.Date(Villia_maria_april_2019$harvest_date,
                                                 origin = "1970-01-01") #this is the starting date value in R

#I have some high values for birx - this seems to be a data entry problem
Villia_maria_april_2019$brix[Villia_maria_april_2019$brix == 1945.00] <- NA
Villia_maria_april_2019$brix[Villia_maria_april_2019$brix == 198.00] <- NA
Villia_maria_april_2019$yield_t_ha[Villia_maria_april_2019$yield_t_ha == 0] <- NA
Villia_maria_april_2019$yield_kg_m[Villia_maria_april_2019$yield_kg_m == 0] <- NA
Villia_maria_april_2019$bunch_weight[Villia_maria_april_2019$bunch_weight == 12439.0000] <- NA
Villia_maria_april_2019$bunch_weight[Villia_maria_april_2019$bunch_weight == 0] <- NA
Villia_maria_april_2019$berry_weight[Villia_maria_april_2019$berry_weight == 0] <- NA


####################################################################################################################
####################################    wither_hills   ############################################################
####################################################################################################################
glimpse(wither_hills_april_2019) #18
#fix up iD so I have year and ID clm
wither_hills_april_2019 <- wither_hills_april_2019 %>% 
  mutate(ID_yr = paste0(ID,"_", year))
wither_hills_april_2019 <- select(wither_hills_april_2019, -  ID)
str(wither_hills_april_2019)
wither_hills_april_2019$harvest_date <- as.Date(wither_hills_april_2019$harvest_date,
                                                origin = "1970-01-01") #this is the starting date value in R

wither_hills_april_2019$yield_t_ha[wither_hills_april_2019$yield_t_ha == 0] <- NA
wither_hills_april_2019$yield_kg_m[wither_hills_april_2019$yield_kg_m == 0] <- NA
wither_hills_april_2019$brix[wither_hills_april_2019$brix == 0] <- NA
wither_hills_april_2019$bunch_weight[wither_hills_april_2019$bunch_weight == 0] <- NA
wither_hills_april_2019$berry_weight[wither_hills_april_2019$berry_weight == 0] <- NA
wither_hills_april_2019$bunch_numb_m[wither_hills_april_2019$bunch_numb_m == 0] <- NA


####################################################################################################################
####################################    white_haven     ############################################################
####################################################################################################################
glimpse(white_haven_april_2019) #21
white_haven_april_2019 <- select(white_haven_april_2019, -Vineyard, -Block, -ID)

glimpse(wither_hills_april_2019)
white_haven_april_2019$harvest_date <- as.Date(white_haven_april_2019$harvest_date,
                                                origin = "1970-01-01") #this is the starting date value in R

####################################################################################################################
####################################    constellation     ############################################################
####################################################################################################################
str(constellation_2020) #18

## No harvest date
constellation_2020$yield_t_ha[constellation_2020$yield_t_ha == 0] <- NA
constellation_2020$yield_kg_m[constellation_2020$yield_kg_m == 0] <- NA
constellation_2020$berry_weight[constellation_2020$berry_weight == 0] <- NA

str(constellation_2020)

####################################    wine_portfolio_2020     ############################################################
####################################################################################################################
str(wine_portfolio_2020) #18

wine_portfolio_2020$harvest_date <- as.Date(wine_portfolio_2020$harvest_date,
                                               origin = "1970-01-01") #this is the starting date value in R


####################################    Rob_agnew_2020     ############################################################
####################################################################################################################
str(Rob_agnew_2020)#18
Rob_agnew_2020$harvest_date <- as.Date(Rob_agnew_2020$harvest_date,
                                            origin = "1970-01-01") #this is the starting date value in R
####################################################################################################################
####################################    merge one step at a time   #################################################
####################################################################################################################
###site added in 2019
# delegate_pern <- rbind(delegates_april_2019,
#                       pernod_ricard_april_2019)
# delegate_pern_villia <- rbind(delegate_pern,
#                               Villia_maria_april_2019)
# delegate_pern_villia_white_haven <- rbind(delegate_pern_villia,
#                                           white_haven_april_2019)
# del_pern_vill_white_wither <- rbind(delegate_pern_villia_white_haven,
#                                     wither_hills_april_2019)
dim(delegates_april_2019)
dim(pernod_ricard_april_2019)
dim(Villia_maria_april_2019)
dim(white_haven_april_2019)
dim(wither_hills_april_2019)



site_add2019 <- rbind(delegates_april_2019,
                      pernod_ricard_april_2019,
                      Villia_maria_april_2019,
                      white_haven_april_2019,
                      wither_hills_april_2019)
####################################################################################################################
###site added in 2020 jaxs

# del_pern_vill_white_wither_con <- rbind(del_pern_vill_white_wither,
#                                         constellation_2020)
# del_pern_vill_white_wither_con_win_port <- rbind(del_pern_vill_white_wither_con, wine_portfolio_2020)
# del_pern_vill_white_wither_con_win_port_RA <- rbind(del_pern_vill_white_wither_con_win_port, Rob_agnew_2020)
# glimpse(del_pern_vill_white_wither_con_win_port_RA)

#Merge all the Rob Angrew sites into one
str(Rob_agnew_2020)
Rob_agnew_2020 <- mutate(Rob_agnew_2020,
                         company = "Rob_Agnew")

site_add2020_jax <- rbind(cloudyBay_2020,
                          constellation_2020,
                          wine_portfolio_2020,
                          Rob_agnew_2020
                          )
str(site_add2020_jax)
  
###site added in 2020 Rob

dim(yealands_2020)
dim(Giesen_2020)
site_add2020_rob <- rbind(yealands_2020, Giesen_2020)
site_add2020_rob$yield_kg_m[site_add2020_rob$yield_kg_m == 0] <- NA
site_add2020_rob$bunch_numb_m[site_add2020_rob$bunch_numb_m == 0] <- NA



#### Merge site_add2020_jax site_add2020_rob and site_add2019
site_Jan2020 <- rbind(site_add2020_jax,site_add2020_rob,  site_add2019)
str(site_Jan2020)
####do na count again ####
glimpse(site_Jan2020)
site_Jan2020 <- select(site_Jan2020, -na_count)

site_Jan2020$na_count <- apply(is.na(site_Jan2020), 1, sum)




######   Recode variety column so that it is all the same
group_by(site_Jan2020, variety) %>% 
  count()


site_Jan2020 <- mutate(site_Jan2020,
                                     variety =  case_when(
                                       variety == "SAUV" ~ "Sauvignon Blanc",
                                       variety == "sb" ~ "Sauvignon Blanc",
                                       variety == "SB" ~ "Sauvignon Blanc",
                                       variety == "SAB" ~ "Sauvignon Blanc",
                                       variety == "Sauvignon_blanc" ~ "Sauvignon Blanc",
                                       TRUE ~ variety))
group_by(site_Jan2020, variety) %>% 
  count()
group_by(site_Jan2020, company) %>% 
  count()


site_Jan2020 <- mutate(site_Jan2020,
                                     company =  case_when(
                                       company == "Delegates" ~ "Delegat",
                                       company == "pernod_ricard" ~ "Pernod Ricard",
                                       company == "Villa Maria" ~ "Villa Maria",
                                       company == "whitehaven" ~ "Whitehaven",
                                       company == "Wither_Hills" ~ "Wither Hills",
                                       company == "constellation" ~ "Constellation",
                                       company == "Giesen" ~ "Giesen",
                                       #company == "Marlborough Research" ~ "Marlborough Research",
                                       #company == "Matua" ~ "Matua",
                                       #company == "Oyster Bay/ Delegat" ~ "Oyster Bay/ Delegat",
                                       company == "Rob_Agnew" ~ "Rob Agnew",
                                       company == "wine_portfolio" ~ "Wine Portfolio",
                                       company == "Yealands" ~ "Yealands",
                                       company == "Cloudy_bay" ~ "Cloudy Bay",
                                       
                                       TRUE ~ company))


str(site_Jan2020)
test <- filter(site_Jan2020, company == "Yealands")
str(test)

write_csv(site_Jan2020, "V:/Marlborough regional/working_jaxs/site_Jan2020.csv")

 
site_Jan2020 <- mutate(site_Jan2020,year = as.double(year))





##################################################################################################################
######################      Display data                   #####################################################
################################################################################################################

dim(site_Jan2020)
#how many site?
str(site_Jan2020)
glimpse(site_Jan2020) #5,717 records


#how many sites by company by year
ggplot(site_Jan2020, aes(company))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#why do I have na for year?
test <- filter(site_Jan2020, is.na(year))
str(test)
dim(test)
#these are 5 points for wither hills - just coord with no data can remove?

site_Jan2020 <- filter(site_Jan2020, !is.na(year))
#Do again
#how many sites by company by year - now without the no year data
ggplot(site_Jan2020, aes(company))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)
########################################################################
## just years 2014-2019
site_Jan2020_yr_14_19 <- filter(site_Jan2020, between(site_Jan2020$year, 2014, 2019))
site_Jan2020_yr_19 <- filter(site_Jan2020, year == 2019)

#with(site_Jan2020,  table(company, year))
site_table_yr <- with(filter(site_Jan2020,  x_coord >0), table(company, year))
site_table_yr
write.csv(site_table_yr, "V:/Marlborough regional/working_jaxs/site_table_yr.csv")

str(site_Jan2020_yr_14_19)
dim(site_Jan2020_yr_14_19)
#how many sites by company by year - now without the no year data and only between 2014 and 2018
filter(site_Jan2020_yr_14_19,  x_coord >0) %>% 
  ggplot( aes(company))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

test <- filter(site_Jan2020_yr_14_19, company == "Yealands")
dim(test)
with(test,  table(company, year))
test2 <- filter(test,  x_coord >0)
with(test2,  table(company, year))


site_table <- with(filter(site_Jan2020_yr_14_19,  x_coord >0), table(company, year))
site_table_with_noGPS <- with(site_Jan2020_yr_14_19,  table(company, year))
write.csv(site_table, "V:/Marlborough regional/working_jaxs/site_table.csv")

write.csv(site_table_with_noGPS, "V:/Marlborough regional/working_jaxs/site_table_with_withoutGPS.csv")



###Create a new column which changes company name
unique(site_Jan2020_yr_14_19$company)

site_Jan2020_yr_14_19 <- mutate(site_Jan2020_yr_14_19,
                                     company_a =  case_when(
                                       company == "Cloudy Bay" ~     "a",
                                       company == "Constellation" ~  "b",
                                       company == "Delegat" ~        "c",
                                       company == "Giesen" ~         "d",
                                       company == "Pernod Ricard" ~  "e",
                                       company == "Rob Agnew" ~      "f",
                                       company == "Villa Maria" ~    "g",
                                       company == "Whitehaven" ~     "h",
                                       company == "Wine Portfolio" ~ "i",
                                       company == "Wither Hills" ~   "j",
                                       company == "Yealands" ~       "k",
                                       TRUE ~ company))


#create a new variable year_as_factor
site_Jan2020_yr_14_19$year_factor <- as.factor(site_Jan2020_yr_14_19$year)
site_Jan2020_yr_14_19$na_count_factor <- as.factor(site_Jan2020_yr_14_19$na_count)

ggplot(site_Jan2020_yr_14_19, aes(na_count_factor, na_count))+
  geom_col()+
  theme_bw()+
  facet_wrap(.~ year)+
  labs(x = "count of missing data entries",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")

#julian days
ggplot(site_Jan2020_yr_14_19, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")



  ggplot(site_Jan2020_yr_14_19, aes(year_factor, julian, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")+
  facet_wrap(.~ company)
###No names 
  ggplot(site_Jan2020_yr_14_19, aes(year_factor, julian, colour= company_a))+
    geom_boxplot(alpha=0.1)+
    geom_point( alpha = 0.1)+
    theme_bw()+
    theme(legend.position="none")+
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    labs(x = "Year",
         y= "Julian days - Sauvignon Blanc")+
    facet_wrap(.~ company_a)
 str(site_Jan2020_yr_14_19) 
  
  #yield_t_ha
  
ggplot(site_Jan2020_yr_14_19, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")

#yield_t_ha
filter(site_Jan2020_yr_14_19,yield_t_ha > 2 & yield_t_ha < 100 ) %>% 
ggplot( aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")

#yield_t_ha
filter(site_Jan2020_yr_14_19,yield_t_ha > 2 & yield_t_ha < 100) %>% 
  ggplot( aes(year_factor, yield_t_ha, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")+
  facet_wrap(.~ company)

#yield_t_ha with no id
filter(site_Jan2020_yr_14_19,yield_t_ha > 2& yield_t_ha < 100) %>% 
  ggplot( aes(year_factor, yield_t_ha, colour= company_a))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")+
  facet_wrap(.~ company_a)

##yld kg
ggplot(site_Jan2020_yr_14_19, aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield kg/m - Sauvignon Blanc")


#yield_km_m
filter(site_Jan2020_yr_14_19,yield_kg_m > 0) %>% 
  ggplot( aes(year_factor, yield_kg_m, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Yield kg/m - Sauvignon Blanc")+
  facet_wrap(.~ company)


#yield_km_m no id
filter(site_Jan2020_yr_14_19,yield_kg_m > 0) %>% 
  ggplot( aes(year_factor, yield_kg_m, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Yield kg/m - Sauvignon Blanc")+
  facet_wrap(.~ company_a)

str(site_Jan2020_yr_14_19)
summary(site_Jan2020_yr_14_19$brix)
###two site at Villia maria with data enetry problems
#MFOWSB01_2016 and MTEMSB02_2017

filter(site_Jan2020_yr_14_19,brix < 100) %>% 
ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


filter(site_Jan2020_yr_14_19,brix < 100) %>% 
  ggplot( aes(year_factor, brix , colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Brix  - Sauvignon Blanc")+
  facet_wrap(.~ company)


filter(site_Jan2020_yr_14_19,brix < 100) %>% 
  ggplot( aes(year_factor, brix , colour= company_a))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Brix  - Sauvignon Blanc")+
  facet_wrap(.~ company_a)

str(site_Jan2020_yr_14_19)
#filter(site_Jan2020_yr_14_18,bunch_weight < 100) %>% 
  ggplot( site_Jan2020_yr_14_19, aes(year_factor, bunch_weight))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Bunch weight - Sauvignon Blanc")

  ggplot(site_Jan2020_yr_14_19, aes(year_factor, bunch_weight , colour= company))+
    geom_boxplot(alpha=0.1)+
    geom_point( alpha = 0.1)+
    theme_bw()+
    theme(legend.position="none")+
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    labs(x = "Year",
         y= "Bunch weight  - Sauvignon Blanc")+
    facet_wrap(.~ company)
  
  
  ggplot(site_Jan2020_yr_14_19, aes(year_factor, bunch_weight , colour= company_a))+
    geom_boxplot(alpha=0.1)+
    geom_point( alpha = 0.1)+
    theme_bw()+
    theme(legend.position="none")+
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    labs(x = "Year",
         y= "Bunch weight  - Sauvignon Blanc")+
    facet_wrap(.~ company_a)
  
  str(site_Jan2020_yr_14_19$berry_weight)
  
  site_Jan2020_yr_14_19$berry_weight <- as.double(site_Jan2020_yr_14_19$berry_weight)
  
  #filter(site_Jan2020_yr_14_18,bunch_weight < 100) %>% 
  ggplot( site_Jan2020_yr_14_19, aes(year_factor, berry_weight))+
    geom_boxplot(alpha=0.1)+
    geom_point(colour = "blue", alpha = 0.1)+
    theme_bw()+
    labs(x = "Year",
         y= "Berry weight - Sauvignon Blanc")
  
  ggplot(site_Jan2020_yr_14_19, aes(year_factor, berry_weight , colour= company))+
    geom_boxplot(alpha=0.1)+
    geom_point( alpha = 0.1)+
    theme_bw()+
    theme(legend.position="none")+
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    labs(x = "Year",
         y= "Berry weight  - Sauvignon Blanc")+
    facet_wrap(.~ company)
  
  
  ggplot(site_Jan2020_yr_14_19, aes(year_factor, berry_weight , colour= company_a))+
    geom_boxplot(alpha=0.1)+
    geom_point( alpha = 0.1)+
    theme_bw()+
    theme(legend.position="none")+
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    labs(x = "Year",
         y= "Berry weight  - Sauvignon Blanc")+
    facet_wrap(.~ company_a)
  
  
  
  
  #bunch_numb_m 
  
  ggplot( site_Jan2020_yr_14_19, aes(year_factor, bunch_numb_m))+
    geom_boxplot(alpha=0.1)+
    geom_point(colour = "blue", alpha = 0.1)+
    theme_bw()+
    labs(x = "Year",
         y= "Bunch number per m - Sauvignon Blanc")
  
  ggplot(site_Jan2020_yr_14_19, aes(year_factor, bunch_numb_m , colour= company))+
    geom_boxplot(alpha=0.1)+
    geom_point( alpha = 0.1)+
    theme_bw()+
    theme(legend.position="none")+
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    labs(x = "Year",
         y= "Bunch number per m  - Sauvignon Blanc")+
    facet_wrap(.~ company)
  
  
  ggplot(site_Jan2020_yr_14_19, aes(year_factor, bunch_numb_m , colour= company_a))+
    geom_boxplot(alpha=0.1)+
    geom_point( alpha = 0.1)+
    theme_bw()+
    theme(legend.position="none")+
    theme(axis.text.x=element_text(angle=90,hjust=1)) +
    labs(x = "Year",
         y= "Bunch number per m  - Sauvignon Blanc")+
    facet_wrap(.~ company_a)
  
  
  
  
  
  
  
  
  ##############################################################################################################
  ############             site by site check               ##########################################
  #############################################################################################################
  
  #install.packages("plotly")
  library(plotly)
  
  site_Jan2020$na_count_factor <- as.factor(site_Jan2020$na_count)
  site_Jan2020$year_factor <- as.factor(site_Jan2020$year)
  
  
  str(site_Jan2020)
  dim(site_Jan2020)
  unique(site_Jan2020$company)
  #1. Whitehaven
  #site_Jan2020_to_plot <- filter(site_Jan2020, company == "Whitehaven" )
  #site_Jan2020_to_plot <- filter(site_Jan2020, company == "Pernod Ricard" )
  #site_Jan2020_to_plot <- filter(site_Jan2020, company == "Villa Maria" )
  #site_Jan2020_to_plot <- filter(site_Jan2020, company == "Wither Hills" )
  # site_Jan2020_to_plot <- filter(site_Jan2020, company == "Delegat" ) %>% 
  # filter( x_coord > 0)
  #site_Jan2020_to_plot <- filter(site_Jan2020, company == "Constellation" )
  #site_Jan2020_to_plot <- filter(site_Jan2020, company == "Giesen" )
  #site_Jan2020_to_plot <- filter(site_Jan2020, company == "Yealands" )
  #site_Jan2020_to_plot <- filter(site_Jan2020, company == "Wine Portfolio" )
  site_Jan2020_to_plot <- filter(site_Jan2020, company == "Rob Agnew" )
  #site_Jan2020_to_plot <- filter(site_Jan2020, company == "Cloudy Bay" )
  
  with(site_Jan2020_to_plot,  table(  year, company))
  
  dim(site_Jan2020_to_plot) #780
  filter(site_Jan2020_to_plot, x_coord > 0) #780
  
  ggplot(site_Jan2020_to_plot, aes(na_count_factor, na_count))+
    geom_col()+
    theme_bw()+
    facet_wrap(.~ year)+
    labs(x = "count of missing data entries",
         y= "Total counts of missing data entries NA - Sauvignon Blanc")
  #what is the missing data?
  
  #1. Harvest date juilan days 
  str(site_Jan2020_to_plot)
  Julian <- ggplot( site_Jan2020_to_plot, aes(year_factor, julian))+
    geom_boxplot(alpha=0.1)+
    geom_point(colour = "blue", alpha = 0.1)+
    theme_bw()+
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=10,))+
    labs(x = "",
         y= "Julian days")
  Julian_plotly <- ggplotly(Julian)
  Julian_plotly
  Julian
 #how much data is missing for julian days?
  missing_data <- site_Jan2020_to_plot %>%
    group_by(year ) %>% 
    summarise(count = sum(is.na(julian)))  
  missing_data
  #2. yield_t_ha 
  
 # yield_t_ha <- ggplot( site_Jan2020_to_plot, aes(year_factor, yield_t_ha))+
  yield_t_ha <- filter(site_Jan2020_to_plot, yield_t_ha <100 ) %>% 
     ggplot(  aes(year_factor, yield_t_ha))+
    geom_boxplot(alpha=0.1)+
    geom_point(colour = "blue", alpha = 0.1)+
    theme_bw()+
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=10,))+
    labs(x = "",
         y= "yield t/ha")
  yield_t_ha_plotly <- ggplotly(yield_t_ha)
  yield_t_ha_plotly
  yield_t_ha
  #how much data is missing for ?
  missing_data <- site_Jan2020_to_plot %>%
    group_by(year ) %>% 
    summarise(count = sum(is.na(yield_t_ha)))  
  missing_data
  
  
  #3.  yield_kg_m 
  
  yield_kg_m <- ggplot( site_Jan2020_to_plot, aes(year_factor, yield_kg_m))+
    geom_boxplot(alpha=0.1)+
    geom_point(colour = "blue", alpha = 0.1)+
    theme_bw()+
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=10,))+
    labs(x = "",
         y= "yield kg/m")
  yield_kg_m_plotly <- ggplotly(yield_kg_m)
  yield_kg_m_plotly
  yield_kg_m
  #how much data is missing for ?
  missing_data <- site_Jan2020_to_plot %>%
    group_by(year ) %>% 
    summarise(count = sum(is.na(yield_kg_m)))  
  missing_data
  
  #4.  brix 
  
  brix <- ggplot( site_Jan2020_to_plot, aes(year_factor, brix))+
    geom_boxplot(alpha=0.1)+
    geom_point(colour = "blue", alpha = 0.1)+
    theme_bw()+
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=10,))+
    labs(x = "",
         y= "brix")
  brix_plotly <- ggplotly(brix)
  brix_plotly
  brix
  #how much data is missing for ?
  missing_data <- site_Jan2020_to_plot %>%
    group_by(year ) %>% 
    summarise(count = sum(is.na(brix)))  
  missing_data
  
  #5. bunch_weight 
  
  bunch_weight <- ggplot( site_Jan2020_to_plot, aes(year_factor, bunch_weight))+
    geom_boxplot(alpha=0.1)+
    geom_point(colour = "blue", alpha = 0.1)+
    theme_bw()+
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=10,))+
    labs(x = "",
         y= "Bunch weight")
  bunch_weight_plotly <- ggplotly(bunch_weight)
  bunch_weight_plotly
  bunch_weight
  #how much data is missing?
  missing_data <- site_Jan2020_to_plot %>%
    group_by(year ) %>% 
    summarise(count = sum(is.na(bunch_weight)))  
  missing_data
  
  #6. berry_weight 
  
  site_Jan2020_to_plot$berry_weight <- as.double(site_Jan2020_to_plot$berry_weight)
  
  berry_weight <-  ggplot( site_Jan2020_to_plot, aes(year_factor, berry_weight))+
    geom_boxplot(alpha=0.1)+
    geom_point(colour = "blue", alpha = 0.1)+
    theme_bw()+
    theme(axis.text=element_text(size=8),
    axis.title=element_text(size=10,))+
        labs(x = "",
         y= "Berry weight")
  berry_weight
  berry_weight_plotly <- ggplotly(berry_weight)
  berry_weight_plotly
  
  #how much data s?
  missing_data <- site_Jan2020_to_plot %>%
    group_by(year ) %>% 
    summarise(count = sum(is.na(berry_weight)))  
  missing_data
  
  
 
  #6. bunch_numb_m 
  
  site_Jan2020_to_plot$berry_weight <- as.double(site_Jan2020_to_plot$berry_weight)
  
  bunch_numb_m <-  ggplot( site_Jan2020_to_plot, aes(year_factor, bunch_numb_m))+
    geom_boxplot(alpha=0.1)+
    geom_point(colour = "blue", alpha = 0.1)+
    theme_bw()+
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=10,))+
    labs(x = "",
         y= "bunch numb m")
  bunch_numb_m_plotly <- ggplotly(bunch_numb_m)
  bunch_numb_m_plotly
  bunch_numb_m
  #how much data s?
  missing_data <- site_Jan2020_to_plot %>%
    group_by(year ) %>% 
    summarise(count = sum(is.na(bunch_numb_m)))  
  missing_data
  
  
  
  Rob_Agnew <-  plot_grid(Julian, yield_t_ha, yield_kg_m , brix, bunch_weight, berry_weight, bunch_numb_m,
            #labels = c("A", "B", "C", "D", "E", "F"),
            ncol = 2, nrow = 3)
  
  Pernod_Ricard
  
  # Site 1.
  Whitehaven
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Whitehaven.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_whitehaven.csv")
  #NB save yield data only
  site_Jan2020_to_plot_yld <- filter(site_Jan2020_to_plot,
                                     yield_t_ha != "NA")
  write_csv(site_Jan2020_to_plot_yld, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_whitehaven_yld.csv")
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  # Site 2.
  Pernod_Ricard
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Pernod_Ricard.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Pernod_Ricard.csv")
  
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  
  # Site 3.
  Villa_Maria
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Villa_Maria.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Villa_Maria.csv")
  
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  
  
  # Site 4.
  Wither_Hills
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Wither_Hills.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Wither_Hills.csv")
  
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  
  
  # Site 5.
  Delegat
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Delegats.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Delegat.csv")
  
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  
  
  # Site 6.
  Constellation
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Constellation.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Constellation.csv")
  
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  
  
  # Site 7.
  Giesen
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Giesen.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Giesen.csv")
  
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  
  # Site 8.
  Yealands
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Yealands.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Yealands.csv")
  
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  
  # Site 9.
  Wine_Portfolio
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Wine_Portfolio.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Wine_Portfolio.csv")
  
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  
  # Site 10.
  Rob_Agnew
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Rob_Agnew.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Rob_Agnew.csv")
  
  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m"))
  
  #Site 11.
  Cloudy_Bay
  graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites")
  ggsave(path= graph_path, filename = "Cloudy_Bay.png", device = "png" ,
         width = 20, height = 18, units = "cm")
  write_csv(site_Jan2020_to_plot, "//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/check_sites/site_Jan2020_to_plot_Cloudy_Bay.csv")
  