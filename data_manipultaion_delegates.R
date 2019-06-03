library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)
######################################################################################################################
################                         Make DF with GPS coodinates                                 #################
######################################################################################################################

delegates_GPS1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Delegat For MRC Project RGVB rev.xlsx", 
                                 sheet = "Use this Delegat4 NZ2000")
glimpse(delegates_GPS1)
delegates_GPS <- delegates_GPS1 %>% 
  select(ID_temp = Name ,
         x_coord = POINT_X,
         y_coord = POINT_Y) %>% 
  mutate(ID_temp = gsub( "-", "_", ID_temp),
         ID_temp =str_to_lower(ID_temp, locale = "en")) 

glimpse(delegates_GPS)


######################################################################################################################
################                         Make DF of block info                              #################
######################################################################################################################

####Bring in the sub_block info#####
delegates_sub_block1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Delegat For MRC Project RGVB rev.xlsx", 
                                   sheet = "Sub Block info")
glimpse(delegates_sub_block1)

delegates_sub_block <- delegates_sub_block1 %>% 
  mutate(ID_temp = str_to_lower(`Sub-Block Name`, locale = "en"),
         ID_temp = gsub( "-", "_", ID_temp)) %>% 
  select(ID_temp,
         row_width = `Row Spacing (m) (Block)`,
         vine_spacing = `Vine Spacing (m) (Block)`)
glimpse(delegates_sub_block)

######################################################################################################################
################                         join GPS and block info                              #################
######################################################################################################################


glimpse(delegates_sub_block) #47
glimpse(delegates_GPS) #339
delegates_GPS_sub_block <- full_join(delegates_GPS, delegates_sub_block, by= "ID_temp")
glimpse(delegates_GPS_sub_block) #357


######################################################################################################################
################                         bring in the yield data                             #################
######################################################################################################################


delegates_yld_data1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Delegat For MRC Project RGVB rev.xlsx", 
                             sheet = "Yield Info")
glimpse(delegates_yld_data1)

######################################################################################################################
################                         filter the harvest data only                             ####################

delegates_yld_data_harvest1 <- delegates_yld_data1 %>% 
  filter(`Yield Type` == "Actual Yield") 
glimpse(delegates_yld_data_harvest1)

##make an ID clm ####

delegates_yld_data_harvest <- delegates_yld_data_harvest1 %>% 
  mutate(ID_temp = str_to_lower(`Sub-Block`, locale = "en"),
         ID_temp = gsub( "-", "_", ID_temp),
         ID_yr = paste0(ID_temp,"_", Vintage),
         yr = Vintage,
         brix = NA,
         variety = Variety) %>% 
  select(ID_temp,
         ID_yr,
         variety,
         yr,
         brix,
         harvest_date = `Analysis Date`,
         trellis_type = `Trellis Type`,
         pruning_style = `Pruning Method`, #can this be revised with what I used for pernod
         yield_t_ha = `Yield (Tonnes/Hectare)`) %>% 
         #berry_weight_g = `Average Pre-Harvest Berry Weight (g)`,
         #bunch_weight_g = `Average Pre-Harvest Bunch Weight (g)`) %>% 
  mutate(julian = as.numeric(format(harvest_date, "%j")))
glimpse(delegates_yld_data_harvest) #has yield tha here


######################################################################################################################
################                         bring in the Pre - Harvest data                            #################
######################################################################################################################

delegates_yld_data_pre_harvest <- delegates_yld_data1 %>% 
    filter(`Yield Type` == "Pre-Harvest Yield") 
  
  
delegates_yld_data_pre_harvest <- delegates_yld_data_pre_harvest %>% 
    mutate(ID_temp = str_to_lower(`Sub-Block`, locale = "en"),
           ID_temp = gsub( "-", "_", ID_temp),
           ID_yr = paste0(ID_temp,"_", Vintage),
           yr = Vintage) %>% 
    select(ID_yr,
           berry_weight = `Average Pre-Harvest Berry Weight (g)`,
           bunch_weight = `Average Pre-Harvest Bunch Weight (g)`)
glimpse(delegates_yld_data_pre_harvest)

#join pre harvest and yield data togther
glimpse(delegates_yld_data_pre_harvest) #514
glimpse(delegates_yld_data_harvest) # 540

delegates_yld_data <- full_join(delegates_yld_data_harvest, delegates_yld_data_pre_harvest, by= "ID_yr")
#check <- full_join(delegates_yld_data_harvest, delegates_yld_data_pre_harvest, by= "ID_yr")
#glimpse(check)
glimpse(delegates_yld_data)


######################################################################################################################
################                         join GPS data to Pre and Harvest data                            #################
######################################################################################################################


glimpse(delegates_GPS_sub_block) #357
glimpse(delegates_yld_data) # 524

delegates_GPS_sub_block_yld <- full_join(delegates_yld_data,delegates_GPS_sub_block, by= "ID_temp" )
glimpse(delegates_GPS_sub_block_yld) #853


#### ADD Some extra data clms - need to check this #####
glimpse(delegates_GPS_sub_block_yld)

delegates_GPS_sub_block_yld <- delegates_GPS_sub_block_yld %>% 
  mutate(yield_kg_m 	= ( yield_t_ha * 1000) / (10000/row_width), # is row spacing and row width the same thing?
         #bunch_numb_m = bunch_numb_per_vine / Vine_Spacing_m,
         bunch_numb_m = NA,
         #bunch_mass_g 		= 1000 * yield_kg_m /bunch_numb_m,
         bunch_mass_g 		= NA,
         berry_bunch 		= bunch_weight / berry_weight,
         berry_wt = NA,
         company = "Delegates",
         year = yr,
         row_width  ,
         vine_spacing )
glimpse(delegates_GPS_sub_block_yld)

delegates_april_2019 <- delegates_GPS_sub_block_yld %>% 
select(company, ID_temp, ID_yr, variety, x_coord, y_coord,
       year, harvest_date, julian,yield_t_ha,yield_kg_m,
       brix,bunch_weight, berry_weight,
       bunch_numb_m, 
       #bunch_mass_g, berry_bunch, berry_wt,
       pruning_style,
       row_width,
       vine_spacing)
glimpse(delegates_april_2019)
delegates_april_2019$na_count <- apply(is.na(delegates_april_2019), 1, sum)

#glimpse(delegates_april_2019)
#write_csv(delegates_april_2019, "delegates_april_2019.csv")




######################################################################################################################
################                         view and summaries DF                             #################
######################################################################################################################


dim(delegates_april_2019)
#how many site?
dim(delegates_april_2019)
glimpse(delegates_april_2019$year) #853 records

max(delegates_april_2019$year, na.rm = TRUE) #2006 -2018
min(delegates_april_2019$year, na.rm = TRUE) #2006 -2018


#how many sites with GPS pts
glimpse(delegates_april_2019)#853 all records records
colSums(is.na(delegates_april_2019)) #218 with missing GPS records
GPS_only_delegates <- filter(delegates_april_2019, x_coord> 0)
glimpse(GPS_only_delegates) #635



#how many sites with GPS pts by Variety using filter
filter(delegates_april_2019, x_coord > 0) %>% 
  ggplot( aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites with GPS coordinates")
#how many sites with GPS pts by Variety using df with just GPS pts (same graph different methods)
ggplot(GPS_only_delegates, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites with GPS coordinates")




#how many sites by Variety by year with just the GPS data
ggplot(GPS_only_delegates, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#how many sites by Variety with just the GPS data all the data by year
ggplot(delegates_april_2019, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#how many sites by Variety with just the GPS data all the data
ggplot(delegates_april_2019, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")

#create a new variable year_as_factor
delegates_april_2019$year_factor <- as.factor(delegates_april_2019$year)
glimpse(delegates_april_2019)
#filter data for Sauvignon Blanc
delegates_april_2019_sau <- filter(delegates_april_2019, variety == "Sauvignon Blanc") 
glimpse(delegates_april_2019_sau)

#how many sites for Sauvignon Blanc by year
group_by(delegates_april_2019_sau, year) %>% 
  count()
#how many sites for Sauvignon Blanc have missing data - how much missing data?
ggplot(delegates_april_2019_sau, aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")
#how many sites for Sauvignon Blanc have missing data - missing data grouped together?
ggplot(delegates_april_2019_sau, aes(na_count))+
  geom_bar()+
  scale_x_continuous(breaks =  c(2,4,6,8,10))+
  facet_wrap(~year_factor)+
  theme_bw()+
  labs(x = "number of na counts per entry",
       y= "Counts of missing data entries NA")


glimpse(delegates_april_2019_sau)
#julian days
ggplot(delegates_april_2019_sau, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")

#julian days with zero filtered out

filter(delegates_april_2019_sau,julian >20 ) %>% 
  ggplot( aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
  
  
  
  
  
  
#yield_t_ha
ggplot(delegates_april_2019_sau, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")
#yield_kg_m
ggplot(delegates_april_2019_sau, aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")

#yield_kg_m filter out zeros
filter(delegates_april_2019_sau,yield_kg_m != 0) %>% 
  ggplot( aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")


#brix - too many zero
ggplot(delegates_april_2019_sau, aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


#brix - filter out zero
filter(delegates_april_2019_sau,brix != 0) %>% 
  ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


############################################################################## 
########################    File to use   ####################################
delegates_april_2019_sau <- select(delegates_april_2019_sau, -year_factor)
glimpse(delegates_april_2019_sau)
write_csv(delegates_april_2019_sau, "V:/Marlborough regional/working_jaxs/delegates_april_2019_sau.csv")
##############################################################################   



