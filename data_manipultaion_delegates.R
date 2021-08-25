library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)
######################################################################################################################
################                         Make DF with GPS coodinates                                 #################
######################################################################################################################

delegates_GPS1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Delegat/Delegat For MRC Project RGVB rev.xlsx", 
                                 sheet = "Use this Delegat4 NZ2000")
glimpse(delegates_GPS1)
delegates_GPS <- delegates_GPS1 %>% 
  select(ID_temp = Name ,
         x_coord = POINT_X,
         y_coord = POINT_Y) %>% 
  mutate(ID_temp = gsub( "-", "_", ID_temp),
         ID_temp =str_to_lower(ID_temp, locale = "en")) 

glimpse(delegates_GPS)
#recode lig_a1_sb
names(delegates_GPS)
delegates_GPS <- mutate(delegates_GPS,ID_temp = case_when(
  ID_temp == "lig_a1_sb" ~ "lig_a_sb",
  TRUE ~ ID_temp))


delegates_GPS_extra <- read.csv("V:/Marlborough regional/Regional winery data/Raw_data/Delegat/extra_sites_Delegats_jaxs.csv")
names(delegates_GPS_extra)
delegates_GPS_extra <- delegates_GPS_extra %>% 
  select(ID_temp = Name ,
         x_coord = xcoord ,
         y_coord = ycoord ) %>% 
  mutate(ID_temp = gsub( "-", "_", ID_temp),
         ID_temp =str_to_lower(ID_temp, locale = "en")) 

delegates_GPS <- bind_rows(delegates_GPS, delegates_GPS_extra)

rm("delegates_GPS_extra", "delegates_GPS1")

######################################################################################################################
################                         Make DF of block info                              #################
######################################################################################################################

####Bring in the sub_block info#####
delegates_sub_block1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Delegat/Delegat For MRC Project RGVB rev.xlsx", 
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

rm("delegates_GPS", "delegates_sub_block",  "delegates_sub_block1")

#
################                         bring in the yield data                             #################
######################################################################################################################


delegates_yld_data1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Delegat/Delegat For MRC Project RGVB rev.xlsx", 
                             sheet = "Yield Info")
glimpse(delegates_yld_data1)
unique(delegates_yld_data1$Vintage)
delegates_yld_data2 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Delegat/Mike Trought Yield Data V19_2.xlsx", 
                                  sheet = "2019 Yield Data")
unique(delegates_yld_data2$Vintage)
names(delegates_yld_data1)
names(delegates_yld_data2)
#bind togther
delegates_yld_data1 <- bind_rows(delegates_yld_data1, delegates_yld_data2)
unique(delegates_yld_data1$Vintage)

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
unique(delegates_yld_data_harvest$yr)

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
unique(delegates_yld_data$yr)


rm( "delegates_yld_data1", "delegates_yld_data_harvest", 
    "delegates_yld_data_harvest1", "delegates_yld_data_pre_harvest")
######################################################################################################################
################                         join GPS data to Pre and Harvest data                            #################
######################################################################################################################


glimpse(delegates_GPS_sub_block) #358
glimpse(delegates_yld_data) # 524

delegates_GPS_sub_block_yld <- full_join(delegates_yld_data,delegates_GPS_sub_block, by= "ID_temp" )
glimpse(delegates_GPS_sub_block_yld) #853
unique(delegates_GPS_sub_block_yld$yr)

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



unique(delegates_april_2019$variety)
#Only keep sb
delegates_april_2019 <- mutate(delegates_april_2019,
               variety_check =
                 str_sub(delegates_april_2019$ID_temp, -2, -1))
delegates_april_2019 <- filter(delegates_april_2019, variety_check == "sb")
delegates_april_2019 <- dplyr::select(delegates_april_2019,-variety_check)



dim(delegates_april_2019) #589
names(delegates_april_2019)
unique(delegates_april_2019$year)

count(filter(delegates_april_2019, na_count == 14))
count(filter(delegates_april_2019, is.na(x_coord)))
test <- filter(delegates_april_2019, is.na(x_coord))
test <- arrange(test,
                ID_temp, year)

test1 <- filter(delegates_april_2019, na_count == 14)
test1 <- arrange(test1,
                ID_temp, year)
#write_csv(test1, "V:/Marlborough regional/working_jaxs/July2020/delegates_april_no_yld_data_but_cords.csv")

#remove the row that just have block name and coodinates eg dont have harvest data
delegates_april_2019 <- filter(delegates_april_2019,
                               na_count != 14)

names(delegates_april_2019)
delegates_april_2019 <-
  delegates_april_2019 %>%
  mutate(bunch_weight = case_when(bunch_weight == 0.0000 ~ NA_real_,
                                TRUE ~ bunch_weight))
unique(delegates_april_2019$year)
############################################################################## 
########################    File to use   ####################################

write_csv(delegates_april_2019, "V:/Marlborough regional/working_jaxs/July2020/delegates_april_2019_sau.csv")
##############################################################################   






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
########################################################################################################################

#Revised delegates_april_2019_sau 21/0/2021
names(delegates_april_2019_sau)

#just need to make a block 


delegates_april_2019_sau <- delegates_april_2019_sau %>% separate(ID_yr, c("Block"), sep = "_", remove = FALSE)

#1. How many sites?
#for each year
delegates_april_2019_sau %>%
  group_by(year) %>%
  summarise(count = n_distinct(Block))
#overall for the data set from 2014-2019 how many blocks do we have?
delegates_april_2019_sau %>%
  summarise(count = n_distinct(Block))

#2. For harvest date how many sites per year?
names(delegates_april_2019_sau)

delegates_april_2019_sau %>%
  group_by(year) %>%
  summarise(mean_julian_days = mean(julian, na.rm = TRUE),
            min_julian_days = min(julian, na.rm = TRUE),
            max_julian_days = max(julian, na.rm = TRUE),
            sum_na = sum(!is.na(julian)))

#3. For yield kg/m  how many sites per year

delegates_april_2019_sau %>%
  group_by(year) %>%
  summarise(mean_yield_kg_m = mean(yield_kg_m, na.rm = TRUE),
            min_yield_kg_m = min(yield_kg_m, na.rm = TRUE),
            max_yield_kg_m = max(yield_kg_m, na.rm = TRUE),
            sum_na = sum(!is.na(yield_kg_m)))



