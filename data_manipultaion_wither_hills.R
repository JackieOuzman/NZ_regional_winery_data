library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)


#####################################################################################################################
##############################           GPS POINTS for blocks #########################################################
#########################################################################################################################

wither_hills_GPS_temp <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wither_hills/Wither Hills yield data For Mike Trought RGVB.xlsx",
                                    sheet = "Wither Hills NZ2000")



glimpse(wither_hills_GPS_temp)
wither_hills_GPS <- wither_hills_GPS_temp %>% 
                    select(ID_temp = Name ,
                    x_coord = POINT_X,
                    y_coord = POINT_Y) %>% 
                    mutate(ID_temp = tolower(gsub( "-", "_", ID_temp)))

glimpse(wither_hills_GPS)
#####################################################################################################################
##############################           Add in the block info  #########################################################
#########################################################################################################################


wither_hills_block_info <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wither_hills/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                                    sheet = "locations" , skip = 3)
glimpse(wither_hills_block_info)
wither_hills_block_info <- wither_hills_block_info %>% 
                            select(
                            ID_temp = `...1`, 
                            variety,
                            row_spacing = `row spacing (m)`) %>% 
                            mutate(ID_temp = tolower(gsub( "-", "_", ID_temp)))
glimpse(wither_hills_block_info)
###there is a data entry error om__0.3a_sub find and replace with om_03a_sb

wither_hills_block_info <- mutate(wither_hills_block_info,
                                  ID_temp =  case_when(
                                    ID_temp == "om__0.3a_sb" ~ "om_03a_sb",
                               TRUE ~ ID_temp))
###Join GPS and block data

glimpse(wither_hills_GPS) #85
glimpse(wither_hills_block_info) #83

wither_hills_GPS_block_info <- full_join(wither_hills_GPS,wither_hills_block_info, by = "ID_temp")

glimpse(wither_hills_GPS_block_info) #86
#drop the winery data entery
wither_hills_GPS_block_info <- filter(wither_hills_GPS_block_info, ID_temp != "winery")

glimpse(wither_hills_GPS_block_info) #85

#####################################################################################################################
##############################           Add in the yield measures  #########################################################
#########################################################################################################################

wither_hills_harvest_details <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wither_hills/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                                    sheet = "15,16,17,18 ", skip = 3)
glimpse(wither_hills_harvest_details)
#bm_02_(spur09)_pn is only block name on the harvest data, and GPS point only have bm_02_pn – make the assumption that they are the same?
wither_hills_harvest_details <- mutate(wither_hills_harvest_details,
                                       Block =  case_when(
                                         Block == "BM 02 (spur 09)" ~ "BM 02",
                                         TRUE ~ Block))

## A few needs a name change...SC01 to SC 01 and SC05 to SC 05 and SC06 to SC 06 OR 04a&b, OR03, OR 01a&b

wither_hills_harvest_details <- mutate(wither_hills_harvest_details,
                                       Block =  case_when(
                                         Block == "SC01" ~ "SC 01",
                                         Block == "SC05" ~ "SC 05",
                                         Block == "SC06" ~ "SC 06",
                                         Block == "OR 04a&b" ~ "OR 04",
                                         Block == "OR03" ~ "OR 03",
                                         Block == "OR04" ~ "OR 04",
                                         Block == "OR 01a&b" ~ "OR 01",
                                         TRUE ~ Block))

#Create an ID clm
wither_hills_harvest_details_step1 <- wither_hills_harvest_details %>% 
         mutate(vineyard_lower =str_to_lower(Vineyard, locale = "en"),
         block_lower =str_to_lower(Block, locale = "en"),
         Variety_lower =str_to_lower(Variety, locale = "en"),
         year = gsub( "V|v", "20", wither_hills_harvest_details$Vintage),
         ha =`Area \r\n(ha)`)
glimpse(wither_hills_harvest_details_step1)

wither_hills_harvest_details_step2 <- wither_hills_harvest_details_step1 %>% 
         mutate(ID_temp1 = gsub( " ", "_", wither_hills_harvest_details_step1$block_lower),
         ID_temp = paste0(ID_temp1,"_", Variety_lower),
         ID_yr = paste0(ID_temp,"_", year),
         ID_yr_ha = paste0(ID_yr, "_", ha))  
glimpse(wither_hills_harvest_details_step2)



### what are the reps?? and keep only the ones with the largest ha
what_reps <- wither_hills_harvest_details_step2 %>% group_by(ID_yr) %>% 
  filter(n() > 1)

what_reps <-mutate(what_reps,
                   ID_yr_ha = paste0(ID_yr, "_", ha)) 
print(what_reps)

#Yes there are a few lets just use the one that has tne biggest ha...
biggest_rep <- what_reps %>% 
  group_by(ID_yr) %>%
  summarise(value = max(ha, na.rm = FALSE))
biggest_rep <- mutate(biggest_rep,
                      ID_yr_ha = paste0(ID_yr, "_", value))  

print(biggest_rep)
#use this to make a list of sites I dont want to use in analysis
what_to_chuck <- what_reps %>%
  filter(!ID_yr_ha %in% biggest_rep$ID_yr_ha)
#This is removing the sites I dont want to use
wither_hills_harvest_details_step2 <- wither_hills_harvest_details_step2 %>%
  filter(!ID_yr_ha %in% what_to_chuck$ID_yr_ha)


####Note that there is something wrong with the input date clm we have lots of wrong dates
# replace date error with NA


wither_hills_harvest_details <- wither_hills_harvest_details_step2 %>% 
    mutate(
      harvest_date1 = ifelse(Harvest < 1980, NA , Harvest))

wither_hills_harvest_details$harvest_date1 <- as_datetime(wither_hills_harvest_details$harvest_date1)
glimpse(wither_hills_harvest_details)


#####################################################################################################################
##############################           Add in the calulated yield measures  #########################################################
#########################################################################################################################


#cals and selected clm - need to double check cals
wither_hills_harvest_details <- wither_hills_harvest_details %>% 
  select(ID_yr,ID_temp, 
         #Variety = Variety_lower,
         year,
         brix = `Harvest brix` , #no data in the input file
         harvest_date = harvest_date1, #something wrong here the input file has this problem
         yield_t_ha = `Actual T/Ha`,
         #metres_row_ha =`metres row/ha`, there is an error in the input data file dont use this clm
         vine_spacing = `Vine Spacing` ,
         row_width = `Row Width` ,
         pruning_style = `Pruning style`, 
         bunch_numb_per_vine = `Ave Pre-Harvest Bunch #`,
         bunch_weight = `Ave Pre-Harvest Bunch weight`,
         berry_weight = `Ave Pre-Harvest Berry weight`) 

glimpse(wither_hills_harvest_details)

wither_hills_harvest_details <- wither_hills_harvest_details %>%  
  mutate(pruning_style = as.double(gsub( " cane", "", pruning_style)),
         yield_kg_m = (yield_t_ha * 1000) / (10000/row_width), #check this cal
         bunch_numb_m = bunch_numb_per_vine / vine_spacing , #check this cal
         bunch_mass_g = 1000 * yield_kg_m /bunch_numb_m, #check this cal
         berry_bunch = bunch_weight / berry_weight,
         berry_wt = bunch_mass_g / berry_bunch,
         julian = as.numeric(format(harvest_date, "%j")))


glimpse(wither_hills_harvest_details)

#####################################################################################################################
##############################           Join yield measures to GPS data  #########################################################
#########################################################################################################################


#### join the GPS files to the harvest data files
glimpse(wither_hills_GPS_block_info) #85
glimpse(wither_hills_harvest_details) #583
wither_hills_GPS_block_info_harvest <- full_join(wither_hills_GPS_block_info, wither_hills_harvest_details,
                                                 by= "ID_temp") %>% 
  mutate(company = "Wither_Hills")
glimpse(wither_hills_GPS_block_info_harvest) #606

wither_hills_GPS_block_info_harvest <- wither_hills_GPS_block_info_harvest %>% 
  select(company, ID = ID_temp, variety, x_coord, y_coord,
         year,harvest_date, julian,yield_t_ha,yield_kg_m,
         brix,bunch_weight, berry_weight,pruning_style,
         bunch_numb_m,
         row_width, vine_spacing
         #bunch_mass_g, berry_bunch, berry_wt,
         )
glimpse(wither_hills_GPS_block_info_harvest)


#the date is not set as such need to be fixed up

glimpse(wither_hills_GPS_block_info_harvest$year)

wither_hills_GPS_block_info_harvest$year <- as.double(wither_hills_GPS_block_info_harvest$year)


wither_hills_GPS_block_info_harvest$na_count <- apply(is.na(wither_hills_GPS_block_info_harvest), 1, sum)

glimpse(wither_hills_GPS_block_info_harvest)

######################################################################################################################
##############         we have 2019 yield data ############################################################################

wither2019_hills_harvest_details <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wither_hills/V15-V19 Mapping Project Data.xlsx", 
                                           sheet = "15,16,17,18,19 ", skip = 3)
glimpse(wither2019_hills_harvest_details)
unique(wither2019_hills_harvest_details$Variety)

#bm_02_(spur09)_pn is only block name on the harvest data, and GPS point only have bm_02_pn – make the assumption that they are the same?
wither2019_hills_harvest_details <- mutate(wither2019_hills_harvest_details,
                                       Block =  case_when(
                                         Block == "BM 02 (spur 09)" ~ "BM 02",
                                         TRUE ~ Block))
## A few needs a name change...SC01 to SC 01 and SC05 to SC 05 and SC06 to SC 06 OR 04a&b, OR03

wither2019_hills_harvest_details <- mutate(wither2019_hills_harvest_details,
                                       Block =  case_when(
                                         Block == "SC01" ~ "SC 01",
                                         Block == "SC05" ~ "SC 05",
                                         Block == "SC06" ~ "SC 06",
                                         Block == "OR 04a&b" ~ "OR 04",
                                         Block == "OR03" ~ "OR 03",
                                         Block == "OR04" ~ "OR 04",
                                         Block == "OR 01a&b" ~ "OR 01",
                                         TRUE ~ Block))



## only keep the 2019 data V19
wither2019_hills_harvest_details <- filter(wither2019_hills_harvest_details,
                                           Vintage == "V19" )

#Create an ID clm
str(wither2019_hills_harvest_details)
wither2019_hills_harvest_details_step1 <- wither2019_hills_harvest_details %>% 
  mutate(vineyard_lower =str_to_lower(Vineyard, locale = "en"),
         block_lower =str_to_lower(Block, locale = "en"),
         Variety_lower =str_to_lower(Variety, locale = "en"),
         year = gsub( "V|v", "20", wither2019_hills_harvest_details$Vintage),
         ha =`Area \r\n(ha)`)
glimpse(wither2019_hills_harvest_details_step1)
unique(wither2019_hills_harvest_details_step1$Variety)


wither2019_hills_harvest_details_step2 <- wither2019_hills_harvest_details_step1 %>% 
  mutate(ID_temp1 = gsub( " ", "_", wither2019_hills_harvest_details_step1$block_lower),
         ID_temp = paste0(ID_temp1,"_", Variety_lower),
         ID_yr = paste0(ID_temp,"_", year),
         ID_yr_ha = paste0(ID_yr, "_", ha))
glimpse(wither2019_hills_harvest_details_step2)
unique(wither2019_hills_harvest_details_step2$Variety)

#This is removing the sites I dont want to use
wither2019_hills_harvest_details_step2 <- wither2019_hills_harvest_details_step2 %>%
  filter(!ID_yr_ha %in% what_to_chuck$ID_yr_ha)


####Note that there is something wrong with the input date clm we have lots of wrong dates
# replace date error with NA


wither_hills2019_harvest_details <- wither2019_hills_harvest_details_step2 %>% 
  mutate(
    harvest_date1 = ifelse(Harvest < 1980, NA , Harvest))

wither_hills2019_harvest_details$harvest_date1 <- as_datetime(wither_hills2019_harvest_details$harvest_date1)
glimpse(wither_hills2019_harvest_details)
unique(wither_hills2019_harvest_details$Variety)
#####################################################################################################################
##############################           Add in the calulated yield measures 2019 #########################################################
#########################################################################################################################


#cals and selected clm - need to double check cals
wither_hills2019_harvest_details <- wither_hills2019_harvest_details %>% 
  select(ID_yr,ID_temp, 
         #Variety = Variety_lower,
         year,
         brix = `Harvest brix` , #no data in the input file
         harvest_date = harvest_date1, #something wrong here the input file has this problem
         yield_t_ha = `Actual T/Ha`,
         #metres_row_ha =`metres row/ha`, there is an error in the input data file dont use this clm
         vine_spacing = `Vine Spacing` ,
         row_width = `Row Width` ,
         pruning_style = `Pruning style`, 
         bunch_numb_per_vine = `Ave Pre-Harvest Bunch #`,
         bunch_weight = `Ave Pre-Harvest Bunch weight`,
         berry_weight = `Ave Pre-Harvest Berry weight`) 

glimpse(wither_hills2019_harvest_details)

wither_hills2019_harvest_details <- wither_hills2019_harvest_details %>%  
  mutate(pruning_style = as.double(gsub( " cane", "", pruning_style)),
         yield_kg_m = (yield_t_ha * 1000) / (10000/row_width), #check this cal
         bunch_numb_m = bunch_numb_per_vine / vine_spacing , #check this cal
         bunch_mass_g = 1000 * yield_kg_m /bunch_numb_m, #check this cal
         berry_bunch = bunch_weight / berry_weight,
         berry_wt = bunch_mass_g / berry_bunch,
         julian = as.numeric(format(harvest_date, "%j")))


glimpse(wither_hills2019_harvest_details)

#####################################################################################################################
##############################           Join yield measures to GPS data 2019  #########################################################
#########################################################################################################################


#### join the GPS files to the harvest data files
glimpse(wither_hills_GPS_block_info) #85
glimpse(wither_hills2019_harvest_details) #142
wither_hills2019_GPS_block_info_harvest <- full_join(wither_hills_GPS_block_info, wither_hills2019_harvest_details,
                                                 by= "ID_temp") %>% 
  mutate(company = "Wither_Hills")
glimpse(wither_hills2019_GPS_block_info_harvest) #160

#we have a heap of sites with no coodinates for 2019 data


wither_hills2019_GPS_block_info_harvest <- wither_hills2019_GPS_block_info_harvest %>% 
  select(company, ID = ID_temp, variety, x_coord, y_coord,
         year,harvest_date, julian,yield_t_ha,yield_kg_m,
         brix,bunch_weight, berry_weight,pruning_style,
         bunch_numb_m,
         row_width, vine_spacing
         #bunch_mass_g, berry_bunch, berry_wt,
  )
glimpse(wither_hills2019_GPS_block_info_harvest)


#the date is not set as such need to be fixed up

glimpse(wither_hills2019_GPS_block_info_harvest$year)

wither_hills2019_GPS_block_info_harvest$year <- as.double(wither_hills2019_GPS_block_info_harvest$year)


wither_hills2019_GPS_block_info_harvest$na_count <- apply(is.na(wither_hills2019_GPS_block_info_harvest), 1, sum)

glimpse(wither_hills2019_GPS_block_info_harvest)


#####################################################################################################################
#### join the two togther pre 2019 and 2019#####
str(wither_hills2019_GPS_block_info_harvest)
#write_csv(wither_hills2019_GPS_block_info_harvest, "V:/Marlborough regional/working_jaxs/wither_hills2019_GPS_block_info_harvest.csv")
str(wither_hills_GPS_block_info_harvest)

wither_hills_GPS_block_info_harvest_all_yrs <- bind_rows(wither_hills_GPS_block_info_harvest,wither_hills2019_GPS_block_info_harvest)
unique(wither_hills_GPS_block_info_harvest_all_yrs$variety)

############################################################################## 
########################    File to use   ####################################
wither_hills_GPS_block_info_harvest_all_yrs_all_var <- wither_hills_GPS_block_info_harvest_all_yrs
#write_csv(wither_hills_GPS_block_info_harvest_all_yrs_all_var, "V:/Marlborough regional/working_jaxs/wither_hills_GPS_block_info_harvest_sau.csv")
write_csv(wither_hills_GPS_block_info_harvest_all_yrs_all_var, "V:/Marlborough regional/working_jaxs/wither_hills_GPS_block_info_harvest_all_yrs_all_var.csv")
##############################################################################   

#Only keep sb
wither_hills_GPS_block_info_harvest_all_yrs_sb <- mutate(wither_hills_GPS_block_info_harvest_all_yrs_all_var,
               variety_check = 
                 str_sub(wither_hills_GPS_block_info_harvest_all_yrs_all_var$ID,-2,-1))
wither_hills_GPS_block_info_harvest_all_yrs_sb <- filter(wither_hills_GPS_block_info_harvest_all_yrs_sb, variety_check == "sb")
wither_hills_GPS_block_info_harvest_all_yrs_sb <- dplyr::select(wither_hills_GPS_block_info_harvest_all_yrs_sb,
                                                                -variety_check)
############################################################################## 
########################    File to use just sb  ####################################

write_csv(wither_hills_GPS_block_info_harvest_all_yrs_sb, "V:/Marlborough regional/working_jaxs/wither_hills_GPS_block_info_harvest_sau.csv")

####################################################################################################################

##note we have heaps of missing coordinates this seem to relate to when the block have beeb split up for a trial or something like this
## so what should we do about it?
## how many are for the variety we are intersreted in?
# need to make a new clm for variety at the monmnet this datset is comming from the GPS pts but it also is in the ID clm - let get it!
wither_hills_GPS_block_info_harvest_all_yrs_all_var
test <- mutate(wither_hills_GPS_block_info_harvest_all_yrs_all_var,
                                                              variety_check = 
                                                                str_sub(wither_hills_GPS_block_info_harvest_all_yrs_all_var$ID,-2,-1))
                                                                

unique(test$variety)
unique(test$variety_check)


test_sb <- filter(test, variety_check == "sb")
str(test_sb)
unique(test_sb$x_coord)
test_sb_missing_coord <- test_sb %>%  filter(is.na(x_coord))


test_sb_missing_coord_sites <- distinct(test_sb_missing_coord, ID, .keep_all = TRUE)
dim(test_sb_missing_coord_sites)
write_csv(test_sb_missing_coord_sites, 
          "V:/Marlborough regional/working_jaxs/wither_hills_sb_missing_coord_sites.csv")
#there are 23 sites with no GPS pts



######################################################################################################################
################                         view and summaries DF                             #################
######################################################################################################################


dim(wither_hills_GPS_block_info_harvest)
#how many site?
dim(wither_hills_GPS_block_info_harvest)
glimpse(wither_hills_GPS_block_info_harvest$year) #606 records

max(wither_hills_GPS_block_info_harvest$year, na.rm = TRUE) #2015 -2018
min(wither_hills_GPS_block_info_harvest$year, na.rm = TRUE) #2015 -2018


#how many sites with GPS pts
glimpse(wither_hills_GPS_block_info_harvest)#606 records
colSums(is.na(wither_hills_GPS_block_info_harvest)) #330 with missing GPS records
GPS_only <- filter(wither_hills_GPS_block_info_harvest, x_coord> 0)
glimpse(GPS_only)



#how many sites with GPS pts by Variety using filter
filter(wither_hills_GPS_block_info_harvest, x_coord > 0) %>% 
  ggplot( aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites with GPS coordinates")
#how many sites with GPS pts by Variety using df with just GPS pts (same graph different methods)
ggplot(GPS_only, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites with GPS coordinates")




#how many sites by Variety by year with just the GPS data
ggplot(wither_hills_GPS_block_info_harvest, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#how many sites by Variety with just the GPS data all the data
ggplot(wither_hills_GPS_block_info_harvest, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")



#create a new variable year_as_factor
wither_hills_GPS_block_info_harvest$year_factor <- as.factor(wither_hills_GPS_block_info_harvest$year)
glimpse(wither_hills_GPS_block_info_harvest)
#filter data for Sauvignon Blanc
wither_hills_GPS_block_info_harvest_sau <- filter(wither_hills_GPS_block_info_harvest, variety == "sb") 
glimpse(wither_hills_GPS_block_info_harvest_sau)

#how many sites for Sauvignon Blanc by year
group_by(wither_hills_GPS_block_info_harvest_sau, year) %>% 
  count()
#how many sites for Sauvignon Blanc have missing data - how much missing data?
ggplot(wither_hills_GPS_block_info_harvest_sau, aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")
#how many sites for Sauvignon Blanc have missing data - missing data grouped together?
ggplot(wither_hills_GPS_block_info_harvest_sau, aes(na_count))+
  geom_bar()+
  scale_x_continuous(breaks =  c(2,4,6,8,10))+
  facet_wrap(~year_factor)+
  theme_bw()+
  labs(x = "number of na counts per entry",
       y= "Counts of missing data entries NA")


glimpse(wither_hills_GPS_block_info_harvest_sau)
#julian days
ggplot(wither_hills_GPS_block_info_harvest_sau, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
#yield_t_ha
ggplot(wither_hills_GPS_block_info_harvest_sau, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")
#yield_kg_m
ggplot(wither_hills_GPS_block_info_harvest_sau, aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")

#yield_kg_m filter out zeros
filter(wither_hills_GPS_block_info_harvest_sau,yield_kg_m != 0) %>% 
  ggplot( aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")


#brix - too many zero
ggplot(wither_hills_GPS_block_info_harvest_sau, aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


#brix - filter out zero
filter(wither_hills_GPS_block_info_harvest_sau,brix != 0) %>% 
  ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


###########################################################################################################################################
##############   how do I know if the sites are doubled up?? and what to do about it?   ###################################################

check_double_up <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wither_hills/V15-V19 Mapping Project Data.xlsx", 
                                               sheet = "15,16,17,18,19 ", skip = 3)
glimpse(check_double_up)
unique(check_double_up$Variety)

check_double_up_sb <- filter(check_double_up,Variety == "SB" )
check_double_up_sb
#bm_02_(spur09)_pn is only block name on the harvest data, and GPS point only have bm_02_pn – make the assumption that they are the same?
check_double_up_sb <- mutate(check_double_up_sb,
                                           Block =  case_when(
                                             Block == "BM 02 (spur 09)" ~ "BM 02",
                                             TRUE ~ Block))
## A few needs a name change...SC01 to SC 01 and SC05 to SC 05 and SC06 to SC 06 OR 04a&b, OR03

check_double_up_sb <- mutate(check_double_up_sb,
                                           Block =  case_when(
                                             Block == "SC01" ~ "SC 01",
                                             Block == "SC05" ~ "SC 05",
                                             Block == "SC06" ~ "SC 06",
                                             Block == "OR 04a&b" ~ "OR 04",
                                             Block == "OR03" ~ "OR 03",
                                             Block == "OR04" ~ "OR 04",
                                             Block == "OR 01a&b" ~ "OR 01",
                                             TRUE ~ Block))




#Create an ID clm
str(check_double_up_sb)
check_double_up_sb_step1 <- check_double_up_sb %>% 
  mutate(vineyard_lower =str_to_lower(Vineyard, locale = "en"),
         block_lower =str_to_lower(Block, locale = "en"),
         Variety_lower =str_to_lower(Variety, locale = "en"),
         year = gsub( "V|v", "20", check_double_up_sb$Vintage))
glimpse(check_double_up_sb_step1)



check_double_up_sb_step2 <- check_double_up_sb_step1 %>% 
  mutate(ID_temp1 = gsub( " ", "_", check_double_up_sb_step1$block_lower),
         ID_temp = paste0(ID_temp1,"_", Variety_lower),
         ID_yr = paste0(ID_temp,"_", year))  
glimpse(check_double_up_sb_step2)


check_double_up_sb_step2 <- dplyr::select(check_double_up_sb_step2,
                                          Vineyard,
                                          Region,
                                          Block,
                                          ha = `Area \r\n(ha)`,
                                          ID_yr,
                                          ID_temp)
check_double_up_sb_step2

#what are the replicates?
what_reps <- check_double_up_sb_step2 %>% group_by(ID_yr) %>% 
  filter(n() > 1)

what_reps <-mutate(what_reps,
       ID_yr_ha = paste0(ID_yr, "_", ha)) 
print(what_reps)

#Yes there are a few lets just use the one that has tne biggest ha...
biggest_rep <- what_reps %>% 
  group_by(ID_yr) %>%
  summarise(value = max(ha, na.rm = FALSE))
biggest_rep <- mutate(biggest_rep,
       ID_yr_ha = paste0(ID_yr, "_", value))  

print(biggest_rep)


#use this to make a list of sites I dont want to use in analysis
what_to_chuck <- what_reps %>%
filter(!ID_yr_ha %in% biggest_rep$ID_yr_ha)
