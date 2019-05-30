library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)


#####################################################################################################################
##############################           GPS POINTS for blocks #########################################################
#########################################################################################################################

wither_hills_GPS_temp <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wither Hills yield data For Mike Trought RGVB.xlsx",
                                    sheet = "Wither Hills NZ2000")



glimpse(wither_hills_GPS_temp)
wither_hills_GPS <- wither_hills_GPS_temp %>% 
                    select(ID_temp = Name ,
                    x_coord = POINT_X,
                    y_coord = POINT_Y) %>% 
                    mutate(ID_temp = gsub( "-", "_", ID_temp))
glimpse(wither_hills_GPS)
#####################################################################################################################
##############################           Add in the block info  #########################################################
#########################################################################################################################


wither_hills_block_info <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                                    sheet = "locations" , skip = 3)
glimpse(wither_hills_block_info)
wither_hills_block_info <- wither_hills_block_info %>% 
                            select(
                            ID_temp = X__1, 
                            variety,
                            row_spacing = `row spacing (m)`) %>% 
                            mutate(ID_temp = gsub( "-", "_", ID_temp))
glimpse(wither_hills_block_info)
###Join GPS and block data

glimpse(wither_hills_GPS) #85
glimpse(wither_hills_block_info) #83

wither_hills_GPS_block_info <- full_join(wither_hills_GPS,wither_hills_block_info, by = "ID_temp")

glimpse(wither_hills_GPS_block_info) #88
#drop the winery data entery
wither_hills_GPS_block_info <- filter(wither_hills_GPS_block_info, ID_temp != "winery")

glimpse(wither_hills_GPS_block_info) #88

#####################################################################################################################
##############################           Add in the yield measures  #########################################################
#########################################################################################################################

wither_hills_harvest_details <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Wither Hills yield data For Mike Trought RGVB.xlsx", 
                                    sheet = "15,16,17,18 ", skip = 3)
glimpse(wither_hills_harvest_details)
#Create an ID clm
wither_hills_harvest_details_step1 <- wither_hills_harvest_details %>% 
         mutate(vineyard_lower =str_to_lower(Vineyard, locale = "en"),
         block_lower =str_to_lower(Block, locale = "en"),
         Variety_lower =str_to_lower(Variety, locale = "en"),
         year = gsub( "V|v", "20", wither_hills_harvest_details$Vintage))
glimpse(wither_hills_harvest_details_step1)

wither_hills_harvest_details_step2 <- wither_hills_harvest_details_step1 %>% 
         mutate(ID_temp1 = gsub( " ", "_", wither_hills_harvest_details_step1$block_lower),
         ID_temp = paste0(ID_temp1,"_", Variety_lower),
         ID_yr = paste0(ID_temp,"_", year))  
glimpse(wither_hills_harvest_details_step2)



####Note that there is something wrong with the imput date clm we have lots of wrong dates
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
glimpse(wither_hills_GPS_block_info) #87
glimpse(wither_hills_harvest_details) #583
wither_hills_GPS_block_info_harvest <- full_join(wither_hills_GPS_block_info, wither_hills_harvest_details,
                                                 by= "ID_temp") %>% 
  mutate(company = "Wither_Hills")
glimpse(wither_hills_GPS_block_info_harvest)

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
library(lubridate)
glimpse(wither_hills_GPS_block_info_harvest$year)

wither_hills_GPS_block_info_harvest$year <- as.double(wither_hills_GPS_block_info_harvest$year)


wither_hills_GPS_block_info_harvest$na_count <- apply(is.na(wither_hills_GPS_block_info_harvest), 1, sum)

glimpse(wither_hills_GPS_block_info_harvest)

######################################################################################################################
################                         view and summaries DF                             #################
######################################################################################################################


dim(wither_hills_GPS_block_info_harvest)
#how many site?
dim(wither_hills_GPS_block_info_harvest)
glimpse(wither_hills_GPS_block_info_harvest$year) #610 records

max(wither_hills_GPS_block_info_harvest$year) #2008 -2018
min(wither_hills_GPS_block_info_harvest$year) #2008 -2018


#how many sites with GPS pts
glimpse(wither_hills_GPS_block_info_harvest)#610 records
#how many sites with GPS pts by Variety
filter(wither_hills_GPS_block_info_harvest, x_coord > 0) %>% 
ggplot( aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites with GPS coordinates")
GPS_only <- filter(wither_hills_GPS_block_info_harvest, x_coord> 0)
glimpse(GPS_only)

#how many sites by Variety by year
ggplot(pernod_ricard1, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#how many sites by Variety
ggplot(pernod_ricard1, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")



#create a new variable year_as_factor
pernod_ricard1$year_factor <- as.factor(pernod_ricard1$year)

#filter data for Sauvignon Blanc
pernod_ricard1_sau <- filter(pernod_ricard1, variety == "Sauvignon Blanc") 
glimpse(pernod_ricard1_sau)

#how many sites for Sauvignon Blanc by year
group_by(pernod_ricard1_sau, year) %>% 
  count()
#how many sites for Sauvignon Blanc have missing data - how much missing data?
ggplot(pernod_ricard1_sau, aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")
#how many sites for Sauvignon Blanc have missing data - missing data grouped together?
ggplot(pernod_ricard1_sau, aes(na_count))+
  geom_bar()+
  scale_x_continuous(breaks =  c(2,4,6,8,10))+
  facet_wrap(~year_factor)+
  theme_bw()+
  labs(x = "number of na counts per entry",
       y= "Counts of missing data entries NA")


glimpse(pernod_ricard1_sau)
#julian days
ggplot(pernod_ricard1_sau, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
#yield_t_ha
ggplot(pernod_ricard1_sau, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")
#yield_kg_m
ggplot(pernod_ricard1_sau, aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")

#yield_kg_m filter out zeros
filter(pernod_ricard1_sau,yield_kg_m != 0) %>% 
  ggplot( aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")


#brix - too many zero
ggplot(pernod_ricard1_sau, aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


#brix - filter out zero
filter(pernod_ricard1_sau,brix != 0) %>% 
  ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


############################################################################## 
########################    File to use   ####################################
pernod_ricard1_sau <- select(pernod_ricard1_sau, -year_factor)
glimpse(pernod_ricard1_sau)
write_csv(pernod_ricard1_sau, "V:/Marlborough regional/working_jaxs/pernod_ricard1_sau.csv")
##############################################################################   



