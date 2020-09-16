library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)
library(sp)
library(biogeo)
library(stringr)
library(rgdal)
library(sf)


######################################################################################################################
################                         Make DF with GPS coodinates                                 #################
######################################################################################################################

Villia_maria_GPS <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa_Maria/Villia_Maria_updated_Data For Rob Bramley June 2020.xlsx", 
                                                        sheet = "2017-18 Includes GPS",
                                                        col_types = c("text", "text", "text", 
                                                                      "text", "text", "text", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "text", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "text", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric"))


glimpse(Villia_maria_GPS)
Villia_maria_GPS <- select(Villia_maria_GPS,
                           Section,
                           lat = GPS1,
                           long = GPS2, 
                           row_width =  `Row width`  ,
                           vine_spacing =`Vine spacing`,
                           sub_region = `Sub-Region`,
                           variety)
                           


#These section are missing GPS locations drop them from my lookup table
#MENSSB10 (fixed up with newer version)
#MJAPSB09 (fixed up with newer version)
#MKELSB01 (fixed up with newer version)
#MLANSB03 (fixed up with newer version)
#MWTFSB04
#MMASPN04 (this is an error with long and lat having the same value)

Villia_maria_GPS <- filter(Villia_maria_GPS,Section !="MWTFSB04") %>% 
filter(Section !="MMASPN04") 
                             



#write_csv(Villia_maria_GPS, "Villia_maria_GPS_test.csv")
######################################################################################################################
################                         change the projection of the data                              #################
######################################################################################################################
#Now I have my data in decimal degrees I want to convert it into GDA

#Villia_maria_GPS_1$Long_DD <- dms2dd(white_haven_GPS_1$Long_dd,
#                                    white_haven_GPS_1$Long_mm,
#                                    white_haven_GPS_1$Long_ss, 
#                                    white_haven_GPS_1$Long_L)

mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(Villia_maria_GPS) # seems to be missing a few values
#Villia_maria_GPS_1 <-drop_na(Villia_maria_GPS)

no_GPS_Villia_maria<- Villia_maria_GPS %>% 
  filter(is.na(lat))
no_GPS_Villia_maria <- no_GPS_Villia_maria %>% 
  filter(variety == "SAUV")


Villia_maria_GPS_1<- Villia_maria_GPS %>% 
  filter(!is.na(lat))

coordinates(Villia_maria_GPS_1) <- ~long + lat

proj4string(Villia_maria_GPS_1) <- wgs84CRS   # assume input lat and longs are WGS84
Villia_maria_GPS_1 <- spTransform(Villia_maria_GPS_1, mapCRS)

glimpse(Villia_maria_GPS_1)

Villia_maria_GPS = as.data.frame(Villia_maria_GPS_1) #this has the new coordinates projected !YES!!
glimpse(Villia_maria_GPS)

rm("mapCRS", "wgs84CRS", "Villia_maria_GPS_1")  
  
                                           
######################################################################################################################
################                         Make DF of yld measure by year                              #################
######################################################################################################################



#####################                               2018                             ##################################
Villia_maria_2017_18 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa_Maria/Villia_Maria_updated_Data For Rob Bramley June 2020.xlsx", 
                               sheet = "2017-18 Includes GPS")
glimpse(Villia_maria_2017_18)
Villia_maria_2017_18 <- select(Villia_maria_2017_18,
                           Section,
                           harvest_date =`Harvest date` ,
                           variety,
                           trellis,
                           yield_t_ha =`Actual T/Ha`,
                           berries_bunch = `Sign Off 2 Agreed Berries per Bunch`,
                           berry_wt_g =  `Pre Harvest Berry weight ( g)`,
                           brix = Brix,
                           bunch_wt_g = `Pre Harvest bunch weight ( g)`) %>% 
  mutate(year = 2018,
         ID = paste0(Section, "_", year))

#####################                               2017                             ##################################

Villia_maria_2016_17 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa_Maria/Villia_Maria_updated_Data For Rob Bramley June 2020.xlsx", 
                               sheet = "2016-17")
glimpse(Villia_maria_2016_17)
Villia_maria_2016_17 <- select(Villia_maria_2016_17,
                               Section,
                               harvest_date =`Harvest date` ,
                               variety,
                               trellis,
                               yield_t_ha =`Actual T/Ha`,
                               berries_bunch = `Sign Off 2 Agreed Berries per Bunch`,
                               berry_wt_g =  `Pre Harvest Berry weight ( g)`,
                               brix = Brix,
                               bunch_wt_g = `VME Pre harvest bunch weight(g)( Sampling Sheets)`) %>% 
  mutate(year = 2017,
         ID = paste0(Section, "_", year))

#####################                               2016                             ##################################

Villia_maria_2015_16 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa_Maria/Villia_Maria_updated_Data For Rob Bramley June 2020.xlsx", 
                                   sheet = "2015-16")
glimpse(Villia_maria_2015_16)
Villia_maria_2015_16 <- select(Villia_maria_2015_16,
                               Section,
                               harvest_date =`Harvest date` ,
                               variety,
                               trellis,
                               yield_t_ha =`Actual T/Ha`,
                               berries_bunch = `Sign Off 2 Agreed Berries per Bunch`,
                               berry_wt_g =  `Pre Harvest Berry weight ( g)`,
                               brix = Brix,
                               bunch_wt_g = `VME Pre harvest bunch weight(g)( Sampling Sheets)`) %>% 
  mutate(year = 2016,
         ID = paste0(Section, "_", year))

#####################                               2015                             ##################################

Villia_maria_2014_15 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa_Maria/Villia_Maria_updated_Data For Rob Bramley June 2020.xlsx", 
                                   sheet = "2014-15")
glimpse(Villia_maria_2014_15)
Villia_maria_2014_15 <- select(Villia_maria_2014_15,
                               Section,
                               harvest_date =`Harvest date` ,
                               variety,
                               trellis,
                               yield_t_ha =`Actual T/Ha`,
                               berries_bunch = `Sign Off 2 Agreed Berries per Bunch`,
                               berry_wt_g =  `Pre Harvest Berry weight ( g)`,
                               brix = Brix,
                               bunch_wt_g = `VME Pre harvest bunch weight(g)( Sampling Sheets)`) %>% 
  mutate(year = 2015,
         ID = paste0(Section, "_", year))

#####################                               2014                             ##################################

Villia_maria_2013_14 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa_Maria/Villia_Maria_updated_Data For Rob Bramley June 2020.xlsx", 
                                   sheet = "2013-14")
glimpse(Villia_maria_2013_14)
Villia_maria_2013_14 <- select(Villia_maria_2013_14,
                               Section,
                               harvest_date =`Harvest date` ,
                               variety,
                               trellis,
                               yield_t_ha =`Actual T/Ha`,
                               berries_bunch = `Sign Off 2 Agreed Berries per Bunch`,
                               berry_wt_g =  `Pre Harvest Berry weight ( g)`,
                               brix = Brix,
                               bunch_wt_g = `VME Pre harvest bunch weight(g)( Sampling Sheets)`)  %>% 
  mutate(year = 2014,
         ID = paste0(Section, "_", year))

#####################                               2013                             ##################################

Villia_maria_2012_13 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa_Maria/Villia_Maria_updated_Data For Rob Bramley June 2020.xlsx", 
                                   sheet = "2012-13")
glimpse(Villia_maria_2012_13)
Villia_maria_2012_13 <- select(Villia_maria_2012_13,
                               Section,
                               harvest_date =`Harvest date` ,
                               variety,
                               trellis,
                               yield_t_ha =`Actual T/Ha`,
                               berries_bunch = `Sign Off 2 Agreed Berries per Bunch`,
                               berry_wt_g =  `Pre Harvest Berry weight ( g)`,
                               brix = Brix,
                               bunch_wt_g = `VME Pre harvest bunch weight(g)( Sampling Sheets)`) %>% 
  mutate(year = 2013,
         ID = paste0(Section, "_", year))


#####################                               2012                             ##################################
                            
Villia_maria_2011_12 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa_Maria/Villia_Maria_updated_Data For Rob Bramley June 2020.xlsx", 
                                                        sheet = "2011-12", col_types = c("text", 
                                                                                         "text", "text", "text", "text", "numeric", 
                                                                                         "text", "numeric", "numeric", "numeric", 
                                                                                         "numeric", "numeric", "numeric", 
                                                                                         "numeric", "numeric", "numeric", 
                                                                                         "numeric", "numeric", "numeric", 
                                                                                         "text", "numeric", "numeric", "numeric", 
                                                                                         "numeric", "numeric", "date"))

glimpse(Villia_maria_2011_12)
Villia_maria_2011_12 <- select(Villia_maria_2011_12,
                               Section,
                               harvest_date =`Harvest date` ,
                               variety,
                               trellis,
                               yield_t_ha =`Actual T/Ha`,
                               berries_bunch = `Sign Off 2 Agreed Berries per Bunch`,
                               berry_wt_g =  `Pre Harvest Berry weight ( g)`,
                               brix = Brix,
                               bunch_wt_g = `VME Pre harvest bunch weight(g)( Sampling Sheets)`) %>% 
  mutate(year = 2012,
         ID = paste0(Section, "_", year))

#####################                               2019                             ##################################



Villia_maria_2018_19 <-read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa_Maria/Villia_Maria_updated_Data For Rob Bramley June 2020.xlsx", 
           sheet = "2018-19", col_types = c("text", 
                                            "text", "text", "text", "text", "text", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "text", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "date", "text"))

glimpse(Villia_maria_2018_19)
Villia_maria_2018_19 <- dplyr::select(Villia_maria_2018_19,
                               Section = Block,
                               harvest_date =`Harvest date` ,
                               variety = Variety,
                               trellis = Trellis,
                               yield_t_ha =`Harvest Yield (T/ha)`,
                               berries_bunch = `Sign Off 2 Agreed Berries per Bunch`,
                               berry_wt_g =  `Pre Harvest Berry weight (g)`,
                               brix = Brix,
                               bunch_wt_g = `Pre Harvest bunch weight (g)`) %>% 
  mutate(year = 2019,
         ID = paste0(Section, "_", year))


#Bring them all togther
glimpse(Villia_maria_2018_19)
glimpse(Villia_maria_2017_18)
glimpse(Villia_maria_2016_17)
glimpse(Villia_maria_2015_16)
glimpse(Villia_maria_2014_15)
glimpse(Villia_maria_2013_14)
glimpse(Villia_maria_2012_13)
glimpse(Villia_maria_2011_12)




#####################                              Join all the yield data togther 2018-2012             ##################################
Villia_maria_2018_2012 <- rbind(Villia_maria_2018_19,
                                Villia_maria_2017_18,
                                Villia_maria_2016_17,
                                Villia_maria_2015_16,
                                Villia_maria_2014_15,
                                Villia_maria_2013_14,
                                Villia_maria_2012_13,
                                Villia_maria_2011_12)


#####################                              Join  yield data to GPS data             ##################################
rm( "Villia_maria_2018_19",
    "Villia_maria_2017_18",
    "Villia_maria_2016_17",
    "Villia_maria_2015_16",
    "Villia_maria_2014_15",
    "Villia_maria_2013_14",
    "Villia_maria_2012_13",
    "Villia_maria_2011_12")

glimpse(Villia_maria_2018_2012) #yld data 2502
glimpse(Villia_maria_GPS) #GPS data 322
#recode the variety so its all the same "Sauvignon Blanc" and "SAUV"

#Villia_maria_2018_2012 <- Villia_maria_2018_2012 %>% 
Villia_maria_2018_2012 <- Villia_maria_2018_2012 %>% 
  mutate(variety = case_when(
    variety == "SAUV" ~ "Sauvignon Blanc",
    TRUE ~ variety
  ))

Villia_maria_GPS <- Villia_maria_GPS %>% 
  mutate(variety = case_when(
    variety == "SAUV" ~ "Sauvignon Blanc",
    TRUE ~ variety
  ))

unique(Villia_maria_2018_2012$variety)
unique(Villia_maria_GPS$variety)


Villia_maria_2018_2012_all <- full_join(Villia_maria_GPS, Villia_maria_2018_2012 )
#rm("Villia_maria_2018_2012")
glimpse(Villia_maria_2018_2012_all)
#stuff around with names and extra clms
Villia_maria_2018_2012_all <- select(Villia_maria_2018_2012_all,
                                     lat,long, year, ID = Section, variety, ID_yr =ID,
                                     row_width,
                                     vine_spacing,
                                     harvest_date,
                                     variety,
                                     trellis,
                                     yield_t_ha,
                                     berries_bunch,
                                     berry_wt_g,
                                     brix,
                                     bunch_wt_g
                                     )
                                     
glimpse(Villia_maria_2018_2012_all)
#write_csv(Villia_maria_2017_2012_sub, "Villia_maria_2017_2012_post_R2.csv")

####cal a few extra columns



Villia_maria_2018_2012_all <- mutate(Villia_maria_2018_2012_all,
                       julian = as.numeric(format(Villia_maria_2018_2012_all$harvest_date, "%j")),
                       meter_row_per_ha = 10000/row_width,
                       yield_kg_m =  (yield_t_ha *1000) / meter_row_per_ha,
                       #bunch_m = (yld_per_m_row_kg * 1000)/ bunch_wt_g,
                       company = "Villa Maria",
                       y_coord = lat,
                       x_coord = long,
                       #yield_kg_m = NA,
                       bunch_numb_m	 = NA
                       )
glimpse(Villia_maria_2018_2012_all)
names(Villia_maria_2018_2012_all)
##tidy up to match the other files
Villia_maria_2018_2012_all <- 
  select(Villia_maria_2018_2012_all, company, ID_yr, variety , x_coord, y_coord,
         year , harvest_date, julian,
         yield_t_ha, 
         yield_kg_m,
         brix,bunch_weight = bunch_wt_g, berry_weight = berry_wt_g,
         pruning_style = trellis,
         row_width,
         vine_spacing,
         bunch_numb_m
         )

glimpse(Villia_maria_2018_2012_all)

Villia_maria_2018_2012_all$na_count <- apply(is.na(Villia_maria_2018_2012_all), 1, sum)
unique(Villia_maria_2018_2012_all$variety)

Villia_maria_2018_2012_all_sav <-
  filter(Villia_maria_2018_2012_all,
         variety == "Sauvignon Blanc" |
           variety ==  "SAUV")


#rm("Villia_maria_2018_2012_all", "no_GPS_Villia_maria")

#heaps pof block with no GPS coordinates - why is this? can we do better?
no_GPS_Villia_maria_sav<- Villia_maria_2018_2012_all_sav %>% 
  filter(is.na(x_coord))


no_GPS_Villia_maria_sav %>% 
  group_by( year) %>% 
  summarise(number = n())

#wow heaps of the 2019 sites dont match - why?? lets see whats missing for the years
no_GPS_2019_sav <- no_GPS_Villia_maria_sav %>% 
  filter(year == 2019)
no_GPS_2019_sav <-as.data.frame( unique(no_GPS_2019_sav$ID))
names(no_GPS_2019_sav)<- "no_coord"
no_GPS_2019_sav <- no_GPS_2019_sav %>% 
  mutate(year = 2019)

no_GPS_2017_sav <- no_GPS_Villia_maria_sav %>% 
  filter(year == 2017) 
no_GPS_2017_sav <-as.data.frame( unique(no_GPS_2017_sav$ID))
names(no_GPS_2017_sav)<- "no_coord"
no_GPS_2017_sav <- no_GPS_2017_sav %>% 
  mutate(year = 2017)

no_GPS_2016_sav <- no_GPS_Villia_maria_sav %>% 
  filter(year == 2016) 
no_GPS_2016_sav <-as.data.frame( unique(no_GPS_2016_sav$ID))
names(no_GPS_2016_sav)<- "no_coord"
no_GPS_2016_sav <- no_GPS_2016_sav %>% 
  mutate(year = 2016)

no_GPS_2015_sav <- no_GPS_Villia_maria_sav %>% 
  filter(year == 2015) 
no_GPS_2015_sav <-as.data.frame( unique(no_GPS_2015_sav$ID))
names(no_GPS_2015_sav)<- "no_coord"
no_GPS_2015_sav <- no_GPS_2015_sav %>% 
  mutate(year = 2015)

no_GPS_2014_sav <- no_GPS_Villia_maria_sav %>% 
  filter(year == 2014) 
no_GPS_2014_sav <-as.data.frame( unique(no_GPS_2014_sav$ID))
names(no_GPS_2014_sav)<- "no_coord"
no_GPS_2014_sav <- no_GPS_2014_sav %>% 
  mutate(year = 2014)

no_GPS_2013_sav <- no_GPS_Villia_maria_sav %>% 
  filter(year == 2013) 
no_GPS_2013_sav <-as.data.frame( unique(no_GPS_2013_sav$ID))
names(no_GPS_2013_sav)<- "no_coord"
no_GPS_2013_sav <- no_GPS_2013_sav %>% 
  mutate(year = 2013)

no_GPS_2012_sav <- no_GPS_Villia_maria_sav %>% 
  filter(year == 2012) 
no_GPS_2012_sav <-as.data.frame( unique(no_GPS_2012_sav$ID))
names(no_GPS_2012_sav)<- "no_coord"
no_GPS_2012_sav <- no_GPS_2012_sav %>% 
  mutate(year = 2012)

no_GPS_2012_2019sav <- rbind(
  no_GPS_2012_sav,
  no_GPS_2013_sav,
  no_GPS_2014_sav,
  no_GPS_2015_sav,
  no_GPS_2016_sav,
  no_GPS_2017_sav,
  no_GPS_2019_sav
)
getwd()
write.csv(no_GPS_2012_2019sav, "Villa_Maria_no_GPS_2012_2019sav.csv")

## some data needs to be removed or recoded eg 0 to NA
names(Villia_maria_2018_2012_all_sav)

Villia_maria_2018_2012_all_sav <- Villia_maria_2018_2012_all_sav %>% 
  mutate(yield_t_ha = case_when(
    yield_t_ha == 0.0000 ~ NA_real_,
    TRUE ~ yield_t_ha))
Villia_maria_2018_2012_all_sav <- Villia_maria_2018_2012_all_sav %>% 
  mutate(yield_kg_m = case_when(
    yield_kg_m == 0.0000 ~ NA_real_,
    TRUE ~ yield_kg_m))
Villia_maria_2018_2012_all_sav <- Villia_maria_2018_2012_all_sav %>% 
  mutate(bunch_weight = case_when(
    bunch_weight == 0.0000 ~ NA_real_,
    TRUE ~ bunch_weight))
Villia_maria_2018_2012_all_sav <- Villia_maria_2018_2012_all_sav %>% 
  mutate(bunch_weight = case_when(
    bunch_weight == 12439 ~ NA_real_,
    TRUE ~ bunch_weight))



Villia_maria_2018_2012_all_sav <- Villia_maria_2018_2012_all_sav %>% 
  mutate(brix = case_when(
    brix == 1945.00 ~ NA_real_,
    brix == 198.00 ~ NA_real_,
    TRUE ~ brix))

str(Villia_maria_2018_2012_all_sav)
Villia_maria_2018_2012_all_sav <- Villia_maria_2018_2012_all_sav %>% 
  mutate(berry_weight = case_when(
    berry_weight == 	"DNC" ~ "NA",
    TRUE ~ berry_weight))

Villia_maria_2018_2012_all_sav$berry_weight <- as.double(Villia_maria_2018_2012_all_sav$berry_weight)
Villia_maria_2018_2012_all_sav <- Villia_maria_2018_2012_all_sav %>% 
  mutate(berry_weight = case_when(
    berry_weight == 0.0000 ~ NA_real_,
    TRUE ~ berry_weight))

names(Villia_maria_2018_2012_all_sav)
write_csv(Villia_maria_2018_2012_all_sav, "V:/Marlborough regional/working_jaxs/July2020/Villia_maria_2017_2012_all_sau.csv")
                       
######################################################################################################################
################                         view and summaries DF 2019 -2014                            #################
######################################################################################################################


dim(Villia_maria_2017_2012_all)
#how many site?
dim(Villia_maria_2017_2012_all)
glimpse(Villia_maria_2017_2012_all) #1754 records
max(Villia_maria_2017_2012_all$year) #2012 -2018
min(Villia_maria_2017_2012_all$year)


#how many sites with GPS pts
glimpse(Villia_maria_GPS  )#312 records
#how many sites with GPS pts by Variety
ggplot(Villia_maria_GPS, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites with GPS coordinates")

#how many sites by Variety by year
ggplot(Villia_maria_2017_2012_all, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#how many sites by Variety
ggplot(Villia_maria_2017_2012_all, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")



#create a new variable year_as_factor
Villia_maria_2017_2012_all$year_factor <- as.factor(Villia_maria_2017_2012_all$year)

#filter data for Sauvignon Blanc
Villia_maria_2017_2012_all_sau <- filter(Villia_maria_2017_2012_all, variety == "SAUV") 
glimpse(Villia_maria_2017_2012_all_sau)

#how many sites for Sauvignon Blanc by year
group_by(Villia_maria_2017_2012_all_sau, year) %>% 
  count()
#how many sites for Sauvignon Blanc have missing data - how much missing data?
ggplot(Villia_maria_2017_2012_all_sau, aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")
#how many sites for Sauvignon Blanc have missing data - missing data grouped together?
ggplot(Villia_maria_2017_2012_all_sau, aes(na_count))+
  geom_bar()+
  scale_x_continuous(breaks =  c(2,4,6,8,10))+
  facet_wrap(~year_factor)+
  theme_bw()+
  labs(x = "number of na counts per entry",
       y= "Counts of missing data entries NA")


glimpse(Villia_maria_2017_2012_all_sau)
#julian days
ggplot(Villia_maria_2017_2012_all_sau, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
#yield_t_ha
ggplot(Villia_maria_2017_2012_all_sau, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")
#yield_kg_m
ggplot(Villia_maria_2017_2012_all_sau, aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")

#yield_kg_m filter out zeros
filter(Villia_maria_2017_2012_all_sau,yield_kg_m != 0) %>% 
  ggplot( aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")


#brix - too many zero
ggplot(Villia_maria_2017_2012_all_sau, aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


#brix - filter out high values
filter(Villia_maria_2017_2012_all_sau,brix <40) %>% 
  ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")



############################################################################## 
########################    File to use   ####################################
Villia_maria_2017_2012_all_sau <- select(Villia_maria_2017_2012_all_sau, -year_factor)
glimpse(Villia_maria_2017_2012_all_sau)
write_csv(Villia_maria_2017_2012_all_sau, "V:/Marlborough regional/working_jaxs/Villia_maria_2017_2012_all_sau.csv")
##############################################################################   

