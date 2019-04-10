library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)

#Villia_maria_GPS1 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Villa Maria Data For Mike Trought.xlsx", 
#                                 sheet = "2017-18 Includes GPS")


Villia_maria_GPS <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Data For Mike Trought historical_Oct 2018.xlsx", 
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
                           sub_region = `Sub-Region`)
                           

#These section are missing GPS locations drop them from my lookup table
#MENSSB10 (fixed up with newer version)
#MJAPSB09 (fixed up with newer version)
#MKELSB01 (fixed up with newer version)
#MLANSB03 (fixed up with newer version)
#MWTFSB04
#MMASPN04 (this is an error with long and lat having the same value)

Villia_maria_GPS <- filter(Villia_maria_GPS,Section != "MWTFSB04",
                           Villia_maria_GPS,Section != "MMASPN04")
                                           


#add all the years tabs togther
#2018
Villia_maria_2017_18 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Data For Mike Trought historical_Oct 2018.xlsx", 
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
#2017
Villia_maria_2016_17 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Data For Mike Trought historical_Oct 2018.xlsx", 
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
#2016
Villia_maria_2015_16 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Data For Mike Trought historical_Oct 2018.xlsx", 
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
#2015
Villia_maria_2014_15 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Data For Mike Trought historical_Oct 2018.xlsx", 
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
#2014
Villia_maria_2013_14 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Data For Mike Trought historical_Oct 2018.xlsx", 
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
#2013
Villia_maria_2012_13 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Data For Mike Trought historical_Oct 2018.xlsx", 
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
#2012
                            
Villia_maria_2011_12 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Data For Mike Trought historical_Oct 2018.xlsx", 
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

#Bring them all togther
glimpse(Villia_maria_2017_18)
glimpse(Villia_maria_2016_17)
glimpse(Villia_maria_2015_16)
glimpse(Villia_maria_2014_15)
glimpse(Villia_maria_2013_14)
glimpse(Villia_maria_2012_13)
glimpse(Villia_maria_2011_12)


Villia_maria_2017_2012 <- rbind(Villia_maria_2017_18,
                                Villia_maria_2016_17,
                                Villia_maria_2015_16,
                                Villia_maria_2014_15,
                                Villia_maria_2013_14,
                                Villia_maria_2012_13,
                                Villia_maria_2011_12)
#assign GPS coords to the section
glimpse(Villia_maria_2017_2012)
glimpse(Villia_maria_GPS)
Villia_maria_2017_2012 <- left_join(Villia_maria_GPS, Villia_maria_2017_2012 )


villia_maria_2017_2012_notjoined <- anti_join(Villia_maria_2017_2012, Villia_maria_GPS)
glimpse(Villia_maria_2017_2012)

Villia_maria_2017_2012_sub <- select(Villia_maria_2017_2012,
                                     lat,long, year, Section, variety, ID)
                                     

write_csv(Villia_maria_2017_2012_sub, "Villia_maria_2017_2012_post_R2.csv")

no_coords_count <- group_by(villia_maria_2017_2012_notjoined,
                      variety) %>% 
                      count(year)
ggplot(no_coords_count, aes(year, n))+
  geom_point()+
  facet_wrap(.~ variety)+
  labs(title = "number of sections without GPS points",
       #subtitle = "xxx",
       x = "years",
       y = "number of sections")+
  theme(axis.text.x=element_text(angle=90, hjust=1))


coords_count <- group_by(Villia_maria_2017_2012,
                            variety) %>% 
  count(year)
ggplot(coords_count, aes(year, n))+
  geom_point()+
  facet_wrap(.~ variety)+
  labs(title = "number of sections with GPS points",
       #subtitle = "xxx",
       x = "years",
       y = "number of sections")+
theme(axis.text.x=element_text(angle=90, hjust=1))





####cal a few extra columns
glimpse(Villia_maria_2017_2012)
Villia_maria_2017_2012 <- mutate(Villia_maria_2017_2012,
                       julian = as.numeric(format(Villia_maria_2017_2012$harvest_date, "%j")),
                       meter_row_per_ha = 10000/row_width,
                       yld_per_m_row_kg =  (yield_t_ha *1000) / meter_row_per_ha,
                       bunch_m = (yld_per_m_row_kg * 1000)/ bunch_wt_g,
                       company = "Villa Maria")
                       
glimpse(Villia_maria_2017_2012)
write_csv(Villia_maria_2017_2012, "Villia_maria_2017_2012_post_R.csv")
