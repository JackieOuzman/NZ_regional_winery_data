
library(dplyr)
library(ggplot2)
library(tidyverse)
#install.packages("cowplot")
library(cowplot)

library(readxl)
library(naniar)


### move the files over to a folder and then mess about with them there

getwd()

# identify the folders

path_melb <- "V:/Viticulture/"

df <-
  read_csv(paste0(path_melb, "Marlborough regional/working_jaxs/for_mapping_july2021/All_sites_2014_2019_july2021.csv"))

names(df)
Julian_days <- df %>% 
  dplyr::select(ID_yr:julian, y_coord:company )

names(Julian_days)


# remove the rows with missing x and y AND julain days 
Julian_days <- Julian_days %>% 
  filter(!is.na(y_coord))

Julian_days <- Julian_days %>% 
  filter(!is.na(julian))


#### split the ID_yr clm into multiple
Julian_days <- Julian_days %>% 
  mutate(site = str_sub(ID_yr,1,-6))


## we seem to have have few duplicates - I dont know why?
Julian_days <- Julian_days %>% 
  distinct(ID_yr, .keep_all = TRUE)

Julian_days <- Julian_days %>% 
  dplyr::select(-ID_yr)
  

# make it wide so I have a clm for each year.
names(Julian_days)
Julian_days_wide <- Julian_days %>% 
  #filter(site == "alfa lea_jc_sb") %>% 
  select(- harvest_date) %>% 
  pivot_wider(names_from = year,
              values_from = julian)


# #replace all the NA values with -9999
# names(Julian_days_wide)
# Julian_days_wide$`2014`      <- Julian_days_wide$`2014`  %>% replace_na(-9999)
# Julian_days_wide$`2015`      <- Julian_days_wide$`2015`  %>% replace_na(-9999)
# Julian_days_wide$`2016`      <- Julian_days_wide$`2016`  %>% replace_na(-9999)
# Julian_days_wide$`2017`      <- Julian_days_wide$`2017`  %>% replace_na(-9999)
# Julian_days_wide$`2018`      <- Julian_days_wide$`2018`  %>% replace_na(-9999)
# Julian_days_wide$`2019`      <- Julian_days_wide$`2019`  %>% replace_na(-9999)

names(Julian_days_wide)
Julian_days_wide <- Julian_days_wide %>% 
  select(x_coord,
         y_coord,
         `2014`:`2019`,
         site,
         company,
         variety)
         
Julian_days_wide <- Julian_days_wide %>% 
  rename(Julian_2014 = `2014`,
         Julian_2015 = `2015`,
         Julian_2016 = `2016`,
         Julian_2017 = `2017`,
         Julian_2018 = `2018`,
         Julian_2019 = `2019`
         )

path_melb <- "V:/Viticulture/"
write.csv(Julian_days_wide,
          paste0(path_melb, "Marlborough regional/working_jaxs/for_mapping_may2022/vesper_Julian_days/julian_days.csv"),
                 row.names = FALSE)   

