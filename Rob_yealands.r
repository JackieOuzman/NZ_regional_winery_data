#sites added in 2020 Rob 

library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)
library(sp)
library(biogeo)
library(stringr)
library(rgdal)
library(sf)


########################## other blocks all years #######################################################

yealands_2020 <-  read_excel( "V:/Marlborough regional/Regional winery data/Raw_data/Yealands/Updated Yealands.xlsx",
                              sheet = "Other blocks all years")
str(yealands_2020)



yealands_2020 <- select(yealands_2020,
                        x_coord = Long,
                        y_coord = Lat,
                        vineyard = VINEYARD,
                        Subblock,
                        year = Year,                                       
                        harvest_date = `Harvest date`,
                        yield_t_ha = `Actual Harvested  T/ha`, #check with ROB - ok to use
                        yield_kg_m = `Yield (kg/m)`,
                        bunch_weight = `Calculated Bunch mass (g)`, #check with ROB - ok to use
                        berry_weight = `Calculated Berry wt (g)`,
                        bunch_per_vine = `Actual Bunch No`,
                        row_width = `row spacing (m)`,
                        vine_spacing = `In-row spacing (m)`)

yealands_2020 <- mutate(yealands_2020,                      
                        company = "Yealands",
                        ID_yr = paste0(vineyard, "_", Subblock, "_", year), 
                        variety = "Sauvignon Blanc",
                        julian = as.numeric(format(harvest_date, "%j")),
                        brix = NA,
                        bunch_numb_m = bunch_per_vine / vine_spacing,
                        pruning_style = NA,
                        na_count = NA)

yealands_2020_GPS <- select(yealands_2020,
                            x_coord,
                            y_coord,
                            ID_yr)
##1 fix up coodinates from long lats

mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

glimpse(yealands_2020_GPS) 
yealands_2020_GPS_1 <-drop_na(yealands_2020_GPS)
glimpse(yealands_2020_GPS_1)


coordinates(yealands_2020_GPS_1) <- ~x_coord + y_coord


#something wrong with the projection I am selecting
CRS(yealands_2020_GPS_1)

proj4string(yealands_2020_GPS_1) <- wgs84CRS   # assume input lat and longs are WGS84
glimpse(yealands_2020_GPS_1) 
yealands_2020_GPS_1 <- spTransform(yealands_2020_GPS_1, mapCRS)

glimpse(yealands_2020_GPS_1)

yealands_2020_GPS = as.data.frame(yealands_2020_GPS_1) #this has the new coordinates projected !YES!!
glimpse(yealands_2020_GPS)

#write.csv(yealands_2020_GPS, "yealands_2020_GPS.csv")

######Put the GPS pts back into the file
yealands_2020 <- select(yealands_2020,
                        -x_coord ,
                        -y_coord )
str(yealands_2020)

str(yealands_2020_GPS)
yealands_2020 <- full_join(yealands_2020, yealands_2020_GPS)

rm("mapCRS", "wgs84CRS", "yealands_2020_GPS_1", "yealands_2020_GPS")



##################################  updated seaview all years   ################################################

yealands_seaview_2020 <-  read_excel( "V:/Marlborough regional/Regional winery data/Raw_data/Yealands/Updated Yealands.xlsx",
                                      sheet = "Updated Seaview all years")
str(yealands_seaview_2020)
yealands_seaview_2020 <- select(yealands_seaview_2020,
                                x_coord = POINT_X,
                                y_coord = POINT_Y ,
                                Subblock,
                                year = Year,                                       
                                harvest_date = `Harvest Date`,
                                yield_t_ha = `Actual Harvested  T/ha`, #check with ROB - ok to use
                                yield_kg_m = `Yield (kg/m)`,
                                bunch_weight = `Calculated Bunch mass (g)`, #check with ROB - ok to use
                                berry_weight = `Calculated Berry wt (g)`,
                                bunch_per_vine = `Actual Bunch No`,
                                #row_width = `row spacing (m)`,
                                vine_spacing = `In-row spacing (m)`)




yealands_seaview_2020 <- mutate(yealands_seaview_2020,                      
                                company = "Yealands",
                                vineyard = "Seaview",
                                ID_yr = paste0(vineyard, "_", Subblock, "_", year), 
                                variety = "Sauvignon Blanc",
                                julian = as.numeric(format(harvest_date, "%j")),
                                brix = NA,
                                bunch_numb_m = bunch_per_vine / vine_spacing,
                                pruning_style = NA,
                                row_width = NA,
                                na_count = NA) #




str(yealands_seaview_2020)




yealands_seaview_2020 <- select(yealands_seaview_2020,
                                company,
                                ID_yr,
                                variety,
                                x_coord,
                                y_coord,
                                year,
                                harvest_date,
                                julian,
                                yield_t_ha,
                                yield_kg_m,
                                brix,
                                bunch_weight,
                                berry_weight,
                                bunch_numb_m,
                                pruning_style,
                                row_width,
                                vine_spacing,
                                na_count
)
yealands_2020 <- select(yealands_2020,
                        company,
                        ID_yr,
                        variety,
                        x_coord,
                        y_coord,
                        year,
                        harvest_date,
                        julian,
                        yield_t_ha,
                        yield_kg_m,
                        brix,
                        bunch_weight,
                        berry_weight,
                        bunch_numb_m,
                        pruning_style,
                        row_width,
                        vine_spacing,
                        na_count
)

names(yealands_seaview_2020) #this is from 'other blocks' tab
names(yealands_2020) # this is from 'Updated Seaview all years' tab

test <- rbind(yealands_seaview_2020,yealands_2020 )
str(test)
yealands_seaview_2020 <- rbind(yealands_seaview_2020,yealands_2020 )


yealands_seaview_2020 <- select(yealands_seaview_2020,
                                company,
                                ID_yr,
                                variety,
                                x_coord,
                                y_coord,
                                year,
                                harvest_date,
                                julian,
                                yield_t_ha,
                                yield_kg_m,
                                brix,
                                bunch_weight,
                                berry_weight,
                                bunch_numb_m,
                                pruning_style,
                                row_width,
                                vine_spacing,
                                na_count
)


#rename the 0 value to NA
yealands_seaview_2020$yield_kg_m[yealands_seaview_2020$yield_kg_m == 0] <- NA
yealands_seaview_2020$bunch_numb_m[yealands_seaview_2020$bunch_numb_m == 0] <- NA


## I think Rob has made as mistake with the yield cal _ I will do this again
# names(yealands_seaview_2020)
# test <- yealands_seaview_2020
# names(test)
# str(test$yield_t_ha)
# str(test$yield_kg_m)
# str(test$row_width)

## but first I need to work out which one Rob did
# test <- test %>% dplyr::select(ID_yr, yield_t_ha, yield_kg_m, row_width)
# test %>%filter(!is.na(row_width))

yealands_seaview_2020 <- yealands_seaview_2020 %>%
  mutate(re_cal_yield_kg_m = case_when(is.na(row_width) ~ yield_kg_m,
                                          TRUE ~ (yield_t_ha * 1000) / (10000 / row_width)))


## now remove the old cal and keep the new one
names(yealands_seaview_2020)
  
yealands_seaview_2020 <- yealands_seaview_2020 %>%  dplyr::select(-yield_kg_m)
yealands_seaview_2020 <- yealands_seaview_2020 %>%  dplyr::rename(yield_kg_m = re_cal_yield_kg_m)                  

### one site is wrong
yealands_seaview_2020 <- yealands_seaview_2020 %>% 
  mutate(
    y_coord  = case_when(
      
      ID_yr == "Seaview_J2_2018" ~ 5388746.638369999800000,
      TRUE ~ y_coord
    ))
yealands_seaview_2020 <- yealands_seaview_2020 %>% 
  mutate(
    x_coord  = case_when(
      ID_yr == "Seaview_J2_2018" ~ 1694942.457269999900000,
      TRUE ~ x_coord
    ))

write_csv(yealands_seaview_2020, "V:/Marlborough regional/working_jaxs/July2020/Updated_Yealands_jax.csv")
str(yealands_seaview_2020)


###########################################################################################################
#Revised  set 26/0/2021
names(yealands_seaview_2020)

#just need to make a block 
yealands_seaview_2020 <- yealands_seaview_2020 %>% separate(ID_yr, c("Block"), sep = "_", remove = FALSE)

#1. How many sites?
#for each year
yealands_seaview_2020 %>%
  group_by(year) %>%
  summarise(count = n_distinct(Block))
#overall for the data set from 2014-2019 how many blocks do we have?
yealands_seaview_2020 %>%
  summarise(count = n_distinct(Block))

#2. For harvest date how many sites per year?
names(yealands_seaview_2020)

yealands_seaview_2020 %>%
  group_by(year) %>%
  summarise(mean_julian_days = mean(julian, na.rm = TRUE),
            min_julian_days = min(julian, na.rm = TRUE),
            max_julian_days = max(julian, na.rm = TRUE),
            sum_na = sum(!is.na(julian)))

#3. For yield kg/m  how many sites per year

yealands_seaview_2020 %>%
  group_by(year) %>%
  summarise(mean_yield_kg_m = mean(yield_kg_m, na.rm = TRUE),
            min_yield_kg_m = min(yield_kg_m, na.rm = TRUE),
            max_yield_kg_m = max(yield_kg_m, na.rm = TRUE),
            sum_na = sum(!is.na(yield_kg_m)))








#  






