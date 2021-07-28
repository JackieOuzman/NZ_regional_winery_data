
library(dplyr)
library(ggplot2)
library(tidyverse)
#install.packages("cowplot")
library(cowplot)

library(readxl)
library(naniar)
library(plotly)

### move the files over to a folder and then mess about with them there



# identify the folders
current.folder <- "V:/Marlborough regional/working_jaxs/July2020"
new.folder <- "V:/Marlborough regional/working_jaxs/July2020/file_for_merge"
# find the files that you want
list.of.files <- list.files(current.folder, ".csv",full.names=T) #the trick is getting the full name
list.of.files

# copy the files to the new folder
file.copy(from=list.of.files, 
          to=new.folder, 
          overwrite = TRUE)
list.files("V:/Marlborough regional/working_jaxs/July2020/file_for_merge")
#make the names a bit shorter

file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "babich_13_08_2020.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "babich.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "yld_spatial_cloudy_bay_2004_19.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "cloudy_bay.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "constellation_2017_2019_all_sau.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "constellation.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "delegates_april_2019_sau.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "delegates.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "Forrest_08_2020.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "forrest.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "Giesen_yld_data.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "giesen.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "grower_coop_V2014_to_2019.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "grower_coop.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "Indevin_02_07_2021.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "indevin.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "TWE_30_06_2021.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "twe.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "Updated_Yealands_jax.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "yealands.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "Villia_maria_2017_2012_all_sau.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "villia_maria.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "weta_yld_data.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "weta.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "white_haven_2019to2014_GPS_updated.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "white_haven.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "Wine_portfolio_yld_GPS_only_SAB.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "Wine_portfolio.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "wither_hills_GPS_block_info_harvest_sau.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "wither_hills.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "Yld_GPS_Rob_Agnew_GPS_SAB_select_sites.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "rob_agnew.csv"))
file.rename(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "pernod_ricard1_sau.csv"), 
            paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",
                   "pernod_ricard.csv"))

########################################################################################################################

file_list <- list.files("V:/Marlborough regional/working_jaxs/July2020/file_for_merge")

file_list

for (file in file_list){
  
  df <-
    read_csv(paste0("V:/Marlborough regional/working_jaxs/July2020/file_for_merge/",file))
  # just for wither hills  add ID_yr clm
  
  #2. select the clm I want only  
  df <- dplyr::select(df,
                      ID_yr,
                      year,
                      variety,
                      harvest_date,
                      julian,
                      yield_t_ha,
                      yield_kg_m,
                      bunch_weight,
                      berry_weight,
                      bunch_numb_m,
                      pruning_style,
                      row_width,
                      vine_spacing,
                      brix,
                      y_coord,
                      x_coord,
                      company)
  #na_count)
  
  #3. make variety name std
  df <- mutate(df,
               variety =  case_when(
                 variety == "SAUV" ~ "Sauvignon Blanc",
                 variety == "sb" ~ "Sauvignon Blanc",
                 variety == "SB" ~ "Sauvignon Blanc",
                 variety == "SAB" ~ "Sauvignon Blanc",
                 variety == "Sauvignon_blanc" ~ "Sauvignon Blanc",
                 TRUE ~ variety))
  
  #c. make company name std
  df <- mutate(df,
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
  site <- str_remove(file, ".csv")
  
  assign(paste("site_",site),df)
}


########################################################################################
###############      Merge all the sites into one data frame       #####################

sites_for_binding <- (ls(pattern="site_ "))
sites_for_binding
all_sites <- rbind(`site_ babich`,
                   `site_ cloudy_bay`, 
                   `site_ delegates`,
                   `site_ constellation`,
                   `site_ forrest`,
                   `site_ giesen`,
                   `site_ grower_coop`,
                   `site_ indevin`,
                   `site_ rob_agnew`,
                   `site_ twe`,
                   `site_ villia_maria`,
                   `site_ weta`,
                   `site_ white_haven`,
                   `site_ Wine_portfolio`,
                   `site_ wither_hills`,  
                   `site_ yealands`,
                   `site_ pernod_ricard`) 

#########################################################################################
### remove_low_high_values 
#d. relace 0 values with NA
all_sites <- all_sites %>% 
  replace_with_na_at(.vars = c("brix","yield_t_ha", "bunch_weight", "berry_weight"),
                     condition = ~.x == 0) 
all_sites <- all_sites %>% 
  replace_with_na_at(.vars = c("julian"),
                     condition = ~.x < 2) 
all_sites <- all_sites %>% 
  replace_with_na_at(.vars = c("brix"),
                     condition = ~.x > 100)
all_sites <- all_sites %>% 
  replace_with_na_at(.vars = c("bunch_weight"),
                     condition = ~.x > 500)

#d. set clm as certain data types
all_sites$harvest_date <- as.Date(all_sites$harvest_date,
                           origin = "1970-01-01") #this is the starting date value in R



##########################################################################################

write.csv(all_sites, "V:/Marlborough regional/working_jaxs/for_mapping_july2021/All_sites_july2021.csv")
names(all_sites)

all_sites_2014_2019 <- all_sites %>% 
  filter(between(year,2014, 2019))
write.csv(all_sites_2014_2019, "V:/Marlborough regional/working_jaxs/for_mapping_july2021/All_sites_2014_2019_july2021.csv")




############################################################################################

#with(site_Jan2020,  table(company, year))
site_table_yr <- with(filter(all_sites_2014_2019,  x_coord >0), table(company, year))
site_table_yr
write.csv(site_table_yr, "V:/Marlborough regional/working_jaxs/for_mapping_july2021/site_table_yr_july2021.csv")



################################################################################################
#Revised  27/0/2021
names(all_sites_2014_2019)



#all_sites_2014_2019 <- all_sites_2014_2019 %>% separate(ID_yr, c("Block"), sep = "_", remove = FALSE)


#2. For harvest date how many sites per year?
names(all_sites_2014_2019)

summary_harvest_date <- all_sites_2014_2019 %>%
  group_by(year, company) %>%
  summarise(mean_julian_days = mean(julian, na.rm = TRUE),
            min_julian_days = min(julian, na.rm = TRUE),
            max_julian_days = max(julian, na.rm = TRUE),
            sum_na = sum(!is.na(julian)))

#3. For yield kg/m  how many sites per year

summary_yld <- all_sites_2014_2019 %>%
  group_by(year, company) %>%
  summarise(mean_yield_kg_m = mean(yield_kg_m, na.rm = TRUE),
            min_yield_kg_m = min(yield_kg_m, na.rm = TRUE),
            max_yield_kg_m = max(yield_kg_m, na.rm = TRUE),
            sum_na = sum(!is.na(yield_kg_m)))

names(summary_harvest_date)
write.csv(summary_harvest_date, "V:/Marlborough regional/working_jaxs/for_mapping_july2021/summary_harvest_date.csv")
write.csv(summary_yld, "V:/Marlborough regional/working_jaxs/for_mapping_july2021/summary_yld.csv")




#####################################################################################################################
## Plots and checks ####
#####################################################################################################################

all_sites_2014_2019$year_factor <- as.factor(all_sites_2014_2019$year)
#julian days
ggplot(all_sites_2014_2019, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
ggplot(all_sites_2014_2019, aes(year_factor, julian, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")+
  facet_wrap(.~ company)

#yield_t_ha
ggplot(all_sites_2014_2019, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")
#yield_t_ha
filter(all_sites_2014_2019,yield_t_ha > 2 & yield_t_ha < 100 ) %>% 
  ggplot( aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")

#yield_t_ha
filter(all_sites_2014_2019,yield_t_ha > 2 & yield_t_ha < 100) %>% 
  ggplot( aes(year_factor, yield_t_ha, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")+
  facet_wrap(.~ company)


##yld kg
ggplot(all_sites_2014_2019, aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield kg/m - Sauvignon Blanc")


#yield_km_m
filter(all_sites_2014_2019,yield_kg_m > 0) %>% 
  ggplot( aes(year_factor, yield_kg_m, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Yield kg/m - Sauvignon Blanc")+
  facet_wrap(.~ company)

# brix

filter(all_sites_2014_2019,brix < 100) %>% 
  ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


filter(all_sites_2014_2019,brix < 100) %>% 
  ggplot( aes(year_factor, brix , colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Brix  - Sauvignon Blanc")+
  facet_wrap(.~ company)

#bunc wt
#filter(site_Jan2020_yr_14_18,bunch_weight < 100) %>% 
ggplot( all_sites_2014_2019, aes(year_factor, bunch_weight))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Bunch weight - Sauvignon Blanc")

ggplot(all_sites_2014_2019, aes(year_factor, bunch_weight , colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Bunch weight  - Sauvignon Blanc")+
  facet_wrap(.~ company)



all_sites_2014_2019$berry_weight <- as.double(all_sites_2014_2019$berry_weight)

filter(all_sites_2014_2019,berry_weight < 20) %>% 
  ggplot( aes(year_factor, berry_weight))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Berry weight - Sauvignon Blanc")

filter(all_sites_2014_2019,berry_weight < 20) %>% 
  ggplot( aes(year_factor, berry_weight , colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Berry weight  - Sauvignon Blanc")+
  facet_wrap(.~ company)


#bunch_numb_m 

ggplot( all_sites_2014_2019, aes(year_factor, bunch_numb_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Bunch number per m - Sauvignon Blanc")

ggplot(all_sites_2014_2019, aes(year_factor, bunch_numb_m , colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Bunch number per m  - Sauvignon Blanc")+
  facet_wrap(.~ company)

