
library(dplyr)
library(ggplot2)
library(tidyverse)
#install.packages("cowplot")
library(cowplot)

library(readxl)
library(naniar)
library(plotly)

### move the files over to a folder and then mess about with them there

"V:\Marlborough regional\working_jaxs\July2020\wither_hills_GPS_block_info_harvest_sau.csv"

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
                   `site_ yealands`) 

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

