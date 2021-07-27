library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)



#####################################################################################################################
##############################           GPS POINTS for blocks ######################################################
##########################################################################################################################


import_shapefile_function <- function (site = site){
  shapefile =  st_read(paste0("V:/Marlborough regional/Regional winery data/Raw_data/constellation/", site, "/", site, ".shp"))
  shapefile1 <- aggregate(shapefile, list(shapefile$SectionID ), function(x) x[1])
  geom <- st_geometry(shapefile1)
  
  centroid <- st_centroid(geom)
  centroid_df <- data.frame(matrix(unlist(centroid), nrow=length(centroid), byrow=T))
  Object_ID <- c(unique(shapefile1$OBJECTID)) 
  centroid_df <- mutate(centroid_df, OBJECTID = Object_ID,
                               POINT_X = X2,
                               POINT_Y = X1,
                               Block_name = site)
  shapefile_df <- st_drop_geometry(shapefile1)
  constellation_site_centroid <- full_join(shapefile_df,centroid_df )
  return(constellation_site_centroid)
}



####################################################################################################################
###################            Use function to bring in the spatial data                              ##############
####################################################################################################################

#group 1
Giffords_Creek <- import_shapefile_function( "Giffords_Creek")
Hay <-          import_shapefile_function( "Hay")
Opaoa <-        import_shapefile_function( "Opaoa")
Selak <-        import_shapefile_function( "Selak")
Spy <-          import_shapefile_function( "Spy")
Valleyfield <-  import_shapefile_function( "Valleyfield")
Vieceli <-      import_shapefile_function( "Vieceli")

constellation_spatial_group1 <- rbind(
                               Giffords_Creek,
                               Hay,
                               Opaoa,
                               Selak,
                               Spy,
                               Valleyfield,
                               Vieceli
                               )    


#group 2
Awatere_Hills <-  import_shapefile_function( "Awatere_Hills")
Caseys_Road <-    import_shapefile_function( "Caseys_Road")
Castle_Cliffs <-  import_shapefile_function( "Castle_Cliffs")
Favourite <-      import_shapefile_function( "Favourite")
Brooklands <-     import_shapefile_function( "Brooklands")
Bells_Road <-     import_shapefile_function( "Bells_Road")



constellation_spatial_group2 <- rbind(
                               Awatere_Hills,
                               Bells_Road,
                               Brooklands,
                               Caseys_Road,
                               Castle_Cliffs,
                               Favourite
                               )    
#group 3
Awarua <-         import_shapefile_function( "Awarua")
Awarua <- select(Awarua, -AREA_GEO) #oops I made and extra clm that need removing
Rarangi <-        import_shapefile_function( "Rarangi")
Spring_Creek <-   import_shapefile_function( "Spring_Creek")
Springfields <-   import_shapefile_function( "Springfields")
Drylands <-       import_shapefile_function( "Drylands")
Marukoko <-       import_shapefile_function( "Marukoko")

dim(Awarua)
dim(Drylands)
dim(Marukoko)
dim(Rarangi)
dim(Spring_Creek)
dim(Springfields)


constellation_spatial_group3 <- rbind(Awarua, 
                               Drylands,
                               Marukoko,
                               Rarangi,
                               Spring_Creek,
                               Springfields
                               )    

#group 4
Matador <-          import_shapefile_function( "Matador")
Valley_East <-      import_shapefile_function( "Valley_East")
Watsons_Road <-     import_shapefile_function( "Watsons_Road") 
Scarlett_Rise <-    import_shapefile_function( "Scarlett_Rise")

constellation_spatial_group4 <- rbind(
                                Matador,
                                Scarlett_Rise,
                               Valley_East,
                               Watsons_Road
                               )    

#group 5
Erina <-            import_shapefile_function( "Erina")
Hillersden <-       import_shapefile_function( "Hillersden")
Mill_Stream <-      import_shapefile_function( "Mill_Stream")
The_Narrows <-      import_shapefile_function( "The_Narrows")
Wairau_Riverbank <- import_shapefile_function( "Wairau_Riverbank")

constellation_spatial_group5 <- rbind(
                               Erina,
                               Hillersden,
                               Mill_Stream,
                               The_Narrows,
                               Wairau_Riverbank)                       


##############################################################################################################################################
##########################                         Append all spatial data                 ###########################################################
##############################################################################################################################################
constellation_spatial <- rbind(constellation_spatial_group5,
                               constellation_spatial_group4,
                               constellation_spatial_group3,
                               constellation_spatial_group2,
                               constellation_spatial_group1)                       
                               
                               
                               
                               
head(constellation_spatial)


### Remove the object I dont need  ######
#1
rm(
  list = c(
  "Giffords_Creek",
  "Hay",
  "Opaoa",
  "Selak",
  "Spy",
  "Valleyfield",
  "Vieceli"
) )   

#2
rm(
  list = c(
  "Awatere_Hills",
  "Bells_Road",
  "Brooklands",
  "Caseys_Road",
  "Castle_Cliffs",
  "Favourite"
) )   

#3
rm(
  list = c("Awarua", 
           "Drylands",
           "Marukoko",
           "Rarangi",
           "Spring_Creek",
           "Springfields"
))    
#4
rm(
  list = c(
    "Matador",
    "Scarlett_Rise",
    "Valley_East",
    "Watsons_Road"
  ) )   
#5
rm(
  list = c(
    "Erina",  
    "Hillersden",
    "Mill_Stream",
    "The_Narrows",
    "Wairau_Riverbank"
  )
)
 
#merged sites 1-5
rm(
  list = c(
    "constellation_spatial_group5",
    "constellation_spatial_group4",
    "constellation_spatial_group3",
    "constellation_spatial_group2",
    "constellation_spatial_group1"))   



#Note that I want to match up the sectionID to the block code but note that sometimes I have multiple entries for the section
#I have merged polygons with the same sectionID togther - not sure if this is correct.
#example is Awarua spatial data has section ID called NARUASBM 2729 = 3.97ha, 2732 = 3.03ha, 2736 = 3.00ha all togther about 9 ha
#in excel sheet the block ID NARUASBM says its 7.01 ha
#this is from "SI Company Lower Wairau 2018-2019 Yield Estimation.xlsm" file


##############################################################################################################################################
################            yield  berry wt and bunch wt data         Block summary tab      ################################################
##############################################################################################################################################


#Yike this only works for some of my files
yld_ber_bun_import_function <- function(file_name, year_file){
  
  #yld data first
  yld =  read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/constellation/", file_name, ".xlsm"),
                    "Block Summary" ,  col_names = FALSE,  range = "A1:AA100" )
  
 ######merge the first 3 rows all into first row
  yld <- data.frame( 
    rbind(head(yld, 0), as.character(paste0(yld[1,], "_", yld[2,], "_", yld[3,])), tail(yld, -1)),  
    row.names = NULL) 
  #######now set the row names as column headings
  colnames(yld) <- yld[1,]
  #######now remove the row 2 and 3
  yld = yld[-c(1,2,3),] 
  as.data.frame(colnames(yld))
  #######clmns that I need for now
    yld <- select(yld,
                  block_code = paste0(year_file, "_NA_BLOCK CODE"), 
                  #block_code = `2017-18_NA_BLOCK CODE`, 
                  variety = `NA_NA_Variety Grouping`, 
                  row_spacing = `NA_NA_Row Spacing`, 
                  vine_spacing = `NA_NA_Vine Spacing`, 
                  total_vines = `NA_NA_Total Vines`, 
                  row_m_ha = `NA_NA_Row \r\nM / Ha`, 
                  actual_T = `NA_Mar-Apr_ACTUAL \r\nT`,
                  actual_ha = `NA_Mar-Apr_ACTUAL Ha`)
  
  #remove the subtotal from row_m_ha
  yld <- filter(yld, row_m_ha != "Sub Total:")
  
 #This pulls out the date blockId and bunch wt and berry weight
    berry_bunch =  read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/constellation/", file_name, ".xlsm"),
                      "data entry" ,  col_names = FALSE)
    ######now select the row 1, 2, 3, 4 #Try this is might work better?
    berry_bunch = filter(berry_bunch,`...1` == year_file |  `...1` == "Date" |`...1` == "ACTUAL BUNCH WEIGHT" | `...1` == "ACTUAL BERRY WEIGHT")
    berry_bunch = berry_bunch[-c(2),]
    
    #berry_bunch = berry_bunch[c(4, 108, 114:115),] 
    ######remove two clms
    berry_bunch <- berry_bunch[,-c(2, 3)] 
    berry_bunch <- transpose(berry_bunch)
    #######fix up the first name
    berry_bunch[1,1] = 'block_code'
    berry_bunch[1,3] = 'bunch_wt'
    berry_bunch[1,4] = 'berry_wt'
    colnames(berry_bunch) <- berry_bunch[1,]
    ######remove the first row
    berry_bunch <- berry_bunch[-c(1),] 
    #########remove all the NA for block ID
    berry_bunch <- filter(berry_bunch, block_code != "NA")
    berry_bunch <- mutate(berry_bunch, year = year_file)
    
    #join data
    yld_bun_berr <- full_join(berry_bunch, yld)
    return(yld_bun_berr)
    
}

###############################################################################################################################################
######################                 EStimation is different in file #######################################################################
#############################################################################################################################################
#changed actual berry and bunch wts to ESTIMATED as per the change in file
yld_ber_bun_import_function_est <- function(file_name, year_file){
  
  #yld data first
  yld =  read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/constellation/", file_name , ".xlsm"),
                    "Block Summary" ,  col_names = FALSE,  range = "A1:AA100" )
  
  ######merge the first 3 rows all into first row
  yld <- data.frame( 
    rbind(head(yld, 0), as.character(paste0(yld[1,], "_", yld[2,], "_", yld[3,])), tail(yld, -1)),  
    row.names = NULL) 
  #######now set the row names as column headings
  colnames(yld) <- yld[1,]
  #######now remove the row 2 and 3
  yld = yld[-c(1,2,3),] 
  as.data.frame(colnames(yld))
  #######clmns that I need for now
  yld <- select(yld,
                block_code = paste0(year_file, "_NA_BLOCK CODE"), 
                #block_code = `2017-18_NA_BLOCK CODE`, 
                variety = `NA_NA_Variety Grouping`, 
                row_spacing = `NA_NA_Row Spacing`, 
                vine_spacing = `NA_NA_Vine Spacing`, 
                total_vines = `NA_NA_Total Vines`, 
                row_m_ha = `NA_NA_Row \r\nM / Ha`, 
                actual_T = `NA_Mar-Apr_ACTUAL \r\nT`,
                actual_ha = `NA_Mar-Apr_ACTUAL Ha`)
  
  #remove the subtotal from row_m_ha
  yld <- filter(yld, row_m_ha != "Sub Total:")
  yld <- mutate(yld, year = year_file)
  #This pulls out the date blockId and bunch wt and berry weight
  berry_bunch =  read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/constellation/", file_name , ".xlsm"),
                            "data entry" ,  col_names = FALSE)
  ######now select the row 1, 2, 3, 4 #Try this is might work better?
  berry_bunch = filter( berry_bunch, `...3` == "BLOCK CODE" |  `...1` == "Date" |`...1` == "ESTIMATED BUNCH WEIGHT (g)" | `...1` == "ESTIMATED BERRY WEIGHT (g)")
  berry_bunch = berry_bunch[-c(5),]
  
  #berry_bunch = berry_bunch[c(4, 108, 114:115),] 
  ######remove two clms
  #berry_bunch <- berry_bunch[,-c(2, 3)] 
  berry_bunch <- transpose(berry_bunch)
  #######fix up the first name
  berry_bunch[1,1] = 'block_code'
  berry_bunch[1,3] = 'bunch_wt'
  berry_bunch[1,4] = 'berry_wt'
  colnames(berry_bunch) <- berry_bunch[1,]
  ######remove the first row
  berry_bunch <- berry_bunch[-c(1),] 
  #########remove all the NA for block ID
  berry_bunch <- filter(berry_bunch, block_code != "NA")
  berry_bunch <- mutate(berry_bunch, year = year_file)
  
  #join data
  yld_bun_berr <- full_join(berry_bunch, yld)
  yld_bun_berr <- filter(yld_bun_berr, block_code != "BLOCK CODE")
  yld_bun_berr <- filter(yld_bun_berr, block_code != "Block Code")
  return(yld_bun_berr)
  
}



yld_2017_18_AwatereValley <- yld_ber_bun_import_function("SI Company Awatere Valley 2017-18 Yield Estimation", "2017-18") 
yld_2017_18_North_Wairau <- yld_ber_bun_import_function("SI Company North Wairau 2017-18 Yield Estimation", "2017-18") 
yld_2017_18_South_Wairau <- yld_ber_bun_import_function("SI Company South Wairau 2017-18 Yield Estimation", "2017-18") 
yld_2017_18_SI <- yld_ber_bun_import_function_est("SI Grower 2017-18 Yield Estimation", "2017-18") #use estimation function



yld_2018_19_Central_Wairau <- yld_ber_bun_import_function("SI Company Central Wairau 2018-2019 Yield Estimation", "2018-19") #problem - fixed
yld_2018_19_Lower_Wairau <- yld_ber_bun_import_function("SI Company Lower Wairau 2018-2019 Yield Estimation", "2018-19") 
yld_2018_19_Upper_Wairau <- yld_ber_bun_import_function("SI Company Upper Wairau 2018-2019 Yield Estimation", "2018-19") 
yld_2018_19_SI <- yld_ber_bun_import_function_est("SI Grower 2018-19 Yield Estimation", "2018-19") 


##############################################################################################################################################
##########################                         Append all yld data                 ###########################################################
##############################################################################################################################################
constellation_yld2017_2018 <-     rbind(yld_2017_18_AwatereValley,
                                        yld_2017_18_North_Wairau,
                                        yld_2017_18_South_Wairau,
                                        yld_2017_18_SI)



constellation_yld2018_2019 <- rbind(yld_2018_19_Central_Wairau,
                               yld_2018_19_Lower_Wairau,
                               yld_2018_19_Upper_Wairau,
                               yld_2018_19_SI)                       


constellation_yld2017_2019 <- rbind(constellation_yld2017_2018,constellation_yld2018_2019 )

############################################################################################################################
############## Remove the objects I don't need #############################################################################
#2017-2018
rm(
  list = c(
    "yld_2017_18_AwatereValley",
    "yld_2017_18_North_Wairau",
    "yld_2017_18_South_Wairau",
    "yld_2017_18_SI")
  )

#2018-2019
rm(
  list = c(
    "yld_2018_19_Central_Wairau",
    "yld_2018_19_Lower_Wairau",
    "yld_2018_19_Upper_Wairau",
    "yld_2018_19_SI")
)

rm(
  list = c("constellation_yld2017_2018","constellation_yld2018_2019"))



#Note that I want to match up the sectionID to the block code but note that sometimes I have multiple entries for the section
#this relates to the sub_sect_ID - Not sure 

###########################################################################################################################

#Join the spatial data to the yield data

str(constellation_yld2017_2019)
str(constellation_spatial1) #this is too much info select less

constellation_spatial <- select(constellation_spatial,
                                block_code = SectionID ,
                                #BlockID = block_code,
                                POINT_X,
                                POINT_Y)

constellation_yld2017_2019_spatial <- left_join(constellation_yld2017_2019, constellation_spatial)
str(constellation_yld2017_2019_spatial)

#change some clm to double
constellation_yld2017_2019_spatial$bunch_wt <- as.double(constellation_yld2017_2019_spatial$bunch_wt)
constellation_yld2017_2019_spatial$berry_wt <- as.double(constellation_yld2017_2019_spatial$berry_wt)                                     
constellation_yld2017_2019_spatial$ row_spacing <- as.double(constellation_yld2017_2019_spatial$ row_spacing)
constellation_yld2017_2019_spatial$vine_spacing <- as.double(constellation_yld2017_2019_spatial$vine_spacing)
constellation_yld2017_2019_spatial$total_vines <- as.double(constellation_yld2017_2019_spatial$total_vines)
constellation_yld2017_2019_spatial$ row_m_ha <- as.double(constellation_yld2017_2019_spatial$ row_m_ha)
constellation_yld2017_2019_spatial$ actual_T <- as.double(constellation_yld2017_2019_spatial$ actual_T)
constellation_yld2017_2019_spatial$ actual_ha <- as.double(constellation_yld2017_2019_spatial$ actual_ha)


str(constellation_yld2017_2019_spatial$year)
unique(constellation_yld2017_2019_spatial$year)
constellation_yld2017_2019_spatial <- mutate(constellation_yld2017_2019_spatial,
                                                             year_vintage =  case_when(
                                                               year == "2017-18" ~ "2018",
                                                               year == "2018-19" ~ "2019",
                                                               TRUE ~ year))

unique(constellation_yld2017_2019_spatial$year_vintage)
#remove year clm
constellation_yld2017_2019_spatial <- select(constellation_yld2017_2019_spatial,
                                             -year)



constellation_yld2017_2019_spatial <- mutate(constellation_yld2017_2019_spatial,
                                             year = year_vintage)

str(constellation_yld2017_2019_spatial)
constellation_yld2017_2019_spatial$year <- as.double(constellation_yld2017_2019_spatial$year)


constellation_yld2017_2019_spatial <- mutate(constellation_yld2017_2019_spatial,
                                     #julian = as.numeric(format(constellation_yld2017_2019_spatial$harvest_date, "%j")),
                                     harvest_date = NA,
                                     julian = NA,
                                     brix = NA,
                                     pruning_style = NA,
                                     yield_t_ha = actual_T / actual_ha,
                                     meter_row_per_ha = 10000/row_spacing,
                                     yld_per_m_row_kg =  (yield_t_ha *1000) / 10000/row_spacing,
                                     bunch_m = (yld_per_m_row_kg * 1000)/ bunch_wt,
                                     company = "constellation",
                                     y_coord = POINT_X,
                                     x_coord =  POINT_Y ,
                                     yield_kg_m = NA,
                                     bunch_numb_m	 = NA,
                                     yield_kg_m 	= ( yield_t_ha * 1000) / (10000/row_spacing),
                                     ID_yr = paste0(block_code, "_", year)
)




constellation_yld2017_2019_spatial <- select(constellation_yld2017_2019_spatial,
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
                                              bunch_weight = bunch_wt,
                                              berry_weight = berry_wt,
                                              bunch_numb_m,
                                              pruning_style,
                                              row_width = row_spacing,
                                              vine_spacing
                                              )
                                              
                                              
                                        
constellation_yld2017_2019_spatial$na_count <- apply(is.na(constellation_yld2017_2019_spatial), 1, sum)

str(constellation_yld2017_2019_spatial)
###################################################################################################
#Keep only the sav


str(constellation_yld2017_2019_spatial)


unique(constellation_yld2017_2019_spatial$variety)
  
constellation_yld2017_2019_spatial_SAU <- filter(constellation_yld2017_2019_spatial,
                                                 variety == "Sauvignon Blanc")

#heaps of data points are missing coordinates these are:

missing_coords <- constellation_yld2017_2019_spatial_SAU %>% 
  filter(is.na(x_coord))
str(missing_coords)
missing_coords<-missing_coords %>%
  mutate(yield_t_ha = case_when(yield_t_ha == 0.0000 ~ NA_real_,
                                yield_t_ha == "NaN" ~ NA_real_, 
                                  TRUE ~ yield_t_ha))
missing_coords<-missing_coords %>%
  mutate(yield_kg_m = case_when(yield_kg_m == 0.0000 ~ NA_real_,
                                yield_kg_m == "NaN" ~ NA_real_,
                                TRUE ~ yield_kg_m)) 
  
sites_with_no_coord_bunch_weight    <- filter(missing_coords,is.na(bunch_weight ))
sites_with_no_coord_noYld   <- filter(missing_coords,is.na(yield_t_ha))

sites_with_no_coord_no_att <- filter(missing_coords,
                                     is.na(bunch_weight) & is.na(yield_t_ha ))
sites_with_no_coord_more_att <- filter(missing_coords,
                                     !is.na(bunch_weight) & !is.na(yield_t_ha ))
dim(sites_with_no_coord_noYld)
dim(sites_with_no_coord_bunch_weight)
dim(missing_coords) 

worth_chasing_up <- sites_with_no_coord_more_att
names(worth_chasing_up)
worth_chasing_up <-separate(worth_chasing_up, ID_yr, into = c("temp1", "temp2"), remove = FALSE)
worth_chasing_up <- distinct(worth_chasing_up, temp1)
# write_csv(worth_chasing_up, 
#           "V:/Marlborough regional/working_jaxs/July2020/constellation_worth_chasing_up_no_coods.csv")

############################################################################## 
########################    File to use   ####################################
constellation_yld2017_2019_spatial_SAU_gps <- filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0)
names(constellation_yld2017_2019_spatial_SAU_gps)

constellation_yld2017_2019_spatial_SAU_gps <-
  constellation_yld2017_2019_spatial_SAU_gps %>%
  mutate(yield_t_ha = case_when(yield_t_ha == 0.0000 ~ NA_real_,
                                  TRUE ~ yield_t_ha))
constellation_yld2017_2019_spatial_SAU_gps <-
  constellation_yld2017_2019_spatial_SAU_gps %>%
  mutate(yield_kg_m = case_when(yield_kg_m == 0.0000 ~ NA_real_,
                                  TRUE ~ yield_kg_m))
constellation_yld2017_2019_spatial_SAU_gps <-
  constellation_yld2017_2019_spatial_SAU_gps %>%
  mutate(bunch_weight = case_when(bunch_weight == 0.0000 ~ NA_real_,
                                  TRUE ~ bunch_weight))
constellation_yld2017_2019_spatial_SAU_gps <-
  constellation_yld2017_2019_spatial_SAU_gps %>%
  mutate(berry_weight = case_when(berry_weight == 0.0000 ~ NA_real_,
                                  TRUE ~ berry_weight))
View(constellation_yld2017_2019_spatial_SAU_gps)
write_csv(constellation_yld2017_2019_spatial_SAU_gps, 
          "V:/Marlborough regional/working_jaxs/July2020/constellation_2017_2019_all_sau.csv")
##############################################################################   





######################################################################################################################
################                         view and summaries DF 2019 -2014                            #################
######################################################################################################################


dim(constellation_yld2017_2019_spatial_SAU)
#how many site are SAU?
dim(constellation_yld2017_2019_spatial_SAU)
glimpse(constellation_yld2017_2019_spatial_SAU) #418 records
max(constellation_yld2017_2019_spatial_SAU$year) #2018-2019
min(constellation_yld2017_2019_spatial_SAU$year) #2017-2018
#how many site are SAU with GPS
count(filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0)) #297



#how many sites with GPS pts all varieties
glimpse(constellation_yld2017_2019_spatial_SAU  )#418 records
#how many sites with GPS pts all varieties
count(filter(constellation_yld2017_2019_spatial,  x_coord >0)) #399

filter(constellation_yld2017_2019_spatial,  x_coord >0) %>% 
ggplot( aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites with GPS coordinates")

#how many sites with GPS pts by Variety by year
filter(constellation_yld2017_2019_spatial,  x_coord >0) %>% 
ggplot( aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)




#create a new variable year_as_factor
constellation_yld2017_2019_spatial_SAU$year_factor <- as.factor(constellation_yld2017_2019_spatial_SAU$year)

#filter data for Sauvignon Blanc
constellation_yld2017_2019_spatial_SAU

#how many sites for Sauvignon Blanc by year
filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0) %>% 
group_by(year) %>% 
  count() # 2017 - 2018 = 164 and 2018 -2019 = 133

####################################################################################################



str(constellation_yld2017_2019_spatial_SAU)

#how many sites for Sauvignon Blanc have missing data - how much missing data?
filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0) %>% 
ggplot( aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")
#how many sites for Sauvignon Blanc have missing data - missing data grouped together?
filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0) %>%
ggplot( aes(na_count))+
  geom_bar()+
  scale_x_continuous(breaks =  c(2,4,6,8,10))+
  facet_wrap(~year_factor)+
  theme_bw()+
  labs(x = "number of na counts per entry",
       y= "Counts of missing data entries NA")

########################################################################################################

#check stuff 


glimpse(constellation_yld2017_2019_spatial_SAU)
#julian days
filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0) %>%
ggplot( aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
#yield_t_ha
filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0) %>%
ggplot( aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")
#yield_kg_m
filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0) %>%
ggplot( aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")

#yield_kg_m filter out zeros
filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0) %>%
filter(yield_kg_m != 0) %>% 
  ggplot( aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")


#brix - too many zero
filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0) %>%
ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


#brix - filter out high values
filter(constellation_yld2017_2019_spatial_SAU,  x_coord >0) %>%
filter(brix <40) %>% 
  ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")
#######################################################################################

#Revised constellation_yld2017_2019_spatial_SAU 21/0/2021
names(constellation_yld2017_2019_spatial_SAU)

#just need to make a block 
# yld_spatial_cloudy_bay_2004_19 <- yld_spatial_cloudy_bay_2004_19 %>%
#   dplyr::mutate(

constellation_yld2017_2019_spatial_SAU <- constellation_yld2017_2019_spatial_SAU %>% separate(ID_yr, c("Block"), sep = "_", remove = FALSE)

#1. How many sites?
#for each year
constellation_yld2017_2019_spatial_SAU %>%
  group_by(year) %>%
  summarise(count = n_distinct(Block))
#overall for the data set from 2014-2019 how many blocks do we have?
constellation_yld2017_2019_spatial_SAU %>%
  summarise(count = n_distinct(Block))

#2. For harvest date how many sites per year?
names(constellation_yld2017_2019_spatial_SAU)

constellation_yld2017_2019_spatial_SAU %>%
  group_by(year) %>%
  summarise(mean_julian_days = mean(julian, na.rm = TRUE),
            min_julian_days = min(julian, na.rm = TRUE),
            max_julian_days = max(julian, na.rm = TRUE),
            sum_na = sum(!is.na(julian)))

#3. For yield kg/m  how many sites per year

constellation_yld2017_2019_spatial_SAU %>%
  group_by(year) %>%
  summarise(mean_yield_kg_m = mean(yield_kg_m, na.rm = TRUE),
            min_yield_kg_m = min(yield_kg_m, na.rm = TRUE),
            max_yield_kg_m = max(yield_kg_m, na.rm = TRUE),
            sum_na = sum(!is.na(yield_kg_m)))







