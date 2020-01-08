library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)
library(data.table)

#mapCRS <- CRS("+init=epsg:2120")     # 2120 = NZGD2000 / Marlborough 2000 
#wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs
####################################################################################################################
################     Some polygons in the blocks need to be merged          #######################################
####################################################################################################################
site <- "Awarua"

test_shapefile =  st_read(paste0("V:/Marlborough regional/Regional winery data/Raw_data/constellation/", "Awarua", "/", 
                                 "Awarua", ".shp"))

# str(test_shapefile)
# list_awarua <- unique(test_shapefile$SectionID)

test <- aggregate(test_shapefile, list(test_shapefile$SectionID ), function(x) x[1])
# dim(test_shapefile) #45 22
# dim(test) #20 23 this has reduced the number of rows and added a extra clm
# plot(test) #looks like this has not changed the shape of the vineyards
# plot(test_shapefile)


#create a centroid for newly aggreated polygons
centroid_test_shapefile <- st_centroid(test_shapefile) #this is just a check
centroid_test <- st_centroid(test)


# plot(centroid_test_shapefile)
# plot(centroid_test)

### check the centroids are where they should be
# ggplot(test) +
#   geom_sf() +
#   geom_sf(data = centroid_test_shapefile, size = 3, color = "black")+
#   geom_sf(data = centroid_test, size = 5, color = "blue", alpha = 0.5)+
#   ggtitle("check that centriods have moved")  

#can export the files to view in arcmap
# str(centroid_test)
#  st_write(centroid_test_shapefile, "test_cent1.csv", layer_options = "GEOMETRY=AS_XY")
#  st_write(centroid_test, "test_cent2.csv", layer_options = "GEOMETRY=AS_XY")



#####################################################################################################################
##############################           GPS POINTS for blocks ######################################################
##########################################################################################################################

#####################################################################################################################
#################                  perhaps a function  ?                 #############################################
#####################################################################################################################


import_shapefile_function <- function (site = site){
  shapefile =  st_read(paste0("V:/Marlborough regional/Regional winery data/Raw_data/constellation/", site, "/", site, ".shp"))
  shapefile1 <- aggregate(shapefile, list(shapefile$SectionID ), function(x) x[1])
  geom <- st_geometry(shapefile1)
  
  centroid <- st_centroid(geom)
  centroid_df <- data.frame(matrix(unlist(centroid), nrow=length(centroid), byrow=T))
  Object_ID <- c(unique(shapefile1$OBJECTID)) 
  centroid_df <- mutate(centroid_df, OBJECTID = Object_ID,
                               POINT_X = X1,
                               POINT_Y = X2,
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
  yld =  read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/constellation/", "SI Grower 2017-18 Yield Estimation", ".xlsm"),
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
                block_code = paste0("year_file", "_NA_BLOCK CODE"), 
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
  berry_bunch =  read_excel(paste0("V:/Marlborough regional/Regional winery data/Raw_data/constellation/", "SI Grower 2017-18 Yield Estimation", ".xlsm"),
                            "data entry" ,  col_names = FALSE)
  ######now select the row 1, 2, 3, 4 #Try this is might work better?
  berry_bunch = filter(berry_bunch,`...1` == year_file |  `...1` == "Date" |`...1` == "ESTIMATED BUNCH WEIGHT (g)" | `...1` == "ESTIMATED BERRY WEIGHT (g)")
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


yld_2017_18_AwatereValley <- yld_ber_bun_import_function("SI Company Awatere Valley 2017-18 Yield Estimation", "2017-18") 
yld_2017_18_North_Wairau <- yld_ber_bun_import_function("SI Company North Wairau 2017-18 Yield Estimation", "2017-18") 
yld_2017_18_South_Wairau <- yld_ber_bun_import_function("SI Company South Wairau 2017-18 Yield Estimation", "2017-18") 
yld_2017_18_SI <- yld_ber_bun_import_function_est("SI Grower 2017-18 Yield Estimation", "2017-18") #use estimation function



yld_2018_19_Central_Wairau <- yld_ber_bun_import_function("SI Company Central Wairau 2018-2019 Yield Estimation", "2018-19") #problem
yld_2018_19_Lower_Wairau <- yld_ber_bun_import_function("SI Company Lower Wairau 2018-2019 Yield Estimation", "2018-19") 
yld_2018_19_Upper_Wairau <- yld_ber_bun_import_function("SI Company Upper Wairau 2018-2019 Yield Estimation", "2018-19") 
yld_2018_19_SI <- yld_ber_bun_import_function("SI Grower 2018-19 Yield Estimation", "2018-19") 


##############################################################################################################################################
##########################                         Append all yld data                 ###########################################################
##############################################################################################################################################
constellation_yld2017_2018 <-     rbind(yld_2017_18_AwatereValley,
                                        yld_2017_18_North_Wairau,
                                        yld_2017_18_South_Wairau,
                                        yld_2017_18_SI)



constellation_yld201_2019 <- rbind(yld_2018_19_Central_Wairau,
                               yld_2018_19_Lower_Wairau,
                               yld_2018_19_Upper_Wairau,
                               yld_2018_19_SI)                       




head(yld_2017_18_AwatereValley)
dim(yld_2017_18_AwatereValley)
head(yld_2017_18_North_Wairau)
head(yld_2017_18_South_Wairau)
head(yld_2017_18_SI)

dim(yld_2018_19_Central_Wairau)
dim(yld_2018_19_Lower_Wairau)
dim(yld_2018_19_Upper_Wairau)
dim(yld_2018_19_SI)

#Note that I want to match up the sectionID to the block code but note that sometimes I have multiple entries for the section
#this relates to the sub_sect_ID - Not sure 



############################################################################################################################

##########################################################################################################################
##################                 Giffords_Creek  spatial data              #############################################
##########################################################################################################################

constellation_GF <- st_read("V:/Marlborough regional/Regional winery data/Raw_data/constellation/Giffords_Creek/Giffords_Creek.shp")
#head(constellation_GF,3)


constellation_GF_geom <- st_geometry(constellation_GF)
#head(constellation_GF_geom,3)

centroid_Con_GF <- st_centroid(constellation_GF_geom)
#head(centroid_Con_GF,3)

ggplot() + 
  geom_sf(data = constellation_GF, size = 3, color = "black") + 
  geom_sf(data = centroid_Con_GF, size = 3, color = "blue") + 
  ggtitle("constellation_GF") + 
  coord_sf()

centroid_Con_GF_df <- data.frame(matrix(unlist(centroid_Con_GF), nrow=length(centroid_Con_GF), byrow=T))
#head(centroid_Con_GF_df,3)
#str(centroid_Con_GF_df)
#table(df_constellation_GF$OBJECTID)
#head(centroid_Con_GF_df)
#### I have a big problem here all my centroid values have no attributes attached I need to have this!
##### This is sooo not the way to do it!
#### create a list of object ID to put back into centroids

GF_Object_ID <- c(unique(constellation_GF$OBJECTID)) 
centroid_Con_GF_df <- mutate(centroid_Con_GF_df, OBJECTID = GF_Object_ID,
                             POINT_X = X1,
                             POINT_Y = X2,
                             Block_name = "Giffords_creek")

df_constellation_GF <- st_drop_geometry(constellation_GF)
#str(df_constellation_GF)

constellation_GF <- full_join(centroid_Con_GF_df,df_constellation_GF )
head(constellation_GF)


##This looks good####
#check_GF <- write.csv(constellation_GF, "constellation_GF.csv")





#