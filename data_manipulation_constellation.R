library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(rgdal)

#mapCRS <- CRS("+init=epsg:2120")     # 2120 = NZGD2000 / Marlborough 2000 
#wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs


#####################################################################################################################
##############################           GPS POINTS for blocks #########################################################
#########################################################################################################################


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

##########################################################################################################################
##################                 Hay  spatial data              #############################################
##########################################################################################################################

constellation_Hay <- st_read("V:/Marlborough regional/Regional winery data/Raw_data/constellation/Hay/Hay.shp")
constellation_Hay_geom <- st_geometry(constellation_Hay)
centroid_Con_Hay <- st_centroid(constellation_Hay_geom)

###check of my data ####
ggplot() + 
  geom_sf(data = constellation_Hay, size = 3, color = "black") + 
  geom_sf(data = centroid_Con_Hay, size = 3, color = "blue") + 
  ggtitle("Hay") + 
  coord_sf()
#### make the centroid into a data frame
centroid_Con_Hay_df <- data.frame(matrix(unlist(centroid_Con_Hay), nrow=length(centroid_Con_Hay), byrow=T))
#### create a list of object ID to put back into centroids

hay_obID <- c(unique(constellation_Hay$OBJECTID))  
centroid_Con_Hay_df <- mutate(centroid_Con_Hay_df, OBJECTID  = hay_obID,
                              POINT_X = X1,
                              POINT_Y = X2,
                              Block_name = "Hay")

df_constellation_Hay <- st_drop_geometry(constellation_Hay)
#str(df_constellation_Hay)
### join data
constellation_Hay <- full_join(centroid_Con_Hay_df,df_constellation_Hay )
head(constellation_Hay)




##############################################################################################################################################
##########################                         Append GF to Hay                ###########################################################
##############################################################################################################################################
constellation_GF_Hay <- rbind(constellation_Hay, constellation_GF)
head(constellation_GF_Hay)


##########################################################################################################################
##################                 Opaoa  spatial data              #############################################
##########################################################################################################################

constellation_Opaoa <- st_read("V:/Marlborough regional/Regional winery data/Raw_data/constellation/Opaoa/Opaoa.shp")
constellation_Opaoa_geom <- st_geometry(constellation_Opaoa)
centroid_Con_Opaoa <- st_centroid(constellation_Opaoa_geom)

###check of my data ####
ggplot() + 
  geom_sf(data = constellation_Opaoa, size = 3, color = "black") + 
  geom_sf(data = centroid_Con_Opaoa, size = 3, color = "blue") + 
  ggtitle("Opaoa") + 
  coord_sf()
#### make the centroid into a data frame
centroid_Con_Opaoa_df <- data.frame(matrix(unlist(centroid_Con_Opaoa), nrow=length(centroid_Con_Opaoa), byrow=T))
#### create a list of object ID to put back into centroids

Opaoa_obID <- c(unique(constellation_Opaoa$OBJECTID))  
centroid_Con_Opaoa_df <- mutate(centroid_Con_Opaoa_df, OBJECTID  = Opaoa_obID,
                              POINT_X = X1,
                              POINT_Y = X2,
                              Block_name = "Opaoa")

df_constellation_Opaoa <- st_drop_geometry(constellation_Opaoa)
#str(df_constellation_Hay)
### join data
constellation_Opaoa <- full_join(centroid_Con_Opaoa_df,df_constellation_Opaoa )
head(constellation_Opaoa)

##########################################################################################################################
##################                 Selak  spatial data              #############################################
##########################################################################################################################

constellation_Selak <- st_read("V:/Marlborough regional/Regional winery data/Raw_data/constellation/Selak/Selak.shp")
constellation_Selak_geom <- st_geometry(constellation_Selak)
centroid_Con_Selak <- st_centroid(constellation_Selak_geom)

###check of my data ####
ggplot() + 
  geom_sf(data = constellation_Selak, size = 3, color = "black") + 
  geom_sf(data = centroid_Con_Selak, size = 3, color = "blue") + 
  ggtitle("Selak") + 
  coord_sf()
#### make the centroid into a data frame
centroid_Con_Selak_df <- data.frame(matrix(unlist(centroid_Con_Selak), nrow=length(centroid_Con_Selak), byrow=T))
#### create a list of object ID to put back into centroids

Selak_obID <- c(unique(constellation_Selak$OBJECTID))  
centroid_Con_Selak_df <- mutate(centroid_Con_Selak_df, OBJECTID  = Selak_obID,
                                POINT_X = X1,
                                POINT_Y = X2,
                                Block_name = "Selak")

df_constellation_Selak <- st_drop_geometry(constellation_Selak)
#str(df_constellation_Hay)
### join data
constellation_Selak <- full_join(centroid_Con_Selak_df,df_constellation_Selak )
head(constellation_Selak)


##########################################################################################################################
##################                 Spy  spatial data              #############################################
##########################################################################################################################

constellation_Spy <- st_read("V:/Marlborough regional/Regional winery data/Raw_data/constellation/Spy/Spy.shp")
constellation_Spy_geom <- st_geometry(constellation_Spy)
centroid_Con_Spy <- st_centroid(constellation_Spy_geom)

###check of my data ####
ggplot() + 
  geom_sf(data = constellation_Spy, size = 3, color = "black") + 
  geom_sf(data = centroid_Con_Spy, size = 3, color = "blue") + 
  ggtitle("Spy") + 
  coord_sf()
#### make the centroid into a data frame
centroid_Con_Spy_df <- data.frame(matrix(unlist(centroid_Con_Spy), nrow=length(centroid_Con_Spy), byrow=T))
#### create a list of object ID to put back into centroids

Spy_obID <- c(unique(constellation_Spy$OBJECTID))  
centroid_Con_Spy_df <- mutate(centroid_Con_Spy_df, OBJECTID  = Spy_obID,
                                POINT_X = X1,
                                POINT_Y = X2,
                                Block_name = "Spy")

df_constellation_Spy <- st_drop_geometry(constellation_Spy)
#str(df_constellation_Hay)
### join data
constellation_Spy <- full_join(centroid_Con_Spy_df,df_constellation_Spy )
head(constellation_Spy)


##########################################################################################################################
##################                 Valleyfield  spatial data              #############################################
##########################################################################################################################


constellation_Valleyfield <- st_read("V:/Marlborough regional/Regional winery data/Raw_data/constellation/Valleyfield/Valleyfield.shp")
constellation_Valleyfield_geom <- st_geometry(constellation_Valleyfield)
centroid_Con_Valleyfield <- st_centroid(constellation_Valleyfield_geom)

###check of my data ####
ggplot() + 
  geom_sf(data = constellation_Valleyfield, size = 3, color = "black") + 
  geom_sf(data = centroid_Con_Valleyfield, size = 3, color = "blue") + 
  ggtitle("Valleyfield") + 
  coord_sf()
#### make the centroid into a data frame
centroid_Con_Valleyfield_df <- data.frame(matrix(unlist(centroid_Con_Valleyfield), nrow=length(centroid_Con_Valleyfield), byrow=T))
#### create a list of object ID to put back into centroids

Valleyfield_obID <- c(unique(constellation_Valleyfield$OBJECTID))  
centroid_Con_Valleyfield_df <- mutate(centroid_Con_Valleyfield_df, OBJECTID  = Valleyfield_obID,
                              POINT_X = X1,
                              POINT_Y = X2,
                              Block_name = "Valleyfield")

df_constellation_Valleyfield <- st_drop_geometry(constellation_Valleyfield)
#str(df_constellation_Hay)
### join data
constellation_Valleyfield <- full_join(centroid_Con_Valleyfield_df,df_constellation_Valleyfield )
head(constellation_Valleyfield)

##########################################################################################################################
##################                 Vieceli  spatial data              #############################################
##########################################################################################################################


constellation_Vieceli <- st_read("V:/Marlborough regional/Regional winery data/Raw_data/constellation/Vieceli/Vieceli.shp")
constellation_Vieceli_geom <- st_geometry(constellation_Vieceli)
centroid_Con_Vieceli <- st_centroid(constellation_Vieceli_geom)

###check of my data ####
ggplot() + 
  geom_sf(data = constellation_Vieceli, size = 3, color = "black") + 
  geom_sf(data = centroid_Con_Vieceli, size = 3, color = "blue") + 
  ggtitle("Vieceli") + 
  coord_sf()
#### make the centroid into a data frame
centroid_Con_Vieceli_df <- data.frame(matrix(unlist(centroid_Con_Vieceli), nrow=length(centroid_Con_Vieceli), byrow=T))
#### create a list of object ID to put back into centroids

Vieceli_obID <- c(unique(constellation_Vieceli$OBJECTID))  
centroid_Con_Vieceli_df <- mutate(centroid_Con_Vieceli_df, OBJECTID  = Vieceli_obID,
                                      POINT_X = X1,
                                      POINT_Y = X2,
                                      Block_name = "Vieceli")

df_constellation_Vieceli <- st_drop_geometry(constellation_Vieceli)
#str(df_constellation_Hay)
### join data
constellation_Vieceli <- full_join(centroid_Con_Vieceli_df,df_constellation_Vieceli )
head(constellation_Vieceli)
