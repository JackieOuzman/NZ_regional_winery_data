
library(terra)
library(raster)
library(sp)
library(ggplot2)
library(rgdal)
library(dplyr)
library(rasterVis)
library(tidyverse)

### bring in the GST grids and extract values for each grid pt
# I havent work out how to do the sample function so I have done this in ESRI



###############################################################################################
###  Bring in the points for every cell in raster with also with the subregions attached
# this was done in Arcmap "V:\Marlborough regional\Council\Regional_Grid\extracting_pts_frm_base_grid.mxd"

grid_pts_df <- read.csv("V:/Marlborough regional/Council/Regional_Grid/Marl_Ag_200_BlockGrid_100m_pts.csv")

grid_pts_vec <- vect( grid_pts_df, geom=c('X','Y'), crs=paste0("epsg:",2193) )
#plot(grid_pts_vec)






## list in the vineyard data - julian
#type <-"Julian" #"Julian""yld", "yld_norm"
type <-"yld_norm" #"Julian""yld", "yld_norm"
#directory <- "V:/Marlborough regional/working_jaxs/for_mapping_july2022/Julian/Julian"
directory <- "V:/Marlborough regional/working_jaxs/for_mapping_july2022/Yield/Yld_norm"

list.files(directory) 


vineyard_files <- list.files(directory, pattern = ".tif")

vineyard_files <- as.data.frame(vineyard_files)
vineyard_files # now I have .ml and .ovr files I need to remove from my list

vineyard_files <- vineyard_files %>% filter(str_detect(vineyard_files, ".ovr", negate = TRUE))
vineyard_files <- vineyard_files %>% filter(str_detect(vineyard_files, ".xml", negate = TRUE))

#And the folder
vineyard_files <- vineyard_files %>% filter(str_detect(vineyard_files, "slighly of tiff and asc", negate = TRUE))
vineyard_files <- vineyard_files %>% filter(str_detect(vineyard_files, "slight_off_tiff and asc", negate = TRUE))

vineyard_files
vineyard_files$vineyard_files <- as.character(vineyard_files$vineyard_files)
#Turn into a list

file_list <- vineyard_files[["vineyard_files"]] 

file_list

#file_list <- file_list[1]

## this will be the loop
for (file_list in file_list){

  
## get the year from the file name
  
  step1<-sub(".tiff*", "", file_list) # Extract characters before pattern # 
  bit_to_extract <- paste0(".*","Krig_", type )
  year2 <-sub(bit_to_extract, "", step1)# Extract characters after pattern
  year <- str_sub(year2,-4,-1)
  rm(step1, year2, bit_to_extract)

## bring in the climate raster 
  vineyard_raster <- rast(paste0(directory, "/",file_list)) #my computer

  ## extract the points for the cliamte raster
  vineyard_raster <- terra::extract(  x = vineyard_raster,
                              y = grid_pts_vec,
                              xy=TRUE)
#head(vineyard_raster)  
colnames(vineyard_raster) <- c("ID",type,"x", "y" ) 
vineyard_raster <- vineyard_raster %>% dplyr::mutate(year = year,
                                     for_join = paste0(x, "_", y))
grid_pts_df <- grid_pts_df %>% dplyr::mutate(for_join = paste0(X, "_", Y))

head(grid_pts_df)
head(vineyard_raster)

vineyard_raster <- left_join(vineyard_raster, grid_pts_df)

vineyard_raster <- vineyard_raster %>% 
  dplyr::select(-Rowid_,
         - MARL_AG_200_BLOC,
         - x,
         -y,
         -MARL_AG_200_BL_1)
  
name <- paste0(type,"_", year)
assign(name,vineyard_raster)  
rm(vineyard_raster, name,year)

} 



# vineyard_raster_julian <- rbind(
#   Julian_2014,
#   Julian_2015,
#   Julian_2016,
#   Julian_2017,
#   Julian_2018,
#   Julian_2019
#   )
# #
#  rm(
#    Julian_2014,
#    Julian_2015,
#    Julian_2016,
#    Julian_2017,
#    Julian_2018,
#    Julian_2019)

vineyard_raster_yld_norm <- rbind(
  yld_norm_2014,
  yld_norm_2015,
  yld_norm_2016,
  yld_norm_2017,
  yld_norm_2018,
  yld_norm_2019
  )
#
 rm(
yld_norm_2014,
yld_norm_2015,
yld_norm_2016,
yld_norm_2017,
yld_norm_2018,
yld_norm_2019)

#########################################################################################




### make dataset wider for each climate variable
 vineyard_raster_julian_wider <- vineyard_raster_julian %>% 
  pivot_wider(
    names_from = year,
    names_prefix = "julian_",
    values_from = Julian)

 vineyard_raster_yld_norm_wider <- vineyard_raster_yld_norm %>% 
  pivot_wider(
    names_from = year,
    names_prefix = "yld_norm_",
    values_from = yld_norm)





vineyard_all <- left_join(vineyard_raster_yld_norm_wider,
                          vineyard_raster_julian_wider)


#remove all the NA
unique(vineyard_all$yld_norm_2014)

vineyard_all_no_missing <- vineyard_all %>% filter(!is.na(yld_norm_2014)) #its the same now - I am happy:)

#######################################################################################################################################
#######################################      save the outputs       ################################################################### 
#######################################################################################################################################

names(vineyard_all_no_missing)
directory

write.csv(
  vineyard_all,
  paste0(
    "V:/Marlborough regional/working_jaxs/for_mapping_july2022/",
    "vineyard_all_cluster_input.csv"),
          row.names = FALSE) 






