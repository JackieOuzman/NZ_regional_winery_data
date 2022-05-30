### bring the climate and winery data togther into file that can be used for clustering

library(ggplot2)
library(dplyr)
library(tidyverse)


vineyard <- read.csv("V:/Marlborough regional/working_jaxs/for_mapping_july2022/vineyard_all_cluster_input.csv")
climate <- read.csv("V:/Marlborough regional/climate/climate_data_2022_vineyards_R/Climate_data_as_pts/climate_all_cluster_input.csv")


names(vineyard)
names(climate)

# I was expecting the files to have the same size, but I was also expecting that the climate might be less than vinyards
# but this is the opposite the vineyard file is smaller ? why
# they use the same grid file to extrcat the pts.
grid_pts_df <- read.csv("V:/Marlborough regional/Council/Regional_Grid/Marl_Ag_200_BlockGrid_100m_pts.csv") #used for climate
grid_pts_df <- read.csv("V:/Marlborough regional/Council/Regional_Grid/Marl_Ag_200_BlockGrid_100m_pts.csv") #used for vineyards / winery

#Join on field called for_join