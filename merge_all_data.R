library(dplyr)
library(ggplot2)
library(tidyverse)

library(readxl)

delegates_april_2019 <-  read_csv( "delegates_april_2019.csv")
wither_hills_april_2019 <-  read_csv("wither_hills_april_2019.csv")
Villia_maria_april_2019<- read_csv( "Villia_maria_2017_2012_april_2019.csv")
pernod_ricard_april_2019 <- read_csv( "pernod_ricard_april_2019.csv")

glimpse(delegates_april_2019)
glimpse(wither_hills_april_2019)
glimpse(Villia_maria_april_2019)
glimpse(pernod_ricard_april_2019)


delegate_wither_villia_pern <- rbind(delegates_april_2019,
                                     wither_hills_april_2019,
                                     Villia_maria_april_2019,
                                     pernod_ricard_april_2019)
write_csv(delegate_wither_villia_pern, "delegate_wither_villia_pern.csv")
write_csv(delegate_wither_villia_pern, "delegate_wither_villia_pern2.csv")
 
