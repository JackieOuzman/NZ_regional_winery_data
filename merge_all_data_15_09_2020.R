library(dplyr)
library(ggplot2)
library(tidyverse)
#install.packages("cowplot")
library(cowplot)

library(readxl)
library(naniar)
library(plotly)

getwd()

### set up function to standardise how the different sites look.
##1. define the function inputs
# site <- "whitehaven"
# file <- "white_haven_2019to2014_GPS_updated.csv"
 # site <- "pernod_ricard"
 # file <- "pernod_ricard_april_2019.csv"
# site <- "Villia_maria"
# file <- "Villia_maria_2017_2012_all_sau.csv"
# site <- "delegates"
# file <- "delegates_april_2019_sau.csv"
# site <- "constellation"
# file <- "constellation_2017_2019_all_sau.csv"
# site <- "Giesen_yld_data"
# file <- "Giesen_yld_data.csv"
# site <- "yealands"
# file <- "Updated_Yealands_jax.csv"
# site <- "Wine_portfolio"
# file <- "Wine_portfolio_yld_GPS_only_SAB.csv"
# site <- "rob_agnew"
# file <- "Yld_GPS_Rob_Agnew_GPS_SAB_select_sites.csv"
# site <- "cloudy_bay"
# file <- "yld_spatial_cloudy_bay_2004_19.csv"
# site <- "weta"
# file <- "weta_yld_data.csv"
# site <- "grower_coop"
# file <- "grower_coop_V2014_to_2019.csv"
# site <- "babich"
# file <- "babich_13_08_2020.csv"
 # site <- "TWE"
 # file <- "TWE_30_06_2021.csv"
 # site <- "wither_hills"
 # file <- "wither_hills_GPS_block_info_harvest_sau_v1.csv"
 # site <- "forrest"
 # file <- "Forrest_08_2020.csv"
 site <- "indevin"
 file <- "Indevin_02_07_2021.csv"


read_and_tidy_FUN <- function(file) {
#1. read the data in
  df <-
    read_csv(paste0("V:/Marlborough regional/working_jaxs/July2020/",file))
  # just for wither hills  add ID_yr clm
  # df <- df %>% 
  #   mutate(ID_yr = paste0(ID, "_", year))
  # names(df)
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
                      company,
                      na_count)

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
}
 
  ######

df <-
  lapply(file, read_and_tidy_FUN) %>% #this call the function read_csv_FUN I made outside this import_function
  bind_rows()

#########################################################################################
### remove_low_high_values 
#d. relace 0 values with NA
df %>% 
  replace_with_na_at(.vars = c("brix","yield_t_ha", "bunch_weight", "berry_weight"),
                     condition = ~.x == 0) 
df %>% 
    replace_with_na_at(.vars = c("julian"),
                       condition = ~.x < 2) 
df %>% 
  replace_with_na_at(.vars = c("brix"),
                     condition = ~.x > 100)
df %>% 
  replace_with_na_at(.vars = c("bunch_weight"),
                     condition = ~.x > 500)

#d. set clm as certain data types
df$harvest_date <- as.Date(df$harvest_date,
                           origin = "1970-01-01") #this is the starting date value in R

### using the site name to write out the df
assign(paste("site_",site),df)

##########################################################################################



##02/07/2021 - Rob has given me 2 extra sites - so I will be a bit lazey
#All_sites <- read_csv ("V:/Marlborough regional/working_jaxs/July2020/All_sites_june2021.csv")






##############################################################################################################
############             graph and check by site check               ##########################################
#############################################################################################################

#install.packages("plotly")
library(plotly)

df$na_count_factor <- as.factor(df$na_count)
df$year_factor <- as.factor(df$year)


str(df)
dim(df)
unique(df$company)
variety <- unique(df$variety)
variety
df <- filter(df, x_coord > 0)

with(df,  table(  year, company))
dim(df)

ggplot(df, aes(na_count_factor, na_count))+
  geom_col()+
  theme_bw()+
  facet_wrap(.~ year)+
  labs(x = "count of missing data entries",
       y= paste0("Total counts of missing data entries -", variety))
#what is the missing data?

df <- df %>%  filter(!is.na(year_factor))

#1. Harvest date juilan days 

Julian <- ggplot( df, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "Julian days")
Julian_plotly <- ggplotly(Julian)
Julian_plotly
Julian
#how much data is missing for julian days?
missing_data <- df %>%
  group_by(year ) %>% 
  summarise(count = sum(is.na(julian)))  
missing_data
#2. yield_t_ha 

# yield_t_ha <- ggplot( site_Jan2020_to_plot, aes(year_factor, yield_t_ha))+
yield_t_ha <- filter(df, yield_t_ha <100 ) %>% 
  ggplot(  aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "yield t/ha")
yield_t_ha_plotly <- ggplotly(yield_t_ha)
yield_t_ha_plotly
yield_t_ha
#how much data is missing for ?
missing_data <- df %>%
  group_by(year ) %>% 
  summarise(count = sum(is.na(yield_t_ha)))  
missing_data


#3.  yield_kg_m 

yield_kg_m <- ggplot( df, aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "yield kg/m")
yield_kg_m_plotly <- ggplotly(yield_kg_m)
yield_kg_m_plotly
yield_kg_m
#how much data is missing for ?
missing_data <- df %>%
  group_by(year ) %>% 
  summarise(count = sum(is.na(yield_kg_m)))  
missing_data

#4.  brix 

brix <- ggplot( df, aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "brix")
brix_plotly <- ggplotly(brix)
brix_plotly
brix
#how much data is missing for ?
missing_data <- df %>%
  group_by(year ) %>% 
  summarise(count = sum(is.na(brix)))  
missing_data

#5. bunch_weight 

bunch_weight <- ggplot( df, aes(year_factor, bunch_weight))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "Bunch weight")
bunch_weight_plotly <- ggplotly(bunch_weight)
bunch_weight_plotly
bunch_weight
#how much data is missing?
missing_data <- df %>%
  group_by(year ) %>% 
  summarise(count = sum(is.na(bunch_weight)))  
missing_data

#6. berry_weight 

df$berry_weight <- as.double(df$berry_weight)

berry_weight <- filter(df, berry_weight < 10) %>% 
#berry_weight <-  ggplot( df, aes(year_factor, berry_weight))+
  ggplot(  aes(year_factor, berry_weight))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "Berry weight")
berry_weight
berry_weight_plotly <- ggplotly(berry_weight)
berry_weight_plotly

#how much data s?
missing_data <- df %>%
  group_by(year ) %>% 
  summarise(count = sum(is.na(berry_weight)))  
missing_data



#6. bunch_numb_m 

df$berry_weight <- as.double(df$berry_weight)

bunch_numb_m <-  ggplot( df, aes(year_factor, bunch_numb_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "",
       y= "bunch numb m")
bunch_numb_m_plotly <- ggplotly(bunch_numb_m)
bunch_numb_m_plotly
bunch_numb_m
#how much data s?
missing_data <- df %>%
  group_by(year ) %>% 
  summarise(count = sum(is.na(bunch_numb_m)))  
missing_data



graph <-  plot_grid(Julian, yield_t_ha, yield_kg_m , brix, bunch_weight, berry_weight, bunch_numb_m,
                      #labels = c("A", "B", "C", "D", "E", "F"),
                      ncol = 2, nrow = 3)

graph


assign(paste("graph_",site),graph)


graph_path <- file.path("//FSSA2-ADL/CLW-SHARE3/Viticulture/Marlborough regional/working_jaxs/July2020/graphs")
ggsave(path= graph_path, filename = paste0("graph_",site, ".png"), device = "png" ,
       width = 20, height = 18, units = "cm")




  
  rm(list = c("Julian", 
              "yield_t_ha", 
              "yield_kg_m" , 
              "brix", 
              "bunch_weight", 
              "berry_weight", 
              "bunch_numb_m", "graph", "df"))
  
 
  rm(list = c("Julian_plotly", 
              "yield_t_ha_plotly", 
              "yield_kg_m_plotly" , 
              "brix_plotly", 
              "bunch_weight_plotly", 
              "berry_weight_plotly", 
              "bunch_numb_m_plotly"))
  
ls(pattern = "site_")

`site_ constellation`$pruning_style <- as.character(`site_ constellation`$pruning_style )
`site_ delegates`$pruning_style <- as.character(`site_ delegates`$pruning_style)
`site_ Giesen_yld_data`$pruning_style <- as.character(`site_ Giesen_yld_data`$pruning_style)
`site_ grower_coop`$pruning_style <- as.character(`site_ grower_coop`$pruning_style)
`site_ pernod_ricard`$pruning_style <- as.character(`site_ pernod_ricard`$pruning_style)
`site_ rob_agnew`$pruning_style <- as.character(`site_ rob_agnew`$pruning_style)
`site_ Villia_maria`$pruning_style <- as.character(`site_ Villia_maria`$pruning_style)
`site_ weta`$pruning_style <- as.character(`site_ weta`$pruning_style)
`site_ whitehaven`$pruning_style <- as.character(`site_ whitehaven`$pruning_style)
`site_ Wine_portfolio`$pruning_style <- as.character(`site_ Wine_portfolio`$pruning_style)
`site_ wither_hills`$pruning_style <- as.character(`site_ wither_hills`$pruning_style )
`site_ yealands`$pruning_style <- as.character(`site_ yealands`$pruning_style)
`site_ TWE`$pruning_style <- as.character(`site_ TWE`$pruning_style)
`site_ forrest`$pruning_style <- as.character(`site_ forrest`$pruning_style)
`site_ indevin`$pruning_style <- as.character(`site_ indevin`$pruning_style)

`site_ constellation`$na_count <- as.character(`site_ constellation`$na_count )
`site_ delegates`$na_count <- as.character(`site_ delegates`$na_count)
`site_ Giesen_yld_data`$na_count <- as.character(`site_ Giesen_yld_data`$na_count)
`site_ grower_coop`$na_count <- as.character(`site_ grower_coop`$na_count)
`site_ pernod_ricard`$na_count <- as.character(`site_ pernod_ricard`$na_count)
`site_ rob_agnew`$na_count <- as.character(`site_ rob_agnew`$na_count)
`site_ Villia_maria`$na_count <- as.character(`site_ Villia_maria`$na_count)
`site_ weta`$na_count <- as.character(`site_ weta`$na_count)
`site_ whitehaven`$na_count <- as.character(`site_ whitehaven`$na_count)
`site_ Wine_portfolio`$na_count <- as.character(`site_ Wine_portfolio`$na_count)
`site_ wither_hills`$na_count <- as.character(`site_ wither_hills`$na_count )
`site_ yealands`$na_count <- as.character(`site_ yealands`$pruning_style)
`site_ TWE`$na_count <- as.character(`site_ TWE`$pruning_style)
`site_ forrest`$na_count <- as.character(`site_ forrest`$pruning_style)
`site_ indevin`$na_count <- as.character(`site_ indevin`$pruning_style)

All_sites <- bind_rows(
  `site_ babich`,
  `site_ cloudy_bay`,
  `site_ constellation` ,
  `site_ delegates`,
  `site_ Giesen_yld_data`,
  `site_ grower_coop`,
  `site_ pernod_ricard` ,
  `site_ rob_agnew` ,
  `site_ Villia_maria` ,
  `site_ weta`,
  `site_ whitehaven`,
  `site_ Wine_portfolio` ,
  `site_ wither_hills`,
  `site_ yealands`,
  `site_ TWE`, 
  `site_ forrest`,
  `site_ indevin`
)

#All_sites <- bind_rows(All_sites, `site_ forrest`)

unique(All_sites$company) 
#do the count thing again

All_sites$na_count <- apply(is.na(All_sites), 1, sum)
# there is a problem with Yealand seaview coordinates (I think Rob made a mistake).
#ID_yr = Seaview_J2_2018
str(All_sites)
All_sites$y_coord <- as.double(All_sites$y_coord)
All_sites$x_coord <- as.double(All_sites$x_coord)

All_sites <- All_sites %>% 
  mutate(
    y_coord  = case_when(
      
      ID_yr == "Seaview_J2_2018" ~ 5388746.638369999800000,
      TRUE ~ y_coord
    ))
All_sites <- All_sites %>% 
  mutate(
    x_coord  = case_when(
      ID_yr == "Seaview_J2_2018" ~ 1694942.457269999900000,
      TRUE ~ x_coord
    ))
#x_coord = 1694942.457269999900000
#y_coord =5388746.638369999800000

write.csv(All_sites, "V:/Marlborough regional/working_jaxs/July2020/All_sites_june2021.csv")
names(All_sites)

All_sites_2014_2019 <- All_sites %>% 
  filter(between(year,2014, 2019))
write.csv(All_sites, "V:/Marlborough regional/working_jaxs/July2020/All_sites_2014_2019_june2021.csv")


#with(site_Jan2020,  table(company, year))
site_table_yr <- with(filter(All_sites_2014_2019,  x_coord >0), table(company, year))
site_table_yr
write.csv(site_table_yr, "V:/Marlborough regional/working_jaxs/July2020/site_table_yr_june2021.csv")

All_sites_2014_2019$year_factor <- as.factor(All_sites_2014_2019$year)
#julian days
ggplot(All_sites_2014_2019, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
ggplot(All_sites_2014_2019, aes(year_factor, julian, colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")+
  facet_wrap(.~ company)

#yield_t_ha
ggplot(All_sites_2014_2019, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")
#yield_t_ha
filter(All_sites_2014_2019,yield_t_ha > 2 & yield_t_ha < 100 ) %>% 
  ggplot( aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")

#yield_t_ha
filter(All_sites_2014_2019,yield_t_ha > 2 & yield_t_ha < 100) %>% 
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
ggplot(All_sites_2014_2019, aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield kg/m - Sauvignon Blanc")


#yield_km_m
filter(All_sites_2014_2019,yield_kg_m > 0) %>% 
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

filter(All_sites_2014_2019,brix < 100) %>% 
  ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


filter(All_sites_2014_2019,brix < 100) %>% 
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
ggplot( All_sites_2014_2019, aes(year_factor, bunch_weight))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Bunch weight - Sauvignon Blanc")

ggplot(All_sites_2014_2019, aes(year_factor, bunch_weight , colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Bunch weight  - Sauvignon Blanc")+
  facet_wrap(.~ company)



All_sites_2014_2019$berry_weight <- as.double(All_sites_2014_2019$berry_weight)

filter(All_sites_2014_2019,berry_weight < 20) %>% 
ggplot( aes(year_factor, berry_weight))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Berry weight - Sauvignon Blanc")

filter(All_sites_2014_2019,berry_weight < 20) %>% 
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

ggplot( All_sites_2014_2019, aes(year_factor, bunch_numb_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Bunch number per m - Sauvignon Blanc")

ggplot(All_sites_2014_2019, aes(year_factor, bunch_numb_m , colour= company))+
  geom_boxplot(alpha=0.1)+
  geom_point( alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x = "Year",
       y= "Bunch number per m  - Sauvignon Blanc")+
  facet_wrap(.~ company)

