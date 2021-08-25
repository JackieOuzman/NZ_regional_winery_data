library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
#########################################################################################################################
##############################           GPS POINTS for blocks #########################################################
#########################################################################################################################

# perno_GPS_temp <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Pernod Ricard Trought BX BchWT 2008 -2018 Co Marl.xlsx", 
#                                  sheet = "Block Location reference")
perno_GPS_temp <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Pernod Ricard BX BchWT 2008 -2018 Co Marl with H Dates_v2.xlsx", 
                             sheet = "Block Location reference")



#perno_GPS_temp <- read_excel("C:/Users/ouz001/NZ_work/Trought BX BchWT 2008 -2018 Co Marl.xlsx", 
#                             sheet = "Block Location reference")
glimpse(perno_GPS_temp)
perno_GPS <- select(perno_GPS_temp,
                    ID_temp = `VendorBlock Key` ,
                    x_temp = `NZGD2000 Centroid Easting`,
                    y_temp = `NZGD2000 Centroid Nrthing`,
                    Vnd,
                    Block,
                    Yr,
                    Variety,
                    trellis =`Trellis Code`) %>% 
  mutate(ID = paste0(Vnd,"_", Block),
         year = as.double(paste0(Yr+2000)))
glimpse(perno_GPS)
#make 2 files one with GPS pts and one without
perno_GPS_no_coord <- filter(perno_GPS, is.na(x_temp))
perno_GPS_coord <- filter(perno_GPS, !is.na(y_temp))
###Create a unquie list of blocks ###
perno_GPS_distinct_no_coords <- distinct(perno_GPS_no_coord, ID, .keep_all = TRUE)
perno_GPS_distinct <- distinct(perno_GPS_coord, ID, .keep_all = TRUE)
glimpse(perno_GPS_distinct_no_coords)


#summary of blocks with no coords
#variety_no_coords <- perno_GPS_distinct_no_coords %>% 
#                      group_by(Variety) %>% 
#                      summarise(number_blocks_no_coords =n())
#summary of blocks with with coords
#variety_coords <- perno_GPS_distinct %>% 
#  group_by(Variety) %>% 
#  summarise(number_blocks_with_coords =n())

###### Summary of blocks with and without coords ########
#glimpse(variety_no_coords)
#glimpse(variety_coords)
#Summary_blocks_coods <- full_join(variety_no_coords, variety_coords)
                      
#########################################################################################################################                      
#####            Fix up the decimal place problem with the x and y in df = perno_GPS_distinct
#####            Change the location of sites based on info from Mary
######################################################################################################################### 


#make a new cloumn that report the lenght of x and y
perno_GPS_distinct <- perno_GPS_distinct %>% 
  mutate(x_length = str_length(x_temp) ,
         y_length = str_length(y_temp))
#glimpse(perno_GPS_distinct)

#depending on the lenght of x and y /value as indicated below

perno_GPS_distinct <- perno_GPS_distinct %>% 
  mutate(x_coord = ifelse(x_length == 8, x_temp/10,
                   ifelse(x_length == 9, x_temp/100,
                   ifelse(x_length == 10, x_temp/1000, 
                   ifelse(x_length == 11, x_temp/10000,0)))),
         
         y_coord = ifelse(y_length == 8, y_temp/10,
                   ifelse(y_length == 9, y_temp/100,
                   ifelse(y_length == 10, y_temp/1000, 
                   ifelse(y_length == 11, y_temp/10000,0)))))  

#change the coordinates for these                        

#ID	          		x_coord	       	 	y_coord
#M18_SBAA	    	1665252.97		  5403846.94
#MV8_SBLAK	  	1664300.02005 	5405949.42973
#MV7_CHDC	    	1674120.96		  5407776.31
#M21_PNNE	    	1692719.51		  5389699.26
#MV7_CHDD     	1674066.57  		5407761.64 
#M14_SBLH    		1681656.44  		5405858.30 
#M16_SBLI     	1666891.79  		5406747.31 

perno_GPS_distinct <- mutate(perno_GPS_distinct,
                             x_coord =  case_when(
                               ID == "M18_SBAA" ~ 1665252.97,
                               ID == "MV8_SBLAK" ~ 1664300.02005,
                               ID == "MV7_CHDC" ~ 1674120.96,
                               ID == "M21_PNNE" ~ 1692719.51,
                               ID == "MV7_CHDD" ~ 1674066.57,
                               ID == "M14_SBLH" ~ 1681656.44,
                               ID == "M16_SBLI" ~ 1666891.79, 
                               TRUE ~ x_coord),
                             y_coord =  case_when(
                               ID == "M18_SBAA" ~ 5403846.94,
                               ID == "MV8_SBLAK" ~ 5405949.42973,
                               ID == "MV7_CHDC" ~ 5407776.31,
                               ID == "M21_PNNE" ~ 5389699.26,
                               ID == "MV7_CHDD" ~ 5407761.64,
                               ID == "M14_SBLH" ~ 5405858.30,
                               ID == "M16_SBLI" ~ 5406747.31, 
                               TRUE ~ y_coord))

###The below is the list of sites with GPS coordinates
perno_GPS_distinct <- perno_GPS_distinct %>% 
  select(ID, x_coord, y_coord, Variety, Vnd, trellis)
glimpse(perno_GPS_distinct)
#write_csv(perno_GPS_distinct, path = "V:/Marlborough regional/working_jaxs/perno_GPS_test1.csv" )



rm(list = c("perno_GPS", "perno_GPS_coord", "perno_GPS_distinct_no_coords", "perno_GPS_no_coord", "perno_GPS_temp"))

#Rob has a few extra coordinates 

Extra_cood <- read.csv("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Extra_coordinates_obtained_Rob2021.csv")
mapCRS <- CRS("+init=epsg:2193")     # 2193 = NZGD2000 / New Zealand Transverse Mercator 2000 
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs
#proj4string(test) <- wgs84CRS   # assume input lat and longs are WGS84

coordinates(Extra_cood) <- ~long+lat
proj4string(Extra_cood) <- wgs84CRS   # assume input lat and longs are WGS84
Extra_cood_1 <- spTransform(Extra_cood, mapCRS)
Extra_cood_1_df = as.data.frame(Extra_cood_1) #this has the new coordinates projected !YES!!
#remove the duplication
Extra_cood_1_df <- Extra_cood_1_df %>% separate(ID_yr, c("step1", "step2"), sep = "_", remove = FALSE)

Extra_cood_1_df <- Extra_cood_1_df %>% mutate(ID = paste0(step1,"_", step2))

Extra_cood_1_df <- Extra_cood_1_df %>% dplyr::distinct(ID, .keep_all = TRUE)
names(perno_GPS_distinct)
names(Extra_cood_1_df)
Extra_cood_1_df <- Extra_cood_1_df %>% 
  rename( x_coord = long,
         y_coord  = lat) %>% 
  dplyr::select(ID,  x_coord, y_coord) 

Extra_cood_1_df <- Extra_cood_1_df %>% 
  mutate(Variety = "Sauvignon Blanc",
  Vnd = NA,
  trellis = NA,)

#join the newly GPS location with the older set
names(perno_GPS_distinct)
names(Extra_cood_1_df)
perno_GPS_distinct <- rbind(perno_GPS_distinct, Extra_cood_1_df)


rm(list=setdiff(ls(), "perno_GPS_distinct"))

#########################################################################################################################
##############################           Yield data  PART 1           #################################################
#########################################################################################################################



#Bring in the yld data and add to coordiates 
 perno_yld_kg_ha <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Pernod Ricard BX BchWT 2008 -2018 Co Marl with H Dates_v2.xlsx", 
                              sheet = "Block Yield reference") 


#perno_yld <- read_excel("C:/Users/ouz001/NZ_work/test/Pernod Ricard Trought BX BchWT 2008 -2018 Co Marl.xlsx" ,
#                        sheet = "Block Yield reference")

perno_yld_kg_ha <- select(perno_yld_kg_ha,
                    ID_temp = `VendorBlock Key` ,
                    Vnd = `Vendor Code`,
                    Block = `Block Ref Number`,
                    Yr = `Vintage`,
                    brix = `Brix at Harvest`,
                    yield_kg_per_m = `Harvested KG per M`) %>% 
  mutate(ID = paste0(Vnd,"_", Block),
         year = as.double(paste0(Yr+2000)))
## add in another ID clm        
perno_yld_kg_ha <- perno_yld_kg_ha %>% 
  mutate(ID_yr = paste0(ID,"_", year))
#tidy this up
perno_yld_kg_ha <- perno_yld_kg_ha %>% 
  select(ID, ID_yr, yield_kg_per_m, brix, year)
glimpse(perno_yld_kg_ha) # 4,112

unique(perno_yld_kg_ha$year)


#########################################################################################################################
##############################           Yield data now with t/ha PART 2    ############################################
#########################################################################################################################
perno_yld_t_ha <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Pernod Ricard BX BchWT 2008 -2018 Co Marl with H Dates_v2.xlsx", 
                        sheet = "Yield per ha") 

perno_yld_t_ha <- select(perno_yld_t_ha,
                    ID_temp = `VendorBlock Key` ,
                    Vnd = `Vendor Code`,
                    Block = `Block Ref Number`,
                    Yr = `Vintage`,
                    yield_t_per_ha = `Harvested T per HA`) %>% 
  mutate(ID = paste0(Vnd,"_", Block),
         year = as.double(paste0(Yr+2000)))
## add in another ID clm        
perno_yld_t_ha <- perno_yld_t_ha %>% 
  mutate(ID_yr = paste0(ID,"_", year))
#tidy this up
perno_yld_t_ha <- perno_yld_t_ha %>% 
  select(ID_yr, yield_t_per_ha)
glimpse(perno_yld_t_ha) # 3,740


glimpse(perno_yld_t_ha) #3740
glimpse(perno_yld_kg_ha) #4112

perno_yld_1 <- full_join(perno_yld_kg_ha, perno_yld_t_ha, by = "ID_yr")
glimpse(perno_yld_1) # 4112
unique(perno_yld_1$year)

#########################################################################################################################
##############################           Yield data now with harvest date PART 3    ############################################
#########################################################################################################################

perno_yld_harvest_date <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Pernod Ricard BX BchWT 2008 -2018 Co Marl with H Dates_v2.xlsx", 
                             sheet = "Harvest Dates") 

glimpse(perno_yld_harvest_date)

perno_yld_harvest_date <- select(perno_yld_harvest_date,
                         ID_temp = `VendorBlock Key` ,
                         Vnd = `Vendor Code`,
                         Block = `Block Ref Number`,
                         Yr = `Vintage`,
                         harvest_date = `Date harvested`,
                         ton_harvested = `Tons Harvested`) %>% 
  mutate(ID = paste0(Vnd,"_", Block),
         year = as.double(paste0(Yr+2000)))
## add in another ID clm        
perno_yld_harvest_date <- perno_yld_harvest_date %>% 
  mutate(ID_yr = paste0(ID,"_", year))
#tidy this up
perno_yld_harvest_date <- perno_yld_harvest_date %>% 
  select(ID, ID_yr, yield_t_per_ha, year)
glimpse(perno_yld_harvest_date) # 4,772

#need to group data by ID_Yr and then work out max yield and what date this occured this works but dont use the tonnes harvested as this is only for the max date harvested
perno_yld_harvest_date_max <- perno_yld_harvest_date  %>%
  group_by(ID_yr) %>%
  filter(row_number() == which.max(ton_harvested)) %>% 
  select(harvest_date, ID_yr)
glimpse(perno_yld_harvest_date_max)


glimpse(perno_yld_harvest_date_max) #3,993
glimpse(perno_yld_1) #4112

perno_yld <- full_join(perno_yld_1, perno_yld_harvest_date_max, by = "ID_yr")
glimpse(perno_yld) #4,112

##### Julian days
perno_yld <- perno_yld %>% 
mutate(julian = as.numeric(format(perno_yld$harvest_date, "%j")))
glimpse(perno_yld)
unique(perno_yld$year)
#########################################################################################################################
##############################           maturity data          #########################################################
#########################################################################################################################



perno_maturity <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Pernod Ricard BX BchWT 2008 -2018 Co Marl with H Dates_v2.xlsx", 
                             sheet = "Grape Sample results by date") 
#perno_maturity <- read_excel("C:/Users/ouz001/NZ_work/test/Pernod Ricard Trought BX BchWT 2008 -2018 Co Marl.xlsx" ,
#                        sheet = "Grape Sample results by date")
glimpse(perno_maturity)
perno_maturity <- select(perno_maturity,
                    ID_temp = `VendorBlock Key`,
                    Vnd = `Vendor Code`,
                    Block = `Block Ref Number`,
                    year = `Vintage Full Year`,
                    sample_date = `Sample Date`,
                    brix_bunch_wt_name = `Analysis Description`,
                    brix_bunch_wt_results = `Analysis Reading`)%>%  
                      mutate(ID = paste0(Vnd,"_", Block),
                      ID_yr = paste0(ID,"_", year))

perno_maturity_Bunch_wt_g <- perno_maturity %>% 
                              filter(brix_bunch_wt_name == 'Bunch wt g')


perno_maturity_Bunch_wt_g <- perno_maturity_Bunch_wt_g %>% 
  group_by(ID_yr) %>% 
  summarise(#max_date    = max(sample_date),
            #Vnd         = max(Vnd),
            #Block       = max(Block),
            #bunch_check       = max(brix_bunch_wt_name),
            bunch_wt_g_results       = max(brix_bunch_wt_results),
            #year        = max(year),
            sample_date = max(sample_date),
            ID          = max(ID ))


perno_maturity1 <- perno_maturity_Bunch_wt_g %>% 
  select(ID_yr,  bunch_wt_g = bunch_wt_g_results)


################## now join the yield data   #############################
glimpse(perno_maturity1) #3985
glimpse(perno_yld) #4112

pernod_ricard <- left_join(perno_yld, perno_maturity1, by = "ID_yr")
glimpse(pernod_ricard) # 4112
#remove the ID.x clm












#########################################################################################################################
##############################           berry wt info data          #########################################################
#########################################################################################################################




##############################           2011 berry wt info data          ####################################################
#perno_berryWt_2011 <- read_excel("C:/Users/ouz001/NZ_work/Trought bry wt and number Co Marl.xlsx" ,
#                             sheet = "2011 data")
# read in the data
perno_berryWt_2011 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Trought bry wt and number Co Marl.xlsx" ,
                                 sheet = "2011 data")

#create ID clm,select Av. berry weight
perno_berryWt_2011 <- perno_berryWt_2011 %>% 
  separate(`Vendor Block`, into = c("vendor_text", "vendor_numb","variety"), 
      sep = "(?<=[A-Za-z])(?=[0-9])|(?<=[0-9])(?=[A-Za-z])", remove = FALSE, extra = "merge") %>% 
      mutate(ID_yr = paste0(vendor_text, vendor_numb,"_", variety,"_",Vintage)) %>% 
      select(ID_temp =`Vendor Block`,
             ID_yr, year = Vintage,
             #sample_date = `Date sample taken`,
             berry_weight_g = `Av. Berry weight (g)` )
glimpse(perno_berryWt_2011)

##############################           2016 berry wt info data          ####################################################
#perno_berryWt_2016 <- read_excel("C:/Users/ouz001/NZ_work/Trought bry wt and number Co Marl.xlsx" ,
#                                 sheet = "2016 data")
perno_berryWt_2016 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Trought bry wt and number Co Marl.xlsx" ,
                                 sheet = "2016 data")
#glimpse(perno_berryWt_2016)  
perno_berryWt_2016 <- perno_berryWt_2016 %>% 
  separate(VendBlock, into = c("vendor_text", "vendor_numb","variety"), 
           sep = "(?<=[A-Za-z])(?=[0-9])|(?<=[0-9])(?=[A-Za-z])", remove = FALSE, extra = "merge") %>% 
  mutate(ID_yr = paste0(vendor_text, vendor_numb,"_", variety,"_",Vintage)) %>% 
  select(ID_temp = VendBlock,
         ID_yr, year = Vintage,
         berry_weight_g = `Av berry wgt (g)`)
glimpse(perno_berryWt_2016)

###Locate the controls in the ID_temp clm and remove####
#M14SBLD CONTROL
#M14SBLO CONTROL
#M14SBLR CONTROL
perno_berryWt_2016_remove_control <- perno_berryWt_2016 %>% 
  filter(ID_temp != "M14SBLD CONTROL",
         ID_temp != "M14SBLO CONTROL",
         ID_temp != "M14SBLR CONTROL")
##Aveage the reps
temp <- perno_berryWt_2016_remove_control %>% 
  mutate(rep = ifelse(ID_temp == "MV8PNNL#1", "MV8_PNNL",
               ifelse(ID_temp == "MV8PNNL#2", "MV8_PNNL", 
               ifelse(ID_temp == "M13RR1B#1", "M13_RR1B",
               ifelse(ID_temp == "M13RR1B#2", "M13_RR1B",
               ifelse(ID_temp == "M13RRIA#1", "M13_RRIA",
               ifelse(ID_temp == "M13RRIA#2", "M13_RRIA",
                             "")))))))
temp1 <- temp %>%
  group_by(rep) %>% 
  summarise(berry_weight_g = mean(berry_weight_g)) 
  
temp1 <- temp1 %>% 
  select(ID_temp = rep,
         berry_weight_g) %>% 
         mutate(ID_yr = paste0(ID_temp, "_", "2016"),
                year = "2016")%>% 
          filter(ID_temp > 0)
  
  
 glimpse(temp1)
temp2 <- temp %>% 
  filter(ID_temp != "MV8PNNL#1",
         ID_temp != "MV8PNNL#2", 
         ID_temp != "M13RR1B#1",
         ID_temp != "M13RR1B#2",
         ID_temp != "M13RRIA#1",
         ID_temp != "M13RRIA#2") %>% 
  select(-rep) 

glimpse(temp1)
glimpse(temp2)

perno_berryWt_2016 <- rbind(temp1,temp2 )
glimpse(perno_berryWt_2016) 

##############################           2017 berry wt info data          #################################################### 

#perno_berryWt_2017 <- read_excel("C:/Users/ouz001/NZ_work/Trought bry wt and number Co Marl.xlsx" ,
#                                 sheet = "2017 data")
perno_berryWt_2017 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Trought bry wt and number Co Marl.xlsx" ,
                                 sheet = "2017 data")
str(perno_berryWt_2017)
unique(perno_berryWt_2017$...1)

perno_berryWt_2017 <- perno_berryWt_2017 %>% 
  separate(`...1`  , into = c("vendor_text", "vendor_numb","variety"), 
           sep = "(?<=[A-Za-z])(?=[0-9])|(?<=[0-9])(?=[A-Za-z])", remove = FALSE, extra = "merge") %>% 
  mutate(ID_yr = paste0(vendor_text, vendor_numb,"_", variety,"_",Vintage)) %>% 
  select(ID_temp = `...1`  ,
         ID_yr, year = Vintage,
         berry_weight_g = `Av berry wgt (g)` )

##############################           2018 berry wt info data          #################################################### 

perno_berryWt_2018 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod_Ricard/Trought bry wt and number Co Marl.xlsx" ,
                                 sheet = "2018 data")

perno_berryWt_2018 <- perno_berryWt_2018 %>% 
  separate(VendorBlock   , into = c("vendor_text", "vendor_numb","variety"), 
           sep = "(?<=[A-Za-z])(?=[0-9])|(?<=[0-9])(?=[A-Za-z])", remove = FALSE, extra = "merge") %>% 
  mutate(ID_yr = paste0(vendor_text, vendor_numb,"_", variety,"_",Vintage)) %>% 
  select(ID_temp = VendorBlock   ,
         ID_yr, year = Vintage,
         berry_weight_g = `Berry Wt` )

##############################           2011, 2016, 2017, 2018 berry wt info data        ############################## 

glimpse(perno_berryWt_2011)
glimpse(perno_berryWt_2016)
glimpse(perno_berryWt_2017)
glimpse(perno_berryWt_2018)
perno_berryWt_all <- rbind(perno_berryWt_2011,perno_berryWt_2016,
                           perno_berryWt_2017, perno_berryWt_2018)
perno_berryWt_all <- perno_berryWt_all %>% 
  select(ID_yr, berry_weight_g)

#############################           berry wt info data with yld and maturity        ############################## 
glimpse(perno_berryWt_all) #763 berry wts for subet of years - data supplied
glimpse(pernod_ricard) #4112 maturity data and yld data

pernod_ricard1_a <- left_join(pernod_ricard,perno_berryWt_all, by= "ID_yr")
glimpse(pernod_ricard1_a)
unique(pernod_ricard1_a$year)
######################################################################################################################
################                         join df to GPS on ID not ID_yr                             #################
######################################################################################################################

glimpse(pernod_ricard1_a) #4135 maturity data and yld data and berry wts
glimpse(perno_GPS_distinct) #372 sites with GPS 

pernod_ricard1 <- left_join(pernod_ricard1_a,perno_GPS_distinct, by= "ID")
glimpse(pernod_ricard1) #4135

pernod_ricard1 <- mutate(pernod_ricard1,
         company = "pernod_ricard",
         yield_t_ha = yield_t_per_ha,
         bunch_numb_m = NA, 
         bunch_mass_g = NA,
         berry_bunch = NA,
         row_width = NA,
         vine_spacing = NA,
         bunch_numb_m = NA,
         variety = Variety,
         harvest_date,
         julian,
         yield_kg_m = yield_kg_per_m,
         trellis
         )
glimpse(pernod_ricard1) #4518

pernod_ricard1 <- pernod_ricard1 %>% 
  select(company, ID, ID_yr, variety , x_coord, y_coord,
                year , harvest_date, julian,
                yield_t_ha, yield_kg_m,
                brix,bunch_weight = bunch_wt_g, berry_weight = berry_weight_g,
         row_width, vine_spacing, bunch_numb_m,
                pruning_style = trellis)

glimpse(pernod_ricard1)
unique(pernod_ricard1$year)
##### need to convert trellis to canes - need trellis first

pernod_ricard1 <- mutate(pernod_ricard1,
                        number_canes =  case_when(
                          pruning_style == "1CN" ~ "1",
                          pruning_style == "2CE" ~ "2",
                          pruning_style == "2CN" ~ "2",
                          pruning_style == "3CN" ~ "3",
                          pruning_style == "4CN" ~ "4",
                          pruning_style == "ARC" ~ "2",
                          pruning_style == "GDC" ~ "4",
                          pruning_style == "SCH" ~ "4",
                          pruning_style == "SPR" ~ "0",
                          pruning_style == "SYL" ~ "4",
                          pruning_style == "VSP" ~ "4",
                          pruning_style == "YVT" ~ "1",
                          TRUE ~ pruning_style),
                        number_canes = as.double(number_canes))

pernod_ricard1$na_count <- apply(is.na(pernod_ricard1), 1, sum)

str(pernod_ricard1)
## recode the yld with NA not zeros...
pernod_ricard1 < mutate(pernod_ricard1,
                         brix =case_when(
                 brix == 	0.00000000 ~ NA_real_ ,
                 TRUE ~ brix))
## recode the yld with NA not zeros...
pernod_ricard1 <- mutate(pernod_ricard1,
                         yield_kg_m =case_when(
                           yield_kg_m == 	0.00000000 ~ NA_real_ ,
                           TRUE ~ yield_kg_m))


# only keep the SB Sauvignon Blanc and NA
unique(pernod_ricard1$variety)
pernod_ricard1 <- filter(pernod_ricard1,
               variety == "Sauvignon Blanc" |
               is.na(variety) )
pernod_ricard1 <- separate(pernod_ricard1, ID, into = c("temp1", "temp2"), remove = FALSE )

pernod_ricard1 <- pernod_ricard1 %>% 
  mutate(variety_check =  str_sub(pernod_ricard1$temp2, start = 1, end = 2))
#remove the clm I dont want and fill in the missing variety NA values

pernod_ricard1 <- mutate(pernod_ricard1,
               variety = case_when(is.na(variety) ~ variety_check ,
                                   TRUE ~ variety))
pernod_ricard1 <- mutate(pernod_ricard1,
               variety = case_when(variety == "SB"  ~ "Sauvignon Blanc" ,
                                   TRUE ~ variety))
pernod_ricard1 <- filter(pernod_ricard1,
               variety == "Sauvignon Blanc")
#drop a few temp clms
names(pernod_ricard1)
pernod_ricard1 <- dplyr::select(pernod_ricard1, -temp1, -temp2, -variety_check)

write_csv(pernod_ricard1, "pernod_ricard_april_2019.csv")


glimpse(pernod_ricard1)

View(pernod_ricard1)
## Rob want a few site removed because they are too old

filter_vals <- c(
"M10_SBLP_2008",
"M13_SBDY_2008",
"M13_SBLD_2008",
"M13_SBLK_2008",
"M16_SBAA_2008",
"MV2_SBI1_2008",
"MV2_SBLC_2008",
"MV2_SBLM_2009",
"MV4_SBL27_2010",
"MV6_SBLD_2008")


pernod_ricard1 <- filter(pernod_ricard1, !ID_yr  %in% filter_vals)


############################################################################## 



#Revised  set 21/0/2021
names(pernod_ricard1)

#just need to make a block 
#pernod_ricard1 <- pernod_ricard1 %>% separate(ID_yr, c("Block"), sep = "_", remove = FALSE)
pernod_ricard1 <- pernod_ricard1 %>% 
  rename(Block = ID)
#1. How many sites?
#for each year
pernod_ricard1 %>%
  group_by(year) %>%
  summarise(count = n_distinct(Block))
#overall for the data set from 2014-2019 how many blocks do we have?
pernod_ricard1 %>%
  summarise(count = n_distinct(Block))

#2. For harvest date how many sites per year?
#names(pernod_ricard1)

pernod_ricard1 %>%
  group_by(year) %>%
  summarise(mean_julian_days = mean(julian, na.rm = TRUE),
            min_julian_days = min(julian, na.rm = TRUE),
            max_julian_days = max(julian, na.rm = TRUE),
            sum_na = sum(!is.na(julian)))

#3. For yield kg/m  how many sites per year

pernod_ricard1 %>%
  group_by(year) %>%
  summarise(mean_yield_kg_m = mean(yield_kg_m, na.rm = TRUE),
            min_yield_kg_m = min(yield_kg_m, na.rm = TRUE),
            max_yield_kg_m = max(yield_kg_m, na.rm = TRUE),
            sum_na = sum(!is.na(yield_kg_m)))







######################################################################################################################
################                         view and summaries DF                             #################
######################################################################################################################


dim(pernod_ricard1)
#how many site?
dim(pernod_ricard1)
glimpse(pernod_ricard1) #4518 records
max(pernod_ricard1$year) #2008 -2019
summary(pernod_ricard1)


#how many sites with GPS pts
glimpse(perno_GPS_distinct)#372 records
#how many sites with GPS pts by Variety
ggplot(perno_GPS_distinct, aes(Variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites with GPS coordinates")
  
#how many sites by Variety by year
ggplot(pernod_ricard1, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")+
  facet_wrap(~year)

#how many sites by Variety
ggplot(pernod_ricard1, aes(variety))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))+
  labs(y = "Count of sites")
  


#create a new variable year_as_factor
pernod_ricard1$year_factor <- as.factor(pernod_ricard1$year)

#filter data for Sauvignon Blanc
pernod_ricard1_sau <- filter(pernod_ricard1, variety == "Sauvignon Blanc") 
glimpse(pernod_ricard1_sau)

#how many sites for Sauvignon Blanc by year
group_by(pernod_ricard1_sau, year) %>% 
  count()
#how many sites for Sauvignon Blanc have missing data - how much missing data?
ggplot(pernod_ricard1_sau, aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA - Sauvignon Blanc")
#how many sites for Sauvignon Blanc have missing data - missing data grouped together?
ggplot(pernod_ricard1_sau, aes(na_count))+
  geom_bar()+
  scale_x_continuous(breaks =  c(2,4,6,8,10))+
  facet_wrap(~year_factor)+
  theme_bw()+
  labs(x = "number of na counts per entry",
       y= "Counts of missing data entries NA")


glimpse(pernod_ricard1_sau)
#julian days
ggplot(pernod_ricard1_sau, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days - Sauvignon Blanc")
#yield_t_ha
ggplot(pernod_ricard1_sau, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")

#yield_t_ha filter out high value
filter(pernod_ricard1_sau, yield_t_ha <100 ) %>%
ggplot( aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha - Sauvignon Blanc")


#yield_kg_m
ggplot(pernod_ricard1_sau, aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")

#yield_kg_m filter out zeros
filter(pernod_ricard1_sau,yield_kg_m != 0 ) %>% 
ggplot( aes(year_factor, yield_kg_m))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "yield kg/m - Sauvignon Blanc")


#brix - too many zero
ggplot(pernod_ricard1_sau, aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


#brix - filter out zero
filter(pernod_ricard1_sau,brix != 0) %>% 
ggplot( aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix - Sauvignon Blanc")


############################################################################## 
########################    File to use   ####################################
pernod_ricard1_sau <- select(pernod_ricard1_sau, -year_factor)
glimpse(pernod_ricard1_sau)
write_csv(pernod_ricard1_sau, "V:/Marlborough regional/working_jaxs/July2020/pernod_ricard1_sau.csv")
##############################################################################   



