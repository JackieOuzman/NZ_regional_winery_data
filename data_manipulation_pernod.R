library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
#########################################################################################################################
##############################           GPS POINTS for blocks #########################################################
#########################################################################################################################

perno_GPS_temp <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod Ricard Trought BX BchWT 2008 -2018 Co Marl.xlsx", 
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


#########################################################################################################################
##############################           Yield data             #########################################################
#########################################################################################################################



#Bring in the yld data and add to coordiates 
perno_yld <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod Ricard Trought BX BchWT 2008 -2018 Co Marl.xlsx", 
                             sheet = "Block Yield reference") 
#perno_yld <- read_excel("C:/Users/ouz001/NZ_work/test/Pernod Ricard Trought BX BchWT 2008 -2018 Co Marl.xlsx" ,
#                        sheet = "Block Yield reference")

perno_yld <- select(perno_yld,
                    ID_temp = `VendorBlock Key` ,
                    Vnd = `Vendor Code`,
                    Block = `Block Ref Number`,
                    Yr = `Vintage`,
                    brix = `Brix at Harvest`,
                    yield_kg_per_m = `Harvested KG per M`) %>% 
  mutate(ID = paste0(Vnd,"_", Block),
         year = as.double(paste0(Yr+2000)))
#tidy this up
perno_yld <- perno_yld %>% 
  select(ID, yield_kg_per_m, brix, year)

###########################             Join yield data to the GPS data     ########################
glimpse(perno_GPS_distinct)#372
glimpse(perno_yld) # 4,112
perno_yld_coord <- left_join(perno_yld, perno_GPS_distinct, by =  "ID")
glimpse(perno_yld_coord) # 4,112


#what was not joined and what variety was it?
#perno_yld_coord_anti_join <- anti_join(perno_yld, perno_GPS_distinct, by =  "ID")
#for what was not joined add the variety 264 - includes all the years
#perno_yld_coord_anti_join <- perno_yld_coord_anti_join %>% 
#  separate( ID, into = c("vendor", "variety"), sep = "_", remove = FALSE )

#just get the unquie blocks 123 blocks
#perno_yld_coord_anti_join <- distinct(perno_yld_coord_anti_join, ID, .keep_all = TRUE )

#how many of these block that didnt get coord are SBLB blocks?
#perno_yld_coord_anti_join_variety <- perno_yld_coord_anti_join %>% 
#  group_by(variety) %>% 
#  summarise(perno_yld_coord_anti_join =n())

#######################         Create a new clm for ID_year          ######################################
perno_yld_coord <- perno_yld_coord %>% 
  mutate(ID_yr = paste0(ID,"_", year))

#perno_yld_coord <- perno_yld_coord %>% 
#  filter(x_coord > 0) %>%  #this was removing the data that had missing GPS coordinates - now keeping
#  mutate(ID_yr = paste0(ID,"_", year))

glimpse(perno_yld_coord)

#########################################################################################################################
##############################           maturity data          #########################################################
#########################################################################################################################



perno_maturity <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Pernod Ricard Trought BX BchWT 2008 -2018 Co Marl.xlsx", 
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
### Create two files one with Brix and one with Bunch wt
### only need the bunch wt - brix was captured at harvest
#perno_maturity_brix <- perno_maturity %>% 
#                        filter(brix_bunch_wt_name == 'Brix')
perno_maturity_Bunch_wt_g <- perno_maturity %>% 
                              filter(brix_bunch_wt_name == 'Bunch wt g')
### what is the max sampling date
#glimpse(perno_maturity_brix)
#perno_maturity_brix <- perno_maturity_brix %>% 
#  group_by(ID_yr) %>% 
#  summarise(#max_date    = max(sample_date),
            #Vnd         = max(Vnd),
            #Block       = max(Block),
            #brix_check       = max(brix_bunch_wt_name),
#            brix_results       = max(brix_bunch_wt_results),
            #year        = max(year),
#            sample_date = max(sample_date),
#            ID          = max(ID ))

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
## now join the brix and berry wt data
#perno_maturity1 <- left_join(perno_maturity_brix, perno_maturity_Bunch_wt_g, by= "ID_yr")


perno_maturity1 <- perno_maturity1 %>% 
  select(ID_yr, sample_date,  bunch_wt_g = bunch_wt_g_results)


## now join the GPS data
glimpse(perno_maturity1) #3985
glimpse(perno_yld_coord) #4112

pernod_ricard <- left_join(perno_yld_coord, perno_maturity1, by = "ID_yr")
glimpse(pernod_ricard) # 4112

#pernod_ricard_na <- pernod_ricard %>% 
#  filter(is.na (x_coord) )
#pernod_ricard <- pernod_ricard %>% 
#  filter(!is.na (x_coord) )
#glimpse(pernod_ricard)
#glimpse(pernod_ricard_na)



##### Julian days
#pernod_ricard <- pernod_ricard %>% 
#mutate(julian = as.numeric(format(pernod_ricard$sample_date, "%j")))


##### need to convert trellis to canes - need trellis first

pernod_ricard <- mutate(pernod_ricard,
                             number_canes =  case_when(
                               trellis == "1CN" ~ "1",
                               trellis == "2CE" ~ "2",
                               trellis == "2CN" ~ "2",
                               trellis == "3CN" ~ "3",
                               trellis == "4CN" ~ "4",
                               trellis == "ARC" ~ "2",
                               trellis == "GDC" ~ "4",
                               trellis == "SCH" ~ "4",
                               trellis == "SPR" ~ "0",
                               trellis == "SYL" ~ "4",
                               trellis == "VSP" ~ "4",
                               trellis == "YVT" ~ "1",
                               TRUE ~ trellis),
                               number_canes = as.double(number_canes))
glimpse(pernod_ricard)

#########################################################################################################################
##############################           berry wt info data          #########################################################
#########################################################################################################################




##############################           2011 berry wt info data          ####################################################
#perno_berryWt_2011 <- read_excel("C:/Users/ouz001/NZ_work/Trought bry wt and number Co Marl.xlsx" ,
#                             sheet = "2011 data")
# read in the data
perno_berryWt_2011 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Trought bry wt and number Co Marl.xlsx" ,
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
perno_berryWt_2016 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Trought bry wt and number Co Marl.xlsx" ,
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
perno_berryWt_2017 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Trought bry wt and number Co Marl.xlsx" ,
                                 sheet = "2017 data")
perno_berryWt_2017 <- perno_berryWt_2017 %>% 
  separate(X__1  , into = c("vendor_text", "vendor_numb","variety"), 
           sep = "(?<=[A-Za-z])(?=[0-9])|(?<=[0-9])(?=[A-Za-z])", remove = FALSE, extra = "merge") %>% 
  mutate(ID_yr = paste0(vendor_text, vendor_numb,"_", variety,"_",Vintage)) %>% 
  select(ID_temp = X__1  ,
         ID_yr, year = Vintage,
         berry_weight_g = `Av berry wgt (g)` )

##############################           2018 berry wt info data          #################################################### 

perno_berryWt_2018 <- read_excel("V:/Marlborough regional/Regional winery data/Raw_data/Trought bry wt and number Co Marl.xlsx" ,
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

#### join it to data with coods pernod_ricard
glimpse(perno_berryWt_all) #763
perno_berryWt_all <- perno_berryWt_all %>% 
  select(ID_yr, berry_weight_g)



glimpse(pernod_ricard)#4112 maturity data with coods
glimpse(perno_berryWt_all) #763 berry wts for subet of years - data supplied

pernod_ricard1 <- left_join(pernod_ricard,perno_berryWt_all, by= "ID_yr") %>% 
  mutate(company = "pernod_ricard",
         yield_t_ha = NA,
         bunch_numb_m = NA, 
         bunch_mass_g = NA,
         berry_bunch = NA,
         row_width = NA,
         vine_spacing = NA,
         bunch_numb_m = NA,
         ID_temp = ID,
         variety = Variety,
         harvest_date = NA,
         julian = NA,
         yield_kg_m = yield_kg_per_m
         )
glimpse(pernod_ricard1) #4135



pernod_ricard1 <- pernod_ricard1 %>% 
  select(company, ID_temp, ID_yr, variety , x_coord, y_coord,
                year , harvest_date, julian,
                yield_t_ha, yield_kg_m,
                brix,bunch_weight = bunch_wt_g, berry_weight = berry_weight_g,
         row_width, vine_spacing, bunch_numb_m,
                pruning_style = trellis)

glimpse(pernod_ricard1)

pernod_ricard1$na_count <- apply(is.na(pernod_ricard1), 1, sum)
#write_csv(pernod_ricard1, "pernod_ricard_april_2019.csv")


glimpse(pernod_ricard1)



######################################################################################################################
################                         view and summaries DF                             #################
######################################################################################################################


dim(pernod_ricard1)
#how many site?
dim(pernod_ricard1)
glimpse(pernod_ricard1) #4135 records
max(pernod_ricard1$year) #2008 -2018

#how many sites
glimpse(perno_GPS_distinct)#372 records
filter(perno_GPS_distinct, Variety == "Sauvignon Blanc")

####up to here



#create a new variable year_as_factor
pernod_ricard1$year_factor <- as.factor(pernod_ricard1$year)

filter(pernod_ricard1, variety == "Sauvignon Blanc") %>%
  ggplot(aes(year_factor,variety))+
  geom_count()

  ggplot(pernod_ricard1, aes(year_factor))+
  geom_bar()+
    facet_wrap(~variety)

# how many vineyards?
number_vineyards <- filter(white_haven_2019to2014_GPS, year == 2019) %>% 
  distinct(Vineyard)
glimpse(number_vineyards)
# how many vineyards - not sure this is correct
number_blocks <-filter(white_haven_2019to2014_GPS, year == 2019) %>% 
  distinct(Block)
glimpse(number_blocks)

ggplot(white_haven_2019to2014_GPS, aes(year, julian))+
  geom_point()
#geom_boxplot()



#julian days
ggplot(white_haven_2019to2014_GPS, aes(year_factor, julian))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Julian days")
#yield_t_ha
ggplot(white_haven_2019to2014_GPS, aes(year_factor, yield_t_ha))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Yield t/ha")

#brix
ggplot(white_haven_2019to2014_GPS, aes(year_factor, brix))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  labs(x = "Year",
       y= "Brix")


ggplot(white_haven_2019to2014_GPS, aes(year_factor, na_count))+
  geom_col()+
  theme_bw()+
  labs(x = "Year",
       y= "Total counts of missing data entries NA")

ggplot(white_haven_2019to2014_GPS, aes(na_count))+
  geom_bar()+
  scale_x_continuous(breaks =  c(2,4,6,8,10))+
  facet_wrap(~year_factor)+
  theme_bw()+
  labs(x = "number of na counts per entry",
       y= "Counts of missing data entries NA")

