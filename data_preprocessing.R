##############################################################################################################
######################################## setting working directory ###########################################
##############################################################################################################
setwd("~/Desktop/shiny_IDEAS_capstone/data")


##############################################################################################################
######################################## loading packages ####################################################
##############################################################################################################
packages <- c("tidyverse", "readxl", "Hmisc", "sp", "rworldmap")

lapply(packages, library, character.only = TRUE)

rm(packages)

##############################################################################################################
########################################### loading data #####################################################
##############################################################################################################

# dealing with the excel files that sonia shared -------------------------------------------------------------

sheet_names1 <- excel_sheets(path = "ThreatPSRSoniaAmy2005.xlsx")

threat_data <- read_excel(path = "ThreatPSRSoniaAmy2005.xlsx", sheet = sheet_names1[1])

#extract the MSW binomial name from threat data to join variables from the GMPD

threat_data %>% 
  select(`MSW Binomial`) %>% 
  unique() -> mammal_names 

# dealing with the GMPD file to extract information on other variables ---------------------------------------
features_of_interest <- c("HostCorrectedName", "Group", "HostFamily",
                          "Longitude", "Latitude", 
                          "PopulationType")


gmpd <- read_csv(file = "GMPD_main.csv")

#getting rid of all rows where either Longitude or Latitude was NA and selecting the features of interest
gmpd %>%
  filter(is.na(Longitude) == FALSE & is.na(Latitude) == FALSE) %>% 
  filter(HostCorrectedName %in% mammal_names$`MSW Binomial`)  %>% 
  select(features_of_interest) -> gmpd_subset
  
# Function to convert coordinates to country and continent name
coords2globe <- function(longitude_v, latitude_v) {
  
    # get the world map
    countriesSP <- getMap(resolution = "low")
  
    # gereate a data_frame of longitude and latitude
    points <- tibble(longitude = longitude_v, 
                     latitude = latitude_v)
  
    # setting CRS directly to that from rworldmap
    pointsSP <- SpatialPoints(points, proj4string = CRS(proj4string(countriesSP)))  

    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
  
    # generate either the country or continent name from these variables
    globe_r <- tibble(country = indices$ADMIN, continent = indices$REGION)
  
    return(globe_r)
  }
 
#defining a tibble of countries and continents
countries_continents <- coords2globe(longitude_v = gmpd_subset$Longitude, latitude_v = gmpd_subset$Latitude) 

#adding the columns of countries and continents to the gmpd_subset  
gmpd_subset_cc <- bind_cols(gmpd_subset, countries_continents) 

# intergrating values from the threat data into the gmpd subset
threat_data %>% 
  select(`MSW Binomial`, Threat, ThreatDic, ThreatDic2, ThreatDis) %>% 
  mutate(HostCorrectedName = `MSW Binomial`) %>% 
  select(-`MSW Binomial`) -> threat_subset

gmpd_cc_threat <- left_join(gmpd_subset_cc, threat_subset, by = "HostCorrectedName")


# NOTE:
# from among the 363 mammals presented in the threat data provided by sonia, only 283 
# mammal hosts are present in GMPD hence the reduction during the left_join(). 
# We can see that by executing the following lines of code

#length(unique(gmpd_cc_threat$HostCorrectedName))
#length(unique(threat_subset$HostCorrectedName))


# Dealing with Phylacine data --------------------------------------------------------------------------------
# generate a column of hostName as an ID with the phylacine data
gmpd_cc_threat %>% 
  mutate(hostName = gsub(pattern = " ", replacement = "_", x = HostCorrectedName)) -> gmpd_cc_threat

# read the joined GMPD + phylacine dataset
phy <- read_csv("script4.csv")

# filter out only those mammals that are present in gmpd_cc_threat 
# and summarise the values of the other variables over them 
phy %>%
  select(hostName, hostHomeRange, massKG, 
         groupSizePriUng, 
         groupSizeCar) %>%
  filter(hostName %in% unique(gmpd_cc_threat$hostName)) -> phy_subset 

phy_subset %<>% distinct(hostName, .keep_all = TRUE)

# NOTE: Here too, not all mammals present in gmpd_cc_threat data are present in the the phylacine dataset.
# run the following two codes to get an idea 

length(unique(gmpd_cc_threat$hostName))  
length(unique(phy_subset$hostName))  

# before joining the data, the gmpd_cc_threat data needs to be filtered to only contain mammals 
# that are contained within phy_subset 
gmpd_cc_threat %>% 
  filter(hostName %in% unique(phy_subset$hostName)) -> gmpd_cc_threat_subset


shiny_data <- left_join(gmpd_cc_threat_subset, phy_subset, by = "hostName")

# final edits to rename a few values 
shiny_data %>% 
  mutate(mean_hostHomeRange = hostHomeRange, 
         mean_massKG = massKG, 
         mean_groupSizePriUng = groupSizePriUng, 
         mean_groupSizePriUng_max = groupSizePriUng_max) %>% 
  select(-c(hostHomeRange, massKG, groupSizePriUng, groupSizePriUng_max)) -> shiny_data


# writing the data to use in a .csv file
write_csv(x = shiny_data, path = "shiny_data.csv")

# sanity check
test_the_written_data <- read_csv("shiny_data.csv")
# WORKS !!!







