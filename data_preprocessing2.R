### SCRIPT TO ADD MORE INFORMATION TO DATA FILE ###

# setwd("~/Desktop/GMPD2") # Local location of files

### LIBRARIES ----
library(tidyverse)
library(readxl)

### PROCESS DATA ----

# Add host order and family, then resave data as 'shiny_data2.csv' 
threat_data <- read_excel(path = "data/ThreatPSRSoniaAmy2005.xlsx", sheet = sheet_names1[1]) %>%
  select(order = Order, hostFamily = 'Host family', HostCorrectedName = 'MSW Binomial') %>% # grab host order and family
  distinct()
dat <- read_csv("data/shiny_data.csv") %>%
  left_join(threat_data, by = "HostCorrectedName") %>%
  distinct() # keep unique records
write_csv(x = dat, path = "data/shiny_data2.csv") # save data as 'shiny_data2

