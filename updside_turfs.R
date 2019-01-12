library(tidyverse)
library(readxl)
library(tidyxl)
library(here)


#Load in upside projection data
projection_data <- readRDS(here("data","projection_data.rds")) %>% 
  filter(Scenario == "Historic")

#Load in GDP data
gdp_all <- read_excel(here("data","un_gdp_2016.xls"))

#Load EDF TURF data
edf_turf <- read_excel("data/EDF_TURF_data.xlsx")

#Load in DiscoverTURFs data
d_turfs <- read_excel("data/DiscoverTURFs_simple.xlsx")




gdp <- gdp_all %>%
  select(Country, gdp_center) %>%
  filter( gdp_center != "NA") 

join <- left_join(projection_data, gdp, by = "Country")
