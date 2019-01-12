library(tidyverse)
library(readxl)
library(tidyxl)
library(here)
library(dplyr)

#######
##Data#
#######

#upside projection data
projection_data <- readRDS(here("data","projection_data.rds")) %>% 
  filter(Scenario == "Historic")

#GDP data
gdp_all <- read_excel(here("data","un_gdp_2016.xls"))

#EDF TURF data
edf_turf <- read_excel("data/EDF_TURF_data.xlsx") %>%
  select("Country", "SciName","CatchShare") %>%
  filter(SciName != "NA")

#DiscoverTURFs data
d_turfs <- read_excel("data/DiscoverTURFs_simple.xlsx") %>%
  select("Country", "SciName", "CatchShare") %>%
  filter(SciName != "NA")

#ISSCAAP Codes
isscaap <- read_csv("data/meta_isscaap_codes.csv")
#######################################################

#get turf data to have species category codes

all_turfs <- plyr::join(d_turfs, edf_turf, by= c("Country", "SciName", "CatchShare"), type = "full" ) %>%
  distinct()

turfs <- plyr::join(all_turfs, isscaap, by= c("SciName")) %>%
  filter(SciName !="NA")%>%
  filter(SpeciesCat != "NA")

#add the turf data to upside
projection_turfs <- merge(projection_data, turfs, by = c("Country", "SciName", "SpeciesCat", "CatchShare"), all = T)

projection_turfs_mutate <- projection_turfs  %>%
  mutate(MainCat = case_when(
    SpeciesCat == 22 | SpeciesCat == 23 | SpeciesCat == 24 ~ "2",
    SpeciesCat == 31 | SpeciesCat == 32 | SpeciesCat == 33 | SpeciesCat == 34 | SpeciesCat     == 35 | SpeciesCat == 37 ~ "3",
    SpeciesCat == 42 | SpeciesCat == 43 | SpeciesCat == 44 | SpeciesCat == 45 | SpeciesCat      == 47 ~ "4",
    SpeciesCat == 52 | SpeciesCat == 53 | SpeciesCat ==54 |  SpeciesCat == 55 | SpeciesCat     == 56 | SpeciesCat == 57 | SpeciesCat == 58 ~ "5",
    SpeciesCat == 74 | SpeciesCat == 76 | SpeciesCat == 77 ~ "7"
  )) 


gdp_all <- read_excel(here("data","un_gdp_2016.xls"))

gdp <- gdp_all %>%
  select(Country, gdp_center) %>%
  filter( gdp_center != "NA") 

#question here, will the filter remove out when year is NA? the turf data doesn't have this resolution; edf does have start date though
join_mutate <- left_join(projection_turfs_mutate, gdp, by = "Country") %>% 
  filter(Year == max(Year))














