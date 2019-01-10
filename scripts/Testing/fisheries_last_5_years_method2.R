
#Loading packages
library(tidyverse)
library(tidyr)
library(here)
library(ggplot2)
library(dplyr)
library(readxl)
library(devtools)
library(plotly)
library(rio)
library(data.table)

Countries_ITQ <- read_excel("data/Countries_ITQ.xlsx")

#new dataframe with just most recent year of data for each fishery
corbette_recent <- Countries_ITQ %>% 
  group_by(assess_id_short) %>% 
  filter(year == max(year))

# setDT(Countries_ITQ)[,.SD[which.max(year)],keyby=assess_id_short]


# corbette_recent <- setDT(Countries_ITQ)[,.SD[which.max(year)],keyby=assess_id_short]

#filter out the fisheries with no data on itqs or turfs
corbette_fisheries <- corbette_recent %>%
  select(assess_id_short, year, speciescat, iq, itq, ivq, coop, turf)%>%
  filter(itq != "NA" & iq != "NA" & ivq != "NA")%>%
  filter(turf != "NA" ) 


#new data frame with just data on fisheries with itqs
fisheries_with_itq <- corbette_fisheries %>%
  filter(itq == "TRUE" | iq == "TRUE" | ivq == "TRUE" | turf == "TRUE" )
#Load Data
#Upside data
projection_data <- readRDS("data/projection_data.rds") %>% 
  filter(Scenario == "Historic") %>% 
  group_by(IdOrig) %>% 
  dplyr::mutate(max_year = max(Year)) %>% 
  filter(max_year == 2012)


#RAM
load("data/DBdata-model_fits_included.RData")


#RAM Data Tiddying
ram <- timeseries_values_views %>%
  select("stocklong", "year", "TBbest", "TCbest", "BdivBmsypref", "UdivUmsypref") %>%
  add_column("RAM")

colnames(ram) <- c( "CommName", "Year", "Biomass", "Catch", "BvBmsy", "FvFmsy", "DBase")

#Update Projection with RAM

ram_data <- ram %>% 
  left_join(projection_data %>% filter(Dbase == "RAM") %>% select(-BvBmsy,-FvFmsy,-Catch,-Biomass), by = c("CommName","Year")) %>% 
  select(colnames(projection_data))

# projection_updated <- dplyr::full_join(projection_data, ram, by = c("CommName", "Year"))

projection_updated <- projection_data %>% 
  filter(Dbase != "RAM") %>%
  bind_rows(ram_data) %>%
  group_by(IdOrig) %>% 
  dplyr::mutate(final_year = max(Year))

# 
# dplyr::full_join(projection_data, ram, by = c("CommName", "Year"))

##note: RAM data has some fisheries not in the projection data, thus adding new fisheries to the upside projection data

#separate and unit functions to create a shorter version of the stock assessment ID to merge with corbette ITQ/TURF data later
separate_projection <- projection_updated%>% 
  separate(IdOrig, c("agency", "stock", "t0", "tf", "data_enterer"), sep = '-')

unite_projection <- separate_projection %>% 
  unite(assess_id_short, agency, stock, sep = "-")

colnames(fisheries_with_itq) <- c("assess_id_short", "Year","SpeciesCat" , "iq", "itq", "ivq", "coop", "turf")

#merge corbette's data on ITQ/Turf implemented or not for each fishery
fishery_itq_simple <- fisheries_with_itq %>%
  select(assess_id_short,iq, itq, ivq, turf )

ITQ_projection_merge <- full_join(unite_projection, fisheries_with_itq %>% select(-Year), by = c("assess_id_short"))

#once made seperate columns for each 5 years need to only keep the ones where the "final" year matches the Year

##filter out the last 5 years of data
recent_pre2016_5years <- ITQ_projection_merge %>%
  dplyr::mutate(final_2year = (final_year-1)) %>%
  dplyr::mutate(final_3year = (final_year-2)) %>%
  dplyr::mutate(final_4year = (final_year-3)) %>%
  dplyr::mutate(final_5year = (final_year-4)) %>%
  filter(Year <= final_year | Year <= final_2year | Year <= final_3year | Year <= final_4year | Year <= final_5year) %>%

#keep only info on most recent year for each fishery  
fisheries_recent_5years_1 <- recent_pre2016_5years %>% 
  filter(Year == final_year & final_year > 2007)


#select the columns we are interested in
fisheries_recent_5years <- fisheries_recent_5years_1 %>%
  select(Country, assess_id_short, Year, CommName, Biomass, Catch, BvBmsy, FvFmsy, Dbase, SciName, IdLevel, SpeciesCat.x, Profits, MSY, Price, g, k, c, phi, itq, ivq, iq, turf) %>%
  filter(SpeciesCat.x != "11")

#write dataframe to csv file
#fisheries_recent_5years <- write.csv(fisheries_recent_5years, file = "fisheries_recent_5years.csv")



