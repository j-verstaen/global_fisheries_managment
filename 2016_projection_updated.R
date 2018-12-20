
#Loading packages

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyr)
  library(here)
  library(ggplot2)
  library(dplyr)
  library(readxl)
  library(devtools)
  library(plotly)
  library(base)
  library(rio)
  library(plyr)
  library(data.table)
})

Countries_ITQ <- read_excel("data/Countries_ITQ.xlsx")

#new dataframw with just most recent year of data for each fishery
corbette_recent <- setDT(Countries_ITQ)[,.SD[which.max(year)],keyby=assess_id_short]

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
projection_data <- readRDS("data/projection_data.rds")

#RAMS
load("data/DBdata-model_fits_included.RData")


#RAM Data Tiddying
ram <- timeseries_values_views %>%
  select("stocklong", "year", "TBbest", "TCbest", "BdivBmsypref", "UdivUmsypref") %>%
  add_column("RAM")

colnames(ram) <- c( "CommName", "Year", "Biomass", "Catch", "BvBmsy", "FvFmsy", "DBase")

#Update Projection with RAM
projection_updated <- join(ram, projection_data, by = c("CommName", "Year"), type = "full")
##note: RAM data has some fisheries not in the projection data, thus adding new fisheries to the upside projection data

#separate and unit functions to create a shorter version of the stock assessment ID to merge with corbette ITQ/TURF data later
separate_projection <- projection_updated%>% 
  separate(IdOrig, c("agency", "stock", "t0", "tf", "data_enterer"))

unite_projection <- separate_projection %>% 
  unite(assess_id_short, agency, stock, sep = "-")

colnames(fisheries_with_itq) <- c("assess_id_short", "Year","SpeciesCat" , "iq", "itq", "ivq", "coop", "turf")

#merge corbette's data on ITQ/Turf implemented or not for each fishery
fishery_itq_simple <- fisheries_with_itq %>%
  select(assess_id_short,iq, itq, ivq, turf )

ITQ_projection_merge <- merge(unite_projection, fisheries_with_itq, by = c("assess_id_short"), all = TRUE)

#filter out data post 2016 because we know that those are projected/modeled data not recorded
recent_pre2016 <- ITQ_projection_merge %>%
  filter(Year.x <= "2016")

#keep only info on most recent year for each fishery  
fisheries_recent_1 <- setDT(recent_pre2016)[,.SD[which.max(Year.x)],keyby=assess_id_short]

#select the columns we are interested in
fisheries_recent <- fisheries_recent_1 %>%
  select(Country, assess_id_short, Year.x, CommName, Biomass, Catch, BvBmsy, FvFmsy, Dbase, SciName, IdLevel, SpeciesCat.x, Profits, MSY, Price, g, k, c, phi, itq, ivq, iq, turf)

#write dataframe to csv file
#fisheries_recent <- write.csv(fisheries_recent, file = "fisheries_recent.csv")

