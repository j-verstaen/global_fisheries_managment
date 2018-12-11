####################################
##Updating Upside Data with RAMS##
###################################


#Load Packages
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
})


#Load Data
#Upside data
projection_data <- readRDS("data/projection_data.rds")

#RAMS
load("data/DBdata-model_fits_included.RData")

#Corbet data: when countries began using RBFs
countries_ITQ <- read_excel("data/Countries_ITQ.xlsx")



#RAM Data Tiddying
ram <- timeseries_values_views %>%
  select("stocklong", "year", "TBbest", "TCbest", "BdivBmsypref", "UdivUmsypref") %>%
  add_column("RAM")

colnames(ram) <- c( "CommName", "Year", "Biomass", "Catch", "BvBmsy", "FvFmsy", "DBase")



#Update Projection with RAM
projection_updated <- join(projection_data, ram, by = c("CommName", "Year"), type = "full")

#Merge Projection and ITQ
#seperate out IdOrig column for Projection Data
separate_projection <- projection_updated %>% separate(IdOrig, c("agency", "stock", "t0", "tf", "data_enterer"))

head(separate_projection, 10)

#reunite just the two parts we care about: agency and stock
unite_projection <- separate_projection %>% unite(assess_id_short, agency, stock, sep = "-")

#simplifying Countries ITQ with unique columns to prepare to merge
countries_ITQ_simple <- select(countries_ITQ, "assess_id_short", "year", "yearitq", "region_id", "idnumber", "harvest_fb", "itq_now", "iq", "itq", "ivq", "coop", "communityquota", "turf", "individualeffort", "iteq", "totalappliedeffort", "producerorg", "aquaculture", "programstart")

#rename year to be capitalized to merge with projection data
colnames(countries_ITQ_simple) <- c("assess_id_short", "Year", "yearitq", "region_id", "idnumber", "harvest_fb", "itq_now", "iq", "itq", "ivq", "coop", "communityquota", "turf", "individualeffort", "iteq", "totalappliedeffort", "producerorg", "aquaculture", "programstart")

ITQ_projection_merge <- merge(unite_projection, countries_ITQ_simple, by = c("assess_id_short", "Year"), all = TRUE)

#cleaning it up
ITQ_projection <- ITQ_projection_merge %>% unite(IdOrig, assess_id_short, t0, tf, data_enterer, sep = "-")


#Simplified Projection Data
#this includes ITQ information
ITQ_projection_simple <- ITQ_projection %>%
  select("Country", "Year", "CommName", "SciName", "SpeciesCat", "CatchShare", "Catch", "Biomass", "BvBmsy", "FvFmsy","MSY", "Price", "g", "k", "c", "phi", "yearitq", "itq_now", "turf", "programstart")

#create RDS from datafram
#saveRDS(ITQ_projection_simple, "projection_updated.rds")


#upside updated
#this does not include ITQ information
upside_2018_updated <- ITQ_projection %>%
  select("Country", "Year", "CommName", "SciName", "SpeciesCat", "CatchShare", "Catch", "Biomass", "BvBmsy", "FvFmsy","MSY", "Price", "g", "k", "c", "phi")

#create RDS from dataframe
#saveRDS(upside_2018_updated, "upside_2018_updated")
#write.csv(upside_2018_updated, file = "upside_2018_updated")





              