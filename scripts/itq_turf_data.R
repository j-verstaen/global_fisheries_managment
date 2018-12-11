################################################################
##Creating dataframe with countries, species, and itq/turf info##
################################################################

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

#load data

ITQ_projection <- readRDS("processed_data/ITQ_projection.rds")

# create dataframe with 
itq <- ITQ_projection %>%
  select("Country", "SciName", "itq_now","iq", "itq", "ivq", "turf", "programstart")

#Load EDF TURF data
edf_turf <- read_excel("data/EDF_TURF_data.xlsx")

#load in DiscoverTURFs data
d_turfs <- read_excel("data/DiscoverTURFs_simple.xlsx")


edf_turf_simple <- select(edf_turf, "country", "start_date", "SciName", "turf")
#create new column that says "turf" and TRUE or FALSE


colnames(edf_turf_simple) <- c("Country", "programstart", "SciName", "turf")

#combining edf turf data from upside data
edf_projection_1 <- join(edf_turf_simple, itq, by = c("Country", "programstart", "SciName", "turf"), type = "full")

#combine edf/upside with discover turfs
all_turf_1 <- join(edf_projection_1, d_turfs, by = c("Country", "SciName", "turf"), type = "full")

#change "blank" NAs to real NAs
#all_turf_1[is.na(all_turf_1)] <- "NA"

#rearrange columns
all_turf_itq <- all_turf_1[, c(1,3,2,5,6,7,8,4)]

#turning NAs from no ITQ data to FALSE
#all_turf_2[all_turf_2 == 0] <- NA

all_turf_itq$iq[is.na(all_turf_itq$iq)] <- FALSE

all_turf_itq$itq[is.na(all_turf_itq$itq)] <- FALSE

all_turf_itq$ivq[is.na(all_turf_itq$ivq)] <- FALSE

all_turf_itq$itq_now[is.na(all_turf_itq$itq_now)] <- 0

#filter out all countries that don't have itqs or turfs
true_turf_itq_1 <- all_turf_itq %>%
  filter (iq == "TRUE" | itq == "TRUE" | ivq == "TRUE" | turf == "TRUE") %>%
  filter( Country != "NA") %>%
  select("Country", "SciName", "iq", "itq", "ivq", "turf") 

true_turf_itq <- unique(true_turf_itq_1 [ , 1:6])

##Using ISSCAPP species numbers from upsideside data
iscaap_cat_1 <- upside_2018_updated %>%
  select("SciName", "SpeciesCat") 

iscaap_cat <- unique(iscaap_cat_1 [ , 1:2])

true_turf_itq_iscaap_1 <- merge(true_turf_itq, iscaap_cat, by = "SciName", all = T)

true_turf_itq_iscaap <- true_turf_itq_iscaap_1 %>%
  filter( itq != "NA" & ivq != "NA" & iq != "NA" & turf != "NA") 

true_turf_itq_iscaap[is.na(true_turf_itq_iscaap)] <- "NA"

#write.csv(true_turf_itq_iscaap, file = "true_turf_itq_iscaap.csv")


####FAO website has been down, can't access the species list with FAO numbers associated. did it by hanf for now: read in below csv file for updated with FAO numbers
species_rightsbased <- read_csv("data/species_rightsbased.csv")
