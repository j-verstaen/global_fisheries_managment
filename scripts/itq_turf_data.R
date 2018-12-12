################################################################
##Creating dataframe with countries, species, and itq/turf info##

###has all countries that have ever had ITQ and/or TURFs and the ISSCAAP category code associated with the scientific name of the species##

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

ITQ_projection <- readRDS("data/ITQ_projection.rds")

# create dataframe with 
itq <- ITQ_projection %>%
  select("Country", "SciName", "itq_now","iq", "itq", "ivq", "turf", "programstart")

#Load EDF TURF data
edf_turf <- read_excel("data/EDF_TURF_data.xlsx")

#load in DiscoverTURFs data
d_turfs <- read_excel("data/DiscoverTURFs_simple.xlsx")

edf_turf_simple <- select(edf_turf, "country", "start_date", "SciName", "turf")

colnames(edf_turf_simple) <- c("Country", "programstart", "SciName", "turf")

#combining edf turf data from upside data
edf_projection_1 <- join(edf_turf_simple, itq, by = c("Country", "programstart", "SciName", "turf"), type = "full")

#combine edf/upside with discover turfs
all_turf_1 <- join(edf_projection_1, d_turfs, by = c("Country", "SciName", "turf"), type = "full")

#rearrange columns
all_turf_itq <- all_turf_1[, c(1,3,2,5,6,7,8,4)]

#make all NAs into False with regards to ITQs and TURFs
all_turf_itq$iq[is.na(all_turf_itq$iq)] <- FALSE

all_turf_itq$itq[is.na(all_turf_itq$itq)] <- FALSE

all_turf_itq$ivq[is.na(all_turf_itq$ivq)] <- FALSE

all_turf_itq$itq_now[is.na(all_turf_itq$itq_now)] <- 0


#filter out all countries that don't have itqs or turfs
all_turf_itq_1 <- all_turf_itq %>%
  filter (iq == "TRUE" | itq == "TRUE" | ivq == "TRUE" | turf == "TRUE") %>%
  filter( Country != "NA") %>%
  select("Country", "SciName", "iq", "itq", "ivq", "turf") 

#getting rid of all duplicates beause the data was still from all the years with ITQs
unique_turf_itq <- unique(all_turf_itq [ , 1:8])


turf_itq_isscaap_1 <- join(unique_turf_itq, meta_isscaap_codes, by = "SciName", type = "full")

turf_itq_isscaap <- turf_itq_isscaap_1 %>%
  filter( itq != "NA" & ivq != "NA" & iq != "NA" & turf != "NA") 

turf_itq_isscaap[is.na(turf_itq_isscaap)] <- "NA"

#write.csv(turf_itq_isscaap, file = "turf_itq_isscaap.csv")



