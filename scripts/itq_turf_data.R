##Creating datafram with countries, species, and itq/turf info

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

#data tidyying
edf_turf_simple <- select(edf_turf, "country", "start_date", "SciName", "turf")

colnames(edf_turf_simple) <- c("Country", "programstart", "SciName", "turf")

#combining edf turf data from upside data
edf_projection_1 <- join(edf_turf_simple, itq, by = c("Country", "programstart", "SciName", "turf"), type = "full")

#combine edf/upside with discover turfs
all_turf_1 <- join(edf_projection_1, d_turfs, by = c("Country", "SciName", "turf"), type = "full")

#change "blank" NAs to real NAs
all_turf_1[is.na(all_turf_1)] <- "NA"

#rearrange columns
all_turf_2 <- all_turf_1[, c(1,3,2,5,6,7,8,4)]

#rename itq_now from NA -> 0 and all other NAs to FALSE
