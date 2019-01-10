
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
  library(here)
  
Countries_ITQ <- read_excel(here("data","Countries_ITQ.xlsx"))

#new dataframw with just most recent year of data for each fishery
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
projection_data <- readRDS(here("data","projection_data.rds")) %>% 
  filter(Scenario == "Historic") %>% 
  group_by(IdOrig) %>% 
  dplyr::mutate(max_year = max(Year)) %>% 
  filter(max_year == 2012)

test <- projection_data %>% 
  filter(Year == max_year) %>% 
  group_by(CatchShare, Dbase) %>% 
  summarise(total_catch = sum(Catch))


#RAM
load(here("data","DBdata-model_fits_included.RData"))


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

#filter out data post 2016 because we know that those are projected/modeled data not recorded
recent_pre2016 <- ITQ_projection_merge %>%
  filter(Year <= final_year)

#keep only info on most recent year for each fishery  
fisheries_recent_1 <- recent_pre2016 %>% 
  filter(Year == final_year & final_year > 2010)
  
  
#select the columns we are interested in
fisheries_recent <- fisheries_recent_1 %>%
  select(Country, assess_id_short, Year, CommName, Biomass, Catch, BvBmsy, FvFmsy, Dbase, SciName, IdLevel, SpeciesCat.x, Profits, MSY, Price, g, k, c, phi, itq, ivq, iq, turf)

 b <- fisheries_recent %>% 
   filter(!is.na(itq) | !is.na(ivq) | !is.na(iq))

a = fisheries_recent %>% 
  ggplot(aes(BvBmsy, FvFmsy, color =Dbase )) + 
  geom_point() + 
  coord_cartesian(xlim = c(0,4), ylim = c(0,4))

a = fisheries_recent %>% 
  ggplot(aes(BvBmsy, FvFmsy, color =itq )) + 
  geom_point() + 
  coord_cartesian(xlim = c(0,4), ylim = c(0,4))


#write dataframe to csv file
write.csv(fisheries_recent, file = here("processed_data","fisheries_recent.csv"))

