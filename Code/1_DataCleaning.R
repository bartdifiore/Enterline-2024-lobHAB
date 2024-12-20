#-----------------------
## Libraries
#-----------------------

library(tidyverse)
library(sdmTMB)
library(sdmTMBextra)
library(sf)

#-----------------------
## Get data
#-----------------------

df <- read.csv("Data/trawl_lobster_ME_end_hab_sed_statarea_ics_fvcom_reduced_sdm.csv") %>%
  janitor::clean_names() %>%
  select(season, year, start_lati, start_long, weight_kg, bathy_depth, habitat, sed_type, fvcom_bot_temp_c) %>%
  add_utm_columns(ll_names = c("start_long", "start_lati"), units = "km") %>%
  rename(BT_seasonal = fvcom_bot_temp_c, 
         depth = bathy_depth) %>%
  mutate(depth = depth*-1)

write_rds(df, "Data/Derived/model_data.rds")
