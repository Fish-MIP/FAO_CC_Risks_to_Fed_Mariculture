# Description ----------------------------------------------------------------------------------------------
# This document prepares the aquaculture locations from:
# Clawson, G., Kuempel, C. D., Frazier, M., Blasco, G., Cottrell, R. S., Froehlich, H. E., Metian, M., Nash, K. L., Többen, J., Verstaen, J., Williams, D. R., & Halpern, B. S. (2022). Mapping the spatial distribution of global mariculture production. Aquaculture, 553, 738066. https://doi.org/10.1016/j.aquaculture.2022.738066
# and combines it with FAO fishing regions, ready to be paired with FAO production data.

library(tidyverse)
library(terra)
library(here)
library(purrr)

here("src", "dirs.R") %>% source()
here("src", "functions.R") %>% source()

# Raw data -------------------------------------------------------------------------------------------------
mari_locs <- file.path(rawdata_path, "all_marine_aquaculture_farms_sources_final.csv") %>% 
  read.csv()

mari_locs_sf <- mari_locs %>% 
  st_as_sf(coords = c("X", "Y"), crs = "EPSG:4326") %>% 
  mutate(row_num = row_number()) %>% 
  group_by(row_num) %>% 
  group_split()

# Get FAO fishing/country data -----------------------------------------------------------------------------
FAO_shp <- file.path(bigdata_path, "fao/FAO_AREAS_CWP_NOCOASTLINE") %>% 
  read_sf() %>% 
  filter(F_LEVEL == "MAJOR") %>% 
  select(F_CODE)

# Just the paired codes and countries - for later
FAO_code_key <- file.path(bigdata_path, "fao/FAO_AREAS_CWP_NOCOASTLINE") %>% 
  read_sf() %>% 
  filter(F_LEVEL == "MAJOR") %>% 
  distinct(F_CODE, NAME_EN)


unique(mari_locs$species_group)
