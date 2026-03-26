library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

# The Robinson projection --------------------------------------------------------------------------------------------------------------
worldmap_robinson <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(crs = "+proj=robin")
graticules_robinson <- st_graticule(worldmap_robinson, lon = seq(-180, 180, 30), lat = seq(-90, 90, 30)) 

p_bigmap_robinson <- ggplot() +
  geom_sf(data = graticules_robinson, color = "gray80", size = 0.3) +
  geom_sf(data = worldmap_robinson, fill = "white", color = "dimgray") +
  coord_sf() +
  theme_void()

# The Mercator projection --------------------------------------------------------------------------------------------------------------
worldmap_mercator <- ne_countries(scale = "large", returnclass = "sf")
graticules_mercator <- st_graticule(worldmap_mercator, lon = seq(-180, 180, 30), lat = seq(-90, 90, 30)) 

p_bigmap_mercator <- ggplot() +
  geom_sf(data = graticules_mercator, color = "gray80", size = 0.3) +
  geom_sf(data = worldmap_mercator, fill = "white", color = "dimgray") +
  coord_sf() +
  labs(y = "Latitude", x = "Longitude")
