




match_to_master_catalog <- read.csv("~/Downloads/match_to_master_catalog.csv")

colnames(match_to_master_catalog)
str(match_to_master_catalog)

library(tidyverse)
library(sf)
library(mapview)

length(unique(match_to_master_catalog$Dolphin.ID))

mapview(match_to_master_catalog, xcol = "Long", ycol = "Lat", crs = 4269, grid = FALSE, 
        zcol = "Xenobalanus.globicipitis.Presence.Absence") 


starbucks <- read_csv("https://raw.githubusercontent.com/libjohn/mapping-with-R/master/data/All_Starbucks_Locations_in_the_US_-_Map.csv")
starbucksNC <- starbucks  %>% 
    filter(State == "NC")

mapview(starbucksNC, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)

my_sf <- st_as_sf(match_to_master_catalog, coords = c('Long', 'Lat'))

my_sf <- st_set_crs(my_sf, 4326)

#Plot it:
ggplot(my_sf) + 
    geom_sf() +
    geom_line(data = match_to_master_catalog, aes(x = Long, y = Lat, group = Dolphin.ID, col = Dolphin.ID)) +
    theme(legend.position = "none") +
    coord_sf(xlim = c(-75, -79), ylim = c(33, 36), expand = FALSE)



library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
    geom_sf() +
    geom_line(data = match_to_master_catalog, aes(x = Long, y = Lat, group = Dolphin.ID, col = Dolphin.ID)) +
    theme(legend.position = "none") +
    coord_sf(xlim = c(-75, -79), ylim = c(33, 36), expand = FALSE)





