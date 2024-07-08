## Prepare the Spain map data
library(sf)
library(foreign)
library(tidyverse)
library(basemaps)

# Load the data
shp_files <- list.files(path = "data/maps/spain_map", pattern = ".shp$", full.names = TRUE)

# Load the file
map_esp <- read_sf(shp_files)

# Harmonize the regions
map_esp$region <- map_esp$acom_name
map_esp$region[map_esp$acom_name == 'Cataluña'] <- 'Cataluna' 
map_esp$region[map_esp$acom_name == 'Castilla y León'] <- 'Castilla y Leon' 
map_esp$region[map_esp$acom_name == 'Comunitat Valenciana'] <- 'Comunidad Valenciana' 
map_esp$region[map_esp$acom_name == 'Castilla-La Mancha'] <- 'Castilla-la Mancha' 
map_esp$region[map_esp$acom_name == 'Andalucía'] <- 'Andalucia' 
map_esp$region[map_esp$acom_name == 'País Vasco'] <- 'Pais Vasco' 
map_esp$region[map_esp$acom_name == 'Aragón'] <- 'Aragon' 
map_esp$region[map_esp$acom_name == 'Región de Murcia'] <- 'Region de Murcia' 
map_esp$region[map_esp$acom_name == 'Ciudad Autónoma de Melilla'] <- 'Ciudad Autonoma de Melilla' 
map_esp$region[map_esp$acom_name == 'Territorio no asociado a ninguna autonomía'] <- NA
map_esp$region[map_esp$acom_name == 'Ciudad Autónoma de Ceuta'] <- 'Ciudad Autonoma de Ceuta'

# Aggregate the data
map_esp <- map_esp |> 
  filter(!is.na(region)) |> 
  group_by(region) |> 
  summarise()

# Add the country
map_esp$country <- "Spain"

# Save the map
save(map_esp, file = "data/maps/map_data/map_esp.Rda")

### END ###########################################