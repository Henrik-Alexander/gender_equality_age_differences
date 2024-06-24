## Prepare the Spain map data
library(sf)
library(foreign)
library(tidyverse)

# Load the data
shp_files <- list.files(path = "data/maps/spain_map", pattern = ".shp$", full.names = TRUE)
dbf_files <- list.files(path = "data/maps/spain_map", pattern = ".dbf$", full.names = TRUE)
esp_map <- read_sf("data/maps/spain_map/es.dbf")

# Clean the data
names(map_esp) <- tolower(names(map_esp))

# Rename the variables
map_esp <- rename(map_esp, region = name)
map_esp$country <- "Spain"

# Select the important variables
map_esp <- map_esp[, c("country", "region")]

# Save the map
save(map_esp, file = "data/maps/map_data/map_esp.Rda")

### END ###########################################