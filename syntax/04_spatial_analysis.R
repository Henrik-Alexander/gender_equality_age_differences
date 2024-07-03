#########################################
# Project: Age gaps and gender equality #
# Purpose: Spatial analysis             #
# Author: Henrik-Alexander Schubert     #
# E-mail: schubert@demogr.mpg.de        #
# Date: 24.06.2024                      #        
#########################################

# Packages
library(data.table)
library(tidyverse)
library(spdep)
library(sf)
library(spatialreg)

# Functions -----------------------------


# Preparations --------------------------

# Load the data
map_files <- list.files(path = "data/maps/map_data", full.names = T)
for (file in map_files) load(file)
map_aus <- st_transform(map_aus, "WGS84")
map_usa <- st_transform(map_usa, "WGS84")
map_usa$geometry <- st_geometry(map_usa)
map_aus$geometry <- st_geometry(map_aus)
rm(map_files)
map_files <- mget(ls(pattern = "map_"))
map_files <- lapply(map_files, st_transform, "WGS84")
map_files <- bind_rows(map_files)
map_files <- map_files |> select(region, geometry, country)
map_files <- map_files[map_files$region != "Outside Australia", ]

# Estimate the queens matrix ------------

# Create the queens matrix
nb <- poly2nb(map_files, queen = TRUE)
nbw <- nb2listw(nb, style = "B", zero.policy = TRUE)

# Attach the neighours to the data
nb |> 
  nb2listw(style="W", zero.policy=TRUE) |> 
  spweights.constants(zero.policy=TRUE) |> 
  data.frame() |> 
  subset(select=c(n, S0, S1, S2))

### define the formula -----------------

form <- formula(mac_diff_delta ~ hdi_delta)

## Estimate the Durbin model -----------

eigs <- eigenw(nbw)
errorsarlm(form, data )


# Spatial analysis ---------------------

# Moran's I
moran.text(TFR, nbw, alternative = "greater")

# Moran's plot
moran.plot(TFR, nbw)

# Local moran
localmoran(TFR, nbw, alternative = "greater")

### END #################################