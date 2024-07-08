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
library(ggspatial)
library(patchwork)
library(cowplot)
library(sp)
library(units)

# Load the development data
load("data/analysis_data.Rda")

# Load the basic graphics
source("functions/graphics.R")

dev <- dev[dev$year < 2018, ]

# Load the basemap function
source("functions/plot_base_map.R")

# Functions -----------------------------

# Clean the french map names
clean_french_regions <- function(department) {
  region <- department
  region[map_fra$region %in% c("Auvergne", "Rhône-Alpes")] <- "Auvergne-Rhône-Alpes" 
  region[map_fra$region %in% c("Bourgogne", "Franche-Comté")] <- "Bourgogne-Franche-Comté" 
  region[map_fra$region %in% c("Bretagne")] <- "Bretagne" 
  region[map_fra$region %in% c("Centre-Val de Loire", "Centre")] <- "Centre-Val-de-Loire" 
  region[map_fra$region %in% c("Corse")] <- "Corse" 
  region[map_fra$region %in% c("Alsace", "Champagne-Ardenne", "Lorraine")] <- "Grand Est" 
  region[map_fra$region %in% c("Nord-Pas-de-Calais", "Picardie")] <- "Hauts-de-France" 
  region[map_fra$region %in% c("Île-de-France")] <- "Île-de-France" 
  region[map_fra$region %in% c("Languedoc-Roussillon", "Midi-Pyrénées")] <- "Occitanie" 
  region[map_fra$region %in% c("Pays de la Loire")] <- "Pays de la Loire" 
  region[map_fra$region %in% c("Aquitaine", "Poitou-Charentes", "Limousin")] <- "Nouvelle-Aquitaine" 
  region[map_fra$region %in% c("Haute-Normandie", "Basse-Normandie")] <- "Normandie" 
  region[map_fra$region %in% c("Provence-Alpes-Côte d'Azur")] <- "Provence-Alpes-Côte d'Azur" 
  region[map_fra$region %in% c("Guadeloupe")] <- "Guadeloupe" 
  region[map_fra$region %in% c("Martinique")] <- "Martinique" 
  region[map_fra$region %in% c("Guyane")] <- "Guyane" 
  region[map_fra$region %in% c("DOM")] <- "DOM" 
  region[map_fra$region %in% c("La Réunion")] <- "La Réunion"
  region[map_fra$region %in% c("Mayotte")] <- "Mayotte" 
  return(region)
}

# Clean region names in Colombia
clean_col_regions <- function(regions) {
  reg <- regions
  reg[regions == 'Archipiélago de San Andrés, Providencia y Santa Catalina'] <- 'Archipelago of San Andrés, Providencia and Santa Catalina' 
  reg[regions == 'Bogotá, D.C.'] <- 'Bogotá' 
  reg[regions == 'Bolívar'] <- 'Bolivar' 
  reg[regions == 'Boyacá'] <- 'Boyaca' 
  reg[regions == 'Caquetá'] <- 'Caqueta' 
  reg[regions == 'Chocó'] <- 'Choco' 
  reg[regions == 'Córdoba'] <- 'Cordoba' 
  reg[regions == 'La Guajira'] <- 'La guajira' 
  return(reg)
}



# Preparations --------------------------

# Load the data
map_files <- list.files(path = "data/maps/map_data", full.names = T)
for (file in map_files) load(file)
map_aus <- st_transform(map_aus, "WGS84")
map_usa <- st_transform(map_usa, "WGS84")
map_usa$geometry <- st_geometry(map_usa)
map_aus$geometry <- st_geometry(map_aus)
map_fra$region <- clean_french_regions(map_fra$region)
map_fra <- map_fra |> group_by(country, region) |> summarise()
map_col$region <- clean_col_regions(map_col$region)
rm(map_files)
map_files <- mget(ls(pattern = "map_"))
map_files <- lapply(map_files, st_transform, "WGS84")
map_files <- bind_rows(map_files)
map_files <- map_files |> select(region, geometry, country)
map_files <- map_files[map_files$region != "Outside Australia", ]

# Create a capital column
map_files$capital <- "other"
capitals <- c("Berlin", "Helsinki-Uusimaa", "District of Columbia", "Australian Capital Territory",
              "Bogotá", "Ciudad de México", "Île-de-France", "Comunidad de Madrid" )
map_files$capital[map_files$region %in% capitals] <- "capital"

# Save the maps
save(map_files, file="data/map_files.Rda")

# Estimate the queens matrix ------------

# Create the queens matrix
nb <- map_files |> group_by(country) |> poly2nb(row.names = map_files$region, queen = TRUE)

# Transform the object into a matrix
nbw <- nb2listw(nb, style = "W", zero.policy = TRUE)


# Attach the neighours to the data
nb |> 
  nb2listw(style="W", zero.policy=TRUE) |> 
  spweights.constants(zero.policy=TRUE) |> 
  data.frame() |> 
  subset(select=c(n, S0, S1, S2))

### Define the formula -----------------

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

# 2. Estimate the spatial-distance model -------------

# Basic analysis: correlation
# Remove the value from the capital
dev <- full_join(map_files, dev[, !(names(dev) == "country")], by = "region")


# Create Contiguity (Queens) neighbours weights
queens.nb <- dev |> 
  #group_by(country, year) |> 
  poly2nb(queen = TRUE, snap = 1) # we consider points in 1m distance as 'touching'
queens.lw <- dev |> 
  group_by(country, year) |> 
  nb2listw(style = "W")

# Specify variables and formula
fm <- mac_diff ~ gdi + factor(country) + factor(year)

# Standardize variables
vars <- all.vars(fm)
msoa_sd.spdf <- dev
apply(dev, 2, function(x) as.numeric(ascale(x)))
for(v in vars){
  dev[, v] <- as.numeric(scale(dev[, v, drop = TRUE]))
}

# Estimate the models
mod_1.ols <- lm(fm, data = dev)

# Spatial autoregressive model
mod_1.sar <- lagsarlm(fm,  
                      data = dev, 
                      listw = queens.lw,
                      Durbin = FALSE) # we could here extend to SDM

# Spatial error model
mod_1.sem <- errorsarlm(fm,  
                        data = dev, 
                        listw = queens.lw,
                        Durbin = FALSE) # we could here extend to SDEM

# SLX
mod_1.slx <- lmSLX(fm,  
                   data = dev, 
                   listw = queens.lw, 
                   Durbin = TRUE) # use a formula to lag only specific covariates

# Spatial Durbin
mod_1.sdm <- lagsarlm(fm,  
                      data = dev, 
                      listw = queens.lw,
                      Durbin = TRUE) # we could here extend to SDM

# Spatial Durbun Error
mod_1.sdem <- errorsarlm(fm,  
                         data = dev, 
                         listw = queens.lw,
                         Durbin = TRUE) # we could here extend to SDEM


### Coefficient Output
# Get AIC and N for all models to get common gof stats
aic.l <- sapply(list(mod_1.ols, mod_1.sar, mod_1.sem, mod_1.slx, mod_1.sdm, mod_1.sdem),
                FUN = function(x) AIC(x))

n.l <- sapply(list(mod_1.ols, mod_1.sar, mod_1.sem, mod_1.slx, mod_1.sdm, mod_1.sdem),
              FUN = function(x) length(residuals(x)))

# Create table
mod_1.slx.lm <- mod_1.slx
class(mod_1.slx.lm) <- "lm" # only for gofs
screenreg(list(mod_1.ols, mod_1.sar, mod_1.sem, mod_1.slx.lm, mod_1.sdm, mod_1.sdem),
          custom.coef.map = list('(Intercept)' =  '(Intercept)',
                                 'park_kmsq' =  'Green space',
                                 'pt_access_index' =  'Public transport access',
                                 'POPDEN' =  'Population density',
                                 'per_nonUK' =  '% non-UK',
                                 'per_social' = '% social housing',
                                 'lag.park_kmsq' =  'W Green space',
                                 'lag.pt_access_index' =  'W Public transport access',
                                 'lag.POPDEN' =  'W Population density',
                                 'lag.per_nonUK' =  'W % non-UK',
                                 'lag.per_social' = 'W % social housing',
                                 'rho' = 'rho',
                                 'lambda' = 'lambda'),
          custom.model.names = c("OLS", "SAR", "SEM", "SLX", "SDM", "SDEM"),
          dcolumn = TRUE, caption.above = TRUE, digits = 3,
          caption = "Spatial regression models. Outcome variable: median house price.",
          include.nobs = FALSE,
          include.loglik = FALSE,
          include.aic = FALSE,
          include.lr = TRUE,
          include.wald = FALSE,
          include.fstatistic = FALSE,
          include.rmse = FALSE,
          custom.gof.rows = list('Num. obs.' = n.l, 
                                 'AIC' = aic.l), 
          reorder.gof = c(1, 3:6, 2))










####

# add column for distnance to capital
map_files$distance_to_capital <- NA

for(country in unique(map_files$country)) {
  
# Select a country map
mp <- map_files[map_files$country == country, ]

# Create the centroids
centroids <- st_centroid(st_geometry(mp))
capital <- mp$capital == "capital"
dist <- st_distance(centroids, centroids, method = "euclidean")
rownames(dist) <- colnames(dist) <- mp$region
mp$dist_to_capital <- dist[capital, ]

# Assign distance to capital also to the general map data
map_files$distance_to_capital[map_files$region %in% mp$region] <- mp$dist_to_capital

# Plot with distance to capital
p <- plot_base_map(mp) +
  geom_sf(data = mp, aes(fill = as.numeric(dist_to_capital)/1000)) +
  theme_void() +
  scale_fill_gradient(low = "yellow", high = "darkred", name = "Distance (km)") +
  theme(
    plot.background = element_rect(fill = "lightblue"),
    legend.position = c(0.8, 0.2),
    legend.background = element_rect(fill = "white"),
  )

print(p)
Sys.sleep(2)
}


# Extract the capital data
capital_measures <- dev |> 
  st_drop_geometry() |> 
  filter(capital == "capital") |>
  select(country, year, gdi, mac_diff) |> 
  rename(gdi_capital = gdi, mac_diff_capital = mac_diff)

# Remove the 
dev <- full_join(dev, capital_measures, by = c("country", "year"))

# Estimate the difference between capital and
dev_dist_data <- dev |> 
  st_drop_geometry() |> 
  group_by(country, year) |> 
  mutate(mac_mean = mean(mac_diff),
         gdi_mean = mean(gdi)) |> 
  mutate(mac_diff_capital = mac_diff - mac_mean,
         gdi_capital = gdi - gdi_mean) 
dev_dist_data |> 
  group_by(country) |> 
  summarise(dist_gdi_correlation = cor(distance_to_capital, mac_diff_capital, use = "pairwise.complete.obs"),
            dist_mac_correlation = cor(distance_to_capital, gdi_capital, use = "pairwise.complete.obs"))


# Is gender development lower in the capital value than rest of the country
dev |> 
  group_by(year, country) |> 
  mutate(gdi_mean = mean(gdi, na.rm = T)) |> 
  filter(capital == "capital") |> 
  ggplot(aes(x = gdi, y = gdi_mean, colour = country, alpha = year)) + 
    geom_abline(intercept = 0, slope = 1) +
    geom_point() +
  guides(alpha = "none")

# Is age gap lower in the capital value than rest of the country
dev |> 
  group_by(year, country) |> 
  mutate(mac_diff_mean = mean(mac_diff)) |> 
  filter(capital == "capital") |> 
  ggplot(aes(x = mac_diff, y = mac_diff_mean, colour = country, alpha = year)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  guides(alpha = "none")


# Plot the age gap data
ggplot(dev_dist_data, aes(x = log(distance_to_capital/1000), mac_diff_capital, colour = country), alpha = 0.3) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", se = F)

ggplot(dev_dist_data, aes(x = log(distance_to_capital/1000), gdi_capital, colour = country)) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", se = F)

### END #################################
