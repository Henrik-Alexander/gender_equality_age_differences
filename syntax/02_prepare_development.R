########################################
# Project: Gender equality and MAC     #
# Purpose: Development data            #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.06.2024                     #
########################################

# Load the packages
library(data.table)
library(tidyverse)
library(janitor)
library(stargazer)
library(patchwork)

### Settings ----------------------------------------

# Specify the baseline path
path_dev <- "U:/data/global/subnational_hdi/"

# Specify countries
countries <- c("Australia", "United States", "Germany", "Finland", "Spain", "France", "Mexico", "Colombia")

### Load the data ----------------------------------

# Load the development data
hdi <- read.csv(paste0(path_dev, "GDL-Subnational-HDI-data.csv"))
gdi <- read.csv(paste0(path_dev, "GDl-Subnational-GDI-data.csv"))

# Development indicators
hdi <- pivot_longer(hdi, cols = starts_with("X"), names_to = "year", values_to = "hdi", names_prefix = "X") |> 
  clean_names()

# Gender indicators
gdi <- pivot_longer(gdi, starts_with("X"), names_to = "year", values_to = "gdi", names_prefix = "X") |> 
  clean_names()

# Combine the data
dev <- inner_join(hdi, gdi, by = c("country", "region", "year", "level", "iso_code", "continent", "gdlcode"))

# Clean the data
dev$year <- as.numeric(dev$year)

# Filter the data
dev <- subset(dev, subset = country %in% countries & level == "Subnat")

# Remove variables
dev <- subset(dev, select = c("country", "region", "year", "hdi", "gdi"))

### Clean the french regions --------------------------------

# Rename regions
dev[dev$region %in% c("Auvergne", "Rhone-Alpes"), ]$region <- "Auvergne-Rhône-Alpes"
dev[dev$region %in% c("Bourgogne", "Franche-Comte"), ]$region <- "Bourgogne-Franche-Comté"
dev[dev$region %in% c("Bretagne"), ]$region <- "Bretagne"
dev[dev$region %in% c("Centre"), ]$region <- "Centre-Val-de-Loire"
dev[dev$region %in% c("Corse"), ]$region <- "Corse"
dev[dev$region %in% c("Alsace", "Lorraine", "Champagne-Ardenne"), ]$region <- "Grand Est"
dev[dev$region %in% c("Nord", "Picardie"), ]$region <- "Hauts-de-France"
dev[dev$region %in% c("Haute-Normandie", "Basse-Normandie"), ]$region <- "Normandie"
dev[dev$region %in% c("Limousin", "Aquitaine", "Poitou-Charentes"), ]$region <- "Nouvelle-Aquitaine"
dev[dev$region %in% c("Languedoc-Roussillon", "Midi-Pyrenees"), ]$region <- "Occitanie"
dev[dev$region %in% c("Pays de la Loire"), ]$region <- "Pays de la Loire"
dev[dev$region %in% c("Provence-Alpes-Cote dAzur"), ]$region <- "Provence-Alpes-Côte d'Azur"
dev[dev$region %in% c("Ile de France"), ]$region <- "Île-de-France"
dev[dev$region %in% c("Guadeloupe"), ]$region <- "Guadeloupe"
dev[dev$region %in% c("Reunion"), ]$region <- "La Réunion"
dev[dev$region %in% c("Martinique"), ]$region <- "Martinique"

### Clean the German data -------------------------------------

# Harmonize the colombian data
harmonize_deu <- function(regions) {
  new_regions <- regions
  new_regions[regions == "Thuringen"] <- "Thüringen"
  new_regions[regions == "Baden-Wurttemberg"] <- "Baden-Württemberg"
  return(new_regions)
}
dev$region <- harmonize_deu(dev$region)

### Clean the Colombian data ----------------------------------

# Harmonize the colombian names
harmonize_col <- function(regions) {
  new_regions <- regions
  dev_col_regs <- c("Antioquia (incl Medellin)", "Atlantico (incl Barranquilla)", "Bogota D.C.", "Bolivar (Sur and Norte)", "Guainja",
                    "Guajira", "Narino", "San Andres", "Valle (incl Cali)", "Vaupis")
  fert_col_regs <- c("Antioquia", "Atlántico", "Bogotá",
                          "Bolivar", "Guainía", "La guajira", "Nariño", "Archipelago of San Andrés, Providencia and Santa Catalina", "Valle del Cauca", "Vaupés")
  for (i in seq_along(dev_col_regs)) {
    new_regions[regions == dev_col_regs[i]] <- fert_col_regs[i]
  }
  return(new_regions)
}

dev$region <- harmonize_col(dev$region)


### Harmonize the Mexican data -----------------------------------

harmonize_mex <- function(regions) {
  new_regions <- regions
  fert_mex_regs <- c("México", "Michoacán de Ocampo", "Nuevo León", "Querétaro",
                     "San Luis Potosí", "Veracruz de Ignacio de la Llave", "Yucatán",
                     "Coahuila de Zaragoza", "Ciudad de México")
  dev_mex_regs <- c("Mexico", "Michoacan de Ocampo", "Nuevo Leon", "Queretaro",
                    "San Luis Potosi", "Veracruz", "Yucatan", "Coahuila", "Distrito Federal")
  for (i in seq_along(dev_mex_regs)) {
    new_regions[regions == dev_mex_regs[i]] <- fert_mex_regs[i]
  }
  return(new_regions)
}
dev$region <- harmonize_mex(dev$region)

### Complete the data ---------------------------------------------

# Create groupings
dev <- dev |> 
  group_by(region, year, country) |> 
  summarise(hdi = mean(hdi), gdi = mean(gdi), .groups = "drop")

# Save the data
save(dev, file = "data/development.Rda")

### END #############################################