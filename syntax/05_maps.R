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

# Load the development data
load("data/analysis_data.Rda")

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


plot_spain_map <- function(data = tmp, outcome = gdi, gdi = T) {
  if(gdi) {
    label = "Gender Development Index"
    option = "A"
    direction = 1
    limits = c(0.88, 1.02)
  } else {
    label = "Average parental age gap"
    option = "D"
    direction = -1
    limits = c(1.5, 6)
  }
  # Function to plot spain
  exteriors <- c("Canarias")
  main <- data %>%
    filter(!(region %in% exteriors))
  island <- data %>%
    filter(region %in% exteriors)
  # Main plot
  main_gg <- plot_base_map(main) + 
    geom_sf(data = main, aes(fill = {{outcome}})) +
    geom_sf(data = subset(tmp, capital == "capital"), colour = "white", fill = NA) +
    scale_fill_viridis_c(limits = limits, option = option, name = "", direction = direction) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm"),
          legend.key.height = unit(0.1, "cm"))
  # Sub plot
  sub_gg <- plot_base_map(island) +
    geom_sf(data = island, aes(fill = {{outcome}})) +
    scale_fill_viridis_c(limits = limits, option = option, name = "", direction = direction) +
    #ggtitle("Canarias") +
    theme(
      panel.border = element_rect(fill = NA, colour = "black"),
      plot.background = element_rect(fill = "grey95")
    ) +
    guides(fill = "none")
  plot_final <- ggdraw() +
    draw_plot(main_gg) +
    draw_plot(sub_gg, height = 0.1, x = 0.25, y = 0.24)
  print(plot_final)
  return(plot_final)
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

#

### Maps --------------------------------

# Plot:
plots <- vector("list", length = length(unique(map_files$country)))
names(plots) <- unique(map_files$country)

# Plot left the development and right the mac difference
for (country in unique(map_files$country)) {
  label <- country
  if(country == "USA") label <- "United States"
  tmp <- map_files[map_files$country == country, ]
  tmp <- tmp[, !(names(tmp) == "country")]
  tmp2 <- dev[!is.na(dev$gdi) & !is.na(dev$mac_diff) & dev$country == label, ]
  tmp2 <- dev[dev$year == max(tmp2$year, na.rm = T)  & dev$country == label, ]
  tmp <- full_join(tmp, tmp2, by = c("region"))
  if (country == "Spain") {
    p1 <- plot_spain_map()
    p2 <- plot_spain_map(outcome = mac_diff, gdi = F)
  } else {
    p1 <- plot_base_map(tmp) +
      geom_sf(data = tmp, aes(fill = gdi)) +
      geom_sf(data = subset(tmp, capital == "capital"), colour = "white", fill = NA) +
      scale_fill_viridis_c(limits = c(0.88, 1.02), option = "A", name = "") +
      theme(legend.position = "bottom",
            legend.key.width = unit(1, "cm"),
            legend.key.height = unit(0.1, "cm")) +
      guides(colour = "none")
    p2 <- plot_base_map(tmp) +
      geom_sf(data = tmp, aes(fill = mac_diff, colour = capital)) +
      geom_sf(data = subset(tmp, capital == "capital"), colour = "white", fill = NA) +
      scale_fill_viridis_c(limits = c(1.5, 6), option = "D", name = "", direction = -1) +
      scale_colour_manual(values = c("capital" = "white", "other" = "grey20")) +
      theme(legend.position = "bottom",
            legend.key.width = unit(1, "cm"),
            legend.key.height = unit(0.1, "cm")) +
      guides(colour = "none")
  }
  plots[[country]] <- p1 + p2 
  print(plots[[country]])
}

# Merge regional data
wrap_plots(plots, ncol = 2) +
  plot_layout(guides = "collect") +
  theme(legend.position = "bottom")
