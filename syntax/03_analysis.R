########################################
# Project: Gender equality and MAC     #
# Purpose: Development data            #
# Author: Henrik-Alexander schubert    #
# E-mail: schubert@demogr.mpg.de       #
# Date: 25.06.2024                     #
########################################

rm(list = ls())

# Load the packages
library(data.table)
library(tidyverse)
library(janitor)
library(stargazer)
library(patchwork)

countries <- c("aus", "col", "deu", "esp", "fin", "fra", "mex", "usa")

### Functions ----------------------------------------

# Extract middle age from age group
extract_middle_age <- function(age_group){
  lower <- regmatches(age_group, regexpr("^[0-9]{2}", age_group))
  upper <- regmatches(age_group, regexpr("[0-9]{2}$", age_group))
  (as.numeric(lower) + as.numeric(upper) + 1) / 2
}

# Estimate the mean age of childbearing
make_age_numeric <- function (data) {
  if (any(names(data) == "age_group")) {
    data$age <- extract_middle_age(data$age_group)
  }
  return(data)
}

# From country code to country
cntrcode2country <- function(countrycode) {
  if (!all(countrycode %in% countries))
    country <- character(length = length(countrycode))
    country[countrycode == "aus"] <- "Australia"
    country[countrycode == "col"] <- "Colombia"
    country[countrycode == "deu"] <- "Germany"
    country[countrycode == "esp"] <- "Spain"
    country[countrycode == "fin"] <- "Finland"
    country[countrycode == "fra"] <- "France"
    country[countrycode == "mex"] <- "Mexico"
    country[countrycode == "usa"] <- "United States"
    return(country)
}

### Load the fertility data --------------------------


for (country in countries) {
  cat("----------------\nCountry:", country, "\n")
  path <- paste0("U:/data/", country, "/fertility_rates")
  files <- list.files(path, pattern = "^asfr", full.names = T)
  for (file in files) load(file)
}
rm(asfr_esp_nat, asfr_mex_nat)

# Create the australian data
asfr_aus <- dcast(age + region + year ~ sex, data = asfr_aus, value.var = "asfr_aus", fill = 0)
setnames(asfr_aus, old = c("female", "male"), new = c("asfr_female", "asfr_male"))

# Collect the data
asfrs <- mget(ls(pattern = "asfr"))

# Estimate the asfrs
asfrs <- lapply(asfrs, make_age_numeric)

# Select the important data
asfrs <- lapply(asfrs, function(x) x |> select(age, region, year, asfr_female, asfr_male))

# Bind the data
asfrs <- bind_rows(asfrs, .id = "country")

# Estimate the mean age childbearing
macs <- asfrs[, .(mac_female = sum(asfr_female * age, na.rm = T) / sum(asfr_female, na.rm = T),
                  mac_male = sum(asfr_male * age, na.rm = T) / sum(asfr_male, na.rm = T)), by = .(year, region, country)]

# Bind with Finnish data
tfr_fin$country <- "fin"
macs <- bind_rows(macs, tfr_fin[, c("region", "year", "country", "mac_female", "mac_male")])

# Clean country data
macs$country <- cntrcode2country(str_extract(macs$country, paste0(countries, collapse = "|")))


### Merge fertility and development data -------------

# Load the development data
load("data/development.Rda")

# Merge the data
dev <- left_join(dev, macs, by = c("region", "country", "year"))

# Print the missings
dev |> group_by(country, year) |> summarise(miss = mean(is.na(mac_female))) |> pivot_wider(names_from = "year", values_from = miss)

# Estimate the mean age gap
dev$mac_diff <- dev$mac_male - dev$mac_female

# Remove missing observation
dev <- dev[!is.na(dev$mac_diff) & !is.na(dev$gdi), ]


# Save the data
save(dev, file = "data/analysis_data.Rda")

# Remove Ciudad Autonoma de Melilla
dev <- dev[dev$region != "Ciudad Autonoma de Melilla", ]
dev <- dev[dev$region != "Ciudad Autonoma de Ceuta", ]
dev <- dev[dev$region != "District of Columbia", ]

### Plot the results ---------------------------------

# Relationship
basic_relationship <- ggplot(dev, aes(x=gdi, y=mac_diff, fill=country)) +
  geom_point(size=2, color="white", shape=22) +
  scale_fill_viridis_d(name="Country", option = "viridis") +
  scale_y_continuous("Mean parental age gap") +
  scale_x_continuous("Gender Development Index") +
  guides(fill=guide_legend(override.aes = list(size=3)))

# Add the regression lines
basic_relationship +
  geom_smooth(method="lm", se=F, formula="y~x", aes(colour=country)) +
  scale_colour_viridis_d(name="Country", option = "viridis")


### Estimate the models ------------------------------

# Model 1
ols1 <- lm(mac_diff ~ gdi, data = dev)
ols2 <- lm(mac_diff ~ gdi + factor(year), data = dev)
ols3 <- lm(mac_diff ~ gdi + factor(year) + country, data = dev)

# Create first difference data
dev_diff <- dev |> 
  arrange(country, region, year) |> 
  group_by(region) |> 
  mutate(hdi_delta = hdi - lag(hdi),
         gdi_delta = gdi - lag(gdi),
         mac_diff_delta = mac_diff - lag(mac_diff)) |> 
  filter(!is.na(mac_diff_delta))

# Model
lm(mac_diff_delta ~ gdi_delta, data = dev_diff)
lm(mac_diff_delta ~ -1 + gdi_delta + factor(year), data = dev_diff)


### END ##############################################