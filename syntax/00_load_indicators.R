#### Load data

# Load the account information
source("functions/account_information.R")


# Load packages
library(eurostat)
library(tidyverse)
library(readxl)

# Functions -----------------------------------------

load_filter_eurostat <- function(search_term = "employment") {
  tmp <- search_eurostat(search_term)
  tmp <- tmp[as.numeric(str_sub(tmp$data.start, 1, 4)) < 2000, ]
  tmp <- tmp[str_detect(tmp$title, "sex") & str_detect(tmp$title, "NUTS|regio"), ]
  return(tmp)
}


# Function to translate 


# 1. Australia --------------------------------------

# Load the australian data
files <- list.files("raw/aus")



# 2. European countries -----------------------------


# Female employment
emp <- load_filter_eurostat("employment")
emp_code <- emp$code[emp$title == "Youth unemployment rate by sex, age and NUTS 2 regions"]
emp <- get_eurostat(emp_code, time_format = "num", stringsAsFactors = F)
emp |> regions::recode_nuts(geo_var = "geo")

# Female education
edu <- load_filter_eurostat("education")
edu_code <- edu$code[edu$title == "Population by sex, age, educational attainment level and NUTS 2 regions (1 000)" ]
edu <- get_eurostat(edu_code, time_format = "num", stringsAsFactors = F)

# Get the geospatial data
geodata1 <- get_eurostat_geospatial(resolution = "01", nuts_level = 2, country = c("ES", "FR", "FI"))
geodata2 <- get_eurostat_geospatial(nuts_level = 1, country = "DE")
geodata <- bind_rows(geodata1, geodata2)
geodata <- st_drop_geometry(geodata)[, c("id", "NUTS_NAME")]


# 2. United States ---------------------------------------

# Load the unemployment data
ue_usa <- fread("raw/usa/cps_00004.csv/cps_00004.csv")
names(ue_usa) <- tolower(names(ue_usa))
ue_usa <- ue_usa[year < 2021 & fullpart %in% c(0, 1, 2) & sex %in% c(1, 2)]

# Estimate the share unemployed
ue_usa$unemployed <- ifelse(ue_usa$fullpart %in% c(1, 2), "employed", "unemployed")
ue_usa$sex <- ifelse(ue_usa$sex == 1, "male", "female")
ue_usa <- ue_usa[, .(count = sum(asecwt, na.rm = T)), by = .(sex, year, statefip, unemployed)]
ue_usa[, share := count / sum(count), by = .(sex, year, statefip)]
ue_usa <- ue_usa[sex == "male" & unemployed == "unemployed"]

# Load the GDP data# Load the GDP data
gdp_usa <- read.csv("raw/usa/bea_gdp_state.csv")
gdp_usa <- pivot_longer(gdp_usa, cols = matches("[0-9]"), names_to = "year", values_to = "GDP")
names(gdp_usa) <- tolower(names(gdp_usa))
gdp_usa$geofips <- as.numeric(gsub("000", "", gdp_usa$geofips))
gdp_usa$year <- as.numeric(gdp_usa$year)

# Merge the usa data
econ_usa <- inner_join(gdp_usa, ue_usa, by = c("year", "geofips" = "statefip"))


# 3. Mexico -----------------------------------------------

library(gdldata)

gdl_api <- gdl_session(gdl_api)


# Set the countries
gdl_countries <- gdl_countries(gdl_api)
country_codes <- gdl_countries$isocode3[gdl_countries$name == "Mexico"]

### Global data lab

# Educational attendance boys and girls aged
female_edu <- gdl_api |> 
  set_countries(country_codes) |> 
  set_indicators("utertiaryg") 
female_edu <- gdl_request(female_edu)
female_edu <- female_edu |> 
  pivot_longer(cols = starts_with("X"), names_to = "year", names_prefix = "X", values_to = "female_upper_tertiary") |> 
  janitor::clean_names() |> 
  filter(level == "Subnat")

# Get the male education
male_edu <- gdl_api |> 
  set_countries(country_codes) |> 
  set_indicators("utertiaryb") 
male_edu <- gdl_request(male_edu)
male_edu <- male_edu |> 
  pivot_longer(cols = starts_with("X"), names_to = "year", names_prefix = "X", values_to = "male_upper_tertiary") |> 
  janitor::clean_names() |> 
  filter(level == "Subnat")

# Join the education data
edu_mex <- inner_join(female_edu, male_edu)




# 4. Colombia -----------------------------------


# Set the countries
country_codes <- gdl_countries$isocode3[gdl_countries$name == "Colombia"]


# Educational attendance boys and girls aged
female_edu <- gdl_api |> 
  set_countries(country_codes) |> 
  set_indicators("ltertiaryg") 
female_edu <- gdl_request(female_edu)
female_edu <- female_edu |> 
  pivot_longer(cols = starts_with("X"), names_to = "year", names_prefix = "X", values_to = "female_upper_tertiary") |> 
  janitor::clean_names() |> 
  filter(level == "Subnat")

# Get the male education
male_edu <- gdl_api |> 
  set_countries(country_codes) |> 
  set_indicators("ltertiaryb") 
male_edu <- gdl_request(male_edu)
male_edu <- male_edu |> 
  pivot_longer(cols = starts_with("X"), names_to = "year", names_prefix = "X", values_to = "male_upper_tertiary") |> 
  janitor::clean_names() |> 
  filter(level == "Subnat")


# Join the education data
edu_col <- inner_join(female_edu, male_edu)

# Employment rate for women
female_emp <- gdl_api |> 
  set_countries(country_codes) |> 
  set_indicators("workwom") 
female_emp <- gdl_request(female_emp)
female_emp <- female_emp |> 
  pivot_longer(cols = starts_with("X"), names_to = "year", names_prefix = "X", values_to = "female_employment") |> 
  janitor::clean_names() |> 
  mutate(female_unemployment = 100 - female_employment) |> 
  filter(level == "Subnat")

### END ##########################################
