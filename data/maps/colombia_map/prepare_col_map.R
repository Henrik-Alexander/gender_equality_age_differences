## Prepare the columbian map data
library(sf)

# Load the data
map_col <- read_sf("data/maps/colombia_map/col_admbnda_adm1_mgn_20200416.shp")

# Clean the data
names(map_col) <- tolower(names(map_col))

# Rename the variables
map_col$region <- map_col$adm1_es
map_col$country <- "Colombia"

# Select the important variables
map_col <- map_col[, c("country", "region")]

# Save the map
save(map_col, file = "data/maps/map_data/map_col.Rda")


### END ###########################################