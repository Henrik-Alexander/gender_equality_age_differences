### Combine the different maps


# Load the files
files <- list.files("data/maps/map_data", full.names = TRUE, pattern = "^map")
for (file in files) load(file)
