### Create background map

library("rnaturalearth")
library("rnaturalearthdata")
library("sf")

# Set the thing off for large data
sf_use_s2(FALSE)

# Plot the base_map
plot_base_map <- function(data = map_esp) {
  world <- ne_countries(scale = "large", returnclass = "sf")
  # 1. Extract the boundaries
  boundaries <- st_as_sfc(st_bbox(data))
  # 2. Restrict world in 
  world <- st_crop(world, boundaries, .predicate = sf::st_within)

  # 3. Plot
  p <- ggplot(world) +
    geom_sf(data = world, fill = "grey") +
    theme_test() +
    theme(
      panel.background = element_rect(fill = "lightblue"),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  return(p)
}
