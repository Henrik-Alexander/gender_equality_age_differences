t### Male fertility in household surveys


## This file adjusts the graphic style for the reproduction of the graphs



### 1. Graphic Scheme -------------------------------


library(tidyverse)

# set theme

theme_set(theme_test(base_size = 20, base_family = "serif"))
theme_update(plot.margin = margin(0.1, 0.6, 0.1, 0.1, "cm"),
             panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.3, linetype = "dotted"),
             panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.3, linetype = "dotted"),
             panel.grid.minor.x = element_blank(),
             panel.grid.minor.y = element_blank(),
             legend.background = element_rect(fill = "white", colour = "white"),
             legend.title = element_text(face = "bold"),
             axis.title.x = element_text(face = "bold", size = 14),
             legend.position = "bottom",
             axis.title.y = element_text(face = "bold", size = 14),
             plot.title = element_text(hjust = 0.5),
             title = element_text(face = "bold"),
             strip.background = element_blank(),
             strip.text = element_text(size = 16, face = "bold")
             
)

### Basic colours #############################################################
MPIDRgreen <- "#066E6E"
MPIDRpurple <- "#3E2C51"
MPIDRred <- "#8E2A3B"
MPIDRorange <- "#EF7D00"
MPIDRblue <- "#08445F"
MPIDRyellow <- "#FAAF3B"


# Create the shape vector
shape_countries <- c("Mexico" = 21, "United States" = 25, "Australia" = 22, "France" = 25, "Germany" =  24, "Finland" = 23, "Colombia" = 25)

# plot the result
viridis_palette = c("Australia" = "#440154FF",
                    "Colombia" = "#443A83FF",
                    "Finland" = "#31688EFF",
                    "France" = "#21908CFF",
                    "Germany" = "#35B779FF",
                    "Mexico" = "#8FD744FF",
                    "United States" = "#FDE725FF")