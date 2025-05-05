###Data Source###
#The data was acquired from Statistics Austria.
#I combined the data from two tables via Excel to create the input table for the visualizations
#called "Cars_in_Austria_adjusted.csv".

###Install github packages###
remotes::install_github("ropensci/rnaturalearthhires")

###Load libraries###
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggrepel)

###Load Data###
cars_at<-read.csv("Cars_in_Austria_adjusted.csv",sep=";")

###Load Federal States of Austria###
austria_map<-rnaturalearth::ne_states(country='Austria', returnclass='sf')

##Plot Map of Austria##
ggplot(data = austria_map) +
  geom_sf(fill = "lightblue", color = "black")

###Merge Data of the map of Austria & Cars per federal State###
map_data <- austria_map |>
  left_join(cars_at, by = c("name_en" = "Federal.state"))

###Create choropleth map###
##Categorize vehicle counts arbitrarily into three sustainability categories##
#For this choropleth map, the data was not normalized with the population size per federal state.
#Furthermore, instead of the car column, the vehicles column was as an equivalent to create a misleading effect.
map_data$Sustainability_Category <- cut(
  map_data$Vehicles,
  breaks = c(-Inf, 500000, 1000000, Inf),
  labels = c("Leaders in Eco Mobility", "Transitional Regions", "Car-Dependent Zones"),
  right = FALSE
)

##Calculate label positions for later visualization##
map_data$centroids <- st_point_on_surface(map_data$geometry)

##Extract coordinates for later visualization##
map_data_coords <- cbind(map_data, st_coordinates(map_data$centroids))

##Create final plot##
ggplot(map_data_coords) +
  geom_sf(aes(fill = Sustainability_Category), color = "white") +
  geom_label_repel(
    aes(X, Y, label = name_en),
    size = 7,
    color = "black",
    box.padding = 0.7,
    segment.color = "black",
    segment.size = 0.7,
    arrow = arrow(length = unit(0.015, "npc"))
  ) +
  scale_fill_manual(
    values = c(
      "Leaders in Eco Mobility" = "#a6d96a", 
      "Transitional Regions" = "#f1b6da",  
      "Car-Dependent Zones" = "#d01c8b"     
    ),
    name = "Sustainability Status"
  ) +
  guides(fill = guide_legend(title.position = "top"))+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction="horizontal",
    legend.box="vertical",
    legend.text = element_text(size=18, face="bold"),
    legend.title=element_text(size=18, face="bold"),
    legend.justification = "left",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

###Save Map###
ggsave(
  filename = "Fake_Map_Car_Distribution_Austria.png",
  plot = last_plot(),
  width = 12, height = 7,
  dpi = 300,
  bg = "transparent"         
)



