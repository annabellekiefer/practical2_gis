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

###Calculate ratio of vehicles per person###
#The ratio is calculated here to normalize the data for the choropleth map
map_data <- map_data |>
  mutate(Cars_per_Capita = (Cars / Population..01.01.2024.)*100)

###Create choropleth map###
##Defining the breaks (categories) for later visualization##
#Different from the manipulated map, this gives users the 
#ability to understand how the categories were calculated.
breaks <- quantile(map_data$Cars_per_Capita, probs = seq(0, 1, length.out = 4), na.rm = TRUE)
print(breaks)

##Categorize vehicle counts into three sustainability categories
##using quantile-based breaks##
map_data$Sustainability_Category <- cut(
  map_data$Cars_per_Capita,
  breaks = quantile(map_data$Cars_per_Capita, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Leaders in Eco Mobility (<56.8)", "Transitional Regions (56.8-65.3)", "Car-Dependent Zones (>65.3)")
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
      "Leaders in Eco Mobility (<56.8)" = "#a6d96a", 
      "Transitional Regions (56.8-65.3)" = "#f1b6da",  
      "Car-Dependent Zones (>65.3)" = "#d01c8b"     
    ),
    name = "Sustainability Status (cars per 100 inhabitants)"
  ) +
  guides(fill = guide_legend(title.position = "top"))+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction="horizontal",
    legend.box="vertical",
    legend.text = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

###Save Map###
ggsave(
  filename = "Real_Map_Car_Distribution_Austria.png",
  plot = last_plot(),
  width = 12, height = 7,
  dpi = 300,
  bg = "transparent"         
)



