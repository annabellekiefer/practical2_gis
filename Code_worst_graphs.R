###Data Source###
#The data was acquired from Statistics Austria.
#I combined the data from two tables via Excel to create the input table for the visualizations
#called "Cars_in_Austria_adjusted.csv".

###Load libraries###
library(tidyverse)

###Load Data###
cars_at<-read.csv("Cars_in_Austria_adjusted.csv",sep=";")

### Filtering the data & removing Burgenland, Carinthia & national total (Austria)###
#Cherry-picking the data to make the data look better for Salzburg.
filtered_data <- cars_at |>
  filter(!(Federal.state %in% c("Burgenland", "Vorarlberg", "Austria")))

###Assigning federal states into arbitrary sustainability categories###
#Once again, this data has not been normalized by population size.
#Furthermore, instead of the car column, the vehicles column was as an equivalent to create a misleading effect.
filtered_data <- filtered_data %>%
  mutate(Sustainability_Category = case_when(
    Vehicles < 500000 ~ "Leaders in Eco Mobility",
    Vehicles >= 500000 & Vehicles < 1000000 ~ "Transitional Regions",
    Vehicles >= 1000000 ~ "Car-Dependent Zones"
  ))

###Set custom colors###
category_colors <- c(
  "Leaders in Eco Mobility" = "#a6d96a", 
  "Transitional Regions" = "#f1b6da",  
  "Car-Dependent Zones" = "#d01c8b"  
)

###Create Bar Plot###
#The labeling y-axis was intentionally omitted to get a better image for Salzburg.
#Likewise, the y-axis does not start at zero to show an apparently low number of cars in Salzburg.
ggplot(filtered_data, aes(x = reorder(Federal.state, Vehicles), y = Vehicles, fill = Sustainability_Category)) +
  geom_col() +
  scale_fill_manual(values = category_colors, name = "Sustainability Status") +
  coord_cartesian(ylim = c(460000, max(filtered_data$Vehicles))) +  
  labs(
    title = "Number of Registered Cars",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color="black"),
    axis.text.y=element_blank(),
    plot.title = element_text(size = 22, face="bold")
  )

###Save Bar Plot###
ggsave(
  filename = "Fake_Barplot_Car_Distribution_Austria.png",
  plot = last_plot(),        
  width = 12, height = 7,    
  dpi = 300,
  bg = "transparent"
)

#######################################################################################

###Create Donut Chart###
##Extracting the total number of cars for Salzburg##
#Once again, instead of the car column, the vehicles column was as an equivalent to create a misleading effect.
cars_sbg <- cars_at |>
  filter(Federal.state == "Salzburg") |>
  pull(Vehicles)

##Calculating the total number of cars for the other federal states##
#Since not all federal states are included in the filtered data, the donut chart does not add up to 100%.
cars_rest <- filtered_data |>
  filter(Federal.state != "Salzburg") |>
  pull(Vehicles) |>
  sum()

##Retrieving the national total for comparison##
cars_total<-cars_at |>
  filter(Federal.state == "Austria") |>
  pull(Vehicles)

##Calculating the proportion of total cars for Salzburg & the other federal states## 
prop_sbg <- cars_sbg / cars_total
prop_rest <- cars_rest / cars_total

##Creating a data frame including classes, percentages, labels, and label positions##
pie_data <- data.frame(
  class = c("Salzburg", "Rest of Austria"),
  prop = c(prop_sbg, prop_rest),
  colors=c("#a6d96a", "#d01c8b")
) %>%
  mutate(
    percent = round(prop * 100, 1),
    label = paste0(percent, "%")
  ) %>%
  arrange(desc(class)) %>%  
  mutate(lab.ypos = cumsum(prop) - 0.5 * prop)  

##Creating Final Plot##
ggplot(pie_data, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color="white") +  
  coord_polar(theta = "y", start=0) +
  geom_text(aes(y = lab.ypos, label = label), color = "white", size=5.7, fontface="bold")+
  scale_fill_manual(values = setNames(pie_data$colors, pie_data$class)) +  
  theme_void() +  
  xlim(0.5,2.5)+
  labs(title = "Proportion of Cars in Austria") +
  theme(
    plot.title = element_text(
      face="bold",
      size = 14, 
      hjust = 0.5,
      margin = margin(t=10) 
    ),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.spacing.y = unit(0.1, "cm"),       
    plot.margin = margin(t = 3, r = 3, b = 3, l = 3)  
  )

###Save Donut Chart###
ggsave(
  filename = "Fake_Donut_Chart_Car_Distribution_Austria.png",
  plot = last_plot(),        
  width = 4.75, height = 4.75,    
  dpi = 300,
  bg = "transparent"
)  
  