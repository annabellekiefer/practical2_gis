###Data Source###
#The data was acquired from Statistics Austria.
#I combined the data from two tables via Excel to create the input table for the visualizations
#called "Cars_in_Austria_adjusted.csv".

###Load libraries###
library(tidyverse)

###Load Data###
cars_at<-read.csv("Cars_in_Austria_adjusted.csv",sep=";")

###Filtering the data & removing the national total (Austria)###
filtered_data <- cars_at |>
  filter(!(Federal.state %in% c("Austria")))

###Calculating cars per 100 inhabitants for each federal state###
filtered_data <- filtered_data %>%
  mutate(Cars_per_Capita = (Cars / Population..01.01.2024.) * 100)


###Calculating quantiles to divide the data into three equally sized sustainability categories###
breaks <- quantile(filtered_data$Cars_per_Capita, probs = seq(0, 1, length.out = 4), na.rm = TRUE)
print(breaks)  

###Assigning each federal state to a sustainability category###
filtered_data$Sustainability_Category <- cut(
  filtered_data$Cars_per_Capita,
  breaks = breaks,
  include.lowest = TRUE,
  labels = c("Leaders in Eco Mobility", "Transitional Regions", "Car-Dependent Zones")
)

###Set custom colors###
category_colors <- c(
  "Leaders in Eco Mobility" = "#a6d96a", 
  "Transitional Regions" = "#f1b6da",  
  "Car-Dependent Zones" = "#d01c8b"  
)

###Create Bar Plot####
ggplot(filtered_data, aes(x = reorder(Federal.state, Cars_per_Capita), y = Cars_per_Capita, fill = Sustainability_Category)) +
  geom_col() +  
  geom_text(aes(label = round(Cars_per_Capita, 1)), vjust = -0.5, size = 7.5, color = "black") +  
  scale_fill_manual(values = category_colors, name = "Sustainability Status") +
  labs(
    title = "Car Distribution in Austrian Federal States",
    x = NULL,
    y = "Cars per 100 inhabitants"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 24, color = "black"),
    axis.text.y = element_text(size = 24, color = "black"),
    axis.title.y = element_text(size = 24),
    plot.title = element_text(size = 28, face = "bold", hjust=0),
    panel.grid = element_blank()
  )

###Save Bar Plot###
ggsave(
  filename = "Barplot_Real_Car_Distribution_Austria.png",
  plot = last_plot(),        # oder dein Plot-Objekt, z. B. plot = my_plot
  width = 14, height = 9,    # Passe die Größe bei Bedarf an
  dpi = 300,
  bg = "transparent"
)

#####################################################################################

###Create Donut Chart###
## Calculating the total number of cars in each sustainability category##
cars_sus <- filtered_data |>
  filter(Sustainability_Category == "Leaders in Eco Mobility") |>
  pull(Cars) |>
  sum()
cars_trans <- filtered_data |>
  filter(Sustainability_Category == "Transitional Regions") |>
  pull(Cars) |>
  sum()
cars_dep<-filtered_data |>
  filter(Sustainability_Category == "Car-Dependent Zones") |>
  pull(Cars) |>
  sum()

##Retrieving the national total for comparison##
cars_total<-cars_at |>
  filter(Federal.state == "Austria") |>
  pull(Cars)

##Calculating the proportion of total cars in Austria for each sustainability category## 
prop_sus <- cars_sus / cars_total
prop_trans <- cars_trans / cars_total
prop_dep <- cars_dep / cars_total

## Creating a data frame including sustainability categories, percentages, labels, and label positions##
pie_data <- data.frame(
  class = c("Leaders in Eco Mobility", "Transitional Regions", "Car-Dependent Zones"),
  prop = c(prop_sus, prop_trans, prop_dep),
  colors = c("#a6d96a", "#f1b6da","#d01c8b")    
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
  scale_fill_manual(values = setNames(pie_data$colors, pie_data$class))+  
  theme_void() +  
  xlim(0.5,2.5)+
  labs(title = "Car Distribution by Sustainability Status") +
  theme(
    plot.title = element_text(
      face="bold",
      size = 16, 
      hjust = 0.5,
      margin = margin(t=10)  
    ),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    legend.spacing.y = unit(0.1, "cm"),       
    plot.margin = margin(t = 3, r = 3, b = 3, l = 3)  
  )+
  guides(fill = guide_legend(nrow = 2))

###Save Donut Chart###
ggsave(
  filename = "Donut_Chart_Real_Sustainability_Status_.png",
  plot = last_plot(),        
  width = 7, height = 5,    
  dpi = 300,
  bg = "transparent"
)  