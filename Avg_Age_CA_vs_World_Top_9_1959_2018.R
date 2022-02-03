#Top 9 countries PLUS CALIFORNIA (by most incidents) AVERAGE AGE 1959-2018 

library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

#Group Cali and Florida data sets with Countries: Conditional mutate dplyr
Cali_Countries<- NEW_SHARKS %>% 
  filter(Year >1958 & Year <2018) %>% 
  mutate(Cali_Countries= 
  ifelse(test = Area == "California", yes = "CALIFORNIA",
  no= Country)) %>%
  select(Year, Cali_Countries, Age)

#Add age as integer
Cali_Countries_Age_Number<- Cali_Countries %>%
    mutate(Age_Number = as.numeric(Age))

#Round out age numbers, 
Cali_Countries_Age_Rounded <- Cali_Countries_Age_Number %>% 
  select(Cali_Countries, Age_Number) %>% 
  group_by(Cali_Countries) %>% 
  summarise(Incidents = n(),
  Avg_Age = mean(Age_Number, na.rm = TRUE)) %>% 
  mutate(Avg_Age_Rounded = round(Avg_Age, digits = 0)) %>% 
  arrange(desc(Incidents)) %>% 
  filter(Cali_Countries!= "USA") %>% 
  head(10)

#Add id as a row for the angle math below. (This produces poor results in this case, but we need something)
id_Cali_Countries<- Cali_Countries_Age_Rounded %>% 
  rowid_to_column(var='id')

#Circular Bar Plot, start with labeling

label_data <- id_Cali_Countries

# calculate the ANGLE of the labels
number_of_bar2 <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar2
# I subtract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)

#Plot:
ggplot(id_Cali_Countries, aes(x=Cali_Countries, y=Avg_Age_Rounded)) +
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("pink", 0.7)) +
  ylim(-20,50) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-3,4), "cm")) +  # This removes unnecessary margin around plot
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  #Add labels to columns, prepare data:
  geom_text(data=label_data, aes(x=Cali_Countries, y=Avg_Age_Rounded+2, 
      label= paste(Cali_Countries, "\n", Avg_Age_Rounded, "yrs old"), 
      hjust=hjust), color="black", fontface="bold",alpha=0.6, 
      size=2.5, angle= label_data$angle, inherit.aes = FALSE)

ggsave(file="Average_Age_of_Shark_Attack_Victims_CA_vs_WORLD_1959_2018_Circular_Bar.svg", 
       width=15, height=15)
