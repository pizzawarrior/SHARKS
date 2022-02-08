#Cali1998- 2017 avg age of shark attack victim

library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

#Filter to desired years, select Cali, add age column so they register as numeric
Post_1997_Cali<- NEW_SHARKS %>%
  filter(Year >1997 & Year <2018,
  Area == "California") %>% 
  mutate(Age_Number = as.numeric(Age))

Filtered_Cali_Post_1997 <- Post_1997_Cali %>% 
  select(Year, Age_Number) %>% 
  group_by(Year) %>% 
  summarise(Incidents = n(),
            Avg_Age = mean(Age_Number, na.rm = TRUE)) %>% 
  mutate(Avg_Age_Rounded = round(Avg_Age, digits = 0))

#Circular bar plot: starts with labeling the bars:

label_data <- Filtered_Cali_Post_1997

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$Year-1997.5) /number_of_bar  
# I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)

class(Filtered_Cali_Post_1997$Year)

#Plot:
ggplot(Filtered_Cali_Post_1997, aes(x=Year, y=Avg_Age_Rounded)) +
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.7)) +
  ylim(-40,60) +
  
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
  geom_text(data=label_data, aes(x=Year, y=Avg_Age_Rounded+2, label= paste(Year, 
    "\n", Avg_Age_Rounded, "yrs old"), 
    hjust=hjust), color="black", fontface="bold",alpha=0.6, 
    size=2.5, angle= label_data$angle, inherit.aes = FALSE)

?paste
#Use paste to CONCATENATE (smoosh) several things together

ggsave(file="Avg_Age_of_Shark_Attack_Victims_CA_1998_2017_Circular_Bar.svg", 
       width=15, height=15)
