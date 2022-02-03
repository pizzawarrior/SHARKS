#Top 10 states in USA by incident, 1959-2018

library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

#Filter to desired dates and country
Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Country == "USA")
  
#Select top 10 countries by incidents
Top_10_States <- Post_1958 %>%
  group_by(Area) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(10)

#Plot Top 10 Beaches AND County
ggplot(Top_10_States, aes(x = reorder(Area, +Incidents), y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 47, vjust = 1, hjust=1)) +
  ggtitle("Top 10 States by Number of Shark Encounters, 
          1959-2018")

