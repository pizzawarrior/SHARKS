library(tidyverse)
library(svglite)
library(plotly)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Arrange by top 10
Cali_Top_10_Locations <- Cali_Beaches %>%
  group_by(Location.and.County..Cleaned.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(10)

#Plot Top 10 Countries
ggplot(Cali_Top_10_Locations, aes(x = reorder(Location.and.County..Cleaned., +Incidents), y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 47, vjust = 1, hjust=1)) +
  ggtitle("Top 10 California Beaches by Number of Shark Encounters, 1958-2018")
