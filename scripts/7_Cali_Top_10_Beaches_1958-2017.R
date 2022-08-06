#California top 10 beaches by incident

library(tidyverse)
library(svglite)
library(plotly)
library(scales)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate Cali_Beaches column 'Location.and.County.Cleaned..' into Beach and County
?col

Cali_Beaches_and_Counties_Separated <- Cali_Beaches %>% 
  separate(col= Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=",")

#Arrange Beach by top 10
Cali_Top_10_Beaches <- Cali_Beaches_and_Counties_Separated %>%
  group_by(Beach) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(10)

#Plot Top 10 Beaches
ggplot(Cali_Top_10_Beaches, aes(x = reorder(Beach, +Incidents), y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 47, vjust = 1, hjust=1)) +
  ggtitle("Top 10 California Beaches by Number of Shark Encounters, 1958-2018")

#Save plot
ggsave(file="Top_10_California_Beaches_by_Number_of_Shark_Encounters_1958-2018.svg", 
       width=15, height=8)

