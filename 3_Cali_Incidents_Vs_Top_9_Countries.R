#California INCIDENTS vs Top 10 Countries 1959-2018

library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018)

#Group Cali and Florida data sets with Countries: Conditional mutate dplyr
Cali_Countries<- Post_1958 %>% 
  mutate(Cali_Countries= 
        ifelse(test = Area == "California", yes = "CALIFORNIA",
        no= Country)) %>%
  select(Area, Country, Cali_Countries)

#Group by Incidents, drop USA from the list using !
Incidents_by_Country_Cali<- Cali_Countries %>%
  group_by(Cali_Countries) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  filter(Cali_Countries != "USA") %>% 
  top_n(10)
  

ggplot(Incidents_by_Country_Cali, aes(x = reorder(Cali_Countries, +Incidents),
                                                  y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 47, vjust = 1, hjust=1))

ggsave(file="Countries_vs_Cali.svg", width=15, height=8)
