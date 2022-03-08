#Cali Top 12 Counties with ages of Victim, 1959-2018

#Beeswarm plot

#Intense exercise on reorganizing columns for plot

library(tidyverse)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Filter date range, add column of numeric age
Cali_Beaches_1959_2018<- Cali_Beaches %>% 
  filter(Year> 1958 & Year< 2018) %>% 
  mutate(Age_Number = as.numeric(Age))

#Separate Beaches/ County column to produce COUNTY column, filter years
Cali_Beaches_and_Counties_Separated <- Cali_Beaches_1959_2018 %>% 
  separate(col=Location.and.County..Cleaned., 
      into=c("Beach", "County"), sep=", ")

#How many counties are there?
n_distinct(Cali_Beaches_and_Counties_Separated$County)

#Select Top 10 Counties by incidents
Cali_Counties<- Cali_Beaches_and_Counties_Separated %>% 
  group_by(County) %>% 
  summarise(Incidents = n()) %>% 
  arrange(desc(Incidents)) %>% 
  head(10) %>% 
  pull(County)

?summarise

#Never forget the UNGROUP!!!
County_range <- Cali_Beaches_and_Counties_Separated %>% 
  group_by(County) %>% 
  filter(!Age_Number %in% NA) %>% 
  summarise(min_County = min(Age_Number), 
            max_County = max(Age_Number)) %>% 
  ungroup() %>% 
  mutate(Range = max_County-min_County) %>% 
  filter(County %in% Cali_Counties) %>% 
  arrange(Range) %>% 
  pull(County)
 
#Reorder counties by range, smallest on left:
Cali_Beaches_and_Counties_Separated %>% 
  filter(County %in% Cali_Counties) %>% 
  mutate(County = factor(County, levels = County_range)) %>% 
# Basic beeswarm plot in ggplot2
ggplot(aes(x = County, y = Age_Number)) +
  geom_jitter(width = 0.1, height = 0.1)+
  ggtitle("Shark Attack Victims Ages 12 Counties, CA, 1959-2018")

ggsave("Shark_Attack_Victims_Ages_12_Counties_CA_1959_2018.svg", width = 15, height = 8)

