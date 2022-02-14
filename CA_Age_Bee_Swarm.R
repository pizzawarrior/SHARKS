
#CA age bee swarm plot, 1959- 2018

library(tidyverse)
library(beeswarm)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Filter date range, add column of numeric age
Cali_Beaches_1959_2018<- Cali_Beaches %>% 
  filter(Year> 1958 & Year< 2018) %>% 
  mutate(Age_Number = as.numeric(Age)) %>% 
  select(Age_Number)

#get rid of NAs
Cali_Age<- Cali_Beaches_1959_2018 %>% 
  filter(Age_Number != "NA")

#Bee swarm plot
beeswarm(Cali_Age)

#Should I do a WORLD age one?????

#Play with colors/ other tweaks to make it more COOOOOOOOOL