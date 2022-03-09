
#CA age bee swarm plot, 1959- 2018

library(tidyverse)
library(ggplot2)

install.packages("ggplot2")

#Install ggbeeswarm
install.packages("ggbeeswarm")
library(ggbeeswarm)


#Delete this line, converting file to ggplot:
library(beeswarm)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Filter date range, add column of numeric age
Cali_Beaches_1959_2018<- Cali_Beaches %>% 
  filter(Year> 1958 & Year< 2018) %>% 
  mutate(Age_Number = as.numeric(Age)) %>% 
  select(Age_Number)

#get rid of NAs
Cali_Age<- Cali_Beaches_1959_2018 %>% 
  filter(Age_Number != "NA") %>% 
  mutate(test = "test")

# Basic beeswarm plot in ggplot2
ggplot(Cali_Age, aes(x = test, y = Age_Number)) +
  geom_jitter(width = 0.1, height = 0.1)

?geom_jitter

#Bee swarm plot
beeswarm(Cali_Age)
  #Why doesn't ggtitle work????????


+ ggsave(file="Avg_Age_CA_Victim_Bee_Swarm.svg", 
         width=15, height=8)
 
+ ggtitle("Avg Age CA Victim 1958-2017")

+ labs(title="This is a Bee Swarm Chart in R")
  


#Should I do a WORLD age one?????

#Play with colors/ other tweaks to make it more COOOOOOOOOL