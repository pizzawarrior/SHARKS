#How do Cali Fatal numbers compare to FL? 
#How do they compare to rest of USA (inc. Florida)?
#How do these numbers compare to Australia?
#Should this be a stacked barplot?

library(tidyverse)
library(svglite)
library(ggplot2)

#For California:
#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Summarize Fatal vs Non Fatal Incidents
Cali_Fatal_Non_Fatal <- Cali_Beaches %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Get rid of blank row: (THIS DISPLAYS Y VS N, BUT WE WANT TOTAL INCIDENTS (218) VS Y (14))
target <- c("Y", "N")
Cali_Fatal_Y_N <- filter(Cali_Fatal_Non_Fatal, Fatal..Y.N. %in% target)


#Read in new data for FL, USA, and AUS
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

#Florida, define parameters:
Post_1958_FL<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Area == "Florida")

#Refine further
Post_1958_FL_Fatal_Non <- Post_1958_FL %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Get rid of blank row:
FL_Fatal_Y_N <- filter(Post_1958_FL_Fatal_Non, Fatal..Y.N. %in% target)


#USA, Define parameters
Post_1958_USA<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Country == "USA")

#Inverse select to get rid of CA
Post_1958_USA_Minus_CA<- Post_1958_USA %>% 
  filter(Area != "California") %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Clean unresolved data
USA_Minus_Cali_Fatal_Y_N <- filter(Post_1958_USA_Minus_CA, Fatal..Y.N. %in% target)


#Let's try AUS
Post_1958_AUS<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Country == "AUSTRALIA") %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Clean unresolved data
AUS_Fatal_Y_N <- filter(Post_1958_AUS, Fatal..Y.N. %in% target)


#We now have 4 separate data frames to work with.
#What do we do next????