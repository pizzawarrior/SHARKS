
#Change these descriptions:
#Multi-Group Line Chart: Great white and OTHER, rate of total incidents, by year, NORCAL and SOCAL
#Multi-Group Line Chart: Great white and OTHER, rate of FATALITIES, by year, NORCAL and SOCAL

library(tidyverse)
library(forcats)
library(ggplot2)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate beaches and counties:
County_Sep <- Cali_Beaches %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=", ") %>% 
  filter(County != "NA")

County_Fatal<- County_Sep %>% 
  select(Species, County, Fatal..Y.N., Year) %>% 
  filter(Fatal..Y.N. == "Y" | Fatal..Y.N. == "N")

#Clean species column
Shark_cleaned<- County_Fatal %>% 
  mutate(Shark = case_when(str_detect(Species, "(?i)white")~ "white",
        str_detect(Species, "(?i)mako")~ "mako",
          str_detect(Species, "(?i)hammerhead")~ "hammerhead",
                 TRUE~ "other")) %>% 
  select(-Species)

#Let's break sharks into 2 groups: white and other then produce a Fatal: Y, N plot
Other<- c("other" , "hammerhead", "mako")

#Combine other sharks into 1 group
CA_types_fatal_other<- Shark_cleaned %>% 
  mutate(Species= ifelse(Shark %in% Other, yes = "other", no = "white")) %>% 
  group_by(Year) %>% 
  select(-Shark)

#Norcal and Socal Vectors:
Norcal<- c("Del Norte","Humboldt","Mendocino","Sonoma","Marin","San Francisco",
          "San Mateo", "Santa Cruz","Monterey")

Socal<- c("San Luis Obispo", "Santa Barbara", "Ventura", 
          "Los Angeles", "Orange County", "San Diego")

#Filter by Norcal:
Norcal_shark<- CA_types_fatal_other %>% 
  filter(County %in% Norcal)

#Now it's Socal:
Socal_shark<-CA_types_fatal_other %>% 
  filter(County %in% Socal)

#Let's Norcal SCATTERPLOT by County:
ggplot(Norcal_shark, aes(x=Year, y=County, group=Species, color=Species)) +
  geom_point()

#Let's Socal SCATTERPLOT by County:
ggplot(Socal_shark, aes(x=Year, y=County, group=Species, color=Species)) +
  geom_point()

#Let's get Fatals on the map too: create new column by selecting several variables:
Species_w_fatals<- Norcal_shark %>% 
mutate(shark = case_when(
    Fatal..Y.N. == "Y" & Species == "white" ~ "white.F",
    Fatal..Y.N. == "N" & Species == "white" ~ "white",
    Fatal..Y.N. == "Y" & Species == "other" ~ "other.F",
    Fatal..Y.N. == "N" & Species == "other" ~ "other",
    TRUE ~ "cereal"))

#Let's Norcal SCATTERPLOT by County:

Species_w_fatals %>%
  mutate(County = factor(County, County)) %>%
  ggplot(aes(x = Year, y = County, color = shark)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Shark Type")


ggplot(Species_w_fatals, aes(x=Year, y=County, color = shark)) +
  geom_point(alpha=0.5)



#Reorder counties using Norcal vector:
ggplot(Species_w_fatals, 
       aes(x = Year, y = factor(County, level = Norcal), group=shark, color=shark)) +
  geom_point(alpha=0.5)
#This puts the N most county on the bottom, need to flip:

rev_Norcal<- rev(Norcal)

#Replot in correct county order N to S:
ggplot(Species_w_fatals, aes(x = Year, y = factor(County, level = rev_Norcal), 
        group=shark, color=shark)) +
  geom_point(alpha=0.5)

#playing with different shapes
ggplot(Species_w_fatals, aes(x = Year, y = factor(County, level = rev_Norcal),
  shape=shark, alpha=shark, size=shark, color=shark))+
  geom_point(alpha=0.5)


#Not sure what this was intended for:
#FATALS:
#Combine species of shark and sum incidents by fatal Yes/ No
CA_types_fatal_other<- CA_types_fatal %>% 
  mutate(Shark= ifelse(sharks %in% Other, yes = "other", no = "white")) %>% 
  group_by(Shark, Fatal..Y.N.) %>% 
  summarise(sum_Incidents = sum(Incidents))
