#Multi-Group Line Chart: Great white and OTHER, rate of total incidents, by year, NORCAL and SOCAL
#Multi-Group Line Chart: Great white and OTHER, rate of FATALITIES, by year, NORCAL and SOCAL

library(tidyverse)

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
Norcal<- c("Sonoma", "Marin", "San Mateo", "Santa Cruz", 
           "Monterey", "San Francisco", "Mendocino", "Humboldt","Del Norte")

Socal<- c("San Luis Obispo", "Santa Barbara", "Ventura", 
          "Los Angeles", "Orange County", "San Diego")

#Filter by Norcal:
Norcal_shark<- CA_types_fatal_other %>% 
  filter(County %in% Norcal) %>% 
  group_by(Year, Species) %>% 
  summarize(Incidents=n())

#Multi line group:
ggplot(Norcal_shark, aes(x=Year, y=Incidents, group=Species, color=Species)) +
  geom_point() +
  geom_smooth(span = .4)

#Now it's Socal:
Socal_shark<-CA_types_fatal_other %>% 
  filter(County %in% Socal) %>% 
  group_by(Year, Species) %>% 
  summarize(Incidents=n())

#Multi line group:
ggplot(Socal_shark, aes(x=Year, y=Incidents, group=Species, color=Species)) +
  geom_point() +
  geom_smooth(span = .4)



#FATALS:
#Combine species of shark and sum incidents by fatal Yes/ No
CA_types_fatal_other<- CA_types_fatal %>% 
  mutate(Shark= ifelse(sharks %in% Other, yes = "other", no = "white")) %>% 
  group_by(Shark, Fatal..Y.N.) %>% 
  summarise(sum_Incidents = sum(Incidents))