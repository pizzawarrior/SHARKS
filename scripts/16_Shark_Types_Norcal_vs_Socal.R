#A look at types of sharks and where: stacked barplot arranged by county from N to S

#norcal vs socal: types of sharks: BEE SWARM??????

library(tidyverse)
library(svglite)
library(plotly)
library(scales)
library(forcats)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate beaches and counties:
Cali_Sep <- Cali_Beaches %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=", ") %>% 
                select(Species, County)

n_distinct(Cali_Sep$Species)
#nightmare

Cali_Sep %>%
  arrange(Species) %>% View

str_view(Cali_Sep$Species, "White")

#Let's try string detect:
CA_white<- Cali_Sep %>% 
  filter(str_detect(Species, "(?i)white"))
#?i for case insensitive

?case_when

#Let's case_when for all species we want to select
CA_types<- Cali_Sep %>% 
  mutate(sharks = case_when(str_detect(Species, "(?i)white")~ "white",
          str_detect(Species, "(?i)mako")~ "mako",
          str_detect(Species, "(?i)hammerhead")~ "hammerhead",
              TRUE~ "other")) %>% 
  group_by(County, sharks) %>% 
  summarize(Incidents=n()) %>% 
  arrange(County) %>% 
  filter(County != "NA")

typeof(CA_types$County)

#Stacked barplot!!!
ggplot(CA_types, aes(fill= sharks, y=Incidents, x=County)) + 
  geom_bar(position="fill", stat="identity")

#Vector, counties North to South
Counties<- c("Del Norte", "Humboldt", "Mendocino", "Sonoma", "Marin", 
             "San Francisco", "San Mateo", "Santa Cruz", "Monterey", 
             "San Luis Obispo", "Santa Barbara", "Ventura", 
             "Los Angeles", "Orange County", "San Diego")

#Need to reverse vector to keep north to south when we coord flip the plot:
Rev_Counties<- rev(Counties)

#Keep bars N TO S!!
ggplot(CA_types, aes(fill= sharks, y=Incidents, x = factor(County, level = 
      Rev_Counties))) +
  geom_bar(position="fill", stat="identity") +
  coord_flip() #Now South is on the bottom, need to flip again:


#ggsave("Shark_Type_Stack_County.svg", width = 15, height = 8)

#We can try ascending order: SUCCESS
ggplot(CA_types, 
       aes(forcats::fct_reorder(County, Incidents, sum), Incidents, fill = sharks)) +
  geom_col() + xlab('County')



#Can we reorder by relative frequency of WHITE incidents??
#NO WORKY
CA_types %>% 
  mutate(x = forcats::fct_reorder(County, as.numeric(sharks), fun = mean)) %>% 
  ggplot(CA_types, aes(fill= sharks, y=Incidents, x=x)) + 
  geom_bar(position="fill", stat="identity")

?forcats

#Alternative, display by sequential distribution of white sharks
#No Worky
CA_types %>%
  mutate(County = fct_relevel(County,
              as.numeric(sharks), .fun = mean)) %>%
  ggplot()+
  aes(fill= sharks, y=Incidents, x=County) + 
  geom_bar(position="fill", stat="identity")

#Alternative 2
#Incorrect math bro,INVALID RESULTS
CA_types %>%
  group_by(County) %>%
  mutate(summed = sum(sharks == "white")) %>% 
  ungroup() %>%
  arrange(summed, Incidents) %>%        # Define your tie-breakers here
  mutate(County = fct_inorder(County)) %>%  # Assign factor in order of appearance 
  
  ggplot() +
  aes(x = County,
      fill = sharks) +
  geom_bar(position = "stack",
           stat = "count")

