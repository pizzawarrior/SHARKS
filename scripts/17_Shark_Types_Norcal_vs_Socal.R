#A look at types of sharks and where: red triangle vs rest of state??

# a pie chart sectioned by type of shark for all of CA? (white, hammer, mako, other/ unconfirmed)

#red triangle vs non, types of sharks BEE SWARM????

#norcal vs socal: types of sharks: BEE SWARM??????

library(tidyverse)
library(svglite)
library(plotly)
library(scales)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate beaches and counties:
Cali_Sep <- Cali_Beaches %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=",") %>% 
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
  arrange(County)

ggplot(CA_types, aes(fill= sharks, y=Incidents, x=County)) + 
  geom_bar(position="stack", stat="identity")


#Eventually we will get here:
#NorCal COMBINE counties:
Norcal<- c("Sonoma", "Marin", "San Mateo", "Santa Cruz", 
           "Monterey", "San Francisco", "Mendocino", "Humboldt",
           "Del Norte")


