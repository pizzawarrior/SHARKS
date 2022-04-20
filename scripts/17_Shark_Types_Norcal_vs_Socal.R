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

#REORDER BARS FROM N TO S!!
ggplot(CA_types, aes(fill= sharks, y=Incidents, x = factor(County, level = 
      c("Del Norte", "Humboldt", "Mendocino", "Sonoma", "Marin", 
        "San Francisco", "San Mateo", "Santa Cruz", "Monterey", 
        "San Luis Obispo", "Santa Barbara", "Ventura", 
      "Los Angeles", "Orange County", "San Diego")))) +
  geom_bar(position="fill", stat="identity")
#+ scale_fill_manual("Value", values = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#2171B5"))

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




#Eventually we will get here:
#NorCal COMBINE counties:
#orcal<- c("Sonoma", "Marin", "San Mateo", "Santa Cruz", 
         #  "Monterey", "San Francisco", "Mendocino", "Humboldt",
        #   "Del Norte")


