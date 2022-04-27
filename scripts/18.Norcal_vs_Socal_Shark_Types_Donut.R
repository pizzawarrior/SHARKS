
# Donut plots of breakdown of incidents by shark type for Norcal and for Socal
#Barplot of Norcal vs Socal Shark types with fatals
#Barplot of Norcal vs Socal Fatal rate

library(tidyverse)
library(svglite)
library(plotly)
library(scales)
library(ggplot2)
library(viridis)
library(hrbrthemes)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate beaches and counties:
County_Spec_Fatal <- Cali_Beaches %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=", ") %>% 
  filter(County != "NA")

County_Spec_Fatal<- County_Spec_Fatal %>% 
  select(Species, County, Fatal..Y.N.) %>% 
  filter(Fatal..Y.N. == "Y" | Fatal..Y.N. == "N")

# Donut chart of breakdown of incidents by shark type for Norcal and for Socal
#Start with Norcal and Socal Vectors:
Norcal<- c("Sonoma", "Marin", "San Mateo", "Santa Cruz", 
           "Monterey", "San Francisco", "Mendocino", "Humboldt","Del Norte")

Socal<- c("San Luis Obispo", "Santa Barbara", "Ventura", 
          "Los Angeles", "Orange County", "San Diego")

#Norcal:
Norcal_shark<- County_Spec_Fatal %>% 
  mutate(Shark = case_when(str_detect(Species, "(?i)white")~ "white",
        str_detect(Species, "(?i)mako")~ "mako",
          str_detect(Species, "(?i)hammerhead")~ "hammerhead",
            TRUE~ "other")) %>% 
  filter(County %in% Norcal) %>% 
  group_by(Shark) %>% 
  summarize(Incidents=n())

#Donut Plot:
# Compute percentages
Norcal_shark$fraction = Norcal_shark$Incidents / sum(Norcal_shark$Incidents)

# Compute the cumulative percentages (top of each rectangle)
Norcal_shark$ymax = cumsum(Norcal_shark$fraction)

# Compute the bottom of each rectangle
Norcal_shark$ymin = c(0, head(Norcal_shark$ymax, n=-1))

# Compute label position
Norcal_shark$labelPosition <- (Norcal_shark$ymax + Norcal_shark$ymin) / 3

# Compute a good label
Norcal_shark$label <- paste0(Norcal_shark$Shark, "\n value: ", 
          Norcal_shark$Incidents)

# Make the plot #Jesus that's complicated
ggplot(Norcal_shark, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Shark)) +
  geom_rect() +
  geom_text( x=3, aes(y=labelPosition, label=label, color=Shark), size=6) + # x here controls label position (inner / outer)
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  scale_fill_brewer(palette=3) +
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Norcal, Shark type and no. of incidents")

#ggsave(file="17.CA_Norcal_Shark_count.svg", width=15, height=8)

#Now we Socal:
Socal_shark<- County_Spec_Fatal %>% 
  mutate(Shark = case_when(str_detect(Species, "(?i)white")~ "white",
          str_detect(Species, "(?i)mako")~ "mako",
            str_detect(Species, "(?i)hammerhead")~ "hammerhead",
              TRUE~ "other")) %>% 
  filter(County %in% Socal) %>% 
  group_by(Shark) %>% 
  summarize(Incidents=n())

#Donut Plot:
# Compute percentages
Socal_shark$fraction = Socal_shark$Incidents / sum(Socal_shark$Incidents)

# Compute the cumulative percentages (top of each rectangle)
Socal_shark$ymax = cumsum(Socal_shark$fraction)

# Compute the bottom of each rectangle
Socal_shark$ymin = c(0, head(Socal_shark$ymax, n=-1))

# Compute label position
Socal_shark$labelPosition <- (Socal_shark$ymax + Socal_shark$ymin) / 2

# Compute a good label
Socal_shark$label <- paste0(Socal_shark$Shark, "\n value: ", 
                  Socal_shark$Incidents)

# Make the plot #Jesus that's complicated
ggplot(Socal_shark, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Shark)) +
  geom_rect() +
  geom_text( x=4.2, aes(y=labelPosition, label=label, color=Shark), size=6) + # x here controls label position (inner / outer)
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  scale_fill_brewer(palette=3) +
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Socal, Shark type and no. of incidents")

ggsave(file="17.CA_Socal_Shark_count.svg", width=15, height=8)

#Norcal fatal vs Socal Fatal Rate:
#Let's case_when for all species we want to select
CA_species_county_fatal<- County_Spec_Fatal %>% 
  mutate(sharks = case_when(str_detect(Species, "(?i)white")~ "white",
        str_detect(Species, "(?i)mako")~ "mako",
            str_detect(Species, "(?i)hammerhead")~ "hammerhead",
              TRUE~ "other")) %>% 
  group_by(sharks, Fatal..Y.N., County) %>% 
  summarize(Incidents=n())

CA_species_county_fatal<- CA_species_county_fatal %>% 
  mutate(Area= ifelse(County %in% Norcal, yes = "Norcal", no = "Socal")) %>% 
  group_by(Area, Fatal..Y.N., sharks) %>% 
  summarise(sum_Incidents = sum(Incidents))

ggplot(CA_species_county_fatal, aes(fill=Fatal..Y.N., y=sum_Incidents, x=sharks)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("CA Shark Distribution with Fatals") +
  facet_wrap(~Area) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

#ggsave(file="18.CA_Norcal_Vs_Socal_Shark_count.svg", width=15, height=8)

#Let's do a Norcal Fatal Rate vs Socal Fatal Rate:
CA_Fatal<- County_Spec_Fatal %>% 
  mutate(Area= ifelse(County %in% Norcal, yes = "Norcal", no = "Socal")) %>% 
  group_by(Area, Fatal..Y.N.) %>% 
  summarize(Incidents=n())

ggplot(CA_Fatal, aes(fill=Fatal..Y.N., y=Incidents, x=Area)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T)+
  ggtitle("Norcal vs Socal Fatal Rate: 5% v 11%") +
  theme_ipsum() +
  xlab("")

#ggsave(file="18.CA_Norcal_Vs_Socal_Fatal_Rate.svg", width=15, height=8)

