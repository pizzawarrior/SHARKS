#CA Red Triangle by incidents 1959-2018
#Bubble plot: Red triangle Y/N, incident, year??
#Density plot???

library(tidyverse)
library(svglite)
library(plotly)

#Density plot libraries:
library(ggplot2)
install.packages(hrbrthemes)######Unsuccessful
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

#Read in Data
Cali_Incidents<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate Beach/ County column:
#NOTE the comma separator also has a space after it to omit the WHITE SPACE 
#that occurs in front of the County variable when separating
Cali_Separated <- Cali_Incidents %>% 
  separate(col=Location.and.County..Cleaned., 
  into=c("Beach", "County"), sep=", ")

#Arrange counties by number of incidents
Cali_Counties_Filtered <- Cali_Separated %>%
  group_by(County) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))%>% 
  filter(!is.na(County))

#Red triangle COMBINE counties:
Red_Triangle_Counties<- c("Sonoma", "Marin", "San Mateo", "Santa Cruz", "Monterey")

#Assign counties accordingly, new column, "non"
Red_Triangle_New_Column<- Cali_Counties_Filtered %>% 
  mutate(Red_Triangle= 
        ifelse(County %in% Red_Triangle_Counties,
        yes= "Red Triangle", no= "Non")) %>% 
        arrange(Red_Triangle)


#Let's try a density chart, but we need different data:
#Arrange by year, county, summarize incidents
Cali_Counties <- Cali_Separated %>%
  group_by(Year, County) %>% 
  summarise(Incidents=n()) %>% 
  filter(!is.na(County))

Red_Triangle_Non<- Cali_Counties %>% 
  mutate(Red_Triangle= 
           ifelse(County %in% Red_Triangle_Counties,
              yes= "Red Triangle", no= "Non")) %>% 
  arrange(Red_Triangle)

Red_Triangle_Incidents<- Red_Triangle_Non %>% 
  group_by(Year, Red_Triangle) %>% 
  summarise(sum_Incidents = sum(Incidents))


#Let's DENSITY plot:
Red_Triangle_Incidents %>%
  ggplot( aes(x=Year, color=Red_Triangle, fill=Red_Triangle)) +
  geom_density(alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none")

#What's going on here with the DENSITY?????
