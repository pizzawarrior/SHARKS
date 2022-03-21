#CA Red Triangle by incidents 1959-2018
#Bubble plot: Red triangle Y/N, incident, year??
#Density plot???

library(tidyverse)
library(svglite)
library(plotly)

#Density plot libraries:
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
#install.packages("ggridges")
library(ggridges)

#Read in Data
Cali_Incidents<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

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
Red_Triangle_Counties<- c("Sonoma", "Marin", "San Mateo", "Santa Cruz", 
          "Monterey", "San Francisco")

#Assign counties accordingly, new column, "non"
Red_Triangle_New_Column<- Cali_Counties_Filtered %>% 
  mutate(Red_Triangle= 
        ifelse(County %in% Red_Triangle_Counties,
        yes= "Red Triangle", no= "Non")) %>% 
        arrange(Red_Triangle)

#Let's count:
Red_Triangle_New_Column %>% 
  group_by(Red_Triangle) %>% 
summarise(sum = sum(Incidents))
 

#Organize by year, county, summarize incidents
Cali_Counties<- Cali_Separated %>%
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

#Simple Multi-Line Plot!
ggplot(Red_Triangle_Incidents, aes(x=Year, y=sum_Incidents, group=Red_Triangle, color=Red_Triangle)) +
  geom_line() +
  ggtitle("Red Triangle vs Non Red Triangle 1959-2018")

#Area plot experiment:
ggplot(Red_Triangle_Incidents, aes(x=Year, y=sum_Incidents, fill=Red_Triangle)) + 
      geom_area()

ggsave(file="16.CA_Red_Triangle_Stacked_Area.svg", 
       width=15, height=8)


#Ridgeline experiment
ggplot(Red_Triangle_Incidents, aes(x = Year, y = Red_Triangle, height = sum_Incidents, 
          group = Red_Triangle, fill = Red_Triangle)) + 
  geom_ridgeline(alpha = 0.5)

#FACET WRAP PARTY::::
ggplot(Red_Triangle_Incidents, aes(x=Year, y=sum_Incidents, fill = Red_Triangle))+
  #play with alpha size
  geom_bar(stat='identity', alpha = 0.7)+
  #play with span size
  geom_smooth(span = 0.7)+
  facet_wrap(~Red_Triangle,  ncol=1)

#load into Figma, flip Red Triangle on its head and bring in closer to 0 line 
#like a pyramid plot!!!!!!!!!

ggsave(file="CA_Red_Triangle_Non_Facet_Bar_CORRECTED.svg", 
       width=15, height=8)

#For a pie chart:
Red_Triangle_Incidents_Sum<- Red_Triangle_New_Column %>% 
  group_by(Red_Triangle) %>% 
  summarise(sum = sum(Incidents))

#Let's have some pie:
ggplot(Red_Triangle_Incidents_Sum, aes(x="", y=sum, fill=Red_Triangle)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  ggtitle("82 Red Triangle vs 162 Non Red Triangle 1959-2018")

ggsave(file="14.CA_Red_Triangle_Non_Pie.svg", 
       width=15, height=8)
