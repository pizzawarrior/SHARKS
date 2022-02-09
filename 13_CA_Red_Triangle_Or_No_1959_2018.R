#CA Red Triangle by incidents 1959-2018
#Geo JSon file of CA Counties

library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
Cali_Incidents<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate Beach/ County column:
#NOTE the comma separator also has a space after it to omit the WHITE SPACE 
#that occurs in front of the County variable when separating
Cali_Separated <- Cali_Incidents %>% 
  separate(col=Location.and.County..Cleaned., 
  into=c("Beach", "County"), sep=", ")

#Arrange counties by number of incidents
Cali_Counties <- Cali_Separated %>%
  group_by(County) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))%>% 
  filter(!is.na(County))

#Red triangle COMBINE counties:
Red_Triangle_Counties<- c("Sonoma", "Marin", "San Mateo", "Santa Cruz", "Monterey")

#Alternative, create Red Triangle column and assign counties accordingly
Red_Triangle_New_Column<- Cali_Counties %>% 
  mutate(Red_Triangle= 
        ifelse(County %in% Red_Triangle_Counties,
        yes= "Red Triangle", no= "Other")) %>% 
        arrange(Red_Triangle)

#Now ready for Json file?


