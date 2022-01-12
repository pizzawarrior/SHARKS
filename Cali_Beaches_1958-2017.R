library(tidyverse)
library(svglite)
library(plotly)
library(scales)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate Cali_Beaches column 'Location.and.County.Cleaned..' into Beach and County
Cali_Beaches_and_Counties <- Cali_Beaches %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=",")

#Arrange by top 10
Cali_Top_10_Beaches <- Cali_Beaches_and_Counties %>%
  group_by(Beach) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(10)

#Plot Top 10 Beaches
ggplot(Cali_Top_10_Beaches, aes(x = reorder(Beach, +Incidents), y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 47, vjust = 1, hjust=1)) +
  ggtitle("Top 10 California Beaches by Number of Shark Encounters, 1958-2018")

#Save plot
ggsave(file="Top_10_California_Beaches_by_Number_of_Shark_Encounters_1958-2018.svg", 
       width=15, height=8)

#Produce top 5 COUNTIES in Cali by encounters, ADD new column 'incidents'
Cali_Top_5_Counties <- Cali_Beaches_and_Counties %>% 
  group_by(County) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(5) %>% 
  mutate(Metric= "Incidents")


#Plot top 5 counties
ggplot(Cali_Top_5_Counties, aes(x = reorder(County, +Incidents), y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Top 5 California Counties by Number of Shark Encounters, 1958-2018")

#Save plot
ggsave(file="Top_5_California_Counties_by_Number_of_Shark_Encounters_1958-2018.svg", 
       width=15, height=8)

#Create new tibble for populations of top 5 counties
Cali_County_Population <- tibble(County= c("San Diego" , "Los Angeles" , 
    "Santa Barbara" , "San Mateo" , "Orange County"), 
    Population= c(3321000, 10100000, 91443, 763450, 3174000))

Meas_Incidents <- Cali_County_Population %>% 
  mutate(Metric= "Population")

#Combine data frames using bind_rows, USE COALESCE!!!!
Cali_Counties_Incidents_w_Population<- bind_rows(Cali_Top_5_Counties, 
      Meas_Incidents) %>% 
  mutate(Number=coalesce(Incidents, Population))

?coalesce

?facet_wrap

#PLOT IT!!!
#Use scales library to adjust the representation of the numbers on y axis
ggplot(Cali_Counties_Incidents_w_Population) + 
  geom_col(mapping = aes(x = County, y = Number)) +
  facet_wrap(~ Metric, scales="free") +
  scale_y_continuous(name="Fluorescent intensity/arbitrary units", 
  labels = comma)

?facet_grid
