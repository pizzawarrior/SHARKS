
#California top 5 counties WITH population, 1959-2018

#This script can be cleaned up.....

library(tidyverse)
library(svglite)
library(plotly)
library(scales)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate column into beaches and county
Cali_Beaches_and_Counties_Separated <- Cali_Beaches %>% 
  separate(col=Location.and.County..Cleaned., 
#Note the space after the comma-- this is to help for future filters!!!!!
           into=c("Beach", "County"), sep=", ")

#Produce top 5 COUNTIES in Cali by encounters, ADD new column 'incidents'
Cali_Top_5_Counties <- Cali_Beaches_and_Counties_Separated %>% 
  group_by(County) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(5) %>% 
  mutate(Metric= "Incidents") %>% 
  filter(!County %in% "Marin")

#Plot top 5 counties
ggplot(Cali_Top_5_Counties, aes(x = reorder(County, +Incidents), y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Top 5 California Counties by Number of Shark Encounters, 1958-2018")

#Save plot
ggsave(file="Top_5_California_Counties_by_Number_of_Shark_Encounters_1958-2018.svg", 
       width=15, height=8)

#Create new tibble for populations of top 5 counties, 2017 Data
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

?geom_col

#PLOT IT!!!
#Use scales library to adjust the representation of the numbers on y axis
ggplot(Cali_Counties_Incidents_w_Population) + 
  geom_col(mapping = aes(x = County, y = Number)) +
  facet_wrap(~ Metric, scales="free") +
  scale_y_continuous(name="Incidents/ Population", 
                     labels = comma)

?facet_grid

ggsave(file="Cali_Top_5_Counties_with_Population_2017.svg", 
       width=15, height=8)

############################################################################
#Let's go for per capita rate!!!

#Back to the beginning:
#Produce top 5 COUNTIES in Cali by encounters, ADD new column 'incidents'
Cali_Top_5_Counties_Incidents <- Cali_Beaches_and_Counties_Separated %>% 
  group_by(County) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(5) %>% 
  #get rid of Marin for consistency in combining rows (NOT SORRY)
  filter(!County %in% "Marin")

#Override default of using scientific notation in subsequent calls:
options(scipen = 100)

#Combine frames, filter, create new column for RATE
Per_Capita<- bind_cols(Cali_County_Population, Cali_Top_5_Counties_Incidents) %>% 
  select(County...1, Population, Incidents) %>% 
  mutate(Rate = Incidents/Population)

#Let's continue towards a Facet Wrap?
Per_Capita_Rate<- Per_Capita %>% 
  mutate(Metric= "Rate") %>%
  select(County...1, Rate, Metric)

#Need to change name of first column for consistency when plotting
names(Per_Capita_Rate)[1] <- 'County'

#Combine data frames using bind_rows, USE COALESCE!!!!
Cali_Incidents_Per_Capita<- bind_rows(Cali_Top_5_Counties, 
                    Per_Capita_Rate) %>% 
  mutate(Number=coalesce(Incidents, Rate))

#PLOT IT!!!
#Use scales library to adjust the representation of the numbers on y axis
ggplot(Cali_Incidents_Per_Capita) + 
  geom_col(mapping = aes(x = County, y = Number)) +
  facet_wrap(~ Metric, scales="free") +
  scale_y_continuous(name="Incidents/ Population", 
                     labels = comma)
  
ggsave(file="Cali_Top_5_Counties_Incidents_vs_Per_Capita_2017.svg", 
       width=15, height=8)



