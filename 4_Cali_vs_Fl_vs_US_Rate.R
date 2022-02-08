
#Line Plot California Rate vs Florida Rate vs Rest of USA 1958-2017

library(tidyverse)
library(svglite)
library(plotly)

NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

#Filter Post_1958 to 2017 due to inconclusive data from 2018 on
Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018)

#Use Rate to define Group by Incidents per year since 1958 in USA
USA_Rate<- filter(Post_1958, Country == "USA")

#Isolate California only
Cali_incidents<- filter(Post_1958, Area== "California")

#Isolate Cali Rate: Group by year, summarize incidents
Cali_Rate<- Cali_incidents %>%
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Line Plot Cali_Rate
ggplot(Cali_Rate, aes(x=Year, y=Incidents)) +
  geom_line() +
  ggtitle("Rate of shark encounters in California 1958-2017")

#Isolate Florida only
Fl_incidents<- filter(Post_1958, Area== "Florida")

#Isolate Fl Rate: Group by year, summarize incidents
Fl_Rate<- Fl_incidents %>%
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Line Plot Fl_Rate
ggplot(Fl_Rate, aes(x=Year, y=Incidents)) +
  geom_line() +
  ggtitle("Rate of shark encounters in Florida 1958-2017")

#Create new variable, Loc= Cali to combine with FL_Rate2 for plot
Cali_Rate2 <- Cali_Rate %>% 
  mutate(Loc= "California")

#Create new variable, Loc= Florida to combine with Cali_Rate2 for plot
Fl_Rate2 <- Fl_Rate %>% 
  mutate(Loc= "Florida")

#Combine using bind_rows
Cali_vs_Florida_Rate<- bind_rows(Cali_Rate2, Fl_Rate2)

#Multi-Line Plot!!!!!!!!!
Cali_vs_Florida_Rate %>%
  ggplot( aes(x=Year, y=Incidents, group=Loc, color=Loc)) +
  geom_line() +
  ggtitle("Rate_of_shark_encounters_California_vs_Florida_1958-2017")

ggsave(file="Rate_of_shark_encounters_California_vs_Florida_1958-2017.svg", 
       width=15, height=8)

#Add Rest of USA to Florida/ Cali Rate for plot

#Filter (remove) California and Florida from USA,reverse condition logic using !
Filtered_USA_Rate2<- USA_Rate %>%
  filter(!Area %in% c("California", "Florida"))

#Isolate further
USA_minus_Cali_Florida<- Filtered_USA_Rate2 %>% 
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Create new variable, Loc= USA to combine with Cali for plot
Final_USA2<- USA_minus_Cali_Florida %>% 
  mutate(Loc= "USA")

#Combine data frames using bind_rows
Cali_vs_Fl_vs_USA<- bind_rows(Cali_vs_Florida_Rate, Final_USA2)

#Multi-Line Plot!!!!!!!!!
Cali_vs_Fl_vs_USA %>%
  ggplot( aes(x=Year, y=Incidents, group=Loc, color=Loc)) +
  geom_line() +
  ggtitle("Rate_of_shark_encounters_California_vs_Florida_vs_USA_1958-2017")

ggsave(file="Rate_of_shark_encounters_California_vs_Florida_vs_USA_1958-2017.svg", 
       width=15, height=8)
