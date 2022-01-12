
#Line Plot California Rate vs USA Rate 1958-2017

library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018)

#Use USA_Rate to define Group by Incidents per year since 1958 in USA
USA_Rate<- filter(Post_1958, Country == "USA")

#Isolate Rate further
USA_by_Year <- USA_Rate %>%
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Line Plot Rate_USA
ggplot(Final_USA, aes(x=Year, y=Incidents)) +
  geom_line() +
  ggtitle("Rate of shark encounters in US 1958-2017")

#Isolate California only
Cali_incidents<- filter(Post_1958, Area== "California")

#Isolate Cali Rate further
Cali_Rate<- Cali_incidents %>%
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Line plot Cali Rate
ggplot(Cali_Rate, aes(x=Year, y=Incidents)) +
  geom_line()+ 
  ggtitle("Rate of shark encounters in California since 1958")

#Filter (remove) California from USA/ reverse condition logic using !
Filtered_USA_Rate<- USA_Rate %>%
  filter(!Area %in% c("California"))

USA_minus_Cali<- Filtered_USA_Rate %>% 
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Create new variable, Loc= Cali to combine with USA for plot
Cali_Rate2 <- Cali_Rate %>% 
  mutate(Loc= "California")

#Create new variable, Loc= USA to combine with Cali for plot
Final_USA2<- USA_minus_Cali %>% 
  mutate(Loc= "USA")

#Combine data frames using bind_rows
Cali_or_USA<- bind_rows(Cali_Rate2, Final_USA2)

#Multi-Line Plot!!!!!!!!!
Cali_or_USA %>%
  ggplot( aes(x=Year, y=Incidents, group=Loc, color=Loc)) +
  geom_line() +
  ggtitle("Rate_of_shark_encounters_California_vs_rest_of_USA_1958-2018")

#Add population info??

ggsave(file="Rate_of_shark_encounters_California_vs_rest_of_USA_1958-2018.svg", 
       width=15, height=8)

#Alternative: Use bind_cols instead of using bind_rows: JOIN DATA!!!!!!!
Cali_or_USA2<- bind_cols(Cali_Rate2, Final_USA2)