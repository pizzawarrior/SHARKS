library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
NEW_SHARKS <- read.csv("~/First-Repo/data/GSAF5.csv")

#Filter Post_1958 to 2017 due to inconclusive data from 2018 on
Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018)

#Exercise: US vs Rest of World: What are the numbers?
Post_1958 %>% 
  mutate(World= ifelse(test = Country == "USA" , 
        yes = "USA" , no= "world")) %>% 
 select(World) %>% 
  group_by(World) %>% 
 summarise(Incidents=n())
  

Top_10_Countries <- Post_1958 %>%
  group_by(Country) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(10)

#Plot Top 10 Countries
ggplot(Top_10_Countries, aes(x = reorder(Country, +Incidents), y = Incidents)) +
  geom_bar(position="dodge", stat="identity")

#Save new plot
ggsave(file="Sharks_Plot_Top_10_Countries_1958-2017.svg", width=15, height=8)
