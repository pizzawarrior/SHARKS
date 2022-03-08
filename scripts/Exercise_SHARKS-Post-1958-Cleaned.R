library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
NEW_SHARKS <- read.csv("~/First-Repo/data/GSAF5.csv")

?ggplot

#Filter Post_1958 to 2017 due to inconclusive data from 2018 on
Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018)

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

#Use Rate to define Group by Incidents per year since 1958 in USA
USA_Rate<- filter(Post_1958, Country == "USA")

#Isolate Rate further
Final_USA <- USA_Rate %>%
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))
  
#Line Plot Rate_USA
ggplot(Final_USA, aes(x=Year, y=Incidents)) +
  geom_line() +
ggtitle("Rate of shark encounters in US since 1958")

ggsave(file="USA_Sharks_Line_Plot_1958-2018.svg", width=15, height=8)

#Isolate California only
Cali_incidents<- filter(Post_1958, Area== "California")

#Export Cali locations for cleanup
write.csv(Cali_incidents,"~/Desktop/EMERGENT WORKS/SHARKS\\Cali Incidents Post_1958.csv", 
          row.names = TRUE)

#Isolate Cali Rate
Cali_Rate<- Cali_incidents %>%
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#plotly interactive graph
ggplotly(p)

#Plot Cali_Rate
ggplot(Cali_Rate, aes(x=Year, y=Incidents)) +
  geom_line()+ 
  ggtitle("Rate of shark encounters in California since 1958")

#California top beaches, Exercise: flip x and y
ggplot(Cali_top_25, aes(x = Incidents, y = reorder(Location, +Incidents))) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Top 25 California Beaches by Number of Encounters")

ggsave(file="Cali_Top_25_Beaches.svg", width=15, height=8)
 
#change x axis labels to 45 degree
ggplot(Cali_top_25, aes(x = reorder(Location, +Incidents), y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 47, vjust = 1, hjust=1))

#Split variable "Location" into beach and county: INCONCLUSIVE
Cali_incidents %>% 
  separate(Location, c("Beach", "County") ,",",convert = TRUE) %>% 
  select(Location, Beach, County)

#Flag- indicator, California, create new variable
Cali_flag<- Post_1958 %>% 
  mutate(California_Flag= ifelse(
   test =  Area == "California",
   yes ="California", 
   no = "Rest of the World")) %>% 
  select(Area, California_Flag)

?ifelse

#Summarize Cali incidents vs Rest of World incidents
Incidents_by_Cali_flag <- Cali_flag %>%
  group_by(California_Flag) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))
  
ggplot(Incidents_by_Cali_flag, aes(x = California_Flag, y = Incidents)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Cali vs The World")

#Add in new variable of Cali population and world population

#Create new variable, USA_flag from Post_1958: Conditional mutate dplyr
USA_flag<- Post_1958 %>% 
  mutate(USA_flag= ifelse(
    test =  Country == "USA",
    yes ="USA", 
    no = "Rest of the World")) %>% 
  select(Country, USA_flag)

#Add California to Countries variable to separate it from USA: Conditional mutate dplyr
USA_plus_Cali<- Post_1958 %>% 
  mutate(USA_plus_Cali= ifelse(
    test =  Area == "California",
    yes ="CALIFORNIA", 
    no = Country)) %>% 
  select(Area, Country, USA_plus_Cali)

Incidents_by_Country_and_Cali <- USA_plus_Cali %>%
  group_by(USA_plus_Cali) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(20)

#Plot Cali with USA and Countries
ggplot(Incidents_by_Country_and_Cali, aes(x = reorder(USA_plus_Cali, +Incidents),
  y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 47, vjust = 1, hjust=1))
 
ggsave(file="California_Country_USA_Incidents.svg", width=15, height=8)

#Add Florida to Countries List to separate it from USA: Conditional mutate dplyr
USA_plus_Florida<- Post_1958 %>% 
  mutate(USA_plus_Florida= ifelse(
    test =  Area == "Florida",
    yes ="FLORIDA", 
    no = Country)) %>% 
  select(Area, Country, USA_plus_Florida)

#Group by Incidents
Incidents_by_Country_and_Florida <- USA_plus_Florida %>%
  group_by(USA_plus_Florida) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(20)

ggplot(Incidents_by_Country_and_Florida, aes(x = reorder(USA_plus_Florida, +Incidents),
  y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 47, vjust = 1, hjust=1))

#Group Cali and Florida data sets with Countries: Conditional mutate dplyr
Cali_Florida_Countries<- Post_1958 %>% 
  mutate(Cali_Florida_Countries= 
  ifelse(test = Area == "Florida", yes =  "FLORIDA",
  ifelse(test = Area == "California", yes =  "CALIFORNIA",
  no= Country))) %>%
  select(Area, Country, Cali_Florida_Countries)

#Group by Incidents
Incidents_by_Country_Cali_and_Florida <- Cali_Florida_Countries %>%
  group_by(Cali_Florida_Countries) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  top_n(20)

ggplot(Incidents_by_Country_Cali_and_Florida, aes(x = reorder(Cali_Florida_Countries, +Incidents),
   y = Incidents)) +
  geom_bar(position="dodge", stat="identity") + 
  theme(axis.text.x = element_text(angle = 47, vjust = 1, hjust=1))

ggsave(file="Countries_Cali_and_Florida.svg", width=15, height=8)
  