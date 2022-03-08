
#Line Plot California Incident Rate vs USA Rate 1958-2017
#Use join/ bind to combine dataframes

library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
NEW_SHARKS <- read.csv("~/First-Repo/data/GSAF5.csv")

Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018)

#Isolate USA
USA_by_Year <- Post_1958 %>%
  filter(Country == "USA") %>% 
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Line Plot Rate_USA
ggplot(USA_by_Year, aes(x=Year, y=Incidents)) +
  geom_line() +
  ggtitle("Rate of shark encounters in US 1958-2017")

#Isolate California only
Cali_incidents<- Post_1958 %>% 
  filter(Area == "California")

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

#Alternative: Use bind_cols instead of using bind_rows: Won't work because rows are not equal
Cali_or_USA2<- bind_cols(Cali_Rate2, Final_USA2)

#Left join: will lose rows that are not present in other
Cali_USA_Left_Joined<- Cali_Rate2 %>%
  left_join(Final_USA2, by = "Year")

#Full Join: will join and add all rows even if there is a discrepancy between 2 sets
Cali_USA_Full_Join <- Cali_Rate2 %>%
  full_join(Final_USA2, by = "Year")

#example of a join:
joined_data <- full_join(Cali_Rate2, Final_USA2, 
            by = "Year",
            suffix = c("_cali", "_usa")) %>%
  select(-starts_with("Loc")) %>% # get rid of unnecessary "Loc_" columns
  arrange(-Year)

?left_join

?tribble

#Show years where incidents in Cali were greater than incidents USA
Cali_USA_Joined_Greater_than <- Cali_USA_Full_Join %>% 
  filter(Incidents.x > Incidents.y)

#Show years where Incidents in Cali and USA were greater than 20
Cali_USA_Full_Join %>% 
  filter(Incidents.x > 20 | Incidents.y >20)

#Show max number of Cali incidents by year: (Inconclusive)
Cali_USA_Full_Join %>% 
  filter(Incidents.x == max(Incidents.x))

## is the difference in the number of incidents between usa and cali increasing over time? 
out <- joined_data %>%
  mutate(diff = Incidents_usa - Incidents_cali) 

## yes!
plot(out$Year, out$diff)

