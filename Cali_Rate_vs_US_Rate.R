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
  ggtitle("Rate of shark encounters in US since 1958")

#Isolate California only
Cali_incidents<- filter(Post_1958, Area== "California")

#Isolate Cali Rate further
Cali_Rate<- Cali_incidents %>%
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Line plot Cali Rate
(p<- ggplot(Cali_Rate, aes(x=Year, y=Incidents)) +
    geom_line()) +
  ggtitle("Rate of shark encounters in California 1958-2018")

#Filter (remove) California from USA
Filtered_USA_Rate<- USA_Rate %>%
  filter(!Area %in% c("California"))

USA_minus_Cali<- Filtered_USA_Rate %>% 
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Combine Cali_Rate plot with Final_USA plot
Cali_Rate2 <- Cali_Rate %>% 
  mutate(Loc= "California")

Final_USA2<- USA_minus_Cali %>% 
  mutate(Loc= "USA")

Cali_or_USA<- bind_rows(Cali_Rate2, Final_USA2)

#Multi-Line Plot!!!!!!!!!
Cali_or_USA %>%
  ggplot( aes(x=Year, y=Incidents, group=Loc, color=Loc)) +
  geom_line() +
  ggtitle("Rate of shark encounters California vs. rest of USA 1958-2018")

#Alternative: Use bind_cols instead of using bind_rows: JOIN DATA!!!!!!!
Cali_or_USA2<- bind_cols(Cali_Rate2, Final_USA2)

#example of a join:
joined_data <- full_join(Cali_Rate2, Final_USA2, 
                         by = "Year",
                         suffix = c("_cali", "_usa")) %>%
  select(-starts_with("Loc")) %>% # get rid of unnecessary "Loc_" columns
  arrange(-Year)

## in what years were there more incidents in Cali than the rest of the US combined?
joined_data %>%
  filter(Incidents_cali > Incidents_usa)

## is the difference in the number of incidents between usa and cali increasing over time? 
out <- joined_data %>%
  mutate(diff = Incidents_usa - Incidents_cali) 

## yes!
plot(out$Year, out$diff)