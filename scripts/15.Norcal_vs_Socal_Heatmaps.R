#We want separate heatmaps for Norcal and Socal: can we SEE seasonality?

library(tidyverse)
library(lubridate)
library(viridis)

#Read in Data
Cali_Incidents<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate Beach/ County column:
#NOTE the comma separator also has a space after it to omit the WHITE SPACE 
#that occurs in front of the County variable when separating
Cali_Separated <- Cali_Incidents %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=", ")

#NorCal COMBINE counties:
Norcal<- c("Sonoma", "Marin", "San Mateo", "Santa Cruz", 
              "Monterey", "San Francisco", "Mendocino", "Humboldt",
           "Del Norte")

#Assign counties accordingly, create new column for Red Triangle
Norcal_column<- Cali_Separated %>% 
  mutate(Norcal= 
           ifelse(County %in% Norcal,
                  yes= "yes", no= "no"))

#For the first Heatmap: Norcal Yes, filter dates
Norcal_counties<- Norcal_column %>% 
  filter(Norcal == "yes") %>% 
  mutate(clean_date = dmy(Date))

Norcal_counties %>%
  filter(is.na(clean_date)) %>%
  select(Date, clean_date)

# manually fix those dates
Norcal_counties <- Norcal_counties %>%
  mutate(clean_date = case_when(
    Date == "Aug-1995" ~ as.Date("1995-08-01"),
    TRUE  ~ clean_date)) %>% 
  filter(!Date %in% c("1986", "1984"))

# confirm dates that all dates parsed:
Norcal_counties %>%
  filter(is.na(clean_date)) %>%
  select(Date, clean_date)

#More date processing:
#Remove year, change to day, weekday
Norcal_minus_year<- Norcal_counties %>% 
  mutate(month = month(clean_date), 
         day = day(clean_date), 
         wday = wday(clean_date))

Norcal_minus_year<- Norcal_minus_year %>% 
  group_by(month, wday) %>% 
  #ADD LOCATION LATER!!!!!
  summarise(Incidents= n())

#Let's shift Sunday over to be next to Saturday:
Norcal_minus_year_shifted<- Norcal_minus_year %>%
  mutate(new_day= replace(wday, wday == 1, 8))


#For the second heatmap: Norcal NO (Socal)
Socal<- Norcal_column %>% 
  filter(Norcal == "no") %>% 
  ## try built-in date converter function first
  mutate(clean_date = dmy(Date))

#Let's work on those dates next:
# check out dates that failed to parse, don't need to make new dataframe
Socal %>%
  filter(is.na(clean_date)) %>%
  select(Date, clean_date)

# manually fix those dates
Socal <- Socal %>%
  mutate(clean_date = case_when(
    Date == "Summer of 1996" ~ as.Date("1996-07-01"),
    Date == "Feb-1961" ~ as.Date("1961-02-01"),
    TRUE  ~ clean_date)) %>% 
  filter(!Date %in% "1965")

Socal %>%
  filter(is.na(clean_date)) %>%
  select(Date, clean_date)

#Remove year, change to day, weekday
Socal_minus_year<- Socal %>% 
  mutate(month = month(clean_date), 
         day = day(clean_date), 
         wday = wday(clean_date))

Socal_minus_year<- Socal_minus_year %>% 
  group_by(month, wday) %>% 
  #ADD LOCATION LATER!!!!!
  summarise(Incidents= n())

#Let's shift Sunday over to be next to Saturday:
Socal_minus_year_shifted<- Socal_minus_year %>%
  mutate(new_day= replace(wday, wday == 1, 8))

#Norcal Plot PARTY:
ggplot(Norcal_minus_year_shifted, aes(new_day, month, fill=Incidents))+
  geom_tile(color= "white",size= 2)+ 
  scale_fill_viridis(name="Incidents", option ="D")+
  coord_equal()+
  theme_classic()+
  ggtitle("Norcal, Mon= 2, Sun= 8, Dec= 12, 1959-2018")

#ggsave(file="15.Norcal_Heatmap_1959_2018.2.svg", width=15, height=8)

#Plot PARTY:
ggplot(Socal_minus_year_shifted, aes(new_day, month, fill=Incidents))+
  geom_tile(color= "white",size= 2)+ 
  scale_fill_viridis(name="Incidents", option ="D", limits=c(0,12),
#Let's try to get the legend similar to the one for Norcal:
                     breaks = c(0, 3, 6, 9, 12),
                     labels = c(0, 3, 6, 9, 12))+
  coord_equal()+
  theme_classic()+
  ggtitle("Socal, Mon= 2, Sun= 8, Dec= 12")

#ggsave(file="15.Socal_Heatmap_1959_2018.2.svg", width=15, height=8)

