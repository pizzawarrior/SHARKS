
#Line Plot California Rate vs Florida Rate vs Rest of USA 1958-2017

#Calculate per capita rate for same locations and line plot that instead!!!!

library(tidyverse)
library(svglite)
library(plotly)

NEW_SHARKS <- read.csv("~/First-Repo/data/GSAF5.csv")

#Filter 1958 to 2018, by USA
Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Country == "USA")

#Isolate California only
Cali_Rate<- filter(Post_1958, Area== "California") %>% 
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Line Plot Cali_Rate
ggplot(Cali_Rate, aes(x=Year, y=Incidents)) +
  geom_line() +
  ggtitle("Rate of shark encounters in California 1958-2017")

#Isolate Florida only
Fl_Rate<- filter(Post_1958, Area== "Florida") %>% 
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
#Create new variable, Loc= USA to combine with Cali for plot
USA_minus_Cali_Florida<- USA_Rate %>%
  filter(!Area %in% c("California", "Florida")) %>% 
  group_by(Year) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents)) %>% 
  mutate(Loc= "USA")
  
#Combine data frames using bind_rows
Cali_vs_Fl_vs_USA<- bind_rows(Cali_vs_Florida_Rate, USA_minus_Cali_Florida)

#Multi-Line Plot!!!!!!!!!
Cali_vs_Fl_vs_USA %>%
  ggplot( aes(x=Year, y=Incidents, group=Loc, color=Loc)) +
  geom_line() +
  ggtitle("Rate_of_shark_encounters_California_vs_Florida_vs_USA_1958-2017")

ggsave(file="Rate_of_shark_encounters_California_vs_Florida_vs_USA_1958-2017.svg", 
       width=15, height=8)

#############################################################################################

#Let's Census!!!!!

Census<- read_csv("~/First-Repo/apportionment.csv")

Filtered_Census<- Census %>% 
  filter(Name %in% c("California" , "Florida" , "United States")) %>% 
  select(Name, Year, "Resident Population")

#Specify variable name around United States using ``
Wide_Census<- Filtered_Census %>% 
  spread(Name, "Resident Population") %>% 
  mutate(USA = `United States`- California - Florida)

#Convert long data to wide data
Long_Census<- Wide_Census %>% 
  gather("USA", "California", "Florida", key = State, value = Population) %>% 
  select(-`United States`)

#Round dates to nearest 10
Round_data<- Cali_vs_Fl_vs_USA %>% 
  mutate(Rounded_Year = round(Year, -1))

#Summarize incidents according to year, join data frames!!!
Summed_data<- Round_data %>%
  group_by(Loc, Rounded_Year) %>% 
  summarise(sum_Incidents = sum(Incidents)) %>% 
  left_join(Long_Census, by = c("Rounded_Year" = "Year", "Loc" = "State"))

#Override default of using scientific notation in subsequent calls:
options(scipen = 100)

#Produce per capita rate
Per_capita<- Summed_data %>%
  mutate(Rate = sum_Incidents/Population) %>% 
  select(Loc, Rounded_Year, Rate)
 
#Multi-Line Plot!!!!!!!!!
Per_capita %>%
  ggplot( aes(x=Rounded_Year, y=Rate, group=Loc, color=Loc)) +
  geom_line() +
  ggtitle("Rate of Per Capita encounters CA FL USA_1958-2017")

ggsave(file="Rate_of_per_capita_encounters_CA_FL_USA_1958-2017.svg", 
       width=15, height=8)



