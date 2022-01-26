#Cali Top 10 Counties Average Age of Victim, 1998-2017

library(tidyverse)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Filter date range, add column of numeric age
Cali_Beaches_1997_2017<- Cali_Beaches %>% 
  filter(Year> 1997 & Year< 2018) %>% 
  mutate(Age_Number = as.numeric(Age))

#Average age of cali victim?
mean(Cali_Beaches_1997_2017$Age_Number, na.rm = TRUE)
#[1] 35.43662

#Separate Beaches/ County column to produce COUNTY column, filter years
Cali_Beaches_and_Counties_Separated <- Cali_Beaches_1997_2017 %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=",")
  
#Select Top 10 Counties by incidents
Filtered_Cali_Beaches_1997_2018 <- Cali_Beaches_and_Counties_Separated %>% 
  select(County, Age_Number) %>% 
  group_by(County) %>% 
  summarise(Incidents = n(),
            Avg_Age = mean(Age_Number, na.rm = TRUE)) %>% 
  mutate(Avg_Age_Rounded = round(Avg_Age, digits = 0)) %>% 
  arrange(desc(Incidents)) %>% 
  head(10)

#Plot top 10 counties
ggplot(Filtered_Cali_Beaches_1997_2018, aes(x = County, y = Avg_Age_Rounded)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Avg Age of Victims Top 10 Counties in CA, 1997-2018")

#Save plot
ggsave(file="Avg_Age_of_Victims_Top_10_Counties_in_CA_1997_2018.svg", 
       width=15, height=8)
