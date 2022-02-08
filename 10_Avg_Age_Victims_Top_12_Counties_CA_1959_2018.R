#Cali Top 12 Counties Average Age of Victim, 1959-2018

library(tidyverse)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Filter date range, add column of numeric age
Cali_Beaches_1959_2018<- Cali_Beaches %>% 
  filter(Year> 1958 & Year< 2018) %>% 
  mutate(Age_Number = as.numeric(Age))

#Average age of cali victim?
mean(Cali_Beaches_1958_2017$Age_Number, na.rm = TRUE)
#[1] 35.43662

#Separate Beaches/ County column to produce COUNTY column, filter years
Cali_Beaches_and_Counties_Separated <- Cali_Beaches_1959_2018 %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=",")
  
#Select Top 10 Counties by incidents
Filtered_Cali_Beaches_1959_2018 <- Cali_Beaches_and_Counties_Separated %>% 
  select(County, Age_Number) %>% 
  group_by(County) %>% 
  summarise(Incidents = n(),
            Avg_Age = mean(Age_Number, na.rm = TRUE)) %>% 
  mutate(Avg_Age_Rounded = round(Avg_Age, digits = 0)) %>% 
  arrange(desc(Incidents)) %>% 
  head(12)

#Lollipop plot Top 12 Counties
ggplot(Filtered_Cali_Beaches_1959_2018, aes(x=County, y=Avg_Age_Rounded)) +
  geom_point() + 
  geom_segment( aes(x=County, xend=County, y=0, yend=Avg_Age_Rounded)) +
  ggtitle("Average Age of Shark Attack Victims Top 12 Counties, CA, 1959-2018")

#Save plot
ggsave(file="Avg_Age_of_Victims_Top_12_Counties_in_CA_1959_2018.svg", 
       width=15, height=8)
