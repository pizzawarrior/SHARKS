
#Cali ALL Counties, 1959-2018
#Pie chart 

library(tidyverse)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Filter date range, add column of numeric age
Cali_Beaches_1959_2018<- Cali_Beaches %>% 
  filter(Year> 1958 & Year< 2018)

#Separate Beaches/ County column to produce COUNTY column, filter years
Cali_Beaches_and_Counties_Separated <- Cali_Beaches_1959_2018 %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=", ")

View(Cali_Beaches_and_Counties_Separated)

#How can we turn the value of collected incidents by county into a column that shows percentage?

#Filter more
Filtered_Cali_Beaches_1959_2018 <- Cali_Beaches_and_Counties_Separated %>% 
  select(County) %>% 
  group_by(County) %>% 
  summarise(Incidents = n()) %>% 
  arrange(desc(Incidents))


#Can we do anything with this??? (Vector help???)
Other_Counties<- c("Ventura", "Mendocino", "Del Norte")

#Let'screate new column for 'OTHER' counties
#Caveman style:
County_flag<- Filtered_Cali_Beaches_1959_2018 %>% 
  mutate(County_flag= 
    ifelse(test =  County == "Marin", yes ="Marin", 
    ifelse(test =  County == "Sonoma", yes = "Sonoma", 
    ifelse(test =  County == "Humboldt", yes = "Humboldt", 
    ifelse(test =  County == "Los Angeles", yes = "Los Angeles", 
    ifelse(test =  County == "Santa Barbara", yes = "Santa Barbara", 
    ifelse(test =  County == "San Luis Obispo", yes = "San Luis Obispo", 
    ifelse(test =  County == "Monterey", yes = "Monterey", 
    ifelse(test =  County == "Orange County", yes = "Orange County", 
    ifelse(test =  County == "San Francisco", yes = "San Francisco", 
    ifelse(test =  County == "San Mateo", yes = "San Mateo", 
    ifelse(test =  County == "Santa Cruz", yes = "Santa Cruz", 
    ifelse(test =  County == "San Diego", yes = "San Diego", 
    no = "Other"))))))))))))) %>% 
  filter(!is.na(County))

#Combine 'Other' incidents:
County_flag_sum<- County_flag %>% 
  group_by(County_flag) %>% 
  summarise(sum_Incidents = sum(Incidents)) %>% 
  arrange(desc(sum_Incidents))

ggplot(County_flag_sum, aes(x="", y=sum_Incidents, fill=County_flag)) +
  geom_bar(stat="identity", width=1, color="white") + #white is for the border around the triangle shape
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  ggtitle("Cali Counties ALL INCIDENTS 1959-2018 Pie")

ggsave(file="CA_Counties_All_Incidents_1959_2018_Pie.svg", 
       width=15, height=8)

sum(County_flag_sum$sum_Incidents)
#[1] 244
