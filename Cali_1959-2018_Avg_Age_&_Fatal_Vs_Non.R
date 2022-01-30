#Cali mean age of victim 1959-2018
#Cali 1959-2018 fatal vs non fatal

library(tidyverse)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Add age as numeric column
Cali_Beaches_Age_Number_1959_2018<- Cali_Beaches %>% 
  filter(Year> 1958 & Year< 2018) %>% 
  mutate(Age_Number = as.numeric(Age))

#Average age of victim?
mean(Cali_Beaches_Age_Number_1959_2018$Age_Number, na.rm = TRUE)
#[1] 32.85366

#Summarize Fatal vs Non Fatal Incidents
Cali_Fatal_Non_Fatal <- Cali_Beaches_Age_Number_1959_2018 %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Get rid of blank row:
target <- c("Y", "N")
Cali_Fatal_Y_N <- filter(Cali_Fatal_Non_Fatal, Fatal..Y.N. %in% target)
  
#Let's make a bar plot!!
ggplot(Cali_Fatal_Y_N, aes(x= Fatal..Y.N., y= Incidents)) + 
  geom_bar(stat = "identity", width=0.5) +
  ggtitle("California 1959-2018 Shark Incidents")

ggsave(file="Cali_1959-2018_Fatal_Non_Fatal.svg", width=15, height=8)
