#Cali mean age of victim 1959-2018
#Cali 1959-2018 fatal vs non fatal PIE CHART

library(tidyverse)
library(svglite)
library(ggplot2)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Add age as numeric column
Cali_Beaches_Age_Number_1959_2018<- Cali_Beaches %>% 
  filter(Year> 1958 & Year< 2018) %>% 
  mutate(Age_Number = as.numeric(Age))

#Average age of victim?
mean(Cali_Beaches_Age_Number_1959_2018$Age_Number, na.rm = TRUE)
#[1] 32.85366

##########################################


#Summarize Fatal vs Non Fatal Incidents
Cali_Total_Incidents <- Cali_Beaches %>% 
  summarise(Incidents=n())

#Summarize Fatal vs Non Fatal Incidents
Cali_Fatal_Non_Fatal <- Cali_Beaches %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Get rid of blank row: (THIS DISPLAYS Y VS N, BUT WE WANT TOTAL INCIDENTS (218) VS Y (14))
target <- c("Y", "N")
Cali_Fatal_Y_N <- filter(Cali_Fatal_Non_Fatal, Fatal..Y.N. %in% target)

# Basic piechart
ggplot(Cali_Fatal_Y_N, aes(x="", y=Incidents, fill=Fatal..Y.N.)) +
  geom_bar(stat="identity", width=1, color="white") + #white is for the border around the triangle shape
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  ggtitle("Cali_17_Fatals_215_Non_Fatals_1959_2018_Pie")

ggsave("Cali_Fatal_Non_Fatal_Incidents_1959_2018_Pie.svg", width = 15, height = 8)

