#Cali mean age of victim 1959-2018
#Cali 1959-2018 fatal vs non fatal

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

########################################################################################

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

#Let's make a bar plot!! (DO NOT USE::THIS IS NOT THE INFO WE WANT, SEE NOTE ABOVE)
ggplot(Cali_Fatal_Y_N, aes(x= Fatal..Y.N., y= Incidents)) + 
  geom_bar(stat = "identity", width=0.5) +
  ggtitle("California 1959-2018 Shark Incidents")

ggsave(file="Cali_Fatal_vs_Non_1959_2018.svg",  
       width=15, height=8)

#Would a pie chart be better?
Cali_Fatal_Y_N_Pie<- Cali_Fatal_Y_N %>% 
  select(Incidents)

#WORTHLESS: (Can not save, not a gg object)
pie(Cali_Fatal_Y_N_Pie$Incidents , labels = c("Non Fatal", "Fatal"))

####################################################################################

#Try to arrive at DF that shows fatalities among TOTAL incidents:
#Alternative attempts:
Cali_Fatal_Yes_No <- filter(Cali_Beaches, Fatal..Y.N. %in% target) %>% 
  group_by(Fatal..Y.N.) %>%
  summarise(Incidents=n())

# Basic piechart
ggplot(Cali_Fatal_Yes_No, aes(x="", y=Incidents, fill=Fatal..Y.N.)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() + # remove background, grid, numeric labels

  ggtitle("CA 204 Fatal, 14 Non Fatal Incidents, 1959-2018")

ggsave(file="Cali_Fatal_Non_Fatal_Incidents_1959_2018_Pie.svg",  
       width=15, height=8)






