#How do Cali Fatal numbers compare to FL? 
#How do they compare to rest of USA (inc. Florida)?
#How do these numbers compare to Australia?
#Should this be a stacked barplot?

library(tidyverse)
library(svglite)
library(ggplot2)

#For California:
#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Summarize Fatal vs Non Fatal Incidents
Cali_Fatal_Non_Fatal <- Cali_Beaches %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Get rid of blank row: 
target <- c("Y", "N")

Cali_Fatal_Y_N <- filter(Cali_Fatal_Non_Fatal, Fatal..Y.N. %in% target)%>% 
  mutate(Loc= "California")


#Read in new data for FL, USA, and AUS
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

#Florida, define parameters:
Post_1958_FL<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Area == "Florida")

#Refine further
Post_1958_FL_Fatal_Non <- Post_1958_FL %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Get rid of blank row:
FL_Fatal_Y_N <- filter(Post_1958_FL_Fatal_Non, Fatal..Y.N. %in% target) %>% 
  mutate(Loc= "Florida")


#SOUTH AFRICA, Define parameters
Post_1958_South_Africa<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Country == "SOUTH AFRICA")

#Filter
Post_1958_SA<- Post_1958_South_Africa %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Clean unresolved data, add column identifier for plot
Post_1958_SA_Cleaned <- filter(Post_1958_SA, 
  Fatal..Y.N. %in% target) %>% 
  mutate(Loc= "South Africa")

#Let's try AUS
Post_1958_AUS<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Country == "AUSTRALIA") %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Clean unresolved data
AUS_Fatal_Y_N <- filter(Post_1958_AUS, Fatal..Y.N. %in% target)%>% 
  mutate(Loc= "Australia")

#BINDROWS
Target_Fatal_Loc<- bind_rows(Cali_Fatal_Y_N, FL_Fatal_Y_N, AUS_Fatal_Y_N, Post_1958_SA_Cleaned)

# Stacked
ggplot(Target_Fatal_Loc, aes(fill=Fatal..Y.N., y=Incidents, x=Loc)) + 
  geom_bar(position="stack", stat="identity")

#Save plot
ggsave(file="CA_vs_FL_vs_AUS_vs_SA_Stacked_Barplot.svg", width=15, height=8)
