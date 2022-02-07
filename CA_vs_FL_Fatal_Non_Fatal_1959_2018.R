
#Fatal / non fatal Cali vs FL 1959-2018 STACKED BAR PLOT!!!

library(tidyverse)
library(svglite)
library(ggplot2)

#Read in Data
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

#Filter Post_1958 to 2017 USA
Post_1958_USA<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Country == "USA")

#Filter for CA, summarize fatal vs non: 
#THIS USES A LIST THAT IS NOT THE SAME AS THE CLEANED BEACHES FILE. RESULTS ARE NOT THE SAME
#READ IN THE CLEANED BEACHES FILE FOR THIS?????
Cali_Fatal_Non_Fatal<- Post_1958_USA %>% 
  filter(Area == "California") %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Get rid of blank row: (THIS DISPLAYS Y VS N, BUT WE WANT TOTAL INCIDENTS (218) VS Y (14))
target <- c("Y", "N")
Cali_Fatal_Y_N <- filter(Cali_Fatal_Non_Fatal, Fatal..Y.N. %in% target)

#Add new California column
Cali_Fatal_Y_N_New_Column<- Cali_Fatal_Y_N %>% 
mutate(Loc= "California")

#Back to USA df, filter FL
FL_Fatal_Non_Fatal<- Post_1958_USA %>% 
  filter(Area == "Florida") %>% 
  group_by(Fatal..Y.N.) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#Get rid of blank row: (THIS DISPLAYS Y VS N, BUT WE WANT TOTAL INCIDENTS (218) VS Y (14))
target <- c("Y", "N")
FL_Fatal_Y_N <- filter(FL_Fatal_Non_Fatal, Fatal..Y.N. %in% target)

#Add new FL column
FL_Fatal_Y_N_New_Column<- FL_Fatal_Y_N %>% 
  mutate(Loc= "Florida")

#Combine data frames using bind_rows
Cali_vs_FL<- bind_rows(Cali_Fatal_Y_N_New_Column, FL_Fatal_Y_N_New_Column)

#Stacked Bar Plot!!
