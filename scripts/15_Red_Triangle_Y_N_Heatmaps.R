library(tidyverse)

#Read in Data
Cali_Incidents<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate Beach/ County column:
#NOTE the comma separator also has a space after it to omit the WHITE SPACE 
#that occurs in front of the County variable when separating
Cali_Separated <- Cali_Incidents %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=", ")

#Red triangle COMBINE counties:
Red_Triangle_Counties<- c("Sonoma", "Marin", "San Mateo", "Santa Cruz", "Monterey")

#Assign counties accordingly, create new column for Red Triangle
Red_Triangle_column<- Cali_Separated %>% 
  mutate(Red_Triangle= 
           ifelse(County %in% Red_Triangle_Counties,
                  yes= "yes", no= "no"))

#For the first Heatmap: Red Triangle Yes
Red_Triangle_Yes<- Red_Triangle_column %>% 
  filter(Red_Triangle == "yes")

#For the second heatmap: Red Triangle NO
Red_Triangle_No<- Red_Triangle_column %>% 
  filter(Red_Triangle == "no")





test_df <- raw_df %>% 
  ## try built-in date converter function first
  mutate(clean_date = dmy(Date))

# check out dates that failed to parse, don't need to make new dataframe
test_df %>%
  filter(is.na(clean_date)) %>%
  select(Date, clean_date)

# manually fix those dates
out_df <- test_df %>%
  mutate(clean_date = case_when(
    Date == "Aug-1995" ~ as.Date("1995-08-01"),
    Date == "Summer of 1996" ~ as.Date("1996-07-01"),
    Date == "Feb-1961" ~ as.Date("1961-02-01"), 
    Date == "Between 10 and 12-Sep-1959"  ~ as.Date("1959-09-11"),
    TRUE  ~ clean_date)) %>% 
  filter(!Date %in% c("1984" , "1986", "1965"))

#Cool Trick!!:
## make sure there's nothing crazy unexpected here
out_df %>%
  select(Date, clean_date) %>% View

#Cool Trick!! ALTERNATIVE:
## make sure there's nothing crazy unexpected here
summary(out_df$clean_date)

