library(tidyverse)
library(lubridate)

raw_df <- read_csv("GSAF5-Cali_Post_1958-2017_BEACHES.csv" )
head(raw_df$Date)

test_df <- raw_df %>% 
  ## try built-in date converter function first
  mutate(clean_date = dmy(Date))

# check out dates that failed to parse
test_df %>%
  filter(is.na(clean_date)) %>%
  select(Date, clean_date)

# manually fix those ones
out_df <- test_df %>%
  mutate(clean_date = case_when(
    Date == "Aug-1995"        ~ as.Date("1995-08-01"),
    Date == "Summer of 1996" ~ as.Date("1996-07-01"),
    Date == "Feb-1961" ~ as.Date("1961-02-01"),
    ## for dates when we only have the year, does it make sense to choose january 1st?
    Date == "1986"~ as.Date("1986-01-01"),
    Date == "1984" ~ as.Date("1984-01-01"),
    Date == "1965" ~ as.Date("1965-01-01"),
    TRUE                      ~ clean_date
  ))

## make sure there's nothing crazy unexpected here
summary(out_df$clean_date)

  
