#Time of year/ season, Bubble plot? 

library(tidyverse)
library(lubridate)
library(viridis)

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

#Remove year
df_minus_year<- out_df %>% 
  mutate(month = month(clean_date), 
         day = day(clean_date), 
         wday = wday(clean_date))
      

df_minus_year<- df_minus_year %>% 
  group_by(month, wday) %>% 
#ADD LOCATION LATER!!!!!
  summarise(Incidents= n())
  
  
?lubridate

######## Plotting starts here#####################
ggplot(df_minus_year,aes(wday,month,fill=Incidents))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Incidents",option ="C")



p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Hourly Temps - Station",statno), x="Day", y="Hour Commencing")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra




