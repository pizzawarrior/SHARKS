#Time of year/ season, HEAT MAP

library(tidyverse)
library(lubridate)
library(viridis)

raw_df <- read_csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

head(raw_df$Date)

?lubridate

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

#Remove year, change to day, weekday
df_minus_year<- out_df %>% 
  mutate(month = month(clean_date), 
         day = day(clean_date), 
         wday = wday(clean_date))
      
df_minus_year<- df_minus_year %>% 
  group_by(month, wday) %>% 
#ADD LOCATION LATER!!!!!
  summarise(Incidents= n())

#Need to reorganize dataframe so day 7 is close to day 1 (Move Saturday to the left of Sunday)
class(df_minus_year$wday)

#Let's try replace:
#(IS THERE AN EASIER WAY TO DO THIS???)
df_minus_year_shifted<- df_minus_year %>%
  mutate(new_day= replace(wday, wday == 1, 8))

#To flip how months appear on plot:
%>% 
  mutate(new_month=
          (ifelse(test =  month == 12, yes =1, 
          (ifelse(test =  month == 11, yes =2, 
          (ifelse(test =  month == 10, yes =3,
          (ifelse(test =  month == 9, yes =4,
          (ifelse(test =  month == 8, yes =5,
          (ifelse(test =  month == 7, yes =6,
          (ifelse(test =  month == 6, yes =7,
          (ifelse(test =  month == 5, yes =8,
          (ifelse(test =  month == 4, yes =9,
          (ifelse(test =  month == 3, yes =10,
          (ifelse(test =  month == 2, yes =11,
          (ifelse(test =  month == 1, yes =12,
          no = "NA")))))))))))))))))))))))))

#Look how the dates are all rearranged now on plot-- how to vfy what happened here?

?str_detect

######## Plotting starts here#####################
ggplot(df_minus_year_shifted,aes(month, new_day, fill=Incidents))+
  geom_tile(color= "white",size= 2) + 
  scale_fill_viridis(name="Incidents",option ="C")+ 
  coord_equal()+
  theme_classic()+
  ggtitle("Monday= 2, Sunday= 8")

ggsave(file="Days_of_Incidents_CA_Heatmap_REORG_flipped.svg", width=15, height=8)


#Add ons to play with:
+
  theme_minimal(base_size = 8)
    
  theme(legend.position = "bottom")
  
  
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

ggsave(file="Days_of_Incidents_REORG.svg", 
       width=15, height=8)


