#We want a heatmap for Red Triangle and one for Non Red Triangle

library(tidyverse)
library(lubridate)
library(viridis)

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

#Let's work on those dates next:
Red_Triangle_Yes <-  Red_Triangle_Yes%>% 
  ## try built-in date converter function first
  mutate(clean_date = dmy(Date))

Red_Triangle_Yes %>%
  filter(is.na(clean_date)) %>%
  select(Date, clean_date)

# manually fix those dates
Red_Triangle_Yes <- Red_Triangle_Yes %>%
  mutate(clean_date = case_when(
    Date == "Aug-1995" ~ as.Date("1995-08-01"),
    TRUE  ~ clean_date)) %>% 
  filter(!Date %in% 1986)

#Let's check for irregularities:
summary(Red_Triangle_Yes$clean_date)

#For the second heatmap: Red Triangle NO
Red_Triangle_No<- Red_Triangle_column %>% 
  filter(Red_Triangle == "no")

#Let's work on those dates next:
Red_Triangle_No <-  Red_Triangle_No%>% 
  ## try built-in date converter function first
  mutate(clean_date = dmy(Date))

# check out dates that failed to parse, don't need to make new dataframe
Red_Triangle_No %>%
  filter(is.na(clean_date)) %>%
  select(Date, clean_date)

# manually fix those dates
Red_Triangle_No <- Red_Triangle_No %>%
  mutate(clean_date = case_when(
    Date == "Summer of 1996" ~ as.Date("1996-07-01"),
    Date == "Feb-1961" ~ as.Date("1961-02-01"),
    TRUE  ~ clean_date)) %>% 
  filter(!Date %in% c("1984" , "1965"))

# confirm dates that all dates parsed:
Red_Triangle_No %>%
  filter(is.na(clean_date)) %>%
  select(Date, clean_date)

## triple check nothing crazy unexpected here
Red_Triangle_No %>%
  select(Date, clean_date) %>% View

#ALTERNATIVE:
## make sure there's nothing crazy unexpected here
summary(Red_Triangle_No$clean_date)

#More date processing:
#Remove year, change to day, weekday
Yes_minus_year<- Red_Triangle_Yes %>% 
  mutate(month = month(clean_date), 
         day = day(clean_date), 
         wday = wday(clean_date))

Yes_minus_year<- Yes_minus_year %>% 
  group_by(month, wday) %>% 
  #ADD LOCATION LATER!!!!!
  summarise(Incidents= n())

#Remove year, change to day, weekday
No_minus_year<- Red_Triangle_No %>% 
  mutate(month = month(clean_date), 
         day = day(clean_date), 
         wday = wday(clean_date))

No_minus_year<- No_minus_year %>% 
  group_by(month, wday) %>% 
  #ADD LOCATION LATER!!!!!
  summarise(Incidents= n())

#Let's shift Sunday over to be next to Saturday:
Yes_minus_year_shifted<- Yes_minus_year %>%
  mutate(new_day= replace(wday, wday == 1, 8))

#Plot PARTY:
ggplot(Yes_minus_year_shifted,aes(new_day, month, fill=Incidents))+
  geom_tile(color= "white",size= 2) + 
  scale_fill_viridis(name="Incidents",option ="C")+ 
  coord_equal()+
  theme_classic()+
  ggtitle("Red Tri, Mon= 2, Sun= 8, Dec= 12, 1959-2018")

ggsave(file="15.Red_Tri_Heatmap_1959_2018.svg", 
       width=15, height=8)

#Now let's plot the NON RED TRIANGLES:
#Let's shift Sunday over to be next to Saturday:
No_minus_year_shifted<- No_minus_year %>%
  mutate(new_day= replace(wday, wday == 1, 8))

#Plot PARTY:
ggplot(No_minus_year_shifted,aes(new_day, month, fill=Incidents))+
  geom_tile(color= "white",size= 2) + 
  scale_fill_viridis(name="Incidents",option ="C")+ 
  coord_equal()+
  theme_classic()+
  ggtitle("15.Red Tri NO, Mon= 2, Sun= 8, Dec= 12")

ggsave(file="15.Red_Tri_NO_Heatmap_1959_2018.svg", 
       width=15, height=8)

#Other settings to play with:
  
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

#ggsave(file="Days_of_Incidents_REORG.svg", width=15, height=8)



