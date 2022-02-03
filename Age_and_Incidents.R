
#Does age have any significance in shark incidents????
#Average age of victims in USA EACH YEAR from 1998-2017
#Average age of victims in top 10 Countries from 1998-2017

library(tidyverse)
library(svglite)
library(plotly)

#Read in Data
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

#Filter Post_1958 to 2017 due to inconclusive data from 2018 on
Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018)

#What is the average age GLOBALLY of shark attack victims?
mean(Post_1958$Age, na.rm = TRUE)
?mean #This doesn't work because there are characters in the Age column. Test Age column
#to see how the code reads the entries using Class:

class(Post_1958$Age)
#[1] "character" means there are characters in what should be a numeric column

#Make new column called Age_Number and convert all entries in Age column to numeric:
Post_1958_Age_Number <- Post_1958 %>% 
  mutate(Age_Number = as.numeric(Age))

#Now the code will read all numbers as numbers and will discard any NA's:
mean(Post_1958_Age_Number$Age_Number, na.rm = TRUE)
#Average age is 28.15387!!! (Answer shows in console)

#How is the average USA age changing over time? (Over last 20 years?)

#Narrow down to USA and date range:
USA_Last_20_Years<- Post_1958 %>%
  filter(Year >1997 & Year <2018,
         Country == "USA") %>% 
  mutate(Age_Number = as.numeric(Age))

Filtered_USA_Last_20_Years <- USA_Last_20_Years %>% 
  select(Year, Age_Number) %>% 
  group_by(Year) %>% 
  summarise(Incidents = n(),
  Avg_Age = mean(Age_Number, na.rm = TRUE)) %>% 
  mutate(Avg_Age_Rounded = round(Avg_Age, digits = 0))

?round

mean(Filtered_USA_Last_20_Years$Avg_Age_Rounded)
# [1] 28

#Quickly view in console by pasting into console:
plot(Filtered_USA_Last_20_Years$Year, Filtered_USA_Last_20_Years$Avg_Age_Rounded)

#Lolipop chart!
ggplot(Filtered_USA_Last_20_Years, aes(x=Year, y=Avg_Age_Rounded)) +
  geom_point() + 
  geom_segment( aes(x=Year, xend=Year, y=0, yend=Avg_Age_Rounded)) +
  ggtitle("Average Age of Shark Attack Victims in US, 1998-2017")

ggsave(file="Average_Age_of_Shark_Attack_Victims_in_US_1998_2017_Lolipop.svg", 
       width=15, height=8)

#Circular bar plot: starts with labeling the bars:

label_data <- Filtered_USA_Last_20_Years

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$Year-1997.5) /number_of_bar  
# I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)

class(Filtered_USA_Last_20_Years$Year)

#Plot:
ggplot(Filtered_USA_Last_20_Years, aes(x=Year, y=Avg_Age_Rounded)) +
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.7)) +
    ylim(-40,40) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-3,4), "cm")) +  # This removes unnecessary margin around plot

  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +

  #Add labels to columns, prepare data:
geom_text(data=label_data, aes(x=Year, y=Avg_Age_Rounded+2, label= paste(Year, 
      "\n", Avg_Age_Rounded, "yrs old"), 
      hjust=hjust), color="black", fontface="bold",alpha=0.6, 
      size=2.5, angle= label_data$angle, inherit.aes = FALSE)

?paste
#Use paste to CONCATENATE (smoosh) several things together

ggsave(file="Average_Age_of_Shark_Attack_Victims_in_US_1998_2017_Circular_Bar.svg", 
       width=15, height=15)

#What is the avg age of shark attack victims of the Top 10 Countries in the last 20 years?
# Add Age_Number column
World_Last_20_Years<- Post_1958 %>%
  filter(Year >1997 & Year <2018) %>% 
  mutate(Age_Number = as.numeric(Age))

Filtered_World_Last_20_Years <- World_Last_20_Years %>% 
  select(Country, Age_Number) %>% 
  group_by(Country) %>% 
  summarise(Incidents = n(),
            Avg_Age = mean(Age_Number, na.rm = TRUE)) %>% 
  mutate(Avg_Age_Rounded = round(Avg_Age, digits = 0)) %>% 
  arrange(desc(Incidents)) %>% 
  head(10)

?head
#This was used above to select the first 10 values from the data frame above because 
#they were already arranged by most incidents

#Circular Bar Plot World Top 10 Avg Age:
id_Filtered_World_20_Yrs<- Filtered_World_Last_20_Years %>% 
  ## ggplot automatically sorts bars alphabetically for non-numeric x-axis
  ## in order to get labels in the right angle, need to calculate angles based on alphabetically sorted dataframe
  arrange(Country) %>%
  rowid_to_column(var='id') 

label_data2 <- id_Filtered_World_20_Yrs 
 
# calculate the ANGLE of the labels
number_of_bar2 <- nrow(label_data2)
angle <-  90 - 360 * (label_data2$id-0.5) /number_of_bar2
# I subtract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data2$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data2$angle<-ifelse(angle < -90, angle+180, angle)

#Plot:
ggplot(id_Filtered_World_20_Yrs, aes(x=Country, y=Avg_Age_Rounded)) +
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("pink", 0.7)) +
  ylim(-20,50) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-3,4), "cm")) +  # This removes unnecessary margin around plot
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  #Add labels to columns, prepare data:
  geom_text(data=label_data2, aes(x=Country, y=Avg_Age_Rounded+2, 
        label= paste(Country, "\n", Avg_Age_Rounded, "yrs old"), 
        hjust=hjust), color="black", fontface="bold",alpha=0.6, 
            size=2.5, angle= label_data2$angle, inherit.aes = FALSE)

ggsave(file="Average_Age_of_Shark_Attack_Victims_in_WORLD_1998_2017_Circular_Bar.svg", 
       width=15, height=15)


#Calculate average age of incidents by beach!!!
  
