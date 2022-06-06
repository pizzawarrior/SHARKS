library(tidyverse)
library(ggplot2)
library(lubridate)

#Insert this to avoid Scientific Notation:
options(scipen=999)

#Read in Data
DF <- read.csv("/Users/ME/Desktop/Tesla PS Charts - Fig 1.csv")

#Alternate way to ditch scientific notation:
#DF<- DF %>% 
#mutate(No.sci = format(Percent.of.Market.Cap...10x, 
   # scientific = FALSE, big.mark = ","))

summary(DF$Percent.of.Market.Cap...10x)

#how to determine class? (is the date operating as a date?)
class(DF$caldt)
class(DF$Percent.of.Market.Cap...10x)

#or:
str(DF.date)
#caldt needs to be changed to a date format
#may need to use scale in the ggplot call to get it to capture the decimals

#Turn caldt column into useable date
DF.date <- DF %>% 
  mutate(clean_date = mdy(caldt)) %>% 
  select(clean_date, Percent.of.Market.Cap...10x)

# Plot
ggplot(DF.date, aes(x=clean_date, y=Percent.of.Market.Cap...10x)) +
  scale_y_continuous(name="% of market cap 10x", limits=c(0, 0.3)) +
  geom_line()
