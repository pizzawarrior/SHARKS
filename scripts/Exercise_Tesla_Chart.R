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

DF2<- DF %>% 
mutate(rounded = signif(Percent.of.Market.Cap...10x, digits = 4))

summary(DF$Percent.of.Market.Cap...10x)

DF.date <- DF %>% 
  mutate(clean_date = mdy(caldt))

# Plot
ggplot(DF.date, aes(x=clean_date, y=Percent.of.Market.Cap...10x)) +
  scale_y_continuous(name="% of market cap 10x", limits=c(0, 0.3)) +
  geom_line()

#how to determine class? (is the date operating as a date?)
class(DF$caldt)
class(DF$Percent.of.Market.Cap...10x)

#or:
str(DF.date)
#caldt needs to be changed to a date format
#may need to use scale in the ggplot call to get it to capture the decimals