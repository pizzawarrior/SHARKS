# % of incidents in CA by species with Fatal: Y/N: stacked barplot
# % of WHITE incidents that are fatal vs % of all others Fatal
#Norcal fatal rate vs Socal fatal rate: what kind of plot?

# a pie chart sectioned by type of shark for all of CA? (white, hammer, mako, other/ unconfirmed)

library(tidyverse)
library(svglite)
library(plotly)
library(scales)
library(forcats)
library(ggplot2)

#read in filtered dataset of 1958-2018 Cali Beaches
Cali_Beaches<- read.csv("~/First-Repo/data/GSAF5-Cali_Post_1958-2017_BEACHES.csv")

#Separate beaches and counties:
County_Spec_Fatal <- Cali_Beaches %>% 
  separate(col=Location.and.County..Cleaned., 
           into=c("Beach", "County"), sep=", ") %>% 
  filter(County != "NA")

County_Spec_Fatal<- County_Spec_Fatal %>% 
  select(Species, County, Fatal..Y.N.) %>% 
  filter(Fatal..Y.N. == "Y" | Fatal..Y.N. == "N")

#Let's case_when for all species we want to select
CA_types_fatal<- County_Spec_Fatal %>% 
  mutate(sharks = case_when(str_detect(Species, "(?i)white")~ "white",
             str_detect(Species, "(?i)mako")~ "mako",
              str_detect(Species, "(?i)hammerhead")~ "hammerhead",
                TRUE~ "other")) %>% 
  group_by(sharks, Fatal..Y.N.) %>% 
  summarize(Incidents=n())

#Stacked barplot!!!
ggplot(CA_types_fatal, aes(fill= Fatal..Y.N., y=Incidents, x=sharks)) + 
  geom_bar(position="stack", stat="identity")

#We can try ascending order:
ggplot(CA_types_fatal, aes(forcats::fct_reorder(sharks, Incidents, sum), 
          Incidents, fill = Fatal..Y.N.)) +
  geom_col() + xlab('Species')

#ggsave(file="17.CA_shark_species_Fatal.svg", width=15, height=8)

#Let's break sharks into 2 groups: white and other then produce a Fatal: Y, N plot
Other<- c("other" , "hammerhead", "mako")

#Combine species of shark and sum incidents by fatal Yes/ No
CA_types_fatal_other<- CA_types_fatal %>% 
  mutate(Shark= ifelse(sharks %in% Other, yes = "other", no = "white")) %>% 
  group_by(Shark, Fatal..Y.N.) %>% 
  summarise(sum_Incidents = sum(Incidents))

#Let's check our work:
sum(CA_types_fatal_other$sum_Incidents)
#231

sum(CA_types_fatal$Incidents)
#231, Looks good!

#Grouped Barchart party:
# Grouped
ggplot(CA_types_fatal_other, aes(fill=Fatal..Y.N., y=sum_Incidents, x=Shark)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("CA Other= 80/6, White= 134/11")

ggsave(file="17.CA_White_vs_Other_Fatal_Non_Pie.svg", width=15, height=8)

CA_white<- CA_types_fatal_other %>% 
  filter(Shark == "white")

#Let's try as PIE:
ggplot(CA_white, aes(x=Shark, y=sum_Incidents, fill=Fatal..Y.N.)) +
  geom_bar(stat="identity", width=1, color="white") + #white is for the border around the triangle shape
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  ggtitle("CA White, 134/ 11")

#ggsave(file="17.CA_white_Fatal_Non_Pie.svg", width=15, height=8)

#Let's do it for Other too: 
CA_other<- CA_types_fatal_other %>% 
  filter(Shark == "other")

#PIE:
ggplot(CA_other, aes(x=Shark, y=sum_Incidents, fill=Fatal..Y.N.)) +
  geom_bar(stat="identity", width=1, color="white") + #white is for the border around the triangle shape
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  ggtitle("CA Other, 80/ 6")

#ggsave(file="17.CA_Other_Fatal_Non_Pie.svg", width=15, height=8)

