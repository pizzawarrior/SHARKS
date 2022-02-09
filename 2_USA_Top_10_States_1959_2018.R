#Top 10 states in USA by incident, 1959-2018

library(tidyverse)
library(svglite)
library(plotly)
library(geojsonio)
library(RColorBrewer)
library(rgdal)

#Read in Data
NEW_SHARKS <- read.csv("~/Desktop/EMERGENT WORKS/SHARKS/Datasets/NEW GSAF5.csv")

#Filter to desired dates and country
Post_1958<- NEW_SHARKS %>%
  filter(Year >1958 & Year <2018, Country == "USA")
  
#Select all states by incidents
USA_States <- Post_1958 %>%
  group_by(Area) %>% 
  summarise(Incidents=n()) %>% 
  arrange(desc(Incidents))

#HEXBINNNNNNN!!!!!
# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("/Users/ME/First-Repo/us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Show it
plot(spdf)

# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", 
      google_name)) spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

# Merge geospatial and numerical information
spdf_fortified <- spdf_fortified %>%
  left_join(. , USA_States, by=c("id"="Area")) 

# Make a first chloropleth map
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill =  Incidents, x = long, y = lat, group = group)) +
  scale_fill_gradient() +
  theme_void() +
  coord_map()

spdf_fortified<- spdf_fortified %>% 
  mutate(bin = case_when(Incidents >= 250 ~ 'More than 250'
                         ,Incidents >= 200 ~ 'More than 200, less than 250'
                         ,Incidents >= 100 ~ 'More than 100, less than 200'
                         ,Incidents >= 50 ~ 'More than 50, less than 100'
                         ,Incidents >= 0 ~ 'More than 0, less than 50'
                         ,TRUE ~ 'No reported incidents'))

fill_map1 <- c("More than 250" = "#a50f15",
               "More than 200, less than 250" = "#de2d26", 
               "More than 100, less than 200" = "#fb6a4a",
               "More than 50, less than 100" = "#fcae91",
               "More than 0, less than 50" = "#fee5d9",
                "No reported incidents" = "#969696")
            
fill_map1_vector= c("More than 250",
                    "More than 200, less than 250", 
                    "More than 100, less than 200",
                    "More than 50, less than 100",
                    "More than 0, less than 50",
                    "No reported incidents")

# plot
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=1, alpha=0.9 , color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="black", size=5, alpha=0.6) +
  theme_void() +
  scale_fill_manual(
    values = fill_map1, 
    breaks = fill_map1_vector, 
    labels = fill_map1_vector,
    guide = guide_legend(ncol = 1)) +
  ggtitle( "Shark Incidents USA 1959-2018" ) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    text = element_text(color = "#22211d"),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

ggsave(file="USA_Top_States_Hexbin.svg", width=15, height=8)
