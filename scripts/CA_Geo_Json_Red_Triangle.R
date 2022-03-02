library(tidyverse)
library(geojsonio)
library(jsonlite)
library(broom)

# read data ---------------------------------------------------------------


# Link data ---------------------------------------------------------------

## json
json_file <- "https://query.data.world/s/x2fg2potwplsdxughuusj5zjei5gon"

## csv
csv_file <- "https://flourish.studio/data/workshop/countries.csv"

##geojson
geojson_file <- "https://raw.githubusercontent.com/deldersveld/topojson/master/countries/us-states/CA-06-california-counties.json"

# read data ---------------------------------------------------------------
cookies <-read_csv(csv_file)
?read_csv
mexico <-geojson_read(geojson_file, what = "sp")
mexico_clean <-tidy(mexico)
ggplot()+geom_polygon(data=mexico_clean,aes(x=long, y=lat,group=group),fill="blue",
color="white") + theme_void() + coord_map()
