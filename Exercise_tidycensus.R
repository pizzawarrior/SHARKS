library(tidyverse)
install.packages("tidycensus", dep=T)

census_api_key("53d876fde493c706f48c6bd05e891c182876bf67", install = TRUE)

readRenviron("~/.Renviron")

library(tidycensus)

year <- c(1959:2018)

for(x in year){
  #print (x)
  get_decennial(geography = "state", 
          variables = "P013001", #change to pop variable
          year = x)
}
          