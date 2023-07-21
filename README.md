
# SHARKS

### Published portfolio: https://www.iannorstad.com/sharks

An ongoing exploration of shark attacks (encounters/ incidents) from 1959- 2018. The focus is primarily on California, and how the state compares to other high risk areas around the globe. 

Using the Global Shark Attack File I was able to address specific questions I have about the occurrence of shark attacks in California. These questions are:

* Are shark encounters increasing or decreasing?
* Where are the places with the highest rate of encounters?
* How does population play a role in reports of shark encounters?
* How does seasonality play a role in encounters?
* Is Northern California, especially the Red Triangle really that much more dangerous than the rest of California?


## Setup
______________________________________________________________________________________________________________________________

Once you have forked or cloned this repository please verify tidyverse is installed in R studio. 

```
install.packages("tidyverse")
```


## Built With
______________________________________________________________________________________________________________________________

* R
* `tidyverse`


## Data Source
______________________________________________________________________________________________________________________________

The primary data source used for this project was sourced from the Global Shark Attack File:

https://www.sharkattackfile.net/incidentlog.htm

This dataset has been collected by the Shark Research Institute and contains records going back to BC times. I chose to filter the data from 1959-2018 on most of the plots I created because that is a significant range that can be considered both modern and recent, and seems to have reliable record keeping. Surfing also surged in popularity during this period, which I felt was especially relevant for this project.


Census data came from here (no API key needed:)

https://www.census.gov/programs-surveys/acs/microdata/access.2012.html

## Directory Structure
* `Data` folder contains datasets used to generate the plots for this project
* `Scripts` folder contains all R code used to generate plots for the portfolio
* The prefix number in each script title corresponds to plot number in the portfolio, some are repetetive as works in progress/ exercises

### Thank you for looking!
