
#Sync up google goc with R studio for greater version control. This can be utilised
# when sharing the same file, or just to control the versions of your dataset so there 
# aren't so many different ones floating around. You can also make changes live
# in google docs and reload the dataset to refresh it

install.packages("googlesheets4")
library(googlesheets4)

Cali_Beaches_and_Counties_Google <- read_sheet("1zqLEEcUUlbvAAO75ZdbYkO3o1RwjA4yJvRfsGcst6Io")

read_sheet()
