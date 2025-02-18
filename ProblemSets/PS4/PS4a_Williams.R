# Set CRAN mirror (This seems to be causing issues)
options(repos = c(CRAN = "https://cran.rstudio.com/"))

#Removing lock file from within R to allow jsonlite to download
unlink("/home/ouecon010/R/x86_64-pc-linux-gnu-library/4.0/00LOCK-jsonlite", recursive = TRUE)

# Install Packages
install.packages('tidyverse')
install.packages('jsonlite')

#libraries needed
library(tidyverse)
library(jsonlite)

#Download file into R, and specify local name of file
system("wget -O dates.json \"https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en\"")

#Convert the JSON to a list
mylist <- fromJSON('dates.json')

#Convert the list into a data frame
mydf <- bind_rows(mylist$result[-1])

#Check class of dataframe that was created
class(mydf)

#List the first 6 rows of the dataframe
head(mydf)