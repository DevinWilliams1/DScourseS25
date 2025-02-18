#Changes made based on "Zero exit status" when trying to install jsonlite as done within the problem set. 

# Set CRAN mirror (This seems to be causing issues)
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Check and install packages if needed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("jsonlite")) install.packages("jsonlite")

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
print("Dataframe class:")
print(class(mydf))  

#List the first 6 rows of the dataframe
print("First 6 rows:")
print(head(mydf))   