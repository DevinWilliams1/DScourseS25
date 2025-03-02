#Packages used for web scraping and API access
library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)

#Part 1: Web scraping 

#Page scraped and stored in TopScorers
TopScorers <- read_html("https://en.wikipedia.org/wiki/List_of_footballers_with_100_or_more_Premier_League_goals")
TopScorers

#Selecting the table from the page
TopPremierLeagueScorers <- TopScorers %>%
  html_nodes("table.wikitable") %>%
  .[[1]] %>% #Selecting first Table from page
  html_table()

#Printing the table
TopPremierLeagueScorers

#Part 2: API
url <- "https://transfermarket.p.rapidapi.com/transfers/list-market-value" 

#Rapidapi transfer market API format for accessing information 
queryString <- list(
  offset = "0",
  competitionIds = "IT1, GB1",
  orderByLatestUpdate = "true",
  domain = "de"
)

response <- GET(
  url, 
  query = queryString, 
  add_headers(
    'x-rapidapi-key' = '2a8a102715msh70bc50402b1d91ep1af3adjsn907fb6ad09fe', 
    'x-rapidapi-host' = 'transfermarket.p.rapidapi.com'
  )
)

#Check if request is working as required
http_status(response)

if (http_status(response)$category == "Success") {
  response_data <- content(response, "text", encoding = "UTF-8")
  parsed_data <- fromJSON(response_data)
}

#Print data as a tibble for further process and ordering afterwards
  as_tibble(parsed_data)
  
