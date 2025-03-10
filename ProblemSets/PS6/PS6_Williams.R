#Required libraries
library(rvest)
library(tidyverse)

#Read in HTML
transfers <- read_html("https://en.wikipedia.org/wiki/List_of_most_expensive_association_football_transfers")

# Get all tables with class "wikitable"
all_tables <- transfers %>%
  html_elements("table.wikitable")

# Select the top 50 most expensive transfers
top50 <- all_tables[1]

# Convert to data frame for analysis
top50_df <- html_table(top50) %>% 
  as.data.frame()

#cleaning the data
top50_df <- top50_df %>%
  select(-1,-7,-9) #Removes Rank, fee in pounds, and reference list

#Change column names
top50_df <- top50_df %>%
  mutate(across(everything(), ~gsub("\\[.*?\\]", "", as.character(.)))) %>%
  rename(
    Fee_Euros = 'Fee...million.',
    New_Club = "To",
    Old_Club = "From"
  )

#Convert the Fee in Euros tabe to numeric and clean it to remove commas and euro symbols
top50_df$Fee_Euros <- as.numeric(gsub("[€,]", "", top50_df$Fee_Euros))

#Create a new row showing the league that each buying club is in
league_mapping <- c(
  #Premier Legues Teams
  "Manchester United" = "Premier League",
  "Chelsea" = "Premier League",
  "Manchester City" = "Premier League",
  "Liverpool" = "Premier League",
  "Arsenal" = "Premier League",
  "Newcastle United" = "Premier League",
  
  #La Liga Teams
  "Barcelona" = "La Liga",
  "Real Madrid" = "La Liga",
  "Atlético Madrid" = "La Liga",
  
  #Ligue 1 Teams
  "Paris Saint-Germain" = "Ligue 1",
  
  #Serie A Teams
  "Juventus" = "Serie A",
  "Inter Milan" = "Serie A",
  "Napoli" = "Serie A",
  
  #Bundesliga Teams
  "Bayern Munich" = "Bundesliga",
  
  #Saudi Pro League Teams
  "Al Nassr" = "Saudi Pro League",
  "Al Hilal" = "Saudi Pro League"
  
)

#Add the data to the league column
top50_df$League_of_New_Club <- league_mapping[top50_df$New_Club]

#Create a new variable called max fee to use in the scale_y_discrete function
max_fee <- top50_df$Fee_Euros[which.max(top50_df$Fee_Euros)]

#Print to check the data
print(top50_df)

#Plot 1
ggplot(top50_df) + 
  geom_bar(aes(x = fct_rev(fct_infreq(Position))),
           fill = "steelblue",
           color = "black",
           alpha = 0.8) +
  geom_text(aes(x = fct_rev(fct_infreq(Position)), label = after_stat(count)),
            stat = "count", vjust = -0.5, size = 3.5) +
  labs(title = "Distribution of the Most Expensive Football Transfers by Position",
        subtitle = "Based on the 50 most expensive transfers of all time",
         caption = "Data Source: Wikipedia",
            x = NULL, #removed since it is clear without it
            y = "Number of Players") +
  theme_minimal() + 
#Additional elements to improve look
   theme (
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = margin(20,20,20,20)
  )

#Plot 2
ggplot(top50_df) + 
  geom_point(aes(x = Year , y = reorder(Fee_Euros, Fee_Euros), 
                 color = League_of_New_Club),
                 size = 4,
                 stroke = 0.5) +
  labs(title = "Most Expensive Transfer Fees by League",
       subtitle = "Based on the 50 most expensive transfers of all time",
       caption = "Data Source: Wikipedia",
       x = "Year of Transfer",
       y = "Price in Euros") +
  theme_minimal() +
  theme(
  plot.margin = margin(20,20,20,20)
  ) +

  #Add empty years to show progression of transfer market
  scale_x_discrete(
    limits = as.character(seq(min(top50_df$Year), max(top50_df$Year), by = 1))
    ) +
  #Add breaks to the y axis to improve look while keeping max value
  scale_y_discrete(
    breaks = function(y) {
      selected_breaks <- y[seq(1, length(y), by = 3)]
if (!max_fee %in% selected_breaks) {
   selected_breaks <- c(selected_breaks, max_fee)
    } 
return(selected_breaks) 
    }
)

#Create new variable to be used in plot 3 that will allow a look at the top three years for large transfers
top_years <- top50_df %>%
  count(Year) %>%
  arrange(desc(n)) %>%
  slice_head(n=3) %>%
  pull(Year)

filtered_df <- top50_df %>%
  filter(Year %in% top_years)

year_counts <- filtered_df %>%
  count(Year)

#Plot 3
ggplot(filtered_df) +
  geom_dotplot(aes(x = Year),
               dotsize = 0.2,
               stackratio = 1.2, 
               binwidth = 0.3,
               fill = "steelblue",
               color = 'black') +
  geom_text(data = year_counts,
            aes(x = Year, y = 0.95, label = n),
            fontface = "bold",
            size = 4.5,
            color = "black") +
  scale_y_continuous(breaks = NULL, limits = c(0,1)) +
  labs(title = "Top 3 Years with the most top 50 transfers",
       subtitle = "Based on the 50 most expensive transfers of all time",
       caption = "Data Source: Wikipedia",
       y = "", #Removed both labels as it is clear without it
       x = "") +
  theme_minimal() +
 #Additional changes to theme
   theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 11),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20,20,20,20)
  )

