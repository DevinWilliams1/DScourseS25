# Set CRAN mirror (This seems to be causing issues)
options(repos = c(CRAN = "https://cran.rstudio.com/"))

#Packages to install
install.packages('sparklyr')
install.packages('tidyverse')

#Load packages
library(sparklyr)
library(tidyverse)

#Set up a spark connection
sc <- spark_connect(master = "local")

#Create a tibble
df1 <- as_tibble(iris)

#Copy the tibble into Spark
df <- copy_to(sc, df1)

#Verify two dateframe types 
class(df1)
class(df)

#Check column names
colnames(df)
colnames(df1)

#Applying select
df %>%
	select(Sepal_Length,Species) %>%
		head %>% 
			print

#Applying filter
df %>%
	filter(Sepal_Length>5.5) %>%
		head %>% 
			print

#Putting them in one line
df %>% filter(Sepal_Length>5.5) %>% select(Sepal_Length,Species) %>% head %>% print

#Group by function test
df2 <- df %>% group_by(Species) %>% summarize(mean=mean(Sepal_Length),count = n()) %>% head %>% print

#Sort function test
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print

df2 %>% arrange(Species) %>% head %>% print