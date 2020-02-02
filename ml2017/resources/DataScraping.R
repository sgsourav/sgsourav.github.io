# =======================================================
# Data Scraping from the Web using "rvest" package in R
# =======================================================

# rvest Web Scraping Library : https://github.com/hadley/rvest
# install.packages("rvest")
library(rvest)


# =======================================================
# Example 1 : Scrape Data from IMDB
# =======================================================

# Some IMDB IDs from MovieLens 
# https://grouplens.org/datasets/movielens/
movieDB <- read.csv("imdbLinks.csv", header = TRUE)
str(movieDB)

# Create the IMDB URLs for the movies
movieDB$imdbURL <- paste("http://www.imdb.com/title/tt0", movieDB$imdbId, "/", sep = "")

# Elements that we plan to extract
movieDB$Title <- NA
movieDB$Year <- NA
movieDB$Rating <- NA
movieDB$Genre <- NA
movieDB$Summary <- NA

# Populate the fields by web-scraping
# CAUTION -- It will take a long time
# Quick results : Use less iterations

for (i in 1:nrow(movieDB)) {
  # Access the Movie
  movie <- read_html(movieDB$imdbURL[i])
  
  # Scrape Title
  movieTitle <- movie %>% html_nodes("h1") %>% .[[2]] %>% html_text()
  movieTitle <- gsub("\\(.*", "", movieTitle)
  movieTitle <- strip
  movieDB$Title[i] <- ifelse(length(movieTitle)==0, NA, movieTitle)
  
  # Scrape Year
  movieYear <- movie %>% html_nodes("h1 span#titleYear a") %>% html_text() %>% as.numeric()
  movieDB$Year[i] <- ifelse(length(movieYear)==0, NA, movieYear)
  
  # Scrape Rating
  movieRating <- movie %>% html_nodes("strong span") %>% html_text() %>% as.numeric()
  movieDB$Rating[i] <- ifelse(length(movieRating)==0, NA, movieRating)
  
  # Scrape Primary Genre
  movieGenre <- movie %>% html_nodes("[itemprop=genre]") %>% .[[1]] %>% html_text()
  movieDB$Genre[i] <- ifelse(length(movieGenre)==0, NA, movieGenre)
  
  # Scrape Summary
  movieSummary <- movie %>% html_nodes(".summary_text") %>% html_text()
  movieDB$Summary[i] <- ifelse(length(movieSummary)==0, NA, movieSummary)
}

# Check the scraped data
str(movieDB)
head(movieDB)


# =======================================================
# Example 2 : Scrape Table from Election Results India
# =======================================================

# Access ElectionResults Website
upResultsURL <- "http://eciresults.nic.in/PartyWiseResultS24.htm?st=S24"
upResultsPage <- read_html(upResultsURL)

# Fetch the table with results
upResults <- upResultsPage %>%
  html_nodes("#div1 > table:nth-child(2)") %>%
  html_table() %>%
  .[[1]]

# Sanitize the table for clarity
names(upResults) <- upResults[3, ]
upResults <- upResults[-c(1, 2, 3, 13, 14, 15), ]

# Print the sanitized data
upResults


# =======================================================
# Example 3 : Scrape Table from Wikipedia
# =======================================================

# Download the webpage as an HTML file
download.file("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)",
              destfile = "wikiPage.html")

# Access the locally stored HTML file
wikiPage <- read_html("wikiPage.html")

# Extract and print the desired table
wikiPage %>% html_nodes(".wikitable") %>% .[[1]] %>% html_table()
