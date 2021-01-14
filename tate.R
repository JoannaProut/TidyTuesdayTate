library (tidyverse)
library (janitor)
library (treemap)
library (scales)
library (forcats)



# Or read in the data manually

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")
artwork <- tuesdata$artwork

#changing artists$name to artists$artist so it is the same between files
artists$artist <- artists$name

#merging files
merged <- merge (artwork, artists, by = "artist") %>%
  select (artist, title, gender)

gen_count <- merged %>%
  group_by (gender) %>%
  summarize (n = n()) %>%
  adorn_totals (where = "row")