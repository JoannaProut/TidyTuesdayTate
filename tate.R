library (tidyverse)
library (janitor)
library (treemapify)
library (scales)
library (plotly)

#Tidy Tuesday Data for Tate Gallery Artists
#01.13.2021

# Or read in the data manually

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")
artwork <- tuesdata$artwork

#changing artists$name to artists$artist so it is the same between files
artists$artist <- artists$name

#merging files, keep only 20th century artists, select variables
merged <- merge (artwork, artists, by = "artist") %>%
  filter (yearOfBirth >= 1900) %>%
  select (artist, yearOfBirth, gender)

#fun way to reconfigure names from https://stackoverflow.com/questions/33826650/last-name-first-name-to-first-name-last-name
merged$artist  <- sapply(strsplit(merged$artist, split=", "),function(x) 
{paste(rev(x),collapse=" ")})
  
#filtering for male artists
m <- merged %>%
  filter (gender == "Male") %>%
  group_by (artist) %>%
  summarize (n = n()) %>%
  top_n (5) 

m$label <- paste(m$artist, m$n, sep = ", n = ")
  
#creating treemap for top 5 male artists
ggplot (m, aes (area= n, label = label)) +
  geom_treemap (fill = "skyblue3", color = "white") +
  geom_treemap_text (color = "white") +
  labs (title = "Male Artists") 

#filtering for female artists
f <- merged %>%
  filter (gender == "Female") %>%
  group_by (artist) %>%
  summarize (n = n()) %>%
  top_n (5)

f$label <- paste (f$artist, m$n, sep = ", n = ")

ftree <- ggplot (f, aes (area = n, label = label)) +
  geom_treemap (fill = "darksalmon", color = "white") +
  geom_treemap_text (color = "white") +
  labs (title = "Female Artists")

