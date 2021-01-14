#Tidy Tuesday, January 2021 Week 2
#Artists and Artwork from the Tate Gallery

library (tidyverse)
library (janitor)
library (treemapify)
library (kableExtra)

#read in data
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

#changing artists$name to artists$artist so it is the same between files
artists$artist <- artists$name

#merging files, keep only artists born during or after the 20th century, select variables
merged <- merge (artwork, artists, by = "artist") 

#selecting the 10 female artists with the most works in Tate
f <- merged %>%
  filter (gender == "Female") %>%
  select (artist, gender) %>%
  group_by (artist) %>%
  summarize (n = n()) %>%
  top_n (5)

#fun way to reconfigure names from https://stackoverflow.com/questions/33826650/last-name-first-name-to-first-name-last-name
f$artist <- sapply(strsplit(f$artist, split=", "),function(x) 
{paste(rev(x),collapse=" ")})

#add label
f$label <- paste (f$artist, f$n, sep = ", ")

#treemap
ggplot (f, aes (area = n, fill = artist, label = label)) +
  geom_treemap(radius = grid::unit(0.15, "cm"), color = "white", start = "topleft") +
  geom_treemap_text (color = "white") +
  labs (title = "Most Represented Women Artists at the Tate",
        subtitle = "The Tate Modern houses the United Kingdom's national collection of British Art
        in addition to modern and contemporary art from across the world. Only 16% of artists represented are female.  
        This chart highlights the artists who are women with the greatest numbers of work in the collection.",
        caption ="\nSource: Tate Art Museum | Graphic: @joieprout" ) +
  theme (legend.position = "none")
