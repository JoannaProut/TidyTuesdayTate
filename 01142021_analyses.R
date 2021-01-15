#Tidy Tuesday, January 2021 Week 2
#Artists and Artwork from the Tate Gallery

library (tidyverse)
library (treemapify)
library (showtext)

## Loading Google fonts (https://fonts.google.com/)
font_add_google("Montserrat")

## Automatically use showtext to render text
showtext_auto()

#read in data
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

#changing artists$name to artists$artist so it is the same between files
artists$artist <- artists$name

#merging files, keep only artists born during or after the 20th century, select variables
merged <- merge (artwork, artists, by = "artist") 

#selecting female artists with the most works in Tate
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
  geom_treemap ()+
  geom_treemap_text (color = "black", family = "Montserrat", size = 11) +
  labs (title = "Most Represented Women Artists at the Tate",
        subtitle = "\n The Tate Modern houses the UK's national collection of British Art \n in addition to modern and contemporary art from across the world.\n \n  Only 16% of artists represented are female. \n \n This chart highlights the women with the greatest numbers of works. \n ",
        caption ="\nSource: Tate Art Museum | Graphic: @joieprout" ) +
   theme(
    text = element_text(family = "Montserrat", size = 12), 
    plot.title = element_text(size = 18, face = "bold"),
    plot.caption = element_text(size = 10),
    legend.position = "none",
    plot.margin=unit(c(0.5,0.5,0.5, 0.5),"cm")
   ) +
  scale_fill_brewer(palette ="BuPu") 
    
