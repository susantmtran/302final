library(tidyverse)
library(tidytext)
library(showtext)
library(extrafont)
library(ggthemes)
library(RColorBrewer)
library(ggrepel)
library(patchwork)

# load data
malcolm <- readRDS("data/processed/malcolm.rds") # lyrical data
macaroni <- readRDS("data/processed/macaroni.rds") # word-by-word data

# pretty fonts
# royal acid (the bold pretty one)
# https://www.dafont.com/royal-acidbath.font

# inconsolata (the skinny delicate one)
# https://fonts.google.com/specimen/Inconsolata
# install these on the mac "font book" first and then do font_import()
# which i think requires the showtext package

# how many songs per album? (how many "first lines" are there?)
malcolm %>%
  filter(line == 1) %>%
  ggplot(aes(album, fill = album)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..),
            size = 5, family = "Inconsolata", vjust = 2) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  ggtitle("Number of Songs per Album",
          subtitle = "Mac Miller Discography 2010–2020 ") +
  theme(text = element_text(family = "Inconsolata"),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# most common words
macaroni %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, -n))) +
  geom_col() +
  geom_text(aes(label = reorder(word, n)), 
            color = "white", size = 5, family = "Inconsolata",
            hjust = 1.5) +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() + # switch x and y
  theme_classic() +
  ggtitle("Top 10 Most Frequently Used Stop Words",
          subtitle = "Mac Miller Discography 2010–2020 ") +
  theme(text = element_text(family = "Inconsolata"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# most common non-stop words
macaroni %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, -n))) +
  geom_col() +
  geom_text(aes(label = reorder(word, n)), 
            color = "white", size = 5, family = "Inconsolata",
            hjust = 1.5) +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() + # switch x and y
  theme_classic() +
  ggtitle("Top 10 Most Frequently Used Non-Stop Words",
          subtitle = "Mac Miller Discography 2010–2020 ") +
  theme(text = element_text(family = "Inconsolata"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# macaroni subsets by album
macaroni %>% 
  filter(album == "swimming") %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  filter(word != "yeah") %>%
  top_n(5) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, -n))) +
  geom_col() +
  geom_text(aes(label = reorder(word, n)), 
            color = "white", size = 5, family = "Inconsolata",
            hjust = 1.5) +
  scale_fill_brewer(palette = "Blues") +
  coord_flip() + # switch x and y
  theme_classic() +
  ggtitle("Top 5 Most Frequently Used Non-Stop Words",
          subtitle = "Swimming (2018)") +
  theme(text = element_text(family = "Inconsolata"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

macaroni %>% 
  filter(album == "the divine feminine") %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>%
  filter(word != "yeah") %>%
  top_n(5) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, -n))) +
  geom_col() +
  geom_text(aes(label = reorder(word, n)), 
            color = "white", size = 5, family = "Inconsolata",
            hjust = 1.5) +
  scale_fill_brewer(palette = "Reds") +
  coord_flip() + # switch x and y
  theme_classic() +
  ggtitle("Top 5 Most Frequently Used Non-Stop Words",
          subtitle = "The Divine Feminine (2016)") +
  theme(text = element_text(family = "Inconsolata"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
