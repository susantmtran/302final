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

# special fonts for aesthetic

# royal acid (the bold pretty one)
# https://www.dafont.com/royal-acidbath.font

# inconsolata (the skinny delicate one)
# https://fonts.google.com/specimen/Inconsolata

# PLOT 1 ------------------------------------------------------------------
# how many songs per album? (how many "first lines" are there?)

malcolm %>% 
  mutate(album = factor(album, levels = c("k.i.d.s.", "macadelic", "faces",
                                          "the divine feminine", "swimming", "circles"))) %>% 
  filter(line == 1) %>%
  ggplot(aes(album, fill = album)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..),
            size = 5, family = "Inconsolata", vjust = 2) +
  # scale_fill_brewer(palette = "Set2") +
  scale_fill_manual(values = c("#DE9297", "#DEA5A4", "#DEB2A8",
                               "#E2C1B8", "#EFE3CE", "#E4D7B9")) +
  theme_classic() +
  ggtitle("Number of Songs per Album",
          subtitle = "In Order of Oldest to Newest Release") +
  theme(text = element_text(family = "Inconsolata"),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plots/songs_per_album.png")

# PLOT 2 ------------------------------------------------------------------
# number of song releases by year (animated)

library(gganimate)
library(gapminder)
library(gifski)

each_song <- malcolm %>%
  filter(line == 1) %>%
  count(year) %>%
  mutate(year_integer = as.integer(year))

animation <- ggplot(each_song, aes(x = year, y = n, group = 1)) +
  geom_line(aes(group = 1), color = "#BF9B8E") +
  geom_point(size = 3, color = "#8C6E68") +
  labs(title = "Song Releases by Year", 
       caption = "Excluding singles and/or other independent song releases.") +
  theme_linedraw() +
  theme(text = element_text(family = "Inconsolata"),
        axis.title = element_blank(),
        plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1)) +
  transition_reveal(year_integer)

animate(animation, height = 1000, width = 1000, res = 150,  renderer = gifski_renderer())

anim_save("plots/animation.gif")

# PLOT 3 ------------------------------------------------------------------
# top 10 words (including stop words and excluding them)

g1 <- macaroni %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, n))) +
  geom_col() +
  geom_text(aes(label = reorder(word, n)), 
            color = "black", size = 3, family = "Inconsolata",
            hjust = 2) +
  scale_fill_manual(
    values = c(scales::seq_gradient_pal("#D9C1BF", "#8C6E68")(seq(0, 1, length.out = 10)))) +
  coord_flip() + # switch x and y
  theme_classic() +
  ggtitle("Top 10 Most Frequently Used Words") +
  theme(text = element_text(family = "Inconsolata"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 10, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# most common non-stop words
g2 <- macaroni %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, n))) +
  geom_col() +
  geom_text(aes(label = reorder(word, n)), 
            color = "black", size = 3, family = "Inconsolata",
            hjust = 1.3) +
  scale_fill_manual(
    values = c(scales::seq_gradient_pal("#D9C1BF", "#8C6E68")(seq(0, 1, length.out = 10)))) +
  coord_flip() + # switch x and y
  theme_classic() +
  ggtitle("Top 10 Most Frequently Used Words",
          subtitle = "Excluding Stop Words") +
  theme(text = element_text(family = "Inconsolata"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 10, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

g1 | g2

ggsave("plots/most_common_words.png")

# PLOT 4 ------------------------------------------------------------------
# frequently used words pertaining to love

macaroni %>%
  filter(word == "love" | word == "baby" | word == "heart" |
           word == "beautiful"| word == "kiss" | word == "sex") %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, -n))) +
  geom_col() +
  geom_text(aes(label = n),
            color = "black", size = 5, family = "Inconsolata",
            vjust = -0.6) +
  scale_fill_brewer(palette = "RdPu") +
  theme_classic() +
  ggtitle("Wearing Rose-Colored Glasses",
          subtitle = "Frequently Used Words: Love Edition") +
  theme(text = element_text(family = "Inconsolata"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 15, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plots/love_common_words.png")

# PLOT 5 ------------------------------------------------------------------
# frequently used words pertaining to sadness

macaroni %>%
  filter(word == "sad" | word == "hurt" | word == "hate" |
           word == "cry" | word == "pain" | word == "break") %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, -n))) +
  geom_col() +
  geom_text(aes(label = n),
            color = "black", size = 5, family = "Inconsolata",
            vjust = -0.6) +
  scale_fill_brewer(palette = "PuBu") +
  theme_classic() +
  ggtitle("Feeling Blue (Da Ba Dee...)", 
          subtitle = "Frequently Used Words: Sad Edition") +
  theme(text = element_text(family = "Inconsolata"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 15, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plots/sad_common_words.png")

# PLOT 6 ------------------------------------------------------------------



