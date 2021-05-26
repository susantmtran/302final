library(tidyverse)
library(tidytext)
library(stringr)
library(dplyr)

macaroni <- readRDS("data/processed/macaroni.rds")  # word-by-word data

macaroni_love <- macaroni %>%
  filter(word == "love" | word == "baby" | word == "heart" | word == "beautiful" | word == "kiss" | word == "soulmate") 

macaroni_love %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, -n))) +
  geom_col() +
  geom_text(aes(label = reorder(word, n)), 
            color = "black", size = 5, family = "Inconsolata",
            vjust = -0.6) +
  scale_fill_brewer(palette = "RdPu") +
  theme_classic() +
  ggtitle("Top 10 Most Frequently Used Words",
          subtitle = "Mac Miller Discography 2010–2020 ") +
  theme(text = element_text(family = "Inconsolata"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 10, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



