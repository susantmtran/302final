# focusing on love aspect of mac miller songs

library(tidyverse)
library(tidytext)
library(showtext)
library(extrafont)
library(ggthemes)
library(RColorBrewer)
library(ggrepel)
library(patchwork)

macaroni <- readRDS("data/processed/macaroni.rds")  # word-by-word data

macaroni_love <- macaroni %>%
  filter(word == "love" | word == "baby" | word == "heart" | word == "beautiful" | word == "kiss" | word == "sex") 

love_words <- macaroni_love %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = reorder(word, -n))) +
  geom_col() +
  geom_text(aes(label = n),
            color = "black", size = 5, family = "Inconsolata",
            vjust = -0.6) +
  scale_fill_brewer(palette = "RdPu") +
  theme_classic() +
  ggtitle("Frequently Used Words: Love Edition", 
          subtitle = "Mac Miller Discography 2010–2020 ") +
  theme(text = element_text(family = "Inconsolata"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(family = "Royal Acid", size = 15, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plots/love_common_words.png")



