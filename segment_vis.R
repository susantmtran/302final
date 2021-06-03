library(tidyverse)

macaroni <- readRDS("data/processed/macaroni.rds") # word-by-word data

# chose Faces album because it has the most amount of songs.
faces <- macaroni %>%
  filter(album == "faces") %>%
  count(song)


faces %>% 
  ggplot(aes(n, reorder(song, n))) +
  geom_point(color = "#8C6E68") +
  geom_segment(aes(x = 10, xend = n,
                   y = reorder(song, n), yend = reorder(song, n)),
               color = "#BF9B8E") +
  labs(x = NULL,
       y = NULL,
       title = "Faces (2014)",
       subtitle = "Word Count by Song",
       caption = "The final song, 55, is an instrumental piece.") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Inconsolata"),
        plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plots/faces_album.png")
