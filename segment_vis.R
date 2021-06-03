library(tidyverse)

macaroni <- readRDS("data/processed/macaroni.rds") # word-by-word data

# chose Faces album because it has the most amount of songs.
macaroni <- macaroni %>%
  filter(album == "faces") %>%
  count(song)

ggplot(macaroni,
       aes(x = n,
           y = reorder(song, n))) +
  geom_point(color = "#ACDF87") +
  geom_segment(aes(x = 10, 
                   xend = n,
                   y = reorder(song, n),
                   yend = reorder(song, n)),
               color = "#96A563") +
  labs(x = "Number of Words",
       y = "",
       title = "FACES Album",
       subtitle = "Number of words in each song in 'Faces'") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Inconsolata"),
        plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("plots/faces_album.png")
