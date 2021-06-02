library(tidyverse)
library(tidytext)
library(showtext)
library(extrafont)
library(ggthemes)
library(RColorBrewer)
library(ggrepel)
library(gganimate)
library(gapminder)
library(gifski)

malcolm <- readRDS("data/processed/malcolm.rds") # lyrical data

each_song <- malcolm %>%
  filter(line == 1) %>%
  count(year) %>%
  mutate(year_integer = as.integer(year))

animation <- ggplot(each_song, aes(x = year, y = n, group = 1)) +
  geom_line(aes(group = 1), color = "#71a6d2") +
  geom_point(size = 3, color = "#708090") +
  labs(title = "Release of Mac Miller's Songs Across Active Years", 
       caption = "Odd years were inactive.",
       x = "Year", y = "Number of Songs Released") +
  theme_linedraw() +
  theme(text = element_text(family = "Inconsolata"),
        plot.title = element_text(family = "Royal Acid", size = 20, 
                                  hjust = 0.5, color = "#6699cc"),
        plot.caption = element_text(size = 10, hjust = 1),
        axis.title = element_text(family = "Royal Acid", color = "#708090")) +
  transition_reveal(year_integer)

animate(animation, height = 1000, width = 1000, res = 150,  renderer = gifski_renderer())
anim_save("plots/animation.gif")


