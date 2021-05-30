library(tidyverse)
library(tidytext)
library(showtext)
library(extrafont)
library(ggthemes)
library(RColorBrewer)
library(ggrepel)

malcolm <- readRDS("data/processed/malcolm.rds") # lyrical data


##ggplot(each_song, aes(x = year, y = count, group = 1)) +
##  geom_line(aes(group = 1), color = "#71a6d2") +
 ## geom_point(size = 3, color = "#708090") +
##  labs(title = "Release of Mac Miller's Songs Across Active Years", 
##     caption = "Odd years were inactive.",
##    x = "Year", y = "Number of Songs Released") +
##  theme_linedraw() +
##  theme(text = element_text(family = "Inconsolata"),
##       plot.title = element_text(family = "Royal Acid", size = 20, 
##       hjust = 0.5, color = "#6699cc"),
##       plot.caption = element_text(size = 10, hjust = 1),
##       axis.title = element_text(family = "Royal Acid", color = "#708090"))


year_data <- data.frame(year = 2010:2020,
                   count = c(18, 0, 17,0, 24,0, 10,0, 13,0, 12))

ggplot(year_data, aes(x = year, y = count, group = 1)) +
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
  scale_x_continuous(name = "Year", breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))

ggsave("plots/songs_per_year.png")



