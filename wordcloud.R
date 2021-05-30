library(tidyverse)
library(tidytext)
library(tm)
library(dplyr)
library(readr)
library(showtext)
library(extrafont)
library(ggthemes)
library(RColorBrewer)
library(ggrepel)
library(shiny)
library(wordcloud2)
library(markdown)
library(hwordcloud)
library(plyr)
library(wordcloud)
library(webshot)

# reading in word-by-word data
macaroni <- readRDS("data/processed/macaroni.rds")  

wordcloud_data <- macaroni %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  filter(n() >= 10)

wordcloud_data <- wordcloud_data[!(wordcloud_data$word %in% c("yeah", "wanna", 
                                                   "uh","told", "til", "da", "i'ma", "em")),]

word_count <- count(wordcloud_data, 'word') 

mywordcloud <- wordcloud2(data = word_count, color = rep_len(
    c("orange", "#d0c816", "#f2905a", "#ff8668"), 
       nrow(word_count)), shape = "circle") 

htmlwidgets::saveWidget(mywordcloud, file = "mywordcloud.html", selfcontained = F)

webshot("mywordcloud.html", "fig_1.png", delay = 20, vwidth = 2000, vheight = 2000)

webshot::install_phantomjs()


