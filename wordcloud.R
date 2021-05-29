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

# reading in word-by-word data
macaroni <- readRDS("data/processed/macaroni.rds")  

# first tab (initial steps)
love_words <- sort(unique(str_subset(macaroni$word, "^lov")))
love_counts <- macaroni %>%
  group_by(album, song) %>%
  summarise(love_count = sum(word %in% love_words))
top_love <- love_counts %>%
  top_n(5, love_count)

album <- list("The Divine Feminine" = "the divine feminine",
              "Faces" = "faces", 
              "Macadelic" = "macadelic", 
              "Swimming" = "swimming",
              "Circles" = "circles")

# second tab (initial steps)
medium.corpus = Corpus(VectorSource(macaroni$word))
medium.corpus = medium.corpus %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("SMART"))
         
tdm = TermDocumentMatrix(medium.corpus) %>%
  as.matrix()
words = sum(rowSums(tdm), decreasing = TRUE)

df = data.frame(word = names(words), freq = words)

new <- macaroni %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  filter(n() >= 10)

myCorpus = Corpus(VectorSource(new))
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
m = as.matrix(myDTM)


graph <- count(new, 'word') 
wordcloud2(graph)





