<<<<<<< HEAD
=======

## work in progrresss 

>>>>>>> cdf22cb571670aafb11cf0ef263e994aba65ad21
library(tidyverse)
library(tidytext)

library(showtext)
library(extrafont)
library(ggthemes)
library(RColorBrewer)

library(wordcloud2)
library(markdown)

# word-by-word data
macaroni <- readRDS("data/processed/macaroni.rds") 

# first tab initial steps
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

music <- list("The Divine Feminine" = "divinefeminine.mp3",
              "Faces" = "faces.mp3", 
              "Macadelic" = "macadelic.mp3", 
              "Swimming" = "swimming.mp3",
              "Circles" = "circles.mp3")

# second tab initial steps
wordcloud_data <- macaroni %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  filter(n() >= 10)

wordcloud_data <- wordcloud_data[!(wordcloud_data$word %in% c("yeah", "wanna", 
                                                              "uh","told", "til", "da", 
                                                              "i'ma", "em")),]

word_count <- count(wordcloud_data, 'word') 


ui <- navbarPage(
  tags$head(tags$style("label{font-family: Inconsolata;}")),
  tabPanel(
    "Love Lyrics",
    titlePanel("LOVE in Mac Miller's Discography"),
    sidebarLayout(position = "right",
        sidebarPanel(img(src = "mac.jpeg", width = "60%",
                         style = "display: block; margin-left: auto; margin-right: auto;"),
                     br(),
                     style = "background: #ffd1dc",
                     helpText("Pick one of Mac Miller's albums to see how many times 'Love' 
                     (or some variation of 'Love') is in his songs."),
                     selectInput("album", label = "Select album below", choices = album),
                     br(),
                     "LYRICAL DATA PULLED FROM GENIUS",
                     tags$audio(src = "swimming.mp3", type = "audio/mp3",
                                autoplay = NA, controls = NA)),
        mainPanel(plotOutput("lovecountPlot")))
    ),
  tabPanel(
    "Lyrical Word Cloud",
    titlePanel("Mac Miller's Word Cloud"),
    sidebarLayout(position = "right",
        sidebarPanel(style = "background: #ffcba4",
                     img(src = "orangemac.png", width = "80%",
                         style = "display: block; margin-left: auto; margin-right: auto;"),
                     br(),
                     helpText("Hover over the words to see how many times each non-stop
                     word appeared in Mac Miller's song collection."),
                     br(),
                     "LYRICAL DATA PULLED FROM GENIUS"),
        mainPanel(wordcloud2Output("wordcloud"),
                  width = 7)))
  )

server <- function(input, output) {
  
  lovecount_subset <- reactive({
    req(input$album)
    filter(love_counts, album %in% input$album)
    
  })
  
  output$lovecountPlot <- renderPlot({
    ggplot(lovecount_subset(), aes(x = song, love_count)) +
      geom_col(fill = "#FFB6C1") +
      theme_bw() +
      labs(x = NULL, y = "'Love' Count by Song") +
      scale_x_discrete(expand = c(0,0.8)) +
      scale_y_continuous(expand = c(0,0)) +
      coord_flip() +
      theme(text = element_text(family = "Inconsolata"),
            axis.title = element_text(size = 14),
            axis.text.x = element_text(),
            plot.title = element_text(family = "Royal Acid", size = 10, hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))})
  
  output$wordcloud <- renderWordcloud2({
    wordcloud2(word_count, color = rep_len(
      c("orange", "#d0c816", "#f2905a", "#ff8668"), 
      nrow(word_count)), shape = "circle")})
}

shinyApp(ui = ui, server = server)
