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

album <- list("The Divine Feminine (2016)" = "the divine feminine",
              "K.I.D.S. (2010)" = "k.i.d.s.",
              "Macadelic (2012)" = "macadelic", 
              "Faces (2014)" = "faces", 
              "Swimming (2018)" = "swimming",
              "Circles (2020)" = "circles")

# second tab initial steps
wordcloud_data <- macaroni %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  filter(n() >= 10)

wordcloud_data <- wordcloud_data[!(wordcloud_data$word %in% c("yeah", "wanna", 
                                                              "uh","told", "til", "da", 
                                                              "i'ma", "em")),]

word_count <- count(wordcloud_data, word) 


ui <- navbarPage(
  tags$head(
    tags$style(HTML(
      "@import url('https://fonts.googleapis.com/css?family=Inconsolata');
      body{
        background-color: white;
        color: #8C6E68;
        font-family: 'Inconsolata';
        text-align: center;
                    }
      h2 {
        font-weight: bolder;s
        position: relative;
        text-align: left;
                    }
      .shiny-input-container {
        color: #474747;
                    }
      .navbar-default {
        background-color: #D9C1BF !important;
                    }
      .navbar-default:hover {
        background-color: #BF9B8E !important;}"))),
  
  tabPanel(
    "are you my soulmate, my angel? [love-related lyrics]",
    titlePanel("lyrics about love"),
    sidebarLayout(position = "right",
                  sidebarPanel(p(em("Yeah, are you my soulmate?")),
                     p(em("My angel, what do you want with me?")),
                     p("– Soulmate, The Divine Feminine"),
                     img(src = "mac.jpeg", width = "60%",
                         style = "display: block; margin-left: auto; margin-right: auto;"),
                     br(),
                     style = "background: #D9C1BF",
                     p("Using the dropdown menu below, select an album to visualize
                       the frequency of love-related lyrics in each of the album's
                       songs. You can also use the audio player to hear an instrumental
                       version of a song on the selected album."),
                     br(),
                     selectInput("album", label = NULL, choices = album),
                     p("Note that the default option, The Divine Feminine, is
                       the most love-heavy album. Fans have theorized that he wrote
                       the album for his high school sweetheart and/or his then-girlfriend
                       Ariana Grande, who is featured on My Favorite Part."),
                     br(),
                     "LYRICAL DATA PULLED FROM GENIUS"),
        mainPanel(plotOutput("lovecountPlot"),
                  br(), br(),
                  htmlOutput("song")))),
  tabPanel(
    "just a conversation [lyrical wordcloud]",
    titlePanel("word frequency throughout mac miller's lyrics"),
    sidebarLayout(position = "left",
        sidebarPanel(p(em("Yeah, it ain't your money 'til you make it")),
                     p(em("Otherwise, it's just a conversation")),
                     p("– Conversation Pt. 1, Swimming"),
                     style = "background: #ffcba4",
                     img(src = "orangemac.png", width = "50%",
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
    filter(love_counts, album %in% input$album)})
  
  music <- reactive(switch(input$album,
                  "the divine feminine" = "thedivinefeminine.mp3",
                  "k.i.d.s." = "kids.mp3",
                  "macadelic" = "macadelic.mp3",
                  "faces" = "faces.mp3",
                  "swimming" = "swimming.mp3",
                  "circles" = "circles.mp3"))
  
  songs <- reactive(switch(input$album,
                           "the divine feminine" = "Planet God Damn (feat. Njomza) by Mac Miller",
                           "k.i.d.s." = "The Spins by Mac Miller",
                           "macadelic" = "macadelic.mp3",
                           "faces" = "faces.mp3",
                           "swimming" = "Self Care by Mac Miller",
                           "circles" = "Good News by Mac Miller"))
  
  output$lovecountPlot <- renderPlot({
    ggplot(lovecount_subset(), aes(x = song, love_count)) +
      geom_col(fill = "#D9C1BF") +
      theme_bw() +
      labs(title = input$album,
           subtitle = "",
           x = NULL,
           y = "\n'love' count by song*",
           caption = "\n*includes love, loved, lovely, lover, lovers, loves, lovin") +
      scale_x_discrete(expand = c(0,0.8)) +
      scale_y_continuous(expand = c(0,0)) +
      coord_flip() +
      theme(text = element_text(family = "Inconsolata"),
            axis.title = element_text(size = 14),
            axis.ticks.y = element_blank(),
            plot.title = element_text(family = "Royal Acid", size = 20, hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))})
  
  output$song <- renderText({ 
    paste("<b>PRESS THE PLAY BUTTON TO LISTEN TO</b>", songs())
  })
  
  # music player, need to figure out how to not make the music players duplicate
  observeEvent(input$album, {
    insertUI(selector = "#album",
             where = "afterEnd",
             ui = tags$audio(src = music(), type = "audio/mp3", controls = NA))},
    )
  
#  eventReactive(input$album, {
#    removeUI(
#      selector = "#album")})
  
  output$wordcloud <- renderWordcloud2({
    wordcloud2(word_count, color = rep_len(
      c("orange", "#d0c816", "#f2905a", "#ff8668"), 
      nrow(word_count)), shape = "circle", size = 0.4)})
}


shinyApp(ui = ui, server = server)
