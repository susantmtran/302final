#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## work in progrresss 

library(tidyverse)
library(tidytext)
library(showtext)
library(extrafont)
library(ggthemes)
library(RColorBrewer)
library(ggrepel)
library(wordcloud2)
library(markdown)
library(patchwork)

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
    tags$style("label{font-family: Inconsolata;}")
  ),
  tabPanel("Love",
           tags$audio(src = "instrumental.mp3", type = "audio/mp3", autoplay = NA, controls = NA),
           # Application title
           titlePanel("LOVE in Mac Miller's Discography"),
           
           # Sidebar with a slider input for number of bins 
           sidebarLayout(position = "right",
                         sidebarPanel(
                           img(src = "mac.jpeg", width = "60%", style="display: block; margin-left: auto; margin-right: auto;"), br(),
                           style = "background: #ffd1dc",
                           helpText("Pick one of Mac Miller's albums to see how many times 'Love' 
                     (or some variation of 'Love') is in his songs."),
                           selectInput("album", label = "Select album below", 
                                       choices = album), br(),
                           "Lyrical data used from___  source"
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           plotOutput("lovecountPlot")
                         )
           )
  ),
  tabPanel("Lyrical Word Cloud",
           titlePanel("Mac's Word Cloud"),
           sidebarLayout(position = "right",
                         sidebarPanel(style = "background:#ffcba4",
                                      img(src = "orangemac.png", width = "80%", style="display: block; margin-left: auto; margin-right: auto;"), br(),
                                      helpText("Hover over the words to see how many times each non-stop
                                       word appeared in Mac Miller's song collection."),
                                      br(),
                                      "Data used from ___source"
                         ),
                         mainPanel(
                           wordcloud2Output("wordcloud", width = "100%", height = "565px")))
  
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  lovecount_subset <- reactive({
    req(input$album)
    filter(love_counts, album %in% input$album)
    
  })
  
  output$lovecountPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(lovecount_subset(), aes(x = song, love_count)) +
      geom_col(fill = "#FFB6C1") +
      theme_bw() +
      labs(x = "Songs in Album", y = "Number of 'Love's") +
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
      nrow(word_count)), shape = "triangle-forward")})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
