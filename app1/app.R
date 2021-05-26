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
library(patchwork)

macaroni <- readRDS("data/processed/macaroni.rds")  # word-by-word data
love_words <- sort(unique(str_subset(macaroni$word, "^lov")))
love_counts <- macaroni %>%
    group_by(album, song) %>%
    summarise(love_count = sum(word %in% love_words))
top_love <- love_counts %>%
    top_n(5, love_count)


album <- list("Circles" = "circles", 
              "Faces" = "faces", 
              "Macadelic" = "macadelic", 
              "Swimming" = "swimming",
              "The Divine Feminine" = "the divine feminine")

ui <- fluidPage(

    # Application title
    titlePanel("Songs with Most Frequent 'Love' by Album"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Pick Mac Miller's albums"),
            selectInput("album", label = "Select album below", 
                        choices = album)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("lovecountPlot")
        )
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
                theme_minimal() +
                coord_flip() +
                labs(x = "Songs in Album", y = "Number of 'Love's") +
                theme(text = element_text(family = "Inconsolata"),
                      axis.title = element_text(size = 14),
                      axis.text.x = element_text(),
                      plot.title = element_text(family = "Royal Acid", size = 10, hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5))
                
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
