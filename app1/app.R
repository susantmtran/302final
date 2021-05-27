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
library(markdown)
library(patchwork)

macaroni <- readRDS("data/processed/macaroni.rds")  # word-by-word data
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

function(input, output, session)
output$summary <- renderPrint({
    summary(macaroni)
})

ui <- navbarPage(
    tags$head(
        tags$style("label{font-family: Inconsolata;}")
    ),
    tabPanel("Love",
    # Application title
    titlePanel("LOVE in Mac Miller's Discography"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(position = "right",
        sidebarPanel(
            img(src = "mac.jpeg", width = "80%", style="display: block; margin-left: auto; margin-right: auto;"), br(),
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
    tabPanel("2love",
             verbatimTextOutput("summary")))


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
                scale_y_continuous(expand = c(0,0.3)) +
                coord_flip() +
                theme(text = element_text(family = "Inconsolata"),
                      axis.title = element_text(size = 14),
                      axis.text.x = element_text(),
                      plot.title = element_text(family = "Royal Acid", size = 10, hjust = 0.5),
                      plot.subtitle = element_text(hjust = 0.5))
            
                
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
