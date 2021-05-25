#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

macaroni <- readRDS("data/processed/macaroni.rds")  # word-by-word data

ui <- fluidPage(

    # Application title
    titlePanel("Top 10 Most Frequently Used Words by Album"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Pick Mac Miller's albums"),
            selectInput("album", label = "Select album below", 
                        choices = rownames(macaroni))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ## ggplot(aes(reorder(word, n), n, fill = reorder(word, -n))) +
            ##ggplot(macaroni, aes_string(input$album)) 
           ## geom_col(aes(word, n)) +
            ##geom_text(aes(label = reorder(word, n)), 
                     ## color = "white", size = 5, family = "Inconsolata",
                     ## hjust = 1.5) +
           ## scale_fill_brewer(palette = "Spectral") +
           ## coord_flip() + # switch x and y
            ##theme_classic() +
          ##  theme(text = element_text(family = "Inconsolata"),
                 ## axis.text.y = element_blank(),
                 ## axis.title = element_blank(),
                ##  legend.position = "none",
               ##   plot.title = element_text(family = "Royal Acid", size = 10, hjust = 0.5),
                ##  plot.subtitle = element_text(hjust = 0.5))
        barplot(macaroni[,input$album],
                main=input$album)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
