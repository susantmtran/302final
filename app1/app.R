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
library(wordcloud2)
library(memoise)
library(stringr)

macaroni <- readRDS("data/processed/macaroni.rds")  # word-by-word data
albums <- list("Circles" = "circles", 
              "Faces" = "faces", 
              "K.I.D.S" = "k.i.d.s", 
              "Macadelic" = "macadelic", 
              "Swimming" = "swimming",
              "The Divine Feminine" = "the divine feminine")

getTermMatrix <- memoise(function(album) {
    if(!(album %in% albums))
        stop("Unknown Album")
 
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"), "i", "the", "you", "a", "and", "to"))
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = TRUE)
})

ui <- fluidPage(

    # Application title
    titlePanel("Most Frequently Used Words by Album"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Pick Mac Miller's albums"),
            selectInput("album", label = "Select album below", 
                        choices = albums),
            sliderInput("freq",
                        "Minimum Frequency:",
                        min = 1,  max = 50, value = 15),
            sliderInput("max",
                        "Maximum Number of Words:",
                        min = 1,  max = 300,  value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    terms <- reactive({
        input$update
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                getTermMatrix(input$selection)
            })
            
        })
        
        
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    
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
        v <- terms()
        wordcloud_rep(names(v), v, scale = c(4, 0.5),
                      min.freq = input$freq, max.words = input$max)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
