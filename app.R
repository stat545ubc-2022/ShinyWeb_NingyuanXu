library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      # Modification: add an image to UI
      img(src='image.jpg', height="25%", width="25%"),
      # Modification: add tabsetPanel() with plot and table
      tabsetPanel(
        tabPanel("Plot", plotOutput("coolplot")),
        tabPanel("Table", tableOutput("results"))
      )
    )
  ),
  # Modification: add download link to UI
  downloadLink('downloadData', 'Download Dataset')
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
  
  # Modification: allow users to down the dataset 
  output$downloadData <- downloadHandler(
     filename = function() {
       paste('dataset', '.csv', sep='')
     },
     content = function(con) {
       write.csv(bcl, con)
     }
  )
}

shinyApp(ui = ui, server = server)