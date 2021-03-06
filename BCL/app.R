library(shiny)
library(tidyverse) 
  
bcl <- read.csv("bcl-data.csv")

options(shiny.autoreload = TRUE)
#max_price <- max(bcl$Price, na.rm = TRUE)

price_filter <- function(lwb,upb) {
  bcl %>% filter(lwb < Price & Price < upb) %>% nrow()
}

ui <- fluidPage(
   titlePanel("BCL app"),
   "This is an app that can help you with exploring the alcohol content at BC Liquor stores between two price points.", 
   tags$br(), 
   tags$br(), 
   sidebarLayout(
     sidebarPanel(
       sliderInput("my_slider", "Select a price range", 
                   min = 0, max = 200, value = c(10,30)),
       radioButtons(
         "my_radio", "Select beverage type.", 
         choices = unique(bcl$Type)
       ), 
       img(src='images.jpeg', align = "left")
     ), 
     mainPanel(
       verbatimTextOutput("result"), 
       colourInput("col", "Choose colour", "#FFC0CB"),
       plotOutput("my_plot"), 
       tableOutput("my_table")
     )
   )
)

server <- function(input, output) {
  filtered <- reactive({
    #print(input$my_slider)
    #print(input$my_radio)
    bcl %>%
    filter(Price < input$my_slider[2],
           Price > input$my_slider[1],
           Type == input$my_radio)
  })
  
  output$my_plot <- renderPlot(
    filtered() %>%
    ggplot(aes(Alcohol_Content)) +
      geom_histogram(fill=input$col)
  )
  output$result <- renderPrint(
      paste("we have found",
            price_filter(input$my_slider[1],input$my_slider[2]) %>% as.character(), 
      "results for you"))
  
  output$my_table <- renderTable(
    filtered()
    
  )
}

library(colourpicker)


shinyApp(ui = ui, server = server)

