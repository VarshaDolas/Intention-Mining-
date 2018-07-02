# Frontend/client of R shiny app

library(shiny)

if (interactive())

shinyUI(fluidPage(
  titlePanel("INTENTION MINING"),
  
  sidebarLayout(
    sidebarPanel(
      #textInput("searchTerm", label = "Enter twitter search keywords below", value = "Election 2019"), 
      textInput("searchTerm", "Enter twitter search keywords below", "Election 2019 "),
      #verbatimTextOutput("value"),
      selectInput('plot_opt', 'Plot options', c('emotion', 'polarity'), selectize=TRUE),
      
      submitButton("Update View")
    ),
    
    mainPanel(plotOutput("plot_emotion"),
              plotOutput("plot_emotion1"),
              plotOutput("plot_emotion2"))
  )
))
