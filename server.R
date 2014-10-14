library(shiny)

source("powr.R")

shinyServer(function(input, output) {
  output$powr <- renderPlot({
    genPowr(input$leagueId, input$leagueYear, 5)
  })
})