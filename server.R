library(ggmap)
library(shiny)

source("powr.R")

shinyServer(function(input, output) {
  output$powr <- renderPlot({
    genPowr(input$leagueId, input$leagueYear, input$leagueWeek, input$avgScoreWeight/10, intput$hiloWeight/10, input$wpWeight/10)
  })
  
  observe({
    input$exportImage
    isolate({
      if (input$exportImage[1] == 0)
        return;
      
      fn <- sprintf("powr_%s_%sw%s.png", input$leagueId, input$leagueYear, input$leagueWeek)
      print(sprintf("exporting image - %s", fn))
      png(fn, 1280, 826)
      genPowr(input$leagueId, input$leagueYear, input$leagueWeek)
      dev.off()
    })
  })
})