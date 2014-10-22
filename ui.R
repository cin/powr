library(shiny)

shinyUI(fluidPage(
  titlePanel("Power Rankings"),
  sidebarLayout(
    sidebarPanel(
      textInput("leagueId", "League ID", "709331"),
      numericInput("leagueYear", "Year", 2014, 2000, 2014, 1),
      numericInput("leagueWeek", "Week", 6, 1, 17, 1),
      numericInput("regularSeasonWeeks", "Regular Season Weeks", 14, 12, 17, 1),
      checkboxInput("includePlayoffs", "Include Playoffs", FALSE),
      actionButton("exportImage", "Export PNG")
    ),
    mainPanel(
      plotOutput("powr")
    )
  )
))