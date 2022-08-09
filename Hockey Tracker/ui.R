library(png)
library(ggplot2)
library(ggpubr)
library(shiny)

# Table more 'centered'
ui <- pageWithSidebar(
  headerPanel("Hockey Tracker"),
  sidebarPanel(
    # Change radio button info to Team
    radioButtons("color", "Team", c("Pink", "Blue")),
    
    # Add numericInput field for player number
    numericInput("player", "Player Number", 50, min = 0, max = 99),
    
    # Change selectInput to Event
    selectInput("shape", "Event", c("Circle", "Triangle"))
    
  ),
  mainPanel(
      fluidRow(column(12,
                      h4("Click to place event location"),
                      actionButton("rem_point", "Remove Last Point"),
                      plotOutput("plot1", click = "plot_click",
                      # Set fixed values so plot does not 'stretch'
                      width = "600px", height = "300px"))
  ),
  fluidRow(column(12,
                  h4("Table of points on plot"),
                  downloadLink("download"),
                  tableOutput("table"))
  )
  )
)
