library(png)
library(ggplot2)
library(ggpubr)
library(shiny)

# Table more 'centered'
ui <- fluidPage(
  headerPanel("Hockey Tracker"),
  sidebarPanel(
    # Change radio button info to Team
    radioButtons("team", "Team", c("Pink", "Blue")),
    
    # Add numericInput field for player number
    numericInput("player", "Player Number", 10, min = 0, max = 99),
    
    # Change selectInput to Event
    selectInput("event", "Event", c("Blocked", "Missed", "Saved", "Goal"))
    
  ),
  mainPanel(
    fluidRow(column(12,
                    h4("Click to place event location"),
                    actionButton("rem_point", "Remove Last Point"),
                    plotOutput(
                      
                      "plot1", click = "plot_click",
                      # Set fixed values so plot does not 'stretch'
                      width = "800px", height = "340px"
                      
                    )
    )
    ),
    fluidRow(column(12,
                    h4("Table of points on plot"),
                    downloadLink("download"),
                    tableOutput("table"))
    )
  )
)
