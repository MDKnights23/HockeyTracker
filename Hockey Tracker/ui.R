library(png)
library(ggplot2)
library(ggpubr)
library(shiny)

# Table more 'centered'
ui <- pageWithSidebar(
  headerPanel("Hockey Tracker"),
  sidebarPanel(
    radioButtons("color", "Pick Color", c("Pink", "Blue")),
    selectInput("shape", "Team", c("Circle", "Triangle"))
  ),
  mainPanel(fluidRow(column(12,
                            h4("Click to place shot location"),
                            actionButton("rem_point", "Remove Last Point"),
                            plotOutput("plot1", click = "plot_click"))
  ),
  fluidRow(column(12,
                  h4("Table of points on plot"),
                  downloadLink("download"),
                  tableOutput("table"))
  )
  )
)
