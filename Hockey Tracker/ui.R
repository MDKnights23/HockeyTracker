# Table more 'centered'
ui <- fluidPage(
  headerPanel("Hockey Tracker"),
  sidebarPanel(
    
    #Timer
    textOutput('timeleft'),
    # Set Timer
    numericInput('seconds','Seconds:',value=1200,min=0,max=99999,step=1),
    actionButton('reset','Set Time'),
    actionButton('start','Start'),
    actionButton('stop','Stop'),
    
    
    
    # Change radio button info to Team
    radioButtons("team", "Team", c("Home", "Away")),
    
    # Add period radio buttons
    radioButtons("period", "Period", c(1, 2, 3, "OT"), inline = TRUE),
    
    # Time Input
    #timeInput("time_input", "Time", value = strptime("00:00", "%M:%S")),
    
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
