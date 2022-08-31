situation_list <- c("5v5", "5v4", "4v5", "6v5", "5v6", "4v4", "3v3",
                    "4v3", "3v4", "5v3", "3v5", "6v4", "4v6", "6v3" , "3v6")
# Table more 'centered'
ui <- navbarPage("Hockey Tracker",
                 tabPanel(
                   "Shot Attempts",
                      sidebarPanel(
                        #Timer
                        textOutput('timeleft'),
                        # Set Timer
                        numericInput('seconds','Seconds:',value=1200,min=0,max=99999,step=1),
                        actionButton('reset','Set Time'),
                        actionButton('start','Start'),
                        actionButton('stop','Stop'),
    
                        #Team Info
                        radioButtons("team", "Team", c("Home", "Away")),
    
                        # Add period radio buttons
                        radioButtons("period", "Period", c(1, 2, 3, "OT"), inline = TRUE),
    
                        # Time Input
                        #timeInput("time_input", "Time", value = strptime("00:00", "%M:%S")),
    
                        #numericInput field for player number
                        numericInput("player", "Player Number", 10, min = 0, max = 99),
        
                        # Change selectInput to Event
                        selectInput("event", "Event", c("Blocked", "Missed", "Saved", "Goal")),
    
                        # Situation input
                        selectInput("situation", "Situation", situation_list)
                        ), #This parenthesis closes the sidebarPanel
                  
                        mainPanel(
                          fluidRow(column(12,
                            h4("Click to place event location"),
                            actionButton("rem_point", "Remove Last Point"),
                            plotOutput(
                                  "plot1", click = "plot_click",
                                  # Set fixed values so plot does not 'stretch'
                                  width = "800px", height = "340px"
                                  )#This parenthesis closes the first column.
                                    )
                                  ),#This parenthesis closes the first fluidRow.
                          fluidRow(column(12,
                            h4("Table of points on plot"),
                            downloadLink("download"),
                            tableOutput("table")
                                  )#This parenthesis closes the first column of the second Row.
                                      #This might do weird things since I've changed it from a 
                                      #fluidpage to a navbar page. Remains to be seen.
                                  )#This parenthesis closes the second fluidRow.
                                )#Closes first tab's main panel
                        ),#Closes tabPanel 1
                  tabPanel(
                    "Goals",
                    #Genos and Apples
                    flowLayout(
                    #Team Info
                    radioButtons("team", "Team", c("Home", "Away")),
                    #numericInput field for player number
                    numericInput("playerG", "Goal Scorer", 1, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("playerA1", "Primary Assist", 2, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("playerA2", "Secondary Assist", 3, min = 0, max = 99),
                    ), # Closes flowLayout
                    
                    #Plus/Minus
                    #Plus
                    flowLayout(
                    #numericInput field for player number
                    numericInput("Plus1", "Plus", 1, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Plus2", "Plus", 2, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Plus3", "Plus", 3, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Plus4", "Plus", 4, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Plus5", "Plus", 5, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Plus5", "Plus", 6, min = 0, max = 99),
                    ), # Closes flowLayout 2
                    
                    #Minus
                    flowLayout(
                    #numericInput field for player number
                    numericInput("Minus1", "Minus", 1, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Minus2", "Minus", 2, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Minus3", "Minus", 3, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Minus4", "Minus", 4, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Minus5", "Minus", 5, min = 0, max = 99),
                    #numericInput field for player number
                    numericInput("Minus5", "Minus", 6, min = 0, max = 99),
                    ), # Closes flowLayout 3
                    
                    actionButton("submit", "The Submit Button")
                  
                  ),#Closes tabPanel 2
                  tabPanel(
                    "Penalties",
                    "Poop"
                  )#Closes tabPanel 3
            )#Closes UI
