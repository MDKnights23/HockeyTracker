situation_list <- c("5v5", "5v4", "4v5", "6v5", "5v6", "4v4", "3v3",
                    "4v3", "3v4", "5v3", "3v5", "6v4", "4v6", "6v3" , "3v6")

penalty_list <- c("Boarding",
                  "Charging",
                  "Checking from Behind",
                  "Closing Hand on Puck",
                  "Cross-checking",
                  "Delay of game",
                  "Elbowing",
                  "Embellishment",
                  "Face-off violation",
                  "Fighting",
                  "Goaltender interference",
                  "High-sticking",
                  "Holding",
                  "Holding the stick",
                  "Hooking",
                  "Instigator",
                  "Interference",
                  "Kneeing",
                  "Roughing",
                  "Slashing",
                  "Spearing",
                  "Too Many Players",
                  "Tripping",
                  "Unsportsmanlike Conduct",
                  "Other"
)

# Table more 'centered'
ui <- navbarPage("Hockey Tracker",
                 tabPanel(
                   "Shot Attempts",
                   sidebarPanel(
                     #Timer
                     column(12, textOutput('timeleft'), style='padding:0%; text-align: center'),
                     # Set Timer
                     fluidRow(
                       column(6, numericInput('minutes', 'Minutes:', value = 20, min=0, max=99), style='padding:0%; text-align: right'),
                       column(6, numericInput('seconds','Seconds',value=0,min=0,max=59), style='padding:0%; text-align: left')
                     ),
                     
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
                     selectInput("situation1", "Situation", situation_list)
                   ), #This parenthesis closes the sidebarPanel
                   
                   mainPanel(
                     fluidRow(column(12,
                                     h4("Click to place event location"),
                                     actionButton("rem_point", "Remove Last Point"),
                                     # textOutput("plot_hoverinfo"),
                                     plotOutput(
                                       "plot1", click = "plot_click",
                                       hover = hoverOpts("plot_hover", delay = 300, delayType = "throttle"),
                                       # Set fixed values so plot does not 'stretch'
                                       width = "800px", height = "340px"
                                     )#This parenthesis closes the first column.
                     )
                     ),#This parenthesis closes the first fluidRow.
                     fluidRow(column(12, textOutput("plot_hoverinfo"))),
                     fluidRow(column(12,
                                     h4("Shots"),
                                     downloadLink("download1", "Download"),
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
                   fixedRow(
                     tags$head( ##This is some shit I copied from the internet to get rid of the up/down arrows. Trust it.
                       tags$style(HTML("
                          input[type=number] {
                            -moz-appearance:textfield;
                          }
                          input[type=number]::{
                            -moz-appearance:textfield;
                          }
                          input[type=number]::-webkit-outer-spin-button,
                          input[type=number]::-webkit-inner-spin-button {
                            -webkit-appearance: none;
                            margin: 0;
                          }
                      "))#Close style and html 
                     ),
                     sidebarPanel(
                       #copy of time from page 1
                       textOutput("timeleft2"),
                       #Team Info
                       radioButtons("team2", "Team", c("Home", "Away")),
                       # Period
                       radioButtons("period2", "Period", c(1, 2, 3, "OT"), inline = TRUE),
                       
                       selectInput("situation2", "Situation", situation_list)
                       
                     ),
                     mainPanel(
                       fixedRow(
                         #numericInput field for player number
                         column(2, numericInput("playerG", "Goal", NA, min = 0, max = 99, width = '80%')),
                         #numericInput field for player number
                         column(2, numericInput("playerA1", "Assist 1", NA, min = 0, max = 99,width = '80%')),
                         #numericInput field for player number
                         column(2, numericInput("playerA2", "Assist 2", NA, min = 0, max = 99,width = '80%')),
                       ), # Closes fixedRow
                       
                       #Plus/Minus
                       #Plus
                       fixedRow(
                         #numericInput field for player number
                         column(2, numericInput("Plus1", "Plus", NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Plus2", "Plus" , NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Plus3", "Plus", NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Plus4", "Plus", NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Plus5", "Plus", NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Plus6", "Plus", NA, min = 0, max = 99, width = '60%')),
                       ), # Closes fixedRow 2
                       
                       #Minus
                       fixedRow(
                         #numericInput field for player number
                         column(2, numericInput("Minus1", "Minus", NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Minus2", "Minus", NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Minus3", "Minus", NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Minus4", "Minus", NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Minus5", "Minus", NA, min = 0, max = 99, width = '60%')),
                         #numericInput field for player number
                         column(2, numericInput("Minus6", "Minus", NA, min = 0, max = 99, width = '60%')),
                       ), # Closes fixedRow 3
                       
                       actionButton("submit2", "Submit")
                       
                     )),
                   
                   fluidRow(column(12,
                                   h4("Goals"),
                                   downloadLink("download2", "Download"),
                                   tableOutput("table2")
                   ))
                   
                 ),#Closes tabPanel 2
                 
                 tabPanel(
                   "Penalties",
                   sidebarPanel(
                     #copy of time from page 1
                     textOutput("timeleft3"),
                     #Team Info
                     radioButtons("team3", "Team", c("Home", "Away")),
                     # Add period radio buttons
                     radioButtons("period3", "Period", c(1, 2, 3, "OT"), inline = TRUE),
                     
                     #numericInput field for player number
                     numericInput("player3", "Player Number", 10, min = 0, max = 99),
                     
                     # Change selectInput to Event
                     selectInput("event3", "Penalty", penalty_list),
                     
                     radioButtons("pen_minutes", "Minutes", c(2, 4, 5, 10), inline = TRUE),
                     
                     # Additional Penalty Details
                     textInput("additional_details","Additional Details", value=""),
                     
                     numericInput("pplay", "Goals", 0, min = 0, max = 10),
                     
                     actionButton("submit3", "Submit")
                     
                   ),
                   mainPanel(
                     fluidRow(column(12,
                                     h4("Penalties"),
                                     downloadLink("download3", "Download"),
                                     tableOutput("table3")
                     )) 
                   )
                   
                 ), # Closes tabPanel 3
                 
                 tabPanel(
                   "Summary",
                   
                   actionButton("go", "Take a screenshot"),
                   
                   fluidRow(column(12,
                                   h4("Shot Chart"),
                                   plotOutput(
                                     "shots",
                                     width = "800px", height = "340px"
                                   )
                   )),
                   
                   # Copy of goals and penalty tables
                   fixedRow(column(6,
                                   h4("Goals"),
                                   tableOutput("table4"),
                   ),
                            column(6,
                                   h4("Penalties"),
                                   tableOutput("table5")
                   )),
                   
                   # Aggregate tables
                   fixedRow(column(6,
                                   h4("Home"),
                                   tableOutput("table6"),
                                   ),
                            
                            column(6,
                                   h4("Away"),
                                   tableOutput("table7"),
                    ))
                 ), # Closes tabPanel 4
                 
                 tabPanel(
                     "Faceoffs",
                     sidebarPanel(

                         textOutput("timeleft5"),
                         
                         radioButtons("period5", "Period", c(1, 2, 3, "OT"), inline = TRUE),

                         selectInput("location", "Location", c("CEN", "AZL", "AZR", "ANL",
                                                               "ANR", "DNL", "DNR",
                                                               "DZL", "DZR")),

                         numericInput("foplayer1", "Player H", 0, min = 0, max = 99, width = "40%"),

                         numericInput("foplayer2", "Player A", 0, min = 0, max = 99, width = "40%"),

                         radioButtons("attempt", "Attempt", c("F", "B"), inline = TRUE),
                         radioButtons("attempt2", "Attempt", c("F", "B"), inline = TRUE),
                         
                         checkboxInput("tieup", "Tie-Up"),
                         
                         radioButtons("result", "Result", c("W", "L"), inline = TRUE),
                         
                         actionButton("submit5", "Submit")
                         
                     ),
                     
                     mainPanel(
                         fluidRow(column(12,
                                         h4("Faceoffs"),
                                         downloadLink("download5", "Download"),
                                         tableOutput("table8")
                         )) 
                     ),
                     
                     fluidRow(column(12,
                                     h4("Home Summary"),
                                     tableOutput("table9")
                                     )),
                     
                     fluidRow(column(12,
                                     h4("Away Summary"),
                                     tableOutput("table10")
                     ))
                 )
            
                 
                 
)#Closes UI
