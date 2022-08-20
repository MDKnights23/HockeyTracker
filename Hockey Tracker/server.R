library(ggpubr)
library(png)
library(shiny)
library(lubridate)
library(ggplot2)
library(shinyTime)

#Changing image
img<-png::readPNG("icehockeylayout.png")

server <- function(input, output, session){
  
  ## 1. set up reactive dataframe ##
  values <- reactiveValues()
  values$DT <- data.frame(period = factor(),
                          time = ms(character()),
                          #time = character(),
                          team = factor(),
                          number = numeric(),
                          event = factor(),
                          x = numeric(),
                          y = numeric())
  
  ## 2. Create a plot ##
  output$plot1 = renderPlot({
    ggplot(values$DT, aes(x = x, y = y)) +
      #This must be on top of the points in the code or the points drawn behind    
      background_image(img) + 
      geom_point(aes(color = team,
                     shape = event), size = 5) +
      lims(x = c(0, 200), y = c(-42.5, 42.5)) +
      
      # If we want to remove x and y labels from plot    
      labs(x = "",
           y = "") +
      
      theme(legend.position = "none", 
            # Option 1: Remove grey background, 
            # add more subtle grid lines (if desired)
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(color = "grey",
                                      linetype = 2)) +
      
      # Option 2: remove lines and color from background
      #panel.grid = element_blank(),
      #panel.background = element_blank(),
      #plot.margin = margin(t=5)) +
      
      # Option 3 Remove everything from plot but image
      # theme_void() +
      # theme(plot.margin = margin(5, 0, 0, 0))
      
      # We can only have 1 color scale set
    scale_color_manual(values = c("Blue", "Green"), 
                       drop = FALSE) +   
      # scale_color_discrete(drop = FALSE) +
      
      # include so that colors don't change as more color/shape chosen   
      scale_shape_discrete(drop = FALSE)
  })
  
  ## 3. add new row to reactive dataframe upon clicking plot ##
  observeEvent(input$plot_click, {
    # each input is a factor so levels are consistent for plotting characteristics
    add_row <- 
      data.frame(period = input$period,
                 time = toString(seconds_to_period(timer())),
                 # time = as.integer(seconds(timer()),
                 team = factor(input$team, levels = c("Home", "Away")),
                 number = input$player,
                 event = factor(input$event, 
                                levels = c("Blocked", "Missed", "Saved", "Goal")), #These are the valid DF entries, which MUST match the UI's list of Entries. Unmatched items in the Table go in as NA. Unmatched items in the UI are not drawn.
                 x = input$plot_click$x, 
                 y = input$plot_click$y
                 
      )
    # add row to the data.frame
    values$DT <- rbind(values$DT, add_row)
  })
  
  ## 4. remove row on actionButton click ##
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })
  
  ## 5. render a table of the growing dataframe ##
  output$table <- renderTable(
    values$DT,
    # tail(values$DT, 5),
    width = "75%"
  )
  
  ## 6. Download dataframe
  output$download <- downloadHandler(
    filename = paste0(Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(values$DT, file)
      }
  )
  
  ## 7. Timer Stuff
    # Initialize the timer, 10 seconds, not active.
    timer <- reactiveVal(0)
    active <- reactiveVal(FALSE)
    
    # Output the time left.
    output$timeleft <- renderText({
      paste("Time left: ", seconds_to_period(timer()))
    })
    
    # observer that invalidates every second. If timer is active, decrease by one.
    observe({
      invalidateLater(1000, session)
      isolate({
        if(active())
        {
          timer(timer()-1)
          if(timer()<1)
          {
            active(FALSE)
            showModal(modalDialog(
              title = "End of Period message",
              "Period completed!"
            ))
          }
        }
     })
    })
    
    # observers for actionbuttons
    observeEvent(input$start, {active(TRUE)})
    observeEvent(input$stop, {active(FALSE)})
    observeEvent(input$reset, 
                 if(active()){ #I know there's a better way to write this but I'm not looking up syntax right now
                   
                 } else {
                   timer(input$seconds) #basically, you can only set the timer if it's not already running
                 })
                 
}
