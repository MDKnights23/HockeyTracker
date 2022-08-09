library(ggpubr)
library(png)

#Changing image
img<-png::readPNG("icehockeylayout.png")

server <- function(input, output, session){
  
  ## 1. set up reactive dataframe ##
  values <- reactiveValues()
  values$DT <- data.frame(
    
    team = factor(),
    number = numeric(),
    event = factor(),
    x = numeric(),
    y = numeric()
    
    
  )
  
  ## 2. Create a plot ##
  output$plot1 = renderPlot({
    ggplot(values$DT, aes(x = x, y = y)) +
      #This must be on top of the points in the code or the points drawn behind    
      background_image(img) + 
      geom_point(
                 aes(color = team,
                     shape = event), 
                 
                 size = 5, ) +
      lims(x = c(0, 200), y = c(-44.5, 44.5)) +
      theme(legend.position = "none") +
      
      # include so that colors don't change as more color/shape chosen
      scale_color_discrete(drop = FALSE) +
      scale_shape_discrete(drop = FALSE)
  })
  
  ## 3. add new row to reactive dataframe upon clicking plot ##
  observeEvent(input$plot_click, {
    # each input is a factor so levels are consistent for plotting characteristics
    add_row <- 
      data.frame(
        
        team = factor(input$team, levels = c("Pink", "Blue")),
        number = input$player,
        event = factor(input$event, levels = c("Blocked", "Missed", "Saved", "Goal")), #These are the valid DF entries, which MUST match the UI's list of Entries. Unmatched items in the Table go in as NA. Unmatched items in the UI are not drawn.
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
}
