server <- function(input, output, session){
  
  ## 1. set up reactive dataframe ##
  values <- reactiveValues()
  values$DT <- data.frame(x = numeric(),
                          y = numeric(),
                          color = factor(),
                          shape = factor())
  
  ## 2. Create a plot ##
  output$plot1 = renderPlot({
    ggplot(values$DT, aes(x = x, y = y)) +
      geom_point(aes(color = color,
                     shape = shape), size = 5) +
      lims(x = c(0, 100), y = c(0, 100)) +
      theme(legend.position = "bottom") +
      # include so that colors don't change as more color/shape chosen
      scale_color_discrete(drop = FALSE) +
      scale_shape_discrete(drop = FALSE)
  })
  
  ## 3. add new row to reactive dataframe upon clicking plot ##
  observeEvent(input$plot_click, {
    # each input is a factor so levels are consistent for plotting characteristics
    add_row <- data.frame(x = input$plot_click$x,
                          y = input$plot_click$y,
                          color = factor(input$color, levels = c("Pink", "Blue")),
                          shape = factor(input$shape, levels = c("Circle", "Triangle")))
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