library(ggpubr)
library(png)
library(shiny)
library(lubridate)
library(ggplot2)
library(shinyTime)
library(dplyr)
library(shinyscreenshot)

options(shiny.sanitize.errors = TRUE)

#Changing image
img<-png::readPNG("icehockeylayout2.png")

server <- function(input, output, session){
  
  ## 1. set up reactive dataframe for Shots##
  values <- reactiveValues()
  values$DT <- data.frame(period = factor(),
                          time = hms::hms(),
                          team = factor(),
                          situation = factor(),
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
      lims(x = c(0,200), y = c(-42.5, 42.5)) +
      
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
                 time = strftime(hms::hms(seconds_to_period(timer())), "%M:%S"),
                 team = factor(input$team, levels = c("Home", "Away")),
                 situation = factor(input$situation1),
                 number = input$player,
                 event = factor(input$event,
                                # These are the valid DF entries, which MUST match 
                                # the UI's list of Entries. Unmatched items in the 
                                # Table go in as NA. Unmatched items in the UI are not drawn.
                                levels = c("Blocked", "Missed", "Saved", "Goal")), 
                 x = input$plot_click$x,
                 y = input$plot_click$y
                 # x = case_when(
                 #   input$period %in% c(1,3) ~ input$plot_click$x,
                 #   TRUE ~ 200-input$plot_click$x
                 # ),
                 # y = case_when(
                 #   input$period %in% c(1,3) ~ input$plot_click$y,
                 #   TRUE ~ -1*input$plot_click$y
                 # )
                 
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
    align = 'l'
  )
  
  ## 6. Download dataframes
  output$download1 <- downloadHandler(
    filename = paste0("shots_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(values$DT, file)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = paste0("goals_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(goalValues$DT, file)
    }
  )
  
  output$download3 <- downloadHandler(
    filename = paste0("penalties_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(penaltyValues$DT, file)
    }
  )
  
  output$download5 <- downloadHandler(
    filename = paste0("faceoffs_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(faceoffValues$DT, file)
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
  output$timeleft2 <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  output$timeleft3 <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  output$timeleft5 <- renderText({
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
  observeEvent(input$start, 
               if(timer()>0){
                 active(TRUE)
               } else {
                 #do nothing 
               }
  )
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, 
               if(active()){ # I know there's a better way to write this but I'm 
                 # not looking up syntax right now
                 
               } else {
                 timer(60*input$minutes + input$seconds) # basically, you can only set the timer if 
                 # it's not already running
                 # basically, you can only set the timer if 
                 # it's not already running
               })
  
  #8. Goals page dataframe
  goalValues <- reactiveValues()
  goalValues$DT <- data.frame(Period = factor(),
                              Time = hms::hms(),
                              Team = factor(),
                              Situation = factor(),
                              G = numeric(),
                              A1 = numeric(),
                              A2 = numeric(),
                              plus1 = numeric(),
                              plus2 = numeric(),
                              plus3 = numeric(),
                              plus4 = numeric(),
                              plus5 = numeric(),
                              plus6 = numeric(),
                              minus1 = numeric(),
                              minus2 = numeric(),
                              minus3 = numeric(),
                              minus4 = numeric(),
                              minus5 = numeric(),
                              minus6 = numeric())
  
  #9 Goals page add to dataframe
  observeEvent(input$submit2, {
    add_row2 <- 
      data.frame( 
        Period = input$period2,
        Time = strftime(hms::hms(seconds_to_period(timer())), "%M:%S"),
        Team = factor(input$team2, levels = c("Home", "Away")),
        Situation = input$situation2,
        G = input$playerG,
        A1 = input$playerA1,
        A2 = input$playerA2,
        plus1 = input$Plus1,
        plus2 = input$Plus2,
        plus3 = input$Plus3,
        plus4 = input$Plus4,
        plus5 = input$Plus5,
        plus6 = input$Plus6,
        minus1 = input$Minus1,
        minus2 = input$Minus2,
        minus3 = input$Minus3,
        minus4 = input$Minus4,
        minus5 = input$Minus5,
        minus6 = input$Minus6
      )
    # add row to the data.frame
    goalValues$DT <- rbind(goalValues$DT, add_row2)
  })
  
  
  # 10 Render Goals table
  output$table2 <- renderTable(goalValues$DT,
                               align = "l")
  
  # 11 Penalties page data frame
  penaltyValues <- reactiveValues()
  penaltyValues$DT <- data.frame(Period = numeric(),
                                 Time = hms::hms(),
                                 Team = factor(),
                                 Player = numeric(),
                                 Penalty = factor(),
                                 Minutes = numeric(),
                                 Details = character(),
                                 Goals = numeric())
  
  # 12 Goals page add to dataframe
  observeEvent(input$submit3, {
    add_row3 <- 
      data.frame(
        Period = input$period3,
        Time = strftime(hms::hms(seconds_to_period(timer())), "%M:%S"),
        Team = input$team3,
        Player = input$player3,
        Penalty = input$event3,
        Minutes = input$pen_minutes,
        Details = input$additional_details,
        Goals = input$pplay
      )
    # add row to the data.frame
    penaltyValues$DT <- rbind(penaltyValues$DT, add_row3)
  })
  
  # 13 Render Penalties table
  output$table3 <- renderTable(penaltyValues$DT,
                               align = "l")
  
  # Summary tab
  output$shots = renderPlot({
    ggplot(values$DT, aes(
      
      x = x, 
      y = y
      
    )) +
      #This must be on top of the points in the code or the points drawn behind    
      background_image(img) + 
      geom_point(aes(color = team,
                     shape = event), size = 5) +
      lims(x = c(0,200), y = c(-42.5, 42.5)) +
      
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
  
  output$table4 <- renderTable(goalValues$DT %>% 
                                 select(Period:A2))
  
  output$table5 <- renderTable(penaltyValues$DT %>% 
                                 select(-Details))
  
  # Hover over plot
  output$plot_hoverinfo <- renderPrint({
    if (isTRUE(as.numeric(input$plot_hover$x) >= 0) &
        isTRUE(as.numeric(input$plot_hover$x) <= 200) &
        isTRUE(as.numeric(input$plot_hover$y) >= -42.5)  &
        isTRUE(as.numeric(input$plot_hover$y) <= 42.5)) {
      
      cat("X = ", round(as.numeric(input$plot_hover$x), 2), ", ",
          "Y = ", round(as.numeric(input$plot_hover$y), 2),
          sep = "")
      
    }
    
    else {
      cat("Values not within range")
    }
  })
  
  # Screenshot
  observeEvent(input$go, {
    screenshot()
  })
  
  
  
  output$table6 <- renderTable(values$DT %>% 
                                 group_by(team) %>% 
                                 summarize(Goals = sum(event == "Goal"),
                                           SoG = sum(event %in% c("Goal", "Saved")),
                                           saved_placeholder = sum(event == "Saved") /
                                             sum(event %in% c("Saved", "Goal")),
                                           ShotAttempts = sum(event %in% c("Goal", "Saved",
                                                                           "Missed", "Blocked")),
                                           "Corsi%" = sum(event %in% c("Goal", "Saved",
                                                                       "Missed", "Blocked")) / nrow(values$DT),
                                           "Fenwick%" = sum(event %in% c("Goal", "Saved",
                                                                         "Missed")) / nrow(values$DT %>% filter(
                                                                           event != "Blocked"
                                                                         ))
                                 ) %>%
                                 mutate("Save%" = case_when(
                                   team == "Away" ~ saved_placeholder[1],
                                   TRUE ~ saved_placeholder[2]
                                 )) %>% 
                                 select(team:SoG, "Save%", ShotAttempts:"Fenwick%") %>% 
                                 filter(team == "Home") %>%
                                 select(- team)
  )
  
  output$table7 <- renderTable(values$DT %>% 
                                 group_by(team) %>% 
                                 summarize(Goals = sum(event == "Goal"),
                                           SoG = sum(event %in% c("Goal", "Saved")),
                                           # Will use this column to "swap" the values"
                                           saved_placeholder = sum(event == "Saved") /
                                             sum(event %in% c("Saved", "Goal")),
                                           ShotAttempts = sum(event %in% c("Goal", "Saved",
                                                                           "Missed", "Blocked")),
                                           "Corsi%" = sum(event %in% c("Goal", "Saved",
                                                                       "Missed", "Blocked")) / nrow(values$DT),
                                           "Fenwick%" = sum(event %in% c("Goal", "Saved",
                                                                         "Missed")) / nrow(values$DT %>% filter(
                                                                           event != "Blocked"
                                                                         ))
                                 ) %>%
                                 mutate("Save%" = case_when(
                                   team == "Away" ~ saved_placeholder[1],
                                   TRUE ~ saved_placeholder[2]
                                 )) %>% 
                                 select(team:SoG, "Save%", ShotAttempts:"Fenwick%") %>% 
                                 filter(team == "Away") %>%
                                 select(- team)
  )
  
  # Faceoff page data frame
  faceoffValues <- reactiveValues()
  faceoffValues$DT <- data.frame(Period = numeric(),
                                 Time = hms::hms(),
                                 Location = factor(),
                                 PlayerH = numeric(),
                                 PlayerA = factor(),
                                 AttemptH = numeric(),
                                 AttemptA = numeric(),
                                 TieUp = factor(),
                                 Result = factor())
  
  observeEvent(input$submit5, {
    add_row5 <- 
      data.frame(
        Period = input$period5,
        Time = strftime(hms::hms(seconds_to_period(timer())), "%M:%S"),
        Location = input$location,
        PlayerH = input$foplayer1,
        AttemptH = input$attempt,
        Result = input$result,
        
        PlayerA = input$foplayer2,
        AttemptA = input$attempt2,
        
        
        TieUp = ifelse(input$tieup == TRUE, "Y", "N")
      
      )
    
    # add row to the data.frame
    faceoffValues$DT <- rbind(faceoffValues$DT, add_row5)
    
    })
    
    # Needs to be outside of the observeEvent for adding row to faceoff DF
    observeEvent(input$rem_point23, {
      rem_row <- faceoffValues$DT[-nrow(faceoffValues$DT), ]
      faceoffValues$DT <- rem_row
    })
  
  
  # 13 Render Faceoff table
  output$table8 <- renderTable(faceoffValues$DT,
                               align = "l")
  
  
  
  # 14 Render Faceoff summary Home
  output$table9 <- renderTable(faceoffValues$DT %>%
                                 group_by(PlayerH) %>% 
                                 summarize(W = sum(Result == "W"),
                                           L = n() - sum(Result == "W"),
                                           `W+L` = W + L,
                                           `Faceoff%` = W / (`W+L`),
                                           `TieUp%` = sum(TieUp == "Y") / `W+L`) %>% 
                                 select(Player = PlayerH,
                                        W,
                                        L,
                                        `Faceoff%`,
                                        `TieUp%`))
  
  # Render Faceoff summary Away
  output$table10 <- renderTable(faceoffValues$DT %>%
                                  group_by(PlayerA) %>% 
                                  summarize(W = sum(Result == "L"),
                                            L = n() - sum(Result == "L"),
                                            `W+L` = W + L,
                                            `Faceoff%` = W / (`W+L`),
                                            `TieUp%` = sum(TieUp == "Y") / `W+L`) %>% 
                                  select(Player = PlayerA,
                                         W,
                                         L,
                                         `Faceoff%`,
                                         `TieUp%`))
  
}
