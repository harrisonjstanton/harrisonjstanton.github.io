#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(kableExtra)

load("2024NFL.RData")

ui <- fluidPage(
  
  # Application title
  titlePanel(paste("Fantasy Running Backs Analysis as of Week", paste0(week, ","), season, sep = " ")),
  

  
  tabsetPanel(
    tabPanel("Introduction",
             #             fluidRow(
             #               column(width = 10, offset = 1, 
             #                      tags$style(".custom-padding { padding-top: 20px; }"),  # Define a custom CSS class for padding-top
             #                      tags$div(class = "custom-padding"),
             #                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Center and style
             #                             "Trivial Rush Attempt Percentage (TRAP) is a stat invented by Ben Gretch that is used to help managers target running backs in fantasy football. The stat tells a manager what percentage of touches a running back had that were not high value touches, meaning not carries inside the 10 yard line or receptions. 
             #                             There are 2 effective ways in which this stat is used: first, TRAP can help identify which running back in a split backfield is worth having or which handcuff running back to roster. 
             #                             The second way is that it can help you differentiate between backs who get the same amount of touches per week."
             #                      ),
             #                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
             #                             "The purpose of this app is to help people to interact with this data visually, and more effectively target which running backs they should roster."
             #                      ),
             #                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
             #                             "The most effective way, in my opinion, to use the TRAP Bar Chart tab is to filter by team. This helps you to identify which running backs in a split backfield, or which handcuff, to roster.
             #                              For example, if you filter to the Steelers, you will see that Jaylen Warren and Najee Harris were the two primary backs for the Steelers this year.
             #                              And, while Najee Harris received more touches per game than Jaylen Warren did, he received 1 fewer high value touch per game.
             #                              This is reflected in their total points per game outputs for the season."
             #                      ),
             #                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
             #                             "The most effective way to use the Running Back Scatterplots tab is to look at 3 graphs.
             #                              The first is HVT per game vs. Touches per game. This graph is a visual demonstration of why not all touches are built the same.
             #                              If you select a range of running backs who all have similar touches per game but different high value touches per game using the brush tool, and then view
             #                              their stats in the table below, you will see that the running backs who consistently get more high value touches per game are the ones who score more points.
             #                              This would lead you to value the guys who are above the line more highly than the guys who are below the line, as long as they are getting the same touches per game.
             # "
             #                      ),
             #                      tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",  # Style the second paragraph
             #                             "The two other graphs to look at are PPG vs. HVT per game and PPG vs. Touches per game. The most important piece of information from these two graphs actually
             #                              comes from the slope of the best fit line. As you will see, the slope of the best fit line for the first graph is 3.19, and the slope for the second is 0.83.
             #                              This is a great way to see why high value touches are important, because they lead to roughly 3 times more fantasy points than touches per game. Additionally,
             #                              another important takeaway is that touches per game is not unimportant. Volume can still drive scoring."
             #                      ),
             #                      tags$div(class = "custom-padding"),
             #               )),
             fluidRow(
               column(width = 10, offset = 1,
                      accordion(
                        id = "accordion",
                        
                        accordionItem(
                          title = tags$span(style = "font-size: 24px;", "TRAP Bar Chart Explanation"),
                          color = "primary",
                          collapsed = TRUE,
                          tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",
                                 "Trivial Rush Attempt Percentage (TRAP) is a stat invented by Ben Gretch that is used to help managers target running backs in fantasy football. The stat tells a manager what percentage of touches a running back had that were not high value touches, meaning not carries inside the 10 yard line or receptions. 
                              There are 2 effective ways in which this stat is used: first, TRAP can help identify which running back in a split backfield is worth having or which handcuff running back to roster. 
                              The second way is that it can help you differentiate between backs who get the same amount of touches per week."),
                          
                          tags$br(),
                          
                          tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",
                                 "The most effective way, in my opinion, to use the TRAP Bar Chart tab is to filter by team. This helps you to identify which running backs in a split backfield, or which handcuff, to roster.
                        Since receptions and touches near the goal line lead to more fantasy points on average than a normal touch, guys who receive a lot of those High Value Touches are guys that we should be targeting.
                        A great example of this is the Steelers backfield from 2023, which saw Najee Harris receive much more volume than Jaylen Warren. However, Warren received more HVT, which was reflected in their PPG 
                        outputs."
                          )
                        ),
                        
                        accordionItem(
                          title = tags$span(style = "font-size: 24px;", "Running Back Scatterplots Explanation"),
                          color = "primary",
                          collapsed = TRUE,
                          tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",
                                 "The most effective way to use the Running Back Scatterplots tab is to look at 3 graphs.
                            The first is HVT per game vs. Touches per game. This graph is a visual demonstration of why not all touches are built the same.
                            If you select a range of running backs who all have similar touches per game but different high value touches per game using the brush tool, and then view
                            their stats in the table below, you will see that the running backs who consistently get more high value touches per game are the ones who score more points.
                            This would lead you to value the guys who are above the line more highly than the guys who are below the line, as long as they are getting the same touches per game."),
                          
                          tags$br(),
                          
                          tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",
                                 "The two other graphs to look at are PPG vs. HVT per game and PPG vs. Touches per game. The most important piece of information from these two graphs actually
                            comes from the slope of the best fit line. As you will see, the slope of the best fit line for the first graph is ~3.2, and the slope for the second is ~0.8.
                            This is a great way to see why high value touches are important, because they lead to roughly 4 times more fantasy points than touches per game. Additionally,
                            another important takeaway is that touches per game is not unimportant. Volume can still drive scoring."
                          )
                        ),
                        
                        accordionItem(
                          title = tags$span(style = "font-size: 24px;", "Weighted Opportunities Explanation"),
                          color = "primary",
                          collapsed = TRUE,
                          tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",
                                 "There are two effective ways to use this tab. First, being able to sort by week ranges is a super valuable tool. For instance, say the starting running back 
                                in a backfield got injured three weeks ago. Since then, the backfield has been split. This tool allows you to filter to the three weeks without the starter, and
                                gives you an easy way to understand how valuable a running backs workload was through the WO/G number. This can help with start/sit decisions, or even deciding 
                                whether it is worth rostering the backup running back or whether he is droppable."
                          ),
                          tags$br(),
                          tags$p(style = "text-align: left; font-size: 16px; line-height: 1.5;",
                                 "The second way that this tab can be useful is by looking at the column labeled “diff”. This is the difference between someone’s PPG and WO/G. What this is showing 
                                us is how efficient a running back is at converting his workload into points. There are two reasons why this can be helpful. First, efficiency typically leads to a 
                                higher workload. Ryan Heath has done some great studies that show that people who have a high difference in their PPG and WO/G in a year typically receive more volume
                                the next year, which is very important to us as fantasy managers. The second reason this number is important is because players often regress to their mean. The difference
                                between PPG and WO/G can fluctuate year to year. For example, if someone’s diff for their career has been 1 and the next year it shoots up to 4, it is likely that the next
                                season their diff is going to come back down to their career mean around 1. There is obviously more to football than just regressing to a mean (ex. scheme change, offensive
                                line improvement, an injury heals), but this can be helpful when making assessments of players year to year and predicting bounce back candidates or players to avoid."
                          )
                        )
                      )
               )
             )
    ),
    
    tabPanel("TRAP Bar Chart",
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("user_choice", "Choose between Tier and Team", choices = c("Tier", "Team"), selected = "Tier"),
                   uiOutput("user_rank_input"),
                   checkboxInput("data", "View RBs season stats"),
                 ),
                 
                 
                 mainPanel(
                   plotOutput("histogram"),
                 )
               ),
             ),
             fluidRow(
               column(8, style = "margin-left: calc(8.33% * 0.75);",
                      tableOutput("table")
               ),
             ),
    ),
    
    #need to get text to work for this as well
    tabPanel("Running Back Scatterplots",
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("x", "Choose x-axis", choices = column_names, selected = "Touches_per_game"),
                   selectInput("y", "Choose y-axis", choices = column_names, selected = "HVT_per_game"),
                   checkboxInput("user_equation", "Show equation of line"),
                   checkboxInput("see_stats", "Show season stats")
                 ),
                 
                 
                 mainPanel(
                   plotOutput("scatter", brush = "plot_brush"),
                   textOutput("equation"),
                   textOutput("rsquared")
                 ))
             ), 
             fluidRow(
               tableOutput("data")
             )
    ),
    #newest tab I plan on adding
    tabPanel("Weighted Opportunities",
             fluidRow(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("user_choice_2", "Choose between Tier and Team", choices = c("Tier", "Team"), selected = "Tier"),
                   uiOutput("user_rank_input_2"),
                   fluidRow(
                     div(style = "text-align: center; margin-left: -20px;",
                         div(style = "display: inline-block; vertical-align: middle;",
                             strong("View from")
                         ),
                         div(style = "display: inline-block; width: 120px; margin-left: 5px;",
                             selectInput("start_week", " ", choices = week_list, selected = 1)
                             #selectInput("start_week", " ", choices = c(1, 2), selected = 1)
                         ),
                         div(style = "display: inline-block; vertical-align: middle; margin-left: 5px;",
                             strong("to")
                         ),
                         div(style = "display: inline-block; width: 120px; margin-left: 5px;",
                             selectInput("end_week", " ", choices = week_list, selected = max(week_list))
                             #selectInput("end_week", " ", choices = c(1, 2), selected = 1)
                         )
                     )
                   )
                 ),
                 
                 
                 mainPanel(
                   tableOutput("wo_table"),
                 )
               ),
             ),
    ),
    tabPanel("Glossary", 
             fluidRow(4, offset = 4,
                      #might want to center this... that's the last thing I want to change on this tab
                      uiOutput("glossaryTable")
             ),
    ),
  ),
)



server <- function(input, output) {
  
  
  
  #glossary
  styled_table_content <- reactive({
    data <- data.frame(
      Term = c("PPG", "xPPG", "pos_rank", "ATT", "Rush_yds", "Y/A", "20+", 
               "Rush_TD", "REC", "TGT", "Rec_yds", "Y/R", "Rec_TD", "FL", "HVT", 
               "Total_touches", "TRAP", "Weighted Opportunities", "WO/G"),
      Definition = c("Fantasy points per game", "Expected fantasy points pergame",
                     "Position Rank: sorted by total points, sorted by position", 
                     "Rushing attempts on the season", "Rushing yards on the season", 
                     "Yards per attempt",
                     "Number of rushes over 20 yards", "Rushing Touchdowns", "Receptions", 
                     "Targets", "Receiving yards", "Yards per reception", 
                     "Receiving Touchdowns", "Fumbles lost", 
                     "High value touches: rushes within the 10 yard line + receptions", 
                     "Receptions + carries", 
                     "Trivial Rush Attempt Percentage: (Total touches - HVT) / Total touches",
                     "A weighted measure for an RB's workload on a ppg scale",
                     "Weighted Opportunities per game")
    )
    
    # Create the HTML table with alternating row colors
    styled_table <- kable(data, "html") %>%
      kable_styling("striped", full_width = FALSE) %>%
      column_spec(1, bold = TRUE)  # Make the first column bold
    return(styled_table)
  })
  
  output$glossaryTable <- renderUI({
    HTML(styled_table_content())
  })
  
  #warning when both inputs are same on second graph
  observe({
    if(input$x == input$y){
      showNotification("You have selected the same variable for x-axis and y-axis", type = "warning")
    }
  })
  
  #conditional drop down tab on first tab
  output$user_rank_input <- renderUI({
    if(input$user_choice == "Team"){
      selectInput("user_team", "Choose Team", choices = teams, selected = "ARI")
    }
    else if(input$user_choice == "Tier"){
      selectInput("user_rank", "Choose RB Tier", choices = c("RB1", "RB2", "RB3"), selected = "RB1")
    }
    
  })
  
  #equation of scatterplot
  output$equation <- renderText({
    req(input$user_equation)
    lm_model <- lm(scatterplot_df[[input$y]] ~ scatterplot_df[[input$x]])
    
    slope <- coef(lm_model)[2]
    intercept <- coef(lm_model)[1]
    
    equation_string <- paste0("y = ", round(slope, 2), "x + ", round(intercept, 2))
    return(equation_string)
  })
  
  output$rsquared <- renderText({
    if (input$user_equation) {
      #might need req() here
      model <- lm(scatterplot_df[[input$y]] ~ scatterplot_df[[input$x]])
      paste("R-squared is:", round(summary(model)$r.squared, 3))
    }
  })
  
  

  output$histogram <- renderPlot({
    # print("Tab 1")
    # print(input$user_choice)
    # print(input$user_rank)
    # print(input$user_team)
    if (input$user_choice == "Tier") {
      req(!is.null(input$user_rank))
      names <- trap_df %>%
        filter(Rank == input$user_rank) %>%
        arrange(desc(ppg)) %>%
        distinct(gsis_id, .keep_all = TRUE) %>%
        pull(full_name)
      
      names <- rev(names)
      

      trap_df %>%
        filter(Rank == input$user_rank) %>%
        ggplot(aes(x = full_name, y = percentage, fill = Touch_type)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("springgreen4", "brown2"), name = "Touch Type",
                          labels = c("High Value Touches", "Trivial Rushes")) +
        labs(x = "", 
             y = "Percentage",
             caption = "Data from: nflfastR") +
        theme(
          plot.title = element_text(size = 24),
          axis.title = element_text(size = 18), 
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14),
          plot.caption = element_text(size = 14, face = "italic")
        ) +
        scale_x_discrete(limits = names) +
        ggtitle(paste0(input$user_rank, "'s HVT for 2024")) +
        geom_text(aes(y = label_y, label = label), color = 'white', size = 5) +
        scale_y_continuous(labels = scales::percent)
    }
    else if(input$user_choice == "Team"){
    req(!is.null(input$user_team))
      names <- trap_df %>%
        filter(team == input$user_team) %>%
        arrange(desc(ppg)) %>%
        distinct(gsis_id, .keep_all = TRUE) %>%
        pull(full_name)
      
      names <- rev(names)
      
      #table for teams of first tab
      req(!is.null(input$user_team))
      trap_df %>%
        filter(team == input$user_team) %>%
        ggplot(aes(x = full_name, y = percentage, fill = Touch_type)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("springgreen4", "brown2"), name = "Touch Type",
                          labels = c("High Value Touches", "Trivial Rushes")) +
        labs(x = "", 
             y = "Percentage",
             caption = "Data from: nflfastR") +
        theme(
          plot.title = element_text(size = 24),
          axis.title = element_text(size = 18), 
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14),
          plot.caption = element_text(size = 14, face = "italic")
        ) +
        scale_x_discrete(limits = names) +
        ggtitle(paste0(input$user_team, "'s HVT for 2024")) +
        geom_text(aes(y = label_y, label = label), color = 'white', size = 5) +
        scale_y_continuous(labels = scales::percent)
    }
  })
  
  #data for first tab
  #figure out ordering for this tomorrow
  output$table <- renderTable({
    req(input$data)
    if(input$user_choice == "Tier"){
      req(!is.null(input$user_rank))
      names <- trap_df %>%
        filter(Rank == input$user_rank) %>%
        arrange(desc(ppg)) %>%
        distinct(gsis_id, .keep_all = TRUE) %>%
        pull(full_name)
      
      
      stats_df %>%
        filter(Rank == input$user_rank) %>%
        filter(!gsis_id == "NA") %>%
        filter(!is.na(full_name)) %>%
        #ungroup() %>%
        select(full_name, team, ATT, Rush_yards, Rush_TD, "Y/A", REC, TGT, Rec_yards, Rec_TD, "Y/R", Rank, ppg, TRAP, HVT_per_game, Touches_per_game) %>%
        mutate(full_name = factor(full_name, levels = names)) %>%
        arrange(full_name)
    }
    else if(input$user_choice == "Team"){
      req(!is.null(input$user_team))
      names <- trap_df %>%
        filter(team == input$user_team) %>%
        arrange(desc(ppg)) %>%
        distinct(gsis_id, .keep_all = TRUE) %>%
        pull(full_name)
      
      
      stats_df %>%
        filter(team == input$user_team) %>%
        filter(!gsis_id == "NA") %>%
        filter(!is.na(full_name)) %>%
        #ungroup() %>%
        select(full_name, team, ATT, Rush_yards, Rush_TD, "Y/A", REC, TGT, Rec_yards, Rec_TD, "Y/R", Rank, ppg, TRAP, HVT_per_game, Touches_per_game) %>%
        mutate(full_name = factor(full_name, levels = names)) %>%
        arrange(full_name)
    }
  })
  
  #data for second tab
  output$data <- renderTable({
    req(input$see_stats)
    brushedPoints(stats_df %>% select(full_name, team, games, ppg, xppg, Rank, ATT, Rush_yards, "Y/A", Rush_TD, REC, TGT, Rec_yards, "Y/R", Rec_TD, input$x, input$y), input$plot_brush) %>%
      arrange(desc(ppg))
    
  })
  
  #WO dropdown
  output$user_rank_input_2 <- renderUI({
    if(input$user_choice_2 == "Team"){
      selectInput("user_team_2", "Choose Team", choices = teams, selected = "ARI")
    }
    else if(input$user_choice_2 == "Tier"){
      selectInput("user_rank_2", "Choose RB Tier", choices = c("RB1", "RB2", "RB3"), selected = "RB1")
    }
    
  })
  
  
  
  
  #table for WO tab
  output$wo_table <- renderTable({
    if(input$user_choice_2 == "Team"){
      req(!is.null(input$user_team_2))
      # print("weighted_opps team")
      # print(input$user_team_2)
      hvt_tt_df %>%
        filter(week >= as.integer(input$start_week) & week <= as.integer(input$end_week)) %>%
        # filter(team == input$user_team_2) %>%
        filter(if (!is.null(input$user_team_2)) team == input$user_team_2 else TRUE ) %>%
        group_by(gsis_id) %>%
        summarize(
          total_fantasy_points = sum(total_fantasy_points, na.rm = TRUE),
          weighted_opps = sum(weighted_opps, na.rm = TRUE),
          games = n(),
          team = first(team),
          full_name = first(full_name),
        ) %>%
        mutate(
          ppg = total_fantasy_points / games,
          "WO/G" = weighted_opps / games,
          diff = ppg - `WO/G`
        ) %>%
        arrange(desc(ppg)) %>%
        select(full_name, team, games, ppg, "WO/G", diff)
    }
    else if(input$user_choice_2 == "Tier"){
      req(!is.null(input$user_rank_2))
      # print("WO tier")
      # print(input$user_rank_2)
      hvt_tt_df %>%
        filter(week >= as.integer(input$start_week) & week <= as.integer(input$end_week)) %>%
        # filter(Rank == input$user_rank_2) %>%
        # filter(Rank == values$user_rank_2) %>%
        filter(if (!is.null(input$user_rank_2)) Rank == input$user_rank_2 else TRUE ) %>%
        group_by(gsis_id) %>%
        summarize(
          total_fantasy_points = sum(total_fantasy_points, na.rm = TRUE),
          weighted_opps = sum(weighted_opps, na.rm = TRUE),
          games = n(),
          team = first(team),
          full_name = first(full_name),
        ) %>%
        mutate(
          ppg = total_fantasy_points / games,
          "WO/G" = weighted_opps / games,
          diff = ppg - `WO/G`
        ) %>%
        arrange(desc(ppg)) %>%
        select(full_name, team, games, ppg, "WO/G", diff)
    }
  })
  
  #warning when start_week > end_week
  # observe({
  #   if(input$start_week > input$end_week){
  #     showNotification("First input needs to be less than second input.", type = "warning")
  #   }
  # })
  
  
  #scatterplot for second tab
  #NEED TO MAKE SURE TRAP_DF HAS ALL THE STATS I'M LOOKING FOR. OR JUST USE DIFFERENT DF THAT DOES, I KNOW I ALREADY MADE ONE
  output$scatter <- renderPlot({
    req(!is.null(input$x))
    req(!is.null(input$y))
    # print('scatterplot')
    # print(input$x)
    # print(input$y)
    scatterplot_df %>%
      ggplot(aes_string(x=input$x, y=input$y)) +
      geom_point() +
      labs(x = input$x,
           y = input$y, 
           caption = "Data from: nflfastR") +
      ggtitle(paste0(input$y, " vs. ", input$x)) +
      geom_smooth(method = "lm", se = FALSE) +
      theme(
        plot.title = element_text(size = 24),
        axis.title = element_text(size = 18), 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14),
        plot.caption = element_text(size = 14, face = "italic")
      )
  })
}



# Run the application 
shinyApp(ui = ui, server = server)