#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

load("All_Players_Database.RData")
load("RookieDatabase.RData")
Rookie_database <- Rookie_database %>% arrange(full_name)

#contains IDs and full_name, going to need to use a lot to pull an ID when someone selects a player name, vice versa
player_search <- All_Players_Database %>%
  filter(season %in% c(0, 2022, 2023, 2024)) %>%
  filter(position %in% c("QB", "WR", "TE", "RB")) %>%
  arrange(desc(total_fantasy_points)) %>%
  select(full_name, player_id) %>%
  filter(!is.na(full_name)) %>%
  distinct()

#names for dropdown
dropdown_names <- player_search %>%
  select(full_name)

# Example default rosters (replace with your real 12 teams)
default_teams <- list(
  "Team B" = c("Ja'Marr Chase", "Joe Burrow", "Derrick Henry"),
  "Team C" = c("Justin Jefferson", "Patrick Mahomes", "Nick Chubb")
)

# Fake scores for demo purposes
player_scores <- data.frame(
  player = c("Christian McCaffrey", "Stefon Diggs", "Josh Allen",
             "Ja'Marr Chase", "Joe Burrow", "Derrick Henry",
             "Justin Jefferson", "Patrick Mahomes", "Nick Chubb"),
  score = c(22, 18, 25, 21, 20, 19, 23, 26, 17)
)

ui <- fluidPage(
  fluidRow(
    # Left panel: Team 1
    column(
      width = 2,
        h3("Team 1", align = "center"),
      wellPanel(
        selectInput("team1_select", "Select Team 1:", choices = names(default_teams))
      ),
      wellPanel(
        selectInput("add_player1",
              "Add Player to Team 1:",
              choices = dropdown_names,
              selectize = TRUE),
        div(
          style = "text-align: center;",
          actionButton("add_player1", "Add Player")
        ),
      ),
      wellPanel(
        selectInput("remove_player1", "Remove Player from Team 1:", choices = NULL),
      ),
      wellPanel(
        tags$b("Add Score from the week"),
        numericInput("add_score1", "Score:", value = 0, min = 0, max = 100),
        selectInput("add_pos1", "Position:", choices = c("QB", "RB", "WR", "TE")),
        div(
          style = "text-align: center;",
          actionButton("add_score1", "Add Score")
        )
      ),
    ),
  
    
    # Middle panel
    column(
      width = 8,
      h2("Team Projections Calculator", align = "center"),
      h4("Format: Best ball, superflex, PPR, 0.5 TEP, Start 9", align = "center"),
      plotOutput("main_plot", height = "200px"),
      fluidRow(
        column(width = 6, h4(textOutput("team1_mean"), align = "left")),
        column(width = 6, h4(textOutput("team2_mean"), align = "right"))
      )
    ),

    
    # Right panel: Team 2
    column(
      width = 2,
      h3("Team 2", align = "center"),
      wellPanel(
        selectInput("team1_select", "Select Team 2:", choices = names(default_teams))
      ),
      wellPanel(
        selectInput("add_player2", "Add Player to Team 2:", choices = NULL),
      ),
      wellPanel(
        selectInput("remove_player1", "Remove Player from Team 2:", choices = NULL),
      ),
      wellPanel(
        tags$b("Add Score from the week"),
        numericInput("add_score1", "Score:", value = 0, min = 0, max = 100),
        selectInput("add_pos1", "Position:", choices = c("QB", "RB", "WR", "TE")),
        actionButton("add_btn1", "Add"))
    ),
  )
)

server <- function(input, output, session) {
  # Store rosters as reactive values
  team1 <- reactiveVal(default_teams[[1]])
  team2 <- reactiveVal(default_teams[[2]])
  
  # Update Team 1 when selection changes
  observeEvent(input$team1_select, {
    roster <- default_teams[[input$team1_select]]
    team1(roster)
    updateSelectInput(session, "remove_player1", choices = roster)
  })
  
  # Update Team 2 when selection changes
  observeEvent(input$team2_select, {
    roster <- default_teams[[input$team2_select]]
    team2(roster)
    updateSelectInput(session, "remove_player2", choices = roster)
  })
  
  # Remove player from Team 1
  observeEvent(input$remove_player1, {
    new_roster <- setdiff(team1(), input$remove_player1)
    team1(new_roster)
    updateSelectInput(session, "remove_player1", choices = new_roster)
  })
  
  # Remove player from Team 2
  observeEvent(input$remove_player2, {
    new_roster <- setdiff(team2(), input$remove_player2)
    team2(new_roster)
    updateSelectInput(session, "remove_player2", choices = new_roster)
  })
  
  # Add player to Team 1
  observeEvent(input$add_btn1, {
    if (nzchar(input$add_player1)) {
      new_roster <- c(team1(), input$add_player1)
      team1(new_roster)
      updateSelectInput(session, "remove_player1", choices = new_roster)
    }
  })
  
  # Add player to Team 2
  observeEvent(input$add_btn2, {
    if (nzchar(input$add_player2)) {
      new_roster <- c(team2(), input$add_player2)
      team2(new_roster)
      updateSelectInput(session, "remove_player2", choices = new_roster)
    }
  })
  
  # Main plot
  output$main_plot <- renderPlot({
    roster1 <- team1()
    roster2 <- team2()
    
    scores1 <- player_scores$score[player_scores$player %in% roster1]
    scores2 <- player_scores$score[player_scores$player %in% roster2]
    
    barplot(
      rbind(scores1, scores2),
      beside = TRUE,
      names.arg = c(roster1, roster2),
      col = c("steelblue", "firebrick"),
      legend.text = c("Team 1", "Team 2"),
      args.legend = list(x = "topright"),
      main = "Team Comparison"
    )
  })
  
  # Projection means
  output$team1_mean <- renderText({
    scores1 <- player_scores$score[player_scores$player %in% team1()]
    paste("Team 1 projection:", round(mean(scores1, na.rm = TRUE), 1))
  })
  
  output$team2_mean <- renderText({
    scores2 <- player_scores$score[player_scores$player %in% team2()]
    paste("Team 2 projection:", round(mean(scores2, na.rm = TRUE), 1))
  })
}

shinyApp(ui, server)