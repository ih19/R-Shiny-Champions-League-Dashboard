################################################################################
# UEFA Champions League Analytics Dashboard - Main Application
# 
# File: 2_ucl_dashboard.R
# Author: Ihor Holubets
# Date: December 15, 2025
# Course: DSS 445 -  R Programming
#
# Purpose: Interactive Shiny dashboard for analyzing UEFA Champions League
#          2024-25 season data with multiple visualization and analysis tabs.
#
# Features:
#   - Interactive European map with all 36 teams
#   - League standings with multiple visualizations
#   - Top scorers analysis
#   - Individual team analysis with radar charts
#   - Match outcome predictor
#   - Head-to-head team comparisons
#
# Required Packages: shiny, shinydashboard, plotly, leaflet, dplyr, DT, shinyWidgets
# Required Data: champions_league_data.RData (created by 1_data_scraper.R)
################################################################################

# Load required packages
library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(dplyr)
library(DT)
library(shinyWidgets)

# Load the Champions League data
load("champions_league_data.RData")

# Define qualification zones
ucl_data <- ucl_data %>%
  mutate(
    QualStatus = case_when(
      Rank <= 8 ~ "Direct to Round of 16",
      Rank <= 24 ~ "Playoff Round",
      TRUE ~ "Eliminated"
    ),
    StatusColor = case_when(
      Rank <= 8 ~ "#2ecc71",
      Rank <= 24 ~ "#f39c12",
      TRUE ~ "#e74c3c"
    )
  )

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "⚽ Champions League Analytics", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("🗺️ Map", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("📊 League Table", tabName = "standings", icon = icon("trophy")),
      menuItem("⚽ Top Scorers", tabName = "scorers", icon = icon("futbol")),
      menuItem("📈 Team Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("🎯 Match Predictor", tabName = "predictor", icon = icon("magic")),
      menuItem("⚔️ Head-to-Head", tabName = "h2h", icon = icon("balance-scale"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f1; }
        .box { border-top: 3px solid #3498db; }
      "))
    ),
    tabItems(
      # TAB 1: European Map
      tabItem(
        tabName = "map",
        fluidRow(
          valueBoxOutput("total_teams", width = 3),
          valueBoxOutput("total_countries", width = 3),
          valueBoxOutput("total_goals", width = 3),
          valueBoxOutput("avg_goals", width = 3)
        ),
        fluidRow(
          box(width = 12, title = "Champions League Club Locations 2024-25",
              status = "primary", solidHeader = TRUE,
              leafletOutput("club_map", height = 550))
        ),
        fluidRow(
          box(width = 6, title = "Qualification Status", status = "info",
              plotlyOutput("qual_status_plot", height = 300)),
          box(width = 6, title = "Teams by Country", status = "info",
              plotlyOutput("country_plot", height = 300))
        )
      ),
      # TAB 2: League Table
      tabItem(
        tabName = "standings",
        fluidRow(
          box(width = 12, title = "Champions League Standings",
              status = "primary", solidHeader = TRUE,
              plotlyOutput("standings_visual", height = 500))
        ),
        fluidRow(
          box(width = 12, title = "Interactive Table", status = "info",
              DTOutput("standings_table"))
        ),
        fluidRow(
          box(width = 6, title = "Attack vs Defense",
              plotlyOutput("attack_defense_plot", height = 350)),
          box(width = 6, title = "Expected Goals Analysis",
              plotlyOutput("xg_analysis", height = 350))
        )
      ),
      # TAB 3: Top Scorers
      tabItem(
        tabName = "scorers",
        fluidRow(
          box(width = 12, title = "Champions League Top Scorers 2024-25",
              status = "primary", solidHeader = TRUE,
              plotlyOutput("top_scorers_plot", height = 450))
        ),
        fluidRow(
          box(width = 6, title = "Goals vs Assists",
              plotlyOutput("goals_assists_scatter", height = 400)),
          box(width = 6, title = "Player Performance Table",
              DTOutput("scorers_table"))
        )
      ),
      # TAB 4: Team Analysis
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(width = 4, status = "primary",
              pickerInput("selected_team", "Select Team:",
                          choices = sort(ucl_data$Squad), selected = "Liverpool",
                          options = list(`live-search` = TRUE))),
          box(width = 8, status = "info", uiOutput("team_summary"))
        ),
        fluidRow(
          box(width = 6, title = "Team Performance Radar", status = "primary",
              plotlyOutput("team_radar", height = 400)),
          box(width = 6, title = "Goal Statistics", status = "primary",
              plotlyOutput("team_goals", height = 400))
        ),
        fluidRow(
          box(width = 12, title = "Compare with League Average", status = "info",
              plotlyOutput("team_comparison", height = 300))
        )
      ),
      # TAB 5: Match Predictor
      tabItem(
        tabName = "predictor",
        fluidRow(
          box(width = 12, title = "⚽ Champions League Match Outcome Predictor",
              status = "primary", solidHeader = TRUE,
              h4("Predict match outcomes based on current team statistics"))
        ),
        fluidRow(
          box(width = 5, title = "Home Team", status = "success",
              pickerInput("home_team", "Select Home Team:",
                          choices = sort(ucl_data$Squad), selected = "Real Madrid",
                          options = list(`live-search` = TRUE)),
              uiOutput("home_team_stats")),
          box(width = 2, title = "VS", status = "warning", align = "center",
              br(), br(),
              actionButton("predict_match", "PREDICT", icon = icon("calculator"),
                           class = "btn-warning btn-lg", width = "100%")),
          box(width = 5, title = "Away Team", status = "danger",
              pickerInput("away_team", "Select Away Team:",
                          choices = sort(ucl_data$Squad), selected = "Barcelona",
                          options = list(`live-search` = TRUE)),
              uiOutput("away_team_stats"))
        ),
        fluidRow(
          box(width = 12, title = "Match Prediction Result", status = "primary",
              solidHeader = TRUE, uiOutput("prediction_output"))
        ),
        fluidRow(
          box(width = 12, title = "Win Probability Visualization",
              plotlyOutput("prob_visual", height = 300))
        )
      ),
      # TAB 6: Head-to-Head
      tabItem(
        tabName = "h2h",
        fluidRow(
          box(width = 6, status = "primary",
              pickerInput("team_a", "Team A:",
                          choices = sort(ucl_data$Squad), selected = "Liverpool",
                          options = list(`live-search` = TRUE))),
          box(width = 6, status = "danger",
              pickerInput("team_b", "Team B:",
                          choices = sort(ucl_data$Squad), selected = "Barcelona",
                          options = list(`live-search` = TRUE)))
        ),
        fluidRow(
          box(width = 12, title = "Statistical Comparison Radar", status = "primary",
              plotlyOutput("h2h_radar", height = 450))
        ),
        fluidRow(
          box(width = 12, title = "Side-by-Side Statistics",
              DTOutput("h2h_table"))
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Value Boxes
  output$total_teams <- renderValueBox({
    valueBox(nrow(ucl_data), "Total Teams", icon = icon("users"), color = "blue")
  })
  
  output$total_countries <- renderValueBox({
    valueBox(length(unique(ucl_data$Country)), "Countries", icon = icon("flag"), color = "green")
  })
  
  output$total_goals <- renderValueBox({
    valueBox(sum(ucl_data$GoalsFor), "Total Goals", icon = icon("futbol"), color = "yellow")
  })
  
  output$avg_goals <- renderValueBox({
    valueBox(round(mean(ucl_data$GoalsFor), 1), "Avg Goals/Team", icon = icon("chart-bar"), color = "red")
  })
  
  # TAB 1: Club Map
  output$club_map <- renderLeaflet({
    pal <- colorFactor(palette = c("#2ecc71", "#f39c12", "#e74c3c"),
                       domain = c("Direct to Round of 16", "Playoff Round", "Eliminated"))
    
    leaflet(ucl_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 10, lat = 50, zoom = 4) %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude, radius = ~sqrt(Points) * 1.5,
        color = ~pal(QualStatus), fillOpacity = 0.7, stroke = TRUE, weight = 2,
        popup = ~paste0("<strong>", Squad, "</strong><br>Rank: ", Rank, 
                        "<br>Points: ", Points, "<br>Record: ", Wins, "W - ", 
                        Draws, "D - ", Losses, "L<br>Status: ", QualStatus),
        label = ~Squad) %>%
      addLegend("bottomright", pal = pal, values = ~QualStatus,
                title = "Qualification Status", opacity = 0.8)
  })
  
  output$qual_status_plot <- renderPlotly({
    status_counts <- ucl_data %>% 
      count(QualStatus)
    
    # Create ordered dataframe with explicit color mapping
    status_data <- data.frame(
      Status = c("Direct to Round of 16", "Playoff Round", "Eliminated"),
      Count = c(
        sum(status_counts$n[status_counts$QualStatus == "Direct to Round of 16"]),
        sum(status_counts$n[status_counts$QualStatus == "Playoff Round"]),
        sum(status_counts$n[status_counts$QualStatus == "Eliminated"])
      ),
      Color = c("#2ecc71", "#f39c12", "#e74c3c")  # Green, Orange, Red
    )
    
    plot_ly(status_data, 
            labels = ~Status, 
            values = ~Count, 
            type = "pie",
            marker = list(colors = ~Color),
            textinfo = "label+percent") %>% 
      layout(showlegend = TRUE)
  })
  
  output$country_plot <- renderPlotly({
    country_counts <- ucl_data %>% count(Country) %>% arrange(desc(n)) %>% head(10)
    plot_ly(country_counts, x = ~reorder(Country, n), y = ~n, type = "bar",
            marker = list(color = "#3498db"), text = ~n, textposition = "outside") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Number of Teams"))
  })
  
  # TAB 2: Standings
  output$standings_visual <- renderPlotly({
    # Ensure all 36 teams are included in rank order
    standings_data <- ucl_data %>%
      arrange(Rank) %>%
      mutate(
        Squad = factor(Squad, levels = Squad),
        QualStatus = factor(QualStatus, 
                            levels = c("Direct to Round of 16", "Playoff Round", "Eliminated"))
      )
    
    plot_ly(standings_data, 
            x = ~Squad, 
            y = ~Points, 
            type = "bar",
            color = ~QualStatus, 
            colors = c("Direct to Round of 16" = "#2ecc71",
                       "Playoff Round" = "#f39c12", 
                       "Eliminated" = "#e74c3c"),
            text = ~paste0(Squad, "\n", Points, " pts"),
            hoverinfo = "text") %>%
      layout(
        xaxis = list(
          title = "", 
          tickangle = -45, 
          tickfont = list(size = 9)
        ), 
        yaxis = list(title = "Points", range = c(0, max(ucl_data$Points) + 2)),
        showlegend = TRUE,
        legend = list(
          orientation = "h", 
          y = -0.35, 
          x = 0.5, 
          xanchor = "center",
          traceorder = "normal"
        ),
        margin = list(b = 140, l = 60, r = 40, t = 40)
      )
  })
  
  output$standings_table <- renderDT({
    datatable(ucl_data %>% select(Rank, Squad, Matches, Wins, Draws, Losses,
                                  GoalsFor, GoalsAgainst, GoalDiff, Points, QualStatus) %>%
                arrange(Rank), options = list(pageLength = 20), rownames = FALSE)
  })
  
  output$attack_defense_plot <- renderPlotly({
    plot_ly(ucl_data, x = ~GoalsFor, y = ~GoalsAgainst, type = "scatter",
            mode = "markers", text = ~Squad,
            marker = list(size = ~Points, color = ~Points, colorscale = "Viridis", showscale = TRUE)) %>%
      layout(xaxis = list(title = "Goals Scored"), yaxis = list(title = "Goals Conceded"))
  })
  
  output$xg_analysis <- renderPlotly({
    # Simple, bulletproof version
    max_val <- ceiling(max(c(ucl_data$ExpectedGoals, ucl_data$GoalsFor), na.rm = TRUE))
    
    plot_ly() %>%
      add_trace(
        data = ucl_data,
        x = ~ExpectedGoals, 
        y = ~GoalsFor,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = "#3498db", opacity = 0.7),
        name = ~Squad,
        showlegend = FALSE
      ) %>%
      add_trace(
        x = c(0, max_val), 
        y = c(0, max_val),
        type = "scatter",
        mode = "lines",
        line = list(dash = "dash", color = "red", width = 2),
        name = "Perfect Prediction",
        showlegend = TRUE
      ) %>%
      layout(
        xaxis = list(title = "Expected Goals (xG)", range = c(0, max_val)), 
        yaxis = list(title = "Actual Goals Scored", range = c(0, max_val)),
        hovermode = "closest"
      )
  })
  
  # TAB 3: Top Scorers
  output$top_scorers_plot <- renderPlotly({
    top_scorers <- players %>% arrange(desc(Goals)) %>% head(15)
    plot_ly(top_scorers, y = ~reorder(Player, Goals), x = ~Goals, type = "bar",
            orientation = "h", marker = list(color = "#e74c3c"),
            text = ~Goals, textposition = "outside", customdata = ~Squad) %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Goals"))
  })
  
  output$goals_assists_scatter <- renderPlotly({
    plot_ly(players, x = ~Assists, y = ~Goals, type = "scatter",
            mode = "markers", text = ~Player,
            marker = list(size = 12, color = ~Goals + Assists, colorscale = "Reds", showscale = TRUE),
            customdata = ~Squad) %>%
      layout(xaxis = list(title = "Assists"), yaxis = list(title = "Goals"))
  })
  
  output$scorers_table <- renderDT({
    datatable(players %>% arrange(desc(Goals)) %>%
                select(Player, Squad, Position, Goals, Assists, xG, xA),
              options = list(pageLength = 10), rownames = FALSE)
  })
  
  # TAB 4: Team Analysis
  output$team_summary <- renderUI({
    team <- ucl_data %>% filter(Squad == input$selected_team)
    HTML(paste0("<div style='padding: 10px;'><h3 style='color: #3498db;'>", team$Squad,
                "</h3><p><strong>Country:</strong> ", team$Country, " | <strong>Rank:</strong> ",
                team$Rank, "/36 | <strong>Status:</strong> <span style='color:", team$StatusColor,
                ";'>", team$QualStatus, "</span></p></div>"))
  })
  
  output$team_radar <- renderPlotly({
    team <- ucl_data %>% filter(Squad == input$selected_team)
    league_avg <- ucl_data %>% summarise(across(c(Points, GoalsFor, GoalDiff, Wins), mean))
    max_vals <- ucl_data %>% summarise(across(c(Points, GoalsFor, GoalDiff, Wins), max))
    
    team_norm <- c((team$Points/max_vals$Points)*100, (team$GoalsFor/max_vals$GoalsFor)*100,
                   ((team$GoalDiff+abs(min(ucl_data$GoalDiff)))/(max(ucl_data$GoalDiff)+abs(min(ucl_data$GoalDiff))))*100,
                   (team$Wins/max_vals$Wins)*100)
    avg_norm <- c((league_avg$Points/max_vals$Points)*100, (league_avg$GoalsFor/max_vals$GoalsFor)*100,
                  ((league_avg$GoalDiff+abs(min(ucl_data$GoalDiff)))/(max(ucl_data$GoalDiff)+abs(min(ucl_data$GoalDiff))))*100,
                  (league_avg$Wins/max_vals$Wins)*100)
    
    plot_ly(type = "scatterpolar", fill = "toself") %>%
      add_trace(r = team_norm, theta = c("Points", "Goals For", "Goal Diff", "Wins"),
                name = input$selected_team, fillcolor = "rgba(52,152,219,0.5)") %>%
      add_trace(r = avg_norm, theta = c("Points", "Goals For", "Goal Diff", "Wins"),
                name = "League Average", fillcolor = "rgba(149,165,166,0.3)") %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))))
  })
  
  output$team_goals <- renderPlotly({
    team <- ucl_data %>% filter(Squad == input$selected_team)
    plot_ly() %>%
      add_trace(x = c("Goals For", "Goals Against"), y = c(team$GoalsFor, team$GoalsAgainst),
                type = "bar", marker = list(color = c("#2ecc71", "#e74c3c")),
                text = c(team$GoalsFor, team$GoalsAgainst), textposition = "outside") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Goals"), showlegend = FALSE)
  })
  
  output$team_comparison <- renderPlotly({
    team <- ucl_data %>% filter(Squad == input$selected_team)
    league_avg <- ucl_data %>% summarise(Points = mean(Points), GoalsFor = mean(GoalsFor),
                                         GoalsAgainst = mean(GoalsAgainst), Wins = mean(Wins))
    comparison <- data.frame(Metric = c("Points", "Goals For", "Goals Against", "Wins"),
                             Team = c(team$Points, team$GoalsFor, team$GoalsAgainst, team$Wins),
                             Average = c(league_avg$Points, league_avg$GoalsFor,
                                         league_avg$GoalsAgainst, league_avg$Wins))
    plot_ly(comparison, x = ~Metric, y = ~Team, type = "bar",
            name = input$selected_team, marker = list(color = "#3498db")) %>%
      add_trace(y = ~Average, name = "League Average", marker = list(color = "#95a5a6")) %>%
      layout(barmode = "group", yaxis = list(title = "Value"))
  })
  
  # TAB 5: Match Predictor
  output$home_team_stats <- renderUI({
    home <- ucl_data %>% filter(Squad == input$home_team)
    HTML(paste0("<div style='padding:10px;background-color:#d5f4e6;border-radius:5px;'>",
                "<p><strong>Rank:</strong> ", home$Rank, "</p><p><strong>Points:</strong> ",
                home$Points, "</p><p><strong>Goals For:</strong> ", home$GoalsFor,
                "</p><p><strong>Goals Against:</strong> ", home$GoalsAgainst, "</p></div>"))
  })
  
  output$away_team_stats <- renderUI({
    away <- ucl_data %>% filter(Squad == input$away_team)
    HTML(paste0("<div style='padding:10px;background-color:#fadbd8;border-radius:5px;'>",
                "<p><strong>Rank:</strong> ", away$Rank, "</p><p><strong>Points:</strong> ",
                away$Points, "</p><p><strong>Goals For:</strong> ", away$GoalsFor,
                "</p><p><strong>Goals Against:</strong> ", away$GoalsAgainst, "</p></div>"))
  })
  
  observeEvent(input$predict_match, {
    home <- ucl_data %>% filter(Squad == input$home_team)
    away <- ucl_data %>% filter(Squad == input$away_team)
    
    home_strength <- (home$Points*0.4) + (home$GoalDiff*0.3) + (home$GoalsFor*0.2) + (8-home$GoalsAgainst)*0.1
    away_strength <- (away$Points*0.4) + (away$GoalDiff*0.3) + (away$GoalsFor*0.2) + (8-away$GoalsAgainst)*0.1
    home_strength <- home_strength * 1.1
    total_strength <- home_strength + away_strength
    home_prob <- (home_strength/total_strength)*100
    away_prob <- (away_strength/total_strength)*100
    draw_prob <- 100 - home_prob - away_prob + 15
    home_prob <- home_prob - 7.5
    away_prob <- away_prob - 7.5
    
    winner <- if(home_prob > away_prob && home_prob > draw_prob) input$home_team
    else if(away_prob > home_prob && away_prob > draw_prob) input$away_team
    else "DRAW"
    
    output$prediction_output <- renderUI({
      HTML(paste0("<div style='text-align:center;padding:30px;'><h2>Predicted Winner: <span style='color:#e74c3c;'>",
                  winner, "</span></h2><p style='font-size:18px;'><strong>", input$home_team,
                  " Win:</strong> ", round(home_prob,1), "%</p><p style='font-size:18px;'><strong>Draw:</strong> ",
                  round(draw_prob,1), "%</p><p style='font-size:18px;'><strong>", input$away_team,
                  " Win:</strong> ", round(away_prob,1), "%</p></div>"))
    })
    
    output$prob_visual <- renderPlotly({
      probs <- data.frame(Outcome = c(input$home_team, "Draw", input$away_team),
                          Probability = c(home_prob, draw_prob, away_prob))
      plot_ly(probs, x = ~Outcome, y = ~Probability, type = "bar",
              marker = list(color = c("#2ecc71", "#f39c12", "#e74c3c")),
              text = ~paste0(round(Probability,1), "%"), textposition = "outside") %>%
        layout(yaxis = list(title = "Win Probability (%)"), xaxis = list(title = ""))
    })
  })
  
  # TAB 6: Head-to-Head
  output$h2h_radar <- renderPlotly({
    team_a <- ucl_data %>% filter(Squad == input$team_a)
    team_b <- ucl_data %>% filter(Squad == input$team_b)
    categories <- c("Points", "Wins", "Goals For", "Goal Diff")
    max_vals <- c(max(ucl_data$Points), max(ucl_data$Wins), max(ucl_data$GoalsFor),
                  max(ucl_data$GoalDiff) - min(ucl_data$GoalDiff))
    
    plot_ly(type = "scatterpolar", fill = "toself") %>%
      add_trace(r = c(team_a$Points/max_vals[1]*100, team_a$Wins/max_vals[2]*100,
                      team_a$GoalsFor/max_vals[3]*100,
                      (team_a$GoalDiff-min(ucl_data$GoalDiff))/max_vals[4]*100),
                theta = categories, name = input$team_a, fillcolor = "rgba(52,152,219,0.5)") %>%
      add_trace(r = c(team_b$Points/max_vals[1]*100, team_b$Wins/max_vals[2]*100,
                      team_b$GoalsFor/max_vals[3]*100,
                      (team_b$GoalDiff-min(ucl_data$GoalDiff))/max_vals[4]*100),
                theta = categories, name = input$team_b, fillcolor = "rgba(231,76,60,0.5)") %>%
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))))
  })
  
  output$h2h_table <- renderDT({
    team_a <- ucl_data %>% filter(Squad == input$team_a)
    team_b <- ucl_data %>% filter(Squad == input$team_b)
    comparison <- data.frame(
      Statistic = c("Rank", "Points", "Matches", "Wins", "Draws", "Losses",
                    "Goals For", "Goals Against", "Goal Difference"),
      TeamA = c(team_a$Rank, team_a$Points, team_a$Matches, team_a$Wins, team_a$Draws,
                team_a$Losses, team_a$GoalsFor, team_a$GoalsAgainst, team_a$GoalDiff),
      TeamB = c(team_b$Rank, team_b$Points, team_b$Matches, team_b$Wins, team_b$Draws,
                team_b$Losses, team_b$GoalsFor, team_b$GoalsAgainst, team_b$GoalDiff)
    )
    names(comparison)[2:3] <- c(input$team_a, input$team_b)
    datatable(comparison, options = list(dom = 't', pageLength = 15), rownames = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)