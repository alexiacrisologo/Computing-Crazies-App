library(shiny)
library(readr)
library(dplyr)
library(shinythemes)
library(shinyjs)

# Load models and dataset
load_assets <- function() {
  tryCatch({
    list(
      data = read_csv("data/march_madness.csv"),
      final_four_model = readRDS("models/final_four_model_v2.rds"),
      final_model = readRDS("models/final_model.rds"),
      champion_model = readRDS("models/champion_model.rds")
    )
  }, error = function(e) {
    message("Error loading assets: ", e$message)
    NULL
  })
}

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$div(
    tags$img(
      src = "https://imgur.com/3Cindsd.png",
      height = "150px",
      style = "display: block; margin: 0 auto;"
    ),
    tags$head(tags$style(
      HTML("
        .btn-primary:hover {
          background-color: #173D7D !important;
        }
      ")
    )),
    h2("NCAA Matchup Predictor", align = "center", style = "color:#173D7D; font-weight:600;")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("season1", "Team 1 Season:", choices = NULL),
      selectInput("team1", "Team 1:", choices = NULL),
      selectInput("season2", "Team 2 Season:", choices = NULL),
      selectInput("team2", "Team 2:", choices = NULL),
      actionButton("go", "Simulate Matchup", class = "btn-primary", style = "width: 100%;")
    ),
    mainPanel(
      textOutput("winner"),
      useShinyjs(),
      uiOutput("probabilities")
    )
  )
)

# Server
server <- function(input, output, session) {
  assets <- load_assets()
  if (is.null(assets))
    stop("Required files not found.")
  
  data <- assets$data
  
  # Initialize season inputs
  updateSelectInput(session, "season1", choices = sort(unique(data$Season)), selected = max(data$Season))
  updateSelectInput(session, "season2", choices = sort(unique(data$Season)), selected = max(data$Season))
  
  # Update team lists based on selected season
  observeEvent(input$season1, {
    teams1 <- data %>% filter(Season == input$season1) %>% pull(`Full Team Name`) %>% unique() %>% sort()
    updateSelectInput(session, "team1", choices = teams1)
  })
  
  observeEvent(input$season2, {
    teams2 <- data %>% filter(Season == input$season2) %>% pull(`Full Team Name`) %>% unique() %>% sort()
    updateSelectInput(session, "team2", choices = teams2)
  })
  
  observeEvent(input$go, {
    req(input$team1, input$team2, input$season1, input$season2)
    
    t1 <- data %>% filter(Season == input$season1, `Full Team Name` == input$team1)
    t2 <- data %>% filter(Season == input$season2, `Full Team Name` == input$team2)
    
    if (nrow(t1) != 1 || nrow(t2) != 1) return()
    
    p1 <- predict(assets$final_four_model, t1, type = "response") *
      predict(assets$final_model, t1, type = "response") *
      predict(assets$champion_model, t1, type = "response")
    
    p2 <- predict(assets$final_four_model, t2, type = "response") *
      predict(assets$final_model, t2, type = "response") *
      predict(assets$champion_model, t2, type = "response")
    
    score1 <- log(p1 / (1 - p1))
    score2 <- log(p2 / (1 - p2))
    
    win1 <- round(100 * exp(score1) / (exp(score1) + exp(score2)), 1)
    win2 <- 100 - win1
    
    output$winner <- renderText({
      if (win1 > win2)
        paste("🏆 Predicted Winner:", input$team1, "with", win1, "% chance")
      else
        paste("🏆 Predicted Winner:", input$team2, "with", win2, "% chance")
    })
    
    output$probabilities <- renderUI({
      tags$table(class = "table table-hover table-bordered text-center",
                 tags$thead(tags$tr(
                   tags$th(style = "background-color:#173D7D; color:white;", "Team"),
                   tags$th(style = "background-color:#173D7D; color:white;", "Win Probability (%)")
                 )),
                 tags$tbody(
                   tags$tr(tags$td(
                     actionLink("info1", label = paste0(input$team1, " (", input$season1, ")"), style = "font-weight: 500;")
                   ), tags$td(sprintf("%.1f", win1))),
                   tags$tr(tags$td(
                     actionLink("info2", label = paste0(input$team2, " (", input$season2, ")"), style = "font-weight: 500;")
                   ), tags$td(sprintf("%.1f", win2)))
                 )
      )
    })
    
    # Info modal for Team 1
    observeEvent(input$info1, {
      t <- data %>% filter(Season == input$season1, `Full Team Name` == input$team1)
      showModal(
        modalDialog(
          title = paste(input$team1, "(", input$season1, ")"),
          paste0("Adjusted Offensive Efficiency: ", round(t$`Adjusted Offensive Efficiency`, 1)), br(),
          paste0("Adjusted Defensive Efficiency: ", round(t$`Adjusted Defensive Efficiency`, 1)), br(),
          paste0("Adjusted Tempo: ", round(t$AdjTempo, 1)), br(),
          paste0("Effective Field Goal % (eFG%): ", round(t$eFGPct, 1)), br(),
          paste0("Average Height: ", round(t$AvgHeight, 1)),
          easyClose = TRUE
        )
      )
    })
    
    # Info modal for Team 2
    observeEvent(input$info2, {
      t <- data %>% filter(Season == input$season2, `Full Team Name` == input$team2)
      showModal(
        modalDialog(
          title = paste(input$team2, "(", input$season2, ")"),
          paste0("Adjusted Offensive Efficiency: ", round(t$`Adjusted Offensive Efficiency`, 1)), br(),
          paste0("Adjusted Defensive Efficiency: ", round(t$`Adjusted Defensive Efficiency`, 1)), br(),
          paste0("Adjusted Tempo: ", round(t$AdjTempo, 1)), br(),
          paste0("Effective Field Goal % (eFG%): ", round(t$eFGPct, 1)), br(),
          paste0("Average Height: ", round(t$AvgHeight, 1)),
          easyClose = TRUE
        )
      )
    })
  })
}

shinyApp(ui, server)