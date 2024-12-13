library(rsconnect)
library(shiny)
library(shinythemes)
library(xgboost)
library(data.table)
library(RCurl)
library(DT)

xgb_model <- readRDS("GoForIt_model.rds")

# distance, yards_to_goal, original_play_call, pos_score_diff, 
# rush_yards_per_attempt, pass_yards_per_attempt, period, 
# Season_Fourth_Down_Conversion_Rate, home_or_away, home_favored, converted

ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel(
                  div(
                    h1("XGBoost 4th Down Decision Making App"), 
                    h3("Created by Chandler Davis", style = "color: gray; font-weight: normal;"),
                    h4(a("GitHub Repository", href = "https://github.com/cdav15/4thDownModelCFB", 
                         style = "color: gray; font-weight: normal;", target = "_blank"))
                  )
                ),
                # Sidebar for user inputs
              
                sidebarLayout(
                  sidebarPanel(
                    h4("Enter data pertaining to the team in possession:"),
                    
                    fluidRow(
                      column(6, numericInput("distance", "Distance to First Down:", value = 4, min = 1, max = 100)),
                      column(6, numericInput("yards_to_goal", "Yards to Goal Line:", value = 35, min = 0, max = 100))
                    ),
                    
                    fluidRow(
                      column(6, numericInput("pos_score_diff", "Score Difference:", value = -7, min = -50, max = 50)),
                      column(6, numericInput("rush_yards_per_attempt", "Rush Yards Per Attempt:", value = 4.3, min = -10, max = 50))
                    ),
                    
                    fluidRow(
                      column(6, numericInput("pass_yards_per_attempt", "Pass Yards Per Attempt:", value = 7.8, min = -10, max = 50)),
                      column(6, numericInput("period", "Quarter:", value = 4, min = 1, max = 4))
                    ),
                    
                    fluidRow(
                      column(6, numericInput("Season_Fourth_Down_Conversion_Rate", "Season 4th Down Conversion Rate:", value = 0.5, min = 0, max = 1)),
                      column(6, numericInput("home_or_away", "Home (1) or Away (0):", value = 1, min = 0, max = 1))
                    ),
                    
                    fluidRow(
                      column(6, numericInput("home_favored", "Home Favored (1) or Not (0):", value = 1, min = 0, max = 1))
                    ),
                    
                    actionButton("predict", "Submit", class = 'btn btn-primary')
                  ),
                  
                  mainPanel(
                    DTOutput("converted_table"),
                    br(),
                    textOutput("recommendation"),
                    br(),
                    br(),
                    br(),
                    br(),
                    div(
                      HTML("<p><strong>Model Accuracy:</strong> Based upon a recommendation to go for it if the probability is above 50%.</p>
                          <p>If both probabilities are above 50%, the model recommends the play call with the higher probability.</p>")
                    ),
                    tags$style("#recommendation {font-size: 18px; font-weight: bold;}")
                  )
                )
)

server <- function(input, output) {
  
  xgb_model <- readRDS("GoForIt_model.rds")
  
  converted <- eventReactive(input$predict, {
    
    
    # Create data frames for pass and run scenarios
    new_data_pass <- data.frame(
      distance = input$distance,
      yards_to_goal = input$yards_to_goal,
      original_play_call = 1, # Pass is 1
      pos_score_diff = input$pos_score_diff,
      rush_yards_per_attempt = input$rush_yards_per_attempt,
      pass_yards_per_attempt = input$pass_yards_per_attempt,
      period = input$period,
      Season_Fourth_Down_Conversion_Rate = input$Season_Fourth_Down_Conversion_Rate,
      home_or_away = input$home_or_away,
      home_favored = input$home_favored
    )
    
    new_data_run <- data.frame(
      distance = input$distance,
      yards_to_goal = input$yards_to_goal,
      original_play_call = 2, # Run is 2
      pos_score_diff = input$pos_score_diff,
      rush_yards_per_attempt = input$rush_yards_per_attempt,
      pass_yards_per_attempt = input$pass_yards_per_attempt,
      period = input$period,
      Season_Fourth_Down_Conversion_Rate = input$Season_Fourth_Down_Conversion_Rate,
      home_or_away = input$home_or_away,
      home_favored = input$home_favored
    )
    
    # Convert the new data to matrices 
    pnew_data_matrix <- as.matrix(new_data_pass)
    rnew_data_matrix <- as.matrix(new_data_run)
    
    # Use the model to predict the probability of a first down for pass and run
    pred_prob_p <- predict(xgb_model, pnew_data_matrix)
    pred_prob_r <- predict(xgb_model, rnew_data_matrix)
    
    # Create a data frame to display both probabilities
    result <- data.frame(
      `Play Type` = c("Pass", "Run"),
      `Probability of Converting 4th Down Try` = paste0(round(c(pred_prob_p, pred_prob_r) * 100, 1), "%"),
      check.names = FALSE # Prevents spaces turning into periods
    )
    
    return(result)
  })
  
  converted1 <- eventReactive(input$predict, { # duplicate used only for the recommendation text
    
    new_data_pass <- data.frame(
      distance = input$distance,
      yards_to_goal = input$yards_to_goal,
      original_play_call = 1, # Pass is 1
      pos_score_diff = input$pos_score_diff,
      rush_yards_per_attempt = input$rush_yards_per_attempt,
      pass_yards_per_attempt = input$pass_yards_per_attempt,
      period = input$period,
      Season_Fourth_Down_Conversion_Rate = input$Season_Fourth_Down_Conversion_Rate,
      home_or_away = input$home_or_away,
      home_favored = input$home_favored
    )
    
    new_data_run <- data.frame(
      distance = input$distance,
      yards_to_goal = input$yards_to_goal,
      original_play_call = 2, # Run is 2
      pos_score_diff = input$pos_score_diff,
      rush_yards_per_attempt = input$rush_yards_per_attempt,
      pass_yards_per_attempt = input$pass_yards_per_attempt,
      period = input$period,
      Season_Fourth_Down_Conversion_Rate = input$Season_Fourth_Down_Conversion_Rate,
      home_or_away = input$home_or_away,
      home_favored = input$home_favored
    )
    
    
    pnew_data_matrix <- as.matrix(new_data_pass)
    rnew_data_matrix <- as.matrix(new_data_run)
    
    
    pred_prob_p <- predict(xgb_model, pnew_data_matrix)
    pred_prob_r <- predict(xgb_model, rnew_data_matrix)
    
    
    result <- data.frame(
      `Play Type` = c("Pass", "Run"),
      `Probability of Converting 4th Down Try` = round(c(pred_prob_p, pred_prob_r) * 100, 1),
      check.names = FALSE 
    )
    
    return(result)
    
  })
  
  output$converted_table <- renderDT({
    datatable(
      converted(),
      options = list(
        paging = FALSE,  # Disable pagination for a small table
        searching = FALSE,  # Remove search bar
        ordering = FALSE,  # Disable column sorting
        dom = 't',  # Simplify table controls
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE,  # Remove row names
      style = "bootstrap4",  
      class = "table table-striped table-bordered"  
    )
  })
  
  output$recommendation <- renderText({
    result <- converted1()

    max_row <- which.max(result$`Probability of Converting 4th Down Try`)
    
    max_play_type <- result$`Play Type`[max_row]
    max_prob <- result$`Probability of Converting 4th Down Try`[max_row]
    if (max_prob > 50) {
      paste0("Recommendation: Go for it with a ", max_play_type,". (",max_prob,"%)")
    } else {
      paste0("Recommendation: Do not go for it. (",max_prob,"%)")
    }
  })

}


shinyApp(ui = ui, server = server)

