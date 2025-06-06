library(shiny)
library(dplyr)
library(ggplot2)
library(ggimage)
library(plotly)
library(nflreadr)

# Data Gathering & Processing
season_data <- load_pbp(2024)
combine_data <- load_combine()

rownames(combine_data)[rownames(combine_data) == "SAF"] <- "S"
cleaned_data <- combine_data[complete.cases(combine_data), ]

# Normalization
cleaned_data$broad_jump <- (cleaned_data$broad_jump - min(cleaned_data$broad_jump)) / (max(cleaned_data$broad_jump) - min(cleaned_data$broad_jump))
cleaned_data$forty <- (cleaned_data$forty - min(cleaned_data$forty)) / (max(cleaned_data$forty) - min(cleaned_data$forty))
cleaned_data$bench <- (cleaned_data$bench - min(cleaned_data$bench)) / (max(cleaned_data$bench) - min(cleaned_data$bench))
cleaned_data$vertical <- (cleaned_data$vertical - min(cleaned_data$vertical)) / (max(cleaned_data$vertical) - min(cleaned_data$vertical))
cleaned_data$cone <- (cleaned_data$cone - min(cleaned_data$cone)) / (max(cleaned_data$cone) - min(cleaned_data$cone))
cleaned_data$shuttle <- (cleaned_data$shuttle - min(cleaned_data$shuttle)) / (max(cleaned_data$shuttle) - min(cleaned_data$shuttle))

# Mapping Team colors
team_colors <- c(
  "Arizona Cardinals" = "#97233F",
  "Atlanta Falcons" = "#A71930",
  "Baltimore Ravens" = "#241773",
  "Buffalo Bills" = "#00338D",
  "Carolina Panthers" = "#0085CA",
  "Chicago Bears" = "#0B162A",
  "Cincinnati Bengals" = "#FB4F14",
  "Cleveland Browns" = "#311D00",
  "Dallas Cowboys" = "#041E42",
  "Denver Broncos" = "#FB4F14",
  "Detroit Lions" = "#0076B6",
  "Green Bay Packers" = "#203731",
  "Houston Texans" = "#03202F",
  "Indianapolis Colts" = "#002C5F",
  "Jacksonville Jaguars" = "#101820",
  "Kansas City Chiefs" = "#E31837",
  "Las Vegas Raiders" = "#000000",
  "Los Angeles Chargers" = "#0072CE",
  "Los Angeles Rams" = "#002244",
  "Miami Dolphins" = "#008E97",
  "Minnesota Vikings" = "#4F2683",
  "New England Patriots" = "#002244",
  "New Orleans Saints" = "#D3BC8D",
  "New York Jets" = "#203731",
  "Philadelphia Eagles" = "#004C54",
  "Pittsburgh Steelers" = "#FFB81C",
  "San Francisco 49ers" = "#AA0000",
  "Seattle Seahawks" = "#002244",
  "Tampa Bay Buccaneers" = "#D50A0A",
  "Tennessee Titans" = "#0C2340",
  "Washington Commanders" = "#773141"
)

# ui
ui <- fluidPage(
  titlePanel("NFL Performance Analytics"),
  sidebarLayout(
    sidebarPanel(
      helpText(
        "The dataset is retrieved from the NFLStat official website. This dataset is based on data as of November 29, 2024.",
        tags$br(), tags$br(),
        "The visualization is created by Jimin Lee and Yoonjoo Lee."
      ),
      selectInput("plot_type", "Choose a Plot Types:", 
                  choices = c("Player Performance Bar Graph", "Combine Spider Chart", "Physical vs Performance Scatter plot")),
      conditionalPanel(
        condition = "input.plot_type == 'Player Performance Bar Graph'",
        selectInput("PerformanceType", "Choose Performance Type:", 
                    choices = c("Passing Yards", "Receiving Yards", "Rushing Yards", "Tackles", "Interceptions"))
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Combine Spider Chart'",
        selectInput("team", "Select Team:", choices = unique(cleaned_data$draft_team)),
        selectInput("position", "Select Position:", choices = c("RB", "FB", "TE", "OT", "OG", "C", "QB", "WR", "OL", "DE", "DT", "OLB", "ILB", "CB", "S", "LB", "DL", "DB", "EDGE")),
        uiOutput("player_ui")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Physical vs Performance Scatter plot'",
        selectInput("physical", "Select Physical Attribute (X-axis):", 
                    choices = c("Height" = "ht", "Weight" = "wt")),
        selectInput("performance", "Select Performance Attribute (Y-axis):", 
                    choices = c("Shuttle" = "shuttle", "Forty" = "forty", "Vertical" = "vertical", "Bench" = "bench", "Broad Jump" = "broad_jump", "Three-Cone" = "cone")),
        selectizeInput("positions", "Select Positions:", 
                       choices = c("All", sort(unique(combine_data$pos))), selected = "All", multiple = TRUE)
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.plot_type == 'Player Performance Bar Graph'",
        plotOutput("barPlot", height = "600px")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Combine Spider Chart'",
        plotlyOutput("spider_chart"),
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: flex-start; margin-top: 20px;",
          tags$div(
            style = "flex: 1; font-size: 16px; margin-right: 30px;",
            tags$b("Offense: "),
            "RB (Running Back), FB (Fullback), TE (Tight End), OT (Offensive Tackle), OG (Offensive Guard), C (Center), QB (Quarterback), WR (Wide Receiver), OL (Offensive Line)",
            tags$br(),
            tags$br(),  
            tags$b("Defense: "),
            "DE (Defensive End), DT (Defensive Tackle), OLB (Outside Linebacker), ILB (Inside Linebacker), CB (Cornerback), S (Safety), LB (Linebacker), DL (Defensive Line), DB (Defensive Back), EDGE"
          ),
          tags$div(
            style = "position: relative; top: -200px; right: 20px; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; border-radius: 5px; width: 250px;", 
            tags$p(
              tags$b("Note: "),
              "The 'Average' represents the normalized performance metrics averaged across all players in the selected position across all teams.",
              style = "font-size: 13px; line-height: 1.5; margin: 0;"
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Physical vs Performance Scatter plot'",
        plotlyOutput("scatter_plot"),
        tags$div(
          style = "font-size: 15.5px; margin-top: 20px; display: flex; justify-content: space-between; align-items: flex-start;",
          tags$div(
            style = "flex: 1; font-size: 16px; margin-right: 30px;",
            tags$b("Offense: "),
            "RB (Running Back), FB (Fullback), TE (Tight End), OT (Offensive Tackle), OG (Offensive Guard), C (Center), QB (Quarterback), WR (Wide Receiver), OL (Offensive Line)",
            tags$br(),
            tags$br(),  
            tags$b("Defense: "),
            "DE (Defensive End), DT (Defensive Tackle), OLB (Outside Linebacker), ILB (Inside Linebacker), CB (Cornerback), S (Safety), LB (Linebacker), DL (Defensive Line), DB (Defensive Back), EDGE"
          )
        )
      )
    )
  )
)



# server
server <- function(input, output, session) {
  # Bar Graph
  output$barPlot <- renderPlot({
    if (input$plot_type != "Player Performance Bar Graph") return(NULL)
    
    if (input$PerformanceType == "Passing Yards") {
      data <- read.csv("Data for Bar Graph/Passing_Yards_Dataset.csv") %>%
        mutate(image_path = c(
          "Players Picture/G.Smith.png", "Players Picture/J.Burrow.png", 
          "Players Picture/L.Jackson.png", "Players Picture/C.J.Stroud.png", 
          "Players Picture/K.Cousins.png", "Players Picture/M.Stafford.png", 
          "Players Picture/B.Mayfield.png", "Players Picture/J.Goff.png", 
          "Players Picture/S.Darnold.png", "Players Picture/P.Mahomes.png"
        ))
      y_var <- "Passing.Yards"
      title <- "Top 10 Passers by Passing Yards"
    } else if (input$PerformanceType == "Receiving Yards") {
      data <- read.csv("Data for Bar Graph/Receiving_Yards_Dataset.csv") %>%
        mutate(image_path = c(
          "Players Picture/J.Chase.png", "Players Picture/J.Jefferson.png", 
          "Players Picture/C.Lambs.png", "Players Picture/T.McLaurin.png", 
          "Players Picture/G.Pickens.png", "Players Picture/J.Smith-Njigba.png", 
          "Players Picture/A.St.Brown.png", "Players Picture/C.Sutton.png", 
          "Players Picture/B.Bowers.png", "Players Picture/A.J.Brown.png"
        ))
      y_var <- "Receiving.Yards"
      title <- "Top 10 Receivers by Receiving Yards"
    } else if (input$PerformanceType == "Rushing Yards") {
      data <- read.csv("Data for Bar Graph/Rushing_Yards_Dataset.csv") %>%
        mutate(image_path = c(
          "Players Picture/S.Barkley.png", "Players Picture/D.Henry.png", 
          "Players Picture/J.Jacobs.png", "Players Picture/J.Gibbs.png", 
          "Players Picture/C.Hubbard.png", "Players Picture/K.Williams.png", 
          "Players Picture/T.Pollard.png", "Players Picture/A.Jones.png", 
          "Players Picture/J.Mixon.png", "Players Picture/B.Robinson.png"
        ))
      y_var <- "Rushing.Yards"
      title <- "Top 10 Rushers by Rushing Yards"
    } else if (input$PerformanceType == "Tackles") {
      data <- read.csv("Data for Bar Graph/Tackles_Dataset.csv") %>%
        mutate(image_path = c(
          "Players Picture/Z.Franklin.png", "Players Picture/N.Cross.png", 
          "Players Picture/R.Smith.png", "Players Picture/E.J.Speed.png", 
          "Players Picture/E.Kendricks.png", "Players Picture/B.Baker.png", 
          "Players Picture/Z.Baun.png", "Players Picture/L.Wilson.png", 
          "Players Picture/J.Brooks.png", "Players Picture/D.Henley.png"
        ))
      y_var <- "Combined.Tackles"
      title <- "Top 10 Tacklers"
    } else {
      data <- read.csv("Data for Bar Graph/Interceptions_Dataset.csv") %>%
        mutate(image_path = c(
          "Players Picture/X.McKinney.png", "Players Picture/K.Joseph.png", 
          "Players Picture/M.Humphrey.png", "Players Picture/J.McCollough.png", 
          "Players Picture/D.Jackson.png", "Players Picture/C.Bulllock.png", 
          "Players Picture/B.Murphy.png", "Players Picture/B.Branch.png", 
          "Players Picture/P.Adebo.png", "Players Picture/B.Bishop Jr.png"
        ))
      y_var <- "Interceptions"
      title <- "Top 10 Players by Interceptions"
    }
    
    ggplot(data, aes(x = reorder(Name, !!sym(y_var)), y = !!sym(y_var), fill = Team)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_image(aes(image = image_path), size = 0.15, by = "width") +
      geom_text(aes(label = !!sym(y_var)), vjust = 0.5, hjust = 2.9, size = 6, face = "bold") + 
      coord_flip() +
      labs(title = title, x = "Players", y = paste("Total", gsub("\\.", " ", y_var))) +
      scale_fill_manual(values = team_colors) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        legend.title = element_text(size = 14),  
        legend.text = element_text(size = 12) 
      )
  })
  
  # ui for Combine Spider Chart
  observe({
    filtered_data <- cleaned_data %>%
      filter(draft_team == input$team, pos == input$position)
    updateSelectInput(session, "player", "Select Player:", choices = unique(filtered_data$player_name))
  })
  
  output$player_ui <- renderUI({
    selectInput("player", "Select Player:", choices = NULL)
  })
  
  player_data <- reactive({
    cleaned_data %>%
      filter(player_name == input$player) %>%
      select(shuttle, forty, vertical, bench, broad_jump, cone)
  })
  
  average_data <- reactive({
    cleaned_data %>%
      filter(pos == input$position) %>%
      summarise(
        shuttle = mean(shuttle, na.rm = TRUE),
        forty = mean(forty, na.rm = TRUE),
        vertical = mean(vertical, na.rm = TRUE),
        bench = mean(bench, na.rm = TRUE),
        broad_jump = mean(broad_jump, na.rm = TRUE),
        cone = mean(cone, na.rm = TRUE)
      )
  })
  
  # Combine Spider Chart
  output$spider_chart <- renderPlotly({
    if (input$plot_type != "Combine Spider Chart") return(NULL)
    if (is.null(input$player)) return(NULL)
    
    player_vals <- as.numeric(player_data())
    avg_vals <- as.numeric(average_data())
    categories <- c("Shuttle", "Forty", "Vertical", "Bench", "Broad Jump", "Three-Cone")
    
    data <- data.frame(
      Category = rep(categories, 2),
      Value = c(player_vals, avg_vals),
      Type = rep(c("Current Player", "Average"), each = length(categories))
    )
    
    # Interactive Title
    dynamic_title <- paste(
      "<b>Performance Spider Chart</b> / Team:",
      input$team,
      "/ Position:",
      input$position
    )
    
    fig <- plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(r = data$Value[data$Type == "Average"],
                theta = data$Category[data$Type == "Average"],
                fill = 'toself', name = 'Average', fillcolor = 'rgba(50, 50, 50, 0.55)', 
                line = list(color = 'grey'), marker = list(color = 'grey')) %>%
      add_trace(r = data$Value[data$Type == "Current Player"],
                theta = data$Category[data$Type == "Current Player"],
                fill = 'toself', name = 'Current Player', fillcolor = 'rgba(0, 0, 255, 0.55)', 
                line = list(color = 'blue'), marker = list(color = 'blue')) %>%
      layout(
        polar = list(radialaxis = list(visible = TRUE)),
        title = list(text = dynamic_title, font = list(size = 15))
      )
    
    fig
  })
  
  
  # Filtering data for scatter plot
  filtered_scatter_data <- reactive({
    if ("All" %in% input$positions) {
      combine_data %>%
        select(player_name, pos, ht, wt, shuttle, forty, vertical, bench, broad_jump, cone)
    } else {
      combine_data %>%
        filter(pos %in% input$positions) %>%
        select(player_name, pos, ht, wt, shuttle, forty, vertical, bench, broad_jump, cone)
    }
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlotly({
    if (input$plot_type != "Physical vs Performance Scatter plot") return(NULL)
    
    data <- filtered_scatter_data()
    
    physical_label <- ifelse(input$physical == "ht", "Height (inches)", 
                             ifelse(input$physical == "wt", "Weight (lbs)", input$physical))
    performance_label <- ifelse(input$performance == "shuttle", "Shuttle (sec)", 
                                ifelse(input$performance == "forty", "40-Yard Dash (sec)",
                                       ifelse(input$performance == "vertical", "Vertical Jump (inches)", 
                                              ifelse(input$performance == "bench", "Bench Press (reps)", 
                                                     ifelse(input$performance == "broad_jump", "Broad Jump (inches)", 
                                                            ifelse(input$performance == "cone", "Three-Cone Drill (sec)", input$performance))))))
    
    fig <- plot_ly(data, x = ~get(input$physical), y = ~get(input$performance), type = 'scatter', mode = 'markers',
                   color = ~pos, colors = "Set1", text = ~paste("Player:", player_name, "<br>Position:", pos)) %>%
      layout(
        title = list(text = paste(physical_label, "vs", performance_label, "by Position")),
        xaxis = list(title = physical_label),
        yaxis = list(title = performance_label)
      )
    
    fig
  })
}

shinyApp(ui = ui, server = server)

