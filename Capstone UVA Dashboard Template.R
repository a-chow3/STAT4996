# UVA EDA Dashboard
# Created By: Adam Chow, Jaelyn Do, Rhiannon Staley

library(tidyverse)
library(shiny) 
library(plotly)
library(reactable)
library(ggplot2)



# Import Singular Game Data
setwd("/Users/adamchow/Library/CloudStorage/Box-Box/UVA/Fall 2024 Classes/STAT 4996/")

game <- read_csv("filtered_uva_games.csv")

# Selective Data Cleaning ----
unique(game$TaggedPitchType)

## Change ChangeUp to Changeup
game <- game %>%
  mutate(TaggedPitchType = str_replace_all(TaggedPitchType, "ChangeUp", "Changeup"))

## Create EffectVelo Column
game <- game %>%
  mutate(EffectVelo = 1.7 * (Extension - 6.3) + RelSpeed)


# Shiny App --------

# Define UI
ui <- fluidPage(
  titlePanel("Pitcher Game Day Analysis"),
  
  # Tabbed header
  tabsetPanel(
    tabPanel("Movement Profile",
             sidebarLayout(
               sidebarPanel(
                 selectInput("pitcher", "Select Pitcher:", 
                             choices = unique(game$Pitcher)),
                 selectInput("pitch_type", "Select Pitch Type:",
                             choices = c("All", unique(game$TaggedPitchType))),
                 selectInput("play_result", "Select Play Result:",
                             choices = c("All", unique(game$PlayResult)))
               ),
               mainPanel(
                 plotlyOutput("movement_profile_plot"),
                 plotlyOutput("release_point_3d_plot")
               )
             )
    ),
    tabPanel("Outcomes",
             #sidebarLayout(
             #  sidebarPanel(
              #   selectInput("pitcher_outcomes", "Select Pitcher:", 
               #              choices = unique(game$Pitcher))
               #),
               #mainPanel(
                # reactableOutput("outcomes_table")
               #)
             #)
    ),
    tabPanel("Strike Zone",
             sidebarLayout(
               sidebarPanel(
                 selectInput("pitcher", "Select Pitcher:", 
                             choices = unique(game$Pitcher))
               ),
               mainPanel(
                 plotlyOutput("strikezone_plot")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Define color palette for pitch types
  pitch_colors <- c(
    "Fastball" = "firebrick1", 
    "Slider" = "gold1", 
    "Changeup" = "springgreen2", 
    "Curveball" = "cyan",
    "Sinker" = "coral", 
    "Cutter" = "indianred4", 
    "Splitter" = "dodgerblue2", 
    "Sweeper" = "mediumorchid1"
  )
  
  # First Tab ---- Movement Profile
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    data <- game %>% 
      filter(Pitcher == input$pitcher)
    
    if (input$pitch_type != "All") {
      data <- data %>% filter(TaggedPitchType == input$pitch_type)
    }
    
    if (input$play_result != "All") {
      data <- data %>% filter(PlayResult == input$play_result)
    }
    
    data
  })
  
  # Update pitch type choices based on selected pitcher
  observeEvent(input$pitcher, {
    pitch_types <- c("All", unique(game$TaggedPitchType[game$Pitcher == input$pitcher]))
    updateSelectInput(session, "pitch_type", choices = pitch_types)
  })
  
  output$movement_profile_plot <- renderPlotly({
    # Get filtered data
    pitcher_data <- filtered_data()
    
    # Create hover text
    hover_text <- paste(
      "Pitch Type: ", pitcher_data$TaggedPitchType, "<br>",
      "Velocity (MPH): ", round(pitcher_data$RelSpeed, 1), "<br>",
      "Spin Rate (RPM): ", round(pitcher_data$SpinRate, 0), "<br>",
      "Vertical Break: ", round(pitcher_data$InducedVertBreak, 1), "<br>",
      "Horizontal Break: ", round(pitcher_data$HorzBreak, 1), "<br>",
      "Vertical Approach Angle: ", round(pitcher_data$VertApprAngle, 1)
    )
    
    # Create the plot
    plot_ly(pitcher_data, x = ~HorzBreak, y = ~InducedVertBreak, 
            type = 'scatter', mode = 'markers',
            color = ~TaggedPitchType, colors = pitch_colors,
            marker = list(size = 10, line = list(color = 'black', width = 1)),
            text = hover_text, hoverinfo = 'text') %>%
      layout(
        title = paste(input$pitcher, "- Movement Profile"),
        xaxis = list(title = "Horizontal Break (inches)", 
                     zeroline = TRUE, zerolinecolor = 'black', zerolinewidth = 2),
        yaxis = list(title = "Vertical Break (inches)", 
                     zeroline = TRUE, zerolinecolor = 'black', zerolinewidth = 2)
      )
  })
  
  output$release_point_3d_plot <- renderPlotly({
    pitcher_data <- filtered_data()
    
    hover_text <- paste(
      "Pitch Type: ", pitcher_data$TaggedPitchType, "<br>",
      "Release Side: ", round(pitcher_data$RelSide, 2), "<br>",
      "Release Height: ", round(pitcher_data$RelHeight, 2), "<br>",
      "Extension: ", round(pitcher_data$Extension, 2), "<br>",
      "Velocity (MPH): ", round(pitcher_data$RelSpeed, 1), "<br>", 
      "Effective Velocity: ", round(pitcher_data$EffectVelo, 1)
    )
    
    plot_ly(pitcher_data, 
            x = ~RelSide, y = ~RelHeight, z = ~Extension,
            type = 'scatter3d', mode = 'markers',
            color = ~TaggedPitchType, colors = pitch_colors,
            marker = list(size = 5),
            text = hover_text, hoverinfo = 'text') %>%
      layout(
        scene = list(
          xaxis = list(title = "Release Side"),
          yaxis = list(title = "Extension"),
          zaxis = list(title = "Release Height")
        ),
        title = paste(input$pitcher, "- 3D Release Point")
      )
  })
  
  # Second Tab ---- Outcomes
  
  
  # Third Slide ---- 
  
  # Reactive expression to filter the data by selected pitcher
  filtered_data <- reactive({
    data <- game %>% 
      filter(Pitcher == input$pitcher)
    
    if (input$pitch_type != "All") {
      data <- data %>% filter(TaggedPitchType == input$pitch_type)
    }
    
    data
  })
  
  # Render the Plotly Strike Zone Plot
  output$strikezone_plot <- renderPlotly({
    # Get the filtered data for the selected pitcher
    pitcher_data <- filtered_data()
    
    # Create the Plotly strike zone graph
    all_pitches_graph <- ggplot(pitcher_data) +
      geom_rect(aes(xmin=-8.5, xmax=8.5, ymin=18, ymax=42), alpha=0.2, color = "black") +
      geom_rect(aes(xmin=-11.5, xmax=11.5, ymin=15, ymax=45), alpha=0.1, color = "black", linetype = "dashed") +
      geom_point(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, color = TaggedPitchType)) +
      theme_minimal() +
      coord_cartesian(xlim = c(-54, 54), ylim = c(-10, 72)) +
      labs(title = paste("All Pitches - Pitcher's View:", input$pitcher)) +
      scale_color_manual(values = pitch_colors,
                         # labels = c("CH", "CB", "CU", "FS", "SI", "SL", "SP", "TS"),
                         breaks = names(pitch_colors)) +
      facet_wrap(~TaggedPitchType, ncol = 3) +
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = "top",
            plot.title = element_text(size = 13, hjust = 0.5, face = "bold"))
    
    # Convert ggplot object to Plotly object
    # ggplotly(all_pitches_graph)
  })
  
  
  ## Filter by Play Outcome: Whiff/Ball/In play
  
}

# Run the application 
shinyApp(ui = ui, server = server)







