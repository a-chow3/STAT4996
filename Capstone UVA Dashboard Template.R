# UVA EDA Dashboard
# Created By: Adam Chow, Jaelyn Do, Rhiannon Staley

library(tidyverse)
library(shiny) 
library(plotly)
library(reactable)
library(ggplot2)
library(DT)



# Import Singular Game Data
setwd("/Users/adamchow/Library/CloudStorage/Box-Box/UVA/Fall 2024 Classes/STAT 4996/")

game <- read_csv("filtered_uva_games.csv")

# Selective Data Cleaning ----
unique(game$Zone)

## Change ChangeUp to Changeup
game <- game %>%
  mutate(TaggedPitchType = str_replace_all(TaggedPitchType, "ChangeUp", "Changeup")) %>%
  filter(TaggedPitchType != "Undefined")
  

## Create EffectVelo and Count Column + Round Numeric Columns
game <- game %>%
  mutate(EffectVelo = 1.7 * (Extension - 6.3) + RelSpeed) %>%
  mutate(Count = paste0(Balls, "-", Strikes)) %>%
  mutate(
    RelSpeed = round(RelSpeed, 2),
    EffectVelo = round(EffectVelo, 2),
    VertApprAngle = round(VertApprAngle, 2),
    InducedVertBreak = round(InducedVertBreak, 1),
    HorzBreak = round(HorzBreak, 1),
    SpinRate = round(SpinRate, 0)
  )

## Create Zone Column
determine_zone <- function(x, y) {
  w <- 17/6
  x <- x * 12
  y <- y * 12

  between <- function(val, lower, upper) {
    val >= lower & val <= upper
  }
  
  if (between(x, -8.5, -w) & between(y, 34, 42)) {
    return(1)
  } else if (between(x, -w, w) & between(y, 34, 42)) {
    return(2)
  } else if (between(x, w, 8.5) & between(y, 34, 42)) {
    return(3)
  } else if (between(x, -8.5, -w) & between(y, 26, 34)) {
    return(4)
  } else if (between(x, -w, w) & between(y, 26, 34)) {
    return(5)
  } else if (between(x, w, 8.5) & between(y, 26, 34)) {
    return(6)
  } else if (between(x, -8.5, -w) & between(y, 18, 26)) {
    return(7)
  } else if (between(x, -w, w) & between(y, 18, 26)) {
    return(8)
  } else if (between(x, w, 8.5) & between(y, 18, 26)) {
    return(9)
  } else if (x < 0 & y >= 30) {
    return(11)
  } else if (x >= 0 & y >= 30) {
    return(12)
  } else if (x < 0 & y >= -20 & y < 30) {
    return(13)
  } else if (x >= 0 & y >= -20 & y < 30) {
    return(14)
  } else {
    return(NA)
  }
}

game <- game %>%
  mutate(Zone = mapply(determine_zone, PlateLocSide, PlateLocHeight))

# Create Heat DF
heat <- game %>% 
  group_by(Pitcher,Count, Zone) %>%
  summarise(
    total = n(),
    putaway = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging")),
    putaway_rate = round(putaway / total, 2),
    # Hard Hit Balls
    hhb = sum(ExitSpeed > 87),
    # In Play Avg EV
    in_ev = mean(ExitSpeed),
    .groups = 'drop'
  )



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
                 plotlyOutput("movement_profile_plot")
               )
             )
    ),
    tabPanel("Release Point Profile",
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
                 plotlyOutput("release_point_3d_plot", width = "700px", height = "700px")
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
    ),
    tabPanel("Count Heat Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("pitcher", "Select Pitcher:", 
                             choices = unique(game$Pitcher)),
                 selectInput("pitch_type", "Select Pitch Type:",
                             choices = c("All", unique(game$TaggedPitchType))),
                 selectInput("batter", "Select Batter Handedness:",
                             choices = c("All", unique(game$BatterSide))),
                 selectInput("count", "Select Count:",
                             choices = c("All", unique(game$Count))),
                 selectInput("zone", "Select Zone:",
                             choices = c("All", sort(unique(game$Zone))))
               ),
               mainPanel(
                 plotlyOutput("count_plot"),
                 dataTableOutput("pitch_metrics_table")
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

  
  # Second Tab ---- Release Point Profile
  
  # Reactive expression for filtered data
  release_data <- reactive({
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
  
  
  output$release_point_3d_plot <- renderPlotly({
    pitcher_r_data <- release_data()
    
    hover_text <- paste(
      "Pitch Type: ", pitcher_r_data$TaggedPitchType, "<br>",
      "Release Side: ", round(pitcher_r_data$RelSide, 2), "<br>",
      "Release Height: ", round(pitcher_r_data$RelHeight, 2), "<br>",
      "Extension: ", round(pitcher_r_data$Extension, 2), "<br>",
      "Velocity (MPH): ", round(pitcher_r_data$RelSpeed, 1), "<br>", 
      "Effective Velocity: ", round(pitcher_r_data$EffectVelo, 1)
    )
    
    plot_ly(pitcher_r_data, 
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
  
  # Third Tab ---- Outcomes
  
  
  # Fourth Slide ---- Strike Zone
  
  # Reactive expression to filter the data by selected pitcher
  zone_data <- reactive({
    zone <- game %>% 
      filter(Pitcher == input$pitcher)
    
    if (input$pitch_type != "All") {
      zone <- zone %>% filter(TaggedPitchType == input$pitch_type)
    }
    
    zone
  })
  
  # Render the Plotly Strike Zone Plot
  output$strikezone_plot <- renderPlotly({
    # Get the filtered data for the selected pitcher
    strikezone_data <- zone_data()
    
    # Create the Plotly strike zone graph
    all_pitches_graph <- ggplot(strikezone_data) +
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
  
  # Fifth Tab - Count Heat Map
  
  # Filter data based on selected pitcher
  heat_data <- reactive({
    heat <- game %>% 
      filter(Pitcher == input$pitcher) %>%
      group_by(Pitcher,Count, Zone, TaggedPitchType, BatterSide) %>%
      summarise(
        total = n(),
        putaway = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging")),
        putaway_rate = round(putaway / total, 2),
        # Hard Hit Balls
        hhb = sum(ExitSpeed > 87),
        # In Play Avg EV
        in_ev = mean(ExitSpeed),
        .groups = 'drop'
      )
    
    if (input$pitch_type != "All") {
      heat <- heat %>% filter(TaggedPitchType == input$pitch_type)
    }
    
    if (input$batter != "All") {
      heat <- heat %>% filter(BatterSide == input$batter)
    }
    
    heat
  })
  
  # Update pitch type choices based on selected pitcher
  observeEvent(input$pitcher, {
    pitch_types <- c("All", unique(game$TaggedPitchType[game$Pitcher == input$pitcher]))
    updateSelectInput(session, "pitch_type", choices = pitch_types)
  })
  
  # Create the heatmap plot
  output$count_plot <- renderPlotly({
    
    hover_text <- paste(
      "Count: ", heat$Count, "<br>",
      "Zone: ", heat$Zone, "<br>",
      "Total Pitches: ", heat$total, "<br>",
      "Put-Away Rate: ", (heat$putaway_rate), "<br>",
      "Hard Hit Balls: ", (heat$hhb), "<br>",
      "Avg EV: ", (heat$in_ev)
    )
    
    plot_ly(heat_data(), 
            x = ~Zone, 
            y = ~Count, 
            z = ~putaway_rate, 
            type = "heatmap",
            colorscale = list(c(0, "blue"), c(0.5, "white"), c(1, "red")),
            zmin = 0, 
            zmax = 1,
            showlegend = FALSE,
            text = hover_text, hoverinfo = 'text') %>%
      layout(
        xaxis = list(title = "Zone", side = "top",
                     tickvals = 1:14, ticktext = 1:14,
                     showgrid = FALSE),
        yaxis = list(title = "Count", autorange = "reversed",
                     showgrid = FALSE),
        
        shapes = list(
          # Vertical lines dividing the zones
          list(type = "line", x0 = 0.5, x1 = 0.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 1.5, x1 = 1.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 2.5, x1 = 2.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 3.5, x1 = 3.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 4.5, x1 = 4.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 5.5, x1 = 5.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 6.5, x1 = 6.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 7.5, x1 = 7.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 8.5, x1 = 8.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 9.5, x1 = 9.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 10.5, x1 = 10.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 11.5, x1 = 11.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 12.5, x1 = 12.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 13.5, x1 = 13.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 14.5, x1 = 14.5, y0 = -.5, y1 = 11.5, line = list(color = "black", width = 1)),
          
          # Horizontal lines dividing the counts
          list(type = "line", x0 = 0, x1 = 14.5, y0 = -.5, y1 = -.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = .5, y1 = .5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 1.5, y1 = 1.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 2.5, y1 = 2.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 3.5, y1 = 3.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 4.5, y1 = 4.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 5.5, y1 = 5.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 6.5, y1 = 6.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 7.5, y1 = 7.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 8.5, y1 = 8.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 9.5, y1 = 9.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 10.5, y1 = 10.5, line = list(color = "black", width = 1)),
          list(type = "line", x0 = 0, x1 = 14.5, y0 = 11.5, y1 = 11.5, line = list(color = "black", width = 1))
          
        )
      )
  })
  
  # Create Reactive for Pitch Metrics Table 
  table_data <- reactive({
    tab <- game %>%
      filter(Pitcher == input$pitcher)
    
    if (input$pitch_type != "All") {
      tab <- tab %>% filter(TaggedPitchType == input$pitch_type)
    }
    
    if (input$batter != "All") {
      tab <- tab %>% filter(BatterSide == input$batter)
    }
    
    if (input$count != "All") {
      tab <- tab %>% filter(Count == input$count)
    }
    
    if (input$zone != "All") {
      tab <- tab %>% filter(Zone == input$zone)
    }
    
    tab
  })
  
  # Update pitch type choices based on selected pitcher
  observeEvent(input$pitcher, {
    pitch_types <- c("All", unique(game$TaggedPitchType[game$Pitcher == input$pitcher]))
    updateSelectInput(session, "pitch_type", choices = pitch_types)
  })
  
  # Pitch Metrics Table
  output$pitch_metrics_table <- renderDT({
    table_data() %>%
      select(TaggedPitchType, RelSpeed, EffectVelo, SpinRate, Tilt, 
             InducedVertBreak, HorzBreak, VertApprAngle)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
