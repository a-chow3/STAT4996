# UVA EDA Dashboard
# Created By: Adam Chow, Jaelyn Do, Rhiannon Staley

# Shiny App for Pitcher Analysis Report
library(shiny)
library(plotly)
library(dplyr)
library(DT)

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


# Define UI
ui <- fluidPage(
  titlePanel("UVA Pitching Dashboard"),
  
  # Simplified inputs at the top
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                accept = c(".csv")),
      uiOutput("pitcher_selector"),
      width = 3
    ),
    
    mainPanel(
      
      # Results Section
      h2("Pitch Distribution"),
      plotlyOutput("pitch_distribution_plot"),
      
      h2("Results/Splits"),
      dataTableOutput("split_table"),
      
      hr(),
      
      # Metrics Section
      h2("Pitch Metrics"),
      dataTableOutput("metrics_table"),
      
      hr(),
      
      # Movement Profile Section
      h2("Movement Profile"),
      plotlyOutput("movement_profile_plot"),
      
      hr(),
      
      # Release Point Section
      h2("Release Point Profile"),
      plotlyOutput("release_point_3d_plot", height = "600px")
      
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value for storing uploaded and processed data
  data <- reactive({
    req(input$file)
    
    # Read the CSV file
    raw_data <- read.csv(input$file$datapath)
    
    # Process the data with all transformations
    processed_data <- raw_data %>%
      mutate(TaggedPitchType = str_replace_all(TaggedPitchType, "ChangeUp", "Changeup")) %>%
      filter(TaggedPitchType != "Undefined") %>%
      mutate(EffectVelo = 1.7 * (Extension - 6.3) + RelSpeed) %>%
      mutate(Count = paste0(Balls, "-", Strikes)) %>%
      mutate(
        RelSpeed = round(RelSpeed, 2),
        EffectVelo = round(EffectVelo, 2),
        VertApprAngle = round(VertApprAngle, 2),
        InducedVertBreak = round(InducedVertBreak, 1),
        HorzBreak = round(HorzBreak, 1),
        SpinRate = round(SpinRate, 0)
      ) %>%
      mutate(Zone = mapply(determine_zone, PlateLocSide, PlateLocHeight))
    
    return(processed_data)
  })
  
  # Dynamic pitcher selector based on uploaded data
  output$pitcher_selector <- renderUI({
    req(data())
    selectInput("pitcher", "Select Pitcher:",
                choices = unique(data()$Pitcher))
  })
  
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
  
  # Movement Profile Plot
  output$movement_profile_plot <- renderPlotly({
    req(data(), input$pitcher)
    
    pitcher_data <- data() %>% 
      filter(Pitcher == input$pitcher)
    
    hover_text <- paste(
      "Pitch Type: ", pitcher_data$TaggedPitchType, "<br>",
      "Velocity (MPH): ", round(pitcher_data$RelSpeed, 1), "<br>",
      "Spin Rate (RPM): ", round(pitcher_data$SpinRate, 0), "<br>",
      "Vertical Break: ", round(pitcher_data$InducedVertBreak, 1), "<br>",
      "Horizontal Break: ", round(pitcher_data$HorzBreak, 1), "<br>",
      "Vertical Approach Angle: ", round(pitcher_data$VertApprAngle, 1)
    )
    
    plot_ly(pitcher_data, x = ~HorzBreak, y = ~InducedVertBreak, 
            type = 'scatter', mode = 'markers',
            color = ~TaggedPitchType, colors = pitch_colors,
            marker = list(size = 10, line = list(color = 'black', width = 1)),
            text = hover_text, hoverinfo = 'text') %>%
      layout(
        xaxis = list(title = "Horizontal Break (inches)", 
                     zeroline = TRUE, zerolinecolor = 'black', zerolinewidth = 2),
        yaxis = list(title = "Vertical Break (inches)", 
                     zeroline = TRUE, zerolinecolor = 'black', zerolinewidth = 2)
      )
  })
  
  # Release Point 3D Plot
  output$release_point_3d_plot <- renderPlotly({
    req(data(), input$pitcher)
    
    pitcher_data <- data() %>% 
      filter(Pitcher == input$pitcher)
    
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
        )
      )
  })
  
  # Metrics Table
  output$metrics_table <- renderDataTable({
    req(data(), input$pitcher)
    
    filtered_data <- data() %>%
      filter(Pitcher == input$pitcher)
    
    table <- filtered_data %>%
      group_by(TaggedPitchType) %>%
      summarize(
        `Velocity (mph)` = round(mean(RelSpeed, na.rm=TRUE), 1),
        `Effective Velocity` = round(mean(EffectVelo, na.rm=TRUE), 1),
        `Spin Rate (rpm)` = round(mean(SpinRate, na.rm=TRUE), 0),
        `Vertical Break` = round(mean(InducedVertBreak, na.rm=TRUE), 1),
        `Horizontal Break` = round(mean(HorzBreak, na.rm=TRUE), 1),
        `Vertical Approach` = round(mean(VertApprAngle, na.rm=TRUE), 2),
        `Horizontal Approach` = round(mean(HorzApprAngle, na.rm=TRUE), 2),
        `Avg Exit Velo` = round(mean(ExitSpeed, na.rm = TRUE), 1)
      )
    
    datatable(table, 
              options = list(dom = 't',
                             scrollX = TRUE,
                             pageLength = 50),
              rownames = FALSE)
  })
  
  # Results Table
  output$split_table <- renderDataTable({
    req(data(), input$pitcher)
    
    filtered_data <- data() %>%
      filter(Pitcher == input$pitcher)
    
    table <- filtered_data %>%
      group_by(TaggedPitchType) %>%
      summarize(
        `Total Pitches` = n(),
        `Home Runs` = sum(PlayResult == "HomeRun", na.rm = TRUE),
        `Total Bases` = sum(PlayResult == "Single") + 
          2 * sum(PlayResult == "Double") + 
          3 * sum(PlayResult == "Triple") + 
          4 * sum(PlayResult == "HomeRun"),
        `Avg Exit Velo` = round(mean(ExitSpeed, na.rm = TRUE), 1)
      ) %>%
      select(`TaggedPitchType`, `Total Pitches`, `Total Bases`, `Avg Exit Velo`)
    
    datatable(table, 
              options = list(dom = 't',
                             scrollX = TRUE,
                             pageLength = 50),
              rownames = FALSE)
  })
  
  # Pitch Distribution Plot
  output$pitch_distribution_plot <- renderPlotly({
    req(data(), input$pitcher)
    
    pitch_distribution <- data() %>%
      filter(Pitcher == input$pitcher) %>%
      group_by(TaggedPitchType) %>%
      summarize(Count = n()) %>%
      mutate(Percentage = round((Count / sum(Count)) * 100, 1))
    
    plot_ly(
      data = pitch_distribution,
      x = ~Percentage,
      y = ~reorder(TaggedPitchType, Percentage),
      type = 'bar',
      orientation = 'h',
      text = ~paste0(Percentage, "%"),
      marker = list(color = ~case_when(
        TaggedPitchType == "Fastball" ~ "red",
        TaggedPitchType == "Slider" ~ "gold",
        TaggedPitchType == "Changeup" ~ "green",
        TaggedPitchType == "Curveball" ~ "cyan",
        TaggedPitchType == "Sinker" ~ "coral",
        TaggedPitchType == "Cutter" ~ "maroon",
        TaggedPitchType == "Splitter" ~ "blue",
        TaggedPitchType == "Sweeper" ~ "purple",
        TRUE ~ "gray"
      ))
    ) %>%
      layout(
        xaxis = list(title = "Percentage of Pitches Thrown"),
        yaxis = list(title = "Pitch Type"),
        margin = list(l = 100)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


