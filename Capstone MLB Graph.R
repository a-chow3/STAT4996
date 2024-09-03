library(tidyverse)
library(shiny) 
library(plotly)


war_values <- c(
  3.1,-1.1,4.2,0.7,2.6,
  0.2,1.0,1.7,2.0,3.3,
  1.7,2.1,2.2,0.3,-1.2,
  2.0,3.1,0.6,-0.5,2.2,
  2.0,3.9,1.7,2.3,1.8,
  5.4,-0.4,3.0,1.4,2.4,
  1.8,0.1,0.8,2.8,2.8,
  2.4,1.7,0.1,0.6,1.5,
  5.5,1.1,1.2,-1.8,1.1,
  2.9,2.3,-0.5,2.8,2.7,
  0.5,3.2,1.9,0,1.8,
  1.8,1.1,2.5,3.7,0,
  1.8,4.5,-1.4,2.4,0.7,
  2.2,0.3,2.5,0.7,-1.3,
  0.8,2.1,1.5,3.5,1.8,
  1.7,2.9,0.8,0.4,1.6,
  2.0,1.0,0.9,0.8,1.8,
  1.3,3.9,2.2,1.3,1.5,
  1.0,2.1,5.1,1.6,1.3,
  3.2,4.6,5.4,2.1,1.2,
  2.1,2.3,1.5,2.3,3.3,
  2.4,1.1,2.8,2.6,1.0,
  3.6,4.4,0.7,2.1
)

left <- c(
  1,1,1,0,0,
  0,0,0,0,0,
  0,0,0,0,0,
  0,0,0,1,1,
  0,1,0,0,1,
  0,0,0,0,1,
  0,0,0,0,0,
  0,1,1,0,0,
  0,1,1,0,0,
  0,1,1,0,0,
  1,0,0,0,0,
  0,0,0,0,0,
  0,0,0,1,0,
  0,0,0,0,1,
  0,0,0,0,0,
  0,0,1,1,0,
  0,0,0,1,0,
  1,1,0,0,1,
  1,0,1,1,0,
  0,0,1,0,0,
  1,0,0,0,0,
  1,0,1,0,0,
  0,0,0,0
)

# Import 2024 MLB Pitching Data
setwd("/Users/adamchow/Library/CloudStorage/Box-Box/UVA/Fall 2024 Classes/STAT 4996/")

pitches <- read_csv("mlb_pitch_data.csv")
expected <- read_csv("mlb_exp_stats.csv")

mlb2024_df <- left_join(expected, pitches, by = "player_id")

mlb2024_df <- mlb2024_df %>%
  select(-pa, -player_id, -year.x, -year.y, -bip, -ba, -est_ba,
         -est_ba_minus_ba_diff, -slg, -est_slg,
         -est_slg_minus_slg_diff, -`last_name, first_name.y`) %>%
  rename(name = `last_name, first_name.x`,
         ev_avg = `exit_velocity_avg`,
         fb_percent = `n_ff_formatted`,
         fb_avg_velo = `ff_avg_speed`,
         fb_avg_spin = `ff_avg_spin`,
         fb_avg_hor = `ff_avg_break_x`,
         fb_avg_vert = `ff_avg_break_z`,
         sl_percent = `n_sl_formatted`,
         sl_avg_velo = `sl_avg_speed`,
         sl_avg_spin = `sl_avg_spin`,
         sl_avg_hor = `sl_avg_break_x`,
         sl_avg_vert = `sl_avg_break_z`,
         ch_percent = `n_ch_formatted`,
         ch_avg_velo = `ch_avg_speed`,
         ch_avg_spin = `ch_avg_spin`,
         ch_avg_hor = `ch_avg_break_x`,
         ch_avg_vert = `ch_avg_break_z`,
         cb_percent = `n_cu_formatted`,
         cb_avg_velo = `cu_avg_speed`,
         cb_avg_spin = `cu_avg_spin`,
         cb_avg_hor = `cu_avg_break_x`,
         cb_avg_vert = `cu_avg_break_z`,
         sk_percent = `n_si_formatted`,
         sk_avg_velo = `si_avg_speed`,
         sk_avg_spin = `si_avg_spin`,
         sk_avg_hor = `si_avg_break_x`,
         sk_avg_vert = `si_avg_break_z`,
         cu_percent = `n_fc_formatted`,
         cu_avg_velo = `fc_avg_speed`,
         cu_avg_spin = `fc_avg_spin`,
         cu_avg_hor = `fc_avg_break_x`,
         cu_avg_vert = `fc_avg_break_z`,
         sp_percent = `n_fs_formatted`,
         sp_avg_velo = `fs_avg_speed`,
         sp_avg_spin = `fs_avg_spin`,
         sp_avg_hor = `fs_avg_break_x`,
         sp_avg_vert = `fs_avg_break_z`,
         sw_percent = `n_st_formatted`,
         sw_avg_velo = `st_avg_speed`,
         sw_avg_spin = `st_avg_spin`,
         sw_avg_hor = `st_avg_break_x`,
         sw_avg_vert = `st_avg_break_z`) %>%
  mutate(war = NA,
         is_left = NA) %>%
  select(name, is_left, war, everything()) %>%
  slice(1:114) %>%
  arrange(name)

mlb2024_df$war[1:length(war_values)] <- war_values
mlb2024_df$is_left[1:length(left)] <- left

last_names <- c("Burnes", "Brown", "Cease", "Crochet", "Flaherty", 
                "Fried", "Gilbert", "Glasnow", "Greene", "King", 
                "Martinez", "Ragans", "Ryan", "Sale", "Skenes", 
                "Skubal", "Steele", "Wheeler")

# Extract the last names from the "name" column
mlb2024_df$last_name <- str_extract(mlb2024_df$name, "^[^,]+")

# Subset the dataframe based on the last names
elite_pitchers <- mlb2024_df %>%
  filter(last_name %in% last_names | row_number() == 105) %>%
  select(-last_name) %>%
  mutate(
    height = c(75, 74, 74, 78, 76, 76, 78, 80, 77, 75, 73, 76, 74, 78, 78, 75, 74, 73, 76),
    weight = c(245, 220, 200, 245, 225, 190, 215, 225, 242, 210, 200, 190, 205, 180, 235, 240, 205, 217, 195)
  )

clusters <- c(
  "Brown, Hunter" = 1, "Burnes, Corbin" = 4, "Cease, Dylan" = 4, 
  "Crochet, Garrett" = 2, "Flaherty, Jack" = 1, "Fried, Max" = 3, 
  "Gilbert, Logan" = 2, "Glasnow, Tyler" = 2, "Greene, Hunter" = 1, 
  "King, Michael" = 4, "Martinez, Nick" = 4, "Ragans, Cole" = 3, 
  "Ryan, Joe" = 4, "Sale, Chris" = 3, "Skenes, Paul" = 2, 
  "Skubal, Tarik" = 1, "Steele, Justin" = 4, "Suárez, Ranger" = 4, 
  "Wheeler, Zack" = 3
)

elite_pitches <- elite_pitchers %>%
  select(-3, -4, -5, -6, -7, -8, -9, -10) %>%
  mutate(cluster = clusters[name]) %>%
  column_to_rownames(var = "name") 

# Convert Vertical Break to IVB
elite_pitches <- elite_pitches %>%
  mutate(fb_avg_vert = fb_avg_vert + (523 / fb_avg_velo)^2) %>%
  mutate(sl_avg_vert = sl_avg_vert + (523 / sl_avg_velo)^2) %>%
  mutate(ch_avg_vert = ch_avg_vert + (523 / ch_avg_velo)^2) %>%
  mutate(cb_avg_vert = cb_avg_vert + (523 / cb_avg_velo)^2) %>%
  mutate(sk_avg_vert = sk_avg_vert + (523 / sk_avg_velo)^2) %>%
  mutate(cu_avg_vert = cu_avg_vert + (523 / cu_avg_velo)^2) %>%
  mutate(sp_avg_vert = sp_avg_vert + (523 / sp_avg_velo)^2) %>%
  mutate(sw_avg_vert = sw_avg_vert + (523 / sw_avg_velo)^2)

# -------------------------------

library(shiny)
library(plotly)
library(dplyr)

# Sample UI for the Shiny app
ui <- fluidPage(
  titlePanel("Elite Pitchers Analysis"),
  
  # Tabbed header
  tabsetPanel(
    tabPanel("Pitch Distribution",
             sidebarLayout(
               sidebarPanel(
                 selectInput("pitcher", "Select Pitcher:", choices = rownames(elite_pitches)),
                 selectInput("stat", "Select Stat:", 
                             choices = c("Percent Thrown", "Velocity", "Spin Rate"))
               ),
               mainPanel(
                 plotlyOutput("pitch_dist_plot")
               )
             )
    ),
    tabPanel("Movement Profile",
             sidebarLayout(
               sidebarPanel(
                 selectInput("pitcher_movement", "Select Pitcher:", choices = rownames(elite_pitches))
               ),
               mainPanel(
                 plotlyOutput("movement_profile_plot")
               )
             )
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output) {
  
  # Define color palette for pitch types
  pitch_colors <- c(
    "FB" = "red", "SL" = "yellow", "CH" = "green", "CB" = "lightblue",
    "SK" = "orange", "CU" = "brown", "SP" = "purple", "SW" = "pink"
  )
  
  # Define full names for pitch types
  pitch_names <- c(
    "FB" = "4-Seam Fastball", "SL" = "Slider", "CH" = "Changeup", "CB" = "Curveball",
    "SK" = "Sinker", "CU" = "Cutter", "SP" = "Splitter", "SW" = "Sweeper"
  )
  
  output$pitch_dist_plot <- renderPlotly({
    selected_pitcher <- elite_pitches[input$pitcher, ]
    
    # Determine which stat to plot based on user input
    if (input$stat == "Percent Thrown") {
      y_data <- c(selected_pitcher$fb_percent, selected_pitcher$sl_percent, selected_pitcher$ch_percent,
                  selected_pitcher$cb_percent, selected_pitcher$sk_percent, selected_pitcher$cu_percent,
                  selected_pitcher$sp_percent, selected_pitcher$sw_percent)
      y_label <- "Percent Thrown"
      y_range <- NULL
    } else if (input$stat == "Velocity") {
      y_data <- c(selected_pitcher$fb_avg_velo, selected_pitcher$sl_avg_velo, selected_pitcher$ch_avg_velo,
                  selected_pitcher$cb_avg_velo, selected_pitcher$sk_avg_velo, selected_pitcher$cu_avg_velo,
                  selected_pitcher$sp_avg_velo, selected_pitcher$sw_avg_velo)
      y_label <- "Velocity (mph)"
      y_range <- c(70, 100)
    } else if (input$stat == "Spin Rate") {
      y_data <- c(selected_pitcher$fb_avg_spin, selected_pitcher$sl_avg_spin, selected_pitcher$ch_avg_spin,
                  selected_pitcher$cb_avg_spin, selected_pitcher$sk_avg_spin, selected_pitcher$cu_avg_spin,
                  selected_pitcher$sp_avg_spin, selected_pitcher$sw_avg_spin)
      y_label <- "Spin Rate (rpm)"
      y_range <- c(1000, 3000)
    }
    
    # Create a data frame for plotting
    pitch_data <- data.frame(
      Pitch = c("FB", "SL", "CH", "CB", "SK", "CU", "SP", "SW"),
      Stat = y_data
    )
    
    # Remove rows with NA values
    pitch_data <- pitch_data[complete.cases(pitch_data), ]
    
    # Plot the selected stat for the pitcher's arsenal
    p <- plot_ly(pitch_data, x = ~Pitch, y = ~Stat, type = 'bar', 
                 marker = list(color = pitch_colors[pitch_data$Pitch])) %>%
      add_text(text = ~round(Stat, 1), textposition = 'outside', 
               textfont = list(color = 'black', size = 12)) %>%
      layout(
        title = paste(input$stat, "Distribution"),
        yaxis = list(title = y_label)
      )
    
    # Apply y-axis range if specified
    if (!is.null(y_range)) {
      p <- p %>% layout(yaxis = list(range = y_range))
    }
    
    p
  })
  
  output$movement_profile_plot <- renderPlotly({
    selected_pitcher <- elite_pitches[input$pitcher_movement, ]
    
    # Create a data frame for plotting movement profile
    movement_data <- data.frame(
      Pitch = c("FB", "SL", "CH", "CB", "SK", "CU", "SP", "SW"),
      Horizontal = c(selected_pitcher$fb_avg_hor, selected_pitcher$sl_avg_hor, selected_pitcher$ch_avg_hor,
                     selected_pitcher$cb_avg_hor, selected_pitcher$sk_avg_hor, selected_pitcher$cu_avg_hor,
                     selected_pitcher$sp_avg_hor, selected_pitcher$sw_avg_hor),
      Vertical = c(selected_pitcher$fb_avg_vert, selected_pitcher$sl_avg_vert, selected_pitcher$ch_avg_vert,
                   selected_pitcher$cb_avg_vert, selected_pitcher$sk_avg_vert, selected_pitcher$cu_avg_vert,
                   selected_pitcher$sp_avg_vert, selected_pitcher$sw_avg_vert),
      Velocity = c(selected_pitcher$fb_avg_velo, selected_pitcher$sl_avg_velo, selected_pitcher$ch_avg_velo,
                   selected_pitcher$cb_avg_velo, selected_pitcher$sk_avg_velo, selected_pitcher$cu_avg_velo,
                   selected_pitcher$sp_avg_velo, selected_pitcher$sw_avg_velo),
      SpinRate = c(selected_pitcher$fb_avg_spin, selected_pitcher$sl_avg_spin, selected_pitcher$ch_avg_spin,
                   selected_pitcher$cb_avg_spin, selected_pitcher$sk_avg_spin, selected_pitcher$cu_avg_spin,
                   selected_pitcher$sp_avg_spin, selected_pitcher$sw_avg_spin)
    )
    
    # Remove rows with NA values
    movement_data <- movement_data[complete.cases(movement_data), ]
    
    # Calculate the range for both axes
    x_range <- range(movement_data$Horizontal)
    y_range <- range(movement_data$Vertical)
    
    # Create hover text
    hover_text <- paste(
      pitch_names[movement_data$Pitch], "<br>",
      "Velocity (MPH): ", round(movement_data$Velocity, 1), "<br>",
      "Spin Rate (RPM): ", round(movement_data$SpinRate, 0), "<br>",
      "Horizontal: ", round(movement_data$Horizontal, 1), "<br>",
      "Vertical: ", round(movement_data$Vertical, 1)
    )
    
    # Plot the movement profile
    plot_ly(movement_data, x = ~Horizontal, y = ~Vertical, type = 'scatter', mode = 'markers',
            marker = list(size = 12, color = pitch_colors[movement_data$Pitch], 
                          line = list(color = 'black', width = 1)),
            text = hover_text, hoverinfo = 'text') %>%
      layout(
        title = "Movement Profile (Hitter POV)",
        xaxis = list(title = "Horizontal Movement (inches)", 
                     zeroline = TRUE, zerolinecolor = 'black', zerolinewidth = 2,
                     range = c(-25, 25)),
        yaxis = list(title = "Vertical Movement (inches)", 
                     zeroline = TRUE, zerolinecolor = 'black', zerolinewidth = 2,
                     range = c(-25, 25)),
        shapes = list(
          list(type = "line", x0 = -25, x1 = 25, y0 = 0, y1 = 0,
               line = list(color = "black", width = 2)),
          list(type = "line", x0 = 0, x1 = 0, y0 = -25, y1 = 25,
               line = list(color = "black", width = 2))
        )
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)





