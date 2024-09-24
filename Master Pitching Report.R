library(ggplot2)
library(dplyr)
library(readxl)
library(magick)
library(readr)
library(knitr)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(qpdf)
library(pdftools)
library(scales)
library(ggrepel)

# Read the CSV file into the gamefile data table
gamefile <- read_csv("C:\\Users\\vonah\\OneDrive\\Documents\\UVA Baseball\\UVABaseballDataResources\\2024 Fall Trackman Files\\20240919-DavenportField-Private-2_unverified.csv")

setwd("/Users/adamchow/Library/CloudStorage/Box-Box/UVA/Fall 2024 Classes/STAT 4996/")

gamefile <- read_csv("filtered_uva_games.csv")

gamefile$Date <- as.Date(gamefile$Date, format = '%m/%d/%Y')
gamefile$TaggedPitchType <- ifelse(gamefile$TaggedPitchType == "ChangeUp", "Changeup", gamefile$TaggedPitchType)
gamefile$TaggedPitchType <- ifelse(gamefile$TaggedPitchType == "TwoSeamFastBall", "Two-Seam", gamefile$TaggedPitchType)
gamefile$TaggedPitchType <- ifelse(gamefile$TaggedPitchType == "FourSeamFastBall", "Four-Seam", gamefile$TaggedPitchType)
gamefile$TaggedPitchType <- ifelse(gamefile$TaggedPitchType == "CurveBall", "Curveball", gamefile$TaggedPitchType)
gamefile$TaggedPitchType <- ifelse(gamefile$TaggedPitchType == "Fastball", "Four-Seam", gamefile$TaggedPitchType)

pitchers <- unique(gamefile$Pitcher)

TemplatePageOne <- image_read_pdf("C:/Users/vonah/OneDrive/Documents/USA Baseball Resources/NTIS Pitching Report Page 2.pdf")
TemplatePageTwo <- image_read_pdf("C:/Users/vonah/OneDrive/Documents/USA Baseball Resources/NTIS Pitching Report Page 2.pdf")

USABaseball <- c("Changeup"="#000075", "Curveball"="#42d4f4", "Cutter"="#ffe119", "Four-Seam"="#e6194B", "Sinker"="#f58231", "Slider"="#3cb44b", "Splitter"="brown", "Two-Seam" = "#f032e6")

setwd("C:\\Users\\vonah\\OneDrive\\Documents\\UVA Baseball\\UVABaseballDataResources\\2024 Fall Trackman Files\\Prospect Event Reports")

for (i in 1:length(pitchers)) {

  pitches <- subset(gamefile, Pitcher == pitchers[i])
  pitches$pitchNumber <- as.numeric(row.names(pitches))
  
  # Pitching Line
  BBs <- sum(pitches$KorBB == "Walk")
  Ks <- sum(pitches$KorBB == "Strikeout")
  HBP <- sum(pitches$PitchCall == "HitByPitch")
  Strikes <- sum(pitches$PitchCall == "StrikeCalled" | pitches$PitchCall == "StrikeSwinging" | pitches$PitchCall == "FoulBallFieldable" | pitches$PitchCall == "FoulBallNotFieldable" | pitches$PitchCall == 'InPlay')
  Whiffs <- sum(pitches$PitchCall == "StrikeSwinging", na.rm = TRUE)
  InZone <- subset(pitches, (pitches$PlateLocSide <= 0.7083) & (pitches$PlateLocSide >= -0.7083) &
                     (pitches$PlateLocHeight >= 1.5) & (pitches$PlateLocHeight <= 3.5))
  InZoneSwings <- sum(InZone$PitchCall %in% c('StrikeSwinging', 'InPlay', 'FoulBallFieldable', 'FoulBallNotFieldable'), na.rm = TRUE)
  InZoneWhiffs <- sum(InZone$PitchCall == "StrikeSwinging", na.rm = TRUE)
  Balls <- sum(pitches$PitchCall == "BallCalled" | pitches$PitchCall == "HitByPitch")
  Sac <- sum(pitches$PlayResult == "Sacrifice")
  Singles <- sum(pitches$PlayResult == "Single")
  Doubles <- sum(pitches$PlayResult == "Double")
  Triples <- sum(pitches$PlayResult == "Triple")
  HomeRuns <- sum(pitches$PlayResult == "HomeRun")
  NonHit <- sum(pitches$PlayResult %in% c("Error", "FieldersChoice", "Out"))
  Runs <- sum(pitches$RunsScored, na.rm = TRUE)
  Innings <- sum(pitches$OutsOnPlay, na.rm = TRUE)
  InningsPitched <-round((Innings + Ks)/3, 2)
  ReachedNonHit <- sum(pitches$PlayResult %in% c("Error", "FieldersChoice"))
  

  # Regular Data
  totalPitches <- nrow(pitches)  # Fix: Use nrow() to get the number of rows
  PAs <- BBs + Ks + Sac + Singles + Doubles + Triples + HomeRuns + NonHit + HBP
  ABs <- PAs - BBs - Sac
  Hits <- Singles + Doubles + Triples + HomeRuns
  PitchesPerPA <- totalPitches / PAs  # Fix: Use 'totalPitches' instead of 'nrow(pitches)'
  
  #Pitching Line 2
  BA <- format(round(Hits/ABs, 3), nsmall = 3)
  OBP <- format(round((Hits + BBs + HBP) / (ABs + BBs + HBP + Sac), 3), nsmall = 3)
  SLG <- format(round((Singles + (Doubles*2) + (Triples*3) + (HomeRuns*4)) / ABs, 3), nsmall = 3)
  OPS <- format(round(((Hits + BBs + HBP) / (ABs + BBs + HBP + Sac)) + ((Singles + (Doubles*2) + (Triples*3) + (HomeRuns*4)) / ABs), 3), nsmall = 3)
  KPerc <-paste0(round((Ks / PAs)*100,digits=1),"%",sep="")
  BBPerc <- paste0(round((BBs / PAs)*100,digits=1),"%",sep="")
  K_BBPerc <- paste0(round(((Ks / PAs)*100) - ((BBs / PAs)*100),digits=1),"%",sep="")
  InZoneWhiffPerc <- paste0(round(InZoneWhiffs/InZoneSwings*100,digits=1),"%",sep="")

  tableSlash <- as.data.frame(cbind(pitchers[i], as.character(pitches$Date[1]), totalPitches, InningsPitched, PAs, ABs, Hits, Runs, Singles, Doubles, Triples, HomeRuns, Ks, BBs, HBP, format(PitchesPerPA, digits = 3)))
  colnames(tableSlash) <- c("Pitcher", "Date", "Pitches", "IP", "PA", "AB", "H", "R", "1B", "2B", "3B", "HR", "K", "BB", "HBP", "Pitches Per PA")
  png(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i],"- pitchingSummary.png"), height=1, width=10,units = "in",res = 300, bg = "transparent")
  table1 <- tableGrob(tableSlash, rows = NULL)
  grid.arrange(table1)
  dev.off()
  
  tableSlash2 <- as.data.frame(cbind(BA, OBP, SLG, OPS, KPerc, BBPerc, K_BBPerc, InZoneWhiffPerc))
  colnames(tableSlash2) <- c("BA", "OBP", "SLG", "OPS", "K%", "BB%", "K-BB%", "In Zone Whiff %")
  png(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i],"- pitchingSummaryEXT.png"), height=1, width=10,units = "in",res = 300, bg = "transparent")
  table2 <- tableGrob(tableSlash2, rows = NULL)
  grid.arrange(table2)
  dev.off()

  pitchSummaries <- image_read(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i],"- pitchingSummary.png"))
  PitchingReport0 <- image_composite(TemplatePageOne,pitchSummaries,offset= "+150+300")
  
  pitchSummariesEXT <- image_read(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i],"- pitchingSummaryEXT.png"))
  PitchingReport01 <- image_composite(PitchingReport0,pitchSummariesEXT,offset= "+150+475")

  additional_pitch_stats <- pitches %>%
    group_by(TaggedPitchType) %>%
    summarize(`#` = n(),
              AvgVelo = format(round(mean(RelSpeed, na.rm = T), 1)),
              MaxVelo = format(round(max(RelSpeed), 1)),
              MinVelo = format(round(min(RelSpeed), 1)),
              AvgSpinRate = format(round(mean(SpinRate, na.rm = T), 0)),
              Tilt = format(median(strptime(Tilt, "%H:%M"), na.rm = T), "%H:%M"),
              AvgIVB = format(round(mean(InducedVertBreak, na.rm = T), 1)),
              AvgHB = format(round(mean(HorzBreak, na.rm = T), 1)),
              AvgExtension = format(round(mean(Extension, na.rm = T), 1)),
              StrikePerc = paste0(round((sum(PitchCall == "StrikeCalled" | PitchCall == "StrikeSwinging" | PitchCall == "FoulBallFieldable" | PitchCall == 'FoulBallNotFieldable' | PitchCall == 'InPlay')/n())*100,digits=1),"%",sep=""),
              WhiffPerc = paste0(round((sum(PitchCall == "StrikeSwinging", na.rm = TRUE))/(sum(PitchCall %in% c('StrikeSwinging', 'InPlay', 'FoulBallFieldable', 'FoulBallNotFieldable')))*100,digits=1),"%",sep=""),
              InZoneWhiffPerc = paste0(round((sum((PlateLocSide <= 0.7083) & (PlateLocSide >= -0.7083) &
                                                    (PlateLocHeight >= 1.5) & (PlateLocHeight <= 3.5) &
                                                    (PitchCall == "StrikeSwinging"), na.rm = TRUE))/
                                               (sum((PlateLocSide <= 0.7083) & (PlateLocSide >= -0.7083) & (PlateLocHeight >= 1.5) & 
                                                      (PlateLocHeight <= 3.5) & (PitchCall %in% c('StrikeSwinging', 'InPlay', 'FoulBallFieldable', 'FoulBallNotFieldable')), na.rm = TRUE))*100,digits=1),"%",sep="")) %>%
    arrange(desc(AvgVelo))
  for (x in 1:length(additional_pitch_stats)) {
    if (additional_pitch_stats$WhiffPerc[x] %in% "NaN%") {
      additional_pitch_stats$WhiffPerc[x] <- paste0("All Takes")
    }
    if (additional_pitch_stats$InZoneWhiffPerc[x] %in% "NaN%") {
      additional_pitch_stats$InZoneWhiffPerc[x] <- paste0("All Takes")
    }
  }
  
  colnames(additional_pitch_stats) <- c('Pitch Type', '#', 'Avg Velo', 'Max Velo', 'Min Velo', 'Avg Spin', 'Tilt', 'Avg IVB', 'Avg HB', "Avg Ext.", "Strike %", "Whiff %", "IZ Whiff %")
  png(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i],"- pitchingSummaryTWO.png"),height=2, width=10.5,units = "in",res = 300, bg = "transparent")
  table2 <- tableGrob(additional_pitch_stats, rows = NULL)
  grid.arrange(table2)
  dev.off()

  pitchSummaries2 <- image_read(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i],"- pitchingSummaryTWO.png"))
  PitchingReport1 <- image_composite(PitchingReport01,pitchSummaries2,offset= "+100+725")

  all_pitches_graph <- ggplot(pitches) +
    geom_rect(aes(xmin=-8.5,xmax=8.5,ymin=18,ymax=42),alpha=0.0, color = "black") +
    geom_rect(aes(xmin=-11.5,xmax=11.5,ymin=15,ymax=45),alpha=0.0,color="black",linetype="dashed") +
    geom_point(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, color = TaggedPitchType)) +
    theme_minimal() +
    coord_cartesian(xlim = c(-54, 54), ylim = c(-10, 72)) +
    labs(title = "All Pitches - Pitcher's View") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FS", "SI", "SL", "SP", "TS"),
                       breaks = names(USABaseball)) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"))

  bip_graph <- ggplot(subset(pitches, PitchCall == "InPlay")) +
    geom_rect(aes(xmin=-8.5,xmax=8.5,ymin=18,ymax=42),alpha=0.0, color = "black") +
    geom_rect(aes(xmin=-11.5,xmax=11.5,ymin=15,ymax=45),alpha=0.0,color="black",linetype="dashed") +
    geom_point(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, color = TaggedPitchType)) +
    theme_minimal() +
    coord_cartesian(xlim = c(-54, 54), ylim = c(-10, 72)) +
    labs(title = "Balls in Play - Pitcher's View") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FS", "SI", "SL", "SP", "TS"),
                       breaks = names(USABaseball)) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"))

  swing_miss_graph <- ggplot(subset(pitches, PitchCall == "StrikeSwinging")) +
    geom_rect(aes(xmin=-8.5,xmax=8.5,ymin=18,ymax=42),alpha=0.0, color = "black") +
    geom_rect(aes(xmin=-11.5,xmax=11.5,ymin=15,ymax=45),alpha=0.0,color="black",linetype="dashed") +
    geom_point(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, color = TaggedPitchType)) +
    theme_minimal() +
    coord_cartesian(xlim = c(-54, 54), ylim = c(-10, 72)) +
    labs(title = "Swing & Miss - Pitcher's View") +
    scale_color_manual(values = USABaseball,
                       labels = c("CH", "CB", "CU", "FS", "SI", "SL", "SP", "TS"),
                       breaks = names(USABaseball)) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 13, hjust = 0.5, face = "bold"))

  plots_row <- ggarrange(all_pitches_graph, bip_graph, swing_miss_graph, nrow = 1)
  ggsave(plots_row,file=paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i]," - strikeZone.png"), width=10,height=4,units="in", dpi = 300)
  pitchCharts <- image_read(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i]," - strikeZone.png"))
  PitchingReport2 <- image_composite(PitchingReport1,pitchCharts, offset= "+150+1300")

  image_write(PitchingReport2,path = "page1.pdf",format="pdf",quality=100,density=300)

  movement_plot <- ggplot(subset(pitches), aes(HorzBreak, InducedVertBreak, color = TaggedPitchType, stroke = 1)) +
    geom_point(na.rm = T) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlim(-25,25) +
    ylim(-25,25) +
    xlab("Horizontal Break") +
    ylab("Induced Vertical Break") +
    ggtitle("Break Plot") +
    scale_color_manual(values = USABaseball) +
    theme_minimal() +
    theme(legend.position = "bottom")  +
    guides(color = guide_legend(nrow = 2, title = "Pitch"))

  release_plot <- ggplot(subset(pitches),aes(RelSide, RelHeight, color = TaggedPitchType, stroke = 1)) +
    geom_point(na.rm = T) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlim(-4,4) +
    ylim(0,8) +
    xlab("Release Side") +
    ylab("Release Height") +
    ggtitle("Release Points") +
    scale_color_manual(values = USABaseball) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2, title = "Pitch"))

  pitch_usage = pitches %>%
    group_by(TaggedPitchType) %>%
    summarise(Total = n()) %>%
    mutate(per=paste0(round(Total/sum(Total)*100, 1), "%"))

  pitch_usage <- pitch_usage %>%
    arrange(desc(TaggedPitchType)) %>%
    mutate(prop = Total / sum(pitch_usage$Total) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop)

  pitch_usage_plot <- ggplot(pitch_usage, aes(x="", y=prop, fill=TaggedPitchType)) +
    geom_bar(stat="identity", width=1, color=  "black") +
    coord_polar("y", start=0) +
    theme_void() +
    geom_text_repel(aes(y = ypos, label = per), color = "white", size=5) +
    scale_fill_manual(values = USABaseball,
                      labels = c("CH", "CB", "CU", "FS", "SI", "SL", "SP", "TS"),
                      breaks = names(USABaseball)) +
    labs(title = "Pitch Usage") +
    theme(legend.position = "bottom",
          legend.title= element_blank(),
          plot.title = element_text(size = 13, face = "bold", hjust = 0.5))


  move_rel_plots <- ggarrange(movement_plot, release_plot, nrow = 1)
  ggsave(move_rel_plots,file=paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i]," - breakRelease.png"),width=10,height=4,units="in", dpi = 300)
  pitchCharts2 <- image_read(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i]," - breakRelease.png"))
  PitchingReport3 <- image_composite(TemplatePageTwo,pitchCharts2, offset= "+150+1300")


  velocity_plot <- ggplot(subset(pitches),aes(x = pitchNumber,y = RelSpeed, color = TaggedPitchType)) +
    geom_line(linewidth = 1) +
    geom_point() +
    ylim(60,100) +    
    xlab("Pitch #") +
    ylab("Velocity") +
    ggtitle("Velocity by Pitch") +
    guides(color=guide_legend(title= "Pitch")) +
    scale_color_manual(values = USABaseball) +
    theme_minimal() +
    theme(legend.position = "bottom")

  velo_use_plots <- ggarrange(velocity_plot, pitch_usage_plot, nrow = 1, widths = c(2,1))
  ggsave(velo_use_plots,file=paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i]," - velocityPlot.png"),width=8,height=4,units="in", dpi = 300)
  pitchCharts3<- image_read(paste0("Pitching Reports Images/",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i]," - velocityPlot.png"))
  PitchingReport4 <- image_composite(PitchingReport3,pitchCharts3,offset= "+400+100")
  
  image_write(PitchingReport4,path = "page2.pdf",format="pdf",quality=100,density=300)
  
  qpdf::pdf_combine(input = c("page1.pdf", "page2.pdf"),
                    output = paste0("Pitching Reports/",as.character(pitches$Date[1]),"-",pitches$PitcherTeam[1]," vs. ",pitches$BatterTeam[1],"-",pitchers[i],".pdf"))
  }


all_pitches_graph <- ggplot(pitches) +
  geom_rect(aes(xmin=-8.5,xmax=8.5,ymin=18,ymax=42),alpha=0.0, color = "black") +
  geom_rect(aes(xmin=-11.5,xmax=11.5,ymin=15,ymax=45),alpha=0.0,color="black",linetype="dashed") +
  geom_point(aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, color = TaggedPitchType)) +
  theme_minimal() +
  coord_cartesian(xlim = c(-54, 54), ylim = c(-10, 72)) +
  labs(title = "All Pitches - Pitcher's View") +
  scale_color_manual(values = USABaseball,
                     labels = c("CH", "CB", "CU", "FS", "SI", "SL", "SP", "TS"),
                     breaks = names(USABaseball)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 13, hjust = 0.5, face = "bold"))
all_pitches_graph



