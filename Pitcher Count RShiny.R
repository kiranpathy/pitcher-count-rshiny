#Pitcher General

#App for Pitching

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(ggpubr)
library(paletteer)
library(shinyWidgets)

#What to make: Whiff plot, chase plot, overall pitch plot, velo distributions, vert/horz movement plot, velo over pitch number (?), exit velo plot (code in zones?)
#NEED IT TO BE ABLE TO CHANGE W DATE

left <- -8.5/12
right <- 8.5/12
top <- 40.53/12
bottom <- 21.06/12
width <- (right - left) / 3
height <- (top - bottom) / 3

df <- read_csv("20250621-ThomasMoreStadium-1_unverified.csv")

df <- df %>%
  mutate(
    TaggedPitchType = case_when(
      TaggedPitchType %in% c("FourSeamFastBall", "FourSeamFastball", "FourSeam")  ~ "Fastball",
      TaggedPitchType %in% c("TwoSeamFastBall", "TwoSeamFastball")                ~ "Sinker",
      TaggedPitchType %in% c("SInker")                                            ~ "Sinker",
      TaggedPitchType == "ChangeUp"                                               ~ "Changeup",
      TRUE                                                                        ~ TaggedPitchType
    ),
      Count = paste0(Balls, "-", Strikes)
  ) %>%
  filter(
    !TaggedPitchType %in% c("Undefined", "Other"),
    PitcherTeam == "FLO_Y'A"
  )

#in-zone calculations, chase set up, whiff, custom game id
df <- df %>%
  mutate(in_zone = ifelse(PlateLocSide < left | PlateLocSide > right | 
                            PlateLocHeight < bottom | PlateLocHeight > top, "0", "1"),
         chase = ifelse(PitchCall %in% c("Foul", "FoulBall", "FoulBallFieldable", 
                                         "FoulBallNotFieldable", "InPlay", "StrikeSwinging") & in_zone == "0", "1", "0"),
         whiff = ifelse(PitchCall == "StrikeSwinging", "1", "0")) %>%
  mutate(
    in_zone = as.numeric(in_zone),
    chase = as.numeric(chase)) %>%
  mutate(CustomGameID = paste0(
    Date, ":", AwayTeam, " at ", HomeTeam
  )) %>%
  mutate(
    Direction = as.numeric(Direction),
    field_side = case_when(
    Direction <= -67.5 ~ "Left",
    Direction > -67.5 & Direction <= -22.5 ~ "Left Center",
    Direction > -22.5 & Direction <= 22.5 ~ "Center",
    Direction > 22.5 & Direction <= 67.5 ~ "Right Center",
    Direction > 67.5 ~ "Right",
    TRUE ~ NA_character_
  ))

#UI
ui <- navbarPage("Pitchers",
                 theme = "flatly",
                 tabPanel("Strike Zone",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Team", label = "Select Team",
                                          choices = levels(as.factor(df$PitcherTeam))),
                              selectInput("Pitcher", label = "Select Pitcher",
                                          choices = levels(as.factor(df$Pitcher))),
                              pickerInput(
                                inputId = "GameInput",
                                label = HTML("Select Game"),
                                choices = levels(as.factor(df$CustomGameID)),
                                options = list(`actions-box` = TRUE),
                                multiple = T),
                              checkboxGroupInput("Pitch", label = "Select Pitch Type",
                                                 choices = levels(as.factor(df$TaggedPitchType))),
                              checkboxGroupInput("Result", label = "Select Play Result",
                                                 choices = levels(as.factor(df$PlayResult))),
                              pickerInput(
                                inputId  = "Count",
                                label    = HTML("Select Count"),
                                choices  = levels(as.factor(df$Count)),
                                options  = list(`actions-box` = TRUE),
                                multiple = TRUE
                              ),
                              width = 2),
                            mainPanel(
                              fluidRow(plotOutput("KZone", height = "1000px", width = "1200px"), plotOutput("InPlay", height = "1000px", width = "1200px"), DTOutput("InPlayStats"), plotOutput("Whiff", height = "1000px", width = "1200px"), DTOutput("WhiffStats"), plotOutput("Chase", height = "1000px", width = "1200px"), DTOutput("ChaseStats"))))),
                 tabPanel("Heatmap",
                          mainPanel(
                            fluidRow(plotOutput("Heatmap", height = "1000px", width = "1200px")))),
                 tabPanel("Heatmap (Hard Hit)",
                          mainPanel(
                            fluidRow(plotOutput("HeatmapHH", height = "1000px", width = "1200px")))))

#Server
server = function(input, output, session) {
  
  #Select Team --> Show those pitchers  
  observeEvent(
    input$Team,
    updateSelectInput(session,
                      "Pitcher", "Select Pitcher",
                      choices = levels(factor(filter(df,
                                                     PitcherTeam == isolate(input$Team))$Pitcher))))
  #select pitcher --> show his pitches
  observeEvent(
    input$Pitcher,
    updateCheckboxGroupInput(session,
                             "Pitch", "Select Pitch Type",
                             choices = levels(factor(filter(df,
                                                            Pitcher == isolate(input$Pitcher))$TaggedPitchType))))
  
  #select pitcher --> update date range
  observeEvent(
    input$Pitcher,
    updatePickerInput(session,
                      inputId = "GameInput",
                      choices = sort(unique(df$CustomGameID[df$Pitcher == input$Pitcher])),
                      selected = sort(unique(df$CustomGameID[df$Pitcher == input$Pitcher]))))
  
  #start with strike zone plot
  output$KZone <- renderPlot({
    df %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             PlayResult %in% input$Result,
             Count %in% input$Count,
             TaggedPitchType %in% input$Pitch,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(TestTrackMan, mapping = aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_point(aes(color = TaggedPitchType), size = 3) +
      scale_color_manual(values = c(Changeup = "blue",
                                    Fastball = "black",
                                    Slider = "orange",
                                    Curveball = "red",
                                    Cutter = "green",
                                    Sinker = "grey",
                                    Splitter = "purple")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-2.5, 2.5) + ylim(-.5, 5) +
      labs(title = "Strike Zone",
           subtitle = "All",
           x = "",
           y = "") +
      coord_fixed() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "right")
  })
    output$InPlay <- renderPlot({
      df %>%
        filter(PitcherTeam == input$Team,
               Pitcher == input$Pitcher,
               PlayResult %in% input$Result,
               Count %in% input$Count,
               TaggedPitchType %in% input$Pitch,
               PitchCall == "InPlay",
               CustomGameID %in% c(input$GameInput)) %>%
        ggplot(TestTrackMan, mapping = aes(x = PlateLocSide, y = PlateLocHeight)) +
        geom_point(aes(color = TaggedPitchType), size = 3) +
        scale_color_manual(values = c(Changeup = "blue",
                                      Fastball = "black",
                                      Slider = "orange",
                                      Curveball = "red",
                                      Cutter = "green",
                                      Sinker = "grey",
                                      Splitter = "purple")) +
        geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
        geom_segment(x = left, y = top, xend = right, yend = top) +
        geom_segment(x = left, y = bottom, xend = left, yend = top) +
        geom_segment(x = right, y = bottom, xend = right, yend = top) +
        geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
        geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
        geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
        geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
        geom_segment(x = left, y = 0, xend = right, yend = 0) +
        geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
        geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
        geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
        geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
        xlim(-2.5, 2.5) + ylim(-.5, 5) +
        labs(title = "Strike Zone",
             subtitle = "In Play",
             x = "",
             y = "") +
        coord_fixed() +
        geom_text(aes(label = PitchNo)) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              legend.position = "none")
    
  })
  output$Chase <- renderPlot({
    df %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             chase == 1,
             PlayResult %in% input$Result,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(TestTrackMan, mapping = aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_point(aes(color = TaggedPitchType), size = 3) +
      scale_color_manual(values = c(Changeup = "blue",
                                    Fastball = "black",
                                    Slider = "orange",
                                    Curveball = "red",
                                    Cutter = "green",
                                    Sinker = "grey",
                                    Splitter = "purple")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-2.5, 2.5) + ylim(-.5, 5) +
      labs(title = "Strike Zone",
           subtitle = "Chase",
           x = "",
           y = "") +
      coord_fixed() +
      geom_text(aes(label = PitchNo)) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none")
  })
  output$Whiff <- renderPlot({
    df %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             whiff == 1,
             PlayResult %in% input$Result,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(TestTrackMan, mapping = aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_point(aes(color = TaggedPitchType), size = 3) +
      scale_color_manual(values = c(Changeup = "blue",
                                    Fastball = "black",
                                    Slider = "orange",
                                    Curveball = "red",
                                    Cutter = "green",
                                    Sinker = "grey",
                                    Splitter = "purple")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-2.5, 2.5) + ylim(-.5, 5) +
      labs(title = "Strike Zone",
           subtitle = "Whiff",
           x = "",
           y = "") +
      coord_fixed() +
      geom_text(aes(label = PitchNo)) +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none")
    
  })
  
  output$InPlayStats <- renderDT({
    inplaydf <- df %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             PitchCall == "InPlay",
             PlayResult %in% input$Result,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput))
    
    req(nrow(inplaydf) > 0)
    
    inplaydf %>%
      select(PitchNo, Count, BatterSide, TaggedPitchType, RelSpeed, TaggedHitType, ExitSpeed, field_side, PlayResult, InducedVertBreak, HorzBreak, SpinRate, VertApprAngle)
    
  })
  
  output$WhiffStats <- renderDT({
    whiffdf <- df %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             whiff == 1,
             PlayResult %in% input$Result,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput))
    
    req(nrow(whiffdf) > 0)
    
    whiffdf %>%
      select(PitchNo, Count, BatterSide, TaggedPitchType, RelSpeed, InducedVertBreak, HorzBreak, SpinRate, VertApprAngle)
  })
  
  output$ChaseStats <- renderDT({
    chasedf <- df %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             chase == 1,
             PlayResult %in% input$Result,
             TaggedPitchType %in% input$Pitch,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput))
    
    req(nrow(chasedf) > 0)
    
    chasedf %>%
      select(PitchNo, Count, BatterSide, TaggedPitchType, RelSpeed, InducedVertBreak, HorzBreak, SpinRate, VertApprAngle)
  })
  
  output$Heatmap <- renderPlot({
    
    heat_colors_interpolated <- colorRampPalette(paletteer_d("RColorBrewer::RdBu", 
                                                             n = 9,
                                                             direction = -1))(16)
    FB <- df %>%
      filter(TaggedPitchType == "Fastball")
    
    FBplot <- FB %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch,
             PlayResult %in% input$Result,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Fastball")) +
      coord_fixed() +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    CH <- df %>%
      filter(TaggedPitchType == "Changeup")
    
    CHplot <- CH %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch,
             PlayResult %in% input$Result,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Changeup")) +
      coord_fixed() +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    SL <- df %>%
      filter(TaggedPitchType == "Slider")
    
    SLplot <- SL %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch,
             PlayResult %in% input$Result,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Slider")) +
      coord_fixed() +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    CB <- df %>%
      filter(TaggedPitchType == "Curveball")
    
    CBplot <- CB %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch,
             PlayResult %in% input$Result,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Curveball")) +
      coord_fixed() +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    CUT <- df %>%
      filter(TaggedPitchType == "Cutter")
    
    CUTplot <- CUT %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch,
             PlayResult %in% input$Result,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Cutter")) +
      coord_fixed() +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    SNK <- df %>%
      filter(TaggedPitchType == "Sinker")
    
    SNKplot <- SNK %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch,
             PlayResult %in% input$Result,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Sinker")) +
      coord_fixed() +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    FS <- df %>%
      filter(TaggedPitchType == "Splitter")
    
    FSplot <- FS %>%
      filter(PitcherTeam == input$Team,
             Pitcher == input$Pitcher,
             TaggedPitchType %in% input$Pitch,
             PlayResult %in% input$Result,
             Count %in% input$Count,
             CustomGameID %in% c(input$GameInput)) %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density2d_filled() +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill" , "color")) +
      geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
      geom_segment(x = left, y = top, xend = right, yend = top) +
      geom_segment(x = left, y = bottom, xend = left, yend = top) +
      geom_segment(x = right, y = bottom, xend = right, yend = top) +
      geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
      geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
      geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
      geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
      geom_segment(x = left, y = 0, xend = right, yend = 0) +
      geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
      geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
      geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
      xlim(-3,3) + ylim(0, 5) + 
      ggtitle(paste("Splitter")) +
      coord_fixed() +
      theme(legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA))
    
    ggarrange(FBplot, CHplot, SLplot, CBplot, CUTplot, SNKplot, FSplot, ncol = 3, nrow = 3)
    
  })
  
  output$HeatmapHH <- renderPlot({
    
    df_hh <- df %>% 
      filter(!is.na(ExitSpeed),
        ExitSpeed >= 95)
    
    heat_colors_interpolated <- colorRampPalette(paletteer_d("RColorBrewer::RdBu", 
                                                             n = 9,
                                                             direction = -1))(16)
   
    FBplot <- df_hh %>%                           
      filter(PitcherTeam     == input$Team,
             Pitcher         == input$Pitcher,
             TaggedPitchType == "Fastball",
             TaggedPitchType %in% input$Pitch,
             PlayResult      %in% input$Result,
             Count           %in% input$Count,
             CustomGameID    %in% input$GameInput) %>%         
      
      {                                                        
        if (nrow(.) < 2 || n_distinct(.$PlateLocSide, .$PlateLocHeight) == 1) {
          ggplot() +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Fastball\n(1 or Fewer Hard Hit)") +
            theme_void() +
            theme(plot.title = element_text(hjust = .5, size = 12))
        } else {
          ggplot(., aes(PlateLocSide, PlateLocHeight)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated,
                              aesthetics = c("fill", "color")) +
            geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
            geom_segment(x = left, y = top, xend = right, yend = top) +
            geom_segment(x = left, y = bottom, xend = left, yend = top) +
            geom_segment(x = right, y = bottom, xend = right, yend = top) +
            geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
            geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
            geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
            geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
            geom_segment(x = left, y = 0, xend = right, yend = 0) +
            geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
            geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Fastball Hard Hit") +
            theme(legend.position = "none",
                  plot.title      = element_text(face = "bold", size = 15),
                  axis.title      = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border     = element_rect(color = "black", size = 1.5, fill = NA))
        }
      }
    
    CHplot <- df_hh %>%                           
      filter(PitcherTeam     == input$Team,
             Pitcher         == input$Pitcher,
             TaggedPitchType == "Changeup",
             TaggedPitchType %in% input$Pitch,
             PlayResult      %in% input$Result,
             Count           %in% input$Count,
             CustomGameID    %in% input$GameInput) %>%         
      
      {                                                        
        if (nrow(.) < 2 || n_distinct(.$PlateLocSide, .$PlateLocHeight) == 1) {
          ggplot() +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Changeup\n(1 or Fewer Hard Hit)") +
            theme_void() +
            theme(plot.title = element_text(hjust = .5, size = 12))
        } else {
          ggplot(., aes(PlateLocSide, PlateLocHeight)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated,
                              aesthetics = c("fill", "color")) +
            geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
            geom_segment(x = left, y = top, xend = right, yend = top) +
            geom_segment(x = left, y = bottom, xend = left, yend = top) +
            geom_segment(x = right, y = bottom, xend = right, yend = top) +
            geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
            geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
            geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
            geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
            geom_segment(x = left, y = 0, xend = right, yend = 0) +
            geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
            geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Changeup Hard Hit") +
            theme(legend.position = "none",
                  plot.title      = element_text(face = "bold", size = 15),
                  axis.title      = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border     = element_rect(color = "black", size = 1.5, fill = NA))
        }
      }

    SLplot <- df_hh %>%                           
      filter(PitcherTeam     == input$Team,
             Pitcher         == input$Pitcher,
             TaggedPitchType == "Slider",
             TaggedPitchType %in% input$Pitch,
             PlayResult      %in% input$Result,
             Count           %in% input$Count,
             CustomGameID    %in% input$GameInput) %>%         
      
      {                                                        
        if (nrow(.) < 2 || n_distinct(.$PlateLocSide, .$PlateLocHeight) == 1) {
          ggplot() +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Slider\n(1 or Fewer Hard Hit)") +
            theme_void() +
            theme(plot.title = element_text(hjust = .5, size = 12))
        } else {
          ggplot(., aes(PlateLocSide, PlateLocHeight)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated,
                              aesthetics = c("fill", "color")) +
            geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
            geom_segment(x = left, y = top, xend = right, yend = top) +
            geom_segment(x = left, y = bottom, xend = left, yend = top) +
            geom_segment(x = right, y = bottom, xend = right, yend = top) +
            geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
            geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
            geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
            geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
            geom_segment(x = left, y = 0, xend = right, yend = 0) +
            geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
            geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Slider Hard Hit") +
            theme(legend.position = "none",
                  plot.title      = element_text(face = "bold", size = 15),
                  axis.title      = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border     = element_rect(color = "black", size = 1.5, fill = NA))
        }
      }
    
    CBplot <- df_hh %>%                           
      filter(PitcherTeam     == input$Team,
             Pitcher         == input$Pitcher,
             TaggedPitchType == "Curveball",
             TaggedPitchType %in% input$Pitch,
             PlayResult      %in% input$Result,
             Count           %in% input$Count,
             CustomGameID    %in% input$GameInput) %>%         
      
      {                                                        
        if (nrow(.) < 2 || n_distinct(.$PlateLocSide, .$PlateLocHeight) == 1) {
          ggplot() +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Curveball\n(1 or Fewer Hard Hit)") +
            theme_void() +
            theme(plot.title = element_text(hjust = .5, size = 12))
        } else {
          ggplot(., aes(PlateLocSide, PlateLocHeight)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated,
                              aesthetics = c("fill", "color")) +
            geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
            geom_segment(x = left, y = top, xend = right, yend = top) +
            geom_segment(x = left, y = bottom, xend = left, yend = top) +
            geom_segment(x = right, y = bottom, xend = right, yend = top) +
            geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
            geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
            geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
            geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
            geom_segment(x = left, y = 0, xend = right, yend = 0) +
            geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
            geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Curveball Hard Hit") +
            theme(legend.position = "none",
                  plot.title      = element_text(face = "bold", size = 15),
                  axis.title      = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border     = element_rect(color = "black", size = 1.5, fill = NA))
        }
      }
    
    CUTplot <- df_hh %>%                           
      filter(PitcherTeam     == input$Team,
             Pitcher         == input$Pitcher,
             TaggedPitchType == "Cutter",
             TaggedPitchType %in% input$Pitch,
             PlayResult      %in% input$Result,
             Count           %in% input$Count,
             CustomGameID    %in% input$GameInput) %>%         
      
      {                                                        
        if (nrow(.) < 2 || n_distinct(.$PlateLocSide, .$PlateLocHeight) == 1) {
          ggplot() +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Cutter\n(1 or Fewer Hard Hit)") +
            theme_void() +
            theme(plot.title = element_text(hjust = .5, size = 12))
        } else {
          ggplot(., aes(PlateLocSide, PlateLocHeight)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated,
                              aesthetics = c("fill", "color")) +
            geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
            geom_segment(x = left, y = top, xend = right, yend = top) +
            geom_segment(x = left, y = bottom, xend = left, yend = top) +
            geom_segment(x = right, y = bottom, xend = right, yend = top) +
            geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
            geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
            geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
            geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
            geom_segment(x = left, y = 0, xend = right, yend = 0) +
            geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
            geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Cutter Hard Hit") +
            theme(legend.position = "none",
                  plot.title      = element_text(face = "bold", size = 15),
                  axis.title      = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border     = element_rect(color = "black", size = 1.5, fill = NA))
        }
      }
    
    SNKplot <- df_hh %>%                           
      filter(PitcherTeam     == input$Team,
             Pitcher         == input$Pitcher,
             TaggedPitchType == "Sinker",
             TaggedPitchType %in% input$Pitch,
             PlayResult      %in% input$Result,
             Count           %in% input$Count,
             CustomGameID    %in% input$GameInput) %>%         
      
      {                                                        
        if (nrow(.) < 2 || n_distinct(.$PlateLocSide, .$PlateLocHeight) == 1) {
          ggplot() +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Sinker\n(1 or Fewer Hard Hit)") +
            theme_void() +
            theme(plot.title = element_text(hjust = .5, size = 12))
        } else {
          ggplot(., aes(PlateLocSide, PlateLocHeight)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated,
                              aesthetics = c("fill", "color")) +
            geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
            geom_segment(x = left, y = top, xend = right, yend = top) +
            geom_segment(x = left, y = bottom, xend = left, yend = top) +
            geom_segment(x = right, y = bottom, xend = right, yend = top) +
            geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
            geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
            geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
            geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
            geom_segment(x = left, y = 0, xend = right, yend = 0) +
            geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
            geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Sinker Hard Hit") +
            theme(legend.position = "none",
                  plot.title      = element_text(face = "bold", size = 15),
                  axis.title      = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border     = element_rect(color = "black", size = 1.5, fill = NA))
        }
      }
    
    FSplot <- df_hh %>%                           
      filter(PitcherTeam     == input$Team,
             Pitcher         == input$Pitcher,
             TaggedPitchType == "Splitter",
             TaggedPitchType %in% input$Pitch,
             PlayResult      %in% input$Result,
             Count           %in% input$Count,
             CustomGameID    %in% input$GameInput) %>%         
      
      {                                                        
        if (nrow(.) < 2 || n_distinct(.$PlateLocSide, .$PlateLocHeight) == 1) {
          ggplot() +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Splitter\n(1 or Fewer Hard Hit)") +
            theme_void() +
            theme(plot.title = element_text(hjust = .5, size = 12))
        } else {
          ggplot(., aes(PlateLocSide, PlateLocHeight)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated,
                              aesthetics = c("fill", "color")) +
            geom_segment(x = left, y = bottom, xend = right, yend = bottom) +
            geom_segment(x = left, y = top, xend = right, yend = top) +
            geom_segment(x = left, y = bottom, xend = left, yend = top) +
            geom_segment(x = right, y = bottom, xend = right, yend = top) +
            geom_segment(x = left, y = (bottom + height), xend = right, yend = (bottom + height)) +
            geom_segment(x = left, y = (top - height), xend = right, yend = (top - height)) +
            geom_segment(x = (left + width), y = bottom, xend = (left + width), yend = top) +
            geom_segment(x = (right - width), y = bottom, xend = (right - width), yend = top) +
            geom_segment(x = left, y = 0, xend = right, yend = 0) +
            geom_segment(x = left, y = 0, xend = left, yend = (4.25/12)) +
            geom_segment(x = left, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = (4.25/12), xend = 0, yend = (8.5/12)) +
            geom_segment(x = right, y = 0, xend = right, yend = (4.25/12)) +
            xlim(-3, 3) + ylim(0, 5) + coord_fixed() +
            ggtitle("Splitter Hard Hit") +
            theme(legend.position = "none",
                  plot.title      = element_text(face = "bold", size = 15),
                  axis.title      = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border     = element_rect(color = "black", size = 1.5, fill = NA))
        }
      }
    
    ggarrange(FBplot, CHplot, SLplot, CBplot, CUTplot, SNKplot, FSplot, ncol = 3, nrow = 3)
    
  })
}

shinyApp(ui = ui, server = server)