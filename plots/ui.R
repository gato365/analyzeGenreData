#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(plotly)


white_theme <- bs_theme()
dark_theme <- white_theme %>% 
    bs_theme_update(bg = "#45ba70", fg = "gray", primary = "orange")


setwd("G:/My Drive/03_research_and_development/04_emans_projects/02_fun_projects/project_52_spotify_music_data/analyzeGenreData")
artists_df = readRDS("songsPresentation.RData")
artists_names = unique(artists_df$artist_name)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = dark_theme ,
    # Application title
    titlePanel(  "Visualize Artist"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons('artist','Select an Artist:',artists_names),
            checkboxInput('moodLabel', 'Display Mood Labels',value = FALSE),
            actionButton('button',' Visualize')
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            # plotlyOutput("artistPlot")
            plotOutput("artistPlot")
        )
    )
))
