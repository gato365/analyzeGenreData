#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

setwd("G:/My Drive/03_research_and_development/04_emans_projects/02_fun_projects/project_52_spotify_music_data/analyzeGenreData")
artists_df = readRDS("songsPresentation.RData")
artists_names = unique(artists_df$artist_name)


df = data.frame(x = c(0,1), y = c(0,1))
p1 = df %>% 
    ggplot(aes(x = x, y = y)) +
    geom_blank() +
    geom_vline(xintercept = 0,size = 1) +
    geom_hline(yintercept = 0.5,size = 1) +
    scale_x_continuous(limits = c(-1,1), 
                       expand = c(0, 0),
                       labels = label_number(accuracy = 0.1)) +
    scale_y_continuous(limits = c(0,1), expand = c(0, 0))  +
    theme_bw() +
    theme(axis.title = element_text(size = 18, face = 'bold')) +
    geom_rect(aes(xmin=-1, xmax=0, ymin=0, ymax=1),alpha = 0.15, fill = 'red') +
    geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1),alpha = 0.15, fill = 'blue') +
    labs(x = 'Valence',y='Energy') +
    theme(axis.text = element_text(size = 12, face = 'bold'),
          plot.margin = margin(0.3, 0.5, 0.1, 0.5, "cm")
    )

neon_colors <- c(
    '#84DE02'
    , '#FF4466'
    , '#4BC7CF'
    , '#FF85CF'
    , '#FFDF46'
    , '#391285'
    , '#E88E5A'
    , '#DDE26A'
    , '#C53151'
    , '#B05C52'
    , '#FD5240'
    , '#FF4681'
    , '#FF6D3A'
    , '#FF404C'
    , '#A0E6FF'
    ,"#999999" 
    ,"#E69F00" 
    ,"#56B4E9"
)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    inform <- eventReactive(input$button, {
        artist_name = input$artist
        showMoods = input$moodLabel
        
        return(list(artist_name = artist_name,showMoods = showMoods))
        
    }
    
    
    )
    
    
    
    output$artistPlot <- renderPlot({
        
        inform = inform()
        
        ## Get Artist
        tmp_df = artists_df  %>% 
            filter(artist_name == inform$artist_name) %>% 
            mutate(m1_valence = 2*valence - 1) 
        p2 <- p1 +
            geom_point(data = tmp_df,aes(x =m1_valence  , y = energy, color = album_name),size = 2.1) +
            scale_color_manual(values=neon_colors) +
            theme(legend.position = 'bottom')
        
        # p3 <- ggplotly(p2, tooltip = "text") %>% partial_bundle()
        
        if(inform$showMoods == FALSE){        
            p2
        } else{
            p2 +
                annotate("text", x=-0.5, y=0.25, label= "Sad",size = 15.5, color = 'white') +
                annotate("text", x=0.55, y=0.25, label= "Chill",size = 15.5, color = 'white') +
                annotate("text", x=-0.5, y=0.75, label= "Anger",size = 15.5, color = 'white') +
                annotate("text", x=0.55, y=0.75, label= "Happy",size = 15.5, color = 'white')
            
        }
    })
    
})
