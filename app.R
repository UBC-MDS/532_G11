library(shiny)
library(tidyverse)
library(shinyWidgets)
library(purrr)
# remove stings in Runtime column
movies <- read.csv("data/imdb_top_1000.csv", stringsAsFactors = FALSE) %>% 
  mutate(Runtime = as.numeric(gsub("([0-9]+).*$", "\\1", Runtime))) %>%
  mutate(Gross = as.numeric(gsub(",", "", Gross))/1000000)

# Create side panels to avoid duplication
sidepanels <- sidebarPanel(pickerInput("genre", "Genre(s):", choices = sort(unique(str_replace_all(string=unlist(strsplit(movies$Genre,',')), pattern=" ", repl=""))),
                      options = list(`actions-box` = TRUE),multiple = T, selected = 'Drama'), 
# Select single star
pickerInput("star", "Star(s):", choices = sort(unique(unlist(select(movies,Star1:Star4) %>% as.list()))),
            options = list(`actions-box` = TRUE, `live-search` = TRUE),multiple = T, selected = 'Morgan Freeman'),
# select min movie rating
sliderInput("minRevenue", "Min Revenue in Million:",
            min = 0, max = round(max(na.omit(movies$Gross))),
            value = median(na.omit(movies$Gross)), step = 50, pre ='$', post ='M'),
# select range of released year 
sliderInput("year", "Released Year Range:",
            min = suppressWarnings(min(na.omit(parse_number(movies$Released_Year)))),
            max = suppressWarnings(max(na.omit(parse_number(movies$Released_Year)))),
            value = c(1920,2020)),
# Select range of runtime
sliderInput("runtimes", "Runtimes Range (Minutes):",
            min = min(movies$Runtime),
            max = max(movies$Runtime),
            value = c(70,200)))

# UI function
ui <- navbarPage('IMDB_Viz_R', # app name
                 
                 tabPanel('Top 3 Movie Recommendations', # first navbar page for movie recommendations
                          sidebarLayout( 
                            sidepanels, # sidebar panel inside the first navbar page
                            mainPanel(
    
                              htmlOutput("picture")
                            )
                          )
                 ),
                 
                 tabPanel('Movie Plots', # second navbar page for three plots
                          sidebarLayout( 
                            sidepanels,
                            mainPanel(
                              tabsetPanel( # there are three tabs in the main panel 
                                tabPanel('Ratings by Genre'
                            
                                ),
                                tabPanel('Runtimes by Genre'
                                         
                                         
                                ),
                                tabPanel('Movies by Genre')
                                
                                
                              )
                            )
                          )
                 ),
                 
)




# Server function
server <- function(input, output) {
  
  
  # use reactive to avoid duplication
  filtered_data <-reactive({ 

    movies %>%
      filter(.data$Gross >= input$minRevenue) %>% # filter by min gross revenue
      filter(.data$Released_Year >= input$year[1] & .data$Released_Year <= input$year[2]) %>% # filter by released year range
      filter(.data$Runtime >= input$runtimes[1] & .data$Runtime <= input$runtimes[2]) %>% # filter by runtimes range
      filter(str_detect(.data$Genre, paste(input$genre, collapse="|"))) %>% # filter by genre(s)
      filter( .data$Star1 %in% input$star | .data$Star2 %in% input$star | .data$Star3 %in% input$star | .data$Star4 %in% input$star ) %>% # filter by actor(s) 
      arrange(-.data$IMDB_Rating) # order by moive rating
    
    
  })
  
  # output for movie recommendations

    output$picture <- renderUI({
    urls <- filtered_data() %>% select(Poster_Link) %>% head(3) %>% pull() # get top 3 movie poster urls
    titles <- filtered_data() %>% select(Series_Title) %>% head(3) %>% pull() # get top 3 movie titles
    overviews <- filtered_data() %>% select(Overview) %>% head(3) %>% pull() # get top 3 movie overviews
    img_tags <- mapply(function(url, caption, text) {
      img_tag <- paste0("<img src='", url, "' style='width: 200px; height: auto; display:block; margin:0 auto;'>")
      title_tag <- paste0("<div style='text-align:center; margin-bottom:5px; word-wrap: break-word; word-break: break-all; font-weight:bold; font-size:16px;'>", caption, "</div>")
      overview_tag <- paste0("<div style='text-align:center; word-wrap: break-word; word-break: break-all; max-width: 200px; line-height: 1.4; margin-top: 5px;'>", text, "</div>")
      paste0("<div style='display:inline-block; margin:70px; vertical-align:top;'>", title_tag, img_tag, overview_tag, "</div>")
    }, urls, titles, overviews, SIMPLIFY = FALSE)
    HTML(paste0(img_tags, collapse = ""))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
