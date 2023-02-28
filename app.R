library(shiny)
library(tidyverse)
library(shinyWidgets)

# We only use the top 1000 movies based on ratings from IMDB
movies <- read.csv("data/imdb_top_1000.csv", stringsAsFactors = FALSE) 

# create UI

# ui <- navbarPage(
#   # main title
#   titlePanel("Explore Movies"),
  # sidebarLayout(
  #   sidebarPanel(
  #     selectInput("genre", "Genre:", choices = unique(unique(unlist(strsplit(movies$Genre,','))))),
  #     selectInput("year", "Release Year:", choices = sort(unique(movies$Released_Year))),
  #     selectInput("certificate", "Certificate:", choices = unique(movies$Certificate)),
  #     radioButtons("sort_by", "Sort By", choices = c("Option 1" = "option1",
  #                                                    "Option 2" = "option2",
  #                                                    "Option 3" = "option3"),
  #                  selected = "option1")
  #   ),
#     
#     
#     mainPanel(
#       h3("Top 10 Movies"),
#       tableOutput("top_movies")
#     )
#   )
# )

ui <- navbarPage('IMDB_Viz_R', # app name
                 
                 tabPanel('Top 3 Movie Recommendations', # first navbar page for movie recommendations
                          sidebarLayout( 
                            sidebarPanel(
                              # Select multiple genres
                              pickerInput("genre", "Genre(s):", choices = sort(unique(str_replace_all(string=unlist(strsplit(movies$Genre,',')), pattern=" ", repl=""))),
                                          options = list(`actions-box` = TRUE),multiple = T), 
                              # Select single star
                              pickerInput("star", "Star(s):", choices = sort(unique(unlist(select(movies,Star1:Star4) %>% as.list()))),
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE),multiple = T),
                              # select min movie rating
                              sliderInput("minrating", "Min Ratings:",
                                          min = 0, max = 10,
                                          value = 0, step = 0.1),
                              # select range of released year 
                              sliderInput("year", "Released Year Range:",
                                          min = suppressWarnings(min(na.omit(parse_number(movies$Released_Year)))),
                                          max = suppressWarnings(max(na.omit(parse_number(movies$Released_Year)))),
                                          value = c(1990,2000)),
                              # Select range of runtime
                              sliderInput("runtime", "Runtimes Range (Minutes):",
                                          min = suppressWarnings(min(na.omit(parse_number(movies$Runtime)))),
                                          max = suppressWarnings(max(na.omit(parse_number(movies$Runtime)))),
                                          value = c(90,200))
                                
                              
                            ), # sidebar panel inside the first navbar page
                            mainPanel(
                            )
                          )
                 ),
                 
                 tabPanel('Movie Plots', # second navbar page for three plots
                          sidebarLayout( 
                            sidebarPanel("You can add Input function here"), # sidebar panel inside the second navbar page
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
  # Server code for both pages...
}

# Run the app
shinyApp(ui = ui, server = server)
