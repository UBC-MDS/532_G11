library(shiny)
library(tidyverse)
#load data
movies <- read.csv("data/imdb_top_1000.csv", stringsAsFactors = FALSE) 
# create UI

ui <- navbarPage(
  # main title
  titlePanel("Explore Movies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("genre", "Genre:", choices = unique(unique(unlist(strsplit(movies$Genre,','))))),
      selectInput("year", "Release Year:", choices = sort(unique(movies$Released_Year))),
      selectInput("certificate", "Certificate:", choices = unique(movies$Certificate)),
      radioButtons("sort_by", "Sort By", choices = c("Option 1" = "option1", 
                                                     "Option 2" = "option2",
                                                     "Option 3" = "option3"),
                   selected = "option1")
    ),
    
    
    mainPanel(
      h3("Top 10 Movies"),
      tableOutput("top_movies")
    )
  )
)



# Server function
server <- function(input, output, session) {
  # Server code for both pages...
}

# Run the app
shinyApp(ui = ui, server = server)
