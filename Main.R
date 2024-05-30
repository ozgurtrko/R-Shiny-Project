library(shiny)
library(shinydashboard)
library(shinythemes)
library(readr)
library(dplyr)
library(DT)
library(ggplot2)
setwd("~/desktop")
# Read the dataset
dataset <- read_csv("dataset.csv")

# Create the UI components
ui <- dashboardPage(
  dashboardHeader(title = "Song Recommendation System"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recommendation", tabName = "recommendations", icon = icon("music")),
      menuItem("Graphs", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Playlist", tabName = "playlist", icon = icon("list"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .theme-selector {
          position: fixed;
          bottom: 0;
          left: 0;
          z-index: 1000;
          background-color: white;
          border-top: 1px solid #ccc;
          padding: 10px;
        }
      "))
    ),
    div(class = "theme-selector",
        shinythemes::themeSelector()
    ),
    tabItems(
      tabItem(tabName = "recommendations",
              fluidRow(
                box(title = "Filters", status = "primary", solidHeader = TRUE, width = 3,
                    selectInput("track_genre", "Select Genre:", choices = c("",unique(dataset$track_genre))),
                    selectInput("artist", "Select Artist:", choices = c("",unique(dataset$artists)), selected = NULL),
                    sliderInput("tempo", "Tempo:", min = min(dataset$tempo), max = max(dataset$tempo), value = c(min(dataset$tempo), max(dataset$tempo))),
                    sliderInput("popularity", "Popularity level:", min = min(dataset$popularity), max = max(dataset$popularity), value = c(min(dataset$popularity), max(dataset$popularity))),
                    actionButton("search", "Search")
                ),
                box(title = "Results", status = "primary", solidHeader = TRUE, width = 9,
                    dataTableOutput("results")
                )
              )
      ),
      tabItem(tabName = "plots",
              fluidRow(
                box(title = "Top Genres", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("genrePlot")
                )
              ),
              fluidRow(
                box(title = "Top Artists by Selected Genre", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("selected_genre", "Select Genre:", choices = unique(dataset$track_genre)),
                    plotOutput("artistPlotByGenre")
                )
              )
      ),
      tabItem(tabName = "playlist",
              fluidRow(
                box(title = "Playlist", status = "primary", solidHeader = TRUE, width = 12,
                    dataTableOutput("playlist"),
                    actionButton("clearPlaylist", "Clear Playlist", icon = icon("trash"))
                )
              ),
              fluidRow(
                box(title = "Playlist Summary", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("playlistSummaryPlot")
                )
              ),
              fluidRow(
                box(title = "Songs per Artist", status = "primary", solidHeader = TRUE, width = 12,
                    dataTableOutput("songsPerArtist")
                )
              )
      )
    )
  ),
  skin = "blue"  # Set the theme color
)

# Create the server function
server <- function(input, output, session) {
  # Create a reactive value to store the playlist
  playlist <- reactiveVal(data.frame(
    `Track Name` = character(),
    `Artists` = character(),
    `Popularity` = numeric(),
    `Album Name` = character(),
    `Genre` = character(),
    stringsAsFactors = FALSE
  ))
  
  # Create a reactive value to store the filtered data
  filtered_data <- reactiveVal(data.frame(
    `Track Name` = character(),
    `Artists` = character(),
    `Popularity` = numeric(),
    `Album Name` = character(),
    `Genre` = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$search, {
    # Filter the dataset based on user selections
    filtered <- dataset %>%
      filter((track_genre == input$track_genre | input$track_genre == "") &
               (artists == input$artist | input$artist == "") &
               tempo >= input$tempo[1] & tempo <= input$tempo[2] &
               popularity >= input$popularity[1] & popularity <= input$popularity[2]) %>%
      select(track_name, artists, popularity, album_name, track_genre) %>%
      rename(
        `Track Name` = track_name,
        `Artists` = artists,
        `Popularity` = popularity,
        `Album Name` = album_name,
        `Genre` = track_genre
      )
    
    filtered$`Add to Playlist` <- paste0(
      '<button id="add_', 1:nrow(filtered), '" type="button" class="btn btn-primary">Add to Playlist</button>'
    )
    
    filtered_data(filtered)
    
    # Render the filtered data
    output$results <- renderDataTable({
      datatable(filtered_data(), escape = FALSE, options = list(pageLength = 10, autoWidth = TRUE), selection = "single")
    })
  })
  
  # Add song to the playlist when the user clicks "Add to Playlist"
  observeEvent(input$results_rows_selected, {
    selected_row <- filtered_data()[input$results_rows_selected, , drop = FALSE]
    current_playlist <- playlist()
    
    # Remove "Add to Playlist" column
    selected_row <- selected_row %>% select(-`Add to Playlist`)
    
    # Add new entries and update the playlist
    updated_playlist <- bind_rows(current_playlist, selected_row)
    playlist(updated_playlist)
  })
  
  # Render the playlist
  output$playlist <- renderDataTable({
    datatable(playlist(), options = list(pageLength = 10, autoWidth = TRUE), escape = FALSE, selection = "single")
  })
  
  # Clear the playlist
  observeEvent(input$clearPlaylist, {
    playlist(data.frame(
      `Track Name` = character(),
      `Artists` = character(),
      `Popularity` = numeric(),
      `Album Name` = character(),
      `Genre` = character(),
      stringsAsFactors = FALSE
    ))
  })
  
  # Plot for top genres
  output$genrePlot <- renderPlot({
    genre_popularity <- dataset %>%
      group_by(track_genre) %>%
      summarise(average_popularity = mean(popularity)) %>%
      arrange(desc(average_popularity)) %>%
      slice_max(average_popularity, n = 10)  # Show top 10 genres
    
    ggplot(genre_popularity, aes(x = reorder(track_genre, average_popularity), y = average_popularity)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +  # Horizontal bar chart
      labs(title = "Top 10 Genres", x = "Genres", y = "Average Popularity") +
      theme_minimal()
  })
  
  # Plot for top artists by selected genre
  observeEvent(input$selected_genre, {
    output$artistPlotByGenre <- renderPlot({
      artist_popularity_by_genre <- dataset %>%
        filter(track_genre == input$selected_genre) %>%
        group_by(artists) %>%
        summarise(average_popularity = mean(popularity)) %>%
        arrange(desc(average_popularity)) %>%
        slice_max(average_popularity, n = 10)  # Show top 10 artists for the selected genre
      
      ggplot(artist_popularity_by_genre, aes(x = reorder(artists, average_popularity), y = average_popularity)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        coord_flip() +  # Horizontal bar chart
        labs(title = paste("Top 10 Artists for", input$selected_genre), x = "Artists", y = "Average Popularity") +
        theme_minimal()
    })
  })
  
  # Playlist summary plot
  output$playlistSummaryPlot <- renderPlot({
    playlist_data <- playlist() %>%
      group_by(Genre, Artists) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    ggplot(playlist_data, aes(x = Genre, y = Count, fill = Genre)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Playlist Summary", x = "Genre", y = "Count of Songs") +
      theme_minimal()
  })
  
  # Table showing the number of songs per artist
  output$songsPerArtist <- renderDataTable({
    songs_per_artist <- playlist() %>%
      group_by(Artists) %>%
      summarise(`Number of Songs` = n()) %>%
      arrange(desc(`Number of Songs`))
    
    datatable(songs_per_artist, options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
