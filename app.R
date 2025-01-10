# Library
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(patchwork)
library(gridExtra)
library(stringr)
library(circlepackeR)
library(data.tree)
library(packcircles)
library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(streamgraph)
library(tidyr)
library(dygraphs)
library(xts)
library(DT)
library(tm)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(ggiraph)
library(htmlwidgets)
library(htmltools)
library(shinyjs)
library(bslib)
library(gganimate)
library(sf)

spotify_clean <- read.csv("Spotify 2010 - 2019 Top 100.csv")

#Cleaning
## spotify_clean <- na.omit(spotify_clean)
spotify_clean <- na.omit(spotify_clean) |>
  filter(year.released != 1975)

# Fixing data
spotify_clean[spotify_clean$title == "Read All About It", "year.released"] <- 2011
spotify_clean[spotify_clean$title == "Hurts So Good", "year.released"] <- 2016
spotify_clean[spotify_clean$title == "Eenie Meenie", "year.released"] <- 2010
spotify_clean[spotify_clean$title == "We No Speak Americano (Edit)", "year.released"] <- 2010
spotify_clean[spotify_clean$title == "Gold", "year.released"] <- 2015
spotify_clean[spotify_clean$title == "Easier", "year.released"] <- 2019
spotify_clean[spotify_clean$title == "i'm so tired...", "year.released"] <- 2019
spotify_clean[spotify_clean$title == "Options", "year.released"] <- 2018
spotify_clean[spotify_clean$title == "I Follow Rivers - The Magician Remix", "year.released"] <- 2011

# --------------------- Circualr genres + table --------------------------------#

# Define subgroups within "Pop"
define_pop_sub_genre <- function(genre) {
  if (genre %in% c("dance pop", "electropop", "synthpop", "pop soul", "pop rock", "pop rap", "indie pop", "art pop", "baroque pop", "chamber pop", "pop punk")) {
    return("Modern Pop")
  } else if (genre %in% c("teen pop", "boy band", "bubblegum pop", "folk-pop", "social media pop", "candy pop", "bedroom pop", "poptimism")) {
    return("Teen Pop")
  } else if (genre %in% c("pop", "alternative pop", "canadian pop", "barbadian pop", "australian pop", "irish pop", "belgian pop", "german pop", "chill pop", "native american pop", "french indie pop", "metropopolis")) {
    return("Regional Pop")
  } else {
    return("Other Pop")
  }
}

# Define subgroups within "Hip Hop/Rap"
define_hip_hop_sub_genre <- function(genre) {
  if (genre %in% c("atl hip hop", "southern hip hop", "dirty south rap", "florida rap", "dfw rap")) {
    return("Southern Hip Hop")
  } else if (genre %in% c("east coast hip hop", "gangster rap", "new jersey rap", "new york hip hop", "philadelphia rap")) {
    return("East Coast Hip Hop")
  } else if (genre %in% c("west coast hip hop", "cali rap", "san diego rap")) {
    return("West Coast Hip Hop")
  } else if (genre %in% c("chicago rap", "detroit hip hop", "ohio hip hop")) {
    return("Midwest Hip Hop")
  } else if (genre %in% c("hip hop", "conscious hip hop", "alternative hip hop", "melodic rap", "trap music", "emo rap")) {
    return("Mainstream Hip Hop")
  } else if (genre %in% c("canadian hip hop", "asian american hip hop", "uk hip hop", "dutch hip hop")) {
    return("International Hip Hop")
  } else {
    return("Other Hip Hop")
  }
}

# Define subcategories within "Other"
define_other_sub_genre <- function(genre) {
  if (genre %in% c("australian indie", "icelandic indie", "electro", "aussietronica", "australian psych", "la indie", "indietronica", "irish singer-songwriter", "neo mellow")) {
    return("Indie/Alternative")
  } else if (genre %in% c("big room", "brostep", "complextro", "destroy techno", "downtempo", "eau claire indie", "french shoegaze", "new french touch", "indietronica")) {
    return("Electronic")
  } else if (genre %in% c("adult standards", "alt z", "comic", "emo", "hollywood", "idol", "lilith", "permanent wave", "talent show")) {
    return("Pop Variants")
  } else if (genre %in% c("afrofuturism", "afroswing", "reggae fusion")) {
    return("Fusion/World")
  } else if (genre %in% c("basshall", "grime", "uk drill")) {
    return("Other")
  } else {
    return(genre)
  }
}


# Define genre groupings
genre_groups <- spotify_clean %>%
  mutate(genre_group = case_when(
    grepl("pop", `top.genre`, ignore.case = TRUE) ~ "Pop",
    grepl("hip hop|rap", `top.genre`, ignore.case = TRUE) ~ "Hip Hop/Rap",
    grepl("rock|metal", `top.genre`, ignore.case = TRUE) ~ "Rock",
    grepl("edm|dance|house|electronic", `top.genre`, ignore.case = TRUE) ~ "Electronic/Dance",
    grepl("r&b|soul|funk", `top.genre`, ignore.case = TRUE) ~ "R&B/Soul",
    grepl("latin|reggaeton", `top.genre`, ignore.case = TRUE) ~ "Latin",
    grepl("country", `top.genre`, ignore.case = TRUE) ~ "Country",
    grepl("folk|americana", `top.genre`, ignore.case = TRUE) ~ "Folk",
    grepl("jazz|blues", `top.genre`, ignore.case = TRUE) ~ "Jazz/Blues",
    TRUE ~ "Other"
  ))

# Count the number of songs in each individual genre within each genre group
genre_counts <- genre_groups %>%
  group_by(genre_group, `top.genre`) %>%
  summarize(count = n(), .groups = 'drop')

# Add sub-genre groups to the data
genre_counts <- genre_counts %>%
  mutate(sub_genre_group = case_when(
    genre_group == "Pop" ~ sapply(`top.genre`, define_pop_sub_genre),
    genre_group == "Hip Hop/Rap" ~ sapply(`top.genre`, define_hip_hop_sub_genre),
    genre_group == "Other" ~ sapply(`top.genre`, define_other_sub_genre),
    TRUE ~ genre_group
  ))

# Prepare the data in a hierarchical structure with a third layer for sub-genres
genre_tree <- Node$new("Genres")
for (group in unique(genre_counts$genre_group)) {
  group_node <- genre_tree$AddChild(group)
  group_data <- genre_counts %>% filter(genre_group == group)
  for (sub_group in unique(group_data$sub_genre_group)) {
    sub_group_node <- group_node$AddChild(sub_group)
    sub_group_data <- group_data %>% filter(sub_genre_group == sub_group)
    for (genre in unique(sub_group_data$`top.genre`)) {
      genre_node <- sub_group_node$AddChild(genre)
      genre_node$size <- sub_group_data %>% filter(`top.genre` == genre) %>% pull(count)
    }
  }
}

# Convert to JSON for circlepackeR
genre_tree_json <- ToListExplicit(genre_tree, unname = TRUE)

# Create the Circular Packing plot
circlepackeR(genre_tree_json, size = "size", color_min = "hsl(194,100%,95%)", color_max = "hsl(194,100%,35%)", width = 800, height = 700)

#-----------#

# Define sub-genre groups based on the circlepacker plot logic
define_sub_genre <- function(genre, genre_group) {
  if (genre_group == "Pop") {
    if (genre %in% c("dance pop", "electropop", "synthpop", "pop soul", "pop rock", "pop rap", "indie pop", "art pop", "baroque pop", "chamber pop", "pop punk")) {
      return("Modern Pop")
    } else if (genre %in% c("teen pop", "boy band", "bubblegum pop", "folk-pop", "social media pop", "candy pop", "bedroom pop", "poptimism")) {
      return("Teen Pop")
    } else if (genre %in% c("pop", "alternative pop", "canadian pop", "barbadian pop", "australian pop", "irish pop", "belgian pop", "german pop", "chill pop", "native american pop", "french indie pop", "metropopolis")) {
      return("Regional Pop")
    } else {
      return("Other Pop")
    }
  } else if (genre_group == "Hip Hop/Rap") {
    if (genre %in% c("atl hip hop", "southern hip hop", "dirty south rap", "florida rap", "dfw rap")) {
      return("Southern Hip Hop")
    } else if (genre %in% c("east coast hip hop", "gangster rap", "new jersey rap", "new york hip hop", "philadelphia rap")) {
      return("East Coast Hip Hop")
    } else if (genre %in% c("west coast hip hop", "cali rap", "san diego rap")) {
      return("West Coast Hip Hop")
    } else if (genre %in% c("chicago rap", "detroit hip hop", "ohio hip hop")) {
      return("Midwest Hip Hop")
    } else if (genre %in% c("hip hop", "conscious hip hop", "alternative hip hop", "melodic rap", "trap music", "emo rap")) {
      return("Mainstream Hip Hop")
    } else if (genre %in% c("canadian hip hop", "asian american hip hop", "uk hip hop", "dutch hip hop")) {
      return("International Hip Hop")
    } else {
      return("Other Hip Hop")
    }
  } else if (genre_group == "Other") {
    if (genre %in% c("australian indie", "icelandic indie", "electro", "aussietronica", "australian psych", "la indie", "indietronica", "irish singer-songwriter", "neo mellow")) {
      return("Indie/Alternative")
    } else if (genre %in% c("big room", "brostep", "complextro", "destroy techno", "downtempo", "eau claire indie", "french shoegaze", "new french touch", "indietronica")) {
      return("Electronic")
    } else if (genre %in% c("adult standards", "alt z", "comic", "emo", "hollywood", "idol", "lilith", "permanent wave", "talent show")) {
      return("Pop Variants")
    } else if (genre %in% c("afrofuturism", "afroswing", "reggae fusion")) {
      return("Fusion/World")
    } else if (genre %in% c("basshall", "grime", "uk drill")) {
      return("Other")
    } else {
      return(genre)
    }
  } else {
    return(genre_group)
  }
}

# Apply the sub-genre grouping logic to the data
genre_table <- genre_counts %>%
  rowwise() %>%
  mutate(sub_genre_group = define_sub_genre(`top.genre`, genre_group)) %>%
  ungroup() %>%
  select(genre_group, sub_genre_group, `top.genre`) %>%
  distinct() %>%
  arrange(genre_group, sub_genre_group, `top.genre`)


# Apply the sub-genre grouping logic to the data
genre_table <- genre_counts %>%
  rowwise() %>%
  mutate(sub_genre_group = define_sub_genre(`top.genre`, genre_group)) %>%
  ungroup() %>%
  select(genre_group, sub_genre_group, `top.genre` ,count) %>%
  distinct() %>%
  arrange(genre_group, sub_genre_group, `top.genre`)

# Create an interactive datatable using DT
datatable(genre_table,
          options = list(pageLength = 10, 
                         autoWidth = TRUE, 
                         searching = TRUE, 
                         ordering = TRUE, 
                         dom = 'Bfrtip', 
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), 
          rownames = FALSE)

# ----------------------- Num of songs per year per genre group ----------------- #

genre_year_data <- genre_groups %>%
  group_by(year.released, genre_group) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(year.released)


# Create the interactive line plot with ggiraph
interactive_plot <- plot_ly(
  data = genre_year_data,
  x = ~year.released,
  y = ~count,
  color = ~genre_group,
  type = 'scatter',
  mode = 'lines+markers',
  line = list(width = 2),
  text = ~paste('Year:', year.released, '<br> Songs:', count, '<br> Genre Group:', genre_group), # Custom annotation text
  hoverinfo = 'text'  # Use the custom text for hover info
) %>%
  layout(
    #title = "Number of Songs per Year by Genre Group",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Count"),
    hovermode = "closest"
  )

interactive_plot <- onRender(interactive_plot, "
  function(el, x) {
    el.on('plotly_hover', function(d) {
      var update = {'opacity': 0.2}; // Reduce opacity of all lines
      Plotly.restyle(el, update);

      var update = {'opacity': 1, 'line.width': 4}; // Highlight the hovered line
      Plotly.restyle(el, update, [d.points[0].curveNumber]);
    }).on('plotly_unhover', function(d) {
      var update = {'opacity': 1, 'line.width': 2}; // Revert all lines to original state
      Plotly.restyle(el, update);
    });
  }
")

# ----------------------- Word cloud -------------------------#

# Extract the titles of the songs
song_titles <- spotify_clean$title
# Remove everything after "feat" or "with" (including the words themselves) from each title
song_titles_cleaned <- gsub("(feat|with).*", "", song_titles, ignore.case = TRUE)

# Convert titles to a text corpus
corpus <- Corpus(VectorSource(song_titles_cleaned))

# Preprocess the text: convert to lower case, remove punctuation, numbers, stopwords, and specific words
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, c(stopwords("english"), "radio", "remix", "edit"))

# Create a document-term matrix
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)

# Calculate word frequencies
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)

# -------------------------- Number of unique artists by artist type -----------------#

# Count unique artists for each artist type
unique_artist_counts <- spotify_clean %>%
  select(artist.type, artist) %>%
  distinct() %>%  # Keep unique artist-type combinations
  group_by(artist.type) %>%
  summarize(count = n(), .groups = 'drop')

# ----------------------- Number of unique artists by artist type each year -------------#

# Prepare the data: Count unique artists by year and artist type
unique_artist_counts_yearly <- spotify_clean %>%
  select(year.released, artist.type, artist) %>%
  distinct() %>%  # Keep unique year-artist-type combinations
  group_by(year.released, artist.type) %>%
  summarize(count = n(), .groups = 'drop')

# Create the animated line plot
animated_plot <- ggplot(unique_artist_counts_yearly, aes(x = year.released, y = count, group = artist.type, color = artist.type)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of Unique Artists by Artist Type Over the Years",
       x = "Year",
       y = "Number of Unique Artists",
       color = "Artist Type") +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  transition_reveal(year.released)



# ----------------------- Number of songs by artists type ----------- #

# Aggregate the number of songs by each artist type
artist_type_counts <- spotify_clean %>%
  group_by(artist.type) %>%
  summarize(count = n(), .groups = 'drop')

# ----------------------- Number of songs by artists type over years ---------------- #




# -------------------------- UI ------------------------------------#

ui <- navbarPage(
  "Spotify",
  useShinyjs(), 
  tags$style(HTML("
  /* Default styling for tab items (li elements) on load */
  .tabbable > .nav.nav-tabs > li.nav-item > a.nav-link {
    background-color: #ffffff; /* Background color for inactive tabs */
    color: black; /* Text color for inactive tabs */
  }
  
  /* Styling for the active tab */
  .tabbable > .nav.nav-tabs > li.nav-item > a.nav-link.active,
  .tabbable > .nav.nav-tabs > li.nav-item > a.nav-link.active:focus {
    background-color: #168f40; /* Background color for active tab */
    color: white; /* Text color for active tab */
  }

  /* Styling for tab items on hover, only if they are not active */
  .tabbable > .nav.nav-tabs > li.nav-item > a:hover {
    background-color: #1DB954; /* Background color on hover */
    color: white; /* Text color on hover */
  }
")),
  navbarMenu("Songs",
             tabPanel("Basic Info",
                      page_fillable(
                        tabsetPanel(
                          tabPanel(
                            title = "Number of songs",
                            layout_columns(
                              card(
                                height = 600,
                                full_screen = TRUE,
                                card_header(class = "bg-success", "Number of songs released each year"),
                                card_body(
                                  plotlyOutput("releaseYearPlot", height = "500px"),
                                  fill = TRUE
                                ),
                                class = "mb-2"
                              ),
                              card(
                                height = 600,
                                full_screen = TRUE,
                                card_header(class = "bg-success", "Table with number of released songs each year"),
                                card_body(
                                  DTOutput("releaseYearTable"),
                                  fill = TRUE
                                ),
                                class = "mb-2"
                              ),
                              col_widths = c(8, 4)
                            )
                          ),
                          tabPanel(
                            title = "Word Cloud",
                            layout_columns(
                              card(
                                height = 700,
                                full_screen = TRUE,
                                card_header(class = "bg-success", "Popular words from song titles"),
                                card_body(
                                  wordcloud2Output("wordcloud", height = "400px", width = "100%"),
                                  fill = TRUE
                                ),
                                
                                class = "mb-2"
                              ),
                              card(
                                height = 700,
                                full_screen = TRUE,
                                card_header(class = "bg-success", "All the words from song titles"),
                                card_body(
                                  DTOutput("allWordsTable"),
                                  fill = TRUE
                                ),
                                class = "mb-2"
                            ),
                            col_widths = c(8,4)
                            )
                          ),
                        selected = "Number of songs")
                      )),
             tabPanel("Popular songs",
                      page_fillable(
                        card(
                          height = 800,
                          full_screen = TRUE,
                          card_header(class = "bg-success", "Popular songs - all time and through years"),
                          layout_sidebar(
                           sidebar = sidebar(sliderInput("songsSlider", "Select number of songs you want to display", min = 1, max = 10, value = 3),
                                       selectInput("metric", "Select Metric:", choices = c("All Time", "Release Years", "Each year")),
                                      radioButtons("songsPopular", "Select top or bottom", choices = c("Top", "Bottom")) ),
                           card_body(
                             plotlyOutput("topSongsPlot", height = "500px"),
                             fill = TRUE
                           ),
                           class = "mb-3"
                          )
                        )
                      )
                     
             ),
             tabPanel("Song Features",
                      page_fillable(
                        card(
                          height = 550,
                          full_screen = TRUE,
                          card_header(class = "bg-success", "Song Features"),
                          layout_sidebar(
                            fillable = TRUE,
                            sidebar = checkboxGroupInput("plots", "Select features to display:",
                                                         choices = list("BPM" = "bpmPlot",
                                                                        "Energy" = "energyPlot",
                                                                        "Dance" = "dncePlot",
                                                                        "Decibel" = "dbPlot",
                                                                        "Speechiness" = "spchPlot",
                                                                        "Acoustic" = "acousPlot",
                                                                        "Valence" = "valPlot",
                                                                        "Duration" = "durPlot"),
                                                         selected = c("bpmPlot", "energyPlot", "dncePlot", "dbPlot", "spchPlot", "acousPlot", "valPlot", "durPlot")),
                            card_body(
                              uiOutput("plotBoxes"),
                              fill = TRUE
                            ),
                            class = "mb-2"
                          )
                        ),
                        card(
                          height = 980,
                          full_screen = TRUE,
                          card_header(class = "bg-success", "Features vs. Popularity"),
                          layout_sidebar(
                            fillable = TRUE,
                            sidebar = checkboxGroupInput("fvsp", "Select features to display:",
                                                         choices = list("BPM" = "bpmPlotVsPop",
                                                                        "Energy" = "energyPlotVsPop",
                                                                        "Dance" = "dncePlotVsPop",
                                                                        "Decibel" = "dbPlotVsPop",
                                                                        "Speechiness" = "spchPlotVsPop",
                                                                        "Acoustic" = "acousPlotVsPop",
                                                                        "Valence" = "valPlotVsPop",
                                                                        "Duration" = "durPlotVsPop"),
                                                         selected = c("bpmPlotVsPop", "energyPlotVsPop", "dncePlotVsPop", "dbPlotVsPop", "spchPlotVsPop", "acousPlotVsPop", "valPlotVsPop", "durPlotVsPop")),
                            card_body(
                              uiOutput("fvspPlotBoxes"),
                              fill = TRUE
                            ),
                            class = "mb-5"
                            )
                        )
                      ))
  ),
  navbarMenu("Genres",
             tabPanel("Basic Info",
                        page_fillable(
                          layout_columns(
                            card(
                              height = 450,
                              full_screen = TRUE,
                              card_header(class= "bg-success","Songs per Year by Genre Group"),
                              class = "mb-2",  # Margin-bottom
                              card_body(plotlyOutput("linePlot", height = "400px"), fill = TRUE)
                            ),
                            card(
                              height = 450,
                              full_screen = TRUE,
                              title= "Genre groupings",
                              class = "mb-2", 
                              card_header(class= "bg-success", "Genre groupings"),
                              card_body(circlepackeROutput("circlePlot", height = "400px", width = "400px"))
                            ),
                            card(
                              height = 500,
                              class = "mb-5",
                              full_screen = TRUE,
                              card_header(class= "bg-success", "Genre groupings table"),
                              DTOutput("genreTable")
                            ),
                            col_widths = c(6, 6, 12)
                        )      
                      )
                    ),
             tabPanel("Popular Genres", 
                      tags$div(class = "custom-tabset", 
                      tabsetPanel(
                        # General about Genres Subtab
                        tabPanel("General about Genres",
                                 page_fillable(
                                   card(
                                     height = 720,
                                     full_screen = TRUE,
                                     layout_sidebar(
                                       sidebar = sidebar(
                                         width = 350,
                                         sliderInput("genresGeneralSlider", "Select number of genres you want to display", min = 1, max = 10, value = 5),
                                         selectInput("GenresTime", "Select Time Frame:", choices = c("All Time", "Release Years", "Release Years + Top Years", "Each year", "Each year + Release year")),
                                         radioButtons("genresPopular", "Select top or bottom genres by popularity score", choices = c("Top", "Bottom"))
                                       ),
                                       card_body(
                                         plotlyOutput("generalGenrePlot", height = "700px"),
                                         fill = TRUE
                                       ),
                                       class = "mb-3"
                                     )
                                   )
                                 )
                        ),
                        
                        # All Genre Groups Subtab
                        tabPanel("General about Genre Groups",
                                 page_fillable(
                                   card(
                                     height = 720,
                                     full_screen = TRUE,
                                     layout_sidebar(
                                       sidebar = sidebar(
                                         width = 350,
                                         sliderInput("genresGroupGeneralSlider", "Select Number of Genre Groups to display:", min = 1, max = 9, value = 3),
                                         selectInput("GenreGroupsTime", "Select Time Frame:", choices = c("All Time", "Release Years", "Each Year")),
                                         radioButtons("genreGroupsPopular", "Select top or bottom genre groups by popularity score", choices = c("Top", "Bottom"))
                                        ), 
                                        card_body(
                                          plotlyOutput("allGenreGroupsPlot", height = "700px") ,
                                          fill = TRUE
                                        ),
                                       class = "mb-3"
                                     )
                                   )
                                 )
                                 
                        ),
                        
                        # Individual Genre Groups Subtab
                        tabPanel("Individual Genre Groups",
                                 page_fillable(
                                   card(
                                     height = 720,
                                     full_screen = TRUE,
                                     layout_sidebar(
                                       sidebar = sidebar(
                                         width = 350,
                                         sliderInput("individualGenresSlider", "Select Number of Genres to display:", min = 1, max = 10, value = 3),
                                         selectInput("IndividualGenresTime", "Select Time Frame:", choices = c("All Time", "Release Years", "Each Year")),
                                         selectInput("individualGenresChoose", "Select Genre Group", choices = c("Pop", "Hip Hop/Rap", "Rock", "Electronic/Dance", "R&B/Soul", "Latin", "Country", "Folk", "Other"))
                                       ),
                                       card_body(
                                         plotlyOutput("allIndividualGenresPlot", height = "700px"),
                                         fill = TRUE
                                       ),
                                       class = "mb-3"
                                     )
                                   )
                                 )
                                 
                        )
                      )
             )
  )),
  navbarMenu("Artists",
             tabPanel("Basic Info", 
                      layout_columns(
                        card(
                          height = 480,
                          full_screen = TRUE,
                          card_header(class = "bg-success", "Unique Artists by Artist Type"),
                          class = "mb-2",
                          card_body(
                            fluidRow(
                              column(6, plotlyOutput("artistTypeBarPlot", height = "400px", width = "100%")),
                              column(6, plotlyOutput("artistTypeLinePlot", height = "400px"))
                            )
                          ) 
                        ),
                        card(
                          height = 480,
                          full_screen = TRUE,
                          card_header(class = "bg-success", "Songs By Artist Type"),
                          class = "mb-4",
                          card_body(
                            fluidRow(
                              column(6, plotlyOutput("artistTypeSongStaticPlot", height = "400px", width = "100%")),
                              column(6, plotlyOutput("artistTypeSongLinePlot", height = "400px") )
                            )
                          )
                        ),
                        col_widths = c(12,12)
                      )
             ),
             tabPanel("Popular artists",
                      tags$div(class = "custom-tabset", 
                               tabsetPanel(
                                 # General about Genres Subtab
                                 tabPanel("All Artists",
                                          page_fillable(
                                            card(
                                              height = 720,
                                              full_screen = TRUE,
                                              layout_sidebar(
                                                sidebar = sidebar(
                                                  width = 350,
                                                  sliderInput("allArtistsSlider", "Select number of artists to display", min = 1, max = 10, value = 5),
                                                  selectInput("ArtistsTime", "Select Time Frame:", choices = c("All Time", "Release Years", "Each year")),
                                                  radioButtons("artistsPopular", "Select top or bottom artists by popularity score", choices = c("Top", "Bottom"))
                                                ),
                                                card_body(
                                                  plotlyOutput("allArtistsPlot", height = "700px"),
                                                  fill = TRUE
                                                ),
                                                class = "mb-3"
                                              )
                                            )
                                          )
                                 ),
                                 
                                 # All Genre Groups Subtab
                                 tabPanel("All Artists Types",
                                          page_fillable(
                                            card(
                                              height = 720,
                                              full_screen = TRUE,
                                              layout_sidebar(
                                                sidebar = sidebar(
                                                  width = 350,
                                                  selectInput("allArtistsTypesTime", "Select Time Frame:", choices = c("All Time", "Release Years", "Each Year"))
                                                ), 
                                                card_body(
                                                  plotlyOutput("allArtistsTypesPlot", height = "700px") ,
                                                  fill = TRUE
                                                ),
                                                class = "mb-3"
                                              )
                                            )
                                          )
                                          
                                 ),
                                 tabPanel("Individual Artists Types",
                                          page_fillable(
                                            card(
                                              height = 720,
                                              full_screen = TRUE,
                                              layout_sidebar(
                                                sidebar = sidebar(
                                                  width = 350,
                                                  sliderInput("individualAristsTypesSlider", "Select Number of Artists to display:", min = 1, max = 10, value = 3),
                                                  selectInput("individualArtistsTypesTime", "Select Time Frame:", choices = c("All Time", "Release Years", "Each Year")),
                                                  selectInput("individualArtistsTypesSelect", "Select type of artist to display", choices = c("Solo", "Duo", "Trio", "Band/Group")),
                                                  radioButtons("individualArtistsTypesPopular", "Select top or bottom artists by popularity score", choices = c("Top", "Bottom"))
                                                ), 
                                                card_body(
                                                  plotlyOutput("individualArtistsTypesPlot", height = "700px") ,
                                                  fill = TRUE
                                                ),
                                                class = "mb-3"
                                              )
                                            )
                                          )
                                          
                                 )
                               )
                      )),
             tabPanel("Number of songs by artists", 
                      page_fillable(
                        card(
                          height = 800,
                          full_screen = TRUE,
                          card_header(class = "bg-success", "Number of songs by artists - all time and through years"),
                          layout_sidebar(
                            sidebar = sidebar(sliderInput("artistsSlider", "Select number of artists to display", min = 1, max = 10, value = 3),
                                              selectInput("metric", "Select Time Frame:", choices = c("All Time", "Release Years", "Each year"))
                            ),
                            card_body(
                              plotlyOutput("numArtistsPlot", height = "500px"),
                              fill = TRUE
                            ),
                            class = "mb-3"
                          )
                        )
                      )
             )
             
  )
)

server <- function(input, output, session) {
  
  runjs('$("#popularityTabs li:eq(0) a").tab("show");')
  # ---------------------------------------------------------- #
  # ---------------------- SONGS ----------------------------- #
  # ---------------------------------------------------------- #
  
  # ------- Number of songs released per year ---------------- #
  

  output$releaseYearPlot <- renderPlotly({
    # Ensure year.released is numeric
    spotify_clean$year.released <- as.numeric(spotify_clean$year.released)
    
    # Preprocess the data: count the number of songs released each year
    yearly_counts <- spotify_clean %>%
      group_by(year.released) %>%
      summarize(count = n(), .groups = 'drop')
    
    
    releaseYear_p <- yearly_counts %>%
      ggplot(aes(x = year.released, y = count)) +
      geom_area(fill = "#69b3a2", alpha = 0.5) +
      geom_line(color = "#69b3a2") +
      scale_x_continuous(breaks = seq(min(yearly_counts$year.released), max(yearly_counts$year.released), by = 3)) +  # Ensure x-axis shows only whole years
      ylab("Number of Songs Released") +
      xlab("Year Released") +
      theme_ipsum()
    
    # Make plot interactive with ggplotly
    releaseYear_p <- ggplotly(releaseYear_p)
    
    # Round values and update hover text
    for (i in seq_along(releaseYear_p$x$data)) {
      releaseYear_p$x$data[[i]]$text <- paste("Year: ", round(releaseYear_p$x$data[[i]]$x), "<br>Count: ", round(releaseYear_p$x$data[[i]]$y))
    }
    
  
    releaseYear_p <- releaseYear_p %>% layout(
      hoverlabel = list(
        bgcolor = "white",
        bordercolor = "#191414",
        font = list(color = "#191414")
      ),
      hovermode = "closest"
    )
    
    releaseYear_p
  })
  
  # ------ Number of songs released per year - Table --------- #
  
  # Preprocess the data: count the number of songs released each year
  yearly_counts <- spotify_clean %>%
    group_by(year.released) %>%
    summarize(count = n(), .groups = 'drop')
  
  # Render the table
  output$releaseYearTable <- renderDT({
    datatable(
      yearly_counts,
      options = list(
        pageLength = 11,
        autoWidth = TRUE,
        dom = 't',
        columnDefs = list(list(className = 'dt-center', targets = '_all')),
        searching = FALSE,
        ordering = TRUE
      ),
      rownames = FALSE,
      class = "cell-border stripe"
    )
  })
  
  
  # --------------- Popular songs ----------------------------- #
  
  spotify_clean <- spotify_clean %>%
  mutate(unique_title = paste(title, "by", artist))
  
  # Reactive - generating the correct plot based on user inputs
  output$topSongsPlot <- renderPlotly({
    
    # Get the number of songs to display and the metric
    num_songs <- input$songsSlider
    metric <- input$metric
    popular_choice <- input$songsPopular
    
    all_time <- spotify_clean |>
      select(-top.year) |>
      group_by(title, artist, year.released) |>
      distinct() |> 
      ungroup()
    
    # Case 1: "All Time" selected
    if (metric == "All Time") {
      
      if (popular_choice == "Top") {
        # Get the top N popular songs
        selected_songs <- all_time %>%
          arrange(desc(pop)) %>%
          slice_head(n = num_songs)  %>%
          mutate(unique_title = factor(unique_title, levels = unique(unique_title)))
        
        # Create the lollipop plot for top N songs
        plot <- ggplot(selected_songs, aes(x = unique_title, y = pop)) +
          geom_segment(aes(x = unique_title, xend = unique_title, y = 0, yend = pop), color = "#191414") +
          geom_point(aes(text = paste("Song: ", title, "<br>Artist: ", artist, "<br>Popularity: ", pop)), color = "#1DB954", size = 3) +  
          labs(title = paste("Top", num_songs, "Songs by Popularity"),
               x = "Song",
               y = "Popularity Score") +
          scale_y_continuous(breaks = seq(0 ,100, by = 10), limits = c(0, 100)) +  # Start y ticks from 0
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels for readability
        
        # Convert ggplot to plotly for interactivity
        plot_interactive <- ggplotly(plot, tooltip = "text") %>%
          layout(
            hovermode = "closest",
            xaxis = list(categoryorder = "array", categoryarray = unique(selected_songs$unique_title))  # Maintain song order
          )
        
      } else {
        # Get the bottom N popular songs
        selected_songs <- all_time %>%
          arrange(pop) %>%
          slice_head(n = num_songs)  %>%
          mutate(unique_title = factor(unique_title, levels = unique(unique_title)))
        
        # Create the lollipop plot for bottom N songs
        plot <- ggplot(selected_songs, aes(x = unique_title, y = pop)) +
          geom_segment(aes(x = unique_title, xend = unique_title, y = 0, yend = pop), color = "#191414") +
          geom_point(aes(text = paste("Song: ", title, "<br>Artist: ", artist, "<br>Popularity: ", pop)), color = "#1DB954", size = 3) +
          labs(title = paste("Bottom", num_songs, "Songs by Popularity"),
               x = "Song",
               y = "Popularity Score") +
          scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +  # Y-axis goes up to 100 with ticks every 10
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels for readability
        
        # Convert ggplot to plotly for interactivity
        plot_interactive <- ggplotly(plot, tooltip = "text") %>%
          layout(
            hovermode = "closest",
            xaxis = list(categoryorder = "array", categoryarray = unique(selected_songs$unique_title))  # Maintain song order
          )
      }
      
      # Case 2: "Release Years" selected
    } else if (metric == "Release Years") {
      
      if (popular_choice == "Top") {
        plot <- create_bar_plot(num_songs, metric = "Release Years", popular_choice = popular_choice)
      } else {
        plot <- create_bar_plot(num_songs, metric = "Release Years", popular_choice = popular_choice)
      }
      
      # Case 3: "Each Year" selected
    } else if (metric == "Each year") {
      
      if (popular_choice == "Top") {
        plot <- create_bar_plot_top(num_songs, metric = "Each year", popular_choice = popular_choice)
      } else {
        plot <- create_bar_plot_top(num_songs, metric = "Each year", popular_choice = popular_choice)
      }
    }
    
    return(plot)
  })
  
  
  wrap_text <- function(text, width = 15) {
    if (nchar(text) > width) {
      return(paste0(substr(text, 1, width - 3), "..."))
    }
    return(text)
  }
  
  # Create the bar plot for top or bottom songs by release year
  create_bar_plot <- function(top_n = 3, metric, popular_choice) {
    # Group data by release year and song, summing the popularity scores
    song_popularity <- spotify_clean %>%
      group_by(year.released, title, artist) %>%
      summarize(total_popularity = pop, .groups = 'drop') |>
      distinct()
    
    # Calculate the total popularity for all songs per release year
    total_popularity_per_year <- song_popularity %>%
      group_by(year.released) %>%
      summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top or bottom N songs per release year
    top_songs <- song_popularity %>%
      inner_join(total_popularity_per_year, by = "year.released") %>%
      group_by(year.released) %>%
      arrange(year.released, if(popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        prop = total_popularity / yearly_total_popularity,
        hover_text = paste("Song:", title, "<br>Artist:", artist, 
                           "<br>Popularity:", total_popularity, 
                           "<br>Release Year:", year.released),
        wrapped_title = sapply(title, wrap_text)
      )
    
    # Create the stacked bar plot
    p <- ggplot(top_songs, aes(x = as.factor(year.released), y = prop, fill = title, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_title), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Songs by Popularity (Release Years)"),
           x = "Release Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none") 
    
    # Make the plot interactive with plotly
    p_interactive <- ggplotly(p, tooltip = "text")
    
    return(p_interactive)
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  create_bar_plot_top <- function(top_n = 3, metric, popular_choice) {
    # Group data by release year and song, summing the popularity scores
    song_popularity <- spotify_clean %>%
      group_by(top.year, title, artist) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop')
    
    # Calculate the total popularity for all songs per release year
    total_popularity_per_year <- song_popularity %>%
      group_by(top.year) %>%
      summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top or bottom N songs per release year
    top_songs <- song_popularity %>%
      inner_join(total_popularity_per_year, by = "top.year") %>%
      group_by(top.year) %>%
      arrange(top.year, if(popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        prop = total_popularity / yearly_total_popularity,
        hover_text = paste("Song:", title, "<br>Artist:", artist, 
                           "<br>Popularity:", total_popularity, 
                           "<br>Year:", top.year),
        wrapped_title = sapply(title, wrap_text)
      )
    
    # Create the stacked bar plot
    p <- ggplot(top_songs, aes(x = as.factor(top.year), y = prop, fill = title, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_title), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Songs by Popularity (Each Year)"),
           x = "Each Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none")  
    
    # Make the plot interactive with plotly
    p_interactive <- ggplotly(p, tooltip = "text")
    
    return(p_interactive)
  }
  
  # -------------------- Feature plots ----------------------- #
  
  # Function to render plots conditionally
  renderFeaturePlot <- function(plot_name, ggplot_object) {
    output[[plot_name]] <- renderPlotly({
      req(plot_name %in% input$plots)
      ggplotly(ggplot_object)
    })
  }
  
  # Initialize plot rendering
  observe({
    renderFeaturePlot("bpmPlot", ggplot(spotify_clean, aes(x = bpm)) + 
                        geom_histogram(binwidth = 10, fill = "#1DB954", color = "#191414") + 
                        ggtitle("BPM") + xlab("Beats Per Minute") + ylab("Number of songs") + theme_minimal())
    
    renderFeaturePlot("energyPlot", ggplot(spotify_clean, aes(x = nrgy)) + 
                        geom_histogram(binwidth = 10, fill = "#1DB954", color = "#191414") + 
                        ggtitle("Energy") + xlab("Energy") + ylab("") + theme_minimal())
    
    renderFeaturePlot("dncePlot", ggplot(spotify_clean, aes(x = dnce)) + 
                        geom_histogram(binwidth = 10, fill = "#1DB954", color = "#191414") + 
                        ggtitle("Danceability") + xlab("Danceability") + ylab("Number of songs") + theme_minimal())
    
    renderFeaturePlot("dbPlot", ggplot(spotify_clean, aes(x = dB)) + 
                        geom_histogram(binwidth = 1, fill = "#1DB954", color = "#191414") + 
                        ggtitle("Loudness") + xlab("Loudness in Decibels") + ylab("Number of songs") + theme_minimal())
    
    renderFeaturePlot("spchPlot", ggplot(spotify_clean, aes(x = spch)) + 
                        geom_histogram(binwidth = 1, fill = "#1DB954", color = "#191414") + 
                        ggtitle("Speechiness") + xlab("Speechiness") + ylab("Number of songs") + theme_minimal())
    
    renderFeaturePlot("acousPlot", ggplot(spotify_clean, aes(x = acous)) + 
                        geom_histogram(binwidth = 10, fill = "#1DB954", color = "#191414") + 
                        ggtitle("Acousticness") + xlab("Acousticness") + ylab("Number of songs") + theme_minimal())
    
    renderFeaturePlot("valPlot", ggplot(spotify_clean, aes(x = val)) + 
                        geom_histogram(binwidth = 10, fill = "#1DB954", color = "#191414") + 
                        ggtitle("Valence") + xlab("Valence - positive mood") + ylab("Number of songs") + theme_minimal())
    
    renderFeaturePlot("durPlot", ggplot(spotify_clean, aes(x = dur)) + 
                        geom_histogram(binwidth = 10, fill = "#1DB954", color = "#191414") + 
                        ggtitle("Duration") + xlab("Duration in seconds") + ylab("Number of songs") + theme_minimal())
  })
  
  # Dynamically generate plot boxes based on selection
  output$plotBoxes <- renderUI({
    plotList <- lapply(input$plots, function(plot_name) {
      column(3, plotlyOutput(plot_name, height = "200px"))
    })
    
    do.call(fluidRow, plotList)
  })
  
  # ------------ Feature vs popularity plots ------------------ #
  
  # Generate the plots
  p_bpm <- ggplot(spotify_clean, aes(x = bpm, y = pop, color = bpm, text = paste("BPM: ", bpm, "<br>Popularity: ", pop))) +
    geom_point(alpha = 0.85) +
    scale_color_viridis_c() +
    labs(title = "BPM Versus Popularity", x = "BPM", y = "Popularity") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_nrgy <- ggplot(spotify_clean, aes(x = nrgy, y = pop, color = nrgy, text = paste("Energy: ", nrgy, "<br>Popularity: ", pop))) +
    geom_point(alpha = 0.85) +
    scale_color_viridis_c() +
    labs(title = "Energy Versus Popularity", x = "Energy", y = "Popularity") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_dnce <- ggplot(spotify_clean, aes(x = dnce, y = pop, color = dnce, text = paste("Danceability: ", dnce, "<br>Popularity: ", pop))) +
    geom_point(alpha = 0.85) +
    scale_color_viridis_c() +
    labs(title = "Danceability Versus Popularity", x = "Danceability", y = "Popularity") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_val <- ggplot(spotify_clean, aes(x = val, y = pop, color = val, text = paste("Valence: ", val, "<br>Popularity: ", pop))) +
    geom_point(alpha = 0.85) +
    scale_color_viridis_c() +
    labs(title = "Valence Versus Popularity", x = "Valence", y = "Popularity") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_dur <- ggplot(spotify_clean, aes(x = dur, y = pop, color = dur, text = paste("Duration: ", dur, "<br>Popularity: ", pop))) +
    geom_point(alpha = 0.85) +
    scale_color_viridis_c() +
    labs(title = "Duration Versus Popularity", x = "Duration in seconds", y = "Popularity") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_acous <- ggplot(spotify_clean, aes(x = acous, y = pop, color = acous, text = paste("Acousticness: ", acous, "<br>Popularity: ", pop))) +
    geom_point(alpha = 0.85) +
    scale_color_viridis_c() +
    labs(title = "Acousticness Versus Popularity", x = "Acousticness", y = "Popularity") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_spch <- ggplot(spotify_clean, aes(x = spch, y = pop, color = spch, text = paste("Speechiness: ", spch, "<br>Popularity: ", pop))) +
    geom_point(alpha = 0.85) +
    scale_color_viridis_c() +
    labs(title = "Speechiness Versus Popularity", x = "Speechiness", y = "Popularity") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_db <- ggplot(spotify_clean, aes(x = dB, y = pop, color = dB, text = paste("Loudness: ", dB, "<br>Popularity ", pop))) +
    geom_point(alpha = 0.85, size = 0.5) +
    scale_color_viridis_c_interactive() +
    labs(title = "Loudness Versus Popularity", x = "Loudness", y = "Popularity") +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5))
  
  # List of plotly objects mapped to their names
  plot_map <- list(
    bpmPlotVsPop = ggplotly(p_bpm, tooltip = "text"),
    energyPlotVsPop = ggplotly(p_nrgy, tooltip = "text"),
    dncePlotVsPop = ggplotly(p_dnce, tooltip = "text"),
    dbPlotVsPop = ggplotly(p_db, tooltip = "text"),
    spchPlotVsPop = ggplotly(p_spch, tooltip = "text"),
    acousPlotVsPop = ggplotly(p_acous, tooltip = "text"),
    valPlotVsPop = ggplotly(p_val, tooltip = "text"),
    durPlotVsPop = ggplotly(p_dur, tooltip = "text")
  )
  
  # Dynamically generate plot boxes based on selection
  output$fvspPlotBoxes <- renderUI({
    req(input$fvsp) 
    
    plotList <- lapply(input$fvsp, function(plot_name) {
      plot_output_id <- paste0(plot_name, "_output")
      output[[plot_output_id]] <- renderPlotly({
        plot_map[[plot_name]]
      })
      column(4, plotlyOutput(plot_output_id, height = "270px"))
    })
    
    do.call(fluidRow, plotList)
  })
  
  
  # -------------------- Word cloud -------------------------- #
  
  
  output$wordcloud <- renderWordcloud2({
    wordcloud2(df, size = 1.4, color = 'random-dark', backgroundColor = "white", shape = "circle")
  })
  
  # ---------------- Table for word cloud -------------------- #
  
  # Render the table of all words with rounded frequencies using DT
  output$allWordsTable <- renderDT({
    df$freq <- round(df$freq, 0)  # Round frequency to no decimals
    
    datatable(
      df,
      options = list(
        searching = TRUE,  # Ensure search is enabled
        pageLength = 10, 
        dom = 'ftip',  # Include 'f' in 'dom' to make sure the search box is rendered
        columnDefs = list(list(className = 'dt-center', targets = '_all')), 
        autoWidth = TRUE
      ),
      rownames = FALSE,
      class = "cell-border stripe"
    )
  })
  
  
  
  
  # ---------------------------------------------------------- #
  # ---------------------- GENRES ---------------------------- #
  # ---------------------------------------------------------- #
  
  
  # ------------ Circlepacker plot --------------------------- #
  
  
  output$circlePlot <- renderCirclepackeR({
    circlepackeR(genre_tree_json, size = "size", color_min = "hsl(194,100%,95%)", color_max = "hsl(194,100%,35%)", width = 500, height = 600)
  })
  
  # ------------ Table for genre groupings ------------------- #
  
  
  output$genreTable <- renderDT({
    datatable(genre_table, 
              options = list(pageLength = 7, 
                             autoWidth = TRUE, 
                             searching = TRUE, 
                             ordering = TRUE, 
                             dom = 'Bfrtip', 
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), 
              rownames = FALSE)
  })
  
  # ----------- Songs per Year by Genre Group ---------------- #
  
  
  output$linePlot <- renderPlotly({
    interactive_plot
  })
  
  
  
  # ------------------- General about genres ----------------------- #
  
  # Reactive - generating the correct plot based on user inputs
  output$generalGenrePlot <- renderPlotly({
    
    # Get the number of genres to display and the metric
    num_genres <- input$genresGeneralSlider
    metric <- input$GenresTime
    popular_choice <- input$genresPopular
    
    # Filter the data based on the metric
    if (metric == "All Time") {
     
      if (popular_choice == "Top") {
        # Get the top N popular genres
        selected_genres <- spotify_clean %>%
          group_by(top.genre) %>%
          summarize(total_popularity = sum(pop)) %>%
          arrange(desc(total_popularity)) %>%
          slice_head(n = num_genres) %>%
          mutate(top.genre = factor(top.genre, levels = unique(top.genre)))
        
        # Create the lollipop plot for top N genres
        plot <- ggplot(selected_genres, aes(x = top.genre, y = total_popularity)) +
          geom_segment(aes(x = top.genre, xend = top.genre, y = 0, yend = total_popularity), color = "#191414") + 
          geom_point(aes(text = paste("Genre: ", top.genre, "<br>Popularity Score: ", total_popularity)), color = "#1DB954", size = 3) +
          labs(title = paste("Top", num_genres, "Genres by Popularity"),
               x = "Genre",
               y = "Total Popularity") +
          scale_y_continuous(breaks = seq(0, max(selected_genres$total_popularity, na.rm = TRUE), by = 5000), limits = c(0, NA)) +  # Set y-axis ticks every 5000, up to max value
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels for readability
        
        # Convert ggplot to plotly for interactivity
        plot_interactive <- ggplotly(plot, tooltip = "text") %>%
          layout(
            hovermode = "closest",
            xaxis = list(categoryorder = "array", categoryarray = unique(selected_genres$top.genre))  # Maintain genre order
          )
      } else {
        # Get the bottom N popular genres
        selected_genres <- spotify_clean %>%
          group_by(top.genre) %>%
          summarize(total_popularity = sum(pop)) %>%
          arrange(total_popularity) %>%
          slice_head(n = num_genres) %>%
          mutate(top.genre = factor(top.genre, levels = unique(top.genre)))
        
        # Create the lollipop plot for bottom N genres
        plot <- ggplot(selected_genres, aes(x = top.genre, y = total_popularity)) +
          geom_segment(aes(x = top.genre, xend = top.genre, y = 0, yend = total_popularity), color = "#191414") +  
          geom_point(aes(text = paste("Genre: ", top.genre, "<br>Popularity Score: ", total_popularity)), color = "#1DB954", size = 3) +
          labs(title = paste("Bottom", num_genres, "Genres by Popularity"),
               x = "Genre",
               y = "Total Popularity") +
          scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +  # Y-axis goes up to 100 with ticks every 10
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels for readability
        
        # Convert ggplot to plotly for interactivity
        plot_interactive <- ggplotly(plot, tooltip = "text") %>%
          layout(
            hovermode = "closest",
            xaxis = list(categoryorder = "array", categoryarray = unique(selected_genres$top.genre))  # Maintain genre order
          )
      }
    }
    else if (metric == "Release Years") {
        
        if(popular_choice == "Top"){
          plot <- create_genre_plot(top_n = num_genres, popular_choice = popular_choice)
        } else {
          plot <- create_genre_plot(top_n = num_genres, popular_choice = popular_choice)
        }
        
      }
    else if (metric == "Release Years + Top Years"){
        if(popular_choice == "Top"){
          plot <- create_genre_plot_with_top_year(top_n = num_genres, popular_choice = popular_choice)
        } else {
          plot <- create_genre_plot_with_top_year(top_n = num_genres, popular_choice = popular_choice)
        }
    }
    
    else if (metric == "Each year"){
      if (popular_choice == "Top"){
        plot <- create_genre_plot_by_top_year(top_n = num_genres, popular_choice = popular_choice)
      } else {
        plot <- create_genre_plot_by_top_year(top_n = num_genres, popular_choice = popular_choice)
      }
    }
    
    else if (metric == "Each year + Release year"){
      if (popular_choice == "Top"){
        plot <- create_genre_plot_each_release_year(top_n = num_genres, popular_choice = popular_choice)
      } else {
        plot <- create_genre_plot_each_release_year(top_n = num_genres, popular_choice = popular_choice)
      }
    }
    
  })
  
  
  
  # ~~~~~~~~ Genre general plot - release years
  
  # Function to create the bar plot for top or bottom genres by release year
  create_genre_plot <- function(top_n = 3, popular_choice) {
    # Group data by release year and genre, summing the popularity scores
    genre_popularity <- spotify_clean %>%
      group_by(year.released, top.genre) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop')
    
    # Calculate the total popularity for all genres per year
    total_popularity_per_year <- genre_popularity %>%
      group_by(year.released) %>%
      summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top or bottom N genres per year
    top_genres <- genre_popularity %>%
      inner_join(total_popularity_per_year, by = "year.released") %>%
      group_by(year.released) %>%
      arrange(year.released, if (popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        prop = total_popularity / yearly_total_popularity,
        hover_text = paste("Genre:", top.genre, "<br>Popularity:", total_popularity, "<br>Year:", year.released),
        wrapped_genre = sapply(top.genre, function(x) {
          if (nchar(x) > 15) {
            paste0(substr(x, 1, 12), "...")
          } else {
            x
          }
        })
      )
    
    # Create the stacked bar plot
    p <- ggplot(top_genres, aes(x = as.factor(year.released), y = prop, fill = top.genre, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_genre), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Genres by Popularity (Release Years)"),
           x = "Release Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Make the plot interactive with plotly
    p_interactive <- ggplotly(p, tooltip = "text")
    
    return(p_interactive)
  }
  
  # ~~~~~~~~~~ Genre general plot - release years + top years
  
  # Function to create the plot for top genres by release year with additional top year info
  create_genre_plot_with_top_year <- function(top_n = 3, popular_choice) {
    
    # Group data by release year, genre, and top year, summing the popularity scores
    genre_popularity_top_year <- spotify_clean %>%
      group_by(year.released, top.genre, top.year) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop')
    
    # Calculate the total popularity for all genres per release year
    total_popularity_per_release_year <- genre_popularity_top_year %>%
      group_by(year.released) %>%
      summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top or bottom N genres per year
    top_genres_top_year <- genre_popularity_top_year %>%
      inner_join(total_popularity_per_release_year, by = "year.released") %>%
      group_by(year.released) %>%
      arrange(if (popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        prop = total_popularity / yearly_total_popularity,
        hover_text = paste("Genre:", top.genre, "<br>Popularity:", total_popularity, "<br>Release Year:", year.released, "<br>Top Year:", top.year),
        wrapped_genre = sapply(top.genre, function(x) {
          if (nchar(x) > 15) {
            paste0(substr(x, 1, 12), "...")
          } else {
            x
          }
        })
      )
    
    # Create the stacked bar plot
    p_top_year <- ggplot(top_genres_top_year, aes(x = as.factor(year.released), y = prop, fill = top.genre, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_genre), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Genres by Popularity (Release Years + Top Years)"),
           x = "Release Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Make the plot interactive with plotly
    p_interactive_top_year <- ggplotly(p_top_year, tooltip = "text")
    
    return(p_interactive_top_year)
  }

# ~~~~~~~~ Genres general plot - top years
  
  # Function to create the plot for top genres by top year with detailed hover text
  create_genre_plot_by_top_year <- function(top_n = 3, popular_choice) {
    
    # Group data by top year and genre, summing the popularity scores
    genre_popularity_topy <- spotify_clean %>%
      group_by(top.year, top.genre) %>%
      summarize(total_popularity_topy = sum(pop), .groups = 'drop')
    
    # Calculate the total popularity for all genres per top year
    total_popularity_per_year_topye <- genre_popularity_topy %>%
      group_by(top.year) %>%
      summarize(yearly_total_popularity_topy = sum(total_popularity_topy), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top or bottom N genres per top year
    top_genres_topye <- genre_popularity_topy %>%
      inner_join(total_popularity_per_year_topye, by = "top.year") %>%
      group_by(top.year) %>%
      arrange(if (popular_choice == "Top") desc(total_popularity_topy) else total_popularity_topy) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        propor = total_popularity_topy / yearly_total_popularity_topy,
        hover_text = paste("Genre:", top.genre, "<br>Popularity:", total_popularity_topy, "<br>Top Year:", top.year),
        wrapped_genre_topy = sapply(top.genre, function(x) {
          if (nchar(x) > 15) {
            paste0(substr(x, 1, 12), "...")
          } else {
            x
          }
        })
      )
    
    # Create the stacked bar plot
    p_topy <- ggplot(top_genres_topye, aes(x = as.factor(top.year), y = propor, fill = top.genre, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_genre_topy), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Genres by Popularity (Top Year)"),
           x = "Top Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Make the plot interactive with plotly
    p_interactive_topye <- ggplotly(p_topy, tooltip = "text")
    
    return(p_interactive_topye)
  }

  
  # ~~~~~~ Genres general plot - top years + release years
  
  # Function to create the plot for top genres by each year and release year with detailed hover text
  create_genre_plot_each_release_year <- function(top_n = 3, popular_choice) {
    
    # Group data by release year, top year, and genre, summing the popularity scores
    genre_popularity_top <- spotify_clean %>%
      group_by(top.year, top.genre,year.released) %>%
      summarize(total_popularity_topy = sum(pop), .groups = 'drop')
    
    # Calculate the total popularity for all genres per top year
    total_popularity_per_year_topy <- genre_popularity_top %>%
      group_by(top.year) %>%
      summarize(yearly_total_popularity_topy = sum(total_popularity_topy), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top or bottom N genres per top year
    top_genres_topy <- genre_popularity_top %>%
      inner_join(total_popularity_per_year_topy, by = "top.year") %>%
      group_by(top.year) %>%
      arrange(if (popular_choice == "Top") desc(total_popularity_topy) else total_popularity_topy) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        propor = total_popularity_topy / yearly_total_popularity_topy,
        hover_text = paste("Genre:", top.genre, "<br>Popularity:", total_popularity_topy, "<br>Released Year:", year.released, "<br>Top Year:", top.year),
        wrapped_genre_topy = sapply(top.genre, function(x) {
          if (nchar(x) > 15) {
            paste0(substr(x, 1, 12), "...")
          } else {
            x
          }
        })
        
      )
    
    # Create the stacked bar plot
    p <- ggplot(top_genres_topy, aes(x = as.factor(top.year), y = propor, fill = top.genre, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_genre_topy), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Genres by Popularity (Each Year + Release Year)"),
           x = "Top Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Make the plot interactive with plotly
    p_interactive_topy <- ggplotly(p, tooltip = "text")
    
    return(p_interactive_topy)
  }
  
  
# ---------------- General about Genre Groups ---------------------- #
  
  # Reactive - generating the correct plot based on user inputs
  output$allGenreGroupsPlot <- renderPlotly({
    
    # Get the number of genre groups to display and the selected time frame
    num_genre_groups <- input$genresGroupGeneralSlider
    time_frame <- input$GenreGroupsTime
    popular_choice <- input$genreGroupsPopular
    
    # Case 1: "All Time" selected
    if (time_frame == "All Time") {
      
      if (popular_choice == "Top") {
        # Get the top N genre groups by total popularity
        selected_genre_groups <- genre_groups %>%
          group_by(genre_group) %>%
          summarize(total_popularity = sum(pop), .groups = 'drop') %>%
          arrange(desc(total_popularity)) %>%
          slice_head(n = num_genre_groups) %>%
          mutate(genre_group = factor(genre_group, levels = unique(genre_group)))
        
        # Create the lollipop plot for top N genre groups
        plot <- ggplot(selected_genre_groups, aes(x = genre_group, y = total_popularity)) +
          geom_segment(aes(x = genre_group, xend = genre_group, y = 0, yend = total_popularity), color = "#191414") +  
          geom_point(aes(text = paste("Genre group:", genre_group, "<br>Popularity score:", total_popularity)), color = "#1DB954", size = 3) +
          labs(title = paste("Top", num_genre_groups, "Genre Groups by Popularity"),
               x = "Genre Group",
               y = "Total Popularity") +
          scale_y_continuous(breaks = seq(0, max(selected_genre_groups$total_popularity, na.rm = TRUE), by = 5000), limits = c(0, NA)) +  # Set y-axis ticks based on data range
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels for readability
        
        # Convert ggplot to plotly for interactivity
        plot_interactive <- ggplotly(plot, tooltip = "text") %>%
          layout(
            hovermode = "closest",
            xaxis = list(categoryorder = "array", categoryarray = unique(selected_genre_groups$genre_group))  # Maintain genre order
          )
        
      } else {
        # Get the bottom N genre groups by total popularity
        selected_genre_groups <- genre_groups %>%
          group_by(genre_group) %>%
          summarize(total_popularity = sum(pop), .groups = 'drop') %>%
          arrange(total_popularity) %>%
          slice_head(n = num_genre_groups) %>%
          mutate(genre_group = factor(genre_group, levels = unique(genre_group)))
        
        # Create dynamic ticks based on the data range
        y_ticks <- pretty(c(0, max(selected_genre_groups$total_popularity, na.rm = TRUE)), n = 5)  # Generate 5 evenly spaced ticks
        
        # Create the lollipop plot for bottom N genre groups
        plot <- ggplot(selected_genre_groups, aes(x = genre_group, y = total_popularity)) +
          geom_segment(aes(x = genre_group, xend = genre_group, y = 0, yend = total_popularity), color = "#191414") + 
          geom_point(aes(text = paste("Genre group:", genre_group, "<br>Popularity score:", total_popularity)), color = "#1DB954", size = 3) +
          labs(title = paste("Bottom", num_genre_groups, "Genre Groups by Popularity"),
               x = "Genre Group",
               y = "Total Popularity") +
          scale_y_continuous(breaks = y_ticks, limits = c(0, max(selected_genre_groups$total_popularity, na.rm = TRUE))) +  # Dynamic y-axis ticks
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels for readability
        
        # Convert ggplot to plotly for interactivity
        plot_interactive <- ggplotly(plot, tooltip = "text") %>%
          layout(
            hovermode = "closest",
            xaxis = list(categoryorder = "array", categoryarray = unique(selected_genre_groups$genre_group))  # Maintain genre order
          )
      }
      
      
    } else if (time_frame == "Release Years") {
      if(popular_choice == "Top"){
        plot <- create_top_genre_groups_plot(num_genre_groups, popular_choice)
      } else {
        plot <- create_top_genre_groups_plot(num_genre_groups, popular_choice)
      }
      
    } else if (time_frame == "Each Year"){
      if(popular_choice == "Top"){
        plot <- create_top_genre_groups_plot_each_year(num_genre_groups, popular_choice)
      } else {
        plot <- create_top_genre_groups_plot_each_year(num_genre_groups, popular_choice)
      }
    }
    
  })

  
  # ~~~~~~~ General genre groups - release year
  
  # Function to create the plot for top genre groups by release year
  create_top_genre_groups_plot <- function(top_n = 5, popular_choice) {
    
    # Group data by release year and genre group, summing the popularity scores
    genre_group_popularity <- genre_groups %>%
      group_by(year.released, genre_group) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop')
    
    # Calculate the total popularity for all genre groups per year
    total_popularity_per_year <- genre_group_popularity %>%
      group_by(year.released) %>%
      summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top or bottom N genre groups per year
    top_genre_groups <- genre_group_popularity %>%
      inner_join(total_popularity_per_year, by = "year.released") %>%
      group_by(year.released) %>%
      arrange(if (popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        prop = total_popularity / yearly_total_popularity,
        hover_text = paste("Genre Group:", genre_group, "<br>Popularity:", total_popularity, "<br>Year:", year.released),
        wrapped_genre_group = sapply(genre_group, function(x) {
          if (nchar(x) > 13) {
            paste0(substr(x, 1, 10), "...")
          } else {
            x
          }
        })
      )
    
    # Create the stacked bar plot
    p <- ggplot(top_genre_groups, aes(x = as.factor(year.released), y = prop, fill = genre_group, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_genre_group), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Genre Groups by Popularity (Release Years)"),
           x = "Release Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Make the plot interactive with plotly
    p_interactive <- ggplotly(p, tooltip = "text")
    
    return(p_interactive)
  }

  
  # ~~~~~~~~ General genre groups - top year
  
  create_top_genre_groups_plot_each_year <- function(top_n = 5, popular_choice) {
    
    # Group data by top year and genre group, summing the popularity scores
    genre_group_popularity <- genre_groups %>%
      group_by(top.year, genre_group) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop')
    
    # Calculate the total popularity for all genre groups per year
    total_popularity_per_year <- genre_group_popularity %>%
      group_by(top.year) %>%
      summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top or bottom N genre groups per year
    top_genre_groups <- genre_group_popularity %>%
      inner_join(total_popularity_per_year, by = "top.year") %>%
      group_by(top.year) %>%
      arrange(if (popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        prop = total_popularity / yearly_total_popularity,
        hover_text = paste("Genre Group:", genre_group, "<br>Popularity:", total_popularity, "<br>Year:", top.year),
        wrapped_genre_group = sapply(genre_group, function(x) {
          if (nchar(x) > 13) {
            paste0(substr(x, 1, 10), "...")
          } else {
            x
          }
        })
        
      )
    
    # Create the stacked bar plot
    p <- ggplot(top_genre_groups, aes(x = as.factor(top.year), y = prop, fill = genre_group, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_genre_group), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Genre Groups in Each Year"),
           x = "Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Make the plot interactive with plotly
    p_interactive <- ggplotly(p, tooltip = "text")
    
    return(p_interactive)
  }
  
  
  
# ------------------------ Individual Genre groups - all time ----------------------#
  
  
  genre_groups <- spotify_clean %>%
    mutate(genre_group = case_when(
      grepl("pop", `top.genre`, ignore.case = TRUE) ~ "Pop",
      grepl("hip hop|rap", `top.genre`, ignore.case = TRUE) ~ "Hip Hop/Rap",
      grepl("rock|metal", `top.genre`, ignore.case = TRUE) ~ "Rock",
      grepl("edm|dance|house|electronic", `top.genre`, ignore.case = TRUE) ~ "Electronic/Dance",
      grepl("r&b|soul|funk", `top.genre`, ignore.case = TRUE) ~ "R&B/Soul",
      grepl("latin|reggaeton", `top.genre`, ignore.case = TRUE) ~ "Latin",
      grepl("country", `top.genre`, ignore.case = TRUE) ~ "Country",
      grepl("folk|americana", `top.genre`, ignore.case = TRUE) ~ "Folk",
      TRUE ~ "Other"
    ))
  
  # Render the correct plot based on user input
  output$allIndividualGenresPlot <- renderPlotly({
    
    # Get the number of genres to display, the time frame, and the selected genre group
    num_genres <- input$individualGenresSlider
    metric <- input$IndividualGenresTime
    genre_group_choice <- input$individualGenresChoose
    
    # Filter the data based on the metric and genre group choice
    if (metric == "All Time") {
      
      if (input$individualGenresChoose == "Pop") {
        plot <- create_genre_group_line_plot(selected_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Hip Hop/Rap") {
        plot <- create_genre_group_line_plot(selected_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Rock") {
        plot <- create_genre_group_line_plot(selected_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Electronic/Dance") {
        plot <- create_genre_group_line_plot(selected_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "R&B/Soul") {
        plot <- create_genre_group_line_plot(selected_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Latin") {
        plot <- create_genre_group_line_plot(selected_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Country") {
        plot <- create_genre_group_line_plot(selected_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Folk") {
        plot <- create_genre_group_line_plot(selected_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Other") {
        plot <- create_genre_group_line_plot(selected_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      
    } else if (metric == "Release Years") {
      
      if (input$individualGenresChoose == "Pop") {
        plot <- create_top_genres_in_group_plot(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Hip Hop/Rap") {
        plot <- create_top_genres_in_group_plot(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Rock") {
        plot <- create_top_genres_in_group_plot(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Electronic/Dance") {
        plot <- create_top_genres_in_group_plot(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "R&B/Soul") {
        plot <- create_top_genres_in_group_plot(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Latin") {
        plot <- create_top_genres_in_group_plot(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Country") {
        plot <- create_top_genres_in_group_plot(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Folk") {
        plot <- create_top_genres_in_group_plot(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Other") {
        plot <- create_top_genres_in_group_plot(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
    } else if (metric == "Each Year") {
      
      if (input$individualGenresChoose == "Pop") {
        plot <- create_top_genres_in_group_plot_top_year(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Hip Hop/Rap") {
        plot <- create_top_genres_in_group_plot_top_year(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Rock") {
        plot <- create_top_genres_in_group_plot_top_year(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Electronic/Dance") {
        plot <- create_top_genres_in_group_plot_top_year(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "R&B/Soul") {
        plot <- create_top_genres_in_group_plot_top_year(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Latin") {
        plot <- create_top_genres_in_group_plot_top_year(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Country") {
        plot <- create_top_genres_in_group_plot_top_year(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Folk") {
        plot <- create_top_genres_in_group_plot_top_year(main_genre_group = genre_group_choice, top_n = num_genres)
      }
      
      if (input$individualGenresChoose == "Other") {
        plot <- create_top_genres_in_group_plot_top_year(main_genre_group = genre_group_choice, top_n = num_genres)
      }
    }
    
    return(plot)
  })
  
  
  # ~~~~~~~~ Individual genre groups - all time
  
  create_genre_group_line_plot <- function(selected_genre_group, top_n = 3) {
    
    # Filter data for the specified genre group and group by genre
    genre_data <- genre_groups %>%
      filter(genre_group == selected_genre_group) %>%
      group_by(top.genre) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop') %>%
      arrange(desc(total_popularity)) %>%
      mutate(
        hover_text = paste("Genre:", top.genre, "<br>Popularity:", total_popularity),
        wrapped_genre = sapply(top.genre, function(x) paste(strwrap(x, width = 15), collapse = "\n"))
      ) %>%
      slice_max(order_by = total_popularity, n = top_n) # Select top N genres within the specified group
    
    # Create the lollipop plot
    plot <- ggplot(genre_data, aes(x = reorder(wrapped_genre, -total_popularity), y = total_popularity)) +
      geom_segment(aes(x = reorder(wrapped_genre, -total_popularity), xend = reorder(wrapped_genre, -total_popularity), y = 0, yend = total_popularity), color = "#191414") + 
      geom_point(aes(text = hover_text), color = "#1DB954", size = 3) + 
      labs(title = paste("Most Popular Genres in", selected_genre_group, "Group (All Time)"),
           x = "Genre",
           y = "Total Popularity") +
      scale_y_continuous(breaks = seq(0, max(genre_data$total_popularity, na.rm = TRUE), by = 5000), limits = c(0, NA)) +  # Set y-axis ticks every 5000, up to max
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels for readability
    
    # Convert ggplot to plotly for interactivity
    plot_interactive <- ggplotly(plot, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        xaxis = list(categoryorder = "array", categoryarray = unique(genre_data$wrapped_genre))  # Maintain genre order
      )
    
    return(plot_interactive)
  }
  
  
  
  
  # ~~~~~~~ Individual genre groups - release years
  
  # Function to create the plot for top individual genres in each main genre group (Release Years)
  create_top_genres_in_group_plot <- function(main_genre_group, top_n = 3) {
    
    # Filter data by the specified main genre group
    genre_data <- genre_groups %>%
      filter(genre_group == main_genre_group) %>%
      group_by(year.released, top.genre) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop')
    
    # Calculate the total popularity for all genres in the main genre group per year
    total_popularity_per_year <- genre_data %>%
      group_by(year.released) %>%
      summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top N genres per year
    top_genres_in_group <- genre_data %>%
      inner_join(total_popularity_per_year, by = "year.released") %>%
      group_by(year.released) %>%
      arrange(desc(total_popularity)) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        prop = total_popularity / yearly_total_popularity,
        hover_text = paste("Genre:", top.genre, "<br>Popularity:", total_popularity, "<br>Year:", year.released),
        wrapped_genre = sapply(top.genre, function(x) {
          if (nchar(x) > 13) {
            paste0(substr(x, 1, 10), "...")
          } else {
            x
          }
        })
        
      )
    
    # Create the stacked bar plot
    p <- ggplot(top_genres_in_group, aes(x = as.factor(year.released), y = prop, fill = top.genre, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_genre), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste("Top Genres in", main_genre_group, "from Release Year"),
           x = "Release Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Make the plot interactive with plotly
    p_interactive <- ggplotly(p, tooltip = "text")
    
    return(p_interactive)
  }
  
  
  
  # ~~~~~~ Individual genre groups - top year
  
  create_top_genres_in_group_plot_top_year <- function(main_genre_group, top_n = 3) {
    
    # Filter data
    genre_data_top_year <- genre_groups %>%
      filter(genre_group == main_genre_group) %>%
      group_by(top.year, top.genre) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop')
    
    # Calculate the total popularity for all genres in the main genre group per year
    total_popularity_per_top_year <- genre_data_top_year %>%
      group_by(top.year) %>%
      summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
    
    # Merge the two dataframes and filter to the top N genres per year
    top_genres_in_group_top_year <- genre_data_top_year %>%
      inner_join(total_popularity_per_top_year, by = "top.year") %>%
      group_by(top.year) %>%
      arrange(desc(total_popularity)) %>%
      slice_head(n = top_n) %>%
      ungroup() %>%
      mutate(
        prop = total_popularity / yearly_total_popularity,
        hover_text = paste("Genre:", top.genre, "<br>Popularity:", total_popularity, "<br>Year:", top.year),
        wrapped_genre = sapply(top.genre, function(x) {
          if (nchar(x) > 13) {
            paste0(substr(x, 1, 10), "...")
          } else {
            x
          }
        })
      )
    
    # Create the stacked bar plot
    p <- ggplot(top_genres_in_group_top_year, aes(x = as.factor(top.year), y = prop, fill = top.genre, text = hover_text)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = wrapped_genre), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
      labs(title = paste("Top Genres in", main_genre_group, "by Year"),
           x = "Year",
           y = "Proportion of Total Popularity") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Make the plot interactive with plotly
    p_interactive <- ggplotly(p, tooltip = "text")
    
    return(p_interactive)
  }
  
  
  
  
# ----------------------------------------------------------------- #
# ---------------------- Artists -----------------------------------#
# ----------------------------------------------------------------- #
  
  # -------------------- Base info ------------------------------- #

# ~~~~~~~~ Num of artists by artists type 
  
  
  # Render the bar plot for number of unique artists by artist type
  output$artistTypeBarPlot <- renderPlotly({
    unique_artist_counts <- spotify_clean %>%
      select(artist.type, artist) %>%
      distinct() %>%  # Keep unique artist-type combinations
      group_by(artist.type) %>%
      summarize(count = n(), .groups = 'drop')
    
    artist_type_plot <- ggplot(unique_artist_counts, aes(x = reorder(artist.type, -count), y = count)) +
      geom_bar(stat = "identity", fill = "#1DB954", color = "white") +
      labs(title = "Number of Unique Artists by Artist Type",
           x = "Artist Type",
           y = "Number of Unique Artists") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    ggplotly(artist_type_plot, tooltip = c("y"))
  })
  
  # ~~~~~~~ Num of artists by artist types - over years
  
  # Render the interactive line plot for number of unique artists by artist type over years
  output$artistTypeLinePlot <- renderPlotly({
    unique_artist_counts_yearly <- spotify_clean %>%
      group_by(year.released, artist.type) %>%
      summarize(count = n(), .groups = 'drop')
    
    interactive_artist_type_plot <- plot_ly(
      data = unique_artist_counts_yearly,
      x = ~year.released,
      y = ~count,
      color = ~artist.type,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2),
      text = ~paste('Year:', year.released, '<br>Artists:', count, '<br>Artist Type:', artist.type),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Number of Unique Artists by Artist Type Over the Years",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Unique Artists"),
        hovermode = "closest"
      )
    
    interactive_artist_type_plot <- onRender(interactive_artist_type_plot, "
      function(el, x) {
        el.on('plotly_hover', function(d) {
          var update = {'opacity': 0.2}; // Reduce opacity of all lines
          Plotly.restyle(el, update);

          var update = {'opacity': 1, 'line.width': 4}; // Highlight the hovered line
          Plotly.restyle(el, update, [d.points[0].curveNumber]);
        }).on('plotly_unhover', function(d) {
          var update = {'opacity': 1, 'line.width': 2}; // Revert all lines to original state
          Plotly.restyle(el, update);
        });
      }
    ")
    
    interactive_artist_type_plot
  })

# ~~~~~~ Number of songs by artists types 

# Render the bar plot for number of songs by artist type
output$artistTypeSongStaticPlot <- renderPlotly({
  artist_type_counts <- spotify_clean %>%
    group_by(artist.type) %>%
    summarize(count = n(), .groups = 'drop')
  
  
  artist_type_plot <- ggplot(artist_type_counts, aes(x = reorder(artist.type, -count), y = count, text = paste("Artist Type: ", artist.type, "<br>Count: ", count))) +
    geom_bar(stat = "identity", fill = "#1DB954") +
    labs(title = "Number of Songs by Artist Type", x = "Artist Type", y = "Number of Songs") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(artist_type_plot, tooltip = "text")
})
  
# ~~~~~~ Number of songs by artists types - over years 

# Render the interactive line plot for number of songs by artist type over years
output$artistTypeSongLinePlot <- renderPlotly({
  song_counts_yearly <- spotify_clean %>%
    group_by(year.released, artist.type) %>%
    summarize(count = n(), .groups = 'drop')
  
  interactive_song_plot <- plot_ly(
    data = song_counts_yearly,
    x = ~year.released,
    y = ~count,
    color = ~artist.type,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 2),
    text = ~paste('Year:', year.released, '<br>Songs:', count, '<br>Artist Type:', artist.type),
    hoverinfo = 'text'
  ) %>%
    layout(
      title = "Number of Songs by Artist Type Over the Years",
      xaxis = list(title = "Year"),
      yaxis = list(title = "Number of Songs"),
      hovermode = "closest"
    )
  
  interactive_song_plot <- onRender(interactive_song_plot, "
      function(el, x) {
        el.on('plotly_hover', function(d) {
          var update = {'opacity': 0.2}; // Reduce opacity of all lines
          Plotly.restyle(el, update);

          var update = {'opacity': 1, 'line.width': 4}; // Highlight the hovered line
          Plotly.restyle(el, update, [d.points[0].curveNumber]);
        }).on('plotly_unhover', function(d) {
          var update = {'opacity': 1, 'line.width': 2}; // Revert all lines to original state
          Plotly.restyle(el, update);
        });
      }
    ")
  
  interactive_song_plot
})




# --------------- Number of songs by artists -------------------- #

# Reactive - generating the correct plot based on user inputs
output$numArtistsPlot <- renderPlotly({
  
  # Get the number of artists to display and the metric
  num_artists <- input$artistsSlider
  metric <- input$metric
  
  # Case 1: "All Time" selected
  if (metric == "All Time") {
    # Get the top N artists by number of songs
    selected_artists <- spotify_clean %>%
      group_by(artist) %>%
      summarize(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      slice_head(n = num_artists) %>%
      mutate(artist = factor(artist, levels = unique(artist)))
    
    # Create the lollipop plot for top N artists
    plot <- ggplot(selected_artists, aes(x = artist, y = count)) +
      geom_segment(aes(x = artist, xend = artist, y = 0, yend = count), color = "#191414") +
      geom_point(aes(text = paste("Artist:", artist, "<br>Number of Songs:", count)), color = "#1DB954", size = 3) +
      labs(title = paste("Top", num_artists, "Artists by Number of Songs"),
           x = "Artist",
           y = "Number of Songs") +
      scale_y_continuous(breaks = seq(0, max(selected_artists$count, na.rm = TRUE), by = 10), limits = c(0, NA)) +  # Adjust y-axis tick interval
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels 45 degrees for readability
    
    # Convert ggplot to plotly for interactivity
    plot_interactive <- ggplotly(plot, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        xaxis = list(categoryorder = "array", categoryarray = unique(selected_artists$artist))  # Maintain artist order
      )
  }
  
  
  # Case 2: "Release Years" selected
  else if (metric == "Release Years") {
    wrap_text_6 <- function(text, width = 15) {
      if (nchar(text) > width) {
        return(paste0(substr(text, 1, width - 3), "..."))
      }
      return(text)
    }
    
    # Group data by year and artist, and count the number of songs
    artist_song_count <- spotify_clean %>%
      group_by(year.released, artist) %>%
      summarize(song_count = n(), .groups = 'drop') %>%
      arrange(year.released, desc(song_count))
    
    # Get the top N artists by number of songs for each year
    top_artists_by_year <- artist_song_count %>%
      group_by(year.released) %>%
      slice_head(n = num_artists) %>%
      ungroup() %>%
      mutate(wrapped_artists = sapply(artist, wrap_text_6))
    
    # Create the bar plot with artist names as labels on the bars
    plot <- ggplot(top_artists_by_year, aes(x = as.factor(year.released), y = song_count, fill = artist)) +
      geom_bar(stat = "identity", position = "stack", color = "white") +
      geom_text(aes(label = wrapped_artists), position = position_stack(vjust = 0.5), size = 3, color = "#191414") +
      labs(title = paste("Top", num_artists, "Artists by Number of Songs Released Each Year"),
           x = "Year",
           y = "Number of Songs Released") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Convert the ggplot to an interactive plotly plot
    plot <- ggplotly(plot, tooltip = c("y", "artist"))
  }
  
  # Case 3: "Each Year" selected
  else if (metric == "Each year") {
    wrap_text_6 <- function(text, width = 15) {
      if (nchar(text) > width) {
        return(paste0(substr(text, 1, width - 3), "..."))
      }
      return(text)
    }
    
    # Group data by top year and artist, and count the number of hit songs
    artist_hit_count <- spotify_clean %>%
      group_by(top.year, artist) %>%
      summarize(hit_count = n(), .groups = 'drop') %>%
      arrange(top.year, desc(hit_count))
    
    # Get the top N artists by number of hit songs for each top year
    top_artists_by_year <- artist_hit_count %>%
      group_by(top.year) %>%
      slice_head(n = num_artists) %>%
      ungroup() %>%
      mutate(wrapped_artist = sapply(artist, wrap_text_6))
    
    # Create the bar plot with artist names as labels on the bars
    plot <- ggplot(top_artists_by_year, aes(x = as.factor(top.year), y = hit_count, fill = wrapped_artist)) +
      geom_bar(stat = "identity", position = "stack", color = "white") +
      geom_text(aes(label = wrapped_artist), position = position_stack(vjust = 0.5), size = 3, color = "#191414") +
      labs(title = paste("Top", num_artists, "Artists by Number of Hit Songs in Each Year"),
           x = "Top Year",
           y = "Number of Hit Songs") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove legend
    
    # Convert the ggplot to an interactive plotly plot
    plot <- ggplotly(plot, tooltip = c("y", "wrapped_artist"))
  }
  
  # Return the plot
  return(plot)
})


# -------------- Popularity of artists / artists types ----------- #
# ---------------------------------------------------------------- #


# -------- All Artists ------ #

# ~~~~ All time
  
# Reactive - generating the correct plot based on user inputs
output$allArtistsPlot <- renderPlotly({
  
  # Get the number of artists to display, the selected time frame, and the popular choice
  num_artists <- input$allArtistsSlider
  time_frame <- input$ArtistsTime
  popular_choice <- input$artistsPopular
  
  # Case 1: "All Time" selected
  if (time_frame == "All Time") {
    
    if (popular_choice == "Top") {
      # Get the top N artists by total popularity
      selected_artists <- spotify_clean %>%
        group_by(artist) %>%
        summarize(total_popularity = sum(pop, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(total_popularity)) %>%
        slice_head(n = num_artists) %>%
        mutate(artist = factor(artist, levels = unique(artist)))
      
      # Create the lollipop plot for top N artists
      plot <- ggplot(selected_artists, aes(x = artist, y = total_popularity)) +
        geom_segment(aes(x = artist, xend = artist, y = 0, yend = total_popularity), color = "#191414") +  # Line segment
        geom_point(aes(text = paste("Artist:", artist, "<br>Total Popularity:", total_popularity)), color = "#1DB954", size = 3) +  # Circle marker
        labs(title = paste("Top", num_artists, "Artists by Popularity (All Time)"),
             x = "Artist",
             y = "Total Popularity") +
        scale_y_continuous(breaks = seq(0, max(selected_artists$total_popularity, na.rm = TRUE), by = 500), limits = c(0, NA)) +  # Set y-axis ticks
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels 45 degrees for readability
      
      # Convert ggplot to plotly for interactivity
      plot_interactive <- ggplotly(plot, tooltip = "text") %>%
        layout(
          hovermode = "closest",
          xaxis = list(categoryorder = "array", categoryarray = unique(selected_artists$artist))  # Maintain artist order
        )
      
    } else {
      # Get the bottom N artists by total popularity
      selected_artists <- spotify_clean %>%
        group_by(artist) %>%
        summarize(total_popularity = sum(pop, na.rm = TRUE), .groups = 'drop') %>%
        arrange(total_popularity) %>%
        slice_head(n = num_artists) %>%
        mutate(artist = factor(artist, levels = unique(artist)))
      
      # Create the lollipop plot for bottom N artists
      plot <- ggplot(selected_artists, aes(x = artist, y = total_popularity)) +
        geom_segment(aes(x = artist, xend = artist, y = 0, yend = total_popularity), color = "#191414") +  # Line segment
        geom_point(aes(text = paste("Artist:", artist, "<br>Total Popularity:", total_popularity)), color = "#1DB954", size = 3) +  # Circle marker
        labs(title = paste("Bottom", num_artists, "Artists by Popularity (All Time)"),
             x = "Artist",
             y = "Total Popularity") +
        scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, NA)) +  # Set y-axis ticks
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels 45 degrees for readability
      
      # Convert ggplot to plotly for interactivity
      plot_interactive <- ggplotly(plot, tooltip = "text") %>%
        layout(
          hovermode = "closest",
          xaxis = list(categoryorder = "array", categoryarray = unique(selected_artists$artist))  # Maintain artist order
        )
    }
    
    
  } else if (time_frame == "Release Years") {
    if ( popular_choice == "Top"){
      plot <- create_artist_plot_release_year(num_artists, popular_choice)
    } else {
      plot <- create_artist_plot_release_year(num_artists, popular_choice)
    }
   
    
  } else if (time_frame == "Each year") {
    if(popular_choice == "Top"){
      plot <- create_artist_plot_each_year(num_artists, popular_choice)
    }
    else {
      plot <- create_artist_plot_each_year(num_artists, popular_choice)
    }
  }
  
  # Return the generated plot
  return(plot)
})

# ~~~~~~~ All artist types - Release Year

create_artist_plot_release_year <- function(top_n = 5, popular_choice) {
  wrap_text_4 <- function(text, width = 15) {
    if (nchar(text) > width) {
      return(paste0(substr(text, 1, width - 3), "..."))
    }
    return(text)
  }
  
  # Calculate the total popularity for each artist per year
  artist_popularity <- spotify_clean %>%
    group_by(year.released, artist) %>%
    summarize(total_popularity = sum(pop), .groups = 'drop') %>%
    ungroup()
  
  # Calculate the total popularity for all artists per year
  total_popularity_per_year <- artist_popularity %>%
    group_by(year.released) %>%
    summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop') %>%
    ungroup()
  
  # Merge the two dataframes and filter to the top or bottom N artists per year
  top_artists <- artist_popularity %>%
    inner_join(total_popularity_per_year, by = "year.released") %>%
    group_by(year.released) %>%
    arrange(if (popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    mutate(
      prop = total_popularity / yearly_total_popularity,
      hover_text = paste("Artist:", artist, "<br>Popularity:", total_popularity, "<br>Year:", year.released),
      wrapped_artist = sapply(artist, function(x) {
        if (nchar(x) > 15) {
          paste0(substr(x, 1, 12), "...")
        } else {
          x
        }
      })
      
    )
  
  # Create the stacked bar plot
  p <- ggplot(top_artists, aes(x = as.factor(year.released), y = prop, fill = wrapped_artist, text = hover_text)) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(aes(label = wrapped_artist), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
    labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Artists by Popularity (Release Years)"),
         x = "Release Year",
         y = "Proportion of Total Popularity") +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend
  
  # Make the plot interactive with plotly
  p_interactive <- ggplotly(p, tooltip = "text")
  
  return(p_interactive)
}

# ~~~~~~ All artists types - Each year (top.year)

create_artist_plot_each_year <- function(top_n = 5, popular_choice) {
  wrap_text_4 <- function(text, width = 15) {
    if (nchar(text) > width) {
      return(paste0(substr(text, 1, width - 3), "..."))
    }
    return(text)
  }
  
  # Calculate the total popularity for each artist per top year
  artist_popularity <- spotify_clean %>%
    group_by(top.year, artist) %>%
    summarize(total_popularity = sum(pop), .groups = 'drop') %>%
    ungroup()
  
  # Calculate the total popularity for all artists per top year
  total_popularity_per_top_year <- artist_popularity %>%
    group_by(top.year) %>%
    summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop') %>%
    ungroup()
  
  # Merge the two dataframes and filter to the top or bottom N artists per top year
  top_artists <- artist_popularity %>%
    inner_join(total_popularity_per_top_year, by = "top.year") %>%
    group_by(top.year) %>%
    arrange(if (popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    mutate(
      prop = total_popularity / yearly_total_popularity,
      hover_text = paste("Artist:", artist, "<br>Popularity:", total_popularity, "<br>Year:", top.year),
      wrapped_artist = sapply(artist, function(x) {
        if (nchar(x) > 15) {
          paste0(substr(x, 1, 12), "...")
        } else {
          x
        }
      })
    )
  
  # Create the stacked bar plot
  p <- ggplot(top_artists, aes(x = as.factor(top.year), y = prop, fill = wrapped_artist, text = hover_text)) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(aes(label = wrapped_artist), size = 3, position = position_stack(vjust = 0.5), color = "#191414") +
    labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Artists by Popularity (Top Year)"),
         x = "Top Year",
         y = "Proportion of Total Popularity") +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend
  
  # Make the plot interactive with plotly
  p_interactive <- ggplotly(p, tooltip = c("text", "y")) 
  
  
  return(p_interactive)
}



# --------- All Artists Types ----------------------- #
# --------------------------------------------------- #

output$allArtistsTypesPlot <- renderPlotly({
  
  # Get the selected time frame
  time_frame <- input$allArtistsTypesTime
  
  # Case 1: "All Time" selected
  if (time_frame == "All Time") {
    
    # Generate the lollipop plot for artist types by total popularity
    selected_artist_types <- spotify_clean %>%
      group_by(artist.type) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop') %>%
      arrange(desc(total_popularity)) %>%
      mutate(artist.type = factor(artist.type, levels = unique(artist.type)))
    
    # Create the lollipop plot
    plot <- ggplot(selected_artist_types, aes(x = artist.type, y = total_popularity)) +
      geom_segment(aes(x = artist.type, xend = artist.type, y = 0, yend = total_popularity), color = "#191414") +  
      geom_point(aes(text = paste("Artist Type:", artist.type, "<br>Total Popularity:", total_popularity)), color = "#1DB954", size = 3) +  
      labs(title = "Artist Types by Total Popularity (All Time)",
           x = "Artist Type",
           y = "Total Popularity") +
      scale_y_continuous(breaks = seq(0, max(selected_artist_types$total_popularity, na.rm = TRUE), by = 5000), limits = c(0, NA)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels for readability
    
    # Convert ggplot to plotly for interactivity
    plot_interactive <- ggplotly(plot, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        xaxis = list(categoryorder = "array", categoryarray = unique(selected_artist_types$artist.type))  # Maintain artist type order
      )
  }
  else if (time_frame == "Release Years") {
    
    # Aggregate data by year.released and artist type, summing popularity
    artist_year_data <- spotify_clean %>%
      group_by(year.released, artist.type) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop') %>%
      arrange(year.released)
    
    # Create the interactive line plot for Release Years
    plot <- plot_ly(
      data = artist_year_data,
      x = ~year.released,
      y = ~total_popularity,
      color = ~artist.type,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2),
      text = ~paste('Release Year:', year.released, '<br> Popularity:', total_popularity, '<br> Artist Type:', artist.type),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Popularity of Artist Types by Release Year",
        xaxis = list(title = "Release Year"),
        yaxis = list(title = "Total Popularity"),
        hovermode = "closest"
      )
    
    # Apply custom hover behavior
    plot <- onRender(plot, "
      function(el, x) {
        el.on('plotly_hover', function(d) {
          var update = {'opacity': 0.2}; // Reduce opacity of all lines
          Plotly.restyle(el, update);

          var update = {'opacity': 1, 'line.width': 4}; // Highlight the hovered line
          Plotly.restyle(el, update, [d.points[0].curveNumber]);
        }).on('plotly_unhover', function(d) {
          var update = {'opacity': 1, 'line.width': 2}; // Revert all lines to original state
          Plotly.restyle(el, update);
        });
      }
    ")
  }
  else if (time_frame == "Each Year") {
    
    # Aggregate data by top.year and artist type, summing popularity
    artist_year_data <- spotify_clean %>%
      group_by(top.year, artist.type) %>%
      summarize(total_popularity = sum(pop), .groups = 'drop') %>%
      arrange(top.year)
    
    # Create the interactive line plot for Each Year
    plot <- plot_ly(
      data = artist_year_data,
      x = ~top.year,
      y = ~total_popularity,
      color = ~artist.type,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2),
      text = ~paste('Top Year:', top.year, '<br> Popularity:', total_popularity, '<br> Artist Type:', artist.type),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Popularity of Artist Types by Years",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Popularity"),
        hovermode = "closest"
      )
    
    # Apply custom hover behavior
    plot <- onRender(plot, "
      function(el, x) {
        el.on('plotly_hover', function(d) {
          var update = {'opacity': 0.2}; // Reduce opacity of all lines
          Plotly.restyle(el, update);

          var update = {'opacity': 1, 'line.width': 4}; // Highlight the hovered line
          Plotly.restyle(el, update, [d.points[0].curveNumber]);
        }).on('plotly_unhover', function(d) {
          var update = {'opacity': 1, 'line.width': 2}; // Revert all lines to original state
          Plotly.restyle(el, update);
        });
      }
    ")
  }
  
  
  # Return the generated plot
  return(plot)
})


# -------- Individual Artists Types ----------------- #
# --------------------------------------------------- #

# Server code to render the correct plot based on user input for Individual Artists Types
output$individualArtistsTypesPlot <- renderPlotly({
  
  # Get the number of artists to display, the time frame, and the selected artist type
  num_artists <- input$individualAristsTypesSlider
  metric <- input$individualArtistsTypesTime
  artist_type <- input$individualArtistsTypesSelect
  popular_choice <- input$individualArtistsTypesPopular
  
  # Function to create a lollipop plot for the given artist type
  create_artist_type_line_plot <- function(artist_type, top_n, popular_choice) {
    # Filter data for the selected artist type and calculate total popularity
    artist_data <- spotify_clean %>%
      filter(artist.type == artist_type) %>%
      group_by(artist) %>%
      summarize(total_popularity = sum(pop, na.rm = TRUE), .groups = 'drop') %>%
      arrange(if (popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
      slice_head(n = top_n)
    
    # Sorting based on popular_choice
    if (popular_choice == "Bottom") {
      artist_data <- artist_data %>%
        arrange(total_popularity)  # Lowest to highest sorting for Bottom
    } else {
      artist_data <- artist_data %>%
        arrange(desc(total_popularity))  # Highest to lowest sorting for Top
    }
    
    # Make sure artist factor levels are set correctly after sorting
    artist_data$artist <- factor(artist_data$artist, levels = artist_data$artist)
    
    # Create the lollipop plot
    plot <- ggplot(artist_data, aes(x = artist, y = total_popularity)) +
      geom_segment(aes(x = artist, xend = artist, y = 0, yend = total_popularity), color = "#191414") +  
      geom_point(aes(text = paste("Artist:", artist, "<br>Total Popularity:", total_popularity)), color = "#1DB954", size = 3) +
      labs(title = paste(ifelse(popular_choice == "Top", "Top", "Bottom"), top_n, "Most Popular", artist_type, "Artists"),
           x = "Artist",
           y = "Total Popularity") +
      scale_y_continuous(breaks = seq(0, max(artist_data$total_popularity, na.rm = TRUE), by = 5000), limits = c(0, NA)) +  
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  # Rotate x-axis labels for readability
    
    
    # Convert ggplot to plotly for interactivity
    plot_interactive <- ggplotly(plot, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        xaxis = list(categoryorder = "array", categoryarray = unique(artist_data$artist))  # Maintain artist order
      )
    
    return(plot_interactive)
  }
  
  
  # Determine which plot to render based on the selected time frame
  if (metric == "All Time") {
    plot <- create_artist_type_line_plot(artist_type, num_artists, popular_choice)
  } else if (metric == "Release Years") {
    plot <- create_top_artists_per_year_plot(artist_type, num_artists, popular_choice)
  } else if (metric == "Each Year") {
    plot <- create_top_artists_per_top_year_plot(artist_type, num_artists, popular_choice)
  }
  
  return(plot)
})

# Function to create a plot for top artists by release year
create_top_artists_per_year_plot <- function(artist_type, top_n, popular_choice) {
  # Prepare the data
  artist_popularity <- spotify_clean %>%
    filter(artist.type == artist_type) %>%
    group_by(year.released, artist) %>%
    summarize(total_popularity = sum(pop), .groups = 'drop') %>%
    arrange(desc(total_popularity))
  
  total_popularity_per_year <- artist_popularity %>%
    group_by(year.released) %>%
    summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
  
  top_artists <- artist_popularity %>%
    inner_join(total_popularity_per_year, by = "year.released") %>%
    group_by(year.released) %>%
    arrange(if (popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    mutate(
      prop = total_popularity / yearly_total_popularity,
      wrapped_artist = sapply(artist, function(x) {
        if (nchar(x) > 15) {
          paste0(substr(x, 1, 12), "...")
        } else {
          x
        }
      })
    )
  
  # Create the line plot
  p <- ggplot(top_artists, aes(x = as.factor(year.released), y = prop, fill = wrapped_artist, text = artist)) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(aes(label = wrapped_artist), position = position_stack(vjust = 0.5), size = 3) +
    labs(title = paste("Top", top_n, artist_type, "Artists by Release Year"),
         x = "Year",
         y = "Proportion of Total Popularity") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Make the plot interactive with plotly
  p_interactive <- ggplotly(p, tooltip = c("text", "y")) 
  
  return(p_interactive)
}

# Function to create a plot for top artists by top year
create_top_artists_per_top_year_plot <- function(artist_type, top_n, popular_choice) {
  # Prepare the data
  artist_popularity <- spotify_clean %>%
    filter(artist.type == artist_type) %>%
    group_by(top.year, artist) %>%
    summarize(total_popularity = sum(pop), .groups = 'drop') %>%
    arrange(desc(total_popularity))
  
  total_popularity_per_top_year <- artist_popularity %>%
    group_by(top.year) %>%
    summarize(yearly_total_popularity = sum(total_popularity), .groups = 'drop')
  
  top_artists <- artist_popularity %>%
    inner_join(total_popularity_per_top_year, by = "top.year") %>%
    group_by(top.year) %>%
    arrange(if (popular_choice == "Top") desc(total_popularity) else total_popularity) %>%
    slice_head(n = top_n) %>%
    ungroup() %>%
    mutate(
      prop = total_popularity / yearly_total_popularity,
      wrapped_artist = sapply(artist, function(x) {
        if (nchar(x) > 15) {
          paste0(substr(x, 1, 12), "...")
        } else {
          x
        }
      })
    )
  
  # Create the line plot
  p <- ggplot(top_artists, aes(x = as.factor(top.year), y = prop, fill = wrapped_artist, text = artist)) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(aes(label = wrapped_artist), position = position_stack(vjust = 0.5), size = 3) +
    labs(title = paste("Top", top_n, artist_type, "Artists by Each Year"),
         x = "Year",
         y = "Proportion of Total Popularity") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Make the plot interactive with plotly
  p_interactive <- ggplotly(p, tooltip = c("text", "y")) 
  
  return(p_interactive)
}






}

shinyApp(ui, server)
