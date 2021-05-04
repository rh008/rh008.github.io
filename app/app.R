# load packages
#if(!require("devtools"))
#  install.packages("devtools")
#devtools::
#install_github("rstudio/rsconnect")
library(rsconnect)
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(broom)
library(RColorBrewer)
library(wordcloud)
library(tidytext)
library(ggthemes)
library(corrplot)
#library(car)
library(shinydashboard)
library(fmsb)
library(radarchart)
library(ECharts2Shiny)


# prepare data
tracks <- read_csv('/Users/ruihuang/Documents/github/spotify_eda_project/rh008.github.io/output/tracks.csv')
artists <- read_csv('/Users/ruihuang/Documents/github/spotify_eda_project/rh008.github.io/output/artists.csv')
dict <- read_csv('/Users/ruihuang/Documents/github/spotify_eda_project/rh008.github.io/data/audio_features_dictionary.csv')

# Convert keys
library(plyr)
tracks <- tracks %>%
  mutate(mode = as.factor(mode), key = as.factor(revalue(as.character(.$key), c("0" = "C", "1" = "C♯,D♭", "2" = "D", "3" = "D♯,E♭", "4" = "E", "5" =  "F", "6" = "F♯,G♭","7" = "G","8" = "G♯,A♭","9" = "A","10" = "A♯,B♭","11" = "B"))), time_signature = as.factor(time_signature))
detach("package:plyr", unload=TRUE)

# Assign popularity class
tracks <- tracks %>%
  mutate(popularity_class = ifelse(track_popularity > 80, 'A-Popularity > 80',
                                   ifelse(track_popularity > 60, 'B-Popularity > 60',
                                          ifelse(track_popularity > 40, 'C-Popularity > 40',
                                                 ifelse(track_popularity > 20, 'D-Popularity > 20',
                                                        'E-Popularity >= 0')))))

# Prepare data for correlation plot
audio_features <- tracks %>%
  select(danceability:time_signature)


# Prepare data for univariate analysis
tracks_data_ana <- keep(tracks, ~class(.) != 'character') %>%
  select(-release_year)
num.vars.uni <- keep(tracks_data_ana, ~class(.) == 'numeric') %>% 
  colnames()

# Prepare data for linear models
tracks_artists <- tracks %>%
  inner_join(artists, by = 'artist_id')

# Prepare data for bivariate analysis
num.vars.bi <- keep(audio_features, ~class(.) == 'numeric') %>% colnames()

# Prepare data for word cloud plot
tracks_word <- tracks %>%
  filter(release_year >= 2010) %>%
  arrange(release_year)

# Prepare data for genre analysis
artist_genres <- artists %>%
  arrange(desc(artist_popularity)) %>%
  distinct(artist_genre) %>%
  arrange(artist_genre)

# Prepare data for artist radar plot
radar_genres <- c('hip-hop', 'r-n-b', 'classical', 'folk', 'acoustic', 'reggae', 'rock-n-roll', 'electronic', 'blues', 'jazz', 'k-pop', 'house')

genre_radar_df <- tracks_artists[tracks_artists$artist_genre %in% radar_genres, ] %>%
  arrange(desc(artist_popularity)) %>%
  group_by(artist_genre) %>%
  summarise(artist_popularity = mean(artist_popularity),
            danceability = mean(danceability),
            energy = mean(energy),
            loudness = mean(loudness),
            speechiness = mean(speechiness),
            acousticness = mean(acousticness),
            instrumentalness = mean(instrumentalness),
            liveness = mean(liveness),
            valence = mean(valence),
            duration_ms = mean(duration_ms))


# Normalize radar plot data
genre_radar_df_norm <- cbind(genre_radar_df[,1], apply(genre_radar_df[,-1], 2, function(x){(x-min(x)) / diff(range(x))}))

# Mutate radar data
genre_radar_final <- gather(genre_radar_df_norm, key=Attribute, value=Score, -artist_genre) %>%
  spread(key=artist_genre, value=Score)

row.names(genre_radar_final) <- genre_radar_final[,1]

genre_radar_final <- genre_radar_final[,-1]

########################  SHINY APP STARTS HERE ######################## 
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    #title = tags$img(src='spotifylogo.png', width = 200, height= 60)
    title = 'Spotify EDA Analysis'
  ),
  dashboardSidebar(
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    
    # The dynamically-generated user panel
    uiOutput("userpanel"),
    sidebarMenu(
      menuItem(text = 'Home', tabName = 'home'),
      menuItem(text = 'Sample Data', tabName = 'sampledata'),
      menuItem(text = 'Data Dictionary', tabName = 'dictionary'),
      menuItem(text = 'Track Analyses', tabName = 'tracks_ana'),
      menuItem(text = 'Artists Analyses', tabName = 'artist_ana'),
      #menuItem(text = 'Linear Regression', tabName = 'predictions'),
      menuItem(text = 'Favorite Words', tabName = 'favorite_words')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'home',
        HTML('<center><img src="home.jpg" width="1000" height="500"></center>'),
        h1(strong("Discover Spotify Data"), align="center", style = "font-family:500px; 'Times', serif;
    font-weight: 500; font-size: 500; text-shadow: 3px 3px 3px #aaa; line-height: 1; 
     color: #404040;"),
        h3(strong("by Rui Huang"), align="center", style = "font-family:500px; 'Times', serif;
    font-weight: 500; font-size: 500; text-shadow: 3px 3px 3px #aaa; line-height: 1; 
     color: #404040;")
      ),
      tabItem(
        tabName = 'sampledata',
        fluidRow(
          box(
            dataTableOutput('tracks'),
            title = 'Tracks',
            width = 24
          )
        ),
        fluidRow(
          box(
            dataTableOutput('artists'),
            title = 'Artists',
            width = 12
          )
        )
      ),
      tabItem(
        tabName = 'dictionary',
        box(
          dataTableOutput('dictionary'),
          title = 'Audio Feature Dictionary',
          width = 12
        )
      ),
      tabItem(
        tabName = 'tracks_ana',
        fluidRow(
          box(
            plotOutput('corrplot'),
            title = 'Correlation Plot of Audio Features',
            width = 12
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = 'Univariate Analysis',
            sidebarLayout(
              sidebarPanel(
                selectInput('var', 'Variable', names(tracks_data_ana)),
                checkboxInput('log', 'Log', value = FALSE),
                checkboxInput('uni_fill', 'Color By Popularity Class', value = FALSE),
                sliderInput('bins', 'Bins', 1, 100, 20),
                numericInput('hNull', 'Null Value', value = 0),
                tableOutput('summary')
              ),
              mainPanel(
                plotOutput('uni_tracks')
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = 'Bivariate Analysis',
            sidebarLayout(
              sidebarPanel(
                selectInput('var1', 'Audio Feature 1 (x)', names(tracks_data_ana)),
                selectInput('var2', 'Audio Feature 2 (y)', names(tracks_data_ana)),
                checkboxInput('log1', 'Log Audio Feature 1', value = FALSE),
                checkboxInput('log2', 'Log Audio Feature 2', value = FALSE),
                checkboxInput('ols', 'Add OLS Line'),
                checkboxInput('bi_fill', 'Color By Popularity Class', value = FALSE)
              ),
              mainPanel(
                plotOutput('bi_tracks')
              )
            )
          )
        )
      ),
      tabItem(
        tabName = 'artist_ana',
        fluidRow(
          box(
            width = 12,
            title = 'Artist Popularity Analysis',
            sidebarLayout(
              sidebarPanel(
                selectInput('genre1', 'Pick a genre', artist_genres$artist_genre),
                selectInput('genre2', 'Pick another genre', artist_genres$artist_genre),
                checkboxInput('log_artist_popularity', 'Log Artist Popularity', value = FALSE),
                tableOutput('mean_difference')
              ),
              mainPanel(
                plotOutput('genre_boxplot')
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = 'Audio Features By Artist Genre',
            loadEChartsLibrary(),
            tags$div(id="test", style="width:100%;height:600px;"),
            deliverChart(div_id = "test")
          )
        )
      ),
      tabItem(
        tabName = 'favorite_words',
        sidebarLayout(
          sidebarPanel(
            selectInput('year', 'Release Year', choices = unique(tracks_word$release_year)),
            checkboxInput('animation', 'Go Auto!', value = FALSE)
          ),
          mainPanel(
            plotOutput('wordcloud')
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  renderRadarChart(div_id = "test",
                   data = genre_radar_final)
  
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
  
  ####  SAMPLE DATA & DICTIONARY  ####
  output$tracks <- renderDataTable(
    tracks,
    options = list(scrollX = TRUE)
  )
  
  output$artists <- renderDataTable(
    artists,
    options = list(scrollX = TRUE)
  )
  
  output$dictionary <- renderDataTable({
    dict
  })
  
  ####  CORRELATION PLOT  ####
  output$corrplot <- renderPlot({
    tracks %>%
      select(danceability:time_signature, track_popularity) %>%
      map_df(~as.numeric(.)) %>%
      cor() %>%
      corrplot(method="color", type = "upper", tl.col="black",tl.srt=40, addCoef.col = "gray8", diag = T, number.cex = 0.65)
  })
  
  #### UNIVARIATE ANALYSES (TRACKS) ####
  # univariate plots
  output$uni_tracks <- renderPlot({
    # base plot
    if (input$uni_fill == TRUE) {
      bPlot <- ggplot(data = tracks, aes(x = .data[[input$var]], fill = popularity_class)) + theme_tufte()
    } else {
      bPlot <- ggplot(data = tracks, aes(x = .data[[input$var]])) + theme_tufte()
    }
    
    # define plots per variable type
    if (input$var %in% num.vars.uni) {
      if (input$log == TRUE) {
        bPlot + 
          geom_histogram(bins = input$bins, color = 'grey') +
          scale_x_log10()
      } else {
        bPlot + 
          geom_histogram(bins = input$bins, color = 'grey')
      }
    } else {
      bPlot + 
        geom_bar(color = 'grey')
    }
  })
  
  # univariate t test
  output$summary <- renderTable({
    if (input$var %in% num.vars.uni) {
      if (input$log == TRUE) {
        t.test(x = log10(tracks_data_ana[[input$var]] + 1 - min(tracks_data_ana[[input$var]])), mu = input$hNull) %>%
          tidy() %>%
          select(`P-value` = p.value, Lower = conf.low, Upper = conf.high)
      } else {
        t.test(x = tracks_data_ana[[input$var]], mu = input$hNull) %>%
          tidy() %>%
          select(`P-value` = p.value, Lower = conf.low, Upper = conf.high)
      }
    } else {
      tibble(Data = 'Not a numeric.')
    }
  })
  
  # Track popularity predictions
  output$audio_feature_lm_summary <- renderPrint({
    fit <- lm(track_popularity ~ acousticness + danceability + energy + instrumentalness + loudness + liveness + valence + mode + key + speechiness + tempo + duration_ms + time_signature, data=tracks_artists)
    summary(fit)
  })
  
  output$audio_feature_lm_plots <- renderPlot({
    fit <- lm(track_popularity ~ acousticness + danceability + energy + instrumentalness + loudness + liveness + valence + mode + key + speechiness + tempo + duration_ms + time_signature, data=tracks_artists)  
    par(mfrow = c(1,4))
    plot(fit)
  })
  
  
  #### BIVARIATE ANALYSES (TRACKS) ####
  output$bi_tracks <- renderPlot({
    # base plot
    if (input$bi_fill == TRUE) {
      bPlot2 <- ggplot(data = tracks, aes(x = .data[[input$var1]], y = .data[[input$var2]], color = popularity_class)) + theme_tufte()
    } else {
      bPlot2 <- ggplot(data = tracks, aes(x = .data[[input$var1]], y = .data[[input$var2]])) + theme_tufte()
    }
    # define plots per variable types
    if (input$var1 %in% num.vars.uni == input$var2 %in% num.vars.uni) {
      plot <- bPlot2 + 
        geom_jitter(alpha = 0.5)
    } else if (input$var1 %in% num.vars.uni != input$var2 %in% num.vars.uni) {
      plot <- bPlot2 + 
        geom_boxplot()
    }
    # define log scale condition
    if (input$log1 == TRUE && input$log2 == TRUE && input$var1 %in% num.vars.uni && input$var2 %in% num.vars.uni) {
      plot <- plot + scale_x_log10() + scale_y_log10()
    } else if (input$log1 == TRUE && input$var1 %in% num.vars.uni) {
      plot <- plot + scale_x_log10()
    } else if (input$log2 == TRUE && input$var2 %in% num.vars.uni) {
      plot <- plot + scale_y_log10()
    } else {
      plot
    }
    # define OLS line condition
    if (input$ols == TRUE && input$var1 %in% num.vars.uni && input$var2 %in% num.vars.uni) {
      plot + geom_smooth(se = FALSE, method = 'lm', color = 'darkgrey')
    } else {
      plot
    }
  })
  
  #### RENDER ARTIST GENRE BOXPLOTS ####
  output$genre_boxplot <- renderPlot({
    bplot <- artists %>%
      filter(artist_genre %in% c(input$genre1, input$genre2)) %>%
      ggplot(aes(x = artist_genre, y = artist_popularity)) +
      geom_boxplot(coef = Inf, fill = 'light green', color = 'grey') +
      geom_jitter(alpha = 1/3, color = 'light green') +
      labs(x = 'Genre', y = 'Artist Popularity') +
      theme_tufte()
    if (input$log_artist_popularity == FALSE) {
      bplot
    } else {
      bplot + scale_y_log10()
    }
  })
  
  output$mean_difference <- renderTable({
    if (input$log_artist_popularity == FALSE) {
      artists %>%
        filter(artist_genre %in% c(input$genre1, input$genre2)) %>%
        t.test(artist_popularity ~ artist_genre, data = .) %>%
        tidy() %>%
        select(`P-value` = p.value, Lower = conf.low, Upper = conf.high)
    } else {
      artists %>%
        mutate(logPopularity = log(artist_popularity)) %>%
        filter(artist_genre %in% c(input$genre1, input$genre2)) %>%
        t.test(logPopularity ~ artist_genre, data = .) %>%
        tidy() %>%
        select(`P-value` = p.value, Lower = conf.low, Upper = conf.high)
    }
  })
  
  # Artist popularity predictions
  output$artist_lm_summary <- renderPrint({
    fit <- lm(track_popularity ~ artist_popularity, data = tracks_artists)
    summary(fit)
  })
  
  output$artist_lm_plots <- renderPlot({
    fit <- lm(track_popularity ~ artist_popularity, data = tracks_artists)
    par(mfrow = c(1,4))
    plot(fit)
  })
  
  
  ########### RENDER WORDCLOUD  ###########
  autoInvalidate <- reactiveTimer(1000, session)
  output$wordcloud <- renderPlot({
    if (input$animation == FALSE) {
      tracks_word %>%
        filter(release_year == input$year) %>%
        unnest_tokens(word, track_name) %>%
        anti_join(stop_words) %>%
        filter(str_to_lower(word) != 'version', str_to_lower(word) != 'remix', str_to_lower(word) != 'mix', str_to_lower(word) != 'feat', str_to_lower(word) != 'remix', str_to_lower(word) != 'vivo', str_to_lower(word) != 'de', str_to_lower(word) != 'ao', str_to_lower(word) != 'edit', str_to_lower(word) != 'live', str_to_lower(word) != 'original', str_to_lower(word) != 'song', str_to_lower(word) != 'radio', str_to_lower(word) != 'soundtrack', str_to_lower(word) != 'instrumental', str_to_lower(word) != 'la', str_to_lower(word) != 'op', str_to_lower(word) != 'en') %>%
        count(word, sort = TRUE) %>%
        top_n(200) %>%
        mutate(word = reorder(word, n)) %>%
        with(wordcloud(word, n, max.words = Inf,random.order = FALSE, colors = brewer.pal(4,"Dark2")))
    } else {
      autoInvalidate()
      tracks_word %>%
        filter(release_year == input$year) %>%
        unnest_tokens(word, track_name) %>%
        anti_join(stop_words) %>%
        filter(str_to_lower(word) != 'version', str_to_lower(word) != 'remix', str_to_lower(word) != 'mix', str_to_lower(word) != 'feat', str_to_lower(word) != 'remix', str_to_lower(word) != 'vivo', str_to_lower(word) != 'de', str_to_lower(word) != 'ao', str_to_lower(word) != 'edit', str_to_lower(word) != 'live', str_to_lower(word) != 'original', str_to_lower(word) != 'song', str_to_lower(word) != 'radio', str_to_lower(word) != 'soundtrack', str_to_lower(word) != 'instrumental', str_to_lower(word) != 'la', str_to_lower(word) != 'op', str_to_lower(word) != 'en') %>%
        count(word, sort = TRUE) %>%
        top_n(200) %>%
        mutate(word = reorder(word, n)) %>%
        with(wordcloud(word, n, max.words = Inf,random.order = FALSE, colors = brewer.pal(4,"Dark2")))
    }
  })
  
}

shinyApp(ui, server)

