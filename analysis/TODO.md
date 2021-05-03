# Todo

This file contains the details of your progress report. This should be
a fairly detailed description of (i) what you have done, (ii) what you
plan to do, and (iii) some ideas for what your shiny app will do. I
provide some guidance in this file, but you should use your own
words. That is, don't just fill in the blanks from what I wrote.

# Progress

1. All of our data were obtained through Spotify API requests. We have used the script 
    in "spotify_api.Rmd" to make the requests through a combination of the `httr` package 
    and the `spotifyr` package.
3. We ran some initial data explorations in our script stored in the "spotify_analysis.Rmd" file. 
    The analysis data is from the "spotify.csv" file under the "output" folder. These data are focused on 
    single tracks, along with their audio features and popularity defined by the Spotify's internal algorithms.
    Based on our analysis, we found:

# Conclusion

1. After running initial analysis, we did not find a strong relationship between track_popularity and audio features. We might need to add variable - genre to further analyze the relationships within each genre.

2. We might want to perform the decision tree algorithm to predict track popularity on the basis of audio-based metrics such as key, mode, and genre.


# Future analysis questions:

1. More Data
    1. We still desire to request more data from Spotify to polish our analysis goals:
        1. Genre for each track
        2. Release year for each track

2. Variable Selection
    1. Based on our analysis, a variable selection process is desired to help us build 
        a sustainable prediction model on track popularity.
        
Rui plans to request more data from Spotify per the team's needs. Sujata and Aliya will work
on variable selection to build a prediction model.

# Shiny App Ideas:

We are planning on making 3 tabs in our Shiny App:

1. One tab will allow the user to select an artist to visualize the frequencies of 
    words used in this artist’s track names.
2. One tab will allow the user to select any 2 different variables of audio features 
    to visualize the relationship between them. Depending on the data type of the 
    variables, the graph type will change accordingly.
3. One tab will use the prediction model we developed to predict the popularity of 
    a song given variables selected through our variable selection method.

