# Spotify Data Analysis

Rui Huang

# Summary

The purpose of this project is to explore the patterns of audio features of tracks 
and artists data that I retrieved through Spotify's API requests. I explore the 
relationship between any two audio features. I found energy to be positively
correlated with loudness, danceability to be positively correlated with valence. I
also discovered the artist popularity in between genres. I found that Hip-Hop tends
to have higher artist popularity comparing to the rest of the genre.

# Shiny App

The shiny app in the "app" folder presents our findings on tracks audio features, analyses on
artists popularity and artist genres, as well as our regression analyses on track popularity. 
There are 5 tabs:

1. Home: The front page of our presentation.
2. Sample Data: Contains our tracks data and artists data.
3. Data Dictionary: A dictionary for track audio features.
4. Tracks Analyses: Our analyses findings on tracks.
5. Artists Analyses: Our analyses findings on artists.
6. Regression Analysis: Our regression models on track popularity.
7. Favorite Words: Word clouds for popular words in track names.

# Analyses

This Shiny App was motivated by the analyses run in the "analysis" folder.
The analysis folder contains the following files:

- spotify_analysis.Rmd. This file summarizes the exploratory data analysis on our data.
- spotify_predictive_analysis.Rmd. This file contains our prediction analysis findings.

# Running the app
To download and run the Shiny App, do the following:

1. Download tracks.csv and aritsts.csv data files from the "data" folder.
2. Make sure you install the following packages:
	- shiny
	- shinythemes
	- ggplot2
	- tidyverse
	- broom
	- RColorBrewer
	- wordcloud
	- tidytext
	- ggthemes
	- corrplot
	- car
	- shinydashboard
	- fmsb
	- radarchart
	- ECharts2Shiny
3. When running the app, click "Run App" button instead of using "Ctrl + Return", otherwise
the image in the home page won't render. This is a bug I found.

# References

We used the following resources when building our app:

- https://www.rdocumentation.org/packages/ECharts2Shiny/versions/0.2.13/topics/renderRadarChart
- https://shiny.rstudio.com/articles/images.html
- https://stackoverflow.com/questions/50182910/load-shinydashboard-image
- https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
- https://stackoverflow.com/questions/21996887/embedding-image-in-shiny-app
