# IMDB_Viz_R

* Authors: Yukun Zhang, Gaoxiang Wang, Guron Mike and Radhakrishnan Arjun.

The dashboard aims to help movie enthusiasts to discover and explore new movies through an interactive and intuitive interface that provides information on a vast database of movies based on various metrics, presented through engaging visuals, and designed to be user-friendly and accessible.

## Dashboard Description

Our dashboard contains a landing page that displays the top 10 movies on IMDB with radio buttons to sort by IMDB rating, the number of votes, or the gross revenue. From drop-down lists, users can filter movies based on genre, release year, and/or age rating. The thumbnail, title, and chosen sort criteria value are shown for each of the top 10 movies. 

Our dashboard also contains a second page that will recommend 6 movies for the user based on their preferences with radio buttons to sort by IMDB rating, the number of votes, or runtime.  Users can use drop-down lists to select multiple genres, age ratings, directors, and/or actors of interest, as well as sliders to indicate the minimum IMDB rating, release year range, and runtime range they prefer.  The thumbnail, title, and a short summary are shown for each of the 6 movies.

Finally, our app contains a third page to provide users with some metrics and plots to visualize their movie preferences based on the same filters applied on the movie recommendation page.  There are 4 high level summary metrics showing the number of movies, the average IMDB rating, the average runtime, and the average number of votes for all movies based on the applied filters.  There are also 3 reactive plots that show the distribution of IMDB ratings by selected genres (boxplots), the distribution of runtimes by selected genre (boxplots), and the number of movies by genre (bar chart).  The boxplots and bars are coloured by genres.

This dashboard is aimed at providing users with an easy-to-use and efficient way to find and select the movies they'll love.

## App Sketch

<img src="images/Landing_Page.jpg"/>

<img src="images/Recommendations.jpg"/>

<img src="images/Metrics_Plots.jpg"/>

## License

`IMDB_Viz_R` was created by Yukun Zhang, Gaoxiang Wang, Guron Mike and Radhakrishnan Arjun. It is licensed under the terms of the MIT license.
