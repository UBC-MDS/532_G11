# Proposal: IMDB Movie Metrics Dashboard

## Motivation and purpose

Our role: Data consultancy firm

Target audience: The Discerning Movie Enthusiast Seeking an Immersive Cinematic Experience of Their Liking.

Our target audience consists of individuals who are passionate about film and are always on the lookout for new and exciting cinematic experiences. The dashboard aims to provide a valuable resource for movie enthusiasts who are looking to discover and explore new movies to watch but are overloaded with information on the internet. With countless options to choose from and varying levels of quality, we understand that the process of finding a great movie could often be overwhelming and time-consuming. Through our dashboard, our users can get an interactive and intuitive interface that allows them to explore a vast database of movies and discover exciting titles based on various metrics, including the movie's runtime, genre, rating, and gross earnings. We highlight the unique features and characteristics of each movie through engaging and immersive visuals in a web portal that is designed to be user-friendly and accessible. Whether you are randomly looking for the next movie to watch or a seasoned movie critique, our dashboard provides you with all the necessary information for discovering the metrics that matter the most through effective visualizations.

## Description of the data

For the dashboard, we're sourcing the data from Kaggle, which is a well-known platform for data science and machine learning projects. It can be accessed [here](https://www.kaggle.com/datasets/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows). The dataset contains 1000 observations of movie data with a compressive set of features. Some of the key features are the movie's title (Series_Title), the certificate issued (Certificate), release year (Released_Year), runtime (Runtime), genre (Genre), IMDB rating (IMDB_Rating), meta score (Meta_score), director (Director), star power (Star1, Star2, Star3, Star4), number of votes (No_of_votes), and box office earnings (Gross). The dataset also contains a summary of the movie (Overview) which allows the users to quickly understand if the story is something they would prefer or not.

## Research questions and usage scenarios

We would like to explore the following research questions:
* Which year has the largest number high-rated movies? 
* Which combination of actors are getting good IMDB_Rating maximum time?
* What are the top recommended six movies for users based on their preferences? 

Now imagine the following usage scenarios:

Jack is a movie lover who loves watching and reviewing movies in his spare time. He's always on the lookout for new movies to add to his watch list and is always interested in discovering cool movies he never watched before. 

One day, Jack discovers the raw IMDB dataset, which contains a vast collection of movie information, including ratings, reviews, and other relevant data. But he has no technical skills and it is hard for him to process the raw data. Luckily, he has found out our dashboard for IMDB. 

By using our dashboard, Jack can filters the data based on the highest-rated movies and selects a few movies that he hasn't seen before with interactive and intuitive interface experiences. He can also use our visualization to explore the most popular movies of the year and checks out the cast and crew members of these movies to see if they have worked on any of his favorite films. Based on movie run time sorting, he can choose the proper movie and allocate enough time in his schedule to solely enjoy his movie time at home without any interpretation. 

## App Sketch

<img src="../images/Landing_Page.JPG"/>

<img src="../images/Recommendations.JPG"/>

<img src="../images/Metrics_Plots.JPG"/>


