#Project Name: A BI Exploration of Spotify Songs


#Dataset
#30000 spotify songs from Joakin Arvidsson on Kaggle.

#Description
#The data this week comes from Spotify via the spotifyr package. Charlie Thompson, Josiah Parry, 
#Donal Phipps, and Tom Wolff authored this package to make it easier to get either your own data 
#or general metadata arounds songs from Spotify's API. Make sure to check out the 
#spotifyr package website to see how you can collect your own data!

#Training vs Test data
#We will train the model with 20000 songs and test it will 10000 songs.


#Attributes information:


#data source: https://www.kaggle.com/datasets/joebeachcapital/30000-spotify-songs


#Transformation of the data:

#First we will load the dataset

library(readr)

spotify_songs_data<-read_csv(
  "data/spotify_songs.csv"
)

View(spotify_songs_data)

dim(spotify_songs_data)

#We will now Identify the data types in the datatset. ThiS will help us choose the
#most efficient visualization types and algorithms that we can use.

sapply(spotify_songs_data, class)

#Now we will perform descriptive statistics.

#1. Measure of frequency.

# Here we will try to understand number of instances that belong to each class.

song_data_genre_freq<- spotify_songs_data$playlist_genre
cbind(frequency=table(song_data_genre_freq),
      percentage=prop.table(table(song_data_genre_freq))*100)

#Here we can see that we have:
# 6043 edm
# 5155 latin
# 5507 pop
# 5431 r&b
# 5746 rap
# 4951 rock

song_data_mode_freq <- spotify_songs_data$mode
cbind(frequency=table(song_data_mode_freq),
                 percentage=prop.table(table(song_data_mode_freq))*100)

#Here we can see the modality of the tracks are:
# major- 18574 (1)
# minor- 14259 (0)


song_data_subgenre_freq <- spotify_songs_data$playlist_subgenre
cbind(frequency=table(song_data_subgenre_freq),
      percentage=prop.table(table(song_data_subgenre_freq))*100)


# Calculating the Mode

song_data_genre_mode <- names(table(spotify_songs_data$playlist_genre))[
  which(table(spotify_songs_data$playlist_genre)==max(table(spotify_songs_data$playlist_genre)))
]
print(song_data_genre_mode)


song_data_subgenre_mode <- names(table(spotify_songs_data$playlist_subgenre))[
  which(table(spotify_songs_data$playlist_subgenre)==max(table(spotify_songs_data$playlist_subgenre)))
]
print(song_data_subgenre_mode)


song_playlistname_mode <- names(table(spotify_songs_data$playlist_name))[
  which(table(spotify_songs_data$playlist_name)==max(table(spotify_songs_data$playlist_name)))
]
print(song_playlistname_mode)


# Measuring the distribution of the data for each variable

summary(spotify_songs_data)

#Measuring the standard deviation of each variable
#   Important point to note is Low variability is ideal because it means that you
#   can predict information about the population based on sample data. High variability
#   means that the values are less consistent, thus making it harder to make predictions.

sapply(spotify_songs_data[, c(4,12,13,14,15,16,17,18,19,20,21,22,23)], sd)

#Meassuring the variance of each variable

sapply(spotify_songs_data[, c(4,12,13,14,15,16,17,18,19,20,21,22,23)], var)

#Measuring the Kurtosis of each variable
#   Kurtosis informs you of how often outliers occur in the results

#First we will install the necessary packages

if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

sapply(spotify_songs_data[, c(4,12,13,14,15,16,17,18,19,20,21,22,23)], kurtosis, type=2)


#Measuring the skewness of each variable

sapply(spotify_songs_data[, c(4,12,13,14,15,16,17,18,19,20,21,22,23)], skewness, type=2)


#Measuring the relationship

songs_data_cov <- cov(spotify_songs_data[, c(4,12,13,14,15,16,17,18,19,20,21,22,23)])
View(songs_data_cov)


#Measuring correlation between variables

song_data_cor <- cor(spotify_songs_data[,c(4,12,13,14,15,16,17,18,19,20,21,22,23)])
View(song_data_cor)

#One way ANOVA to see if there are significant differences in the means of track
#popularity across different genres

song_data_oneway_anova <- aov(track_popularity ~ playlist_genre, data = spotify_songs_data)
summary(song_data_oneway_anova)


#Two-way ANOVA to test the impact of both playlist genre and playlist subgenre on 
#danceability of a song

song_data_twoway_anova<- aov(danceability ~ playlist_genre* playlist_subgenre, data= spotify_songs_data)
summary(song_data_twoway_anova)

#Two-way ANOVA to test the impact of both playlist genre and playlist subgenre on 
#popularity of a song

song_data_twoway_anova<- aov(track_popularity ~ playlist_genre* playlist_subgenre, data= spotify_songs_data)
summary(song_data_twoway_anova)


#Two-way ANOVA to test the impact of both playlist genre and playlist subgenre on 
#acousticness of a song

song_data_twoway_anova<- aov(acousticness ~ playlist_genre* playlist_subgenre, data= spotify_songs_data)
summary(song_data_twoway_anova)


#Basic visualization to understand the dataset
#   Histogram to represent the dataset


# Assuming your dataset is named "your_dataset"
par(mfrow = c(1, 3))  

hist(spotify_songs_data[, 21], main = names(spotify_songs_data)[21])
hist(spotify_songs_data[, 23], main = names(spotify_songs_data)[23])
hist(spotify_songs_data[, 22], main = names(spotify_songs_data)[22])
hist(spotify_songs_data[, 20], main = names(spotify_songs_data)[20])
hist(spotify_songs_data[, 19], main = names(spotify_songs_data)[19])
hist(spotify_songs_data[, 18], main = names(spotify_songs_data)[18])
hist(spotify_songs_data[, 17], main = names(spotify_songs_data)[17])
hist(spotify_songs_data[, 16], main = names(spotify_songs_data)[16])


boxplot(spotify_songs_data[, 21], main = names(spotify_songs_data)[21])
boxplot(spotify_songs_data[, 23], main = names(spotify_songs_data)[23])
boxplot(spotify_songs_data[, 22], main = names(spotify_songs_data)[22])
boxplot(spotify_songs_data[, 20], main = names(spotify_songs_data)[20])
boxplot(spotify_songs_data[, 19], main = names(spotify_songs_data)[19])
boxplot(spotify_songs_data[, 18], main = names(spotify_songs_data)[18])
boxplot(spotify_songs_data[, 17], main = names(spotify_songs_data)[17])
boxplot(spotify_songs_data[, 16], main = names(spotify_songs_data)[16])


#Creating a map to identify the missing data in each dataset

if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE)
}
require("Amelia")
missmap(spotify_songs_data, col = c("red", "grey"), legend = TRUE)

#We now create a correlation plot

if (!is.element("corrplot", installed.packages()[, 1])) {
  install.packages("corrplot", dependencies = TRUE)
}
require("corrplot")


corrplot(cor(spotify_songs_data[c(4,12,13,14,15,16,17,18,19,20,21,22,23)]), method = "circle")

#We will now perform data Imputation. We will try and check for missing values and
# try to remove them for data consistency.

#First we will install the necessary packages to perform imputation

if(!is.element("NHANES", installed.packages()[, 1])){
  install.packages("NHANES", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

require("NHANES")


if(!is.element("dplyr", installed.packages()[, 1])){
  install.packages("dplyr", dependencies=TRUE,
                   repos = "https://cloud.r-project.org")
}

require("dplyr")


if(!is.element("naniar", installed.packages()[, 1])){
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

require("naniar")


if(!is.element("mice", installed.packages()[, 1])){
  install.packages("mince", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

require("mice")


if(!is.element("Amelia", installed.packages()[, 1])){
  install.packages("Amelia", dependencies = TRUE,
                   repos="https://cloud.r-project.org")
}

require("Amelia")

#We now confirm if there are any missing values

any_na(spotify_songs_data)


#We check how many are missing

n_miss(spotify_songs_data)


#For clarity, we check what percentage of missing data is in the entire dataset

prop_miss(spotify_songs_data)

#How many missing values does each variable have

spotify_songs_data %>% is.na() %>% colSums()


#Checking the number and percentage of missing values grouped by each observation

miss_var_summary(spotify_songs_data)


miss_case_summary(spotify_songs_data)

#Checking which variable contain the most missing values

gg_miss_var(spotify_songs_data)



#Checking where the missing values located(The shaded regions in the plot)

vis_miss(spotify_songs_data)+ theme(axis.text.x = element_text(angle=80))


#which combinations fo variables are missing together
gg_miss_upset(spotify_songs_data)


#creating a heatmap of missingness

is.factor(spotify_songs_data$track_name)
