##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
# Package "tidyverse" has many uses when manipulating data
library(tidyverse)
# Package "caret" has many uses when working with machine learning
library(caret)
# Package "data.table" has many uses when working with tables
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(data.table)
# Package "ggplot2" is used for data visualization
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(ggplot2)
# Package "Rcpp" (required when testing the script in other computers)
if(!require(Rcpp)) install.packages("Rcpp", repos = "http://cran.us.r-project.org")
library(Rcpp)
# Package "lubridate" is used with the timestamp in order to handle date-time
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)
# Package "recosystem" is a Recommender System used on the analysis 
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#title = as.character(title),
#genres = as.character(genres))
# if using R 4.0 or later:

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################################################################
# TRAINING AND TEST DATA FRAMES
################################################################################
# Creating training and test data frames
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
training_dataframe <- edx[-test_index,]
temp <- edx[test_index,]
# Make sure userId and movieId in training_dataframe are also in test_dataframe
test_dataframe <- temp %>%
  semi_join(training_dataframe, by = "movieId") %>%
  semi_join(training_dataframe, by = "userId")
# Add rows removed from test_dataframe set back into training_dataframe
# Adding back rows into train set
removed <- anti_join(temp, test_dataframe)
training_dataframe <- rbind(training_dataframe, removed)
rm(test_index, temp, removed)
################################################################################
# edx GENERAL OVERVIEW
################################################################################
# As function sumary() returns no useful information about 'edx' data frame, 
# it was manually processed.
# edx has 10677 movies.
length(unique(edx$movieId))
# edx has 797 unique genres.
length(unique(edx$genres))
# As the number of genres was quite large, I decided to check it out.
# It turned out that the genres can also be the combination of more than one.
head(unique(edx$genres),30)
# edx has 69878 unique userIds, which means that 69878 users rated the movies.
length(unique(edx$userId))
# The highest rating is 5.
max(edx$rating)
# The rating "5" was given 1390114 times.
sum (edx$rating==5)
# The lowest rating is 0.5
min(edx$rating)
# The rating "0.5" was given 85374 times.
sum (edx$rating==0.5)
# The average rating of edx 3.512465.
mean(edx$rating)
################################################################################
# 'edx' GENRES OVERVIEW
################################################################################
# 5336 movies marked with "Drama" genre.
drama <- edx %>% filter(str_detect(genres,"Drama"))
length(unique(drama$movieId))
# The highest rating is 5.
max(drama$rating)
# The rating 5 was given 711447 times.
sum(drama$rating==5)
# The lowest rating is 0.5
min(drama$rating)
# The rating 5 was given 23282 times.
sum(drama$rating==0.5)
# The average rate is 3.673131.
mean(drama$rating)

# 3703 movies marked with "Comedy" genre.
comedy <- edx %>% filter(str_detect(genres,"Comedy"))
length(unique(comedy$movieId))
# The highest rating is 5.
max(comedy$rating)
# The rating 5 was given 492512 times.
sum(comedy$rating==5)
# The lowest rating is 0.5
min(comedy$rating)
# The rating 5 was given 38345 times.
sum(comedy$rating==0.5)
# The average rate is 3.436908.
mean(comedy$rating)

# 1705 movies marked with "Thriller" genre.
thriller <- edx %>% filter(str_detect(genres,"Thriller"))
length(unique(thriller$movieId))
# The highest rating is 5.
max(thriller$rating)
# The rating 5 was given 332142 times.
sum(thriller$rating==5)
# The lowest rating is 0.5
min(thriller$rating)
# The rating 5 was given 19250 times.
sum(thriller$rating==0.5)
# The average rate is 3.507676.
mean(thriller$rating)

# 1685 movies marked with "Romance" genre.
romance <- edx %>% filter(str_detect(genres,"Romance"))
length(unique(romance$movieId))
# The highest rating is 5.
max(romance$rating)
# The rating 5 was given 274910 times.
sum(romance$rating==5)
# The lowest rating is 0.5
min(romance$rating)
# The rating 5 was given 13533 times.
sum(romance$rating==0.5)
# The average rate is 3.553813.
mean(romance$rating)

# 1473 movies marked with "Action" genre.
action <- edx %>% filter(str_detect(genres,"Action"))
length(unique(action$movieId))
# The highest rating is 5.
max(action$rating)
# The rating 5 was given 340607 times.
sum(action$rating==5)
# The lowest rating is 0.5
min(action$rating)
# The rating 5 was given 27453 times.
sum(action$rating==0.5)
# The average rate is 3.421405
mean(action$rating)

# 1025 movies marked with "Action" genre.
adventure <- edx %>% filter(str_detect(genres,"Adventure"))
length(unique(adventure$movieId))
# The highest rating is 5.
max(adventure$rating)
# The rating 5 was given 281465 times.
sum(adventure$rating==5)
# The lowest rating is 0.5
min(adventure$rating)
# The rating 5 was given 23282 times.
sum(adventure$rating==0.5)
# The average rate is 3.493544
mean(adventure$rating)
################################################################################
## 'edx' USERS, RATINGS and TIMESTAMP
################################################################################
# Ratings histogram
ratings_qty <- edx %>% group_by(rating) %>%
  summarize(count = n())
ratings_histogram <- ratings_qty %>% mutate(rating = factor(rating)) %>%
  ggplot(aes(rating, count)) +
  geom_col(fill = "gray")+ 
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Ratings", y = "Qty",
       tag = "Fig. 1 - Histogram showing the quantity of each given rating.") +
  theme(plot.tag.position = "bottom")+ 
  ggtitle("Histogram of ratings")

# Histogram showing the number of users per given ratings
ratings_user_hist <- edx %>% group_by(userId) %>%
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(color = "black", fill = "gray", bins = 30) +
  theme_minimal()+
  scale_x_log10() +
  labs(title = "Histogram - ratings per user", x = "Ratings given (logarithm scale)", y = "Users", tag = "Fig. 2 - Histogram showing the quantity of users and rating given.")+
  theme(plot.tag.position = "bottom")

# Creating a summary table grouped by userId
ratings_per_user <- edx %>% group_by(userId) %>%
  summarize(ratings_per_user_qty = n(),
            ratings_given_mean = mean(rating),
            mu_user = mean(rating),
            sd_user = sd(rating),)

# Average of 128.7967 ratings given per user
mean(ratings_per_user$ratings_per_user_qty)

# Sorting (descending) and showing the users according to their number of rating
ratings_per_user_descending <- ratings_per_user %>% arrange(desc(ratings_per_user_qty))
head(ratings_per_user_descending)

# Sorting (ascending) and showing the users according to their number of rating
ratings_per_user_ascending <- ratings_per_user %>% arrange(ratings_per_user_qty) 
head(ratings_per_user_ascending)

# Density plot showing ratings per user
ratings_user_density <- ratings_per_user %>% ggplot(aes(ratings_per_user_qty)) +
       geom_density(fill = "gray", alpha = 0.8) + 
       geom_vline(aes(xintercept = mean(ratings_per_user_qty)), color = "red", linetype="dashed", size=1)+
       theme_minimal()+  
  annotate("text", x = 215, y = 0.77, label = "avg=128.79",
                                  color = "red", size = 4, fontface =2) +
  scale_x_log10() +
  labs(title = "Density plot - ratings per user", x = "Ratings given (logarithm scale)", y = "Density", tag = "Fig. 3 - Density plot showing the quantity of each given rating.") +
  theme(plot.tag.position = "bottom")

# Histogram showing the number of ratings per year
ratings_year_histogram <- edx %>% mutate(release = round_date(as_datetime(timestamp), unit = "year")) %>% 
  group_by(release) %>%
  summarize(count = n()) %>%
  ggplot(aes(release, count)) +
  geom_col(fill = "gray")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "Histogram - ratings per year", x = "Year", y = "Qty", tag = "Fig. 4 - Histogram showing the quantity of ratings per year.")+
  theme(plot.tag.position = "bottom")

# 2 ratings given in 1995 (timestamp of January first 1996)
sum(edx$timestamp <= 820522677)

################################################################################
################################################################################
###TESTS
################################################################################
################################################################################

################################################################################
###INCREMENTAL APPROACH
################################################################################
###ONLY THE MEAN
# Calculating the mean of the training data frame
mean_rating_test <- mean(training_dataframe$rating)
# RMSE between mean_rating_test and the ratings in the test data frame
rmse_mean_test <- RMSE(test_dataframe$rating, mean_rating_test)
rmse_mean_test

# Calculating the mean of the edx data frame
mean_rating_validation <- mean(edx$rating)
# RMSE between mean_rating_validation and the ratings in "validation" data frame
rmse_mean_validation  <- RMSE(validation$rating, mean_rating_validation)
mean_rating_validation

###MEAN AND MOVIE EFFECT/BIAS
movie_effect_test <- training_dataframe %>%  group_by(movieId) %>%  
  summarize(movie_effect = mean(rating - mean_rating_test))
prediction_movie_test <- mean_rating_test + test_dataframe %>% 
  left_join(movie_effect_test, by = "movieId") %>%  
  pull(movie_effect)
rmse_mean_movie_test <- RMSE(prediction_movie_test, test_dataframe$rating)
rmse_mean_movie_test

movie_effect_validation <- edx %>%  group_by(movieId) %>%  
  summarize(movie_effect = mean(rating - mean_rating_validation))
prediction_movie_validation <- mean_rating_validation + validation %>% 
  left_join(movie_effect_validation, by = "movieId") %>%  pull(movie_effect)
rmse_mean_movie_validation <- RMSE(prediction_movie_validation, validation$rating)
rmse_mean_movie_validation

###MEAN, MOVIE AND USER EFFECT/BIAS
user_movie_effect_test <- training_dataframe %>%  
  left_join(movie_effect_test, by = "movieId") %>%  
  group_by(userId) %>%
  summarize(user_movie_effect = mean(rating - mean_rating_test - movie_effect))
prediction_user_test <- test_dataframe %>%  
  left_join(movie_effect_test, by = "movieId") %>%
  left_join(user_movie_effect_test, by = "userId") %>%  
  mutate(user_movie_effect = mean_rating_test + movie_effect + user_movie_effect) %>%
  pull(user_movie_effect)
rmse_mean_user_movie_test <- RMSE(prediction_user_test, test_dataframe$rating)
rmse_mean_user_movie_test

user_movie_effect_validation <- edx %>%  
  left_join(movie_effect_validation, by = "movieId") %>%  
  group_by(userId) %>%
  summarize(user_movie_effect = mean(rating - mean_rating_validation - movie_effect))
predicted_ratings <- validation %>%  
  left_join(movie_effect_validation, by = "movieId") %>%
  left_join(user_movie_effect_validation, by = "userId") %>%  
  mutate(user_movie_effect = mean_rating_validation + movie_effect + user_movie_effect) %>%
  pull(user_movie_effect)
rmse_mean_user_movie_validation <- RMSE(predicted_ratings, validation$rating)
rmse_mean_user_movie_validation

# Compiling all results and and showing them in a table
results_incremental_approach<- tibble(predictors = "mean", RMSE = rmse_mean_test, type = "test")
results_incremental_approach<- bind_rows(results_incremental_approach, tibble(predictors = "mean", RMSE = rmse_mean_validation, type = "validation"))
results_incremental_approach<- bind_rows(results_incremental_approach, tibble(predictors = "mean, movie", RMSE = rmse_mean_movie_test, type = "test"))
results_incremental_approach<- bind_rows(results_incremental_approach, tibble(predictors = "mean, movie", RMSE = rmse_mean_movie_validation, type = "validation"))
results_incremental_approach<- bind_rows(results_incremental_approach, tibble(predictors = "mean, movie, user", RMSE = rmse_mean_user_movie_test, type = "test"))
results_incremental_approach<- bind_rows(results_incremental_approach, tibble(predictors = "mean, movie, user", RMSE = rmse_mean_user_movie_validation, type = "validation"))

results_incremental_approach%>% knitr::kable()
################################################################################
###LINEAR MODELS
################################################################################
# lm() function using timestamp as a predictor.
model <- lm(formula=rating ~ timestamp, data = training_dataframe)
prediction_lm_test_ts <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts <- RMSE(prediction_lm_test_ts, test_dataframe$rating)
model <- lm(formula=rating ~ timestamp, data = edx)
prediction_validation_ts <- predict(model, newdata = validation)
rmse_lm_validation_ts <- RMSE(prediction_validation_ts, validation$rating)
# lm() function using timestamp and userId as predictors.
model <- lm(formula=rating ~ timestamp+userId, data = training_dataframe)
prediction_lm_test_ts_ui <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts_ui <- RMSE(prediction_lm_test_ts_ui, test_dataframe$rating)
model <- lm(formula=rating ~ timestamp+userId, data = edx)
prediction_lm_validation_ts_ui <- predict(model, newdata = validation)
rmse_lm_validation_ts_ui <- RMSE(prediction_lm_validation_ts_ui, validation$rating)
# lm() function using timestamp, userId and movieId as predictors.
model <- lm(formula=rating ~ timestamp+userId+movieId, data = training_dataframe)
prediction_lm_test_ts_ui_mi <- predict(model, newdata = test_dataframe)
rmse_lm_test_ts_ui_mi <- RMSE(prediction_lm_test_ts_ui_mi, test_dataframe$rating)
model <- lm(formula=rating ~ timestamp+userId+movieId, data = edx)
prediction_lm_validation_ts_ui_mi <- predict(model, newdata = validation)
rmse_lm_validation_ts_ui_mi <- RMSE(prediction_lm_validation_ts_ui_mi, validation$rating)

# THE FOLLOWING LINES COULD NOT BE COMPUTED DUE TO THE VERY LARGE VECTOR SIZE
# Impossible to compute (> 48 Gb vector)
# model <- lm(formula=rating ~ timestamp+userId+movieId+genres, data = training_dataframe)
# prediction_test_ts <- predict(model, newdata = test_dataframe)
# RMSE(prediction_test_ts, test_dataframe$rating)
# Impossible to compute (> 48 Gb vector)
# model <- lm(formula=rating ~ genres, data = training_dataframe)
# prediction_test_ts <- predict(model, newdata = test_dataframe)
# RMSE(prediction_test_ts, test_dataframe$rating)

# Compiling all results and and showing them in a table
results_lm <- tibble(predictors = "timestamp", RMSE = rmse_lm_test_ts, type = "test")
results_lm <- bind_rows(results_lm, tibble(predictors = "timestamp", RMSE = rmse_lm_validation_ts, type = "validation"))
results_lm <- bind_rows(results_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_lm_test_ts_ui, type = "test"))
results_lm <- bind_rows(results_lm, tibble(predictors = "timestamp and userId", RMSE = rmse_lm_validation_ts_ui, type = "validation"))
results_lm <- bind_rows(results_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_lm_test_ts_ui_mi, type = "test"))
results_lm <- bind_rows(results_lm, tibble(predictors = "timestamp, userId and movieId", RMSE = rmse_lm_validation_ts_ui_mi, type = "validation"))
results_lm %>% knitr::kable()

rm(model)
################################################################################
###RECOSYSTEM
################################################################################
set.seed(1, sample.kind="Rounding")
# Test and validation data frames to use with "recosystem" library
train_recosys <- with(training_dataframe, data_memory(user_index = userId, item_index = movieId, rating = rating))
test_recosys <- with(test_dataframe, data_memory(user_index = userId, item_index = movieId, rating = rating))
edx_recosys <- with(edx, data_memory(user_index = userId, item_index = movieId, rating = rating))
validation_recosys <- with(validation, data_memory(user_index = userId, item_index = movieId, rating = rating))

# Creating an object of class RecoSys called "r"
r <- Reco()
# Setting tuning parameters
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 1))
# Training stage
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(test_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
# Printing RMSE
rmse_1_iter_tests

# Setting tuning parameters
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 1))
# Training stage
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 1))
# Predicting results
prediction_recosys <- r$predict(validation_recosys, out_memory())
# Calculating RMSE
rmse_1_iter_final <- RMSE(prediction_recosys, validation$rating)
# Printing RMSE
rmse_1_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 2))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_2_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_2_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 2))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 2))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_2_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_2_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 3))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_3_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_3_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 3))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 3))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_3_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_3_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 4))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_4_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_4_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 4))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 4))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_4_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_4_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 5))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_5_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_5_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 5))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 5))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_5_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_5_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 6))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 6))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_6_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_6_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 6))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 6))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_6_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_6_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 7))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 7))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_7_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_7_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 7))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 7))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_7_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_7_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 8))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 8))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_8_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_8_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 8))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 8))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_8_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_8_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 9))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 9))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_9_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_9_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 9))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 9))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_9_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_9_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 10))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 10))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_10_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_10_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 10))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 10))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_10_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_10_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 11))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 11))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_11_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_11_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 11))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 11))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_11_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_11_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 12))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 12))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_12_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_12_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 12))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 12))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_12_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_12_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 13))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 13))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_13_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_13_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 13))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 13))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_13_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_13_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 14))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 14))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_14_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_14_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 14))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 14))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_14_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_14_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 15))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 15))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_15_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_15_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 15))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 15))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_15_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_15_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 16))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 16))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_16_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_16_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 16))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 16))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_16_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_16_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 17))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 17))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_17_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_17_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 17))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 17))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_17_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_17_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 18))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 18))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_18_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_18_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 18))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 18))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_18_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_18_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 19))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 19))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_19_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_19_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 19))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 19))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_19_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_19_iter_final
################################################################################
tuning_parameters <- r$tune(train_recosys, opts = list(dim = c(20, 30),
                                                       nthread = 4, niter = 20))
r$train(train_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 20))
prediction_recosys <- r$predict(test_recosys, out_memory())
rmse_20_iter_tests <- RMSE(prediction_recosys, test_dataframe$rating)
rmse_20_iter_tests
tuning_parameters <- r$tune(edx_recosys, opts = list(dim = c(20, 30),
                                                     nthread = 4, niter = 20))
r$train(edx_recosys, opts = c(tuning_parameters$min, nthread = 4, niter = 20))
prediction_recosys <- r$predict(validation_recosys, out_memory())
rmse_20_iter_final <- RMSE(prediction_recosys, validation$rating)
rmse_20_iter_final

# Compiling all results and and showing them in a table
results_recosys <- tibble(iterations = 1, RMSE = rmse_1_iter_tests, type = "test")
results_recosys <- bind_rows(results_recosys, tibble(iterations = 1, RMSE = rmse_1_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 2, RMSE = rmse_2_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 2, RMSE = rmse_2_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 3, RMSE = rmse_3_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 3, RMSE = rmse_3_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 4, RMSE = rmse_4_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 4, RMSE = rmse_4_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 5, RMSE = rmse_5_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 5, RMSE = rmse_5_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 6, RMSE = rmse_6_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 6, RMSE = rmse_6_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 7, RMSE = rmse_7_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 7, RMSE = rmse_7_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 8, RMSE = rmse_8_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 8, RMSE = rmse_8_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 9, RMSE = rmse_9_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 9, RMSE = rmse_9_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 10, RMSE = rmse_10_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 10, RMSE = rmse_10_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 11, RMSE = rmse_11_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 11, RMSE = rmse_11_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 12, RMSE = rmse_12_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 12, RMSE = rmse_12_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 13, RMSE = rmse_13_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 13, RMSE = rmse_13_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 14, RMSE = rmse_14_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 14, RMSE = rmse_14_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 15, RMSE = rmse_15_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 15, RMSE = rmse_15_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 16, RMSE = rmse_16_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 16, RMSE = rmse_16_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 17, RMSE = rmse_17_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 17, RMSE = rmse_17_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 18, RMSE = rmse_18_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 18, RMSE = rmse_18_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 19, RMSE = rmse_19_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 19, RMSE = rmse_19_iter_final, type = "validation"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 20, RMSE = rmse_20_iter_tests, type = "test"))
results_recosys <- bind_rows(results_recosys, tibble(iterations = 20, RMSE = rmse_20_iter_final, type = "validation"))
results_recosys %>% knitr::kable()

# Plotting the results
results_recosys_lineplot <- results_recosys %>% 
  group_by(type) %>%
  ggplot( aes(x=iterations, y=RMSE, group=type, color=type)) +
  theme_minimal() + geom_line() +
  geom_hline(aes(yintercept = 0.865), color = "red", linetype="dashed", size=1)+
  annotate("text", x = 10, y = 0.87, label = "RMSE=8649 (goal)",
           color = "red", size = 4, fontface =2) +
  ggtitle("Timestamp, time unit : year") +
  labs(title = "recosystem - RMSE vs. iterations", x = "iterations", y = "RMSE", tag = "Fig. 5 - RMSE of with different iterations using recosystem.")+
  theme(plot.tag.position = "bottom")

