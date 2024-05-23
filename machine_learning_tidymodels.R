# packages
library(tidyverse)
library(tidymodels)
library(corrplot)

# import data
data <- cor_data <- iris

# linear correlation
cor_data$Species %>% as.integer() -> cor_data$Species
cor(cor_data) %>% corrplot()

# split data into training and testing
# default split 0.75 for costum use flag prop
data_split <- initial_split(data)

# for accessing training ans testng data
data_split %>% training() %>% glimpse()
data_split %>% testing() %>% glimpse()

# preprocess
# recipe() - starts a new set of transformations to be applied it's main argument is the model’s formula
# prep() - executes the transformations of the data
# each data transformation is a step
# step_corr() - removes variables that have large absolute correlations with other variables
# step_center() - normalizes numeric data to have a mean of zero
# step_scale() - normalizes numeric data to have a standard deviation of one
# all_outocomes() and all_predictors() provide a convenient way to specify groups of variables
data_recipe <- data_split %>% 
  training() %>%
  recipe(Species ~ .)%>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
data_recipe

# execute the preprocessing
# transformation of testing data
data_testing <- data_recipe %>%
  bake(testing(data_split))
glimpse(data_testing)
# load prepared training data
data_training <- juice(data_recipe)
glimpse(data_training)

# model training
# rand_forest() is used to initialize a Random Forest model
# fit() executes model 
data_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>%
  fit(Species ~ ., data = data_training)
# if we want to run the same model against randomForest, we just change the value in set_engine() to “randomForest”
data_rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(Species ~ ., data = data_training)

# predictions
predict(data_ranger, data_testing)
# add predictions to dataset for comparison
data_ranger %>%
  predict(data_testing) %>%
  bind_cols(data_testing) %>%
  glimpse()

# model validation
# ranger
data_ranger %>%
  predict(data_testing) %>%
  bind_cols(data_testing) %>%
  metrics(truth = Species, estimate = .pred_class)
# random forest
data_rf %>%
  predict(data_testing) %>%
  bind_cols(data_testing) %>%
  metrics(truth = Species, estimate = .pred_class)
# obtain the probability for each possible predicted value 
data_probs <- data_ranger %>%
  predict(data_testing, type = "prob") %>%
  bind_cols(data_testing)

# visualization
data_probs %>%
  gain_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()

data_probs%>%
  roc_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()


# TESTING
test_data <- iris

test_procesed <- data_recipe %>%
  bake(test_data)

test_predictions <- predict(data_ranger, test_procesed)
test_probability <- predict(data_ranger, test_procesed, type = "prob")

cbind(test_data,test_predictions,test_probability) %>% 
  select(Species, .pred_class, .pred_setosa, .pred_versicolor, .pred_virginica) %>% 
  filter(Species != .pred_class) -> mistakes
