# this file is where we build the model for the outcome
# i try models with three sets of differing predictor variables.
# then i compare each against the same test data and see which does best

library(keras)
library(tidyverse)

# cleared the workspace - loading in prior saved data
balls <- read_csv('write_csv/balls_after_5.csv')
batters_data <- read_csv('write_csv/batters_data.csv')
bowlers_data <- read_csv('write_csv/bowlers_data.csv')
batters_df <- read_csv('write_csv/batters_df.csv')
bowlers_df <- read_csv('write_csv/bowlers_df.csv')
new_players_df <- read_csv('write_csv/new+players_df.csv')
players_df <- read_csv('write_csv/players_df.csv')
test_lineups <- read_csv('write_csv/test_lineups.csv')
train_lineups <- read_csv('write_csv/train_lineups.csv')

# before making the models i need to change the datatypes of many of the variables existing
# in the balls data

balls$is_powerplay <- as.logical(balls$is_powerplay)
balls$home_advantage_bat <- as.logical(balls$home_advantage_bat)
balls$is_freehit <- as.logical(balls$is_freehit)
balls$batter_position <- as.integer(balls$batter_position)
balls$average_overs <- as.integer(balls$average_overs)
balls$is_spin <- as.logical(balls$is_spin)
balls$outcome <- as.factor(balls$outcome)
balls$is_noball <- as.logical(balls$is_noball)
balls$venue <- as.factor(balls$venue)
balls$balls_remaining <- as.integer(balls$balls_remaining)
balls$wickets_remaining <- as.integer(balls$wickets_remaining)
balls$runs_required <- as.integer(balls$runs_required)
balls$missing_bat <- as.logical(balls$missing_bat)
balls$missing_bowl <- as.logical(balls$missing_bowl)

# also need to add 3 new variables to the data.

# aggression: defined as wickets remaining / balls remaining. cannot be greater than 1.
aggression <- ifelse(balls$wickets_remaining / balls$balls_remaining > 1, 1,
                     balls$wickets_remaining / balls$balls_remaining)

# required_run_rate: defined as runs required / balls remaining. cannot be greater than 6.
required_run_rate <- ifelse(balls$runs_required / balls$balls_remaining > 6, 6,
                            balls$runs_required / balls$balls_remaining)
# swapping nans for 0s (for modelling purposes)
required_run_rate <- ifelse(required_run_rate %>% is.na(), 0, required_run_rate)

# is_first_innings: logical vector.
is_first_innings <- ifelse(balls$inning == 1, TRUE, FALSE)

# adding the new variables to balls data
balls <- add_column(balls, aggression, required_run_rate, .after = 'wickets_remaining')
balls <- add_column(balls, is_first_innings, .after = 'inning')

# choosing variables for model1
x_vars1 <- c('venue', 'is_first_innings', 'is_powerplay', 'home_advantage_bat', 'is_freehit', 
             'aggression', 'required_run_rate', 'batter_position', 'missing_bat', 'bat_0', 
             'bat_1', 'bat_2', 'bat_4', 'bat_6', 'bat_bye', 'bat_legbye', 'bat_out', 'average_overs', 
             'is_spin', 'missing_bowl', 'bowl_0', 'bowl_1', 'bowl_2', 'bowl_4', 'bowl_6', 
             'bowl_bye', 'bowl_wicket', 'bowl_wide')

y_vars <- c('outcome')

x1 <- balls[, x_vars1]
y1 <- balls[, y_vars]

# one-hot encoding - changing factors and logical columns, to numbers by introducing dummy variables
library(caret)
dmy_x <- dummyVars(" ~ .", data = x1)
x_new1 <- data.frame(predict(dmy_x, newdata = x1))

dmy_y <- dummyVars(" ~ .", data = y1)
y_new1 <- data.frame(predict(dmy_y, newdata = y1))

# getting training and test set
set.seed(111)
test_data <- sample(1:nrow(x_new1), as.integer(nrow(x_new1))/10) %>% sort
x_train1 <- x_new1[-test_data, ]
x_test1 <- x_new1[test_data, ]
y_train1 <- y_new1[-test_data, ]
y_test1 <- y_new1[test_data, ]

# building the model
# adding batch normalization and dropout to reduce over-fitting
model_1 <- keras_model_sequential() %>%
  layer_dense(units = 25, activation = 'relu', input_shape = 67) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 11, activation = 'softmax') %>%
  
  # reduce learning rate from default
  compile(
    optimizer = optimizer_adam(learning_rate = 0.0001),
    loss = "categorical_crossentropy"
  )

# training the model
learn_1 <- model_1 %>%
  fit(x = as.matrix(x_train1), y = as.matrix(y_train1),
      epochs = 300,
      batch_size = 50,
      validation_split = 0.1,
      verbose = 2,
      callbacks = list(callback_early_stopping(patience = 50),
                       callback_reduce_lr_on_plateau()))

# saving the model
model_1_saved <- serialize_model(model_1)
plot(learn_1)

summary(model_1)

model_1 <- unserialize_model(model_1_saved)

# testing predictions
preds1 <- predict(model_1, as.matrix(x_test1))
colnames(preds1) <- colnames(y_test1)

predictions1 <- c()
actual1 <- c()
for(i in 1:ncol(preds1)) {
  predictions1[i] <- sum(preds1[, i])
  actual1[i] <- sum(y_test1[, i])
}

p_values <- ifelse(ppois(mod_comp$actual1, mod_comp$predictions1) > 0.5, 
                   (1 - ppois(mod_comp$actual1, mod_comp$predictions1))*2, 
                   ppois(mod_comp$actual1, mod_comp$predictions1)*2)

ppois(mod_comp$predictions1[1], mod_comp$actual1[1])

# comparison of runs: predicted vs actual
predictions_runs <- (predictions1[1:7]*c(0, 1, 2, 3, 4, 5, 6)) %>% sum()
actual_runs <- (actual1[1:7]*c(0, 1, 2, 3, 4, 5, 6)) %>% sum
p_value_runs <- ifelse(ppois(actual_runs, predictions_runs) > 0.5,
                       (1 - ppois(actual_runs, predictions_runs))*2,
                       ppois(actual_runs, predictions_runs)*2)

mod_comp <- tibble('outcome' = colnames(y_test1), actual1, predictions1, p_values)

#######################################################################33

