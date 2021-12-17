
# add new variable to balls data: number of byes
n_byes <- c()
for (i in 1:nrow(balls)) {
  if (balls$outcome[i] == 'bye') {
    n_byes <- n_byes %>% append(balls$extra_runs[i])    
  }
  else if (balls$outcome[i] == 'legbye') {
    n_byes <- n_byes %>% append(balls$extra_runs[i])
  }
  else if (balls$outcome[i] == 0 & balls$is_noball[i] == 1) {
    n_byes <- n_byes %>% append(balls$extra_runs[i] - 1)
  }
  else if (balls$outcome[i] == 'wide') {
    n_byes <- n_byes %>% append(balls$extra_runs[i] - 1)
  }
  else {
    n_byes <- n_byes %>% append(0)
  }
}

balls <- add_column(balls, n_byes, .after = 'is_noball')

# changing to factor
balls$n_byes <- as.factor(balls$n_byes)

# get subset of balls data for where byes are possible
byes_balls <- subset(balls, (outcome == 'bye' | outcome == 'legbye' | outcome == 'wide' |
                               (outcome == 0 & is_noball == TRUE)))

byes_balls$outcome <- droplevels(byes_balls$outcome)

# choosing variables for the byes model
x_vars_byes <- c('is_first_innings', 'is_powerplay', 'aggression', 'required_run_rate', 'is_freehit',
                 'is_spin', 'outcome')

y_vars_byes <- c('n_byes')

x_byes <- byes_balls[, x_vars_byes]
y_byes <- byes_balls[, y_vars_byes]

# one-hot encoding - changing factors and logical columns, to numbers by introducing dummy variables
library(caret)
dmy_x <- dummyVars(" ~ .", data = x_byes)
x_new_byes <- data.frame(predict(dmy_x, newdata = x_byes))

dmy_y <- dummyVars(" ~ .", data = y_byes)
y_new_byes <- data.frame(predict(dmy_y, newdata = y_byes))

# getting training and test set
set.seed(999)
test_data <- sample(1:nrow(x_new_byes), as.integer(nrow(x_new_byes))/10) %>% sort
x_train_byes <- x_new_byes[-test_data, ]
x_test_byes <- x_new_byes[test_data, ]
y_train_byes <- y_new_byes[-test_data, ]
y_test_byes <- y_new_byes[test_data, ]


# building the model
# a simpler model worked well for me this time - no batch_norm or dropout applied here.
model_byes <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = 'relu', input_shape = 14) %>%
  layer_dense(units = 6, activation = 'softmax') %>%
  
  compile(
    optimizer = optimizer_adam(),
    loss = "categorical_crossentropy"
  )

# training the model
learn_byes <- model_byes %>%
  fit(x = as.matrix(x_train_byes), y = as.matrix(y_train_byes),
      epochs = 300,
      batch_size = 16,
      validation_split = 0.1,
      verbose = 2,
      callbacks = list(callback_early_stopping(patience = 50),
                       callback_reduce_lr_on_plateau()))

# saving the model
model_byes_saved <- serialize_model(model_byes)
plot(learn_byes)

summary(model_byes)

model_byes <- unserialize_model(model_byes_saved)

# testing predictions
preds_byes <- predict(model_byes, as.matrix(x_test_byes))
colnames(preds_byes) <- colnames(y_test_byes)

predictions_byes <- c()
actual_byes <- c()
for(i in 1:ncol(preds_byes)) {
  predictions_byes[i] <- sum(preds_byes[, i])
  actual_byes[i] <- sum(y_test_byes[, i])
}

predictions_byes
actual_byes

error_byes <- abs(predictions_byes - actual_byes) %>% sum()
error_byes

(predictions_byes*c(0, 1, 2, 3, 4, 5)) %>% sum()
(actual_byes*c(0, 1, 2, 3, 4, 5)) %>% sum()
