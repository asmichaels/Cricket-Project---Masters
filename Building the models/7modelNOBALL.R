# tried a similar approach for predicting the probability of a no-ball.
# the no-ball was too rare an event for the process to handle. 
# consequently the predictions were just way off what we expected.
# i go with a much simpler probabilistic approach

# this looks at just one variable: whether the bowler is a spin bowler.
# spin bowlers are much less likely to bowl a noball compared with fast bowlers.

x_vars_nb <- c('is_spin')
y_vars_nb <- c('is_noball')

# getting subset of balls data for where no-ball is eligible
no_ball_balls <- subset(balls, (outcome != 'wide' & outcome != 'bye' & outcome != 'legbye' &
                                  outcome != 'wicket'))

x_nb <- no_ball_balls[, x_vars_nb]
y_nb <- no_ball_balls[, y_vars_nb]

# getting training and test set. using a 90:10 split again
set.seed(555)
test_data <- sample(1:nrow(x_nb), as.integer(nrow(x_nb))/10) %>% sort
x_train_nb <- x_nb[-test_data, ]
x_test_nb <- x_nb[test_data, ]
y_train_nb <- y_nb[-test_data, ]
y_test_nb <- y_nb[test_data, ]

# if spin
p_nb_spin <- subset(no_ball_balls, (is_spin == 1))$is_noball %>% sum() / 
  subset(no_ball_balls, (is_spin == 1)) %>% nrow()

# if pace
p_nb_nospin <- subset(no_ball_balls, (is_spin == 0))$is_noball %>% sum() / 
  subset(no_ball_balls, (is_spin == 0)) %>% nrow()

# testing predictions
preds_nb_F <- ifelse(x_test_nb$is_spin == TRUE, (1 - p_nb_spin), (1- p_nb_nospin))
preds_nb_T <- ifelse(x_test_nb$is_spin == TRUE, p_nb_spin, p_nb_nospin)

# putting into a table
preds_nb <- tibble(preds_nb_F, preds_nb_T)

predictions_nb <- sum(preds_nb[, 2])
actual_nb <- sum(y_test_nb == T)

predictions_nb
actual_nb
