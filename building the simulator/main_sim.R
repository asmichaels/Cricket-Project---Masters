install.packages('keras')
install.packages('caret')

library(keras)
library(tensorflow)
install_tensorflow()
library(caret)
library(tidyverse)
model_1 <- unserialize_model(model_1_saved)
model_byes <- unserialize_model(model_byes_saved)

byes_factor <- as.factor(c('outcome.0', 'outcome.bye', 'outcome.legbye', 'outcome.wide'))

# step1: get info on match to predict
set_up_match <- function(id) {
  # get venue for the match
  # need to keep the factor levels from balls data hence the convoluted command here
  venue <<- balls$venue[which(match_to_predict(id)$venue[1] == balls$venue)][1]
  
  # getting team names for the match
  team_a <<- match_to_predict(id)$team[1]
  team_b <<- match_to_predict(id)$team[12]
  
  # getting team lineups for the match
  team_a_lineup <<- match_to_predict(id)[1:11, 4:6]
  team_b_lineup <<- match_to_predict(id)[12:22, 4:6]
  
  # getting the order for bowlers in each team
  bowlers_a <- c()
  for (i in 1:nrow(team_a_lineup)) {
    bowlers_a <- bowlers_a %>% append(rep(team_a_lineup$player[i], team_a_lineup$n_overs[i]))
  }
  
  bowlers_b <- c()
  for (i in 1:nrow(team_b_lineup)) {
    bowlers_b <- bowlers_b %>% append(rep(team_b_lineup$player[i], team_b_lineup$n_overs[i]))
  }
  
  # using n_overs as a proxy for bowler skill.
  # more skillful bowlers tend to bowl the opening and final overs
  bowlers_4a <- team_a_lineup$player[which(team_a_lineup$n_overs == 4)]
  bowlers_4b <- team_b_lineup$player[which(team_b_lineup$n_overs == 4)]
  
  # final order for bowling 
  bowl_order_a <<- order(bowlers_a, bowlers_4a)
  bowl_order_b <<- order(bowlers_b, bowlers_4b)
}

# step2: toss to decide who bats first
toss <- function() {
  toss <- runif(1)
  if (toss < 0.5) {
    f <<- team_a
    bat_first <<- team_a_lineup$player
    bowl_first <<- bowl_order_b
    bat_second <<- team_b_lineup$player
    bowl_second <<- bowl_order_a
  }
  else {
    f <<- team_b
    bat_first <<- team_b_lineup$player
    bowl_first <<- bowl_order_a
    bat_second <<- team_a_lineup$player
    bowl_second <<- bowl_order_b
  }
}

# step3: initialize innings1 and innings2
innings1_init <- function() {
  # is first innings
  i1 <<- TRUE
  
  # first innings score
  first_innings_score <<- NA
  
  # bat/bowl orders
  bat_order <<- bat_first
  bowl_order <<- bowl_first
  
  # opening batter/non-striker/bowler
  batter <<- bat_order[1]
  non_striker <<- bat_order[2]
  next_in <<- 3
  bowler <<- bowl_order[1]
  next_bowler <<- 2
  
  # game state variables
  current_runs <<- 0
  wickets_remaining <<- 10
  balls_remaining <<- 120
  over_number <<- 1
  ball_number <<- 1
  fh <<- FALSE
}

innings2_init <- function() {
  # is first innings
  i1 <<- FALSE
  
  # first innings score
  first_innings_score <<- current_runs
  
  # bat/bowl orders
  bat_order <<- bat_second
  bowl_order <<- bowl_second
  
  # opening batter/non-striker/bowler
  batter <<- bat_order[1]
  non_striker <<- bat_order[2]
  next_in <<- 3
  bowler <<- bowl_order[1]
  next_bowler <<- 2
  
  # game state variables
  current_runs <<- 0
  wickets_remaining <<- 10
  balls_remaining <<- 120
  over_number <<- 1
  ball_number <<- 1
  fh <<- FALSE
}

delivery <- function() {
  # checking to see if wickets_remaining == 0 or if current runs > target score
  if (i1 == TRUE) {
    if (wickets_remaining == 0) {
      return('All out. First innings over.')
    }
  }
  else {
    if (current_runs > (first_innings_score)) {
      return('Game over. Team batting second wins.')
    }
    if (wickets_remaining == 0) {
      return('Game over. Team batting second all out. Team batting first wins.')
    }
  }
  
  # reset some variables to 0
  wicket <- 0
  dk <- 0
  nb <- 0
  check_byes <- 0
  wide <- 0
  
  # collecting variables for outcome model ########
  venue <- venue
  is_first_innings <- ifelse(i1 == TRUE, TRUE, FALSE)
  is_powerplay <- ifelse(over_number <= 6, TRUE, FALSE)
  home_advantage_bat <- FALSE
  is_freehit <- fh
  aggression <- ifelse(wickets_remaining / balls_remaining > 1, 1, wickets_remaining / balls_remaining)
  if (i1 == FALSE) {
    if ((first_innings_score + 1 - current_runs) / balls_remaining > 6) {
      required_run_rate <- 6
    }
    else {
      required_run_rate <- (first_innings_score + 1 - current_runs) / balls_remaining
    }
  }
  else {
    required_run_rate <- 0
  }
  batter_position <- which(batter == bat_order)
  missing_bat <- ifelse(batter %in% batters_data$batters, FALSE, TRUE)
  if (missing_bat == FALSE) {
    bat_0 <- batters_data$dot_rate[which(batter == batters_data$batters)]
    bat_1 <- batters_data$one_rate[which(batter == batters_data$batters)]
    bat_2 <- batters_data$two_rate[which(batter == batters_data$batters)]
    bat_4 <- batters_data$four_rate[which(batter == batters_data$batters)]
    bat_6 <- batters_data$six_rate[which(batter == batters_data$batters)]
    bat_bye <- batters_data$byes_rate[which(batter == batters_data$batters)]
    bat_legbye <- batters_data$leg_byes_rate[which(batter == batters_data$batters)]
    bat_out <- batters_data$out_rate[which(batter == batters_data$batters)]
  }
  else {
    bat_0 <- 0
    bat_1 <- 0
    bat_2 <- 0
    bat_4 <- 0
    bat_6 <- 0
    bat_bye <- 0
    bat_legbye <- 0
    bat_out <- 0
  }
  average_overs <- sum(bowler == bowl_order)
  is_spin <- ifelse(players_df$is_spin[which(bowler == players_df$players)] == 0, FALSE, TRUE)
  missing_bowl <- ifelse(bowler %in% bowlers_data$bowlers, FALSE, TRUE)
  if (missing_bowl == FALSE) {
    bowl_0 <- bowlers_data$dots_bowl[which(bowler == bowlers_data$bowlers)]
    bowl_1 <- bowlers_data$ones_bowl[which(bowler == bowlers_data$bowlers)]
    bowl_2 <- bowlers_data$twos_bowl[which(bowler == bowlers_data$bowlers)]
    bowl_4 <- bowlers_data$fours_bowl[which(bowler == bowlers_data$bowlers)]
    bowl_6 <- bowlers_data$sixes_bowl[which(bowler == bowlers_data$bowlers)]
    bowl_bye <- bowlers_data$byes_bowl[which(bowler == bowlers_data$bowlers)]
    bowl_wicket <- bowlers_data$wicket_rate[which(bowler == bowlers_data$bowlers)]
    bowl_wide <- bowlers_data$wides[which(bowler == bowlers_data$bowlers)]
  }
  else {
    bowl_0 <- 0
    bowl_1 <- 0
    bowl_2 <- 0
    bowl_4 <- 0
    bowl_6 <- 0
    bowl_bye <- 0
    bowl_wicket <- 0
    bowl_wide <- 0
  }
  #########
  
  # getting distribution #########
  # inputting variables to tibble
  input <<- tibble(venue, is_first_innings, is_powerplay, home_advantage_bat, is_freehit, aggression,
                   required_run_rate, batter_position, missing_bat, bat_0, bat_1, bat_2, bat_4, bat_6,
                   bat_bye, bat_legbye, bat_out, average_overs, is_spin, missing_bowl, bowl_0, bowl_1,
                   bowl_2, bowl_4, bowl_6, bowl_bye, bowl_wicket, bowl_wide)
  
  # one-hot encoding
  dmy_input <- dummyVars(" ~ .", data = input)
  input_ohe <<- data.frame(predict(dmy_input, newdata = input))
  
  # getting distribution
  output <- predict(model_1, as.matrix(input_ohe))
  ################
  
  outcomes <- colnames(y_test1)
  
  # if it's a free-hit, the wicket outcome is impossible
  if (fh == FALSE) {
    outcome <- sample(outcomes, 1, prob = output)
  }
  else {
    outcome <- sample(outcomes[c(1:9, 11)], 1, prob = output[c(1:9, 11)])
  }
  
  if (outcome == 'outcome.wicket') {
    wicket <- TRUE
    # check stumping
    if (is_spin == T) {
      is_stumped <- ifelse(runif(1) < p_st_spin, 1, 0)
      
      if (is_stumped == 1) {
        dk <- 'stumped'
        # check for wide
        wide <- ifelse(runif(1) < p_wide, 1, 0)
      }
      else {
        dk <- 'other'
      }
    }
    else {
      is_stumped <- ifelse(runif(1) < p_st_nospin, 1, 0)
      
      if (is_stumped == 1) {
        dk <- 'stumped'
        # check for wide
        wide <- ifelse(runif(1) < p_wide, 1, 0)
      }
      else {
        dk <- 'other'
      }
    }
  }
  
  else if (outcome == 'outcome.0' | outcome == 'outcome.1' | outcome == 'outcome.2' |
           outcome == 'outcome.3' | outcome == 'outcome.4' | outcome == 'outcome.5' |
           outcome == 'outcome.6') {
    # check for no-ball
    if (is_spin == T) {
      if (runif(1) < p_nb_spin) {
        nb <- TRUE
      }
    }
    else {
      if (runif(1) < p_nb_nospin) {
        nb <- TRUE
      }
    }
    
    # if no ball == TRUE #############
    if (nb == TRUE) {
      if (outcome == 'outcome.0') {
        # check for byes
        # setup #######
        check_byes <- TRUE
        outcome_b <- byes_factor[which(outcome == byes_factor)]
        input_b <- tibble(is_first_innings, is_powerplay, aggression, required_run_rate,
                          is_freehit, is_spin, outcome_b)
        # ohe
        dmy_input_b <- dummyVars(" ~ .", data = input_b)
        input_ohe_b <- data.frame(predict(dmy_input_b, newdata = input_b))
        #####
        # getting distribution
        output_b <- predict(model_byes, as.matrix(input_ohe_b))
        
        n_byes <- sample(colnames(y_test_byes), 1, prob = output_b)
        
        # check if n_byes < 4
        if (n_byes != 'n_byes.4' & n_byes != 'n_byes.5') {
          # check for runout
          input_ro <- tibble(aggression, required_run_rate)
          if (is_first_innings == T) {
            output_ro <- predict(ro_glm1, newdata = input_ro, type = 'response')
            
            # is_runout
            is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
            wicket <- ifelse(is_runout == 1, TRUE, FALSE)
            if (is_runout == 1) {
              dk <- 'run out'
            }
          }
          else {
            output_ro <- predict(ro_glm2, newdata = input_ro, type = 'response')
            
            # is_runout
            is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
            wicket <- ifelse(is_runout == 1, TRUE, FALSE)
            if (is_runout == 1) {
              dk <- 'run out'
            }
          }
        }
      }
      else { # no ball == TRUE & outcome != 0
        if (outcome != 'outcome.4' & outcome != 'outcome.5' & outcome != 'outcome.6') {
          
          # check for runout
          input_ro <- tibble(aggression, required_run_rate)
          if (is_first_innings == T) {
            output_ro <- predict(ro_glm1, newdata = input_ro, type = 'response')
            
            # is_runout
            is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
            wicket <- ifelse(is_runout == 1, TRUE, FALSE)
            if (is_runout == 1) {
              dk <- 'run out'
            }
          }
          else {
            output_ro <- predict(ro_glm2, newdata = input_ro, type = 'response')
            
            # is_runout
            is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
            wicket <- ifelse(is_runout == 1, TRUE, FALSE)
            if (is_runout == 1) {
              dk <- 'run out'
            }
          }
        }
      }
    }
    # else if no ball is NOT TRUE #############
    else {
      if (outcome != 'outcome.4' & outcome != 'outcome.5' & outcome != 'outcome.6') {
        
        # check for runout
        input_ro <- tibble(aggression, required_run_rate)
        if (is_first_innings == T) {
          output_ro <- predict(ro_glm1, newdata = input_ro, type = 'response')
          
          # is_runout
          is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
          wicket <- ifelse(is_runout == 1, TRUE, FALSE)
          if (is_runout == 1) {
            dk <- 'run out'
          }
        }
        else {
          output_ro <- predict(ro_glm2, newdata = input_ro, type = 'response')
          
          # is_runout
          is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
          wicket <- ifelse(is_runout == 1, TRUE, FALSE)
          if (is_runout == 1) {
            dk <- 'run out'
          }
        }
      }
    }
  }
  
  else if (outcome == 'outcome.wide') {
    # check for byes
    # setup #######
    check_byes <- TRUE
    outcome_b <- byes_factor[which(outcome == byes_factor)]
    input_b <- tibble(is_first_innings, is_powerplay, aggression, required_run_rate,
                      is_freehit, is_spin, outcome_b)
    # ohe
    dmy_input_b <- dummyVars(" ~ .", data = input_b)
    input_ohe_b <- data.frame(predict(dmy_input_b, newdata = input_b))
    #####
    # getting distribution
    output_b <- predict(model_byes, as.matrix(input_ohe_b))
    
    n_byes <- sample(colnames(y_test_byes), 1, prob = output_b)
    
    # check if n_byes < 4
    if (n_byes != 'n_byes.4' & n_byes != 'n_byes.5') {
      
      # check for runout
      input_ro <- tibble(aggression, required_run_rate)
      if (is_first_innings == T) {
        output_ro <- predict(ro_glm1, newdata = input_ro, type = 'response')
        
        # is_runout
        is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
        wicket <- ifelse(is_runout == 1, TRUE, FALSE)
        if (is_runout == 1) {
          dk <- 'run out'
        }
      }
      else {
        output_ro <- predict(ro_glm2, newdata = input_ro, type = 'response')
        
        # is_runout
        is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
        wicket <- ifelse(is_runout == 1, TRUE, FALSE)
        if (is_runout == 1) {
          dk <- 'run out'
        }
      }
    }
  }
  
  else if (outcome == 'outcome.bye' | outcome == 'outcome.legbye') {
    # check for byes
    # setup #######
    check_byes <- T
    outcome_b <- byes_factor[which(outcome == byes_factor)]
    input_b <- tibble(is_first_innings, is_powerplay, aggression, required_run_rate,
                      is_freehit, is_spin, outcome_b)
    # ohe
    dmy_input_b <- dummyVars(" ~ .", data = input_b)
    input_ohe_b <- data.frame(predict(dmy_input_b, newdata = input_b))
    #####
    # getting distribution
    output_b <- predict(model_byes, as.matrix(input_ohe_b))
    
    # since byes = 0 is impossible here, we remove this outcome from possible samples
    n_byes <- sample(colnames(y_test_byes)[2:6], 1, prob = output_b[2:6])
    
    # check if n_byes < 4
    if (n_byes != 'n_byes.4' & n_byes != 'n_byes.5') {
      
      # check for runout
      input_ro <- tibble(aggression, required_run_rate)
      if (is_first_innings == T) {
        output_ro <- predict(ro_glm1, newdata = input_ro, type = 'response')
        
        # is_runout
        is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
        wicket <- ifelse(is_runout == 1, TRUE, FALSE)
        if (is_runout == 1) {
          dk <- 'run out'
        }
      }
      else {
        output_ro <- predict(ro_glm2, newdata = input_ro, type = 'response')
        
        # is_runout
        is_runout <- sample(c(1, 0), 1, prob = c(output_ro, (1 - output_ro)))
        wicket <- ifelse(is_runout == 1, TRUE, FALSE)
        if (is_runout == 1) {
          dk <- 'run out'
        }
      }
    }
  }
  
  # appending results to variables which track the events of each ball #########
  t_bat_runs <<- t_bat_runs %>% append(ifelse(as.numeric(sub(".*\\.", "", outcome)) %>% is.na() == T,
                                              0, as.numeric(sub(".*\\.", "", outcome))))
  t_wnb_runs <<- t_wnb_runs %>% append(ifelse(nb == T | outcome == 'outcome.wide' | wide == 1, 1, 0))
  t_bye_runs <<- t_bye_runs %>% append(ifelse(check_byes == T, as.numeric(sub(".*\\.", "", n_byes)), 0))
  t_total_runs <<- t_total_runs %>% append(tail(t_wnb_runs, 1) + tail(t_bye_runs, 1) + 
                                             tail(t_bat_runs, 1))
  t_is_noball <<- t_is_noball %>% append(ifelse(nb == T, 1, 0))
  t_is_wide <<- t_is_wide %>% append(ifelse(outcome == 'outcome.wide' | wide == 1, 1, 0))
  t_is_wicket <<- t_is_wicket %>% append(ifelse(wicket == T, 1, 0))
  t_dismissal_kind <<- t_dismissal_kind %>% append(ifelse(wicket == T, dk, NA))
  if (wicket == T) {
    if (dk == 'other' | dk == 'stumped') {
      t_player_dismissed <<- t_player_dismissed %>% append(batter)
    }
    else {
      t_player_dismissed <<- t_player_dismissed %>% append(sample(c(batter, non_striker), 1))
    }
  }
  else {
    t_player_dismissed <<- t_player_dismissed %>% append(NA)
  }
  ###############
  
  # appending to variables describing the state of the game at the moment of this 
  # ball ##########
  t_id <<- t_id %>% append(match_id)
  t_game <<- t_game %>% append(game)
  t_inning <<- t_inning %>% append(ifelse(i1 == T, 1, 2))
  t_over <<- t_over %>% append(over_number)
  t_ball <<- t_ball %>% append(ball_number)
  t_is_powerplay <<- t_is_powerplay %>% append(is_powerplay)
  t_balls_remaining <<- t_balls_remaining %>% append(balls_remaining)
  t_wickets_remaining <<- t_wickets_remaining %>% append(wickets_remaining)
  t_is_freehit <<- t_is_freehit %>% append(is_freehit)
  t_current_runs <<- t_current_runs %>% append(current_runs)
  t_current_wickets <<- t_current_wickets %>% append(abs(tail(t_wickets_remaining, 1) - 10))
  t_batter <<- t_batter %>% append(batter)
  t_batter_position <<- t_batter_position %>% append(batter_position)
  t_bowler <<- t_bowler %>% append(bowler)
  t_is_spin <<- t_is_spin %>% append(is_spin)
  
  if (i1 == TRUE) {
    t_target_score <<- t_target_score %>% append(NA)
    t_runs_required <<- t_runs_required %>% append(NA)
  }
  else {
    t_target_score <<- t_target_score %>% append(first_innings_score + 1)
    t_runs_required <<- t_runs_required %>% append(tail(t_target_score, 1) - current_runs)
  }
  ###############
  
  # changing game state variables for subsequent ball ###############
  
  # ball counters
  if (tail(t_wnb_runs, 1) == 1) {
    ball_number <<- ball_number
    balls_remaining <<- balls_remaining
  }
  else {
    ball_number <<- ball_number + 1
    balls_remaining <<- balls_remaining - 1
  }
  # wickets
  if (wicket == TRUE) {
    wickets_remaining <<- wickets_remaining - 1
    if (dk == 'other' | dk == 'stumped') {
      batter <<- bat_order[next_in]
      next_in <<- next_in + 1
    }
    else if (dk == 'run out') {
      if (tail(t_player_dismissed, 1) == batter) {
        batter <<- bat_order[next_in]
        next_in <<- next_in + 1
      }
      else {
        non_striker <<- bat_order[next_in]
        next_in <<- next_in + 1
      }
    }
  }
  else {
    wickets_remaining <<- wickets_remaining
  }
  # free hits
  if (fh == TRUE) {
    if (nb == TRUE | outcome == 'outcome.wide') {
      fh <<- TRUE
    }
    else {
      fh <<- FALSE
    }
  }
  else {
    if (nb == TRUE) {
      fh <<- TRUE
    }
    else {
      fh <<- FALSE
    }
  }
  
  # if odd runs, swap batter and non-striker, UNLESS there was a run-out
  if (tail(t_bat_runs, 1) == 1 | tail(t_bat_runs, 1) == 3 | tail(t_bat_runs, 1) == 5 |
      tail(t_bye_runs, 1) == 1 | tail(t_bye_runs, 1) == 3 | tail(t_bye_runs, 1) == 5) {
    if (dk == 'run out') {
      batter <<- batter
      non_striker <<- non_striker
    }
    else {
      swap_ends <- batter
      batter <<- non_striker
      non_striker <<- swap_ends
    }
  }
  else {
    if (dk == 'run out') {
      swap_ends <- batter
      batter <<- non_striker
      non_striker <<- swap_ends
    }
    else {
      batter <<- batter
      non_striker <<- non_striker
    }
  }
  
  # current runs counter
  current_runs <<- current_runs + tail(t_total_runs, 1)
  
  # if no-ball or wide, call delivery() from here
  if (nb == TRUE | outcome == 'outcome.wide' | wide == 1) {
    if (i1 == TRUE) {
      if (wickets_remaining != 0) {
        delivery()
      }
    }
    else {
      if (current_runs <= first_innings_score & wickets_remaining != 0) {
        delivery()
      }
    }
  }
}

over <- function() {
  for (i in 1:6) {
    # checking to see if wickets_remaining < 10 or if current runs > target score
    if (i1 == TRUE) {
      if (wickets_remaining == 0) {
        break
      }
    }
    else {
      if (current_runs > first_innings_score) {
        break
      }
      if (wickets_remaining == 0) {
        break
      }
    }
    delivery()
  }
  # switch batters
  swap_ends <- batter
  batter <<- non_striker
  non_striker <<- swap_ends
  
  # add to counters
  over_number <<- over_number + 1
  bowler <<- bowl_order[next_bowler]
  next_bowler <<- next_bowler + 1
  ball_number <<- 1
}

innings <- function() {
  for (i in 1:20) {
    # checking to see if wickets_remaining < 10 or if current runs > target score
    if (i1 == TRUE) {
      if (wickets_remaining == 0) {
        break
      }
    }
    else {
      if (current_runs > first_innings_score) {
        break
      }
      if (wickets_remaining == 0) {
        break
      }
    }
    over()
  }
}

match <- function(id) {
  set_up_match(id)
  toss()
  innings1_init()
  innings()
  innings2_init()
  innings()
}

model <- function(id, n_sim) {
  # initializing global variables which track what happens on each ball of a simulation.
  match_variables_init()
  game <<- 0
  match_id <<- id
  for (i in 1:n_sim) {
    game <<- game + 1
    match(id)
    print(paste('mod:', i))
  }
}

pred_all_games <- function(n_sim) {
  # gets game ids for each match to predict
  all_games <- unique(test_lineups$id)[1:3]
  
  # define empty lists to save the results of the simulations to
  ball_by_ball <<- list()
  match_by_match <<- list()
  mod_by_mod <<- list()
  
  # simulate each match
  for (i in 1:length(all_games)) {
    id <- all_games[i]
    model(id, n_sim)
    
    # save match info to dataframe
    match_df <- tibble(t_id, t_game, t_inning, t_over, t_ball, t_is_powerplay, t_balls_remaining,
                       t_wickets_remaining, t_is_freehit, t_target_score, t_current_runs,
                       t_current_wickets, t_runs_required, t_batter, t_batter_position,
                       t_bowler, t_is_spin, t_bat_runs, t_wnb_runs, t_bye_runs, t_total_runs,
                       t_is_noball, t_is_wide, t_is_wicket, t_dismissal_kind, t_player_dismissed)
    
    ball_by_ball[[i]] <<- match_df
    
    sim_variables()
    
    for(sim in 1:n_sim) {
      game <- subset(match_df, (t_game == sim))
      i1 <- subset(game, (t_inning == 1))
      i2 <- subset(game, (t_inning == 2))
      s_bat1 <<- s_bat1 %>% append(test_lineups$team[which(i1$t_batter[1] == test_lineups$player)][1])
      s_bat2 <<- s_bat2 %>% append(test_lineups$team[which(i2$t_batter[1] == test_lineups$player)][1])
      s_bat1_runs <<- s_bat1_runs %>% append(tail(i1$t_current_runs, 1) + tail(i1$t_total_runs, 1))
      s_bat1_wickets <<- s_bat1_wickets %>% append(tail(i1$t_current_wickets, 1) + tail(i1$t_is_wicket, 1))
      s_bat2_runs <<- s_bat2_runs %>% append(tail(i2$t_current_runs, 1) + tail(i2$t_total_runs, 1))
      s_bat2_wickets <<- s_bat2_wickets %>% append(tail(i2$t_current_wickets, 1) + tail(i2$t_is_wicket, 1))
      if (tail(s_bat1_runs, 1) == tail(s_bat2_runs, 1)) {
        s_winner <<- s_winner %>% append('Tie')
      }
      else {
        s_winner <<- s_winner %>% append(ifelse(tail(s_bat1_runs, 1) > tail(s_bat2_runs, 1),
                                                tail(s_bat1, 1), tail(s_bat2, 1)))
      }
      s_total_6s <<- s_total_6s %>% append(sum(game$t_bat_runs == 6))
      s_over1_runs <<- s_over1_runs %>% append(sum(subset(i1, (t_over == 1))$t_total_runs))
      #s_highest_batter_score <<- c()
      batters <- game$t_batter %>% unique()
      bowlers <- game$t_bowler %>% unique()
      bat_r <- c()
      bowl_w <- c()
      bowl_r <- c()
      for (bat in batters) {
        bat_df <- subset(game, (t_batter == bat))
        bat_r <- bat_r %>% append(sum(bat_df$t_bat_runs))
      }
      index <- which(max(bat_r) == bat_r)
      s_batter1 <<- s_batter1 %>% append(ifelse(length(batters[index]) == 1,
                                                batters[index], 'Tie'))
      s_batter1_score <<- s_batter1_score %>% append(max(bat_r))
      
      for (bowl in bowlers) {
        bowl_df <- subset(game, (t_bowler == bowl))
        bowl_w <- bowl_w %>% append(sum(bowl_df$t_is_wicket == 1 & 
                                          bowl_df$t_dismissal_kind != 'run out'))
      }
      index <- which(max(bowl_w) == bowl_w)
      if (length(index) > 1) {
        bowlers <- bowlers[index]
        for (bowl in bowlers) {
          bowl_df <- subset(game, (t_bowler == bowl))
          bowl_r <- bowl_r %>% append(sum(bowl_df$t_total_runs))
        }
        index2 <- which(min(bowl_r) == bowl_r)
        s_bowler1 <<- s_bowler1 %>% append(ifelse(length(bowlers[index2]) == 1,
                                                  bowlers[index2], 'Tie'))
      }
      else {
        s_bowler1 <<- s_bowler1 %>% append(bowlers[index])
      }
      s_bowler1_wickets <<- s_bowler1_wickets %>% append(max(bowl_w))
      s_bowler1_runs <<- s_bowler1_runs %>% append(min(bowl_r))
    }
    sim_matches_df <- tibble(s_bat1, s_bat2, s_bat1_runs, s_bat1_wickets, s_bat2_runs, 
                             s_bat2_wickets, s_winner, s_batter1, s_batter1_score, s_bowler1, 
                             s_bowler1_wickets, s_bowler1_runs, s_total_6s, s_over1_runs)
    
    match_by_match[[i]] <<- sim_matches_df
    
    p_team_a <- sum(sim_matches_df$s_winner == team_a) / n_sim
    p_team_b <- sum(sim_matches_df$s_winner == team_b) / n_sim
    median_i1 <- median(sim_matches_df$s_bat1_runs)
    mean_i1 <- mean(sim_matches_df$s_bat1_runs)
    median_bat1 <- median(sim_matches_df$s_batter1_score)
    mean_bat1 <- mean(sim_matches_df$s_batter1_score)
    batter1 <- names(sort(table(sim_matches_df$s_batter1), decreasing = TRUE)[1])
    p_batter1 <- sum(batter1 == sim_matches_df$s_batter1) / (n_sim - sum(batter1 == 'Tie'))
    bowler1 <- names(sort(table(sim_matches_df$s_bowler1), decreasing = TRUE)[1])
    p_bowler1 <- sum(bowler1 == sim_matches_df$s_bowler1) / (n_sim - sum(bowler1 == 'Tie'))
    mean_6s <- mean(sim_matches_df$s_total_6s)
    median_6s <- median(sim_matches_df$s_total_6s)
    mean_o1 <- mean(sim_matches_df$s_over1_runs)
    median_o1 <- median(sim_matches_df$s_over1_runs)
    
    model_predictions_df <- tibble(team_a, team_b, p_team_a, p_team_b, mean_i1, median_i1, 
                                   mean_bat1, median_bat1, batter1, p_batter1, bowler1, 
                                   p_bowler1, mean_6s, median_6s, mean_o1, median_o1)
    
    mod_by_mod[[i]] <<- model_predictions_df
    print(paste('games: ', i))
  }
}

pred_all_games(5)

balls_results_df <- ball_by_ball[[1]]
matches_results_df <- match_by_match[[1]]
models_results_df <- mod_by_mod[[1]]

for (i in 2:3) {
  balls_results_df <- rbind(balls_results_df, ball_by_ball[[i]])
  matches_results_df <- rbind(matches_results_df, match_by_match[[i]])
  models_results_df <- rbind(models_results_df, mod_by_mod[[i]])
}
