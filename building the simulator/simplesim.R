# step1: get info on match to predict
simple_set_up_match <- function() {
  # getting team names for the match
  team_a <<- 'A'
  team_b <<- 'B'
  
  # getting team lineups for the match
  team_a_lineup <<- c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10', 'A11')
  team_b_lineup <<- c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11')
}

# step2: toss to decide who bats first
simple_toss <- function() {
  toss <- runif(1)
  if (toss < 0.5) {
    f <<- team_a
    bat_first <<- team_a_lineup
    bat_second <<- team_b_lineup
  }
  else {
    f <<- team_b
    bat_first <<- team_b_lineup
    bat_second <<- team_a_lineup
  }
}

# step3: initialize innings1 and innings2
simple_innings1_init <- function() {
  # is first innings
  i1 <<- TRUE
  
  # first innings score
  first_innings_score <<- NA
  
  # bat orders
  bat_order <<- bat_first
  
  # opening batter/non-striker
  batter <<- bat_order[1]
  non_striker <<- bat_order[2]
  next_in <<- 3
  
  # game state variables
  current_runs <<- 0
  wickets_remaining <<- 10
  balls_remaining <<- 120
  over_number <<- 1
  ball_number <<- 1
  fh <<- FALSE
}

simple_innings2_init <- function() {
  # is first innings
  i1 <<- FALSE
  
  # first innings score
  first_innings_score <<- current_runs
  
  # bat order
  bat_order <<- bat_second
  
  # opening batter/non-striker
  batter <<- bat_order[1]
  non_striker <<- bat_order[2]
  next_in <<- 3
  
  # game state variables
  current_runs <<- 0
  wickets_remaining <<- 10
  balls_remaining <<- 120
  over_number <<- 1
  ball_number <<- 1
  fh <<- FALSE
}

simple_delivery <- function() {
  # reset some local variables to 0
  wicket <- 0
  dk <- 0
  nb <- 0
  check_byes <- 0
  wide <- 0
  
  is_powerplay <- ifelse(over_number <= 6, TRUE, FALSE)
  
  # collecting variables for outcome model 
  is_freehit <- fh
  
  # getting distribution
  # if it's a free-hit, the wicket outcome is impossible
  if (is_freehit == TRUE) {
    p[8] <- 0
    # sampling outcome
    outcome <- sample(outcomes, 1, prob = p)
    simple_get_p(balls)
  }
  else {
    outcome <- sample(outcomes, 1, prob = p)
  }
  
  if (outcome == 'wicket') {
    wicket <- TRUE
    
    # check stumping
    is_stumped <- ifelse(runif(1) < p_stumped, 1, 0)
    if (is_stumped == 1) {
      dk <- 'stumped'
      # check for wide
      wide <- ifelse(runif(1) < p_stumped_extra, 1, 0)
    }
    else {
      dk <- 'other'
    }
  }
  
  else if (outcome == '0' | outcome == '1' | outcome == '2' |
           outcome == '3' | outcome == '4' | outcome == '5' |
           outcome == '6') {
    
    # check for no-ball
    if (runif(1) < p_nb_spin) {
      nb <- TRUE
      
      # if no ball == TRUE #############
      if (nb == TRUE) {
        if (outcome == '0') {
          
          # get n_byes
          check_byes <- T
          n_byes <- sample(0:5, 1, prob = bye)
          
          # check if n_byes < 4
          if (n_byes < 4) {
            
            # check for runout
            is_runout <- ifelse(runif(1) < p_runout, 1, 0)
            wicket <- ifelse(is_runout == 1, TRUE, FALSE)
            
            if (is_runout == 1) {
              dk <- 'run out'
            }
          }
        }
        else { # no ball == TRUE & outcome != 0
          if (outcome != 'outcome.4' & outcome != 'outcome.5' & outcome != 'outcome.6') {
            
            # check for runout
            is_runout <- ifelse(runif(1) < p_runout, 1, 0)
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
        is_runout <- ifelse(runif(1) < p_runout, 1, 0)
        wicket <- ifelse(is_runout == 1, TRUE, FALSE)
        
        if (is_runout == 1) {
          dk <- 'run out'
        }
      }
    }
  }
  
  else if (outcome == 'wide') {
    
    # get n_byes
    check_byes <- T
    n_byes <- sample(0:5, 1, prob = bye)
    
    # check if n_byes < 4
    if (n_byes < 4) {
      
      # check for runout
      is_runout <- ifelse(runif(1) < p_runout, 1, 0)
      wicket <- ifelse(is_runout == 1, TRUE, FALSE)
      
      if (is_runout == 1) {
        dk <- 'run out'
      }
    }
  }
  
  else if (outcome == 'outcome.bye' | outcome == 'outcome.legbye') {
    
    # get n_byes
    check_byes <- T
    n_byes <- sample(1:5, 1, prob = bye[2:6])
    
    # check if n_byes < 4
    if (n_byes < 4) {
      
      # check for runout
      is_runout <- ifelse(runif(1) < p_runout, 1, 0)
      wicket <- ifelse(is_runout == 1, TRUE, FALSE)
      
      if (is_runout == 1) {
        dk <- 'run out'
      }
    }
  }
  
  # appending results to variables which track the events of each ball #########
  t_bat_runs <<- t_bat_runs %>% append(ifelse(as.numeric(outcome) %>% is.na() == T,
                                              0, as.numeric(outcome)))
  t_wnb_runs <<- t_wnb_runs %>% append(ifelse(nb == T | outcome == 'wide' | wide == T, 1, 0))
  t_bye_runs <<- t_bye_runs %>% append(ifelse(check_byes == T, n_byes, 0))
  t_total_runs <<- t_total_runs %>% append(tail(t_wnb_runs, 1) + tail(t_bye_runs, 1) + 
                                             tail(t_bat_runs, 1))
  t_is_noball <<- t_is_noball %>% append(ifelse(nb == T, 1, 0))
  t_is_wide <<- t_is_wide %>% append(ifelse(outcome == 'wide' | wide == 1, 1, 0))
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
    if (nb == TRUE | outcome == 'wide') {
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
  if (nb == TRUE | outcome == 'wide' | wide == T) {
    if (i1 == TRUE) {
      if (wickets_remaining != 0) {
        simple_delivery()
      }
    }
    else {
      if (current_runs <= first_innings_score & wickets_remaining != 0) {
        simple_delivery()
      }
    }
  }
}

simple_over <- function() {
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
    simple_delivery()
  }
  # switch batters
  swap_ends <- batter
  batter <<- non_striker
  non_striker <<- swap_ends
  
  # add to counters
  over_number <<- over_number + 1
  ball_number <<- 1
}

simple_innings <- function() {
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
    simple_over()
  }
}

simple_match <- function() {
  simple_set_up_match()
  simple_toss()
  simple_innings1_init()
  simple_innings()
  simple_innings2_init()
  simple_innings()
}

simple_model <- function(n_sim) {
  # initializing global variables which track what happens on each ball of a simulation.
  simple_match_variables_init()
  simple_get_p(balls)
  game <<- 0
  for (i in 1:n_sim) {
    game <<- game + 1
    simple_match()
    print(paste('mod:', i))
  }
}

simple_pred_all_games <- function(n_sim) {
  start.time <- Sys.time()
  
  simple_model(n_sim)
  
  # save match info to dataframe
  simple_balls_df <<- tibble(t_game, t_inning, t_over, t_ball, t_is_powerplay, t_balls_remaining,
                             t_wickets_remaining, t_is_freehit, t_target_score, t_current_runs,
                             t_current_wickets, t_runs_required, t_batter, t_bat_runs, t_wnb_runs, 
                             t_bye_runs, t_total_runs, t_is_noball, t_is_wide, t_is_wicket, 
                             t_dismissal_kind, t_player_dismissed)
  
  simple_sim_variables()
  
  # go through each of the simulated matches and gather stats for each
  for(sim in 1:n_sim) {
    game <- subset(simple_balls_df, (t_game == sim))
    i1 <- subset(game, (t_inning == 1))
    i2 <- subset(game, (t_inning == 2))
    s_bat1 <<- s_bat1 %>% append(ifelse(f == team_a, team_a, team_b))
    s_bat2 <<- s_bat2 %>% append(ifelse(f == team_a, team_b, team_a))
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
    batters <- game$t_batter %>% unique()
    bat_r <- c()
    for (bat in batters) {
      bat_df <- subset(game, (t_batter == bat))
      bat_r <- bat_r %>% append(sum(bat_df$t_bat_runs))
    }
    index <- which(max(bat_r) == bat_r)
    s_batter1 <<- s_batter1 %>% append(ifelse(length(batters[index]) == 1,
                                              batters[index], 'Tie'))
    s_batter1_score <<- s_batter1_score %>% append(max(bat_r))
  }
  
  # save
  simple_matches_df <<- tibble(s_bat1, s_bat2, s_bat1_runs, s_bat1_wickets, s_bat2_runs, 
                               s_bat2_wickets, s_winner, s_batter1, s_batter1_score, 
                               s_total_6s, s_over1_runs)
  
  p_team_a <- sum(simple_matches_df$s_winner == team_a) / (n_sim - sum(simple_matches_df$s_winner == 'Tie'))
  p_team_b <- sum(simple_matches_df$s_winner == team_b) / (n_sim - sum(simple_matches_df$s_winner == 'Tie'))
  median_i1 <- median(simple_matches_df$s_bat1_runs)
  mean_i1 <- mean(simple_matches_df$s_bat1_runs)
  median_bat1 <- median(simple_matches_df$s_batter1_score)
  mean_bat1 <- mean(simple_matches_df$s_batter1_score)
  batter1 <- names(sort(table(simple_matches_df$s_batter1), decreasing = TRUE)[1])
  p_batter1 <- sum(batter1 == simple_matches_df$s_batter1) / (n_sim - sum(batter1 == 'Tie'))
  mean_6s <- mean(simple_matches_df$s_total_6s)
  median_6s <- median(simple_matches_df$s_total_6s)
  mean_o1 <- mean(simple_matches_df$s_over1_runs)
  median_o1 <- median(simple_matches_df$s_over1_runs)
  
  simple_model_predictions_df <<- tibble(team_a, team_b, p_team_a, p_team_b, mean_i1, median_i1, 
                                         mean_bat1, median_bat1, batter1, p_batter1, mean_6s, 
                                         median_6s, mean_o1, median_o1)
  
  print(paste('games: ', sim))
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}

simple_pred_all_games(1000)

view(simple_balls_df)
view(simple_matches_df)
simple_model_predictions_df

write_csv(simple_balls_df, 'simple_balls.csv')
write_csv(simple_matches_df, 'simple_matches.csv')
write_csv(simple_model_predictions_df, 'simple_model.csv')
