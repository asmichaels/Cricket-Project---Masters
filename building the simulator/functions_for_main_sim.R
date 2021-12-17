# SETUP / TEAMS / BOWLER ORDER ETC. ##########

# step1: get info on match to predict
match_to_predict <- function(id) {
  test_lineups$venue <- as.factor(test_lineups$venue)
  return(test_lineups[which(test_lineups$id == id), ])
}

# this function checks the randomised bowler order for a suitably realistic order
# it requires that a minimum of 6 of the first 4 and final 4 overs are bowled by
# the most skillful bowlers - as they would tend to be in real-life match
# it also requires that no two consecutive overs be bowled by the same bowler.
check <- function(bowlers, bowlers_4) {
  if (sum(bowlers[c(1, 2, 3, 4, 17, 18, 19, 20)] %in% bowlers_4) < 6) {
    return(FALSE)
  }
  else {
    errors <- 0
    for (i in 1:19) {
      if (bowlers[i] == bowlers[(i+1)]) {
        errors <- errors + 1
      }
    }
    if (errors > 0) {
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  }
}

# order function checks randomised bowler lists until it finds a valid solution according to 
# the check function above
order <- function(bowlers, bowlers_4) {
  ordered <- sample(bowlers)
  while (check(ordered, bowlers_4) == F) {
    ordered <- sample(bowlers)
    check(ordered, bowlers_4)
  }
  return(ordered)
  break
}

# a function to initialise global variables which track what happens on each ball of a 
# simulation.
match_variables_init <- function() {
  t_id <<- c()
  t_game <<- c()
  t_inning <<- c()
  t_over <<- c()
  t_ball <<- c()
  t_is_powerplay <<- c()
  t_balls_remaining <<- c()
  t_wickets_remaining <<- c()
  t_is_freehit <<- c()
  t_target_score <<- c()
  t_current_runs <<- c()
  t_current_wickets <<- c()
  t_runs_required <<- c()
  t_batter <<- c()
  t_batter_position <<- c()
  t_bowler <<- c()
  t_is_spin <<- c()
  t_bat_runs <<- c()
  t_wnb_runs <<- c()
  t_bye_runs <<- c()
  t_total_runs <<- c()
  t_is_noball <<- c()
  t_is_wide <<- c()
  t_is_wicket <<- c()
  t_dismissal_kind <<- c()
  t_player_dismissed <<- c()
}

# function to initialise global variables which track what happens for each match simulated
sim_variables <- function() {
  s_bat1 <<- c()
  s_bat2 <<- c()
  s_bat1_runs <<- c()
  s_bat1_wickets <<- c()
  s_bat2_runs <<- c()
  s_bat2_wickets <<- c()
  s_winner <<- c()
  s_batter1 <<- c()
  s_batter1_score <<- c()
  s_bowler1 <<- c()
  s_bowler1_wickets <<- c()
  s_bowler1_runs <<- c()
  s_total_6s <<- c()
  s_over1_runs <<- c()
}
