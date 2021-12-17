# SETUP / TEAMS / BOWLER ORDER ETC. ##########

# a function to initialise global variables which track what happens on each ball of a 
# simulation.
simple_match_variables_init <- function() {
  p <<- c()
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
simple_sim_variables <- function() {
  s_bat1 <<- c()
  s_bat2 <<- c()
  s_bat1_runs <<- c()
  s_bat1_wickets <<- c()
  s_bat2_runs <<- c()
  s_bat2_wickets <<- c()
  s_winner <<- c()
  s_batter1 <<- c()
  s_batter1_score <<- c()
  s_total_6s <<- c()
  s_over1_runs <<- c()
}
