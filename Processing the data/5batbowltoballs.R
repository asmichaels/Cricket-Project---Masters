# this file adds historical statistics for the given batter and bowler to the 
# ball-by-ball data. if applicable. if not, the missing variable shows a 1, indicating
# no data for the given player
# warning: takes forever to run. 
# i'm sure the way this is written is less than optimal from an efficiency perspective.

# initialising variables
missing_bowl <- c()
bowl_average <- c()
bowl_econ <- c()
bowl_0 <- c()
bowl_1 <- c()
bowl_2 <- c()
bowl_4 <- c()
bowl_6 <- c()
bowl_bye <- c()
bowl_wicket <- c()
bowl_wide <- c()
bowl_noball <- c()
bowl_balls <- c()

missing_bat <- c()
bat_average <- c()
bat_sr <- c()
bat_0 <- c()
bat_1 <- c()
bat_2 <- c()
bat_4 <- c()
bat_6 <- c()
bat_bye <- c()
bat_legbye <- c()
bat_out <- c()
bat_balls <- c()

for (i in 1:nrow(balls)) {
  bowler <- balls$bowler[i]
  batter <- balls$batsman[i]
  
  if (bowler %in% bowlers_data$bowlers) {
    missing_bowl <- missing_bowl %>% append(0)
    index_bowl <- which(bowler == bowlers_data$bowlers)
    
    bowl_average <- append(bowl_average, bowlers_data$bowl_average[index_bowl])
    bowl_econ <- append(bowl_econ, bowlers_data$economy[index_bowl])
    bowl_0 <- append(bowl_0, bowlers_data$dots_bowl[index_bowl])
    bowl_1 <- append(bowl_1, bowlers_data$ones_bowl[index_bowl])
    bowl_2 <- append(bowl_2, bowlers_data$ones_bowl[index_bowl])
    bowl_4 <- append(bowl_4, bowlers_data$fours_bowl[index_bowl])
    bowl_6 <- append(bowl_6, bowlers_data$sixes_bowl[index_bowl])
    bowl_bye <- append(bowl_bye, bowlers_data$byes_bowl[index_bowl])
    bowl_wicket <- append(bowl_wicket, bowlers_data$wicket_rate[index_bowl])
    bowl_wide <- append(bowl_wide, bowlers_data$wides[index_bowl])
    bowl_noball <- append(bowl_noball, bowlers_data$noballs[index_bowl])
    bowl_balls <- append(bowl_balls, bowlers_data$n_balls[index_bowl])
  }
  
  else {
    missing_bowl <- missing_bowl %>% append(1)
    
    bowl_average <- append(bowl_average, 0)
    bowl_econ <- append(bowl_econ, 0)
    bowl_0 <- append(bowl_0, 0)
    bowl_1 <- append(bowl_1, 0)
    bowl_2 <- append(bowl_2, 0)
    bowl_4 <- append(bowl_4, 0)
    bowl_6 <- append(bowl_6, 0)
    bowl_bye <- append(bowl_bye, 0)
    bowl_wicket <- append(bowl_wicket, 0)
    bowl_wide <- append(bowl_wide, 0)
    bowl_noball <- append(bowl_noball, 0)
    bowl_balls <- append(bowl_balls, 0)
  }
  
  if (batter %in% batters_data$batters) {
    missing_bat <- missing_bat %>% append(0)
    index_bat <- which(batter == batters_data$batters)
    
    bat_average <- append(bat_average, batters_data$average[index_bat])
    bat_sr <- append(bat_sr, batters_data$strike_rate[index_bat])
    bat_0 <- append(bat_0, batters_data$dot_rate[index_bat])
    bat_1 <- append(bat_1, batters_data$one_rate[index_bat])
    bat_2 <- append(bat_2, batters_data$two_rate[index_bat])
    bat_4 <- append(bat_4, batters_data$four_rate[index_bat])
    bat_6 <- append(bat_6, batters_data$six_rate[index_bat])
    bat_bye <- append(bat_bye, batters_data$byes_rate[index_bat])
    bat_legbye <- append(bat_legbye, batters_data$leg_byes_rate[index_bat])
    bat_out <- append(bat_out, batters_data$out_rate[index_bat])
    bat_balls <- append(bat_balls, batters_data$balls_faced[index_bat])
  }
  
  else {
    missing_bat <- missing_bat %>% append(1)
    
    bat_average <- append(bat_average, 0)
    bat_sr <- append(bat_sr, 0)
    bat_0 <- append(bat_0, 0)
    bat_1 <- append(bat_1, 0)
    bat_2 <- append(bat_2, 0)
    bat_4 <- append(bat_4, 0)
    bat_6 <- append(bat_6, 0)
    bat_bye <- append(bat_bye, 0)
    bat_legbye <- append(bat_legbye, 0)
    bat_out <- append(bat_out, 0)
    bat_balls <- append(bat_balls, 0)
  }
  
  # this takes ages to run. i add here some kind of status bar to show how far along 
  # we are. there are over 190000 rows to iterate over
  if (i%%10000 == 0) {
    print(i)
  }
}

balls <- add_column(balls, missing_bat, bat_average, bat_sr, bat_0, bat_1, bat_2, bat_4, bat_6, 
                    bat_bye, bat_legbye, bat_out, bat_balls, .after = 'batter_position')

balls <- add_column(balls, missing_bowl, bowl_average, bowl_econ, bowl_0, bowl_1, bowl_2, bowl_4, 
                    bowl_6, bowl_bye, bowl_wicket, bowl_wide, bowl_noball,
                    bowl_balls, .after = 'is_spin')

write_csv(balls, 'write_csv/balls_after_5.csv')