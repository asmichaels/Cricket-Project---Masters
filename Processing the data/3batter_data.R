batters <- batters_df$players

# getting batting average for each batter
average <- c()

for (batter in batters) {
  on_strike <- subset(balls, (batsman == batter))
  runs <- sum(on_strike$batsman_runs)
  outs <- sum(on_strike$outcome == 'wicket')
  if (outs == 0) {
    outs <- 1
  }
  average <- append(average, runs / outs)
}

# getting strike-rate for each batter
strike_rate <- c()

for (batter in batters) {
  on_strike <- subset(balls, (batsman == batter))
  runs <- sum(on_strike$batsman_runs)
  wide <- sum(on_strike$outcome == 'wide')
  nb <- sum(on_strike$is_noball == 1)
  total_balls <- nrow(on_strike)
  
  strike_rate <- append(strike_rate, runs / (total_balls - nb - wide))
}

# getting 6-rate 
six_rate <- c()

for (batter in batters) {
  on_strike <- subset(balls, (batsman == batter))
  sixes <- sum(on_strike$batsman_runs == 6)
  wide <- sum(on_strike$outcome == 'wide')
  nb <- sum(on_strike$is_noball == 1)
  total_balls <- nrow(on_strike)
  
  six_rate <- append(six_rate, sixes / (total_balls - nb - wide))
}

# fours
four_rate <- c()

for (batter in batters) {
  on_strike <- subset(balls, (batsman == batter))
  fours <- sum(on_strike$batsman_runs == 4)
  wide <- sum(on_strike$outcome == 'wide')
  nb <- sum(on_strike$is_noball == 1)
  total_balls <- nrow(on_strike)
  
  four_rate <- append(four_rate, fours / (total_balls - nb - wide))
}

# outs
out_rate <- c()

for (batter in batters) {
  on_strike <- subset(balls, (batsman == batter))
  outs <- sum(on_strike$outcome == 'wicket')
  wide <- sum(on_strike$outcome == 'wide')
  nb <- sum(on_strike$is_noball == 1)
  total_balls <- nrow(on_strike)
  
  out_rate <- append(out_rate, outs / (total_balls - nb - wide))
}

#leg-byes and byes
leg_byes_rate <- c()
byes_rate <- c()

for (batter in batters) {
  on_strike <- subset(balls, (batsman == batter))
  leg_byes <- sum(on_strike$outcome == 'legbye')
  byes <- sum(on_strike$outcome == 'bye')
  wide <- sum(on_strike$outcome == 'wide')
  nb <- sum(on_strike$is_noball == 1)
  total_balls <- nrow(on_strike)
  leg_byes_rate <- append(leg_byes_rate, leg_byes / (total_balls - nb - wide))
  byes_rate <- append(byes_rate, byes / (total_balls - nb - wide))
}

# balls faced
balls_faced <- c()

for (batter in batters) {
  on_strike <- subset(balls, (batsman == batter))
  balls_faced <- append(balls_faced, nrow(on_strike))
}

# 0, 1, 2 rate
dot_rate <- c()
one_rate <- c()
two_rate <- c()

for (batter in batters) {
  on_strike <- subset(balls, (batsman == batter))
  dots <- sum(on_strike$batsman_runs == 0)
  ones <- sum(on_strike$batsman_runs == 1)
  twos <- sum(on_strike$batsman_runs == 2)
  wide <- sum(on_strike$outcome == 'wide')
  nb <- sum(on_strike$is_noball == 1)
  total_balls <- nrow(on_strike)
  dot_rate <- append(dot_rate, dots / (total_balls - nb - wide))
  one_rate <- append(one_rate, ones / (total_balls - nb - wide))
  two_rate <- append(two_rate, twos / (total_balls - nb - wide))
}

batters_data <- tibble(batters, average, strike_rate, dot_rate, one_rate, two_rate, 
                       four_rate, six_rate, byes_rate, leg_byes_rate, out_rate, balls_faced)

write_csv(batters_data, 'write_csv/batters_data.csv')
