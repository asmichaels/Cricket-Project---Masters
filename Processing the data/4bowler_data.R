bowlers <- bowlers_df$players

# get average
bowl_average <- c()

for (bowler1 in bowlers) {
  balls_bowled <- subset(balls, (bowler == bowler1))
  runs_allowed <- sum(balls_bowled$total_runs)
  wickets_taken <- sum(balls_bowled$outcome == 'wicket')
  if (wickets_taken == 0) {
    wickets_taken <- 1
  }
  
  bowl_average <- append(bowl_average, runs_allowed / wickets_taken)
}

# get economy rate
economy <- c()

for (bowler1 in bowlers) {
  balls_bowled <- subset(balls, (bowler == bowler1))
  runs_allowed <- sum(balls_bowled$total_runs)
  
  economy <- append(economy, runs_allowed / nrow(balls_bowled))
}

# wides
wides <- c()

for (bowler1 in bowlers) {
  balls_bowled <- subset(balls, (bowler == bowler1))
  wide <- sum(balls_bowled$outcome == 'wide')
  
  wides <- append(wides, wide / nrow(balls_bowled))
}

# noballs 
noballs <- c()

for (bowler1 in bowlers) {
  balls_bowled <- subset(balls, (bowler == bowler1))
  noball <- sum(balls_bowled$is_noball == 1)
  
  noballs <- append(noballs, noball / nrow(balls_bowled))
}

# dots, ones, twos, fours, sixes, byes
dots_bowl <- c()
ones_bowl <- c()
twos_bowl <- c()
fours_bowl <- c()
sixes_bowl <- c()
byes_bowl <- c()

for (bowler1 in bowlers) {
  balls_bowled <- subset(balls, (bowler == bowler1))
  dots <- sum(balls_bowled$batsman_runs == 0)
  ones <- sum(balls_bowled$batsman_runs == 1)
  twos <- sum(balls_bowled$batsman_runs == 2)
  fours <- sum(balls_bowled$batsman_runs == 4)
  sixes <- sum(balls_bowled$batsman_runs == 6)
  byes <- sum(balls_bowled$outcome == 'bye')
  
  dots_bowl <- append(dots_bowl, dots / nrow(balls_bowled))
  ones_bowl <- append(ones_bowl, ones / nrow(balls_bowled))
  twos_bowl <- append(twos_bowl, twos / nrow(balls_bowled))
  fours_bowl <- append(fours_bowl, fours / nrow(balls_bowled))
  sixes_bowl <- append(sixes_bowl, sixes / nrow(balls_bowled))
  byes_bowl <- append(byes_bowl, byes / nrow(balls_bowled))
}

# wickets rate
wicket_rate <- c()

for (bowler1 in bowlers) {
  balls_bowled <- subset(balls, (bowler == bowler1))
  wickets_taken <- sum(balls_bowled$outcome == 'wicket')
  
  wicket_rate <- append(wicket_rate, wickets_taken / nrow(balls_bowled))
}


# balls bowled
n_balls <- c()

for (bowler1 in bowlers) {
  balls_bowled <- subset(balls, (bowler == bowler1))
  
  n_balls <- append(n_balls, nrow(balls_bowled))
}

bowlers_data <- tibble(bowlers, bowl_average, economy, dots_bowl, ones_bowl, twos_bowl,
                       fours_bowl, sixes_bowl, byes_bowl, wicket_rate, wides, noballs, n_balls)

write_csv(bowlers_data, 'write_csv/bowlers_data.csv')
