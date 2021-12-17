# this file gets many more useful variables for modelling and adds them to the ball
# -by-ball data.
library(tidyverse)

# reading in balls data and games data
balls <- read_csv("read_csv/IPL Ball-by-Ball 2008-2020.csv")
games <- read_csv("read_csv/IPL Matches 2008-2020.csv")

# sorting by id, then innings, over, ball
balls <- balls[
  with(balls, order(id, inning, over, ball)),
]

##########

# adding an outcome variable column to balls data #######
# this will be the 'response' variable of the main model
# takes about 20 minutes to run on my machine 

outcomes <- c('0', '1', '2', '3', '4', '5', '6', 'wide', 'bye', 'legbye', 'wicket')
outcome <- c()

for (i in 1:nrow(balls)) {
  # check immediate wicket
  if (balls$is_wicket[i] == 1) { #
    
    if (balls$dismissal_kind[i] != 'run out' &
        balls$dismissal_kind[i] != 'obstructing the field' &
        balls$dismissal_kind[i] != 'retired hurt') { ##
      outcome <- append(outcome, "wicket")
      next
    }##
  }#
  
  # check extra
  if (is.na(balls$extras_type[i]) == F) { #
    
    if (balls$extras_type[i] == 'wides') { ##
      outcome <- append(outcome, 'wide')
      next
    }
    
    else if (balls$extras_type[i] == 'byes') { ##
      outcome <- append(outcome, 'bye')
      next
    }##
    
    else if (balls$extras_type[i] == 'legbyes') { ##
      outcome <- append(outcome, 'legbye')
      next
    }##
  }#
  
  # check batsman runs
  outcome <- append(outcome, balls$batsman_runs[i])
}

balls <- add_column(balls, outcome, .after = 'bowler')
#########

# adding an is_noball column to balls data ######
is_noball <- c()

for (i in 1:nrow(balls)) {
  if (is.na(balls$extras_type[i]) == F) {
    if (balls$extras_type[i] == 'noballs') {
      is_noball <- append(is_noball, 1)      
    }
    else {
      is_noball <- append(is_noball, 0)
    }
  }
  else {
    is_noball <- append(is_noball, 0)
  }
}

balls <- add_column(balls, is_noball, .after = 'outcome')
#########

# fixing some errors in the data ############

# game 419155 - counting error over 18 of 1st inns. delete this ball from the data
balls <- balls[-39065, ]

# game 1178407 - bye is actually a no-ball. over 16 1st inns
balls$outcome[172273] <- '0'
balls$is_noball[172273] <- 1
balls$extras_type[172273] <- 'noballs'

# game 1216497 - legbye is a no-ball. over 4 2nd inns
balls$outcome[180290] <- '0'
balls$is_noball[180290] <- 1
balls$extras_type[180290] <- 'noballs'

# game 1216500 - byes are noballs. over 18 1st inns
balls$outcome[180995] <- '0'
balls$is_noball[180995] <- 1
balls$extras_type[180995] <- 'noballs'

# game 1216510 - byes are noballs. over 9 1st inns
balls$outcome[183372] <- '0'
balls$is_noball[183372] <- 1
balls$extras_type[183372] <- 'noballs'

# game 1216511 - byes are noballs. over 18 1st inns
balls$outcome[183659] <- '0'
balls$is_noball[183659] <- 1
balls$extras_type[183659] <- 'noballs'

# game 1216527 - byes are noballs. over 4 1st inns
balls$outcome[187430] <- '0'
balls$is_noball[187430] <- 1
balls$extras_type[187430] <- 'noballs'

#############################################################3

# adding game state variables ##########
ids <- balls$id %>% unique()

### current runs ###
current_runs <- c()
current_runs_1 <- c()

for (i in 1:length(ids)) {
  match <- subset(balls, (id == ids[i]))
  
  for (j in 1:length(match$inning %>% unique())) {
    
    for (k in 1:sum(match$inning == j)) {
      current_runs_1[k] <- ifelse(k == 1, 0, sum(subset(match, (inning == j))$total_runs[1:(k-1)]))
    }
    current_runs <- append(current_runs, current_runs_1)
    current_runs_1 <- c()
  }
}
remove(current_runs_1, match)

# add current runs to balls
balls <- add_column(balls, 'current_runs' = current_runs, .before = 'batsman')



### current wickets ###

current_wickets <- c()
current_wickets1 <- c()

for (i in 1:length(ids)) {
  match <- subset(balls, (id == ids[i]))
  
  for (j in 1:length(match$inning %>% unique())) {
    
    for (k in 1:sum(match$inning == j)) {
      current_wickets1[k] <- ifelse(k == 1, 0, sum(subset(match, (inning == j))$is_wicket[1:(k-1)]))
    }
    current_wickets <- append(current_wickets, current_wickets1)
    current_wickets1 <- c()
  }
}
remove(current_wickets1, match)

balls <- add_column(balls, 'current_wickets' = current_wickets, .before = 'batsman')



### first innings score ###

fis <- c()
fis_1 <- c()

for (i in 1:length(ids)) {
  match <- subset(balls, (id == ids[i]))
  i1 <- subset(match, (inning == 1))
  i2 <- subset(match, (inning == 2))
  i1_score <- tail(i1$current_runs, 1) + tail(i1$total_runs, 1)
  
  fis_1 <- c(rep(NA, nrow(i1)), rep(i1_score, nrow(i2)))
  
  fis <- append(fis, fis_1)
}
remove(fis_1, i1_score, i1, i2, match)

balls <- add_column(balls, '1st_inns' = fis, .before = 'current_runs')

# adding is_spin column to balls data ########
spin_df <- read_csv("read_csv/spin_updated.csv")
is_spin <- c()

for (i in balls$bowler) {
  index <- which(i == spin_df$bowler)
  is_spin <- append(is_spin, spin_df$is_spin[index])
}

balls <- add_column(balls, is_spin, .after = 'bowler')
#########

# adding batter position
batter_position <- c()
for (i in 1:nrow(balls)) {
  id <- balls$id[i]
  batter <- balls$batsman[i]
  index <- which(train_lineups$id == id & train_lineups$player == batter)
  batter_position <- batter_position %>% append(train_lineups$position[index])
}

balls <- add_column(balls, batter_position, .after = 'batsman')

# adding average overs for the given bowler
average_overs <- c()
for (i in 1:nrow(balls)) {
  bowler <- balls$bowler[i]
  index <- which(bowler == players_df$players)
  average_overs <- average_overs %>% append(players_df$average_overs[index])
}

balls <- add_column(balls, average_overs, .after = 'bowler')

# adding season column ########
season <- c()

for (i in balls$id) {
  index <- which(i == games$id)
  year <- format(games$date[index], format = "%Y")
  season <- append(season, as.numeric(year))
}
remove(year, index)

balls <- add_column(balls, season = season, .before = 2)
############

# creating new dataset WITHOUT penalty balls AND retired hurt balls #####
p_r <- which(balls$extras_type == 'penalty' | balls$dismissal_kind == 'retired hurt')
balls_pr <- balls[-p_r, ]
remove(p_r)
#######

# creating new dataset of ONLY free hit balls ######
noball_balls <- which(balls$extras_type == 'noballs')
free_hit_balls <- noball_balls + 1
year_2015_start <- which(balls$season == 2015)[1]
free_hit_balls <- free_hit_balls[free_hit_balls >= year_2015_start]
free_hits <- balls[free_hit_balls, ]
remove(noball_balls)
remove(year_2015_start)

# adding is_freehit column to balls data
is_freehit <- rep(0, nrow(balls))
for (i in free_hit_balls) {
  is_freehit[i] <- 1
}
balls <- add_column(balls, is_freehit, .after = 'ball')

#########

# reading in games data about number of overs etc. ##########
limit <- read_csv("read_csv/games_limit.csv")

type <- ifelse(limit$is_short == 'D/L', 'D/L', 
               ifelse(limit$is_short == 'NR', 'NR', 
                      ifelse(limit$full_game == 1, 1,
                             ifelse(limit$full_game == 0, 'short', 'err'))))

limit <- add_column(limit, type, .before = 'is_short')
remove(type)

# removing is_short and full_game columns
limit <- limit[, -c(7, 11)]

interrupted_games <- subset(limit, (i1_balls == 'check' | i2_balls == 'check'))

## 
i1_balls1 <- c(120, NA, 120, 120, 120, NA, NA)
i1_break1 <- c(49, NA, 45, 54, 66, NA, NA)
i1_balls2 <- c(66, NA, 108, 102, 78, NA, NA)
i1_break2 <- c(NA, NA, NA, NA, NA, NA, NA)

i2_balls1 <- c(NA, 72, NA, NA, NA, 90, 120)
i2_break1 <- c(NA, 11, NA, NA, NA, 7, 50)
i2_balls2 <- c(NA, 36, NA, NA, NA, 72, 78)
i2_break2 <- c(NA, NA, NA, NA, NA, 12, NA)
i2_balls3 <- c(NA, NA, NA, NA, NA, 30, NA)
i2_target1 <- c(NA, NA, 187, 135, 102, 117, NA)
i2_target2 <- c(NA, 54, NA, NA, NA, 97, 125)
i2_target3 <- c(NA, NA, NA, NA, NA, 43, NA)
##

check <- tibble('id' = interrupted_games$id, i1_balls1, i1_break1, i1_balls2, i1_break2, 
                i2_balls1, i2_break1, i2_balls2, i2_break2, i2_balls3, i2_target1, 
                i2_target2, i2_target3)
remove(i1_balls1, i1_break1, i1_balls2, i1_break2, i2_balls1, i2_break1, i2_balls2,
       i2_break2, i2_balls3, i2_target1, i2_target2, i2_target3)

############

# adding balls remaining to balls dataset #############

# need new column for balls that doesn't include extras 
balls_ex <- c()

for (i in 1:nrow(balls)) {
  if (i == 1) {
    balls_ex <- append(balls_ex, 1)
    next
  }
  else {
    if (balls$id[i] == balls$id[i-1]) {
      if (balls$inning[i] == balls$inning[i-1]) {
        if (is.na(balls$extras_type[i-1]) == F) {
          if (balls$is_noball[i-1] == 1 | balls$extras_type[i-1] == 'wides') {
            balls_ex <- append(balls_ex, tail(balls_ex, 1))
            next
          }
          else {
            balls_ex <- append(balls_ex, tail(balls_ex, 1) + 1)
            next
          }
        }
        else {
          balls_ex <- append(balls_ex, tail(balls_ex, 1) + 1)
          next
        }
      }
      else {
        balls_ex <- append(balls_ex, 1)
        next
      }
    }
    else {
      balls_ex <- append(balls_ex, 1)
      next
    }
  }
}

balls <- add_column(balls, balls_ex, .after = 'ball')

########### adding balls remaining ###########
balls_remaining <- c()

for (i in 1:length(ids)) {
  if (limit$type[i] == 1) {
    match <- subset(balls, (id == ids[i]))
    balls_remaining <- append(balls_remaining, (120 - match$balls_ex + 1))
    next
  }
  
  else if (ids[i] %in% check$id == F) {
    match <- subset(balls, (id == ids[i]))
    i1 <- subset(match, (inning == 1))
    i1_balls <- limit$i1_balls[i]
    balls_remaining <- append(balls_remaining, (as.numeric(i1_balls) - i1$balls_ex + 1))
    
    if (match$inning %>% unique() %>% length() > 1) {
      i2 <- subset(match, (inning == 2))
      i2_balls <- limit$i2_balls[i]
      balls_remaining <- append(balls_remaining, (as.numeric(i2_balls) - i2$balls_ex + 1))
      next
    }
    else {
      next
    }
  }
  
  else if (ids[i] %in% check$id) {
    if (limit$i1_balls[i] == 'check') {
      match <- subset(balls, (id == ids[i]))
      index <- which(ids[i] == check$id)
      i1 <- subset(match, (inning == 1))
      
      i1_balls1 <- check$i1_balls1[index]
      i1_break1 <- check$i1_break1[index]
      i1_balls2 <- check$i1_balls2[index]
      
      end_point1 <- i1$balls_ex[1:tail(which(i1_break1 == i1$balls_ex), 1)] %>% length()
      
      balls_remaining <- append(balls_remaining, (i1_balls1 - i1$balls_ex[1:end_point1] + 1))
      balls_remaining <- append(balls_remaining, (i1_balls2 - i1$balls_ex[(end_point1 + 1):nrow(i1)] + 1))
      
      i2 <- subset(match, (inning == 2))
      i2_balls <- limit$i2_balls[i]
      balls_remaining <- append(balls_remaining, (as.numeric(i2_balls) - i2$balls_ex + 1))
      next
    }
    
    else {
      match <- subset(balls, (id == ids[i]))
      index <- which(ids[i] == check$id)
      i1 <- subset(match, (inning == 1))
      i1_balls <- limit$i1_balls[i]
      balls_remaining <- append(balls_remaining, (as.numeric(i1_balls) - i1$balls_ex + 1))
      
      i2 <- subset(match, (inning == 2))
      
      if (is.na(check$i2_break2[index])) {
        i2_balls1 <- check$i2_balls1[index]
        i2_break1 <- check$i2_break1[index]
        i2_balls2 <- check$i2_balls2[index]
        
        end_point1 <- i2$balls_ex[1:tail(which(i2_break1 == i2$balls_ex), 1)] %>% length()
        
        balls_remaining <- append(balls_remaining, (i2_balls1 - i2$balls_ex[1:end_point1] + 1))
        balls_remaining <- append(balls_remaining, (i2_balls2 - i2$balls_ex[(end_point1 + 1):nrow(i2)] + 1))
        next
      }
      
      else {
        i2_balls1 <- check$i2_balls1[index]
        i2_break1 <- check$i2_break1[index]
        i2_balls2 <- check$i2_balls2[index]
        i2_break2 <- check$i2_break2[index]
        i2_balls3 <- check$i2_balls3[index]
        
        end_point1 <- i2$balls_ex[1:tail(which(i2_break1 == i2$balls_ex), 1)] %>% length()
        end_point2 <- i2$balls_ex[end_point1 + 1 : tail(which(i2_break2 == i2$balls_ex), 1)] %>% length()
        
        balls_remaining <- append(balls_remaining, (i2_balls1 - i2$balls_ex[1:end_point1] + 1))
        balls_remaining <- append(balls_remaining, (i2_balls2 - i2$balls_ex[(end_point1 + 1):end_point2] + 1))
        balls_remaining <- append(balls_remaining, (i2_balls3 - i2$balls_ex[(end_point2 + 1):nrow(i2)] + 1))
        next
      }
    }
  }
}
remove(i1_balls, i1_balls1, i1_balls2, i1_break1, i2_balls, i2_balls1, i2_balls2, i2_balls3,
       i2_break1, i2_break2, end_point1, end_point2, i1, i2, match)

balls <- add_column(balls, balls_remaining, .after = 'balls_ex')
###########

####### adding wickets remaining #########
wickets_remaining <- c()

for (i in 1:nrow(balls)) {
  wickets_remaining <- append(wickets_remaining, 10 - balls$current_wickets[i])
}

balls <- add_column(balls, wickets_remaining, .after = 'balls_remaining')
#######

####### adding target score ######

short_games <- subset(limit, (type != 1 & i2_balls != i1_balls))
target1 <- c(123, 105, 187, 135, 102, 117, 118, 81, 66, 48, 71, 192, 151)
target2 <- c(NA, 54, NA, NA, NA, 97, NA, NA, NA, NA, NA, 125, NA)
target3 <- c(NA, NA, NA, NA, NA, 43, NA, NA, NA, NA, NA, NA, NA)

short_games$target1 <- target1 
short_games$target2 <- target2
short_games$target3 <- target3

short2 <- subset(short_games, (is.na(target2) == F))
short2$break1 <- c(11, 7, 50)
short2$break2 <- c(NA, 12, NA)

others <- subset(limit, (type != 1 & i2_balls == i1_balls))

target_score <- c()

for (i in 1:nrow(limit)) {
  
  match <- subset(balls, (id == ids[i]))
  i1 <- subset(match, (inning == 1))
  if (length(match$inning %>% unique) > 1) {
    i2 <- subset(match, (inning == 2))
  }
  
  if (ids[i] %in% short_games$id == F) {
    target_score <- append(target_score, match$`1st_inns` + 1)
    next
  }
  
  else if (ids[i] %in% short2$id == F) {
    index <- which(limit$id[i] == short_games$id)
    
    target_score <- append(target_score, c(rep(NA, nrow(i1)), 
                                           rep(short_games$target1[index], nrow(i2))))
    next
  }
  
  else {
    index <- which(limit$id[i] == short2$id)
    if (short2$target3[index] %>% is.na()) {
      target_score <- append(target_score, c(rep(NA, nrow(i1))))
      end_point1 <- i2$balls_ex[1:tail(which(short2$break1[index] == i2$balls_ex), 1)] %>% length()
      
      target_score <- append(target_score, c(rep(short2$target1[index], end_point1)))
      target_score <- append(target_score, c(rep(short2$target2[index], nrow(i2) - end_point1)))
    }
    else {
      target_score <- append(target_score, c(rep(NA, nrow(i1))))
      end_point1 <- i2$balls_ex[1:tail(which(short2$break1[index] == i2$balls_ex), 1)] %>% length()
      end_point2 <- i2$balls_ex[1:tail(which(short2$break2[index] == i2$balls_ex), 1)] %>% length()
      
      target_score <- append(target_score, c(rep(short2$target1[index], end_point1)))
      target_score <- append(target_score, c(rep(short2$target2[index], end_point2 - end_point1)))
      target_score <- append(target_score, c(rep(short2$target3[index], nrow(i2) - end_point2)))
    }
  }
}

balls <- add_column(balls, target_score, .after = '1st_inns')
remove(match, i1, i2, others, end_point1, end_point2, index)

###########

###### adding runs required #########
runs_required <- ifelse(balls$inning == 1, NA, balls$target_score - balls$current_runs)
balls <- add_column(balls, runs_required, .after = 'current_runs')

########

##### adding is_powerplay ##########
is_powerplay <- c()

for (i in 1:nrow(limit)) {
  match <- subset(balls, (id == limit$id[i]))
  i1 <- subset(match, (inning == 1))
  if (length(match$inning %>% unique) > 1) {
    i2 <- subset(match, (inning == 2))
  }
  
  is_powerplay <- append(is_powerplay, ifelse(i1$over <= limit$pp_i1[i], 1, 0))
  
  if (length(match$inning %>% unique) > 1) {
    is_powerplay <- append(is_powerplay, ifelse(i2$over <= limit$pp_i2[i], 1, 0))
  }
}

balls <- add_column(balls, is_powerplay, .after = 'balls_ex')

##########

######## home advantage ##########

# fixing games data #
# neutral_venue #
nv <- c()
for (i in 1:length(games$city)) {
  if (games$city[i] %>% is.na() == F) {
    if (games$city[i] == 'Abu Dhabi') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Ahmedabad') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Bangalore') {
      if (games$team1[i] != 'Royal Challengers Bangalore' & games$team2[i] != 'Royal Challengers Bangalore') {
        nv[i] <- 1
      }
      else {
        nv[i] <- 0
      }
    }
    else if (games$city[i] == 'Bengaluru') {
      nv[i] <- 0
    }
    else if (games$city[i] == 'Bloemfontein') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Cape Town') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Centurion') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Chandigarh') {
      nv[i] <- 0
    }
    else if (games$city[i] == 'Chennai') {
      if (games$team1[i] != 'Chennai Super Kings' & games$team2[i] != 'Chennai Super Kings') {
        nv[i] <- 1
      }
      else {
        nv[i] <- 0
      }
    }
    else if (games$city[i] == 'Cuttack') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Delhi') {
      if (games$team1[i] != 'Delhi Daredevils' & games$team2[i] != 'Delhi Daredevils' &
          games$team1[i] != 'Delhi Capitals' & games$team2[i] != 'Delhi Capitals') {
        nv[i] <- 1
      }
      else {
        nv[i] <- 0
      }
    }
    else if (games$city[i] == 'Dharamsala') {
      nv[i] <- 0
    }
    else if (games$city[i] == 'Dubai') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Durban') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'East London') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Hyderabad') {
      if (games$team1[i] != 'Deccan Chargers' & games$team2[i] != 'Deccan Chargers' &
          games$team1[i] != 'Sunrisers Hyderabad' & games$team2[i] != 'Sunrisers Hyderabad') {
        nv[i] <- 1
      }
      else {
        nv[i] <- 0
      }
    }
    else if (games$city[i] == 'Indore') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Jaipur') {
      nv[i] <- 0
    }
    else if (games$city[i] == 'Johannesburg') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Kanpur') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Kimberley') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Kochi') {
      nv[i] <- 0
    }
    else if (games$city[i] == 'Kolkata') {
      if (games$team1[i] != 'Kolkata Knight Riders' & games$team2[i] != 'Kolkata Knight Riders') {
        nv[i] <- 1
      }
      else {
        nv[i] <- 0
      }
    }
    else if (games$city[i] == 'Mumbai') {
      if (games$team1[i] != 'Mumbai Indians' & games$team2[i] != 'Mumbai Indians') {
        nv[i] <- 1
      }
      else {
        nv[i] <- 0
      }
    }
    else if (games$city[i] == 'Nagpur') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Port Elizabeth') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Pune') {
      if (games$team1[i] != 'Pune Warriors' & games$team2[i] != 'Pune Warriors' &
          games$team1[i] != 'Rising Pune Supergiants' & games$team2[i] != 'Rising Pune Supergiants' &
          games$team1[i] != 'Rising Pune Supergiant' & games$team2[i] != 'Rising Pune Supergiant') {
        nv[i] <- 1
      }
      else {
        nv[i] <- 0
      }
    }
    else if (games$city[i] == 'Raipur') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Rajkot') {
      nv[i] <- 0
    }
    else if (games$city[i] == 'Ranchi') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Sharjah') {
      nv[i] <- 1
    }
    else if (games$city[i] == 'Visakhapatnam') {
      nv[i] <- 1
    }
  }
  else {
    nv[i] <- 1
  }
}

games <- add_column(games, nv, .before = 'neutral_venue')
games <- games[, -7]

cities <- subset(games, (nv==0))$city %>% unique()
cities

teams <- games$team1 %>% unique
city1 <- c('Bangalore', 'Chandigarh', 'Delhi', 'Mumbai', 'Kolkata', 'Jaipur', 'Hyderabad',
           'Chennai', 'Kochi', 'Pune', 'Hyderabad', 'Rajkot', 'Pune', 'Pune', 'Delhi')
city2 <- c('Bengaluru', 'Dharamsala', rep(NA, 13))

team_city <- tibble(teams, city1, city2)

# home advantage to batting team? #
home_advantage_bat <- c()
for (i in 1:nrow(games)) {
  match <- subset(balls, (id == ids[i]))
  
  if (games$nv[i] == 1) {
    home_advantage_bat <- append(home_advantage_bat, rep(0, nrow(match)))
  }
  
  else {
    city <- games$city[i]
    i1 <- subset(match, (inning == 1))
    i1_bat <- i1$batting_team[1]
    index <- which(i1_bat == team_city$teams)
    if (city == team_city$city1[index]) {
      home_advantage_bat <- append(home_advantage_bat, rep(1, nrow(i1)))
      home_advantage_bat <- append(home_advantage_bat, rep(0, nrow(match) - nrow(i1)))
    }
    else if (is.na(team_city$city2[index]) == F & city == team_city$city2[index]) {
      home_advantage_bat <- append(home_advantage_bat, rep(1, nrow(i1)))
      home_advantage_bat <- append(home_advantage_bat, rep(0, nrow(match) - nrow(i1)))
    }
    else {
      home_advantage_bat <- append(home_advantage_bat, rep(0, nrow(i1)))
      home_advantage_bat <- append(home_advantage_bat, rep(1, nrow(match) - nrow(i1)))
    }
  }
}
balls <- add_column(balls, home_advantage_bat, .after = 'is_powerplay')

# adding venue to balls data #

for (i in 1:nrow(games)) {
  if (games$city[i] %>% is.na() == F) {
    if (games$city[i] == 'Bengaluru') {
      games$venue[i] <- 'M Chinnaswamy Stadium'
    }
    else if (games$city[i] == 'Chandigarh') {
      games$venue[i] <- 'Punjab Cricket Association Stadium, Mohali'
    }
    else if (games$city[i] == 'Pune') {
      games$venue[i] <- 'Maharashtra Cricket Association Stadium'
    }
  }
}

venue <- c()
for (i in 1:nrow(games)) {
  match <- subset(balls, (id == ids[i]))
  venue <- append(venue, rep(games$venue[i], nrow(match)))
}

balls <- add_column(balls, venue, .after = 'bowling_team')

############

write_csv(balls, 'write_csv/balls_after_2.csv')
