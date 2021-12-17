# this file compiles info on every player in the training data (2008-2020)

library(tidyverse)
library(fs)

# get character vector of all files in 'info_2020' folder
# each file contains line-up info for every game of the training data
files <- dir_ls('info_2020')

# initialising team, player and game id variables
player <- c()
id <- c()
team <- c()

# for each game, collect players, teams and game id
for (i in 1:length(files)) {
  df <- read_csv(files[i], col_names = as.character(c(1:5)))
  start <- max(which(is.na(df$`4`))) + 1
  team <- team %>% append(df$`3`[start:(start+21)])
  player <- player %>% append(df$`4`[start:(start+21)])
  
  if (substring(files[i], 11, 17) %>% as.numeric() %>% is.na()) {
    id <- id %>% append(rep(substring(files[i], 11, 16) %>% as.numeric(), 22))
  }
  else {
    id <- id %>% append(rep(substring(files[i], 11, 17) %>% as.numeric(), 22))
  }
}

# team line-ups are given in batting order.
# we can infer each player's position in the batting line-up as being their position
# on the team-sheet.
position <- rep(1:11, 816*2)

# create tibble: train_lineups - sort by id, which is chronological
train_lineups <- tibble(id, team, player, position) %>% 
  arrange(id)

#####################################################################

# compiling data for each player
players <- train_lineups$player %>% unique()

# number of appearances
appearances <- c()
for (player in players) {
  appearances <- appearances %>% append(sum(player == train_lineups$player))
}

# get number of overs bowled for each player
balls <- read_csv('read_csv/IPL Ball-by-Ball 2008-2020.csv') # reading ball-by-ball data

n_overs <- c()
for (i in 1:length(players)) {
  # subset data to only rows with the given bowler
  df <- subset(balls, (bowler == players[i]))
  
  # counting the number of '1s' in the 'ball' column is equivalent to the number of overs bowled
  n_overs <- n_overs %>% append(sum(df$ball == 1))
}

# overs per appearance - rounded to nearest int
average_overs <- round(n_overs / appearances)

# balls faced
balls_faced <- c()
for (i in 1:length(players)) {
  balls_faced <- balls_faced %>% append(nrow(subset(balls, (batsman == players[i]))))
}

players_df <- tibble(players, appearances, average_overs, balls_faced)

#######################################################################

# creating separate dataframe for bowlers, for whom we have good data for. 
# included here if bowled more than 3 overs
bowlers_df <- players_df[which(players_df$average_overs * players_df$appearances > 3), ]

# another separate dataframe for batters. - included if faced more than 20 balls. kind of 
# arbitrary choice to prevent the model being trained on weird outlying stats.
batters_df <- players_df[which(players_df$balls_faced > 20), ]

###########################################################################

# dealing with test data

# get character vector of all files in 'info_2020' folder
# each file contains line-up info for every game of the training data
files <- dir_ls('ipl_csv2')

# initialising team, player, game id and venue variables
# venue will be necessary when it comes to prediction
player <- c()
id <- c()
team <- c()

# for each game, collect players, teams and game id
for (i in 1:length(files)) {
  df <- read_csv(files[i], col_names = as.character(c(1:5)))
  start <- max(which(is.na(df$`4`))) + 1
  team <- team %>% append(df$`3`[start:(start+21)])
  player <- player %>% append(df$`4`[start:(start+21)])
  id <- id %>% append(rep(substring(files[i], 10, 16) %>% as.numeric(), 22))
}

# collected venue info and number of overs for each bowler to bowl manually.
# hence it's read here from a csv
test_lineups <- read_csv('read_csv/test_lineups.csv')
test_lineups[, 1] <- id
test_lineups <- rename(test_lineups, id = game)

position <- rep(1:11, 60*2)
test_lineups <- add_column(test_lineups, position, .after = 'player')
test_lineups <- arrange(test_lineups, id)

# getting vector of players in test data
players_test <- unique(test_lineups$player)

# which players are new?
new_players <- players_test[which(players_test %in% players_df$players == F)]

new_players_df <- tibble(players = new_players, appearances = rep(0, length(new_players)),
                         average_overs = rep(0, length(new_players)), 
                         balls_faced = rep(0, length(new_players)))

players_df <- rbind(players_df, new_players_df)

# adding is_spin variable to players_df
spin_df <- read_csv('read_csv/spin_updated.csv')

is_spin <- c()

for(i in 1:nrow(players_df)) {
  if (players_df$players[i] %in% spin_df$bowler) {
    index <- which(players_df$players[i] == spin_df$bowler)
    is_spin <- is_spin %>% append(spin_df$is_spin[index])
  }
  else {
    is_spin <- is_spin %>% append(NA)
  }
}

players_df <- add_column(players_df, is_spin)

write_csv(players_df, 'write_csv/players_df.csv')
write_csv(batters_df, 'write_csv/batters_df.csv')
write_csv(bowlers_df, 'write_csv/bowlers_df.csv')
write_csv(new_players_df, 'write_csv/new_players_df.csv')
write_csv(test_lineups, 'write_csv/test_lineups.csv')
write_csv(train_lineups, 'write_csv/train_lineups.csv')
