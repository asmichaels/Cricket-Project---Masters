outcomes <- c('0', '1', '2', '3', '4', '5', '6', 'wicket', 'wide', 'bye', 'legbye')

simple_get_p <- function(df) {
  p[1] <<- subset(df, (outcome == '0')) %>% nrow() / nrow(df)
  
  p[2] <<- subset(df, (outcome == '1')) %>% nrow / nrow(df)
  
  p[3] <<- subset(df, (outcome == '2')) %>% nrow / nrow(df)
  
  p[4] <<- subset(df, (outcome == '3')) %>% nrow / nrow(df)
  
  p[5] <<- subset(df, (outcome == '4')) %>% nrow / nrow(df)
  
  p[6] <<- subset(df, (outcome == '5')) %>% nrow / nrow(df)
  
  p[7] <<- subset(df, (outcome == '6')) %>% nrow / nrow(df)
  
  p[8] <<- subset(df, (outcome == 'wicket')) %>% nrow / nrow(df)
  
  p[9] <<- subset(df, (outcome == 'wide')) %>% nrow / nrow(df)
  
  p[10] <<- subset(df, (outcome == 'bye')) %>% nrow / nrow(df)
  
  p[11] <<- subset(df, (outcome == 'legbye')) %>% nrow() / nrow(df)
}

# STUMPINGS AND EXTRAS #########
stumped_df <- subset(balls, (dismissal_kind == 'stumped'))
p_stumped <- nrow(stumped_df) / subset(balls, (outcome == 'wicket')) %>% nrow
p_stumped_extra <- nrow(subset(stumped_df, (extra_runs > 0))) / nrow(stumped_df)
########

# NOBALLS #######

# given no wide, bye, legbye, wicket: p(noball)?
p_noball <- 
  subset(balls, (extras_type == 'noballs' & outcome != 'wide' & outcome != 'wicket' &
                   outcome != 'bye' & outcome != 'legbye')) %>% nrow() /
  subset(balls, (outcome != 'wide' & outcome != 'wicket' &
                   outcome != 'bye' & outcome != 'legbye')) %>% nrow()

# BYES ######
bye <- c()
bye[1] <- subset(byes_balls, (n_byes == 0)) %>% nrow / nrow(byes_balls)
bye[2] <- subset(byes_balls, (n_byes == 1)) %>% nrow / nrow(byes_balls)
bye[3] <- subset(byes_balls, (n_byes == 2)) %>% nrow / nrow(byes_balls)
bye[4] <- subset(byes_balls, (n_byes == 3)) %>% nrow / nrow(byes_balls)
bye[5] <- subset(byes_balls, (n_byes == 4)) %>% nrow / nrow(byes_balls)
bye[6] <- subset(byes_balls, (n_byes == 5)) %>% nrow / nrow(byes_balls)
###########

# RUN OUT ####
runout_eligible <- balls[c(which(balls$outcome != 'wicket' & balls$total_runs < 4)) %>% sort(), ]

p_runout <- 
  subset(runout_eligible, (dismissal_kind == 'run out' | 
                             dismissal_kind == 'obstructing the field')) %>% nrow /
  nrow(runout_eligible)
