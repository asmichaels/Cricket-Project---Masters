# getting subset of the data for where stumped is eligible
st_balls <- subset(balls, (outcome == 'wicket'))

# adding new column to st_balls: is_stumped
is_stumped <- c()

for(i in 1:nrow(st_balls)) {
  if (st_balls$dismissal_kind[i] == 'stumped') {
    is_stumped <- is_stumped %>% append(TRUE)
  }
  else {
    is_stumped <- is_stumped %>% append(FALSE)
  }
}

st_balls <- add_column(st_balls, is_stumped, .after = 'outcome')

# if spin:
p_st_spin <- subset(st_balls, (is_spin == T & is_stumped == T)) %>% nrow() /
  subset(st_balls, (is_spin == T)) %>% nrow()

# if nospin:
p_st_nospin <- subset(st_balls, (is_spin == F & is_stumped == T)) %>% nrow() /
  subset(st_balls, (is_spin == F)) %>% nrow()



