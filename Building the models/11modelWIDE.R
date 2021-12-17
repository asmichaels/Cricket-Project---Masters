# adding is_wide to st_balls data
is_wide <- c()

for(i in 1:nrow(st_balls)) {
  if (st_balls$extras_type[i] %>% is.na() == F &
      st_balls$extras_type[i] == 'wides') {
    is_wide <- is_wide %>% append(TRUE)
  }
  else {
    is_wide <- is_wide %>% append(FALSE)
  }
}

st_balls <- add_column(st_balls, is_wide, .after = 'is_noball')

# given stumped, get p_wide
p_wide <- sum(st_balls$is_wide==T & st_balls$is_stumped == T) / sum(st_balls$is_stumped == T)
