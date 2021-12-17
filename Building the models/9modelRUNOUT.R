# removing the factor type from n_byes
balls$n_byes <- as.integer(as.character(balls$n_byes))

# getting subset of balls data for where a run-out is eligible
ro_balls <- balls[c(which(balls$outcome != 'wicket' & balls$total_runs < 5 & balls$batsman_runs < 4 &
                            balls$n_byes < 4)) %>% sort(), ]

# dropping factor levels from outcome
ro_balls$outcome <- droplevels(ro_balls$outcome)

# adding new column to ro_balls: is_runout
is_runout <- c()

for (i in 1:nrow(ro_balls)) {
  if (ro_balls$dismissal_kind[i] %>% is.na() == F) {
    if (ro_balls$dismissal_kind[i] == 'run out') {
      is_runout <- is_runout %>% append(TRUE)
    }
    else if (ro_balls$dismissal_kind[i] == 'obstructing the field') {
      is_runout <- is_runout %>% append(TRUE)
    }
    else {
      is_runout <- is_runout %>% append(FALSE)
    }
  }
  else {
    is_runout <- is_runout %>% append(FALSE)
  }
}

ro_balls <- add_column(ro_balls, is_runout, .after = 'n_byes')

# separating data to 1st and 2nd innings - we make a model for each
ro_balls1 <- subset(ro_balls, (is_first_innings == T))
ro_balls2 <- subset(ro_balls, (is_first_innings == F))

# getting training and test sets
set.seed(333)
test_data <- sample(1:nrow(ro_balls1), as.integer(nrow(ro_balls1))/10) %>% sort
train_ro1 <- ro_balls1[-test_data, ]
test_ro1 <- ro_balls1[test_data, ]

set.seed(333)
test_data <- sample(1:nrow(ro_balls2), as.integer(nrow(ro_balls2))/10) %>% sort
train_ro2 <- ro_balls2[-test_data, ]
test_ro2 <- ro_balls2[test_data, ]

# fitting models
ro_glm1 <- glm(is_runout ~ aggression, family = binomial,
               data = train_ro1)
summary(ro_glm1)

ro_glm2 <- glm(is_runout ~ aggression + required_run_rate, family = binomial,
               data = train_ro2)
summary(ro_glm2)

# testing predictions
preds_ro1 <- predict(ro_glm1, newdata = test_ro1, type = 'response')
preds_ro2 <- predict(ro_glm2, newdata = test_ro2, type = 'response')

actual_ro1 <- test_ro1$is_runout %>% sum()
actual_ro2 <- test_ro2$is_runout %>% sum()

predictions_ro <- c(sum(preds_ro1), sum(preds_ro2))
actual_ro <- c(test_ro1$is_runout %>% sum(), test_ro2$is_runout %>% sum())

glm_test <- tibble(actual_ro, predictions_ro)

ro_plot <- tibble('aggression' = test_ro1$aggression,
                  'p_runout' = preds_ro1)

library(showtext)
font_add("CM", "cm-unicode-0.6.3a/cmunbx.otf")
showtext_auto()
library(ggthemes)


p_runout_plot <- ggplot(ro_plot, aes(x = aggression, y = p_runout)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(text = element_text(family = "CM"),
        axis.title = element_text(size = 11),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = c(0.25, 0.75),
                     labels = c(0.25, 0.75)) +
  scale_y_continuous(breaks = c(0.02, 0.04),
                     labels = c(0.02, 0.04)) +
  labs(x = 'Aggression', y = 'P(Run-Out)')

ggplot(ro_plot, aes(x = aggression, y = p_runout)) +
  geom_line() +
  theme_fivethirtyeight() +
  theme(text = element_text(family = "CM"),
        axis.title = element_text(size = 13),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  scale_x_continuous(breaks = c(0.25, 0.75),
                     labels = c(0.25, 0.75)) +
  scale_y_continuous(breaks = c(0.02, 0.04),
                     labels = c(0.02, 0.04)) +
  labs(x = 'Aggression', y = 'P(Run-Out)')

ggsave('runout_plot.png', plot = p_runout_plot, device = png(),
       wid)
  