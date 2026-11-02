# Load packages
library(tidyverse)

# Read raw data
my_raw_data <- read_csv("admin/run_2.csv")

#### For compensation on Prolific ####

# Find out participants who were not grouped
was_not_grouped <- my_raw_data %>%
  filter(participant.was_not_grouped == 1) %>%
  select(prolific_id = participant.label)
write_csv(
  was_not_grouped,
  file = "admin/run_2_was_not_grouped.csv",
  col_names = FALSE
)

# Find out participants who were affected by dropouts
affected_by_dropouts <- my_raw_data %>%
  filter(participant.affected_by_dropout == 1, participant.is_dropout == 0) %>%
  select(prolific_id = participant.label)
write_csv(
  affected_by_dropouts,
  file = "admin/run_2_affected_by_dropouts.csv",
  col_names = FALSE
)

# Find out participants who dropped out
dropouts <- my_raw_data %>%
  filter(participant.is_dropout == 1) %>%
  select(prolific_id = participant.label)
write_csv(
  dropouts,
  file = "admin/run_2_dropouts.csv",
  col_names = FALSE
)

# Find out participants who completed the study
completed <- my_raw_data %>%
  filter(!is.na(demographics.1.player.attentive)) %>%
  select(prolific_id = participant.label)
write_csv(
  completed,
  file = "admin/run_2_completed.csv",
  col_names = FALSE
)

# Find out participants who should receive a bonus
bonus <- my_raw_data %>%
  filter(
    !is.na(demographics.1.player.attentive),
    participant.selected_for_bonus == 1
  ) %>%
  mutate(bonus = participant.final_point_min_zero/10) %>%
  select(prolific_id = participant.label, bonus)
write_csv(
  bonus,
  file = "admin/run_2_bonus.csv",
  col_names = FALSE
)

#### Keep usable data ####

# Find out participants who started the study
usable <- my_raw_data %>%
  filter(!is.na(many_mind_games.1.player.whether_match)) %>%
  select(-c(
    session.condition_list,
    participant.label,
    introduction.1.player.prolific_id
  ))
usable$run <- 2
write_csv(usable, "run_2_usable.csv", col_names = TRUE)
