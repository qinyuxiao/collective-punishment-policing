library(tidyverse)

# Read raw data
raw <- read_csv("admin/pilot.csv")

# Select out participants affected by dropouts
affected_by_dropouts <- raw %>%
  filter(
    participant.affected_by_dropout == 1,
    participant.is_dropout == 0
  )
write_csv(affected_by_dropouts, "admin/pilot_affected_by_dropouts.csv")

# Select out participants who were not grouped
was_not_grouped <- raw %>% filter(participant.was_not_grouped == 1)
write_csv(was_not_grouped, "admin/pilot_was_not_grouped.csv")

# Keep participants who completed
dat <- raw %>% filter(!is.na(demographics.1.player.attentive))
write_csv(dat, "admin/pilot_completed.csv")

#### Data cleaning ####

cleaned_dat <- dat %>%
  select(
    prolific_id = participant.label,
    condition = participant.condition,
    group = many_mind_games.1.group.id_in_subsession,
    first_check_errors = participant.first_check_errors,
    second_check_errors = participant.second_check_errors,
    matches = participant.matches,
    match_points = participant.match_points,
    points = participant.points,
    points_after_cp = participant.points_after_cp,
    points_after_pp = participant.points_after_pp,
    final_point = participant.final_point,
    final_point_min_zero = participant.final_point_min_zero,
    d_score = participant.d_score,
    selected_for_bonus = participant.selected_for_bonus,
    guess_of_purpose = demographics.1.player.purpose
  ) %>%
  # Matches were stored as [x, y, z]; keep only x, y, z
  mutate(matches = str_sub(matches, 2, -2)) %>%
  separate(col = matches, into = c("first", "second", "third")) %>%
  mutate(across(c("first", "second", "third"), as.numeric)) %>%
  mutate(average_match = (first + second + third)/3)

# Bonus
bonus_data <- cleaned_dat %>%
  filter(selected_for_bonus == 1) %>%
  select(prolific_id, final_point_min_zero) %>%
  mutate(payment = final_point_min_zero/10) %>%
  select(prolific_id, payment)
