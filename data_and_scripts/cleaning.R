# Load packages
library(tidyverse)

# Load data
run_1 <- read_csv(file = "data/run_1_usable.csv", col_names = TRUE)
run_2 <- read_csv(file = "data/run_2_usable.csv", col_names = TRUE)
run_3 <- read_csv(file = "data/run_3_usable.csv", col_names = TRUE)
run_4 <- read_csv(file = "data/run_4_usable.csv", col_names = TRUE)
run_5 <- read_csv(file = "data/run_5_usable.csv", col_names = TRUE)

# Compare column names
which(colnames(run_1) != colnames(run_2))
which(colnames(run_1) != colnames(run_3))
which(colnames(run_1) != colnames(run_4))
which(colnames(run_1) != colnames(run_5))

# See column names
colnames(run_1)[c(45, 46, 47)]
colnames(run_2)[c(45, 46, 47)]
colnames(run_3)[c(45, 46, 47)]
colnames(run_4)[c(45, 46, 47)]
colnames(run_5)[c(45, 46, 47)]

# These columns are irrelevant; drop them
run_1 <- run_1[, -c(45, 46, 47)]
run_2 <- run_2[, -c(45, 46, 47)]
run_3 <- run_3[, -c(45, 46, 47)]
run_4 <- run_4[, -c(45, 46, 47)]
run_5 <- run_5[, -c(45, 46, 47)]

# Combine the data
my_data <- bind_rows(run_1, run_2, run_3, run_4, run_5)

#### Data cleaning ####

# Create a column indicating when the participant dropped out
my_data$dropout_round <- NA
for (i in 1:27) {
  dropout_indicator <- paste0("many_mind_games.", i, ".player.is_dropout")
  my_data$dropout_round[pull(my_data, dropout_indicator) == 1] <- i
}

# Keep useful columns, rename for conciseness
dat <- my_data %>%
  select(
    condition = participant.condition,
    run = run,
    group = many_mind_games.1.group.id_in_subsession,
    id_in_group = many_mind_games.1.player.id_in_group,
    time_started_utc = participant.time_started_utc,
    first_check_fail = participant.first_check_fail,
    first_check_errors = participant.first_check_errors,
    second_check_fail = participant.second_check_fail,
    second_check_errors = participant.second_check_errors,
    matches = participant.matches,
    match_points = participant.match_points,
    points = participant.points,
    whether_cp = participant.collectively_punished,
    points_after_cp = participant.points_after_cp,
    pp_sent = participant.peer_punishment_sent,
    pp_received = participant.peer_punishment_received,
    points_after_pp = participant.points_after_pp,
    final_point = participant.final_point,
    d_score = participant.d_score,
    is_dropout = participant.is_dropout,
    dropout_round,
    affected_by_dropout = participant.affected_by_dropout,
    selected_for_bonus = participant.selected_for_bonus,
    age = demographics.1.player.age,
    english = demographics.1.player.english,
    attentive = demographics.1.player.attentive,
    matches("^many_mind_games\\.([1-9]|[12][0-9])\\.player\\.whether_match$"),
    matches("^many_mind_games\\.(9|18|27)\\.group\\.total_match$"),
    matches("^many_mind_games\\.(9|18|27)\\.player\\.punish_p[1-8]$"),
    matches("^d_factor\\.1\\.player\\.d_([1-9]|[1][0-9])$"),
    gender = demographics.1.player.gender,
    place = demographics.1.player.place,
    error = demographics.1.player.any_error,
    guess_of_purpose = demographics.1.player.purpose
  ) %>%
  rename_with(
    ~paste0("whether_match_", as.numeric(str_extract(., "\\d+"))),
    .cols = matches("^many_mind_games\\.([1-9]|[12][0-9])\\.player\\.whether_match")
  ) %>%
  rename_with(
    ~paste0("group_total_match_", as.numeric(str_extract(., "\\d+"))/9),
    .cols = matches("^many_mind_games\\.(9|18|27)\\.group\\.total_match$")
  ) %>%
  rename_with(
    ~paste0(
      "punish_p",
      as.numeric(str_extract(., "[1-8]$")),
      "_",
      as.numeric(str_extract(., "\\d+"))/9
    ),
    .cols = matches("^many_mind_games\\.(9|18|27)\\.player\\.punish_p[1-8]$")
  ) %>%
  rename_with(
    ~paste0("d_item", as.numeric(str_extract(., "([1-9]|[1][0-9])$"))),
    .cols = matches("^d_factor\\.1\\.player\\.d_([1-9]|[1][0-9])$"),
  )

# Separate round data
cols_to_sep <- c(
  "matches",
  "match_points",
  "points",
  "whether_cp",
  "points_after_cp",
  "pp_sent",
  "pp_received",
  "points_after_pp"
)

dat <- dat %>%
  mutate(across(all_of(cols_to_sep), ~ str_sub(., 2, -2))) %>%
  separate(
    matches,
    into = c("match_1", "match_2", "match_3")
  ) %>%
  separate(
    match_points,
    into = c("match_points_1", "match_points_2", "match_points_3")
  ) %>%
  separate(
    points,
    into = c("points_1", "points_2", "points_3")
  ) %>%
  separate(
    whether_cp,
    into = c("whether_cp_1", "whether_cp_2", "whether_cp_3")
  ) %>%
  separate(
    points_after_cp,
    into = c("points_after_cp_1", "points_after_cp_2", "points_after_cp_3")
  ) %>%
  separate(
    pp_sent,
    into = c("pp_sent_1", "pp_sent_2", "pp_sent_3")
  ) %>%
  separate(
    pp_received,
    into = c("pp_received_1", "pp_received_2", "pp_received_3")
  ) %>%
  separate(
    points_after_pp,
    into = c("points_after_pp_1", "points_after_pp_2", "points_after_pp_3"),
    sep = ", "
  )

# Replace "True" with 1, "False" with 0, and "None" with NA
dat <- dat %>%
  mutate_at(
    vars(which(colnames(dat) == "match_1"):which(colnames(dat) == "attentive")),
    ~ case_when(
      . == "True" ~ "1",
      . == "False" ~ "0",
      . == "None" ~ NA_character_,
      TRUE ~ as.character(.)
    )
  ) %>%
  mutate(
    across(
      c(which(colnames(dat) == "match_1"):which(colnames(dat) == "attentive")),
      as.numeric
    )
  )

# Assign unique group identifiers
dat <- dat %>%
  group_by(run, group) %>%
  mutate(group_id = cur_group_id())

# Upon scrutiny of the data two groups were marked as having dropouts despite
# they all completed the study.
# This was due to a loophole in the program (see Supplement).
remark_grp_id <- unique(dat[dat$affected_by_dropout == 1 & !is.na(dat$attentive), ]$group_id)
dat[dat$group_id %in% remark_grp_id, ]$affected_by_dropout <- 0
dat[dat$group_id %in% remark_grp_id, ]$is_dropout <- 0
dat[dat$group_id %in% remark_grp_id, ]$dropout_round <- NA

# Sort based on Group ID and Subject ID in Group
dat <- dat %>% arrange(group_id, id_in_group)

# Calculate how many rounds groups/individuals have completed
# Distinguish between "game" completed and "round" completed
# for those conditions with peer punishment
# i.e., one only completes a "round" if they also complete the punishment
dat$n_games_done_grp <- NA
dat$n_round_done_grp <- NA
dat$n_games_done_ind <- NA
dat$n_round_done_ind <- NA

for (i in 1:length(unique(dat$group_id))) {
  if (sum(dat[dat$group_id == i, ]$is_dropout) < 1) {
    # No dropouts; they completed all rounds
    dat[dat$group_id == i, ]$n_games_done_grp <- 3
    dat[dat$group_id == i, ]$n_round_done_grp <- 3
    dat[dat$group_id == i, ]$n_games_done_ind <- 3
    dat[dat$group_id == i, ]$n_round_done_ind <- 3
  } else {
    # There is at least one dropout
    grp <- dat[dat$group_id == i, ]
    condi <- grp$condition[1]
    grp_games <- NA
    grp_round <- NA
    dropout_games <- NA
    dropout_round <- NA
    non_dro_games <- NA
    non_dro_round <- NA

    if (condi == "control" | condi == "cp_only") {
      if (sum(is.na(grp$whether_match_10)) > 0) {
        # Dropout happened in Round 1
        grp_games <- 0
        grp_round <- 0
        dropout_games <- 0
        dropout_round <- 0
        non_dro_games <- 1
        non_dro_round <- 1
      } else if (sum(is.na(grp$whether_match_19)) > 0) {
        # Dropout happened in Round 2
        grp_games <- 1
        grp_round <- 1
        dropout_games <- 1
        dropout_round <- 1
        non_dro_games <- 2
        non_dro_round <- 2
      } else {
        # Dropout happened in Round 3
        grp_games <- 2
        grp_round <- 2
        dropout_games <- 2
        dropout_round <- 2
        non_dro_games <- 3
        non_dro_round <- 3
      }
    } else if (condi == "cp_peer" | condi == "pp_only") {
      if (sum(is.na(grp$points_after_cp_1)) == 0 & sum(is.na(grp$points_after_pp_1)) > 0) {
        # Dropout happened in Round 1 Game
        grp_games <- 0
        grp_round <- 0
        dropout_games <- 0
        dropout_round <- 0
        non_dro_games <- 1
        non_dro_round <- 0
      } else if (sum(is.na(grp$points_after_pp_1)) == 0 & sum(is.na(grp$whether_match_10)) > 0) {
        # Dropout happened in Round 1 PP
        grp_games <- 1
        grp_round <- 0
        dropout_games <- 1
        dropout_round <- 0
        non_dro_games <- 1
        non_dro_round <- 1
      } else if (sum(is.na(grp$points_after_cp_2)) == 0 & sum(is.na(grp$points_after_pp_2)) > 0) {
        # Dropout happened in Round 2 Game
        grp_games <- 1
        grp_round <- 1
        dropout_games <- 1
        dropout_round <- 1
        non_dro_games <- 2
        non_dro_round <- 1
      } else if (sum(is.na(grp$points_after_pp_2)) == 0 & sum(is.na(grp$whether_match_19)) > 0) {
        # Dropout happened in Round 2 PP
        grp_games <- 2
        grp_round <- 1
        dropout_games <- 2
        dropout_round <- 1
        non_dro_games <- 2
        non_dro_round <- 2
      } else if (sum(is.na(grp$points_after_cp_3)) == 0 & sum(is.na(grp$points_after_pp_3)) > 0) {
        # Dropout happened in Round 3 Game
        grp_games <- 2
        grp_round <- 2
        dropout_games <- 2
        dropout_round <- 2
        non_dro_games <- 3
        non_dro_round <- 2
      } else if (sum(is.na(grp$points_after_pp_3)) == 0) {
        # Dropout happened in Round 3 PP
        grp_games <- 3
        grp_round <- 2
        dropout_games <- 3
        dropout_round <- 2
        non_dro_games <- 3
        non_dro_round <- 3
      }
    }

    dat[dat$group_id == i, ]$n_games_done_grp <- grp_games
    dat[dat$group_id == i, ]$n_round_done_grp <- grp_round
    dat[dat$group_id == i & dat$is_dropout == 0, ]$n_games_done_ind <- non_dro_games
    dat[dat$group_id == i & dat$is_dropout == 0, ]$n_round_done_ind <- non_dro_round
    dat[dat$group_id == i & dat$is_dropout == 1, ]$n_games_done_ind <- dropout_games
    dat[dat$group_id == i & dat$is_dropout == 1, ]$n_round_done_ind <- dropout_round
  }
}

# Add subject ID
dat$subject_id <- c(1:nrow(dat))

# Factorize conditions
dat <- dat %>% mutate(
  under_cp = case_when(
    condition == "cp_only" | condition == "cp_peer" ~ 1,
    condition == "control" | condition == "pp_only" ~ 0,
  ),
  under_pp = case_when(
    condition == "pp_only" | condition == "cp_peer" ~ 1,
    condition == "control" | condition == "cp_only" ~ 0,
  )
)

# Rearrange data
dat <- dat %>% select(
  subject_id, condition, under_cp, under_pp, run, group, group_id, id_in_group,
  n_games_done_grp, n_games_done_ind, n_round_done_grp, n_round_done_ind,
  everything()
)
write_csv(dat, file = "data/data_full_cleaned.csv", col_names = TRUE)
