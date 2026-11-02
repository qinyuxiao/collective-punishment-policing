# Load packages
library(tidyverse)

# Read raw data
my_raw_data <- read_csv("admin/run_1.csv")

#### For compensation on Prolific ####

# Due to technical error we paid all participants the basic participation fee

#### Keep usable data ####

usable <- my_raw_data %>%
  filter(!is.na(demographics.1.player.attentive)) %>%
  select(-c(
    session.condition_list,
    participant.label,
    introduction.1.player.prolific_id
  ))
usable$run <- 1
write_csv(usable, "run_1_usable.csv", col_names = TRUE)
