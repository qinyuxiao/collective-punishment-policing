# Calculate bonus payment descriptives

library(misty)
library(readr)
misty::setsource()

# Load data
# Note that these data sets are not shared because they contain Prolific IDs
r2 <- read_csv(file = "data/admin/run_2_bonus.csv", col_names = c("id", "bonus"))
r3 <- read_csv(file = "data/admin/run_3_bonus.csv", col_names = c("id", "bonus"))
r4 <- read_csv(file = "data/admin/run_4_bonus.csv", col_names = c("id", "bonus"))
r5 <- read_csv(file = "data/admin/run_5_bonus.csv", col_names = c("id", "bonus"))
bonus <- rbind(r2, r3, r4, r5)

# Descriptives of bonus payment
mean(bonus$bonus)
sd(bonus$bonus)
