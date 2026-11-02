# ============================================================================ #
# Analysis: Does collective punishment mobilize decentralized policing?
# ============================================================================ #
# Date     : 2026-10-20
# Author   : [Blinded for review]
# ORCID    : [Blinded for review]
# Contact  : [Blinded for review]
#
# ============================================================================ #
# Preprocessed data are stored in 'data/data_ready_for_analysis.RData'
# ============================================================================ #


# 1. Preparation ---------------------------------------------------------------

library(MBESS)
library(marginaleffects)
library(rstatix)
library(tidyverse)
library(lme4)
library(lmerTest)
library(cowplot)
library(parameters)
library(datawizard)
library(clubSandwich)
library(misty)
library(apaTables)
library(knitr)

# Please download the entire study directory and set it as the working directory

# Load data
load("data/data_ready_for_analysis.RData")

# 2. Analysis (main text) ------------------------------------------------------

## 2.1. Descriptives -----------------------------------------------------------

### 2.1.1. Demographics --------------------------------------------------------

# Age

dat.c |>
  summarise(
    n_age = sum(!is.na(age)),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE)
  ) |>
  knitr::kable(digits = 2)

# | n_age| min_age| max_age| mean_age| sd_age|
# |-----:|-------:|-------:|--------:|------:|
# |   351|      18|      85|    37.98|  13.53|

# Gender
dat.c |>
  count(gender) |>
  mutate(perc = round(n/sum(n) * 100, 2)) |>
  kable(digits = 2)

# |gender     |   n|  perc|
# |:----------|---:|-----:|
# |female     | 160| 45.45|
# |male       | 178| 50.57|
# |non-binary |  10|  2.84|
# |NA         |   4|  1.14|


### 2.1.2. Correlation matrices ------------------------------------------------

# Helper function
generate_cor_matrix <- function(cond) {
  correlation_matrix <- dat.c.p_match |>
    filter(condition == cond) |>
    select(match_1, match_2, match_3, d_score) |>
    apa.cor.table()
  kable(correlation_matrix$table.body)
}

generate_cor_matrix("Control")

# |   |Variable   |M    |SD   |1          |2           |3          |
# |:--|:----------|:----|:----|:----------|:-----------|:----------|
# |   |1. match_1 |3.31 |2.48 |           |            |           |
# |   |           |     |     |           |            |           |
# |   |2. match_2 |3.58 |2.11 |.78**      |            |           |
# |   |           |     |     |[.68, .85] |            |           |
# |   |           |     |     |           |            |           |
# |   |3. match_3 |3.56 |2.39 |.81**      |.66**       |           |
# |   |           |     |     |[.71, .87] |[.51, .77]  |           |
# |   |           |     |     |           |            |           |
# |   |4. d_score |1.94 |0.48 |.24*       |.18         |.25*       |
# |   |           |     |     |[.02, .44] |[-.04, .38] |[.03, .44] |
# |   |           |     |     |           |            |           |

generate_cor_matrix("CP-Only")

# |   |Variable   |M    |SD   |1          |2           |3          |
# |:--|:----------|:----|:----|:----------|:-----------|:----------|
# |   |1. match_1 |2.41 |1.55 |           |            |           |
# |   |           |     |     |           |            |           |
# |   |2. match_2 |2.45 |1.63 |.56**      |            |           |
# |   |           |     |     |[.39, .69] |            |           |
# |   |           |     |     |           |            |           |
# |   |3. match_3 |2.44 |1.67 |.48**      |.53**       |           |
# |   |           |     |     |[.29, .64] |[.35, .67]  |           |
# |   |           |     |     |           |            |           |
# |   |4. d_score |1.83 |0.53 |.33**      |.21         |.33**      |
# |   |           |     |     |[.12, .52] |[-.01, .41] |[.11, .51] |
# |   |           |     |     |           |            |           |

generate_cor_matrix("PP-Only")

# |   |Variable   |M    |SD   |1           |2           |3           |
# |:--|:----------|:----|:----|:-----------|:-----------|:-----------|
# |   |1. match_1 |3.38 |2.35 |            |            |            |
# |   |           |     |     |            |            |            |
# |   |2. match_2 |3.47 |2.27 |.82**       |            |            |
# |   |           |     |     |[.74, .87]  |            |            |
# |   |           |     |     |            |            |            |
# |   |3. match_3 |3.69 |2.41 |.71**       |.74**       |            |
# |   |           |     |     |[.60, .80]  |[.63, .81]  |            |
# |   |           |     |     |            |            |            |
# |   |4. d_score |1.92 |0.50 |.11         |.09         |.12         |
# |   |           |     |     |[-.08, .30] |[-.10, .28] |[-.08, .30] |
# |   |           |     |     |            |            |            |

generate_cor_matrix("CP-and-PP")

# |   |Variable   |M    |SD   |1          |2          |3          |
# |:--|:----------|:----|:----|:----------|:----------|:----------|
# |   |1. match_1 |2.42 |1.57 |           |           |           |
# |   |           |     |     |           |           |           |
# |   |2. match_2 |2.32 |1.48 |.41**      |           |           |
# |   |           |     |     |[.22, .57] |           |           |
# |   |           |     |     |           |           |           |
# |   |3. match_3 |2.59 |1.83 |.44**      |.58**      |           |
# |   |           |     |     |[.25, .59] |[.42, .70] |           |
# |   |           |     |     |           |           |           |
# |   |4. d_score |1.96 |0.63 |.26*       |.27*       |.35**      |
# |   |           |     |     |[.05, .45] |[.06, .45] |[.16, .53] |
# |   |           |     |     |           |           |           |


### 2.1.3. Group-level match reports -------------------------------------------

dat.c.g_match |>
  group_by(condition) |>
  summarise(
    n = n(),
    across(
      c(match_1, match_2, match_3, match_avg),
      list(m = ~mean(.), sd = ~sd(.))
    )
  ) |>
  kable(digits = 2)

# |condition |  n| match_1_m| match_1_sd| match_2_m| match_2_sd| match_3_m| match_3_sd| match_avg_m| match_avg_sd|
# |:---------|--:|---------:|----------:|---------:|----------:|---------:|----------:|-----------:|------------:|
# |Control   | 10|     26.50|       7.01|     28.60|       7.69|     28.50|       7.14|       27.87|         6.81|
# |CP-Only   | 10|     19.30|       3.13|     19.60|       4.81|     19.50|       5.56|       19.47|         3.52|
# |PP-Only   | 13|     27.08|       6.44|     27.77|       7.21|     29.54|       8.35|       28.13|         7.00|
# |CP-and-PP | 11|     19.36|       2.25|     18.55|       4.87|     20.73|       5.97|       19.55|         3.15|


### 2.1.4. Game-level match reports --------------------------------------------

dat.c.p_match.l |>
  group_by(condition) |>
  summarise(
    m = sum(match),
    N = n() * 9,
    perc = m / N
  ) |>
  kable(digits = 4)

# |condition |    m|    N|   perc|
# |:---------|----:|----:|------:|
# |Control   |  836| 2160| 0.3870|
# |CP-Only   |  584| 2160| 0.2704|
# |PP-Only   | 1097| 2808| 0.3907|
# |CP-and-PP |  645| 2376| 0.2715|


## 2.2. Dishonesty by condition ------------------------------------------------

# Round effect on group-level match reports
dat.c.g_match.l |>
  group_by(condition) |>
  friedman_test(match ~ round_number|group_id) |>
  kable(digits = 4)

# |condition |.y.   |  n| statistic| df|      p|method        |
# |:---------|:-----|--:|---------:|--:|------:|:-------------|
# |Control   |match | 10|    1.4737|  2| 0.4786|Friedman test |
# |CP-Only   |match | 10|    0.0541|  2| 0.9733|Friedman test |
# |PP-Only   |match | 13|    3.6400|  2| 0.1620|Friedman test |
# |CP-and-PP |match | 11|    0.1818|  2| 0.9131|Friedman test |

dat.c.g_match |>
  group_by(condition) |>
  wilcox_test(match_avg ~ 1, mu = 14.4) |>
  kable()

# |condition |.y.       |group1 |group2     |  n| statistic|       p|
# |:---------|:---------|:------|:----------|--:|---------:|-------:|
# |Control   |match_avg |1      |null model | 10|        55| 0.00589|
# |CP-Only   |match_avg |1      |null model | 10|        55| 0.00589|
# |PP-Only   |match_avg |1      |null model | 13|        91| 0.00165|
# |CP-and-PP |match_avg |1      |null model | 11|        66| 0.00384|


## 2.3. Compare conditions -----------------------------------------------------

# Note that the `statistic` reported by the function, which under the hood uses
# wilcox.test(), is actually U, but not W

dat.c.g_match |>
  mutate(condition = factor(
    condition,
    levels = c("CP-and-PP", "CP-Only", "PP-Only", "Control")
  )) |>
  pairwise_wilcox_test(
    match_avg ~ condition,
    comparisons = list(
      c("CP-Only", "Control"),
      c("CP-and-PP", "Control"),
      c("CP-and-PP", "CP-Only"),
      c("PP-Only", "Control")
    ),
    detailed = TRUE,
    paired = FALSE
  ) |>
  select(-c(".y.", "method", "alternative")) |>
  kable(digits = 4)

# | estimate|group1    |group2  | n1| n2| statistic|     p| conf.low| conf.high| p.adj|p.adj.signif |
# |--------:|:---------|:-------|--:|--:|---------:|-----:|--------:|---------:|-----:|:------------|
# | -10.0000|CP-Only   |Control | 10| 10|      17.0| 0.014| -13.6667|   -2.6667| 0.042|*            |
# |  -9.6666|CP-and-PP |Control | 11| 10|      17.0| 0.008| -13.9999|   -3.6667| 0.033|*            |
# |   0.3333|CP-and-PP |CP-Only | 11| 10|      59.0| 0.805|  -3.3333|    3.3333| 1.000|ns           |
# |  -1.3333|PP-Only   |Control | 13| 10|      56.5| 0.619|  -5.3333|    8.3333| 1.000|ns           |


## 2.4. Compare peer punishment between PP-ONLY and CP-AND-PP ------------------

### 2.4.1. Round effects -------------------------------------------------------

# Round effect on group-level punishment instances
dat.c.g_punishment |>
  group_by(condition) |>
  friedman_test(num_p ~ round_number|group_id) |>
  kable(digits = 4)

# |condition |.y.   |  n| statistic| df|      p|method        |
# |:---------|:-----|--:|---------:|--:|------:|:-------------|
# |PP-Only   |num_p | 13|    1.5500|  2| 0.4607|Friedman test |
# |CP-and-PP |num_p | 11|    0.1579|  2| 0.9241|Friedman test |


# Round effect on group-level punishment size
dat.c.g_punishment |>
  group_by(condition) |>
  friedman_test(sum_p ~ round_number|group_id) |>
  kable(digits = 4)

# |condition |.y.   |  n| statistic| df|      p|method        |
# |:---------|:-----|--:|---------:|--:|------:|:-------------|
# |PP-Only   |sum_p | 13|    8.3590|  2| 0.0153|Friedman test |
# |CP-and-PP |sum_p | 11|    0.6486|  2| 0.7230|Friedman test |


# Pairwise Wilcoxon among rounds on punishment size
dat.c.g_punishment |>
  # Rearrange to ensure pairing is right
  arrange(condition, group_id, round_number) |>
  group_by(condition) |>
  pairwise_wilcox_test(
    sum_p ~ round_number,
    p.adjust.method = "bonferroni",
    paired = TRUE
  ) |>
  kable(digits = 4)

# |condition |.y.   |group1 |group2 | n1| n2| statistic|     p| p.adj|p.adj.signif |
# |:---------|:-----|:------|:------|--:|--:|---------:|-----:|-----:|:------------|
# |PP-Only   |sum_p |1      |2      | 13| 13|       4.0| 0.032| 0.097|ns           |
# |PP-Only   |sum_p |1      |3      | 13| 13|       0.0| 0.009| 0.027|*            |
# |PP-Only   |sum_p |2      |3      | 13| 13|      19.0| 0.413| 1.000|ns           |
# |CP-and-PP |sum_p |1      |2      | 11| 11|      17.5| 0.328| 0.984|ns           |
# |CP-and-PP |sum_p |1      |3      | 11| 11|      16.0| 0.260| 0.780|ns           |
# |CP-and-PP |sum_p |2      |3      | 11| 11|      10.0| 1.000| 1.000|ns           |


### 2.4.2. By-round comparisons ------------------------------------------------

# On punishment instances
dat.c.g_punishment |>
  mutate(condition = factor(condition, levels = c("CP-and-PP", "PP-Only"))) |>
  group_by(round_number) |>
  wilcox_test(
    num_p ~ condition,
    paired = FALSE,
    detailed = TRUE
  ) |>
  select(-c(".y.", "method", "alternative")) |>
  kable(digits = 4)

# |round_number | estimate|group1    |group2  | n1| n2| statistic|     p| conf.low| conf.high|
# |:------------|--------:|:---------|:-------|--:|--:|---------:|-----:|--------:|---------:|
# |1            |  -0.9999|CP-and-PP |PP-Only | 11| 13|      62.5| 0.619|       -3|         2|
# |2            |  -1.0000|CP-and-PP |PP-Only | 11| 13|      57.0| 0.412|       -4|         2|
# |3            |  -2.0000|CP-and-PP |PP-Only | 11| 13|      47.0| 0.160|       -5|         1|


# On punishment size
dat.c.g_punishment |>
  mutate(condition = factor(condition, levels = c("CP-and-PP", "PP-Only"))) |>
  group_by(round_number) |>
  wilcox_test(
    sum_p ~ condition,
    paired = FALSE,
    detailed = TRUE
  ) |>
  select(-c(".y.", "method", "alternative")) |>
  kable(digits = 4)

# |round_number | estimate|group1    |group2  | n1| n2| statistic|     p| conf.low| conf.high|
# |:------------|--------:|:---------|:-------|--:|--:|---------:|-----:|--------:|---------:|
# |1            |       -5|CP-and-PP |PP-Only | 11| 13|      57.5| 0.432| -20.0001|        10|
# |2            |       -5|CP-and-PP |PP-Only | 11| 13|      60.5| 0.538| -35.0000|        15|
# |3            |       -5|CP-and-PP |PP-Only | 11| 13|      60.5| 0.540| -34.9999|        15|


# 2.5. Does receiving collective punishment lead to more peer punishment -------

# Prepare data
dat.c.g_punishment_cp_and_pp <- dat.c.g_punishment |>
  filter(under_cp == 1) |>
  mutate(group_match_t = group_match - 20) # Centering group match to threshold

# Frequency
mod.c.g_num_p <- lm(
  num_p ~ whether_cp * group_match_t,
  data = dat.c.g_punishment_cp_and_pp
)
model_parameters(
  mod.c.g_num_p,
  vcov = "CR2",
  vcov_args = list(cluster = dat.c.g_punishment_cp_and_pp$group_id)
)

# Parameter                  | Coefficient |   SE |        95% CI | t(29) |     p
# -------------------------------------------------------------------------------
# (Intercept)                |        2.77 | 0.90 | [ 0.93, 4.61] |  3.07 | 0.005
# whether cp                 |        2.81 | 1.74 | [-0.75, 6.37] |  1.61 | 0.118
# group match t              |        0.25 | 0.17 | [-0.09, 0.59] |  1.48 | 0.149
# whether cp × group match t |       -0.18 | 0.18 | [-0.56, 0.20] | -0.98 | 0.336

coef_test(
  mod.c.g_num_p,
  vcov = "CR2",
  cluster = dat.c.g_punishment_cp_and_pp$group_id,
  test = "Satterthwaite"
)

#                    Coef. Estimate    SE Null value t-stat d.f. (Satt) p-val (Satt) Sig.
#              (Intercept)    2.770 0.902          0  3.072        7.12       0.0176    *
#               whether_cp    2.809 1.741          0  1.613        8.38       0.1437
#            group_match_t    0.248 0.167          0  1.484        4.20       0.2087
# whether_cp:group_match_t   -0.180 0.184          0 -0.978        5.18       0.3715

conf_int(
  mod.c.g_num_p,
  vcov = "CR2",
  cluster = dat.c.g_punishment_cp_and_pp$group_id,
  test = "Satterthwaite",
  level = .95
)

#                    Coef. Estimate    SE d.f. Lower 95% CI Upper 95% CI
#              (Intercept)    2.770 0.902 7.12        0.645        4.895
#               whether_cp    2.809 1.741 8.38       -1.175        6.792
#            group_match_t    0.248 0.167 4.20       -0.207        0.703
# whether_cp:group_match_t   -0.180 0.184 5.18       -0.648        0.288

# Severity
mod.c.g_sum_p <- lm(
  sum_p ~ whether_cp * group_match_t,
  data = dat.c.g_punishment_cp_and_pp
)
model_parameters(
  mod.c.g_sum_p,
  vcov = "CR2",
  vcov_args = list(cluster = dat.c.g_punishment_cp_and_pp$group_id)
)

# Parameter                  | Coefficient |    SE |          95% CI | t(29) |     p
# ----------------------------------------------------------------------------------
# (Intercept)                |       22.23 |  7.63 | [  6.62, 37.85] |  2.91 | 0.007
# whether cp                 |       13.77 | 11.88 | [-10.53, 38.06] |  1.16 | 0.256
# group match t              |        2.11 |  1.20 | [ -0.35,  4.57] |  1.76 | 0.089
# whether cp × group match t |       -0.28 |  1.86 | [ -4.07,  3.52] | -0.15 | 0.883

coef_test(
  mod.c.g_sum_p,
  vcov = "CR2",
  cluster = dat.c.g_punishment_cp_and_pp$group_id,
  test = "Satterthwaite"
)

#                    Coef. Estimate    SE Null value t-stat d.f. (Satt) p-val (Satt) Sig.
#              (Intercept)   22.235  7.63          0  2.913        7.12       0.0222    *
#               whether_cp   13.766 11.88          0  1.159        8.38       0.2785
#            group_match_t    2.113  1.20          0  1.758        4.20       0.1501
# whether_cp:group_match_t   -0.276  1.86          0 -0.149        5.18       0.8873

conf_int(
  mod.c.g_sum_p,
  vcov = "CR2",
  cluster = dat.c.g_punishment_cp_and_pp$group_id,
  test = "Satterthwaite",
  level = .95
)

#                    Coef. Estimate    SE d.f. Lower 95% CI Upper 95% CI
#              (Intercept)   22.235  7.63 7.12         4.24        40.22
#               whether_cp   13.766 11.88 8.38       -13.41        40.94
#            group_match_t    2.113  1.20 4.20        -1.16         5.39
# whether_cp:group_match_t   -0.276  1.86 5.18        -5.00         4.45


# 3. Analysis (supplementary materials) ----------------------------------------

## 3.1. Preregistered H5 -------------------------------------------------------

### 3.1.1. Behavioral change following collective punishment -------------------

df <- dat.c |>
  filter(condition == "CP-Only") |>
  mutate(
    md_1 = match_2 - match_1,
    md_2 = match_3 - match_2
  ) |>
  select(group_id, subject_id, md_1, md_2, match_1, match_2) |>
  pivot_longer(
    cols = c("md_1", "md_2", "match_1", "match_2"),
    names_to = c("variable", "round_number"),
    names_sep = "_",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = "variable",
    values_from = "value"
  ) |>
  rename(i_match = match) |>
  left_join(dat.c.g_match.l, by = c("group_id", "round_number")) |>
  rename(g_match = match) |>
  mutate(
    whether_cp = g_match >= 20,
    round_number = as.factor(round_number),
    # Mean peer match reports
    p_mean_match = (g_match - i_match) / 7
  )

# Fit model
mod <- lm(
  md ~ whether_cp + round_number + i_match + p_mean_match,
  data = df,
  contrasts = list(round_number = contr.deviation)
)
coef_test(
  mod,
  vcov = "CR2",
  cluster = df$group_id,
  test = "Satterthwaite"
)

#          Coef. Estimate    SE Null value  t-stat d.f. (Satt) p-val (Satt) Sig.
#    (Intercept)   2.2404 0.634          0  3.5340        1.82       0.0821    .
# whether_cpTRUE   0.6604 0.417          0  1.5848        4.27       0.1837
#  round_number2  -0.0116 0.253          0 -0.0457        8.21       0.9646
#        i_match  -0.5072 0.145          0 -3.5082        6.07       0.0125    *
#   p_mean_match  -0.5178 0.273          0 -1.8995        2.20       0.1862


### 3.1.2. Behavioral change following peer punishment -------------------------

df_any_p <- dat.c.p_punishment |>
  filter(round_number %in% c(1, 2)) |>
  group_by(condition, group_id, punished_id, round_number) |>
  summarise(
    sum_p = sum(punishment_size),
    g_match = unique(group_match_curr)[1],
    # match_change = match_next - match_curr (see L487 in Preparation.qmd)
    match_change = unique(punished_match_change)[1],
    i_match = unique(punished_match_curr)[1]
  ) |>
  mutate(
    whether_punished = sum_p > 0,
    round_number = factor(round_number, levels = c(1, 2)),
    p_mean_match = (g_match - i_match) / 7
  ) |>
  ungroup()
df_sum_p <- df_any_p |> filter(whether_punished == 1)

mod_match_any_p <- lm(
  match_change ~ whether_punished * condition + i_match + p_mean_match + round_number,
  data = df_any_p,
  contrasts = list(condition = contr.deviation, round_number = contr.deviation)
)
coef_test(
  mod_match_any_p,
  vcov = "CR2",
  cluster = df_any_p$group_id,
  test = "Satterthwaite"
)

#                                   Coef. Estimate     SE Null value  t-stat d.f. (Satt) p-val (Satt) Sig.
#                             (Intercept)  0.56574 0.4613          0  1.2263        7.09        0.259
#                    whether_punishedTRUE  0.00943 0.2170          0  0.0434       18.50        0.966
#                      conditionCP-and-PP -0.12502 0.2082          0 -0.6005       13.35        0.558
#                                 i_match -0.28210 0.0594          0 -4.7507       11.08       <0.001  ***
#                            p_mean_match  0.12400 0.1606          0  0.7720        7.16        0.465
#                           round_number2  0.24198 0.2056          0  1.1770       22.96        0.251
# whether_punishedTRUE:conditionCP-and-PP -0.36611 0.3667          0 -0.9985       17.72        0.331

mod_match_sum_p <- lm(
  match_change ~ sum_p * condition + i_match + p_mean_match + round_number,
  data = df_sum_p,
  contrasts = list(condition = contr.deviation, round_number = contr.deviation)
)
coef_test(
  mod_match_sum_p,
  vcov = "CR2",
  cluster = df_sum_p$group_id,
  test = "Satterthwaite"
)

#                    Coef. Estimate     SE Null value t-stat d.f. (Satt) p-val (Satt) Sig.
#              (Intercept)   1.1909 0.8526          0  1.397        5.23       0.2188
#                    sum_p  -0.0118 0.0157          0 -0.755       10.75       0.4666
#       conditionCP-and-PP  -0.9588 0.7120          0 -1.347       13.88       0.1997
#                  i_match  -0.1053 0.0751          0 -1.402        9.62       0.1923
#             p_mean_match  -0.3229 0.2465          0 -1.310        5.58       0.2415
#            round_number2   0.7945 0.3780          0  2.102       15.16       0.0527    .
# sum_p:conditionCP-and-PP   0.0208 0.0301          0  0.689        7.44       0.5115


## 3.2. Preregistered H6 -------------------------------------------------------

### 3.2.1. D descriptives ------------------------------------------------------

dat.d_items <- dat.c |>
  select(starts_with("d_item")) |>
  mutate(
    d_item1 = 6 - d_item1,
    d_item3 = 6 - d_item3,
    d_item5 = 6 - d_item5,
    d_item7 = 6 - d_item7,
    d_item9 = 6 - d_item9,
    d_item13 = 6 - d_item13,
    d_item14 = 6 - d_item14,
    d_item16 = 6 - d_item16
  )

d_items <- paste0("d_item", c(1:16))

MBESS::ci.reliability(data = dat.d_items[d_items], type = "alpha")$est # 0.8699
MBESS::ci.reliability(data = dat.d_items[d_items], type = "omega")$est # 0.8713

mean(dat.c$d_score) # 1.9154
sd(dat.c$d_score)   # 0.5368

### 3.2.2. Predicting match reports --------------------------------------------

df <- dat.c.p_match |>
  mutate(
    m_avg = (match_1 + match_2 + match_3)/3,
    d_score.z = as.vector(scale(d_score))
  )

mod <- lmer(m_avg ~ d_score.z * condition + (1|group_id), data = df)
model_parameters(mod, ci_method = "satterthwaite")

# Fixed Effects
#
# Parameter                         | Coefficient |   SE |         95% CI |     t |     df |      p
# -------------------------------------------------------------------------------------------------
# (Intercept)                       |        3.46 | 0.21 | [ 3.04,  3.88] | 16.64 |  39.73 | < .001
# d score z                         |        0.58 | 0.22 | [ 0.14,  1.01] |  2.60 | 342.91 | 0.010
# condition [CP-Only]               |       -0.95 | 0.30 | [-1.54, -0.35] | -3.20 |  40.53 | 0.003
# condition [PP-Only]               |        0.06 | 0.28 | [-0.50,  0.61] |  0.20 |  39.66 | 0.841
# condition [CP-and-PP]             |       -1.05 | 0.29 | [-1.63, -0.47] | -3.66 |  39.82 | < .001
# d score z × condition [CP-Only]   |       -0.11 | 0.30 | [-0.70,  0.47] | -0.38 | 342.50 | 0.706
# d score z × condition [PP-Only]   |       -0.30 | 0.29 | [-0.88,  0.27] | -1.05 | 343.44 | 0.295
# d score z × condition [CP-and-PP] |       -0.16 | 0.27 | [-0.70,  0.38] | -0.59 | 343.19 | 0.553
#
# Random Effects
#
# Parameter                | Coefficient
# --------------------------------------
# SD (Intercept: group_id) |        0.23
# SD (Residual)            |        1.74


# Compute average individual-level marginal effect of D
marginaleffects::avg_slopes(mod, variables = "d_score.z", re.form = NA) |>
  kable(digits = 4)

# |term      |contrast | estimate| std.error| statistic| p.value| s.value| conf.low| conf.high|
# |:---------|:--------|--------:|---------:|---------:|-------:|-------:|--------:|---------:|
# |d_score.z |dY/dX    |   0.4213|    0.0961|    4.3859|       0| 16.4014|    0.233|    0.6096|
