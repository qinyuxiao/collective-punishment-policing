# Individual- and group-level match reports

# ---- Panel A ----
# Prepare data
dat.c.p_match.l_summ <- dat.c.p_match.l |>
  mutate(
    round_number = factor(
      round_number, levels = c(1:3), labels = c("First", "Second", "Third")
    ),
    condition = factor(
      condition,
      levels = c("Control", "CP-Only", "PP-Only", "CP-and-PP"),
      labels = c("CONTROL", "CP-ONLY", "PP-ONLY", "CP-AND-PP"),
    )
  ) |>
  group_by(condition, round_number) |>
  summarise(
    n = n(),
    mean = mean(match),
    sd = sd(match),
    se = sd / sqrt(n),
    cil = mean - qt(.975, df = n - 1) * se,
    ciu = mean + qt(.975, df = n - 1) * se,
  )

panel_a <- dat.c.p_match.l |>
  mutate(
    round_number = factor(
      round_number, levels = c(1:3), labels = c("First", "Second", "Third")
    ),
    condition = factor(
      condition,
      levels = c("Control", "CP-Only", "PP-Only", "CP-and-PP"),
      labels = c("CONTROL", "CP-ONLY", "PP-ONLY", "CP-AND-PP"),
    )
  ) |>
  ggplot(aes(x = condition, y = match, color = round_number)) +
  geom_violin(
    aes(fill = round_number),
    position = position_dodge(width = .7),
    width = .65,
    linewidth = 0,
    alpha = .4
  ) +
  geom_point(
    data = dat.c.p_match.l_summ,
    aes(x = condition, y = mean, shape = round_number),
    size = 1.8,
    position = position_dodge(width = .7)
  ) +
  geom_errorbar(
    data = dat.c.p_match.l_summ,
    aes(x = condition, y = mean, ymin = cil, ymax = ciu),
    width = 0.2,
    position = position_dodge(width = .7)
  ) +
  geom_hline(yintercept = 1.8, color = "black", alpha = .4, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 9, 1), expand = c(0, 0)) +
  scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
  scale_fill_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
  labs(x = "", y = "", title = "Individual-level match reports") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(8, units = "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 15)),
    legend.position = "none",
    plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), units = "in")
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

# ---- Panel B ----
panel_b <- dat.c.g_match.l |>
  mutate(
    round_number = factor(
      round_number, levels = c(1:3), labels = c("First", "Second", "Third")
    ),
    condition = factor(
      condition,
      levels = c("Control", "CP-Only", "PP-Only", "CP-and-PP"),
      labels = c("CONTROL", "CP-ONLY", "PP-ONLY", "CP-AND-PP"),
    )
  ) |>
  ggplot(aes(x = condition, y = match, color = round_number, group = round_number)) +
  geom_boxplot(
    aes(group = interaction(condition, round_number)),
    position = position_dodge(width = .7),
    width = .4,
    fill = NA,
    outliers = FALSE,
    show.legend = FALSE
  ) +
  geom_point(
    aes(shape = round_number),
    position = position_jitterdodge(dodge.width = .7, jitter.width = .3, seed = 2024),
    alpha = .4,
    size = 2
  ) +
  geom_hline(yintercept = 20, color = "black", alpha = .4, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 55, 5), expand = c(0, 0), limits = c(5, 50)) +
  scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
  labs(
    x = "", y = "", color = "Round", shape = "Round",
    title = "Group-level match reports"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(8, units = "pt"),
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 15)),
    legend.position = "bottom",
    plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), units = "in")
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1), order = 1),
    shape = guide_legend(order = 1)
  )

# ---- Combine ----
fig <- plot_grid(
  panel_a, panel_b, nrow = 2, ncol = 1, labels = "AUTO", rel_heights = c(1, 1.2)
)

# ggsave(
#   filename = "figures/mind_game_behaviors.png",
#   plot = fig,
#   width = 6, height = 7,
#   units = "in", dpi = "retina"
# )
