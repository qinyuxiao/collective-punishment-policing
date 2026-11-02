# Comparing peer punishment between PP-ONLY and CP-AND-PP

# ---- Total punishment instances ----

panel_num <- dat.c.g_punishment |>
  mutate(
    condition = factor(
      condition,
      levels = c("PP-Only", "CP-and-PP"),
      labels = c("PP-ONLY", "CP-AND-PP")
    ),
    round_number = factor(
      round_number,
      levels = c(1:3),
      labels = c("First", "Second", "Third")
    )
  ) |>
  ggplot(aes(x = condition, y = num_p, color = round_number)) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    width = 0.5,
    alpha = 0.35,
    show.legend = FALSE
  ) +
  geom_point(
    shape = 16,
    position = position_jitterdodge(dodge.width = .7, jitter.width = .2, seed = 2024),
    alpha = .4,
    size = 2
  ) +
  scale_y_continuous(
    breaks = seq(0, 14, 2),
    limits = c(0, 14)
  ) +
  scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
  labs(x = "", y = "", color = "Round", title = "Group-level punishment frequency") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(8, units = "pt"),
    plot.title = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    legend.position = "none",
    plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), units = "in")
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1)),
    y = guide_axis(cap = "both")
  )

# ---- Total punishment size ----

panel_size <- dat.c.g_punishment |>
  mutate(
    condition = factor(
      condition,
      levels = c("PP-Only", "CP-and-PP"),
      labels = c("PP-ONLY", "CP-AND-PP")
    ),
    round_number = factor(
      round_number,
      levels = c(1:3),
      labels = c("First", "Second", "Third")
    )
  ) |>
  ggplot(aes(x = condition, y = sum_p, color = round_number)) +
  geom_boxplot(
    position = position_dodge(width = 0.7),
    width = 0.5,
    alpha = 0.35,
    show.legend = FALSE
  ) +
  geom_point(
    shape = 16,
    position = position_jitterdodge(dodge.width = .7, jitter.width = .2, seed = 2024),
    alpha = .4,
    size = 2
  ) +
  scale_y_continuous(
    breaks = seq(0, 90, 10),
    limits = c(0, 90)
  ) +
  scale_color_manual(values = c("#0072B2", "#E69F00", "#009E73")) +
  labs(x = "", y = "", color = "Round", title = "Group-level punishment severity") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(8, units = "pt"),
    plot.title = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    legend.position = "right",
    plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), units = "in")
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1)),
    y = guide_axis(cap = "both")
  )

# ---- Combine ----

my_legend <- cowplot::get_legend(panel_size)
fig <- plot_grid(
  panel_num, panel_size + theme(legend.position = "none"), my_legend,
  nrow = 1, labels = c("A", "B"), rel_widths = c(10, 10, 3)
)

# ggsave(
#   filename = "figures/punishment_behaviors.png",
#   plot = fig,
#   width = 8, height = 4, units = "in", dpi = "retina", bg = "white"
# )
