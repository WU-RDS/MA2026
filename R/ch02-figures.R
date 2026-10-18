# ------------------------------------------------------------------
# ch02-figures.R
# Shared figure functions for Chapter 2 (book chapter AND slide deck).
# Concept figures only — every analysis of the music data is shown
# with visible code in the chapter itself.
# ------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# --- course-wide look (same values as in ch01-figures.R) ---------------

wu_blue   <- "#0f5da8"
wu_dark   <- "#0a3d6e"
wu_orange <- "#d99a2b"
wu_grey   <- "#58585a"
wu_red    <- "#b03a2e"
wu_green  <- "#1e8449"

theme_course <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = rel(1.0)),
      plot.subtitle = element_text(color = "grey30"),
      legend.position = "bottom"
    )
}

# --- 2.1 Anscombe's quartet ---------------------------------------------
# Built-in R data set 'anscombe' (Anscombe, 1973).

anscombe_long <- function() {
  data.frame(
    set = rep(paste("Data set", 1:4), each = nrow(anscombe)),
    x = c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4),
    y = c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
  )
}

anscombe_table <- function() {
  anscombe_long() |>
    group_by(set) |>
    summarise(
      `Mean of x` = round(mean(x), 2),
      `Mean of y` = round(mean(y), 2),
      `SD of x` = round(sd(x), 2),
      `SD of y` = round(sd(y), 2),
      `Correlation` = round(cor(x, y), 2),
      .groups = "drop"
    ) |>
    rename(` ` = set)
}

fig_anscombe <- function(base_size = 13) {
  ggplot(anscombe_long(), aes(x, y)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                color = wu_orange, linewidth = 0.9, fullrange = TRUE) +
    geom_point(color = wu_blue, size = 2.6) +
    facet_wrap(~set, nrow = 1) +
    coord_cartesian(xlim = c(3, 20), ylim = c(2, 14)) +
    labs(x = "x", y = "y") +
    theme_course(base_size = base_size)
}

# --- 2.5 Selection into the data set (Berkson's paradox) ------------------
# Simulated: catchiness and promotion are independent among ALL songs.
# A song enters the charts only if catchiness + promotion (+ luck) is high.
# Among charted songs, the two appear negatively related.

fig_selection_charts <- function(base_size = 13) {
  set.seed(2026)
  n <- 4000
  d <- data.frame(catchiness = rnorm(n), promotion = rnorm(n))
  d$success <- d$catchiness + d$promotion + rnorm(n, 0, 0.5)
  d$charted <- d$success > quantile(d$success, 0.85)

  r_all <- cor(d$catchiness, d$promotion)
  r_ch  <- with(d[d$charted, ], cor(catchiness, promotion))

  p1 <- ggplot(d, aes(catchiness, promotion)) +
    geom_point(aes(color = charted), alpha = 0.45, size = 1.2) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                color = wu_grey, linewidth = 0.9) +
    scale_color_manual(values = c(`FALSE` = "grey75", `TRUE` = wu_blue),
                       labels = c("not charted", "charted"), name = NULL) +
    labs(title = "All released songs",
         subtitle = sprintf("correlation = %.2f", r_all),
         x = "Catchiness", y = "Promotion budget") +
    theme_course(base_size = base_size)

  p2 <- ggplot(d[d$charted, ], aes(catchiness, promotion)) +
    geom_point(color = wu_blue, alpha = 0.5, size = 1.2) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                color = wu_red, linewidth = 0.9) +
    coord_cartesian(xlim = range(d$catchiness), ylim = range(d$promotion)) +
    labs(title = "Only songs that made the charts",
         subtitle = sprintf("correlation = %.2f", r_ch),
         x = "Catchiness", y = "Promotion budget") +
    theme_course(base_size = base_size)

  p1 + p2 + plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

# --- 2.7 Honest visualization: truncated axis -----------------------------
# Stylized numbers: market shares of two labels.

fig_truncated_axis <- function(base_size = 13) {
  d <- data.frame(label = c("Label A", "Label B"), share = c(31, 33))
  base <- ggplot(d, aes(label, share, fill = label)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = paste0(share, "%")), vjust = -0.4,
              fontface = "bold", size = base_size / 3) +
    scale_fill_manual(values = c(wu_grey, wu_blue)) +
    labs(x = NULL, y = "Market share (%)") +
    theme_course(base_size = base_size) +
    theme(legend.position = "none")

  p1 <- base + coord_cartesian(ylim = c(30.5, 33.4)) +
    labs(title = "Axis starts at 30.5%",
         subtitle = "Label B looks five times as large")
  p2 <- base + scale_y_continuous(limits = c(0, 38), expand = c(0, 0)) +
    labs(title = "Axis starts at 0%",
         subtitle = "The honest picture: a 2-point difference")
  p1 + p2
}
