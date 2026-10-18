# ------------------------------------------------------------------
# ch06-figures.R
# Shared figure functions for Chapter 6 (book chapter AND slide deck).
# Data: data/smartmix_wave2.csv, data/ad_designs.csv, data/promo_campaigns.csv
# (all synthetic; see R/ch06-make-data.R).
# ------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

wu_blue   <- "#0f5da8"
wu_dark   <- "#0a3d6e"
wu_orange <- "#d99a2b"
wu_grey   <- "#58585a"
wu_red    <- "#b03a2e"
wu_green  <- "#1e8449"
wu_light  <- "#dbe7f3"

theme_course <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = rel(1.0)),
          plot.subtitle = element_text(color = "grey30"),
          legend.position = "bottom")
}

data_dir <- if (exists("data_dir")) data_dir else "data"
load_wave2 <- function() readr::read_csv(file.path(data_dir, "smartmix_wave2.csv"), show_col_types = FALSE) |>
  mutate(device = factor(device, levels = c("iOS", "Android", "Desktop")),
         group = factor(smart_mix, levels = c(0, 1), labels = c("Control", "Smart Mix")),
         plan = factor(premium, levels = c(0, 1), labels = c("Free", "Premium")))
load_ads <- function() readr::read_csv(file.path(data_dir, "ad_designs.csv"), show_col_types = FALSE) |>
  mutate(source = factor(source, levels = c("Human", "AI", "Human + AI")),
         disclosure = factor(disclosure, levels = c("Revealed", "Not revealed")))
load_promo <- function() readr::read_csv(file.path(data_dir, "promo_campaigns.csv"), show_col_types = FALSE) |>
  mutate(genre = factor(genre, levels = c("Pop", "HipHop/Rap", "Rock", "Electro/Dance")))

# --- 6.1 / 6.2: three kinds of interaction (stylized) -----------------------
fig_interaction_types <- function(base_size = 12) {
  x <- seq(0, 10, length.out = 50)
  d <- bind_rows(
    data.frame(type = "No interaction: parallel lines", x, A = 2 + 0.8 * x, B = 5 + 0.8 * x),
    data.frame(type = "Interaction: the slope differs by group", x, A = 2 + 0.4 * x, B = 3 + 1.2 * x),
    data.frame(type = "Crossover: the effect reverses", x, A = 2 + 1.0 * x, B = 9 - 0.4 * x)
  ) |> pivot_longer(c(A, B), names_to = "group", values_to = "y")
  d$type <- factor(d$type, levels = unique(d$type))
  ggplot(d, aes(x, y, color = group)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c(A = wu_grey, B = wu_blue), name = "Group") +
    facet_wrap(~type) +
    labs(x = "x", y = "y") +
    theme_course(base_size) + theme(axis.text = element_blank())
}

# --- 6.3: the effect of Smart Mix by plan (categorical moderator) -----------
fig_effect_by_plan <- function(d = load_wave2(), base_size = 13) {
  m <- lm(hours ~ smart_mix * premium + prior_hours + tenure, data = d)
  b <- coef(m); V <- vcov(m)
  eff <- data.frame(
    plan = factor(c("Free users", "Premium users"), levels = c("Free users", "Premium users")),
    est = c(b["smart_mix"], b["smart_mix"] + b["smart_mix:premium"]),
    se = c(sqrt(V["smart_mix", "smart_mix"]),
           sqrt(V["smart_mix", "smart_mix"] + V["smart_mix:premium", "smart_mix:premium"] + 2 * V["smart_mix", "smart_mix:premium"]))
  ) |> mutate(lo = est - 1.96 * se, hi = est + 1.96 * se)
  ggplot(eff, aes(plan, est)) +
    geom_hline(yintercept = 0, color = wu_red, linetype = "dashed") +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, color = wu_blue, linewidth = 1) +
    geom_point(size = 4, color = wu_dark) +
    geom_text(aes(label = sprintf("%.1f hours", est)), nudge_x = 0.28, size = base_size / 3.3) +
    coord_cartesian(ylim = c(-0.5, 7)) +
    labs(x = NULL, y = "Effect of Smart Mix on listening hours (95% CI)") +
    theme_course(base_size)
}

fig_lines_by_plan <- function(d = load_wave2(), base_size = 13) {
  m <- d |> group_by(plan, group) |> summarise(hours = mean(hours), .groups = "drop")
  ggplot(m, aes(plan, hours, color = group, group = group)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3.5) +
    geom_text(aes(label = sprintf("%.1f", hours)), vjust = -1, size = base_size / 3.4, show.legend = FALSE) +
    scale_color_manual(values = c(Control = wu_grey, `Smart Mix` = wu_blue), name = NULL) +
    coord_cartesian(ylim = c(12, 24)) +
    labs(x = NULL, y = "Mean listening hours", subtitle = "Raw means by group and plan") +
    theme_course(base_size)
}

# --- 6.3: the effect of Smart Mix across prior listening (continuous moderator)
fig_marginal_prior <- function(d = load_wave2(), base_size = 13) {
  m <- lm(hours ~ smart_mix * prior_hours + premium + tenure, data = d)
  b <- coef(m); V <- vcov(m)
  grid <- data.frame(prior_hours = seq(3, 45, length.out = 100))
  grid$est <- b["smart_mix"] + b["smart_mix:prior_hours"] * grid$prior_hours
  grid$se  <- sqrt(V["smart_mix", "smart_mix"] + grid$prior_hours^2 * V["smart_mix:prior_hours", "smart_mix:prior_hours"] +
                     2 * grid$prior_hours * V["smart_mix", "smart_mix:prior_hours"])
  ggplot(grid, aes(prior_hours, est)) +
    geom_hline(yintercept = 0, color = wu_red, linetype = "dashed") +
    geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), fill = wu_light, alpha = 0.7) +
    geom_line(color = wu_blue, linewidth = 1.2) +
    geom_rug(data = d, aes(x = prior_hours), inherit.aes = FALSE, alpha = 0.15, sides = "b") +
    labs(x = "Listening hours before the experiment", y = "Effect of Smart Mix on listening hours",
         subtitle = "Marginal effect of Smart Mix, with 95% confidence band") +
    theme_course(base_size)
}

fig_lines_prior <- function(d = load_wave2(), base_size = 13) {
  m <- lm(hours ~ smart_mix * prior_hours + premium + tenure, data = d)
  grid <- expand.grid(prior_hours = seq(3, 45, length.out = 50), smart_mix = c(0, 1), premium = 0, tenure = 30)
  grid$hours <- predict(m, grid)
  grid$group <- factor(grid$smart_mix, levels = c(0, 1), labels = c("Control", "Smart Mix"))
  ggplot() +
    geom_point(data = d, aes(prior_hours, hours, color = group), alpha = 0.15, size = 1) +
    geom_line(data = grid, aes(prior_hours, hours, color = group), linewidth = 1.3) +
    scale_color_manual(values = c(Control = wu_grey, `Smart Mix` = wu_blue), name = NULL) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 60)) +
    labs(x = "Listening hours before the experiment", y = "Listening hours after",
         subtitle = "Predicted lines for a free user with 30 months of tenure") +
    theme_course(base_size)
}

# --- 6.3b: elasticity by genre (continuous x categorical, slopes differ) ----
fig_elasticity_by_genre <- function(p = load_promo(), base_size = 13) {
  m <- lm(log(streams) ~ log(spend) * genre + log(followers) + placements, data = p)
  b <- coef(m)
  el <- data.frame(genre = levels(p$genre)) |>
    mutate(elasticity = b["log(spend)"] + ifelse(genre == "Pop", 0, b[paste0("log(spend):genre", genre)]))
  p2 <- p |> filter(genre %in% c("Pop", "HipHop/Rap"))
  ggplot(p2, aes(log(spend), log(streams), color = genre)) +
    geom_point(alpha = 0.3, size = 1.2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.3, formula = y ~ x) +
    scale_color_manual(values = c(Pop = wu_grey, `HipHop/Rap` = wu_blue), name = NULL,
                       labels = sprintf("%s (elasticity %.2f)", c("Pop", "HipHop/Rap"), el$elasticity[el$genre %in% c("Pop", "HipHop/Rap")])) +
    labs(x = "log(promotion spend)", y = "log(streams)") +
    theme_course(base_size)
}

# --- 6.4: continuous x continuous ---------------------------------------------
fig_marginal_spend_followers <- function(p = load_promo(), base_size = 13) {
  p <- p |> mutate(ls = log(spend) - mean(log(spend)), lf = log(followers) - mean(log(followers)))
  m <- lm(log(streams) ~ ls * lf + placements + genre, data = p)
  b <- coef(m); V <- vcov(m)
  grid <- data.frame(lf = seq(min(p$lf), max(p$lf), length.out = 100))
  grid$est <- b["ls"] + b["ls:lf"] * grid$lf
  grid$se  <- sqrt(V["ls", "ls"] + grid$lf^2 * V["ls:lf", "ls:lf"] + 2 * grid$lf * V["ls", "ls:lf"])
  grid$followers <- exp(grid$lf + mean(log(p$followers)))
  qs <- quantile(p$followers, c(0.1, 0.5, 0.9))
  pts <- data.frame(followers = qs, lf = log(qs) - mean(log(p$followers)), label = c("10th percentile", "median", "90th percentile"))
  pts$est <- b["ls"] + b["ls:lf"] * pts$lf
  ggplot(grid, aes(followers, est)) +
    geom_ribbon(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), fill = wu_light, alpha = 0.7) +
    geom_line(color = wu_blue, linewidth = 1.2) +
    geom_point(data = pts, size = 3.5, color = wu_dark) +
    geom_text(data = pts, aes(label = sprintf("%s:\n%.2f", label, est)), vjust = -0.6, size = base_size / 3.6, lineheight = 0.9) +
    scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    coord_cartesian(ylim = c(-0.2, 0.9)) +
    labs(x = "Artist followers (log scale)", y = "Elasticity of streams w.r.t. spend",
         subtitle = "Marginal effect of log(spend) depends on the follower base") +
    theme_course(base_size)
}

# --- 6.5: the 2 x 3 design -------------------------------------------------------
fig_ads_cells <- function(a = load_ads(), base_size = 13) {
  m <- a |> group_by(source, disclosure) |>
    summarise(mean = mean(creativity), se = sd(creativity) / sqrt(n()), .groups = "drop")
  ggplot(m, aes(source, mean, fill = disclosure)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.62) +
    geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                  position = position_dodge(width = 0.7), width = 0.18, linewidth = 0.7, color = "grey20") +
    geom_text(aes(label = sprintf("%.1f", mean), y = mean + 1.96 * se),
              position = position_dodge(width = 0.7), vjust = -0.6, size = base_size / 3.4) +
    scale_fill_manual(values = c(Revealed = wu_orange, `Not revealed` = wu_blue), name = "Source of the design") +
    scale_y_continuous(limits = c(0, 75), expand = c(0, 0)) +
    labs(x = "Who made the design", y = "Mean creativity rating (0\u2013100, 95% CI)") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
}

# --- 6.6: subgroup fishing and power for interactions --------------------------
fig_subgroup_fishing <- function(base_size = 13) {
  d <- data.frame(k = 1:30) |> mutate(p = 1 - 0.95^k)
  ggplot(d, aes(k, p)) +
    geom_line(color = wu_blue, linewidth = 1.2) +
    geom_point(data = d |> filter(k %in% c(1, 5, 10, 20)), size = 3, color = wu_dark) +
    geom_text(data = d |> filter(k %in% c(5, 10, 20)), aes(label = sprintf("%.0f%%", 100 * p)), vjust = -1, size = base_size / 3.4) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Number of subgroups examined (no true effect in any of them)",
         y = "Probability of at least one 'significant' subgroup") +
    theme_course(base_size)
}

fig_power_interaction <- function(base_size = 13) {
  d <- data.frame(what = factor(c("Main effect", "Interaction of the same size"), levels = c("Main effect", "Interaction of the same size")),
                  n = c(400, 1600))
  ggplot(d, aes(what, n)) +
    geom_col(fill = c(wu_grey, wu_blue), width = 0.55) +
    geom_text(aes(label = format(n, big.mark = ",")), vjust = -0.5, fontface = "bold", size = base_size / 3) +
    scale_y_continuous(limits = c(0, 1900), expand = c(0, 0)) +
    labs(x = NULL, y = "Users needed (80% power)", subtitle = "Detecting an effect of 2 hours vs. a 2-hour difference in effects between two groups") +
    theme_course(base_size)
}

# --- 6.7: mediation diagram ---------------------------------------------------------
fig_mediation_diagram <- function(a = NA, b = NA, c = NA, base_size = 13) {
  lab <- function(v, letter) if (is.na(v)) letter else sprintf("%s = %.2f", letter, v)
  nodes <- data.frame(name = c("Smart Mix\n(X)", "New artists\ndiscovered (M)", "Listening\nhours (Y)"),
                      x = c(0, 1.5, 3), y = c(0, 1, 0))
  ggplot() +
    annotate("segment", x = 0.45, xend = 1.05, y = 0.25, yend = 0.8, arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = wu_blue, linewidth = 1) +
    annotate("segment", x = 1.95, xend = 2.55, y = 0.8, yend = 0.25, arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = wu_blue, linewidth = 1) +
    annotate("segment", x = 0.5, xend = 2.5, y = 0, yend = 0, arrow = arrow(length = unit(0.3, "cm"), type = "closed"), color = wu_grey, linewidth = 1) +
    annotate("text", x = 0.55, y = 0.65, label = lab(a, "a"), size = base_size / 3, color = wu_blue, fontface = "bold") +
    annotate("text", x = 2.45, y = 0.65, label = lab(b, "b"), size = base_size / 3, color = wu_blue, fontface = "bold") +
    annotate("text", x = 1.5, y = -0.2, label = lab(c, "c\u2032"), size = base_size / 3, color = wu_grey, fontface = "bold") +
    geom_label(data = nodes, aes(x, y, label = name), size = base_size / 3, fill = "white", label.padding = unit(0.5, "lines"), lineheight = 0.9) +
    coord_cartesian(xlim = c(-0.6, 3.6), ylim = c(-0.5, 1.4)) +
    theme_void()
}

# --- 6.8: fork, pipe, collider ---------------------------------------------------------
fig_dag_structures <- function(base_size = 12) {
  one <- function(title, nodes, edges, note) {
    ggplot() +
      geom_segment(data = edges, aes(x, y, xend = xend, yend = yend), arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = wu_grey, linewidth = 0.9) +
      geom_label(data = nodes, aes(x, y, label = name), size = base_size / 3.2, fill = "white", label.padding = unit(0.4, "lines")) +
      annotate("text", x = 1, y = -0.75, label = note, size = base_size / 3.4, color = wu_dark, fontface = "bold") +
      coord_cartesian(xlim = c(-0.5, 2.5), ylim = c(-1, 1.4)) +
      labs(title = title) + theme_void(base_size) + theme(plot.title = element_text(face = "bold", hjust = 0.5))
  }
  fork <- one("Fork (confounder)", data.frame(name = c("X", "Z", "Y"), x = c(0, 1, 2), y = c(0, 1, 0)),
              data.frame(x = c(0.8, 1.2), y = c(0.75, 0.75), xend = c(0.2, 1.8), yend = c(0.2, 0.2)), "adjust for Z")
  pipe <- one("Pipe (mediator)", data.frame(name = c("X", "Z", "Y"), x = c(0, 1, 2), y = c(0, 0, 0)),
              data.frame(x = c(0.25, 1.25), y = c(0, 0), xend = c(0.75, 1.75), yend = c(0, 0)), "do NOT adjust for Z\n(unless you want the direct effect)")
  coll <- one("Collider (common effect)", data.frame(name = c("X", "Z", "Y"), x = c(0, 1, 2), y = c(0, -0.5, 0)),
              data.frame(x = c(0.2, 1.8), y = c(-0.15, -0.15), xend = c(0.8, 1.2), yend = c(-0.4, -0.4)), "do NOT adjust for Z")
  fork + pipe + coll
}

# --- A small helper: tidy coefficient tables -------------------------------
# Rounds estimates, standard errors and test statistics, and formats
# p-values the way journals report them (three decimals, "< 0.001").
coef_table <- function(model, digits = 3) {
  tab <- coef(summary(model))
  out <- data.frame(
    Estimate     = round(tab[, 1], digits),
    `Std. Error` = round(tab[, 2], digits),
    statistic    = round(tab[, 3], 2),
    `p-value`    = ifelse(tab[, 4] < 0.001, "< 0.001", sprintf("%.3f", tab[, 4])),
    check.names = FALSE
  )
  names(out)[3] <- colnames(tab)[3]
  rownames(out) <- rownames(tab)
  out
}
