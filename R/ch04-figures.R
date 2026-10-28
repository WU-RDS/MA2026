# ------------------------------------------------------------------
# ch04-figures.R
# Shared data and figure functions for Chapter 4 (book chapter AND
# slide deck). Uses the Smart Mix A/B test from Chapter 3.
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

# --- data sets --------------------------------------------------------

# Smart Mix A/B test (identical to Chapter 3)
ab_data <- function() {
  set.seed(31)
  tibble::tibble(
    group = factor(rep(c("Control", "Smart Mix"), each = 200), levels = c("Smart Mix", "Control")),
    hours = c(rgamma(200, shape = 2, scale = 10), rgamma(200, shape = 2, scale = 11.5))
  )
}

# Premium upgrade test: counts of upgrades out of 2,000 users per group
upgrade_data <- function() {
  tibble::tibble(
    group = factor(rep(c("Trial offer", "Standard page"), each = 2000),
                   levels = c("Trial offer", "Standard page")),
    upgraded = c(rep(1, 131), rep(0, 2000 - 131), rep(1, 104), rep(0, 2000 - 104))
  )
}

# Three versions of Smart Mix (between-subjects, 150 users each)
mix3_data <- function() {
  set.seed(14)
  tibble::tibble(
    version = factor(rep(c("No mix", "Weekly mix", "Daily mix"), each = 150),
                     levels = c("No mix", "Weekly mix", "Daily mix")),
    hours = c(rgamma(150, 2, scale = 10), rgamma(150, 2, scale = 10.8), rgamma(150, 2, scale = 12))
  )
}

# Within-subjects: 60 users search for songs with the old and the new
# search design (order randomized); outcome: seconds to find a song
search_data <- function() {
  set.seed(4)
  skill <- rnorm(60, 0, 8)                     # users differ a lot ...
  tibble::tibble(
    user = 1:60,
    old_design = round(40 + skill + rnorm(60, 0, 3), 1),
    new_design = round(38 + skill + rnorm(60, 0, 3), 1)   # ... the designs a little
  )
}

# --- 4.2: sampling distribution of a difference -----------------------

null_differences <- function(reps = 10000, n = 200, seed = 1) {
  # all users from ONE population (no effect): pooled Smart Mix data as population proxy
  set.seed(seed)
  replicate(reps, mean(rgamma(n, 2, scale = 10.75)) - mean(rgamma(n, 2, scale = 10.75)))
}

fig_null_world <- function(observed = NULL, base_size = 13) {
  ab <- ab_data()
  if (is.null(observed)) {
    m <- ab |> group_by(group) |> summarise(m = mean(hours))
    observed <- m$m[m$group == "Smart Mix"] - m$m[m$group == "Control"]
  }
  d <- null_differences()
  share <- mean(abs(d) >= abs(observed))
  df <- data.frame(d = d) |> mutate(extreme = abs(d) >= abs(observed))
  ggplot(df, aes(d, fill = extreme)) +
    geom_histogram(binwidth = 0.25, boundary = 0) +
    geom_vline(xintercept = c(-observed, observed), color = wu_red, linetype = "dashed") +
    annotate("text", x = observed, y = Inf, label = sprintf("observed:\n+%.1f hours", observed),
             hjust = -0.1, vjust = 1.3, color = wu_red, size = base_size / 3.4, lineheight = 0.9) +
    scale_fill_manual(values = c(`FALSE` = wu_light, `TRUE` = wu_orange),
                      labels = c("less extreme than observed", "at least as extreme as observed"), name = NULL) +
    labs(title = "10,000 A/B tests in a world in which Smart Mix has no effect",
         subtitle = sprintf("%.1f%% of the simulated differences are at least as extreme as the observed one", 100 * share),
         x = "Difference in mean listening hours (Smart Mix \u2212 Control)", y = "Number of simulated tests") +
    theme_course(base_size)
}

# --- 4.3: t distribution with rejection region -------------------------

fig_t_test <- function(t_obs = 2.16, df = 387, base_size = 13) {
  crit <- qt(0.975, df)
  x <- seq(-4.5, 4.5, length.out = 600)
  d <- data.frame(x = x, y = dt(x, df))
  ggplot(d, aes(x, y)) +
    geom_area(data = subset(d, x >= crit), fill = wu_orange, alpha = 0.7) +
    geom_area(data = subset(d, x <= -crit), fill = wu_orange, alpha = 0.7) +
    geom_line(color = wu_dark, linewidth = 1) +
    geom_vline(xintercept = t_obs, color = wu_red, linewidth = 1) +
    annotate("text", x = t_obs, y = 0.33, label = sprintf("observed\nt = %.2f", t_obs), hjust = -0.1,
             color = wu_red, size = base_size / 3.4, lineheight = 0.9) +
    annotate("text", x = c(-3.2, 3.2), y = 0.06, label = "2.5%", color = wu_orange, fontface = "bold",
             size = base_size / 3.2) +
    annotate("text", x = 0, y = 0.12, label = "95% of the t values\nif H0 is true", size = base_size / 3.6,
             lineheight = 0.9) +
    scale_x_continuous(breaks = c(-crit, 0, crit), labels = sprintf("%.2f", c(-crit, 0, crit))) +
    labs(title = "Where does the observed t value fall if Smart Mix has no effect?",
         x = "t value", y = NULL) +
    theme_course(base_size) + theme(axis.text.y = element_blank())
}

# --- 4.3: confidence interval of the difference -----------------------

fig_ci_diff <- function(base_size = 13) {
  tt <- t.test(hours ~ group, data = ab_data())
  est <- unname(tt$estimate[1] - tt$estimate[2])
  d <- data.frame(y = "Smart Mix \u2212 Control", est = est, lo = tt$conf.int[1], hi = tt$conf.int[2])
  ggplot(d, aes(y = y)) +
    geom_vline(xintercept = 0, color = wu_red, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.15, color = wu_blue, linewidth = 1.2) +
    geom_point(aes(x = est), size = 4, color = wu_dark) +
    annotate("text", x = 0, y = 0.7, label = "no effect", color = wu_red, size = base_size / 3.4, hjust = -0.1) +
    geom_text(aes(x = est, label = sprintf("%.1f hours [%.1f; %.1f]", est, lo, hi)), vjust = -1.4,
              size = base_size / 3.3) +
    coord_cartesian(xlim = c(-2, 8), ylim = c(0.6, 1.5)) +
    labs(x = "Difference in mean listening hours (95% confidence interval)", y = NULL) +
    theme_course(base_size)
}

# --- 4.6: conversion rates with confidence intervals ------------------

fig_conversion <- function(base_size = 13) {
  d <- upgrade_data() |> group_by(group) |>
    summarise(n = n(), x = sum(upgraded), p = x / n) |>
    mutate(lo = p - 1.96 * sqrt(p * (1 - p) / n), hi = p + 1.96 * sqrt(p * (1 - p) / n))
  ggplot(d, aes(x = group, y = 100 * p)) +
    geom_col(fill = wu_light, width = 0.55) +
    geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi), width = 0.12, color = wu_dark, linewidth = 0.9) +
    geom_text(aes(y = 100 * hi, label = sprintf("%.2f%%", 100 * p)), vjust = -0.8, fontface = "bold") +
    scale_y_continuous(limits = c(0, 9), expand = c(0, 0)) +
    labs(x = NULL, y = "Upgrade rate (%) with 95% CI") +
    theme_course(base_size)
}

# --- 4.7: power ---------------------------------------------------------

fig_power <- function(delta = 3, sd = 15, n = 200, base_size = 13) {
  se <- sd * sqrt(2 / n)
  crit <- qnorm(0.975) * se
  x <- seq(-4 * se, delta + 4 * se, length.out = 800)
  d <- data.frame(x = x, h0 = dnorm(x, 0, se), h1 = dnorm(x, delta, se))
  pw <- pnorm(crit, delta, se, lower.tail = FALSE) + pnorm(-crit, delta, se)
  ggplot(d, aes(x)) +
    geom_area(data = subset(d, x >= crit), aes(y = h1), fill = wu_green, alpha = 0.45) +
    geom_area(data = subset(d, x < crit & x > -crit), aes(y = h1), fill = wu_red, alpha = 0.25) +
    geom_area(data = subset(d, x >= crit), aes(y = h0), fill = wu_orange, alpha = 0.8) +
    geom_line(aes(y = h0), color = wu_grey, linewidth = 1) +
    geom_line(aes(y = h1), color = wu_blue, linewidth = 1) +
    geom_vline(xintercept = crit, linetype = "dashed") +
    annotate("text", x = -1.2 * se, y = max(d$h0) * 1.04, label = "if there is\nno effect (H0)", color = wu_grey,
             size = base_size / 3.4, hjust = 1, lineheight = 0.9) +
    annotate("text", x = delta + 1.2 * se, y = max(d$h1) * 1.04, label = sprintf("if the true\neffect is %g hours", delta),
             color = wu_blue, size = base_size / 3.4, hjust = 0, lineheight = 0.9) +
    labs(title = sprintf("Power = %.0f%% (n = %d per group)", 100 * pw, n),
         subtitle = "Green: effect detected (power) \u00b7 red: effect missed (type II error) \u00b7 orange: false alarm (type I error)",
         x = "Observed difference in mean listening hours", y = NULL) +
    theme_course(base_size) + theme(axis.text.y = element_blank())
}

fig_n_required <- function(base_size = 13) {
  lifts <- seq(0.02, 0.5, by = 0.01)
  d <- expand.grid(base = c(0.20, 0.05, 0.005), lift = lifts) |>
    rowwise() |>
    mutate(n = power.prop.test(p1 = base, p2 = base * (1 + lift), power = 0.8)$n) |>
    ungroup() |>
    mutate(base = factor(paste0("baseline rate ", 100 * base, "%"),
                         levels = paste0("baseline rate ", c(20, 5, 0.5), "%")))
  ggplot(d, aes(100 * lift, n, color = base)) +
    geom_line(linewidth = 1.1) +
    scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                  breaks = c(100, 1000, 1e4, 1e5, 1e6, 1e7)) +
    scale_color_manual(values = c(wu_green, wu_blue, wu_orange), name = NULL) +
    labs(title = "Users needed per group to detect a lift with 80% power",
         x = "Relative lift to be detected (%)", y = "Users per group (log scale)") +
    theme_course(base_size)
}

# --- 4.8: three groups ---------------------------------------------------

fig_three_groups <- function(base_size = 13) {
  d <- mix3_data()
  m <- d |> group_by(version) |> summarise(mean = mean(hours))
  ggplot(d, aes(version, hours)) +
    geom_boxplot(fill = wu_light, outlier.alpha = 0.3, width = 0.55) +
    geom_point(data = m, aes(y = mean), color = wu_blue, size = 4) +
    labs(x = NULL, y = "Listening hours (four weeks)") +
    theme_course(base_size)
}

# --- 4.9: optional stopping ----------------------------------------------

fig_optional_stopping <- function(k = 20, max_n = 1000, step = 20, base_size = 13) {
  set.seed(2024)
  looks <- seq(step, max_n, by = step)
  paths <- bind_rows(lapply(1:k, function(i) {
    a <- rnorm(max_n); b <- rnorm(max_n)
    p <- sapply(looks, function(n) t.test(a[1:n], b[1:n])$p.value)
    data.frame(test = i, n = looks, p = p, ever = any(p < 0.05))
  }))
  share <- mean(tapply(paths$ever, paths$test, any))
  ggplot(paths, aes(n, p, group = test, color = ever)) +
    geom_hline(yintercept = 0.05, color = wu_red, linetype = "dashed") +
    geom_line(alpha = 0.8) +
    scale_color_manual(values = c(`FALSE` = "grey70", `TRUE` = wu_orange),
                       labels = c("never significant", "significant at some look"), name = NULL) +
    scale_y_continuous(breaks = c(0, 0.05, 0.25, 0.5, 0.75, 1)) +
    labs(title = sprintf("%d A/B tests without any true effect, checked after every %d users per group", k, step),
         subtitle = sprintf("%d of %d tests cross p < .05 at some point", round(share * k), k),
         x = "Users per group so far", y = "p-value") +
    theme_course(base_size)
}

# ------------------------------------------------------------------
# Additions to R/ch04-figures.R (October 2026)
# Paste at the end of ch04-figures.R. fig_ab_bars() comes from the
# Chapter 3 additions (ch03-figures.R), which Chapter 4 also sources.
# ------------------------------------------------------------------

# --- 4.5: the two group means with their intervals and the test result -----
fig_report_bars <- function(ab = ab_data(), base_size = 13) {
  m <- ab |> group_by(group) |>
    summarise(n = n(), mean = mean(hours), se = sd(hours) / sqrt(n), .groups = "drop") |>
    mutate(lower = mean - qt(0.975, n - 1) * se, upper = mean + qt(0.975, n - 1) * se)
  tt <- t.test(hours ~ group, data = ab)
  top <- max(m$upper)
  ggplot(m, aes(x = group, y = mean, fill = group)) +
    geom_col(width = 0.55) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.9, color = "grey20") +
    geom_text(aes(y = lower, label = sprintf("%.1f", mean)), vjust = 1.8, color = "white", fontface = "bold",
              size = base_size / 3) +
    # bracket between the two bars with the test result
    annotate("segment", x = 1, xend = 1, y = top + 1, yend = top + 3, color = "grey20") +
    annotate("segment", x = 2, xend = 2, y = top + 1, yend = top + 3, color = "grey20") +
    annotate("segment", x = 1, xend = 2, y = top + 3, yend = top + 3, color = "grey20") +
    annotate("text", x = 1.5, y = top + 3.6,
             label = sprintf("difference: %.1f hours, 95%% CI [%.1f; %.1f], p = %.3f",
                             diff(rev(tt$estimate)), tt$conf.int[1], tt$conf.int[2], tt$p.value),
             vjust = 0, size = base_size / 3.4) +
    scale_fill_manual(values = c(`Smart Mix` = wu_blue, Control = wu_grey), guide = "none") +
    scale_y_continuous(limits = c(0, top + 7), expand = c(0, 0)) +
    labs(x = NULL, y = "Mean listening hours (four weeks)",
         subtitle = "Bars: means · error bars: 95% CIs · bracket: Welch t-test") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
}

# --- 4.8: three groups as bars with confidence intervals ---------------------
fig_three_groups_bars <- function(d = mix3_data(), base_size = 13) {
  m <- d |> group_by(version) |>
    summarise(n = n(), mean = mean(hours), se = sd(hours) / sqrt(n), .groups = "drop") |>
    mutate(lower = mean - qt(0.975, n - 1) * se, upper = mean + qt(0.975, n - 1) * se)
  ggplot(m, aes(x = version, y = mean)) +
    geom_col(width = 0.55, fill = wu_blue) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.9, color = "grey20") +
    geom_text(aes(y = upper, label = sprintf("%.1f", mean)), vjust = -0.7, fontface = "bold", size = base_size / 3.2) +
    scale_y_continuous(limits = c(0, 32), expand = c(0, 0)) +
    labs(x = NULL, y = "Mean listening hours (four weeks)",
         subtitle = "Bars: group means · error bars: 95% confidence intervals") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
}

# ------------------------------------------------------------------
# Additions to R/ch04-figures.R (October 2026)
# Paste at the end of ch04-figures.R. fig_ab_bars() comes from the
# Chapter 3 additions (ch03-figures.R), which Chapter 4 also sources.
# ------------------------------------------------------------------

# --- 4.5: the two group means with their intervals and the test result -----
fig_report_bars <- function(ab = ab_data(), base_size = 13) {
  m <- ab |> group_by(group) |>
    summarise(n = n(), mean = mean(hours), se = sd(hours) / sqrt(n), .groups = "drop") |>
    mutate(lower = mean - qt(0.975, n - 1) * se, upper = mean + qt(0.975, n - 1) * se)
  tt <- t.test(hours ~ group, data = ab)
  top <- max(m$upper)
  ggplot(m, aes(x = group, y = mean, fill = group)) +
    geom_col(width = 0.55) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.9, color = "grey20") +
    geom_text(aes(y = lower, label = sprintf("%.1f", mean)), vjust = 1.8, color = "white", fontface = "bold",
              size = base_size / 3) +
    # bracket between the two bars with the test result
    annotate("segment", x = 1, xend = 1, y = top + 1, yend = top + 3, color = "grey20") +
    annotate("segment", x = 2, xend = 2, y = top + 1, yend = top + 3, color = "grey20") +
    annotate("segment", x = 1, xend = 2, y = top + 3, yend = top + 3, color = "grey20") +
    annotate("text", x = 1.5, y = top + 3.6,
             label = sprintf("difference: %.1f hours, 95%% CI [%.1f; %.1f], p = %.3f",
                             diff(rev(tt$estimate)), tt$conf.int[1], tt$conf.int[2], tt$p.value),
             vjust = 0, size = base_size / 3.4) +
    scale_fill_manual(values = c(`Smart Mix` = wu_blue, Control = wu_grey), guide = "none") +
    scale_y_continuous(limits = c(0, top + 7), expand = c(0, 0)) +
    labs(x = NULL, y = "Mean listening hours (four weeks)",
         subtitle = "Bars: means · error bars: 95% CIs · bracket: Welch t-test") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
}

# --- 4.8: three groups as bars with confidence intervals ---------------------
fig_three_groups_bars <- function(d = mix3_data(), base_size = 13) {
  m <- d |> group_by(version) |>
    summarise(n = n(), mean = mean(hours), se = sd(hours) / sqrt(n), .groups = "drop") |>
    mutate(lower = mean - qt(0.975, n - 1) * se, upper = mean + qt(0.975, n - 1) * se)
  ggplot(m, aes(x = version, y = mean)) +
    geom_col(width = 0.55, fill = wu_blue) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.9, color = "grey20") +
    geom_text(aes(y = upper, label = sprintf("%.1f", mean)), vjust = -0.7, fontface = "bold", size = base_size / 3.2) +
    scale_y_continuous(limits = c(0, 32), expand = c(0, 0)) +
    labs(x = NULL, y = "Mean listening hours (four weeks)",
         subtitle = "Bars: group means · error bars: 95% confidence intervals") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
}

# --- 4.4: the paired design, user by user ------------------------------------
fig_paired_lines <- function(d = search_data(), base_size = 13) {
  long <- d |> mutate(user = row_number()) |>
    tidyr::pivot_longer(c(old_design, new_design), names_to = "design", values_to = "seconds") |>
    mutate(design = factor(design, levels = c("old_design", "new_design"), labels = c("Old design", "New design")),
           faster = (seconds[design == "New design"] < seconds[design == "Old design"]), .by = user)
  m <- long |> group_by(design) |> summarise(mean = mean(seconds), .groups = "drop")
  ggplot(long, aes(design, seconds, group = user)) +
    geom_line(aes(color = faster), alpha = 0.5) +
    geom_point(color = wu_grey, size = 1.6, alpha = 0.7) +
    geom_line(data = m, aes(design, mean, group = 1), color = wu_red, linewidth = 1.6) +
    geom_point(data = m, aes(design, mean), color = wu_red, size = 4, inherit.aes = FALSE) +
    scale_color_manual(values = c(`TRUE` = wu_blue, `FALSE` = wu_orange),
                       labels = c("slower with the new design", "faster with the new design"), name = NULL) +
    labs(x = NULL, y = "Seconds to find a song",
         subtitle = "Each thin line is one user; the red line connects the two means") +
    theme_course(base_size)
}


# ------------------------------------------------------------------
# Additions to R/ch04-figures.R (October 2026, part 2): ANOVA figures
# ------------------------------------------------------------------

# --- 4.9: the three-group data, user by user (boxplot + jitter + mean) -------
fig_three_groups_points <- function(d = mix3_data(), base_size = 13) {
  ggplot(d, aes(x = version, y = hours)) +
    geom_boxplot(fill = wu_light, outlier.shape = NA, width = 0.55) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.3, size = 1.2, color = wu_grey) +
    stat_summary(fun = mean, geom = "point", color = wu_red, size = 3.5) +
    labs(x = NULL, y = "Listening hours (four weeks)",
         subtitle = "Box: middle 50% and median · grey dots: users · red dot: mean") +
    theme_course(base_size)
}

# --- 4.9: the three sums of squares, drawn from the data ---------------------
fig_sum_of_squares <- function(d = mix3_data(), show_per_group = 20, base_size = 12) {
  # Sums of squares over ALL users; for readability, the chart shows a
  # random subset of users per version (the means are those of all users)
  grand <- mean(d$hours)
  d <- d |> mutate(group_mean = mean(hours), .by = version)
  ss_t <- sum((d$hours - grand)^2); ss_m <- sum((d$group_mean - grand)^2); ss_r <- sum((d$hours - d$group_mean)^2)
  set.seed(1)
  s <- d |> slice_sample(n = show_per_group, by = version) |> arrange(version) |> mutate(obs = row_number())
  gm <- s |> group_by(version) |> summarise(x0 = min(obs) - 0.4, x1 = max(obs) + 0.4, group_mean = first(group_mean), .groups = "drop")
  fmt <- function(x) format(round(x), big.mark = ",")
  cols <- c(`No mix` = wu_grey, `Weekly mix` = wu_orange, `Daily mix` = wu_blue)
  base <- function(title) {
    list(geom_hline(yintercept = grand, linetype = "dashed", color = "black"),
         scale_color_manual(values = cols, name = NULL),
         coord_cartesian(ylim = c(0, 75)),
         labs(title = title, x = sprintf("%d users per version (sorted by version)", show_per_group), y = "Listening hours"),
         theme_course(base_size),
         theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank()))
  }
  means_layer <- geom_segment(data = gm, aes(x = x0, xend = x1, y = group_mean, yend = group_mean, color = version),
                              linewidth = 1.5, inherit.aes = FALSE)
  p1 <- ggplot(s, aes(obs, hours, color = version)) +
    geom_segment(aes(xend = obs, yend = grand), alpha = 0.5) +
    geom_point(size = 1.6) +
    base(sprintf("Total: users vs.\ngrand mean: SS_T = %s", fmt(ss_t)))
  p2 <- ggplot(s, aes(obs, group_mean, color = version)) +
    geom_segment(aes(xend = obs, yend = grand), alpha = 0.5) +
    means_layer +
    base(sprintf("Model: group means vs.\ngrand mean: SS_M = %s", fmt(ss_m))) + guides(color = "none")
  p3 <- ggplot(s, aes(obs, hours, color = version)) +
    geom_segment(aes(xend = obs, yend = group_mean), alpha = 0.5) +
    geom_point(size = 1.6) +
    means_layer +
    base(sprintf("Residual: users vs. their\ngroup mean: SS_R = %s", fmt(ss_r))) + guides(color = "none")
  (p1 | p2 | p3) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
}


# ------------------------------------------------------------------
# Additions to R/ch04-figures.R (October 2026, part 2): ANOVA figures
# ------------------------------------------------------------------

# --- 4.9: the three-group data, user by user (boxplot + jitter + mean) -------
fig_three_groups_points <- function(d = mix3_data(), base_size = 13) {
  ggplot(d, aes(x = version, y = hours)) +
    geom_boxplot(fill = wu_light, outlier.shape = NA, width = 0.55) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.3, size = 1.2, color = wu_grey) +
    stat_summary(fun = mean, geom = "point", color = wu_red, size = 3.5) +
    labs(x = NULL, y = "Listening hours (four weeks)",
         subtitle = "Box: middle 50% and median · grey dots: users · red dot: mean") +
    theme_course(base_size)
}

# --- 4.9: the three sums of squares, drawn from the data ---------------------
fig_sum_of_squares <- function(d = mix3_data(), show_per_group = 20, base_size = 12) {
  # Sums of squares over ALL users; for readability, the chart shows a
  # random subset of users per version (the means are those of all users)
  grand <- mean(d$hours)
  d <- d |> mutate(group_mean = mean(hours), .by = version)
  ss_t <- sum((d$hours - grand)^2); ss_m <- sum((d$group_mean - grand)^2); ss_r <- sum((d$hours - d$group_mean)^2)
  set.seed(1)
  s <- d |> slice_sample(n = show_per_group, by = version) |> arrange(version) |> mutate(obs = row_number())
  gm <- s |> group_by(version) |> summarise(x0 = min(obs) - 0.4, x1 = max(obs) + 0.4, group_mean = first(group_mean), .groups = "drop")
  fmt <- function(x) format(round(x), big.mark = ",")
  cols <- c(`No mix` = wu_grey, `Weekly mix` = wu_orange, `Daily mix` = wu_blue)
  base <- function(title) {
    list(geom_hline(yintercept = grand, linetype = "dashed", color = "black"),
         scale_color_manual(values = cols, name = NULL),
         coord_cartesian(ylim = c(0, 90)),
         labs(title = title, x = sprintf("%d users per version (sorted by version)", show_per_group), y = "Listening hours"),
         theme_course(base_size),
         theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank()))
  }
  means_layer <- geom_segment(data = gm, aes(x = x0, xend = x1, y = group_mean, yend = group_mean, color = version),
                              linewidth = 1.5, inherit.aes = FALSE)
  p1 <- ggplot(s, aes(obs, hours, color = version)) +
    geom_segment(aes(xend = obs, yend = grand), alpha = 0.5) +
    geom_point(size = 1.6) +
    base(sprintf("Total: users vs.\ngrand mean: SS_T = %s", fmt(ss_t)))
  p2 <- ggplot(s, aes(obs, group_mean, color = version)) +
    geom_segment(aes(xend = obs, yend = grand), alpha = 0.5) +
    means_layer +
    base(sprintf("Model: group means vs.\ngrand mean: SS_M = %s", fmt(ss_m))) + guides(color = "none")
  p3 <- ggplot(s, aes(obs, hours, color = version)) +
    geom_segment(aes(xend = obs, yend = group_mean), alpha = 0.5) +
    geom_point(size = 1.6) +
    means_layer +
    base(sprintf("Residual: users vs. their\ngroup mean: SS_R = %s", fmt(ss_r))) + guides(color = "none")
  (p1 | p2 | p3) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
}


# --- 4.9: two contrasting worlds for the sums of squares -----------------------
# Same design (three versions, 150 users each), different data:
# "strong": the group means are far apart and users within a group are similar
#           -> most of the variation is between groups (high eta squared)
# "none":   the group means are (almost) identical -> the version explains
#           (almost) nothing (eta squared close to 0)
ss_data <- function(type = c("strong", "none")) {
  type <- match.arg(type)
  lv <- c("No mix", "Weekly mix", "Daily mix")
  if (type == "strong") {
    set.seed(11)
    hours <- pmax(rnorm(450, mean = rep(c(10, 22, 34), each = 150), sd = 5), 0)
  } else {
    set.seed(23)
    hours <- rgamma(450, shape = 2, scale = 11)
  }
  tibble::tibble(version = factor(rep(lv, each = 150), levels = lv), hours = hours)
}

eta_squared <- function(d) {
  grand <- mean(d$hours)
  gm <- ave(d$hours, d$version)
  sum((gm - grand)^2) / sum((d$hours - grand)^2)
}
