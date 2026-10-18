# ------------------------------------------------------------------
# ch03-figures.R
# Shared figure functions for Chapter 3 (book chapter AND slide deck).
# All populations are simulated, so that the true values are known.
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
wu_light  <- "#dbe7f3"

theme_course <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = rel(1.0)),
          plot.subtitle = element_text(color = "grey30"),
          legend.position = "bottom")
}

# --- the two populations of WU students ------------------------------
# monthly music listening time (hours) of all 25,000 WU students

pop_normal <- function() {
  set.seed(321)
  rnorm(25000, mean = 50, sd = 10)
}

pop_skewed <- function() {
  set.seed(321)
  rgamma(25000, shape = 2, scale = 10)
}

# repeated sampling: means of `reps` samples of size n
sample_means <- function(pop, n, reps = 20000, seed = 12345) {
  set.seed(seed)
  replicate(reps, mean(sample(pop, n)))
}

# --- 3.1: two kinds of error (target diagram) -------------------------
fig_bias_variance <- function(base_size = 13) {
  set.seed(7)
  grid <- expand.grid(bias = c("No bias", "Bias"), noise = c("Little noise", "Much noise"))
  pts <- grid |>
    rowwise() |>
    do({
      g <- .
      sdv <- ifelse(g$noise == "Little noise", 0.25, 0.75)
      shift <- ifelse(g$bias == "Bias", 1.3, 0)
      data.frame(bias = g$bias, noise = g$noise,
                 x = rnorm(12, shift, sdv), y = rnorm(12, shift * 0.6, sdv))
    }) |> ungroup()
  rings <- expand.grid(r = c(0.6, 1.3, 2.0), t = seq(0, 2 * pi, length.out = 100)) |>
    mutate(x = r * cos(t), y = r * sin(t))
  ggplot() +
    geom_path(data = rings, aes(x, y, group = r), color = "grey70") +
    geom_point(data = data.frame(x = 0, y = 0), aes(x, y), shape = 3, size = 4, color = wu_red, stroke = 1.2) +
    geom_point(data = pts, aes(x, y), color = wu_blue, size = 2.2, alpha = 0.8) +
    facet_grid(noise ~ bias) +
    coord_fixed(xlim = c(-2.3, 2.3), ylim = c(-2.3, 2.3)) +
    theme_void(base_size = base_size) +
    theme(strip.text = element_text(face = "bold", margin = margin(4, 4, 4, 4)))
}

# --- 3.3: the population and single samples ---------------------------
fig_population <- function(pop = pop_normal(), base_size = 13,
                           title = "Monthly listening time of all 25,000 WU students") {
  mu <- mean(pop); s <- sd(pop)
  ggplot(data.frame(hours = pop), aes(hours)) +
    geom_histogram(bins = 60, fill = wu_light, color = "white") +
    geom_vline(xintercept = mu, color = wu_dark, linewidth = 1) +
    labs(title = title,
         subtitle = sprintf("Population mean \u03bc = %.2f hours; population SD \u03c3 = %.2f hours", mu, s),
         x = "Hours per month", y = "Number of students") +
    theme_course(base_size)
}

fig_four_samples <- function(pop = pop_normal(), n = 100, base_size = 12) {
  seeds <- c(8830, 6789, 3904, 3333)
  d <- bind_rows(lapply(seq_along(seeds), function(i) {
    set.seed(seeds[i])
    data.frame(sample = paste("Sample", i), hours = sample(pop, n))
  }))
  m <- d |> group_by(sample) |> summarise(mean = mean(hours)) |>
    mutate(lab = sprintf("x\u0304 = %.2f", mean))
  ggplot(d, aes(hours)) +
    geom_histogram(bins = 20, fill = wu_light, color = "white") +
    geom_vline(xintercept = mean(pop), color = wu_red, linewidth = 0.8, linetype = "dashed") +
    geom_vline(data = m, aes(xintercept = mean), color = wu_dark, linewidth = 1) +
    geom_text(data = m, aes(x = Inf, y = Inf, label = lab), hjust = 1.1, vjust = 1.5,
              color = wu_dark, fontface = "bold", size = base_size / 3.2) +
    facet_wrap(~sample, nrow = 1) +
    labs(x = "Hours per month", y = "Number of students",
         subtitle = "Solid line: sample mean; dashed red line: population mean") +
    theme_course(base_size)
}

# --- 3.3: the sampling distribution -----------------------------------
fig_sampling_distribution <- function(pop = pop_normal(), n = 100, reps = 20000, base_size = 13) {
  m <- sample_means(pop, n, reps)
  se <- sd(pop) / sqrt(n)
  ggplot(data.frame(m = m), aes(m)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, fill = wu_light, color = "white") +
    stat_function(fun = dnorm, args = list(mean = mean(pop), sd = se),
                  color = wu_dark, linewidth = 0.9) +
    geom_vline(xintercept = mean(pop), color = wu_red, linewidth = 0.8, linetype = "dashed") +
    labs(title = sprintf("Means of %s samples with n = %d students each", format(reps, big.mark = ","), n),
         subtitle = sprintf("Mean of the sample means = %.2f; SD of the sample means = %.2f", mean(m), sd(m)),
         x = "Sample mean (hours per month)", y = "Density") +
    theme_course(base_size)
}

# --- 3.4: what drives the standard error ------------------------------
fig_se_by_n <- function(sigma = 10, base_size = 13) {
  d <- data.frame(n = 10:1000) |> mutate(se = sigma / sqrt(n))
  marks <- data.frame(n = c(25, 100, 400)) |> mutate(se = sigma / sqrt(n))
  ggplot(d, aes(n, se)) +
    geom_line(color = wu_blue, linewidth = 1.1) +
    geom_point(data = marks, color = wu_dark, size = 3) +
    geom_text(data = marks, aes(label = sprintf("n = %d\nSE = %.1f", n, se)),
              hjust = -0.15, vjust = -0.2, size = base_size / 3.5, lineheight = 0.9) +
    labs(title = "Four times the sample size, half the standard error",
         subtitle = sprintf("Standard error of the mean for a population SD of \u03c3 = %d hours", sigma),
         x = "Sample size n", y = "Standard error (hours)") +
    theme_course(base_size)
}

fig_se_panels_n <- function(pop = pop_normal(), base_size = 12) {
  ns <- c(10, 100, 1000)
  d <- bind_rows(lapply(ns, function(n)
    data.frame(n = sprintf("n = %s  (SE = %.2f)", format(n, big.mark = ","), sd(pop) / sqrt(n)),
               m = sample_means(pop, n, reps = 5000))))
  d$n <- factor(d$n, levels = unique(d$n))
  ggplot(d, aes(m)) +
    geom_histogram(bins = 60, fill = wu_blue, color = "white") +
    geom_vline(xintercept = mean(pop), color = wu_red, linetype = "dashed") +
    facet_wrap(~n, nrow = 1) +
    coord_cartesian(xlim = c(40, 60)) +
    labs(x = "Sample mean (hours per month)", y = "Number of samples") +
    theme_course(base_size)
}

fig_se_panels_sigma <- function(base_size = 12) {
  set.seed(12345)
  d <- bind_rows(lapply(c(5, 15), function(s) {
    pop <- rnorm(25000, 50, s)
    data.frame(sigma = sprintf("\u03c3 = %d hours  (SE = %.2f)", s, s / sqrt(100)),
               m = sample_means(pop, 100, reps = 5000))
  }))
  d$sigma <- factor(d$sigma, levels = unique(d$sigma))
  ggplot(d, aes(m)) +
    geom_histogram(bins = 60, fill = wu_blue, color = "white") +
    geom_vline(xintercept = 50, color = wu_red, linetype = "dashed") +
    facet_wrap(~sigma, nrow = 1) +
    coord_cartesian(xlim = c(44, 56)) +
    labs(x = "Sample mean (hours per month), n = 100", y = "Number of samples") +
    theme_course(base_size)
}

# --- 3.4: two kinds of variation --------------------------------------
fig_sd_vs_se <- function(pop = pop_normal(), base_size = 12) {
  set.seed(6789)
  smp <- sample(pop, 100)
  m <- sample_means(pop, 100, reps = 20000)
  p1 <- ggplot(data.frame(h = smp), aes(h)) +
    geom_histogram(bins = 20, fill = wu_light, color = "white") +
    coord_cartesian(xlim = c(15, 85)) +
    labs(title = "One sample: how students differ",
         subtitle = sprintf("Standard deviation s = %.1f hours", sd(smp)),
         x = "Hours per month", y = "Number of students") +
    theme_course(base_size)
  p2 <- ggplot(data.frame(m = m), aes(m)) +
    geom_histogram(binwidth = 0.5, fill = wu_blue) +
    coord_cartesian(xlim = c(15, 85)) +
    labs(title = "Many samples: how sample means differ",
         subtitle = sprintf("Standard error = %.1f hours", sd(m)),
         x = "Sample mean (hours per month)", y = "Number of samples") +
    theme_course(base_size)
  p1 + p2
}

# --- 3.5: central limit theorem ---------------------------------------
fig_clt <- function(pop = pop_skewed(), base_size = 12) {
  ns <- c(1, 5, 30, 100)
  d <- bind_rows(lapply(ns, function(n)
    data.frame(n = paste("n =", n), m = sample_means(pop, n, reps = 10000))))
  d$n <- factor(d$n, levels = paste("n =", ns))
  ggplot(d, aes(m)) +
    geom_histogram(bins = 50, fill = wu_blue, color = "white") +
    geom_vline(xintercept = mean(pop), color = wu_red, linetype = "dashed") +
    facet_wrap(~n, nrow = 1, scales = "free") +
    labs(x = "Sample mean (hours per month)", y = "Number of samples",
         subtitle = "Distribution of 10,000 sample means for different sample sizes n (skewed population)") +
    theme_course(base_size)
}

# --- 3.7: confidence intervals ----------------------------------------
fig_ci_100 <- function(pop = pop_normal(), n = 100, base_size = 12) {
  set.seed(12345)
  mu <- mean(pop)
  d <- bind_rows(lapply(1:100, function(i) {
    x <- sample(pop, n)
    se <- sd(x) / sqrt(n)
    data.frame(sample = i, mean = mean(x), lower = mean(x) - qt(0.975, n - 1) * se,
               upper = mean(x) + qt(0.975, n - 1) * se)
  })) |> mutate(hit = ifelse(lower <= mu & upper >= mu, "contains \u03bc", "misses \u03bc"))
  ggplot(d, aes(y = sample)) +
    geom_vline(xintercept = mu, color = wu_red, linewidth = 0.8) +
    geom_errorbarh(aes(xmin = lower, xmax = upper, color = hit), height = 0, linewidth = 0.7) +
    geom_point(aes(x = mean, color = hit), size = 1.2) +
    scale_color_manual(values = c(wu_blue, wu_orange), name = NULL) +
    labs(title = sprintf("100 samples, 100 confidence intervals: %d contain the true mean",
                         sum(d$hit == "contains \u03bc")),
         x = "Hours per month", y = "Sample") +
    theme_course(base_size)
}

fig_ci_levels <- function(pop = pop_normal(), n = 100, base_size = 12) {
  set.seed(6789)
  x <- sample(pop, n); se <- sd(x) / sqrt(n)
  d <- data.frame(level = c("90%", "95%", "99%"), conf = c(0.90, 0.95, 0.99)) |>
    mutate(crit = qt(1 - (1 - conf) / 2, n - 1),
           lower = mean(x) - crit * se, upper = mean(x) + crit * se,
           level = factor(level, levels = rev(level)))
  ggplot(d, aes(y = level)) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.25, color = wu_blue, linewidth = 1) +
    geom_point(aes(x = mean(x)), color = wu_dark, size = 3) +
    geom_text(aes(x = upper, label = sprintf("[%.1f; %.1f]", lower, upper)), hjust = -0.15,
              size = base_size / 3.3) +
    coord_cartesian(xlim = c(mean(x) - 4.5, mean(x) + 5.5)) +
    labs(title = "The same sample, three confidence levels",
         x = "Hours per month", y = "Confidence level") +
    theme_course(base_size)
}

# --- 3.1 / 3.7: the NBC News poll of September 2024 (own illustration) ---

# What the news reported: the two vote shares
fig_poll_result <- function(pA = 0.49, pB = 0.44, nameA = "Harris", nameB = "Trump",
                            base_size = 13) {
  d <- data.frame(cand = factor(c(nameA, nameB), levels = c(nameB, nameA)),
                  p = c(pA, pB))
  ggplot(d, aes(y = cand, x = 100 * p, fill = cand)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = sprintf("%.0f%%", 100 * p)), hjust = -0.25,
              fontface = "bold", size = base_size / 3) +
    scale_fill_manual(values = setNames(c(wu_blue, wu_orange), c(nameA, nameB))) +
    scale_x_continuous(limits = c(0, 60), expand = c(0, 0)) +
    labs(title = "NBC News poll, September 13–17, 2024",
         subtitle = "1,000 registered voters · margin of error ± 3.1 percentage points",
         x = "Share of the vote (%)", y = NULL) +
    theme_course(base_size) +
    theme(legend.position = "none", panel.grid.major.y = element_blank())
}

# The same poll with 95% confidence intervals
fig_poll <- function(pA = 0.49, pB = 0.44, n = 1000,
                     nameA = "Harris", nameB = "Trump", base_size = 13) {
  me <- function(p) qnorm(0.975) * sqrt(p * (1 - p) / n)
  d <- data.frame(cand = factor(c(nameA, nameB), levels = c(nameB, nameA)), p = c(pA, pB)) |>
    mutate(lo = p - me(p), hi = p + me(p))
  ggplot(d, aes(y = cand, x = 100 * p)) +
    geom_errorbarh(aes(xmin = 100 * lo, xmax = 100 * hi), height = 0.2, color = wu_blue, linewidth = 1.1) +
    geom_point(size = 4, color = wu_dark) +
    geom_text(aes(label = sprintf("%.0f%%", 100 * p)), vjust = -1.2, fontface = "bold") +
    labs(title = sprintf("A poll of %s voters: 95%% confidence intervals", format(n, big.mark = ",")),
         subtitle = "The intervals overlap — but that alone does not settle whether the lead is real",
         x = "Share of the vote (%)", y = NULL) +
    theme_course(base_size)
}

fig_moe_by_n <- function(base_size = 13) {
  d <- data.frame(n = 50:3000) |> mutate(me = 100 * qnorm(0.975) * sqrt(0.25 / n))
  marks <- data.frame(n = c(120, 400, 1000), lab = c("Your group project", "", "Typical national poll")) |>
    mutate(me = 100 * qnorm(0.975) * sqrt(0.25 / n))
  ggplot(d, aes(n, me)) +
    geom_line(color = wu_blue, linewidth = 1.1) +
    geom_point(data = marks, color = wu_dark, size = 3) +
    geom_text(data = marks, aes(label = sprintf("n = %s: \u00b1%.1f points\n%s", format(n, big.mark = ","), me, lab)),
              hjust = -0.08, vjust = -0.25, size = base_size / 3.6, lineheight = 0.9) +
    coord_cartesian(ylim = c(0, 14)) +
    labs(title = "Margin of error for a share of about 50%",
         x = "Sample size n", y = "Margin of error (percentage points)") +
    theme_course(base_size)
}

# --- 3.1: the motivating A/B test -------------------------------------
# Simulated A/B test at a streaming service: 400 randomly selected users,
# half of them see the new "Smart Mix" feature. Outcome: listening hours
# in the four weeks after the start of the test.
ab_data <- function() {
  set.seed(31)
  tibble::tibble(
    group = factor(rep(c("Control", "Smart Mix"), each = 200), levels = c("Smart Mix", "Control")),
    hours = c(rgamma(200, shape = 2, scale = 10), rgamma(200, shape = 2, scale = 11.5))
  )
}

fig_ab_test <- function(ab = ab_data(), base_size = 13) {
  m <- ab |> group_by(group) |> summarise(mean = mean(hours))
  ggplot(ab, aes(x = group, y = hours)) +
    geom_jitter(width = 0.18, height = 0, alpha = 0.35, color = wu_grey, size = 1.6) +
    geom_point(data = m, aes(y = mean), color = wu_blue, size = 5) +
    geom_text(data = m, aes(y = mean, label = sprintf("mean = %.1f", mean)),
              nudge_x = 0.33, color = wu_blue, fontface = "bold", size = base_size / 3.2) +
    coord_flip() +
    labs(x = NULL, y = "Listening hours in the four weeks after the start of the test") +
    theme_course(base_size)
}

# --- 3.1: the 2016 polling miss in three key states (own illustration) ---
fig_polls_2016 <- function(base_size = 13) {
  d <- data.frame(
    state  = c("Wisconsin", "Michigan", "Pennsylvania"),
    polls  = c(6.5, 3.4, 1.9),     # final polling average: Clinton lead (points)
    result = c(-0.8, -0.2, -0.7)   # election result: Clinton margin (points)
  )
  d_long <- tidyr::pivot_longer(d, c(polls, result), names_to = "what", values_to = "margin")
  ggplot(d, aes(y = state)) +
    geom_vline(xintercept = 0, color = "grey50") +
    geom_segment(aes(x = polls, xend = result, yend = state), color = "grey70", linewidth = 1.5) +
    geom_point(data = d_long, aes(x = margin, color = what), size = 4) +
    scale_color_manual(values = c(polls = wu_blue, result = wu_red),
                       labels = c("Final polling average", "Election result"), name = NULL) +
    annotate("text", x = c(-2.5, 4), y = 3.45, label = c("Trump ahead", "Clinton ahead"),
             color = "grey40", size = base_size / 3.6) +
    labs(title = "2016: the polls missed in the decisive states",
         x = "Clinton's lead over Trump (percentage points)", y = NULL) +
    coord_cartesian(xlim = c(-3, 8), ylim = c(0.7, 3.5), clip = "off") +
    theme_course(base_size)
}

# --- 3.1: many polls, one truth — sampling variation (simulated) --------
fig_poll_variation <- function(base_size = 13) {
  set.seed(2016)
  days <- seq(as.Date("2016-06-01"), as.Date("2016-11-07"), by = "day")
  t <- seq(0, 1, length.out = length(days))
  truth <- data.frame(
    date = days,
    Clinton = 43 + 2.5 * t + 0.8 * sin(2 * pi * t * 1.5),
    Trump   = 37 + 4.5 * t - 0.6 * sin(2 * pi * t * 1.2)
  )
  polls <- do.call(rbind, lapply(seq_along(days), function(i) {
    k <- rpois(1, 1.2)
    if (k == 0) return(NULL)
    n <- round(runif(k, 600, 1500))
    data.frame(date = days[i], n = n,
               Clinton = 100 * rbinom(k, n, truth$Clinton[i] / 100) / n,
               Trump   = 100 * rbinom(k, n, truth$Trump[i] / 100) / n)
  }))
  long_p <- tidyr::pivot_longer(polls, c(Clinton, Trump), names_to = "cand", values_to = "share")
  long_t <- tidyr::pivot_longer(truth, c(Clinton, Trump), names_to = "cand", values_to = "share")
  me <- 100 * 1.96 * sqrt(0.45 * 0.55 / 1000)
  day <- as.Date("2016-09-15")
  ggplot() +
    geom_ribbon(data = long_t, aes(date, ymin = share - me, ymax = share + me, fill = cand), alpha = 0.15) +
    geom_point(data = long_p, aes(date, share, color = cand), alpha = 0.55, size = 1.4) +
    geom_line(data = long_t, aes(date, share, color = cand), linewidth = 1.2) +
    annotate("rect", xmin = day - 3, xmax = day + 3, ymin = 30, ymax = 54, fill = NA,
             color = wu_dark, linetype = "dashed") +
    annotate("text", x = day, y = 55.5, label = "same week —\ndifferent results", size = base_size / 3.6,
             color = wu_dark, lineheight = 0.9) +
    scale_color_manual(values = c(Clinton = wu_blue, Trump = wu_orange), name = NULL) +
    scale_fill_manual(values = c(Clinton = wu_blue, Trump = wu_orange), guide = "none") +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    coord_cartesian(ylim = c(30, 57)) +
    labs(title = "Many polls, one truth (simulated)",
         subtitle = "Dots: individual polls of 600–1,500 voters · lines: true support · bands: ± 3.1 points around the truth",
         x = NULL, y = "Share of the vote (%)") +
    theme_course(base_size)
}

# ------------------------------------------------------------------
# Additions to R/ch03-figures.R (October 2026)
# 1. Paste the three new functions at the end of ch03-figures.R.
# 2. Replace the existing fig_ci_100() with the version at the bottom
#    (it now chooses the seed so that exactly 5 of 100 intervals miss).
# ------------------------------------------------------------------

# --- 3.1 / 3.8: bar charts of the A/B test, with and without intervals ---
# Group means as bars; with ci = TRUE, 95% confidence intervals per group
fig_ab_bars <- function(ab = ab_data(), ci = FALSE, base_size = 13) {
  m <- ab |> group_by(group) |>
    summarise(n = n(), mean = mean(hours), se = sd(hours) / sqrt(n), .groups = "drop") |>
    mutate(lower = mean - qt(0.975, n - 1) * se, upper = mean + qt(0.975, n - 1) * se)
  p <- ggplot(m, aes(x = group, y = mean, fill = group)) +
    geom_col(width = 0.55) +
    scale_fill_manual(values = c(`Smart Mix` = wu_blue, Control = wu_grey), guide = "none") +
    scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
    labs(x = NULL, y = "Mean listening hours (four weeks)") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
  if (ci) {
    p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.9, color = "grey20") +
      geom_text(aes(y = upper, label = sprintf("%.1f  [%.1f; %.1f]", mean, lower, upper)), vjust = -0.7,
                size = base_size / 3.4) +
      labs(subtitle = "Bars: group means · error bars: 95% confidence intervals")
  } else {
    p + geom_text(aes(label = sprintf("%.1f", mean)), vjust = -0.6, fontface = "bold", size = base_size / 3)
  }
}

# Every user as a dot, with the mean (blue dot) and the median (orange diamond)
fig_ab_points <- function(ab = ab_data(), base_size = 13) {
  m <- ab |> group_by(group) |> summarise(mean = mean(hours), median = median(hours), .groups = "drop")
  ggplot(ab, aes(x = group, y = hours)) +
    geom_jitter(width = 0.18, height = 0, alpha = 0.35, color = wu_grey, size = 1.6) +
    geom_point(data = m, aes(y = median), color = wu_orange, shape = 18, size = 5.5) +
    geom_point(data = m, aes(y = mean), color = wu_blue, size = 5) +
    geom_text(data = m, aes(y = mean, label = sprintf("mean %.1f", mean)), nudge_x = 0.33, hjust = 0.2,
              color = wu_blue, fontface = "bold", size = base_size / 3.4) +
    geom_text(data = m, aes(y = median, label = sprintf("median %.1f", median)), nudge_x = -0.33, hjust = 0.8,
              color = wu_orange, fontface = "bold", size = base_size / 3.4) +
    coord_flip() +
    labs(x = NULL, y = "Listening hours in the four weeks after the start of the test") +
    theme_course(base_size)
}

# --- 3.8: a share with its confidence interval as a bar ---------------------
fig_share_bar <- function(x, n, label = "Students listening > 30 hours", base_size = 13) {
  p_hat <- x / n
  se <- sqrt(p_hat * (1 - p_hat) / n)
  d <- data.frame(label = label, p = 100 * p_hat, lower = 100 * (p_hat - 1.96 * se), upper = 100 * (p_hat + 1.96 * se))
  ggplot(d, aes(x = label, y = p)) +
    geom_col(width = 0.45, fill = wu_blue) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.12, linewidth = 0.9, color = "grey20") +
    geom_text(aes(y = upper, label = sprintf("%.0f%%  [%.0f%%; %.0f%%]", p, lower, upper)), vjust = -0.7, size = base_size / 3.3) +
    scale_y_continuous(limits = c(0, 45), expand = c(0, 0)) +
    labs(x = NULL, y = "Share (%) with 95% confidence interval") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
}


# --- REPLACEMENT for fig_ci_100() -------------------------------------
fig_ci_100 <- function(pop = pop_normal(), n = 100, misses = 5, base_size = 12) {
  # Draws 100 samples; the seed is chosen so that exactly `misses` intervals miss
  # the true mean (the textbook case), which keeps the figure easy to read.
  mu <- mean(pop)
  draw <- function(seed) {
    set.seed(seed)
    bind_rows(lapply(1:100, function(i) {
      x <- sample(pop, n)
      se <- sd(x) / sqrt(n)
      data.frame(sample = i, mean = mean(x), lower = mean(x) - qt(0.975, n - 1) * se,
                 upper = mean(x) + qt(0.975, n - 1) * se)
    })) |> mutate(hit = ifelse(lower <= mu & upper >= mu, "contains \u03bc", "misses \u03bc"))
  }
  seed <- 1
  repeat { d <- draw(seed); if (sum(d$hit == "misses \u03bc") == misses || seed > 500) break; seed <- seed + 1 }
  ggplot(d, aes(y = sample)) +
    geom_vline(xintercept = mu, color = wu_red, linewidth = 0.8) +
    geom_errorbarh(aes(xmin = lower, xmax = upper, color = hit), height = 0, linewidth = 0.7) +
    geom_point(aes(x = mean, color = hit), size = 1.2) +
    scale_color_manual(values = c(wu_blue, wu_orange), name = NULL) +
    labs(title = sprintf("100 samples, 100 confidence intervals: %d contain the true mean",
                         sum(d$hit == "contains \u03bc")),
         x = "Hours per month", y = "Sample") +
    theme_course(base_size)
}

# --- 3.8: the poll's vote shares as bars, with or without intervals ----------
fig_poll_bars <- function(pA = 0.49, pB = 0.44, n = 1000, nameA = "Harris", nameB = "Trump",
                          ci = TRUE, base_size = 13) {
  d <- data.frame(cand = factor(c(nameA, nameB), levels = c(nameA, nameB)), p = c(pA, pB)) |>
    mutate(se = sqrt(p * (1 - p) / n), lower = 100 * (p - 1.96 * se), upper = 100 * (p + 1.96 * se), p = 100 * p)
  g <- ggplot(d, aes(x = cand, y = p, fill = cand)) +
    geom_col(width = 0.55) +
    scale_fill_manual(values = setNames(c(wu_blue, wu_orange), c(nameA, nameB)), guide = "none") +
    scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
    labs(x = NULL, y = "Share of the vote (%)") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
  if (ci) {
    g + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.9, color = "grey20") +
      geom_text(aes(y = upper, label = sprintf("%.0f%%  [%.0f%%; %.0f%%]", p, lower, upper)), vjust = -0.7,
                fontface = "bold", size = base_size / 3.3) +
      labs(subtitle = "1,000 voters · bars: shares · error bars: 95% confidence intervals")
  } else {
    g + geom_text(aes(label = sprintf("%.0f%%", p)), vjust = -0.6, fontface = "bold", size = base_size / 3)
  }
}