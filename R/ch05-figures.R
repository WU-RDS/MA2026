# ------------------------------------------------------------------
# ch05-figures.R
# Shared figure functions for Chapter 5 (book chapter AND slide deck).
# Data: data/smartmix_obs.csv, data/campaigns.csv (synthetic; see
# R/ch05-make-data.R) and data/playlist_did.csv (real; WU-RDS/did).
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

# --- data loaders (paths relative to the project root; the slide deck
#     sets `data_dir <- "../data"`) ------------------------------------
data_dir <- if (exists("data_dir")) data_dir else "data"
load_smartmix  <- function() readr::read_csv(file.path(data_dir, "smartmix_obs.csv"), show_col_types = FALSE) |>
  mutate(device = factor(device, levels = c("iOS", "Android", "Desktop")),
         smart_mix_f = factor(smart_mix, levels = c(0, 1), labels = c("Not adopted", "Adopted")))
load_campaigns <- function() readr::read_csv(file.path(data_dir, "campaigns.csv"), show_col_types = FALSE) |>
  mutate(genre = factor(genre, levels = c("Pop", "HipHop/Rap", "Rock", "Electro/Dance")))
load_playlist  <- function() readr::read_csv(file.path(data_dir, "playlist_did.csv"), show_col_types = FALSE) |>
  mutate(week = as.Date(week),
         group = factor(treated, levels = c(0, 1), labels = c("Control songs", "Playlisted songs")))

# --- 5.1: the naive comparison and its reason --------------------------
fig_obs_naive <- function(d = load_smartmix(), base_size = 13) {
  m <- d |> group_by(smart_mix_f) |> summarise(mean = mean(hours), n = n())
  ggplot(d, aes(x = smart_mix_f, y = hours)) +
    geom_jitter(width = 0.2, height = 0, alpha = 0.15, color = wu_grey, size = 1.1) +
    geom_point(data = m, aes(y = mean), color = wu_blue, size = 5) +
    geom_text(data = m, aes(y = mean, label = sprintf("mean = %.1f", mean)), nudge_x = 0.38,
              color = wu_blue, fontface = "bold", size = base_size / 3.2) +
    coord_flip(ylim = c(0, 90)) +
    labs(x = NULL, y = "Listening hours in the four weeks after the launch",
         subtitle = sprintf("Smart Mix adopted by %d of %d users (opt-in)", m$n[2], sum(m$n))) +
    theme_course(base_size)
}

fig_prior_by_adoption <- function(d = load_smartmix(), base_size = 13) {
  m <- d |> group_by(smart_mix_f) |> summarise(mean = mean(prior_hours))
  ggplot(d, aes(x = prior_hours, fill = smart_mix_f)) +
    geom_density(alpha = 0.45, color = NA) +
    geom_vline(data = m, aes(xintercept = mean, color = smart_mix_f), linewidth = 1, linetype = "dashed") +
    scale_fill_manual(values = c(wu_grey, wu_blue), name = NULL) +
    scale_color_manual(values = c(wu_grey, wu_blue), name = NULL) +
    coord_cartesian(xlim = c(0, 70)) +
    labs(title = "Adopters already listened more before Smart Mix existed",
         x = "Listening hours in the month before the launch", y = "Density") +
    theme_course(base_size)
}

fig_adoption_dag <- function(base_size = 13) {
  nodes <- data.frame(name = c("Music enthusiasm\n(unobserved)", "Smart Mix\n(opt-in)", "Listening hours"),
                      x = c(1.5, 0, 3), y = c(1, 0, 0))
  edges <- data.frame(x = c(1.25, 1.75, 0.55), y = c(0.85, 0.85, 0), xend = c(0.35, 2.65, 2.45), yend = c(0.2, 0.2, 0),
                      type = c("confounding", "confounding", "effect"))
  ggplot() +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, color = type, linetype = type),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"), linewidth = 1) +
    geom_label(data = nodes, aes(x, y, label = name), size = base_size / 3, fill = "white",
               label.padding = unit(0.5, "lines"), lineheight = 0.9) +
    scale_color_manual(values = c(confounding = wu_red, effect = wu_blue), guide = "none") +
    scale_linetype_manual(values = c(confounding = "dashed", effect = "solid"), guide = "none") +
    coord_cartesian(xlim = c(-0.6, 3.6), ylim = c(-0.4, 1.4)) +
    theme_void()
}

# --- 5.2: from a scatter plot to a line ---------------------------------
fig_scatter_prior <- function(d = load_smartmix(), base_size = 13, line = TRUE) {
  p <- ggplot(d, aes(prior_hours, hours)) +
    geom_point(alpha = 0.25, color = wu_grey, size = 1.2) +
    coord_cartesian(xlim = c(0, 80), ylim = c(0, 100)) +
    labs(x = "Listening hours before the launch", y = "Listening hours after the launch",
         subtitle = sprintf("Correlation r = %.2f", cor(d$prior_hours, d$hours))) +
    theme_course(base_size)
  if (line) p <- p + geom_smooth(method = "lm", se = FALSE, color = wu_blue, linewidth = 1.2)
  p
}

fig_least_squares <- function(d = load_smartmix(), base_size = 12, seed = 1) {
  set.seed(seed)
  s <- d |> filter(prior_hours < 60, hours < 80) |> slice_sample(n = 25)
  fit <- lm(hours ~ prior_hours, data = s)
  lines <- data.frame(a = c(coef(fit)[1], 2, 12), b = c(coef(fit)[2], 1.3, 0.5),
                      label = c("Least-squares line", "A steeper guess", "A flatter guess"))
  lines$sse <- sapply(1:3, function(i) sum((s$hours - (lines$a[i] + lines$b[i] * s$prior_hours))^2))
  lines$label <- sprintf("%s: sum of squared residuals = %s", lines$label, format(round(lines$sse), big.mark = ","))
  res <- bind_rows(lapply(1:3, function(i) data.frame(label = lines$label[i], x = s$prior_hours, y = s$hours,
                                                      yhat = lines$a[i] + lines$b[i] * s$prior_hours)))
  res$label <- factor(res$label, levels = lines$label)
  ggplot(res, aes(x, y)) +
    geom_segment(aes(xend = x, yend = yhat), color = wu_orange, alpha = 0.8) +
    geom_abline(data = lines |> mutate(label = factor(label, levels = label)), aes(intercept = a, slope = b),
                color = wu_blue, linewidth = 1) +
    geom_point(color = wu_dark, size = 1.8) +
    facet_wrap(~label, ncol = 1) +
    labs(x = "Listening hours before the launch", y = "Listening hours after the launch") +
    theme_course(base_size) + theme(strip.text = element_text(hjust = 0))
}

# --- 5.4: naive vs. adjusted vs. experiment -----------------------------
fig_estimates <- function(d = load_smartmix(), base_size = 13) {
  m0 <- lm(hours ~ smart_mix, d)
  m1 <- lm(hours ~ smart_mix + prior_hours, d)
  m2 <- lm(hours ~ smart_mix + prior_hours + tenure + premium + device, d)
  est <- function(m, lab) data.frame(model = lab, est = coef(m)[2], lo = confint(m)[2, 1], hi = confint(m)[2, 2])
  res <- bind_rows(est(m0, "Naive comparison\n(observational)"),
                   est(m1, "+ listening before launch"),
                   est(m2, "+ tenure, Premium, device"),
                   data.frame(model = "Randomized experiment\n(Chapter 4)", est = 3.24, lo = 0.29, hi = 6.20))
  res$model <- factor(res$model, levels = rev(res$model))
  ggplot(res, aes(y = model, x = est)) +
    geom_vline(xintercept = 3, color = wu_red, linetype = "dashed") +
    annotate("text", x = 3, y = "Naive comparison\n(observational)", label = "true effect = 3", color = wu_red,
             size = base_size / 3.4, hjust = -0.1, vjust = -2.6) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, color = wu_blue, linewidth = 1) +
    geom_point(size = 3.5, color = wu_dark) +
    geom_text(aes(label = sprintf("%.1f", est)), vjust = -1.1, size = base_size / 3.3) +
    coord_cartesian(xlim = c(0, 11), clip = "off") +
    labs(x = "Estimated effect of Smart Mix on listening hours (95% CI)", y = NULL) +
    theme_course(base_size)
}

# --- 5.6: prediction out of sample --------------------------------------
fig_prediction <- function(d = load_smartmix(), base_size = 13, seed = 42) {
  set.seed(seed)
  idx <- sample(nrow(d), 1500)
  train <- d[idx, ]; test <- d[-idx, ]
  m <- lm(hours ~ prior_hours + tenure + premium + device + smart_mix, train)
  test$pred <- predict(m, test)
  rmse <- sqrt(mean((test$hours - test$pred)^2))
  ggplot(test, aes(pred, hours)) +
    geom_abline(intercept = 0, slope = 1, color = wu_red, linetype = "dashed") +
    geom_point(alpha = 0.35, color = wu_blue, size = 1.3) +
    coord_cartesian(xlim = c(0, 70), ylim = c(0, 90)) +
    labs(x = "Predicted listening hours", y = "Actual listening hours",
         subtitle = sprintf("500 users not used for fitting the model · typical prediction error (RMSE): %.1f hours", rmse)) +
    theme_course(base_size)
}

# --- 5.7: functional form ------------------------------------------------
fig_campaign_fits <- function(c2 = load_campaigns(), base_size = 12) {
  lin <- lm(streams ~ spend, c2)
  ll  <- lm(log(streams) ~ log(spend), c2)
  grid <- data.frame(spend = seq(200, 60000, length.out = 300))
  grid$linear <- predict(lin, grid)
  grid$loglog <- exp(predict(ll, grid))
  long <- pivot_longer(grid, c(linear, loglog), names_to = "model", values_to = "streams") |>
    mutate(model = factor(model, levels = c("linear", "loglog"), labels = c("Linear model: streams = a + b × spend", "Log-log model: log(streams) = a + b × log(spend)")))
  p1 <- ggplot(c2, aes(spend, streams)) +
    geom_point(alpha = 0.3, color = wu_grey, size = 1.2) +
    geom_line(data = long, aes(color = model), linewidth = 1.2) +
    scale_color_manual(values = c(wu_orange, wu_blue), name = NULL) +
    scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale(), prefix = "€")) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    coord_cartesian(xlim = c(0, 40000), ylim = c(0, 700000)) +
    labs(x = "Promotion spend", y = "Streams (first four weeks)", title = "Original scale") +
    theme_course(base_size) + guides(color = guide_legend(nrow = 2))
  p2 <- ggplot(c2, aes(log(spend), log(streams))) +
    geom_point(alpha = 0.3, color = wu_grey, size = 1.2) +
    geom_smooth(method = "lm", se = FALSE, color = wu_blue, linewidth = 1.2) +
    labs(x = "log(promotion spend)", y = "log(streams)", title = "Both axes logged: a straight line") +
    theme_course(base_size)
  p1 + p2
}

fig_residuals_form <- function(c2 = load_campaigns(), base_size = 12) {
  lin <- lm(streams ~ spend + followers + placements, c2)
  ll  <- lm(log(streams) ~ log(spend) + log(followers) + placements, c2)
  d <- bind_rows(data.frame(model = "Linear model", fitted = fitted(lin), resid = resid(lin)),
                 data.frame(model = "Log-log model", fitted = fitted(ll), resid = resid(ll)))
  ggplot(d, aes(fitted, resid)) +
    geom_hline(yintercept = 0, color = wu_red, linetype = "dashed") +
    geom_point(alpha = 0.3, color = wu_grey, size = 1.1) +
    geom_smooth(se = FALSE, color = wu_blue, method = "loess", formula = y ~ x) +
    facet_wrap(~model, scales = "free") +
    labs(x = "Fitted values", y = "Residuals") +
    theme_course(base_size)
}

fig_functional_forms <- function(base_size = 12) {
  x <- seq(1, 10, length.out = 200)
  d <- bind_rows(
    data.frame(form = "Level-level: y = a + b·x", x, y = 2 + 0.8 * x),
    data.frame(form = "Log-level: log(y) = a + b·x", x, y = exp(0.5 + 0.2 * x)),
    data.frame(form = "Level-log: y = a + b·log(x)", x, y = 2 + 3 * log(x)),
    data.frame(form = "Log-log: log(y) = a + b·log(x)", x, y = 3 * x^0.5)
  )
  d$form <- factor(d$form, levels = unique(d$form))
  ggplot(d, aes(x, y)) +
    geom_line(color = wu_blue, linewidth = 1.2) +
    facet_wrap(~form, scales = "free_y", nrow = 1) +
    labs(x = "x", y = "y") +
    theme_course(base_size) + theme(axis.text = element_blank())
}

# --- 5.8: logistic regression ---------------------------------------------
fig_logistic <- function(d = load_smartmix(), base_size = 13) {
  lp <- lm(churn ~ hours, d)
  lg <- glm(churn ~ hours, family = binomial, d)
  grid <- data.frame(hours = seq(0, 90, length.out = 300))
  grid$linear <- predict(lp, grid)
  grid$logistic <- predict(lg, grid, type = "response")
  long <- pivot_longer(grid, c(linear, logistic), names_to = "model", values_to = "p") |>
    mutate(model = factor(model, labels = c("Linear model", "Logistic model")))
  bins <- d |> mutate(bin = cut(hours, breaks = seq(0, 90, 10), include.lowest = TRUE)) |>
    group_by(bin) |> summarise(hours = mean(hours), share = mean(churn), n = n()) |> filter(n >= 10)
  ggplot() +
    geom_jitter(data = d, aes(hours, churn), width = 0, height = 0.03, alpha = 0.08, color = wu_grey, size = 1) +
    geom_point(data = bins, aes(hours, share), color = wu_dark, size = 3) +
    geom_line(data = long, aes(hours, p, color = model), linewidth = 1.2) +
    scale_color_manual(values = c(wu_orange, wu_blue), name = NULL) +
    coord_cartesian(xlim = c(0, 90), ylim = c(-0.1, 1.1)) +
    labs(x = "Listening hours", y = "Churn (0 = stayed, 1 = cancelled)",
         subtitle = "Dark dots: share of churners per 10-hour bin") +
    theme_course(base_size)
}

# --- 5.9: diagnostics ----------------------------------------------------
fig_influence <- function(base_size = 12) {
  set.seed(9)
  x <- runif(30, 1, 10); y <- 2 + 0.5 * x + rnorm(30, 0, 1)
  d <- data.frame(x, y, outlier = FALSE)
  d2 <- rbind(d, data.frame(x = 16, y = 2, outlier = TRUE))
  f1 <- lm(y ~ x, d); f2 <- lm(y ~ x, d2)
  ggplot(d2, aes(x, y)) +
    geom_abline(intercept = coef(f1)[1], slope = coef(f1)[2], color = wu_blue, linewidth = 1.1) +
    geom_abline(intercept = coef(f2)[1], slope = coef(f2)[2], color = wu_orange, linewidth = 1.1, linetype = "dashed") +
    geom_point(aes(color = outlier), size = 2.5) +
    scale_color_manual(values = c(`FALSE` = wu_grey, `TRUE` = wu_red), guide = "none") +
    annotate("text", x = 12.5, y = 7.6, label = "line without the outlier", color = wu_blue, size = base_size / 3.4) +
    annotate("text", x = 12.5, y = 4.3, label = "line with the outlier", color = wu_orange, size = base_size / 3.4) +
    annotate("text", x = 16, y = 1.2, label = "one influential\nobservation", color = wu_red, size = base_size / 3.4, lineheight = 0.9) +
    labs(x = "x", y = "y") +
    theme_course(base_size)
}

fig_heteroskedasticity <- function(c2 = load_campaigns(), base_size = 12) {
  lin <- lm(streams ~ spend + followers + placements, c2)
  d <- data.frame(fitted = fitted(lin), resid = resid(lin))
  ggplot(d, aes(fitted, resid)) +
    geom_hline(yintercept = 0, color = wu_red, linetype = "dashed") +
    geom_point(alpha = 0.3, color = wu_grey, size = 1.1) +
    scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    labs(x = "Fitted values (linear model)", y = "Residuals",
         subtitle = "The spread of the residuals grows with the fitted values — a funnel") +
    theme_course(base_size)
}

# --- 5.10: difference-in-differences ---------------------------------------
fig_did_trends <- function(p = load_playlist(), base_size = 13) {
  m <- p |> group_by(group, week) |> summarise(mean_log = mean(log(streams + 1)), .groups = "drop")
  treat_week <- as.Date("2018-03-19")
  pre_c  <- m |> filter(group == "Control songs", week < treat_week)
  post_c <- m |> filter(group == "Control songs", week > treat_week)
  shift  <- mean((m |> filter(group == "Playlisted songs", week < treat_week))$mean_log) - mean(pre_c$mean_log)
  cf <- m |> filter(group == "Control songs") |> mutate(mean_log = mean_log + shift, group = "Counterfactual")
  ggplot(m, aes(week, mean_log, color = group)) +
    annotate("rect", xmin = treat_week - 3, xmax = treat_week + 3, ymin = -Inf, ymax = Inf, fill = "grey90") +
    geom_line(data = cf |> filter(week > treat_week), linetype = "dotted", linewidth = 1, color = wu_blue) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.6) +
    scale_color_manual(values = c(`Control songs` = wu_grey, `Playlisted songs` = wu_blue), name = NULL) +
    annotate("text", x = treat_week, y = 10.35, label = "playlist\nlisting", size = base_size / 3.4, lineheight = 0.9, color = wu_grey) +
    scale_x_date(date_labels = "%b %d", date_breaks = "3 weeks") +
    labs(x = NULL, y = "Mean log(streams + 1) per week",
         subtitle = "Dotted: the playlisted songs' counterfactual — the control songs' trend shifted up by the pre-period gap") +
    theme_course(base_size)
}

fig_did_four <- function(p = load_playlist(), base_size = 13) {
  d <- p |> filter(week != as.Date("2018-03-19")) |>
    mutate(period = factor(post, levels = c(0, 1), labels = c("Before", "After"))) |>
    group_by(group, period) |> summarise(mean_log = mean(log(streams + 1)), .groups = "drop")
  cf <- data.frame(group = "Playlisted songs", period = factor(c("Before", "After"), levels = c("Before", "After")),
                   mean_log = c(d$mean_log[d$group == "Playlisted songs" & d$period == "Before"],
                                d$mean_log[d$group == "Playlisted songs" & d$period == "Before"] +
                                  diff(d$mean_log[d$group == "Control songs"])))
  ggplot(d, aes(period, mean_log, group = group, color = group)) +
    geom_line(data = cf, linetype = "dotted", linewidth = 1, color = wu_blue) +
    geom_line(linewidth = 1.2) + geom_point(size = 3.5) +
    geom_text(aes(label = sprintf("%.2f", mean_log)), vjust = -1, size = base_size / 3.3, show.legend = FALSE) +
    annotate("segment", x = 2.08, xend = 2.08, y = cf$mean_log[2], yend = d$mean_log[d$group == "Playlisted songs" & d$period == "After"],
             arrow = arrow(ends = "both", length = unit(0.2, "cm")), color = wu_red) +
    annotate("text", x = 2.12, y = mean(c(cf$mean_log[2], d$mean_log[4])), label = "DiD\neffect", color = wu_red,
             hjust = 0, size = base_size / 3.4, lineheight = 0.9) +
    scale_color_manual(values = c(`Control songs` = wu_grey, `Playlisted songs` = wu_blue), name = NULL) +
    scale_x_discrete(expand = expansion(add = c(0.3, 0.6))) +
    coord_cartesian(ylim = c(9.5, 10.3)) +
    labs(x = NULL, y = "Mean log(streams + 1)") +
    theme_course(base_size)
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



# ------------------------------------------------------------------
# Additions to R/ch05-figures.R (November 2026): correlation, R-squared,
# logistic function. Paste at the end of ch05-figures.R.
# ------------------------------------------------------------------

# --- 5.2: what the correlation coefficient looks like --------------------------
fig_correlation_panels <- function(base_size = 12) {
  set.seed(7)
  rs <- c(-0.9, -0.5, 0, 0.3, 0.6, 0.95)
  d <- bind_rows(lapply(rs, function(r) {
    x <- rnorm(150); y <- r * x + sqrt(1 - r^2) * rnorm(150)
    data.frame(r = sprintf("r = %.2f", cor(x, y)), target = r, x, y)
  }))
  d$r <- factor(d$r, levels = unique(d$r))
  ggplot(d, aes(x, y)) +
    geom_point(alpha = 0.5, color = wu_blue, size = 1.2) +
    facet_wrap(~r, nrow = 2) +
    labs(x = "x", y = "y") +
    theme_course(base_size) + theme(axis.text = element_blank())
}

fig_correlation_nonlinear <- function(base_size = 12) {
  set.seed(8)
  x <- runif(200, -3, 3); y <- 4 - x^2 + rnorm(200, 0, 0.8)
  d <- data.frame(x, y)
  ggplot(d, aes(x, y)) +
    geom_point(alpha = 0.5, color = wu_blue, size = 1.2) +
    geom_smooth(method = "lm", se = FALSE, color = wu_orange, linewidth = 1) +
    labs(x = "x", y = "y", subtitle = sprintf("A clear relationship, but r = %.2f", cor(x, y))) +
    theme_course(base_size) + theme(axis.text = element_blank())
}

# --- 5.3: the same line, different amounts of scatter -> different R-squared -----
fig_r2_panels <- function(base_size = 12) {
  set.seed(9)
  targets <- c(0.05, 0.3, 0.6, 0.9)
  d <- bind_rows(lapply(targets, function(r2) {
    x <- runif(120, 0, 10); signal <- 2 + 0.8 * x
    noise_sd <- sqrt(var(signal) * (1 - r2) / r2)
    y <- signal + rnorm(120, 0, noise_sd)
    fit <- lm(y ~ x)
    data.frame(panel = sprintf("R\u00b2 = %.2f", summary(fit)$r.squared), x, y, fitted = fitted(fit))
  }))
  d$panel <- factor(d$panel, levels = unique(d$panel))
  ggplot(d, aes(x, y)) +
    geom_segment(aes(xend = x, yend = fitted), color = wu_orange, alpha = 0.35) +
    geom_point(alpha = 0.6, color = wu_grey, size = 1.1) +
    geom_line(aes(y = fitted), color = wu_blue, linewidth = 1.1) +
    facet_wrap(~panel, nrow = 1, scales = "free_y") +
    labs(x = "x", y = "y") +
    theme_course(base_size) + theme(axis.text = element_blank())
}

# --- 5.8: the logistic function for upgrades (free users) -----------------------
load_free_users <- function() load_smartmix() |> filter(premium == 0)

fig_logistic_upgrade <- function(d = load_free_users(), base_size = 13) {
  lp <- lm(upgraded ~ hours, d)
  lg <- glm(upgraded ~ hours, family = binomial, d)
  grid <- data.frame(hours = seq(0, 70, length.out = 300))
  grid$`Linear model` <- predict(lp, grid)
  grid$`Logistic model` <- predict(lg, grid, type = "response")
  long <- pivot_longer(grid, -hours, names_to = "model", values_to = "p")
  bins <- d |> mutate(bin = cut(hours, breaks = seq(0, 70, 5), include.lowest = TRUE)) |>
    group_by(bin) |> summarise(hours = mean(hours), share = mean(upgraded), n = n()) |> filter(n >= 8)
  ggplot() +
    geom_jitter(data = d, aes(hours, upgraded), width = 0, height = 0.03, alpha = 0.08, color = wu_grey, size = 1) +
    geom_point(data = bins, aes(hours, share), color = wu_dark, size = 3) +
    geom_line(data = long, aes(hours, p, color = model), linewidth = 1.2) +
    geom_hline(yintercept = c(0, 1), linetype = "dotted", color = "grey50") +
    scale_color_manual(values = c(`Linear model` = wu_orange, `Logistic model` = wu_blue), name = NULL) +
    coord_cartesian(xlim = c(0, 70), ylim = c(-0.15, 1.15)) +
    labs(x = "Listening hours", y = "Upgraded to Premium (0 = no, 1 = yes)",
         subtitle = "Free users · dark dots: share of upgrades per 5-hour bin") +
    theme_course(base_size)
}

# The predicted probabilities at a few listening times: one more hour changes the
# probability by different amounts, while the odds ratio stays the same
fig_logistic_steps <- function(d = load_free_users(), at = c(10, 20, 30, 40), base_size = 13) {
  lg <- glm(upgraded ~ hours, family = binomial, d)
  grid <- data.frame(hours = seq(0, 70, length.out = 300)); grid$p <- predict(lg, grid, type = "response")
  pts <- data.frame(hours = c(at, at + 1)); pts$p <- predict(lg, pts, type = "response")
  steps <- data.frame(x = at, p0 = predict(lg, data.frame(hours = at), type = "response"),
                      p1 = predict(lg, data.frame(hours = at + 1), type = "response"))
  steps$label <- sprintf("%d \u2192 %d h: %+.1f points", at, at + 1, 100 * (steps$p1 - steps$p0))
  ggplot(grid, aes(hours, p)) +
    geom_line(color = wu_blue, linewidth = 1.2) +
    geom_segment(data = steps, aes(x = x, xend = x + 1, y = p0, yend = p0), color = wu_red) +
    geom_segment(data = steps, aes(x = x + 1, xend = x + 1, y = p0, yend = p1), color = wu_red, linewidth = 1.2) +
    geom_point(data = pts, aes(hours, p), color = wu_dark, size = 2.2) +
    geom_text(data = steps, aes(x = x + 1.5, y = p0 + 0.03, label = label), hjust = 0, size = base_size / 3.6, color = wu_red) +
    coord_cartesian(xlim = c(0, 60), ylim = c(0, 1)) +
    labs(x = "Listening hours", y = "Predicted probability of an upgrade",
         subtitle = "One more hour: the same odds ratio everywhere, a different change in probability") +
    theme_course(base_size)
}



# ------------------------------------------------------------------
# Additions to R/ch05-figures.R (November 5, 2026): endogeneity figures.
# Paste at the end of ch05-figures.R.
# ------------------------------------------------------------------

# --- 5.4: observed variation = exogenous + endogenous variation ---------------
# Simulated: demand falls with price (the causal effect), but weather raises
# both price and demand. The observed cloud mixes the two components.
fig_variation_components <- function(base_size = 11) {
  set.seed(12)
  n <- 150
  weather <- rnorm(n)                                # unobserved
  price_exo <- rnorm(n, 0, 0.8)                      # price variation unrelated to weather
  price <- 10 + 1.5 * weather + price_exo
  demand <- 50 - 2.0 * (price - 10) + 6.0 * weather + rnorm(n, 0, 1.2)
  d <- tibble(price, demand, weather, price_exo)
  p_obs <- ggplot(d, aes(price, demand)) + geom_point(color = wu_blue, alpha = 0.7, size = 1.3) +
    geom_smooth(method = "lm", se = FALSE, color = wu_grey, linewidth = 0.9, formula = y ~ x) +
    labs(title = "Observed variation", subtitle = "The data: higher prices, higher demand?", x = "Price", y = "Demand") +
    theme_course(base_size) + theme(axis.text = element_blank())
  # exogenous part: price moves for reasons unrelated to weather -> demand responds causally
  d_exo <- d |> mutate(price = 10 + price_exo, demand = 50 - 2.0 * price_exo + rnorm(n, 0, 1.2))
  p_exo <- ggplot(d_exo, aes(price, demand)) + geom_point(color = wu_blue, alpha = 0.7, size = 1.3) +
    geom_smooth(method = "lm", se = FALSE, color = wu_grey, linewidth = 0.9, formula = y ~ x) +
    labs(title = "Exogenous variation", subtitle = "Price moves for its own reasons: the causal effect", x = "Price", y = "Demand") +
    theme_course(base_size) + theme(axis.text = element_blank())
  # endogenous part: price and demand both move with the weather
  d_end <- d |> mutate(price = 10 + 1.5 * weather, demand = 50 - 2.0 * (price - 10) + 6.0 * weather + rnorm(n, 0, 1.2))
  p_end <- ggplot(d_end, aes(price, demand)) + geom_point(color = wu_orange, alpha = 0.7, size = 1.3) +
    geom_smooth(method = "lm", se = FALSE, color = wu_grey, linewidth = 0.9, formula = y ~ x) +
    labs(title = "Endogenous variation", subtitle = "Weather moves price AND demand: not the effect", x = "Price", y = "Demand") +
    theme_course(base_size) + theme(axis.text = element_blank())
  p_obs | (p_exo / p_end)
}

# --- 5.4: a generic confounder DAG with custom labels ---------------------------
fig_confounder_dag <- function(x = "Price\n(cause, X)", y = "Sales\n(outcome, Y)", u = "Weather\n(unobserved confounder)", base_size = 13) {
  nodes <- data.frame(name = c(u, x, y), x = c(1.5, 0, 3), y = c(1, 0, 0))
  edges <- data.frame(x = c(1.25, 1.75, 0.55), y = c(0.85, 0.85, 0), xend = c(0.35, 2.65, 2.45), yend = c(0.2, 0.2, 0),
                      type = c("confounding", "confounding", "effect"))
  ggplot() +
    geom_segment(data = edges, aes(x, y, xend = xend, yend = yend, color = type, linetype = type),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"), linewidth = 1) +
    geom_label(data = nodes, aes(x, y, label = name), size = base_size / 3, fill = "white",
               label.padding = unit(0.5, "lines"), lineheight = 0.9) +
    scale_color_manual(values = c(confounding = wu_red, effect = wu_blue), guide = "none") +
    scale_linetype_manual(values = c(confounding = "dashed", effect = "solid"), guide = "none") +
    coord_cartesian(xlim = c(-0.6, 3.6), ylim = c(-0.4, 1.4)) +
    theme_void()
}

# --- 5.4: adjusting for a mediator blocks part of the effect ---------------------
fig_mediator_dag <- function(base_size = 12) {
  one <- function(title, blocked) {
    nodes <- data.frame(name = c("Smart Mix\n(X)", "New artists\ndiscovered (M)", "Listening\nhours (Y)"), x = c(0, 1.5, 3), y = c(0, 1, 0))
    col_ind <- if (blocked) "grey70" else wu_blue
    ggplot() +
      annotate("segment", x = 0.45, xend = 1.05, y = 0.25, yend = 0.8, arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = col_ind, linewidth = 1) +
      annotate("segment", x = 1.95, xend = 2.55, y = 0.8, yend = 0.25, arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = col_ind, linewidth = 1) +
      annotate("segment", x = 0.5, xend = 2.5, y = 0, yend = 0, arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = wu_blue, linewidth = 1) +
      annotate("text", x = 1.5, y = -0.22, label = if (blocked) "only the direct part remains" else "direct part", size = base_size / 3.4, color = wu_blue) +
      annotate("text", x = 1.5, y = 1.38, label = if (blocked) "indirect part: blocked" else "indirect part", size = base_size / 3.4, color = col_ind) +
      geom_label(data = nodes, aes(x, y, label = name), size = base_size / 3.2, fill = ifelse(nodes$name == "New artists\ndiscovered (M)" & blocked, "#fde9c9", "white"),
                 label.padding = unit(0.45, "lines"), lineheight = 0.9, inherit.aes = FALSE) +
      coord_cartesian(xlim = c(-0.6, 3.6), ylim = c(-0.45, 1.6)) +
      labs(title = title) + theme_void(base_size) + theme(plot.title = element_text(face = "bold", hjust = 0.5))
  }
  one("Total effect of Smart Mix", FALSE) | one("Adjusting for the mediator", TRUE)
}


# ------------------------------------------------------------------
# Additions to R/ch05-figures.R (November 8, 2026): functional form and
# logistic-regression intuition. Paste at the end of ch05-figures.R.
# ------------------------------------------------------------------

# --- 5.7: linear vs. multiplicative model, with their elasticities ---------
fig_elasticity_compare <- function(base_size = 11) {
  x <- seq(1, 10, length.out = 200)
  lin <- data.frame(x, y = 2 + 0.8 * x) |> mutate(el = 0.8 * x / y)
  mul <- data.frame(x, y = 2.5 * x^0.4) |> mutate(el = 0.4)
  d <- bind_rows(mutate(lin, model = "Linear model: y = b0 + b1 x"),
                 mutate(mul, model = "Multiplicative model: y = b0 * x^b1"))
  d$model <- factor(d$model, levels = unique(d$model))
  long <- d |> pivot_longer(c(y, el), names_to = "what", values_to = "value") |>
    mutate(what = factor(what, levels = c("y", "el"), labels = c("Sales y", "Elasticity of sales w.r.t. spend")),
           panel = interaction(what, model, sep = "\n", lex.order = TRUE))
  long$panel <- factor(long$panel, levels = c(paste("Sales y", levels(d$model)[1], sep = "\n"), paste("Sales y", levels(d$model)[2], sep = "\n"),
                                              paste("Elasticity of sales w.r.t. spend", levels(d$model)[1], sep = "\n"), paste("Elasticity of sales w.r.t. spend", levels(d$model)[2], sep = "\n")))
  ggplot(long, aes(x, value, color = what)) +
    geom_line(linewidth = 1.3) +
    facet_wrap(~panel, nrow = 2, scales = "free_y") +
    scale_color_manual(values = c(`Sales y` = wu_blue, `Elasticity of sales w.r.t. spend` = wu_orange), guide = "none") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Advertising spend x", y = NULL) +
    theme_course(base_size) + theme(strip.text = element_text(size = rel(0.95), lineheight = 0.9))
}

# --- 5.7: what the multiplicative model looks like for different elasticities
fig_power_shapes <- function(base_size = 12) {
  x <- seq(0.3, 4, length.out = 200)
  d <- bind_rows(
    data.frame(x, y = 2 * x^-0.7, b = "b1 < 0 (e.g. price)"),
    data.frame(x, y = 2 * x^0.4, b = "0 < b1 < 1 (diminishing returns)"),
    data.frame(x, y = 2 * x^1, b = "b1 = 1 (proportional)"),
    data.frame(x, y = 2 * x^1.6, b = "b1 > 1 (increasing returns)")
  )
  d$b <- factor(d$b, levels = unique(d$b))
  ggplot(d, aes(x, y, color = b, linetype = b)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c(wu_red, wu_blue, wu_grey, wu_orange), name = NULL) +
    scale_linetype_manual(values = c("dotdash", "solid", "solid", "dotted"), name = NULL) +
    coord_cartesian(ylim = c(0, 12)) +
    labs(x = "x", y = "y = b0 * x^b1") +
    theme_course(base_size) + theme(axis.text = element_blank(), legend.position = "right")
}

# --- 5.7: why the log transformation straightens the curve ----------------------
# Equal PERCENTAGE steps in x (doublings) produce equal PERCENTAGE steps in y;
# on log scales, equal percentage steps are equal distances -> a straight line
fig_log_intuition <- function(base_size = 11) {
  b0 <- 500; b1 <- 0.4
  x <- seq(500, 32000, length.out = 300); curve <- data.frame(x, y = b0 * x^b1)
  pts <- data.frame(x = 1000 * 2^(0:4)) |> mutate(y = b0 * x^b1)
  steps <- pts |> mutate(x_next = lead(x), y_next = lead(y)) |> filter(!is.na(x_next))
  p1 <- ggplot(curve, aes(x, y)) + geom_line(color = wu_blue, linewidth = 1.1) +
    geom_segment(data = steps, aes(x = x, xend = x_next, y = y, yend = y), color = wu_orange, linewidth = 0.8) +
    geom_segment(data = steps, aes(x = x_next, xend = x_next, y = y, yend = y_next), color = wu_orange, linewidth = 0.8) +
    geom_point(data = pts, aes(x, y), color = wu_dark, size = 2.5) +
    geom_text(data = steps, aes(x = x_next, y = (y + y_next) / 2, label = sprintf("+%.0f%%", 100 * (y_next / y - 1))), hjust = -0.15, size = base_size / 3.4, color = wu_orange) +
    scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale(), prefix = "€")) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    labs(title = "Original scale", subtitle = "Each doubling of spend (+100%) adds the same +32% to streams,\nbut a shrinking number of streams", x = "Promotion spend", y = "Streams") +
    theme_course(base_size)
  p2 <- ggplot(curve, aes(log(x), log(y))) + geom_line(color = wu_blue, linewidth = 1.1) +
    geom_segment(data = steps, aes(x = log(x), xend = log(x_next), y = log(y), yend = log(y)), color = wu_orange, linewidth = 0.8) +
    geom_segment(data = steps, aes(x = log(x_next), xend = log(x_next), y = log(y), yend = log(y_next)), color = wu_orange, linewidth = 0.8) +
    geom_point(data = pts, aes(log(x), log(y)), color = wu_dark, size = 2.5) +
    labs(title = "Both axes logged", subtitle = "Equal percentage steps become equal distances:\nthe curve is a straight line with slope 0.4", x = "log(promotion spend)", y = "log(streams)") +
    theme_course(base_size)
  p1 | p2
}

# --- 5.8: weak vs. strong effects in the logistic model ------------------------
fig_logistic_slopes <- function(base_size = 12) {
  x <- seq(0, 60, length.out = 300)
  d <- bind_rows(
    data.frame(x, p = plogis(0.02 * (x - 30)), b = "b1 = 0.02: weak effect, almost flat"),
    data.frame(x, p = plogis(0.16 * (x - 30)), b = "b1 = 0.16: moderate effect (our model)"),
    data.frame(x, p = plogis(0.60 * (x - 30)), b = "b1 = 0.60: strong effect, almost a step")
  )
  d$b <- factor(d$b, levels = unique(d$b))
  ggplot(d, aes(x, p, color = b)) +
    geom_line(linewidth = 1.2) +
    geom_vline(xintercept = 30, linetype = "dotted", color = "grey50") +
    scale_color_manual(values = c(wu_grey, wu_blue, wu_orange), name = NULL) +
    labs(x = "Listening hours", y = "P(upgrade)",
         subtitle = "Same midpoint (P = 0.5 at 30 hours); the slope sets how sharply the curve separates the outcomes") +
    theme_course(base_size) + theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 3))
}


# --- 5.9: one residual plot (the linear model) for the three-checks slide ----------
fig_residuals_linear <- function(c2 = load_campaigns(), base_size = 11) {
  lin <- lm(streams ~ spend + followers + placements, c2)
  d <- data.frame(fitted = fitted(lin), resid = resid(lin))
  ggplot(d, aes(fitted, resid)) +
    geom_hline(yintercept = 0, color = wu_red, linetype = "dashed") +
    geom_point(alpha = 0.3, color = wu_grey, size = 1.2) +
    geom_smooth(se = FALSE, color = wu_blue, method = "loess", formula = y ~ x, linewidth = 1.2) +
    scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    labs(x = "Fitted values (linear model)", y = "Residuals", subtitle = "A curve in the residuals: the line has the wrong shape") +
    theme_course(base_size)
}
