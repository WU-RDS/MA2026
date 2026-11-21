# ------------------------------------------------------------------
# ch08-figures.R
# Shared figure functions for the survey-design chapter (Module 8):
# book page survey-design.qmd AND slide deck slides/chapter08_slides.qmd.
# Data: data/coffee_seal_qualtrics.csv (synthetic; see R/ch08-make-data.R)
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

# --- the example study: import and cleaning (identical to code/08-survey-design.R) ---
load_coffee_raw <- function(file = file.path(data_dir, "coffee_seal_qualtrics.csv")) {
  col_names <- names(readr::read_csv(file, n_max = 0, show_col_types = FALSE))
  readr::read_csv(file, skip = 3, col_names = col_names, show_col_types = FALSE)
}

clean_coffee <- function(raw = load_coffee_raw()) {
  raw |>
    rename(duration = `Duration (in seconds)`) |>
    filter(Status == 0, Finished == 1) |>
    filter(attn == 2, duration >= median(duration) / 3) |>
    mutate(wtp_text = wtp,
           wtp = as.numeric(stringr::str_replace(stringr::str_extract(wtp_text, "[0-9]+([.,][0-9]+)?"), ",", ".")),
           wtp = if_else(wtp > 30, NA_real_, wtp),
           env_2r = 8 - env_2,
           concern   = (env_1 + env_2r + env_3) / 3,
           warm_glow = (glow_1 + glow_2 + glow_3) / 3,
           condition = factor(condition, levels = c("Control", "Seal")),
           seal = as.numeric(condition == "Seal"),
           gender = factor(gender, levels = 1:4, labels = c("female", "male", "non-binary / other", "prefer not to say"))) |>
    filter(!is.na(wtp))
}

# ------------------------------------------------------------------
# 1. The causal diagram of a group-project study
# ------------------------------------------------------------------
fig_study_diagram <- function(labels = c(X = "Sustainability seal:\nwithout vs. with", M = "Anticipated\nwarm glow",
                                         Y = "Willingness\nto pay", W = "Environmental\nconcern"),
                              base_size = 13) {
  s <- base_size / 3
  nodes <- data.frame(x = c(1.4, 5, 8.6, 5), y = c(2.2, 3.75, 2.2, 0.6),
                      label = c(paste0(labels["X"], "\n(treatment X)"), paste0(labels["M"], "\n(mediator M)"),
                                paste0(labels["Y"], "\n(outcome Y)"), paste0(labels["W"], "\n(moderator W)")),
                      fill = c(wu_blue, wu_green, wu_blue, wu_orange))
  ar <- arrow(length = unit(0.25, "cm"), type = "closed")
  ggplot() +
    annotate("segment", x = 2.85, xend = 7.4, y = 2.2, yend = 2.2, arrow = ar, color = wu_grey, linewidth = 0.9) +
    annotate("segment", x = 2.2, xend = 3.95, y = 2.8, yend = 3.5, arrow = ar, color = wu_grey, linewidth = 0.9) +
    annotate("segment", x = 6.05, xend = 7.8, y = 3.5, yend = 2.8, arrow = ar, color = wu_grey, linewidth = 0.9) +
    annotate("segment", x = 5, xend = 5, y = 1.2, yend = 2.1, arrow = ar, color = wu_orange, linewidth = 0.9, linetype = "dashed") +
    annotate("text", x = 5.15, y = 1.62, label = "for whom?", hjust = 0, color = wu_orange, fontface = "italic", size = s * 0.9) +
    annotate("text", x = 2.75, y = 3.35, label = "why?", hjust = 1, color = wu_green, fontface = "italic", size = s * 0.9) +
    annotate("text", x = 5, y = 2.42, label = "does it work?", color = wu_grey, fontface = "italic", size = s * 0.9) +
    geom_label(data = nodes, aes(x, y, label = label, fill = fill), color = "white", fontface = "bold", size = s,
               lineheight = 0.9, label.padding = unit(0.5, "lines"), label.r = unit(0.25, "lines")) +
    scale_fill_identity() +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 4.35)) +
    theme_void()
}

# ------------------------------------------------------------------
# 2. The questionnaire follows the causal diagram
# ------------------------------------------------------------------
fig_questionnaire_order <- function(base_size = 12, example = TRUE) {
  s <- base_size / 3.1
  blocks <- data.frame(
    pos   = 1:9,
    label = c("Welcome", "Warm-up", "Moderator\n+ controls", "Stimulus", "Mediator", "Outcome\n(DV)",
              "Manipulation\ncheck", "Demo-\ngraphics", "Thank you\n& debrief"),
    ex    = c("topic, duration,\nanonymity", "cups of coffee\nper day", "environmental\nconcern (3 items),\nattention check",
              "page without OR\nwith the seal", "warm glow\n(3 items)", "willingness to pay\n(euros)",
              "\"carried a seal\"\n(1-7)", "age, gender", "brand and seal\nare fictitious"),
    fill  = c("#e9e9e9", "#e9e9e9", wu_orange, wu_blue, wu_green, wu_blue, "#c9d6e6", "#e9e9e9", "#e9e9e9"),
    col   = c("grey15", "grey15", "white", "white", "white", "white", "grey15", "grey15", "grey15")
  )
  ar <- arrow(length = unit(0.22, "cm"), type = "closed")
  p <- ggplot(blocks) +
    # before / after shading
    annotate("rect", xmin = 0.45, xmax = 3.5, ymin = -0.72, ymax = 2.42, fill = "#fdf3e1", color = NA) +
    annotate("rect", xmin = 3.5, xmax = 9.55, ymin = -0.72, ymax = 2.42, fill = "#eef3f8", color = NA) +
    annotate("segment", x = 3.5, xend = 3.5, y = -0.72, yend = 2.42, color = wu_red, linewidth = 0.9, linetype = "dashed") +
    annotate("label", x = 3.5, y = 2.42, label = "RANDOMIZER", color = "white", fill = wu_red, fontface = "bold",
             size = s * 0.9, label.r = unit(0.15, "lines")) +
    annotate("text", x = 1.95, y = 2.2, label = "BEFORE the manipulation:\ncannot be affected by it", color = "#8a5a00",
             fontface = "bold", size = s * 0.92, lineheight = 0.95) +
    annotate("text", x = 7.9, y = 2.2, label = "AFTER the manipulation:\ncan be affected by it", color = wu_dark,
             fontface = "bold", size = s * 0.92, lineheight = 0.95, hjust = 0.5) +
    # the causal diagram, unfolded along the timeline
    annotate("curve", x = 4.15, xend = 5.85, y = 1.45, yend = 1.45, curvature = -0.55, arrow = ar, color = wu_grey, linewidth = 0.8) +
    annotate("segment", x = 4.3, xend = 4.7, y = 1.25, yend = 1.25, arrow = ar, color = wu_grey, linewidth = 0.8) +
    annotate("segment", x = 5.3, xend = 5.7, y = 1.25, yend = 1.25, arrow = ar, color = wu_grey, linewidth = 0.8) +
    annotate("curve", x = 3.25, xend = 4.95, y = 1.4, yend = 1.86, curvature = -0.3, arrow = ar, color = wu_orange,
             linewidth = 0.8, linetype = "dashed") +
    geom_point(data = data.frame(x = c(3, 4, 5, 6), y = 1.25, fill = c(wu_orange, wu_blue, wu_green, wu_blue)),
               aes(x, y, fill = fill), shape = 21, size = base_size * 1.05, color = "white", stroke = 1) +
    annotate("text", x = c(3, 4, 5, 6), y = 1.25, label = c("W", "X", "M", "Y"), color = "white", fontface = "bold", size = s * 1.25) +
    # the blocks of the questionnaire
    geom_tile(aes(x = pos, y = 0.35, fill = fill), width = 0.94, height = 0.62, color = "white") +
    geom_text(aes(x = pos, y = 0.35, label = label, color = col), fontface = "bold", size = s * 0.86, lineheight = 0.9) +
    annotate("segment", x = 0.55, xend = 9.5, y = -0.05, yend = -0.05, arrow = ar, color = "grey45", linewidth = 0.6) +
    scale_fill_identity() + scale_color_identity() +
    coord_cartesian(xlim = c(0.5, 9.5), ylim = c(-0.7, 2.5), clip = "off") +
    theme_void()
  if (example) p <- p + geom_text(aes(x = pos, y = -0.14, label = ex), color = "grey25", size = s * 0.8, lineheight = 0.9, vjust = 1)
  p
}

# ------------------------------------------------------------------
# 3. The Qualtrics survey flow, drawn schematically
# ------------------------------------------------------------------
flow_colors <- list(
  block = c(fill = "#ececec", line = "#b5b5b5"), randomizer = c(fill = "#f0dcec", line = "#b784a7"),
  group = c(fill = "#dde6f0", line = "#8fa8c4"), embedded = c(fill = "#e3ecd3", line = "#9db57a"),
  branch = c(fill = "#dde6f0", line = "#8fa8c4"), end = c(fill = "#f6dede", line = "#c58f8f"))
flow_names <- c(block = "Block", randomizer = "Randomizer", group = "Group", embedded = "Embedded data",
                branch = "Branch", end = "End of survey")

draw_flow <- function(el, base_size = 12, title = NULL, title_color = wu_dark, caption = NULL, width = 10, indent = 0.55,
                      label_x = 2.05) {
  s <- base_size / 3.1
  # rows with a two-line label are taller
  n_lines <- lengths(strsplit(el$label, "\n", fixed = TRUE))
  h   <- 0.8 + 0.55 * (n_lines - 1)
  top <- -(cumsum(h + 0.2) - h - 0.2)
  el <- el |> mutate(ymax = top, ymin = top - h, y = top - h / 2, xmin = level * indent, xmax = width,
                     fill = vapply(type, function(t) flow_colors[[t]]["fill"], ""),
                     line = vapply(type, function(t) flow_colors[[t]]["line"], ""),
                     tag = flow_names[type])
  for (v in c("step", "step_right", "mark")) if (!v %in% names(el)) el[[v]] <- NA
  p <- ggplot(el) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, color = line), linewidth = 0.5) +
    geom_text(aes(x = xmin + 0.15, y = y, label = tag), hjust = 0, fontface = "bold", size = s * 0.85, color = "grey25") +
    geom_text(aes(x = xmin + label_x, y = y, label = label), hjust = 0, size = s * 0.9, color = "grey10", lineheight = 0.95) +
    scale_fill_identity() + scale_color_identity() +
    coord_cartesian(xlim = c(-0.6, width + 0.1), ylim = c(min(el$ymin) - 0.15, 0.1), clip = "off") +
    theme_void(base_size = base_size) +
    theme(plot.title = element_text(face = "bold", color = title_color, size = rel(1.0), margin = margin(b = 4)),
          plot.caption = element_text(hjust = 0, color = "grey20", size = rel(0.85), lineheight = 1.05, margin = margin(t = 4)),
          plot.caption.position = "plot", plot.title.position = "plot", plot.margin = margin(4, 10, 12, 6))
  st <- el |> filter(!is.na(step))
  if (nrow(st) > 0) p <- p +
    geom_point(data = st, aes(x = -0.32, y = y), size = base_size * 0.62, color = wu_blue) +
    geom_text(data = st, aes(x = -0.32, y = y, label = step), color = "white", fontface = "bold", size = s * 0.9)
  sr <- el |> filter(!is.na(step_right))
  if (nrow(sr) > 0) p <- p +
    geom_point(data = sr, aes(x = xmax - 0.35, y = y), size = base_size * 0.62, color = wu_blue) +
    geom_text(data = sr, aes(x = xmax - 0.35, y = y, label = step_right), color = "white", fontface = "bold", size = s * 0.9)
  mk <- el |> filter(!is.na(mark))
  if (nrow(mk) > 0) p <- p +
    geom_rect(data = mk, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA, color = wu_red, linewidth = 1.1)
  p + labs(title = title, caption = caption)
}

flow_correct <- function(steps = TRUE) {
  el <- data.frame(
    type  = c("block", "block", "randomizer", "group", "embedded", "block", "group", "embedded", "block",
              "block", "block", "block", "block", "block"),
    label = c("Welcome", "Warm-up, moderator, attention check",
              "Randomly present  1  of the following elements\n[x]  Evenly Present Elements",
              "Control", "condition = Control", "Stimulus - Control",
              "Seal", "condition = Seal", "Stimulus - Seal",
              "Mediator (warm glow)", "Dependent variable (willingness to pay)", "Manipulation check",
              "Demographics", "Thank you and debriefing"),
    level = c(0, 0, 0, 1, 2, 2, 1, 2, 2, 0, 0, 0, 0, 0))
  if (steps) {
    el$step <- c(NA, NA, "1", "2", "3", "4", "2", "3", "4", NA, NA, NA, NA, NA)
    el$step_right <- c(NA, NA, "5", rep(NA, 11))
  }
  el
}

# label_x = distance between the left edge of a row and its label (increase it for larger fonts)
fig_survey_flow <- function(base_size = 12, steps = TRUE, label_x = 2.05) {
  draw_flow(flow_correct(steps), base_size = base_size, label_x = label_x)
}

# ncol = 2: panels in a 2 x 2 grid (slides); ncol = 1: panels stacked (website)
fig_flow_mistakes <- function(base_size = 11, label_x = 2.5, ncol = 2) {
  a <- data.frame(type = c("block", "randomizer", "group", "group", "block"),
                  label = c("Moderator", "Randomly present  2  of the following elements", "Control: label + stimulus",
                            "Seal: label + stimulus", "Mediator, DV, ..."),
                  level = c(0, 0, 1, 1, 0), mark = c(NA, 1, NA, NA, NA))
  b <- data.frame(type = c("block", "randomizer", "embedded", "block", "embedded", "block", "block"),
                  label = c("Moderator", "Randomly present  1  of the following elements", "condition = Control", "Stimulus - Control",
                            "condition = Seal", "Stimulus - Seal", "Mediator, DV, ..."),
                  level = c(0, 0, 1, 1, 1, 1, 0), mark = c(NA, NA, 1, 1, 1, 1, NA))
  c_ <- data.frame(type = c("block", "randomizer", "embedded", "embedded", "block", "block", "block"),
                   label = c("Moderator", "Randomly present  1  of the following elements", "condition = Control", "condition = Seal",
                             "Stimulus - Control", "Stimulus - Seal", "Mediator, DV, ..."),
                   level = c(0, 0, 1, 1, 0, 0, 0), mark = c(NA, NA, NA, NA, 1, 1, NA))
  d <- data.frame(type = c("block", "randomizer", "group", "block", "group", "block", "block"),
                  label = c("Moderator", "Randomly present  1  of the following elements", "Control  (+ label)",
                            "Stimulus - Control  +  WTP question", "Seal  (+ label)", "Stimulus - Seal  +  WTP question",
                            "Mediator, ..."),
                  level = c(0, 0, 1, 2, 1, 2, 0), mark = c(NA, NA, NA, 1, NA, 1, NA))
  f <- function(el, title, caption) draw_flow(el, base_size = base_size, title = title, title_color = wu_red, caption = caption,
                                              width = 9.6, label_x = label_x)
  pa <- f(a, "A  The number is not 1", "Everyone sees both versions, in random order:\nno comparison between groups is left.")
  pb <- f(b, "B  Label and stimulus are separate elements", "One of FOUR elements is drawn: half of the respondents\nsee no stimulus, the others have no label.")
  pc <- f(c_, "C  The stimulus blocks stayed in the main flow", "The label is random, but everyone sees both versions.")
  pd <- f(d, "D  Questions inside the stimulus blocks", "Two columns for one variable (wtp_control, wtp_seal);\nevery later change must be made twice.")
  if (ncol == 1) {
    # stacked: panel heights proportional to the number of rows, so that all rows have the same height
    pa / pb / pc / pd + patchwork::plot_layout(heights = c(nrow(a), nrow(b), nrow(c_), nrow(d)))
  } else {
    (pa + pb) / (pc + pd)
  }
}

# ------------------------------------------------------------------
# 4. Why the measure matters: the same effect, the same 120 respondents
# ------------------------------------------------------------------
measure_power <- function(n = 60, d = 0.5, item_sd = 0.8, reps = 4000, seed = 8) {
  set.seed(seed)
  rate7 <- function(x) { x[] <- pmin(7, pmax(1, round(4 + 1.3 * x))); x }
  one <- function() {
    th0 <- rnorm(n); th1 <- rnorm(n, d)
    i0 <- matrix(th0 + rnorm(n * 3, 0, item_sd), n); i1 <- matrix(th1 + rnorm(n * 3, 0, item_sd), n)
    r0 <- rate7(i0); r1 <- rate7(i1)
    b0 <- i0[, 1] > d / 2; b1 <- i1[, 1] > d / 2
    c(choice = suppressWarnings(prop.test(c(sum(b1), sum(b0)), c(n, n), correct = FALSE)$p.value),
      single = t.test(r1[, 1], r0[, 1])$p.value,
      scale3 = t.test(rowMeans(r1), rowMeans(r0))$p.value,
      ideal  = t.test(th1, th0)$p.value)
  }
  p <- replicate(reps, one())
  data.frame(measure = c("choice", "single", "scale3", "ideal"), power = as.numeric(rowMeans(p < 0.05)))
}

fig_measure_power <- function(base_size = 13, res = measure_power()) {
  lab <- c(choice = "Yes/no choice\n(\"Would you buy it?\")", single = "One rating question\n(7 points)",
           scale3 = "Scale: mean of three\nrating questions", ideal = "Measured without\nany noise (benchmark)")
  res$label <- factor(lab[res$measure], levels = lab)
  ggplot(res, aes(label, power)) +
    geom_col(aes(fill = measure == "ideal"), width = 0.62) +
    geom_text(aes(label = scales::percent(power, accuracy = 1)), vjust = 1.6, color = "white", fontface = "bold", size = base_size / 2.6) +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = wu_red) +
    annotate("text", x = 0.45, y = 0.84, label = "conventional target: 80%", color = wu_red, hjust = 0, size = base_size / 3.6) +
    scale_fill_manual(values = c(`FALSE` = wu_blue, `TRUE` = "grey70"), guide = "none") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
    labs(x = NULL, y = "Probability of a significant result (power)") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
}

# ------------------------------------------------------------------
# 5. The results of the example study
# ------------------------------------------------------------------
fig_exclusions <- function(raw = load_coffee_raw(), base_size = 12) {
  d0 <- raw |> rename(duration = `Duration (in seconds)`)
  d1 <- d0 |> filter(Status == 0); d2 <- d1 |> filter(Finished == 1)
  d3 <- d2 |> filter(attn == 2);   d4 <- d3 |> filter(duration >= median(d2$duration) / 3)
  d5 <- clean_coffee(raw)
  steps <- data.frame(
    label = c("Recorded responses", "Real responses\n(no previews)", "Finished", "Passed the\nattention check",
              "Not a speeder", "Plausible WTP\n= analysis sample"),
    n = c(nrow(d0), nrow(d1), nrow(d2), nrow(d3), nrow(d4), nrow(d5)))
  steps$label <- factor(steps$label, levels = steps$label)
  steps$drop <- c(NA, -diff(steps$n))
  ggplot(steps, aes(label, n)) +
    geom_col(fill = c(rep("grey70", 5), wu_blue), width = 0.65) +
    geom_text(aes(label = n), vjust = -0.5, fontface = "bold", size = base_size / 3) +
    geom_text(aes(y = n / 2, label = ifelse(is.na(drop), "", paste0("-", drop))), color = "white", fontface = "bold", size = base_size / 3.2) +
    scale_y_continuous(limits = c(0, max(steps$n) * 1.12), expand = expansion(mult = c(0, 0))) +
    labs(x = NULL, y = "Number of responses") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
}

fig_wtp_by_condition <- function(d = clean_coffee(), base_size = 13) {
  tt <- t.test(wtp ~ condition, data = d)
  m <- d |> group_by(condition) |>
    summarise(n = n(), mean = mean(wtp), se = sd(wtp) / sqrt(n()), .groups = "drop") |>
    mutate(lo = mean - qt(0.975, n - 1) * se, hi = mean + qt(0.975, n - 1) * se,
           lab = paste0(condition, "\n(n = ", n, ")"))
  d <- d |> left_join(m |> select(condition, lab), by = "condition")
  top <- max(d$wtp) + 0.9
  ggplot(d, aes(lab, wtp)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, color = "grey45", fill = "grey96") +
    geom_jitter(width = 0.13, height = 0, alpha = 0.35, color = wu_grey, size = 1.6) +
    geom_errorbar(data = m, aes(x = lab, y = mean, ymin = lo, ymax = hi), width = 0.12, color = wu_blue, linewidth = 1.1,
                  position = position_nudge(x = 0.36)) +
    geom_point(data = m, aes(x = lab, y = mean), color = wu_dark, size = 3.5, position = position_nudge(x = 0.36)) +
    geom_text(data = m, aes(x = lab, y = mean, label = sprintf("%.2f", mean)), color = wu_dark, fontface = "bold",
              size = base_size / 3.2, position = position_nudge(x = 0.53)) +
    annotate("segment", x = 1, xend = 2, y = top, yend = top, color = "grey30") +
    annotate("text", x = 1.5, y = top + 0.5, size = base_size / 3.3,
             label = sprintf("difference = %.2f euros, 95%% CI [%.2f; %.2f], p %s", -diff(rev(tt$estimate)),
                             -tt$conf.int[2], -tt$conf.int[1],
                             ifelse(tt$p.value < 0.0005, "< .001", paste("=", sub("^0", "", sprintf("%.3f", tt$p.value)))))) +
    labs(x = NULL, y = "Willingness to pay for 250 g (euros)") +
    theme_course(base_size) + theme(panel.grid.major.x = element_blank())
}

fig_seal_by_concern <- function(d = clean_coffee(), base_size = 13) {
  m <- lm(wtp ~ seal * concern, data = d)
  b <- coef(m); V <- vcov(m)
  g <- data.frame(concern = seq(1.5, 7, by = 0.1)) |>
    mutate(est = b["seal"] + b["seal:concern"] * concern,
           se = sqrt(V["seal", "seal"] + concern^2 * V["seal:concern", "seal:concern"] +
                       2 * concern * V["seal", "seal:concern"]),
           lo = est - qt(0.975, df.residual(m)) * se, hi = est + qt(0.975, df.residual(m)) * se)
  pts <- g |> filter(round(concern, 1) %in% c(3, 4.5, 6))
  ggplot(g, aes(concern, est)) +
    geom_hline(yintercept = 0, color = wu_red, linetype = "dashed") +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = wu_light, alpha = 0.8) +
    geom_line(color = wu_blue, linewidth = 1.2) +
    geom_point(data = pts, color = wu_dark, size = 3) +
    geom_text(data = pts, aes(label = sprintf("%+.2f", est)), vjust = -1.1, hjust = 1.1, fontface = "bold", color = wu_dark, size = base_size / 3.3) +
    geom_rug(data = d, aes(x = concern), inherit.aes = FALSE, alpha = 0.3, sides = "b") +
    scale_x_continuous(breaks = 1:7) +
    labs(x = "Environmental concern (1-7, measured before the stimulus)", y = "Effect of the seal on WTP (euros, 95% CI)") +
    theme_course(base_size)
}

fig_coffee_mediation <- function(d = clean_coffee(), base_size = 13) {
  ma <- lm(warm_glow ~ condition + concern, data = d)
  my <- lm(wtp ~ condition + warm_glow + concern, data = d)
  a <- coef(ma)["conditionSeal"]; b <- coef(my)["warm_glow"]; cdir <- coef(my)["conditionSeal"]
  s <- base_size / 3
  nodes <- data.frame(name = c("Seal vs. control\n(X)", "Anticipated\nwarm glow (M)", "Willingness\nto pay (Y)"),
                      x = c(0, 1.5, 3), y = c(0, 1, 0))
  ar <- arrow(length = unit(0.3, "cm"), type = "closed")
  ggplot() +
    annotate("segment", x = 0.45, xend = 1.05, y = 0.25, yend = 0.8, arrow = ar, color = wu_green, linewidth = 1) +
    annotate("segment", x = 1.95, xend = 2.55, y = 0.8, yend = 0.25, arrow = ar, color = wu_green, linewidth = 1) +
    annotate("segment", x = 0.55, xend = 2.45, y = 0, yend = 0, arrow = ar, color = wu_grey, linewidth = 1) +
    annotate("text", x = 0.5, y = 0.66, label = sprintf("a = %.2f", a), size = s, color = wu_green, fontface = "bold") +
    annotate("text", x = 2.5, y = 0.66, label = sprintf("b = %.2f", b), size = s, color = wu_green, fontface = "bold") +
    annotate("text", x = 1.5, y = -0.17, label = sprintf("direct effect c' = %.2f", cdir), size = s, color = wu_grey, fontface = "bold") +
    annotate("text", x = 1.5, y = 0.42, label = sprintf("indirect effect a x b = %.2f", a * b), size = s, color = wu_green, fontface = "bold") +
    geom_label(data = nodes, aes(x, y, label = name), size = s, fill = "white", label.padding = unit(0.5, "lines"), lineheight = 0.9) +
    coord_cartesian(xlim = c(-0.6, 3.6), ylim = c(-0.4, 1.35)) +
    theme_void()
}

# ------------------------------------------------------------------
# 6. Measurement: reliability and validity, one item or several,
#    reflective or formative, Cronbach's alpha
# ------------------------------------------------------------------

# Cronbach's alpha from the covariances of the items (the raw_alpha of psych::alpha)
cronbach_alpha <- function(items) {
  items <- as.data.frame(items)
  k <- ncol(items)
  v <- stats::var(items, use = "complete.obs")
  k / (k - 1) * (1 - sum(diag(v)) / sum(v))
}

# Alpha for k items with a mean inter-item correlation of r (standardized alpha)
alpha_from_r <- function(k, r) k * r / (1 + (k - 1) * r)

# Three targets: reliable and valid / reliable, but not valid / not reliable
fig_reliability_validity <- function(base_size = 12, seed = 11) {
  set.seed(seed)
  panels <- c("Reliable and valid", "Reliable, but not valid", "Not reliable")
  notes  <- c("consistent and on target", "consistent, but off target:\nsystematic error",
              "scattered: random error -\nand therefore not valid either")
  ring <- function(r, n = 180) { t <- seq(0, 2 * pi, length.out = n); data.frame(x = r * cos(t), y = r * sin(t)) }
  radii <- c(1, 0.8, 0.6, 0.4, 0.2)
  fills <- c(wu_blue, "white", wu_blue, "white", wu_blue)
  shots <- bind_rows(
    data.frame(panel = 1, x = rnorm(9, 0, 0.075), y = rnorm(9, 0, 0.075)),
    data.frame(panel = 2, x = rnorm(9, 0.50, 0.075), y = rnorm(9, 0.42, 0.075)),
    data.frame(panel = 3, x = c(-0.62, 0.55, 0.12, -0.30, 0.70, -0.75, 0.30, -0.10, 0.48),
               y = c(0.50, 0.62, -0.72, -0.35, -0.28, -0.20, 0.22, 0.80, -0.66)))
  shots$panel <- factor(panels[shots$panel], levels = panels)
  lab <- data.frame(panel = factor(panels, levels = panels), note = notes)
  p <- ggplot()
  for (i in seq_along(radii)) {
    rr <- bind_rows(lapply(panels, function(pn) cbind(ring(radii[i]), panel = pn)))
    rr$panel <- factor(rr$panel, levels = panels)
    p <- p + geom_polygon(data = rr, aes(x, y), fill = fills[i], color = wu_blue, linewidth = 0.3)
  }
  p +
    geom_point(data = shots, aes(x, y), shape = 21, fill = wu_orange, color = "grey15", size = base_size / 4.6, stroke = 0.5) +
    geom_text(data = lab, aes(x = 0, y = -1.16, label = note), vjust = 1, lineheight = 0.95, size = base_size / 3.7, color = "grey20") +
    facet_wrap(~ panel, nrow = 1) +
    coord_fixed(xlim = c(-1.25, 1.25), ylim = c(-1.75, 1.05), clip = "off") +
    theme_void(base_size = base_size) +
    theme(strip.text = element_text(face = "bold", color = wu_dark, size = rel(1.0), margin = margin(b = 5)),
          panel.spacing = unit(0.6, "lines"), plot.margin = margin(2, 4, 2, 4))
}

# Formative and reflective measurement models
#   example = "drunkenness": one construct, causes on the left, effects on the right (the classic illustration)
#   example = "coffee":      a formative index (coffee consumption) next to a reflective scale (warm glow)
fig_measurement_models <- function(example = c("drunkenness", "coffee"), base_size = 12) {
  example <- match.arg(example)
  s  <- base_size / 3
  ar <- arrow(length = unit(0.2, "cm"), type = "closed")
  ellipse <- function(cx, cy, a, b, n = 200) { t <- seq(0, 2 * pi, length.out = n); data.frame(x = cx + a * cos(t), y = cy + b * sin(t)) }
  edge <- function(cx, cy, a, b, px, py) { th <- atan2((py - cy) / b, (px - cx) / a); c(cx + a * cos(th), cy + b * sin(th)) }
  col_f <- wu_orange; fill_f <- "#fbf1dc"; col_r <- wu_blue; fill_r <- wu_light
  if (example == "drunkenness") {
    items_f <- c("Number of beers", "Number of spirits", "Time since the\nlast drink", "Fat content of\nthe last meal")
    items_r <- c("Clear\nenunciation", "Ability to walk\nin a straight line", "Reaction time", "Ability to\ninteract")
    y <- c(4.05, 3.02, 1.98, 0.95); hw <- 1.35; hh <- 0.40
    xf <- 1.55; xr <- 9.45; cons <- data.frame(cx = 5.5, cy = 2.5, a = 1.35, b = 0.95, label = "Degree of\ndrunkenness")
    cons_f <- cons; cons_r <- cons; xlim <- c(0, 11); ylim <- c(-0.35, 5.3)
    head_f <- "Formative measurement"; head_r <- "Reflective measurement"
    foot_f <- "The items CAUSE the construct."; foot_r <- "The items are EFFECTS of the construct."
  } else {
    items_f <- c("Cups at home", "Cups at work\nor university", "Cups in caf\u00e9s")
    items_r <- c("\"... feel good:\nI help to protect ...\"", "\"... feel better:\nI do not harm ...\"",
                 "\"... the feeling\nof contributing ...\"")
    y <- c(3.75, 2.5, 1.25); hw <- 1.4; hh <- 0.45
    xf <- 1.5; xr <- 10.5
    cons_f <- data.frame(cx = 4.55, cy = 2.5, a = 1.05, b = 0.85, label = "Coffee\nconsumption")
    cons_r <- data.frame(cx = 7.45, cy = 2.5, a = 1.05, b = 0.85, label = "Warm\nglow")
    cons <- bind_rows(cons_f, cons_r); xlim <- c(0, 12); ylim <- c(-0.2, 5.1)
    head_f <- "Formative: an index"; head_r <- "Reflective: a scale"
    foot_f <- "The items MAKE UP the construct."; foot_r <- "The items EXPRESS the construct."
  }
  bf <- data.frame(x = xf, y = y, label = items_f); br <- data.frame(x = xr, y = y, label = items_r)
  seg_f <- bind_rows(lapply(seq_along(y), function(i) { e <- edge(cons_f$cx, cons_f$cy, cons_f$a, cons_f$b, xf + hw, y[i])
    data.frame(x = xf + hw + 0.05, y = y[i], xend = e[1] - 0.04, yend = e[2]) }))
  seg_r <- bind_rows(lapply(seq_along(y), function(i) { e <- edge(cons_r$cx, cons_r$cy, cons_r$a, cons_r$b, xr - hw, y[i])
    data.frame(x = e[1] + 0.04, y = e[2], xend = xr - hw - 0.05, yend = y[i]) }))
  ell <- bind_rows(lapply(seq_len(nrow(cons)), function(i) cbind(ellipse(cons$cx[i], cons$cy[i], cons$a[i], cons$b[i]), id = i)))
  ggplot() +
    geom_segment(data = seg_f, aes(x = x, y = y, xend = xend, yend = yend), arrow = ar, color = col_f, linewidth = 0.8) +
    geom_segment(data = seg_r, aes(x = x, y = y, xend = xend, yend = yend), arrow = ar, color = col_r, linewidth = 0.8) +
    geom_rect(data = bf, aes(xmin = x - hw, xmax = x + hw, ymin = y - hh, ymax = y + hh), fill = fill_f, color = col_f, linewidth = 0.6) +
    geom_rect(data = br, aes(xmin = x - hw, xmax = x + hw, ymin = y - hh, ymax = y + hh), fill = fill_r, color = col_r, linewidth = 0.6) +
    geom_text(data = bf, aes(x, y, label = label), size = s * 0.86, lineheight = 0.9, color = "grey10") +
    geom_text(data = br, aes(x, y, label = label), size = s * 0.86, lineheight = 0.9, color = "grey10") +
    geom_polygon(data = ell, aes(x, y, group = id), fill = wu_dark, color = wu_dark) +
    geom_text(data = cons, aes(cx, cy, label = label), color = "white", fontface = "bold", size = s * 0.95, lineheight = 0.9) +
    annotate("text", x = xf, y = max(y) + hh + 0.5, label = head_f, fontface = "bold", color = "#9a6a12", size = s) +
    annotate("text", x = xr, y = max(y) + hh + 0.5, label = head_r, fontface = "bold", color = col_r, size = s) +
    annotate("text", x = xf - hw, y = min(y) - hh - 0.42, label = foot_f, hjust = 0, color = "grey20", size = s * 0.85) +
    annotate("text", x = xr + hw, y = min(y) - hh - 0.42, label = foot_r, hjust = 1, color = "grey20", size = s * 0.85) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_void(base_size = base_size) + theme(plot.margin = margin(2, 2, 2, 2))
}

# One statement against the mean of the other two statements of the same scale
fig_item_vs_rest <- function(d, base_size = 12, items = c("glow_1", "glow_2", "glow_3"), which = 1,
                             xlab = "Statement 1: \"... I can feel good because\nI help to protect the environment.\"",
                             ylab = "Mean of the other two statements") {
  dd <- data.frame(x = d[[items[which]]], y = rowMeans(d[items[-which]]))
  r  <- cor(dd$x, dd$y)
  ggplot(dd, aes(x, y)) +
    geom_count(color = wu_blue, alpha = 0.6) +
    scale_size_area(max_size = base_size * 0.5, guide = "none") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = wu_dark, linewidth = 0.8) +
    annotate("label", x = 0.85, y = 7.15, hjust = 0, vjust = 1, size = base_size / 3.2, fill = "white", color = wu_dark,
             label.size = 0.3, lineheight = 1.0,
             label = sprintf("N = %d\nr = %s", nrow(dd), sub("^0", "", sprintf("%.2f", r)))) +
    scale_x_continuous(breaks = 1:7, limits = c(0.7, 7.3)) +
    scale_y_continuous(breaks = 1:7, limits = c(0.7, 7.3)) +
    labs(x = xlab, y = ylab) +
    theme_course(base_size)
}

# What changes if a construct is measured with one item instead of the scale?
# Rows: each single item, then the mean of all items. Columns: the standardized effect of the
# condition on the measure, its correlation with willingness to pay, and the mediation results
# (environmental concern as a control variable, as in H3).
single_vs_scale <- function(d, items = c("glow_1", "glow_2", "glow_3")) {
  cohens_d <- function(x, g) {
    m <- tapply(x, g, mean); n <- table(g); v <- tapply(x, g, var)
    unname(diff(m) / sqrt(sum((n - 1) * v) / (sum(n) - 2)))
  }
  one <- function(m) {
    model_m <- lm(m ~ condition + concern, data = d)
    model_y <- lm(wtp ~ condition + m + concern, data = d)
    a <- unname(coef(model_m)["conditionSeal"]); b <- unname(coef(model_y)["m"])
    direct <- unname(coef(model_y)["conditionSeal"])
    data.frame(d = cohens_d(m, d$condition), r_wtp = cor(m, d$wtp), a = a, b = b,
               indirect = a * b, direct = direct, share = a * b / (a * b + direct))
  }
  res <- bind_rows(lapply(items, function(i) one(d[[i]])), one(rowMeans(d[items])))
  cbind(measure = c(paste("item", seq_along(items)), "scale"), res)
}
