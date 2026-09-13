# ------------------------------------------------------------------
# ch01-figures.R
# Shared figure functions for Chapter 1 (book chapter AND slide deck)
# Every figure that appears in both documents is generated here, so
# the two can never drift apart. Edit here, re-render both.
# ------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

# --- course-wide look ---------------------------------------------------

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

# --- generic arrow-diagram helper --------------------------------------

draw_diagram <- function(nodes, edges, xlim = c(0, 10), ylim = c(0, 6),
                         label_size = 5.5) {
  ggplot() +
    geom_segment(
      data = edges,
      aes(x = x, y = y, xend = xend, yend = yend, linetype = style),
      arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
      linewidth = 0.8, color = wu_grey
    ) +
    geom_label(
      data = nodes,
      aes(x = x, y = y, label = label, fill = role),
      size = label_size, color = "white", fontface = "bold",
      label.padding = unit(0.45, "lines"), label.r = unit(0.25, "lines")
    ) +
    scale_fill_manual(values = c(
      main = wu_blue, confounder = wu_red,
      mediator = wu_green, moderator = wu_orange
    )) +
    scale_linetype_identity() +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_void() +
    theme(legend.position = "none")
}

# --- 1.2 The eBay case --------------------------------------------------

fig_ebay_naive <- function() {
  set.seed(11)
  n <- 52
  demand <- rnorm(n, 100, 20)                    # weekly shopping demand (hidden driver)
  spend  <- 30 + 0.6 * demand + rnorm(n, 0, 6)   # ad platform scales with demand
  sales  <- 200 + 3.0 * demand + rnorm(n, 0, 25) # sales driven by demand

  ggplot(data.frame(spend, sales), aes(spend, sales)) +
    geom_point(color = wu_blue, size = 2.6, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = wu_orange, linewidth = 1) +
    labs(
      title = "What the analysts saw",
      subtitle = "Weekly ad spend vs. weekly sales (illustrative, simulated)",
      x = "Search ad spend", y = "Sales"
    ) +
    theme_course()
}

# --- 1.4 Correlation, causation & what data can show --------------------

fig_two_pictures <- function() {
  set.seed(42)

  # (a) Experimental data: we set X, observe Y
  exp_df <- data.frame(
    group = rep(c("Control\n(no discount)", "Treatment\n(discount)"), each = 60)
  ) |>
    mutate(purchases = rnorm(n(), ifelse(group == "Control\n(no discount)", 20, 26), 6))

  p1 <- ggplot(exp_df, aes(group, purchases, color = group)) +
    geom_jitter(width = 0.12, alpha = 0.65, size = 2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.35, linewidth = 0.7) +
    scale_color_manual(values = c(wu_grey, wu_blue)) +
    labs(
      title = "(a) Experiment: we changed X, Y moved",
      subtitle = "Randomly assigned discount \u2192 purchases",
      x = NULL, y = "Purchases"
    ) +
    theme_course() + theme(legend.position = "none")

  # (b) Observational data: we merely observe X and Y
  obs_df <- local({
    set.seed(43)
    engage <- rnorm(120, 0, 1)                   # hidden: customer engagement
    data.frame(
      newsletters = pmax(0, 4 + 2.2 * engage + rnorm(120, 0, 1.4)),
      purchases   = 20 + 4.5 * engage + rnorm(120, 0, 4)
    )
  })

  p2 <- ggplot(obs_df, aes(newsletters, purchases)) +
    geom_point(color = wu_blue, alpha = 0.65, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = wu_orange, linewidth = 1) +
    labs(
      title = "(b) Observation: X and Y move together",
      subtitle = "Newsletters read vs. purchases (self-selected)",
      x = "Newsletters read", y = "Purchases"
    ) +
    theme_course()

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p1, p2, ncol = 2)
  } else {
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  }
}

fig_three_scatters <- function() {
  set.seed(7)
  n <- 110

  # 1: genuinely causal
  d1 <- data.frame(x = rnorm(n)) |>
    mutate(y = 0.7 * x + rnorm(n, 0, 0.7), panel = "1 \u00b7 Causal: X \u2192 Y")

  # 2: confounded (segment drives both; no effect of x within segment)
  seg <- rep(c(0, 1), each = n / 2)
  d2 <- data.frame(
    x = 1.4 * seg + rnorm(n, 0, 0.55),
    y = 1.4 * seg + rnorm(n, 0, 0.55),
    segment = factor(seg, labels = c("Segment A", "Segment B")),
    panel = "2 \u00b7 Confounded: Z \u2192 X, Z \u2192 Y"
  )

  # 3: spurious - two independent trending series
  t <- 1:n
  d3 <- data.frame(
    x = scale(cumsum(rnorm(n)) + 0.05 * t)[, 1],
    y = scale(cumsum(rnorm(n)) + 0.05 * t)[, 1],
    panel = "3 \u00b7 Spurious: shared trend / chance"
  )

  base <- function(d) {
    ggplot(d, aes(x, y)) +
      geom_smooth(method = "lm", se = FALSE, color = wu_orange, linewidth = 0.9) +
      labs(x = "X", y = "Y") + theme_course(base_size = 13) +
      facet_wrap(~panel)
  }

  p1 <- base(d1) + geom_point(color = wu_blue, alpha = 0.6, size = 1.8)
  p2 <- base(d2) + geom_point(aes(color = segment), alpha = 0.7, size = 1.8) +
    scale_color_manual(values = c(wu_grey, wu_red)) +
    theme(legend.position = "none")
  p3 <- base(d3) + geom_point(color = wu_blue, alpha = 0.6, size = 1.8)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p1, p2, p3, ncol = 3)
  } else {
    gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
  }
}

dgm_reverse <- function(direction = c("forward", "backward")) {
  direction <- match.arg(direction)
  nodes <- data.frame(x = c(2, 8), y = c(3, 3),
                      label = c("Ad spend", "Sales"), role = "main")
  edges <- if (direction == "forward") {
    data.frame(x = 3.4, xend = 6.6, y = 3, yend = 3, style = "solid")
  } else {
    data.frame(x = 6.6, xend = 3.4, y = 3, yend = 3, style = "solid")
  }
  draw_diagram(nodes, edges, ylim = c(1.5, 4.5))
}

# --- 1.5 The cast of variables ------------------------------------------

dgm_dv_iv <- function() {
  nodes <- data.frame(
    x = c(2.5, 7.5), y = c(3, 3),
    label = c("Treatment (IV)\ne.g., discount", "Outcome (DV)\ne.g., purchases"),
    role = "main"
  )
  edges <- data.frame(x = 4.1, xend = 5.9, y = 3, yend = 3, style = "solid")
  draw_diagram(nodes, edges, ylim = c(1.8, 4.2))
}

dgm_confounder <- function() {
  nodes <- data.frame(
    x = c(2.5, 7.5, 5), y = c(2, 2, 5),
    label = c("Ad exposure", "Purchase", "Purchase intent"),
    role = c("main", "main", "confounder")
  )
  edges <- data.frame(
    x    = c(3.9, 4.3, 5.7),
    xend = c(6.1, 2.9, 7.1),
    y    = c(2, 4.45, 4.45),
    yend = c(2, 2.55, 2.55),
    style = "solid"
  )
  draw_diagram(nodes, edges)
}

dgm_mediator <- function() {
  nodes <- data.frame(
    x = c(2, 8, 5), y = c(2, 2, 4.6),
    label = c("Ad campaign", "Sales", "Brand awareness"),
    role = c("main", "main", "mediator")
  )
  edges <- data.frame(
    x    = c(3.3, 2.7, 6.2),
    xend = c(6.7, 4.0, 7.6),
    y    = c(2, 2.5, 4.1),
    yend = c(2, 4.1, 2.5),
    style = "solid"
  )
  draw_diagram(nodes, edges, label_size = 5)
}

dgm_moderator <- function() {
  nodes <- data.frame(
    x = c(2, 8, 5), y = c(2, 2, 4.6),
    label = c("Discount", "Purchases", "Customer type\n(new vs. loyal)"),
    role = c("main", "main", "moderator")
  )
  edges <- data.frame(
    x    = c(3.3, 5),
    xend = c(6.7, 5),
    y    = c(2, 4),
    yend = c(2, 2.25),
    style = c("solid", "dashed")
  )
  draw_diagram(nodes, edges, label_size = 5)
}

dgm_full_cast <- function() {
  nodes <- data.frame(
    x = c(2, 8.5, 5.25, 5.25, 1.3),
    y = c(3.5, 3.5, 1.6, 5.9, 4.9),
    label = c("Treatment (IV)", "Outcome (DV)", "Mediator", "Confounder", "Moderator"),
    role = c("main", "main", "mediator", "confounder", "moderator")
  )
  edges <- data.frame(
    x    = c(3.25, 2.5, 6.1, 4.55, 5.95, 1.75),
    xend = c(7.25, 4.4, 8.0, 2.35, 8.15, 3.35),
    y    = c(3.5, 3.1, 1.95, 5.55, 5.55, 4.55),
    yend = c(3.5, 1.95, 3.1, 3.95, 3.95, 3.62),
    style = c("solid", "solid", "solid", "solid", "solid", "dashed")
  )
  draw_diagram(nodes, edges, ylim = c(0.9, 6.6))
}

# --- 1.6 Why randomization works ----------------------------------------

dgm_randomization <- function() {
  nodes <- data.frame(
    x = c(2.5, 7.5, 5, 1.5), y = c(2, 2, 5, 4.8),
    label = c("Exposure", "Purchase", "Intent", "Coin flip"),
    role = c("main", "main", "confounder", "mediator")
  )
  edges <- data.frame(
    x    = c(3.7, 5.7, 1.9),
    xend = c(6.3, 7.1, 2.3),
    y    = c(2, 4.4, 4.3),
    yend = c(2, 2.6, 2.6),
    style = "solid"
  )
  draw_diagram(nodes, edges) +
    annotate("segment", x = 4.3, xend = 2.9, y = 4.4, yend = 2.6,
             linetype = "dotted", color = wu_red, linewidth = 0.9) +
    annotate("segment", x = 3.32, xend = 3.88, y = 3.26, yend = 3.74,
             color = wu_red, linewidth = 1.3) +
    annotate("segment", x = 3.32, xend = 3.88, y = 3.74, yend = 3.26,
             color = wu_red, linewidth = 1.3)
}

fig_balance <- function() {
  set.seed(2026)
  n <- 2000
  pop <- data.frame(
    age        = rnorm(n, 40, 12),
    income     = rnorm(n, 3000, 800),
    engagement = rnorm(n, 0, 1),
    urban      = rbinom(n, 1, 0.55),
    app_user   = rbinom(n, 1, 0.4),
    intent     = rnorm(n, 0, 1)        # imagine this one is UNOBSERVED
  )

  # Self-selected exposure: driven by engagement & intent
  p_exp <- plogis(0.9 * pop$engagement + 0.9 * pop$intent - 0.2)
  pop$obs_group <- rbinom(n, 1, p_exp)
  # Randomized exposure
  pop$rct_group <- rbinom(n, 1, 0.5)

  smd <- function(x, g) {
    (mean(x[g == 1]) - mean(x[g == 0])) /
      sqrt((var(x[g == 1]) + var(x[g == 0])) / 2)
  }

  vars <- c("age", "income", "engagement", "urban", "app_user", "intent")
  bal <- bind_rows(
    data.frame(variable = vars, design = "Self-selected exposure",
               smd = sapply(vars, function(v) smd(pop[[v]], pop$obs_group))),
    data.frame(variable = vars, design = "Randomized assignment",
               smd = sapply(vars, function(v) smd(pop[[v]], pop$rct_group)))
  ) |>
    mutate(
      variable = ifelse(variable == "intent", "intent (unobserved!)", variable),
      design = factor(design, levels = c("Self-selected exposure",
                                         "Randomized assignment"))
    )

  ggplot(bal, aes(smd, variable)) +
    geom_vline(xintercept = 0, color = "grey60") +
    geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed", color = "grey75") +
    geom_point(size = 3.2, color = wu_blue) +
    facet_wrap(~design) +
    labs(
      title = "Covariate balance: exposed vs. unexposed group (simulated, n = 2,000)",
      subtitle = "Standardized mean difference per variable \u00b7 dashed lines: conventional \u00b10.1 band",
      x = "Standardized mean difference", y = NULL
    ) +
    theme_course()
}
