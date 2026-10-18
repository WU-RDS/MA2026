# ------------------------------------------------------------------
# ch01-figures.R
# Shared figure functions for Chapter 1 (book chapter AND slide deck)
# Every figure that appears in both documents is generated here, so
# the two can never drift apart. Edit here, re-render both.
# ------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

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
  n <- 400
  intent  <- rnorm(n)                                    # latent purchase intent (unobserved)
  exposed <- rbinom(n, 1, plogis(1.6 * intent - 0.4))    # high-intent users search for eBay -> see & click the ads
  # purchases are driven by intent; the TRUE effect of ad exposure is exactly zero
  purchases <- pmax(0, 20 + 12 * intent + rnorm(n, 0, 6))
  d <- data.frame(
    group = factor(exposed, labels = c("Arrived via other channels\n(organic, referrals, direct)",
                                       "Arrived via\npaid search click")),
    purchases = purchases
  )
  ggplot(d, aes(group, purchases, color = group)) +
    geom_jitter(width = 0.14, alpha = 0.45, size = 1.8) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.4, linewidth = 0.8) +
    scale_color_manual(values = c(wu_grey, wu_blue)) +
    labs(
      title = "What the analysts saw",
      subtitle = "Purchases per user, by channel of arrival (simulated)",
      x = NULL, y = "Purchases"
    ) +
    theme_course() + theme(legend.position = "none")
}

# --- 1.4 Correlation, causation & what data can show --------------------

fig_two_pictures <- function() {
  set.seed(42)
  n <- 120
  true_effect <- 3
  
  # (a) EXPERIMENTAL data: delivery randomized (holdout among subscribers)
  d_exp <- data.frame(sub = rbinom(n, 1, 0.5), intent = rnorm(n))
  d_exp$purchases <- pmax(0, 20 + true_effect * d_exp$sub + 8 * d_exp$intent + rnorm(n, 0, 4))
  
  # (b) OBSERVATIONAL data: delivery self-selected (subscribing driven by intent)
  d_obs <- data.frame(intent = rnorm(n))
  d_obs$sub <- rbinom(n, 1, plogis(1.5 * d_obs$intent - 0.2))
  d_obs$purchases <- pmax(0, 20 + true_effect * d_obs$sub + 8 * d_obs$intent + rnorm(n, 0, 4))
  
  lab <- c("No newsletter", "Receives\nnewsletter")
  d_exp$group <- factor(d_exp$sub, labels = lab)
  d_obs$group <- factor(d_obs$sub, labels = lab)
  
  gap <- function(d) diff(tapply(d$purchases, d$group, mean))
  
  panel <- function(d, title, subtitle) {
    ggplot(d, aes(group, purchases, color = group)) +
      geom_jitter(width = 0.12, alpha = 0.6, size = 2) +
      stat_summary(fun = mean, geom = "crossbar", width = 0.35, linewidth = 0.7) +
      scale_color_manual(values = c(wu_grey, wu_blue)) +
      annotate("text", x = 1.5, y = 47,
               label = sprintf("observed difference: +%.0f", gap(d)),
               size = 4.6, fontface = "bold", color = wu_blue) +
      coord_cartesian(ylim = c(0, 48)) +
      labs(title = title, subtitle = subtitle, x = NULL, y = "Purchases") +
      theme_course() + theme(legend.position = "none")
  }
  
  p1 <- panel(d_exp, "(a) Experimental data",
              "Delivery randomized: holdout among subscribers")
  p2 <- panel(d_obs, "(b) Observational data",
              "Delivery self-selected: chosen by the customer")
  patchwork::wrap_plots(p1, p2, ncol = 2)
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
  edges <- data.frame(x = 4.35, xend = 5.65, y = 3, yend = 3, style = "solid")
  draw_diagram(nodes, edges, xlim = c(0.9, 9.1), ylim = c(2.1, 3.9), label_size = 8)
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
    theme_course() +
    theme(panel.spacing.x = unit(2.4, "cm"))
}

# --- 1.1 Motivation: AI adoption trajectory (The CMO Survey 2026) ---------

fig_ai_adoption <- function() {
  d <- data.frame(
    period = factor(c("2024", "2025", "2026", "In 3 years\n(projected)"),
                    levels = c("2024", "2025", "2026", "In 3 years\n(projected)")),
    share = c(13.1, 17.2, 24.2, 55.9),
    status = c("actual", "actual", "actual", "projected")
  )
  ggplot(d, aes(period, share, fill = status)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = paste0(share, "%")), vjust = -0.55,
              fontface = "bold", size = 4.6, color = wu_dark) +
    scale_fill_manual(values = c(actual = wu_blue, projected = wu_orange)) +
    scale_y_continuous(limits = c(0, 63), expand = expansion(mult = c(0, 0.02))) +
    labs(
      title = "Share of marketing activities performed by AI/ML",
      subtitle = "Company use of AI/ML to optimize and automate marketing",
      x = NULL, y = "% of marketing activities",
      caption = "Source: The CMO Survey (2026), 35th edition, n = 308 U.S. marketing leaders"
    ) +
    theme_course() +
    theme(legend.position = "none",
          plot.caption = element_text(color = "grey45", size = rel(0.75)))
}


# --- eBay case DAG: naive view and confounded view -----------------------

dgm_ebay_naive_view <- function() {
  nodes <- data.frame(
    x = c(2.5, 7.5), y = c(2.5, 2.5),
    label = c("Search ad\nexposure", "Sales"),
    role = c("main", "main")
  )
  edges <- data.frame(x = 3.9, y = 2.5, xend = 6.4, yend = 2.5, style = "dashed")
  draw_diagram(nodes, edges, xlim = c(0, 10), ylim = c(0.9, 5.4), label_size = 6.5) +
    annotate("text", x = 5.15, y = 1.55, label = "the correlation we see",
             size = 4.6, color = wu_grey, fontface = "italic")
}

dgm_ebay_confounded <- function() {
  nodes <- data.frame(
    x = c(2.5, 7.5, 5), y = c(2.5, 2.5, 4.6),
    label = c("Search ad\nexposure", "Sales", "Purchase intent\n(unobserved)"),
    role = c("main", "main", "confounder")
  )
  edges <- data.frame(
    x    = c(3.9, 4.15, 5.85),
    y    = c(2.5, 4.15, 4.15),
    xend = c(6.4, 2.85, 7.15),
    yend = c(2.5, 3.05, 3.05),
    style = c("dashed", "solid", "solid")
  )
  draw_diagram(nodes, edges, xlim = c(0, 10), ylim = c(0.9, 5.4), label_size = 6.5) +
    annotate("text", x = 5.15, y = 1.55, label = "the correlation we see",
             size = 4.6, color = wu_grey, fontface = "italic")
}


# --- Spurious correlation warm-up: IE market share vs US murders ---------
# Real public data (approx.): FBI UCR murder counts; IE desktop share.
# The dual axis is the point: ranges chosen so the series overlap.

fig_spurious_ie <- function() {
  d <- data.frame(
    year    = 2006:2011,
    murders = c(17030, 16929, 16442, 15399, 14722, 14661),
    ie      = c(83, 79, 72, 65, 52, 42)
  )
  # map share (30-90%) onto the murder axis (14000-18000)
  d$ie_scaled <- 14000 + (d$ie - 30) / 60 * 4000
  ggplot(d, aes(x = factor(year))) +
    geom_col(aes(y = ie_scaled), fill = wu_green, alpha = 0.75, width = 0.55) +
    geom_line(aes(y = murders, group = 1), color = wu_blue, linewidth = 1.4) +
    geom_point(aes(y = murders), color = wu_blue, size = 3, shape = 21,
               fill = "white", stroke = 1.6) +
    scale_y_continuous(
      name = "Murders in the US",
      labels = scales::comma,
      sec.axis = sec_axis(~ (. - 14000) / 4000 * 60 + 30,
                          name = "Internet Explorer market share",
                          labels = function(x) paste0(x, "%"))
    ) +
    labs(title = "Internet Explorer vs. the US murder rate",
         subtitle = "Two series, one decade, r \u2248 0.98",
         x = NULL) +
    coord_cartesian(ylim = c(14000, 18100)) +
    theme_course()
}

# --- Newsletter example DAG: real effect + confounding ------------------

dgm_newsletter <- function() {
  nodes <- data.frame(
    x = c(2.5, 7.5, 5), y = c(2.5, 2.5, 4.6),
    label = c("Newsletter\ndelivery", "Purchases", "Brand interest\n(unobserved)"),
    role = c("main", "main", "confounder")
  )
  edges <- data.frame(
    x    = c(3.9, 4.15, 5.85),
    y    = c(2.5, 4.15, 4.15),
    xend = c(6.15, 2.85, 7.15),
    yend = c(2.5, 3.05, 3.05),
    style = c("solid", "solid", "solid")
  )
  draw_diagram(nodes, edges, xlim = c(0, 10), ylim = c(0.9, 5.4), label_size = 6.5) +
    annotate("text", x = 5.15, y = 2.9, label = "true effect: +3",
             size = 4.8, fontface = "bold", color = wu_blue) +
    annotate("text", x = 5.15, y = 1.55,
             label = "observed in (b): +11  =  +3 effect  +  ~8 selection",
             size = 4.6, color = wu_grey, fontface = "italic")
}


fig_four_scatters <- function() {
  set.seed(7)
  n <- 110
  
  # 1: genuinely causal (e.g., randomized discount depth -> purchases)
  d1 <- data.frame(x = rnorm(n)) |>
    mutate(y = 0.7 * x + rnorm(n, 0, 0.7),
           panel = "1 · Causal: X → Y")
  
  # 2: confounded (segment drives both; no effect of x within segment)
  seg2 <- rep(c(0, 1), each = n / 2)
  d2 <- data.frame(
    x = 1.4 * seg2 + rnorm(n, 0, 0.55),
    y = 1.4 * seg2 + rnorm(n, 0, 0.55),
    segment = factor(seg2, labels = c("Segment A", "Segment B")),
    panel = "2 · Confounded: Z → X, Z → Y"
  )
  
  # 3: spurious - two independent trending series
  t <- 1:n
  d3 <- data.frame(
    x = scale(cumsum(rnorm(n)) + 0.05 * t)[, 1],
    y = scale(cumsum(rnorm(n)) + 0.05 * t)[, 1],
    panel = "3 · Spurious: shared trend / chance"
  )
  
  # 4: Simpson's paradox - within each tier the slope is NEGATIVE,
  #    but the tier shifts both X and Y upward -> pooled slope positive
  seg4 <- rep(0:2, length.out = n)
  d4 <- data.frame(seg = seg4) |>
    mutate(
      x = 1.5 * seg + rnorm(n, 0, 0.45),
      y = 1.5 * seg - 0.55 * (x - 1.5 * seg) + rnorm(n, 0, 0.35),
      segment = factor(seg, labels = c("Tier A", "Tier B", "Tier C")),
      panel = "4 · Simpson: within groups it reverses"
    )
  
  base <- function(d) {
    ggplot(d, aes(x, y)) +
      geom_smooth(method = "lm", se = FALSE, color = wu_orange, linewidth = 0.9) +
      labs(x = "X", y = "Y") + theme_course(base_size = 12) +
      facet_wrap(~panel)
  }
  
  p1 <- base(d1) + geom_point(color = wu_blue, alpha = 0.6, size = 1.6)
  p2 <- base(d2) + geom_point(aes(color = segment), alpha = 0.7, size = 1.6) +
    scale_color_manual(values = c(wu_grey, wu_red)) +
    theme(legend.position = "none")
  p3 <- base(d3) + geom_point(color = wu_blue, alpha = 0.6, size = 1.6)
  p4 <- base(d4) +
    geom_point(aes(color = segment), alpha = 0.75, size = 1.6) +
    geom_smooth(aes(group = segment, color = segment), method = "lm",
                se = FALSE, linewidth = 0.7) +
    scale_color_manual(values = c(wu_grey, wu_green, wu_red)) +
    theme(legend.position = "none")
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p1, p2, p3, p4, ncol = 4)
  } else {
    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 4)
  }
}

# --- DGPs behind the four scatter panels --------------------------------

fig_four_dgps <- function() {
  mini <- function(title, top_label = NULL, xy_arrow = c("none", "solid", "dashed"),
                   xy_label = NULL) {
    xy_arrow <- match.arg(xy_arrow)
    nodes <- data.frame(
      x = c(2.2, 7.8), y = c(2.2, 2.2),
      label = c("X", "Y"), role = c("main", "main")
    )
    edges <- NULL
    if (!is.null(top_label)) {
      nodes <- rbind(nodes, data.frame(x = 5, y = 5.2, label = top_label,
                                       role = "confounder"))
      edges <- rbind(edges,
                     data.frame(x = 4.0, y = 4.55, xend = 2.55, yend = 3.0, style = "solid"),
                     data.frame(x = 6.0, y = 4.55, xend = 7.45, yend = 3.0, style = "solid"))
    }
    if (xy_arrow != "none") {
      edges <- rbind(edges,
                     data.frame(x = 3.2, y = 2.2, xend = 6.6, yend = 2.2, style = xy_arrow))
    }
    p <- draw_diagram(nodes, edges, xlim = c(0.5, 9.5), ylim = c(0.6, 6.4),
                      label_size = 5) +
      labs(title = title) +
      theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
    if (!is.null(xy_label)) {
      p <- p + annotate("text", x = 5, y = 1.35, label = xy_label,
                        size = 4.2, color = wu_grey, fontface = "italic")
    }
    p
  }
  
  p1 <- mini("1 · Causal", top_label = NULL, xy_arrow = "solid")
  p2 <- mini("2 · Confounded", top_label = "Segment",
             xy_arrow = "dashed", xy_label = "≈ 0")
  p3 <- mini("3 · Spurious", top_label = "Time",
             xy_arrow = "none", xy_label = "no link at all")
  p4 <- mini("4 · Simpson", top_label = "Tier",
             xy_arrow = "solid", xy_label = "negative within")
  
  patchwork::wrap_plots(p1, p2, p3, p4, ncol = 4)
}

# --- Targeting DAG: algorithmic selection into personalized ads ---------

dgm_targeting <- function() {
  nodes <- data.frame(
    x = c(2.5, 7.5, 5), y = c(2.5, 2.5, 4.6),
    label = c("Personalized\nad exposure", "Purchase", "Category interest\n(unobserved)"),
    role = c("main", "main", "confounder")
  )
  edges <- data.frame(
    x    = c(3.9, 4.15, 5.85),
    y    = c(2.5, 4.15, 4.15),
    xend = c(6.3, 2.85, 7.15),
    yend = c(2.5, 3.05, 3.05),
    style = c("dashed", "solid", "solid")
  )
  draw_diagram(nodes, edges, xlim = c(0, 10), ylim = c(0.9, 5.4), label_size = 6.5) +
    annotate("text", x = 5.1, y = 1.55, label = "the lift the dashboard reports",
             size = 4.6, color = wu_grey, fontface = "italic")
}
# --- 1.7 Quasi-experiments ----------------------------------------------
# Figures for the quasi-experiment / difference-in-differences slides.
# Numbers in the Spotify figures are the stylized values from
# Wlömert & Papies (2016) used on the slides: monthly spending on
# CDs & downloads, in EUR.

spotify_spend <- data.frame(
  period = factor(c("T1", "T1", "T2", "T2"), levels = c("T1", "T2")),
  group  = factor(c("Non-Spotify\nusers", "Spotify\nusers",
                    "Non-Spotify\nusers", "Spotify\nusers"),
                  levels = c("Non-Spotify\nusers", "Spotify\nusers")),
  spend  = c(4.60, 10.40, 4.60, 7.90)
)

eur <- function(x) paste0(formatC(x, format = "f", digits = 2), "\u20ac")

# (1) What the press saw: a single cross-section at T2
fig_spotify_cross_section <- function(base_size = 12) {
  d <- subset(spotify_spend, period == "T2")
  ggplot(d, aes(group, spend, fill = group)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = eur(spend)), vjust = -0.5, size = base_size / 3.2, fontface = "bold") +
    scale_fill_manual(values = c(wu_grey, wu_blue)) +
    scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    labs(title = "Monthly spending on CDs & downloads",
         subtitle = "One point in time",
         x = NULL, y = NULL) +
    theme_course(base_size = base_size) +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.line.x = element_line(linewidth = 1),
          plot.title.position = "plot")
}

# (2) Cross-sectional vs. longitudinal design
fig_design_timeline <- function() {
  ellipse <- function(x0, y0, a = 1.25, b = 0.85, id) {
    t <- seq(0, 2 * pi, length.out = 120)
    data.frame(x = x0 + a * cos(t), y = y0 + b * sin(t), id = id)
  }
  ells <- rbind(
    ellipse(3.8, 4.3, id = "cs"),
    ellipse(3.8, 1.9, id = "l1"),
    ellipse(7.8, 1.9, id = "l2")
  )
  fills <- c(cs = "#8fc3e6", l1 = wu_dark, l2 = wu_dark)
  labs_df <- data.frame(
    x = c(3.8, 3.8, 7.8),
    y = c(4.3, 1.9, 1.9),
    label = c("Sample\nsurveyed\nat T1", "Sample\nsurveyed\nat T1",
              "Same sample\nsurveyed again\nat T2")
  )
  ggplot() +
    geom_polygon(data = ells, aes(x, y, group = id, fill = id),
                 color = "grey40", linewidth = 0.4) +
    scale_fill_manual(values = fills) +
    geom_text(data = labs_df, aes(x, y, label = label),
              color = "white", size = 5.2, fontface = "bold", lineheight = 0.9) +
    annotate("text", x = 0.1, y = 4.3, hjust = 0, size = 5.6, fontface = "bold",
             label = "Cross-sectional\ndesign", lineheight = 0.9) +
    annotate("text", x = 0.1, y = 1.9, hjust = 0, size = 5.6, fontface = "bold",
             label = "Longitudinal\ndesign", lineheight = 0.9) +
    # time axis
    annotate("segment", x = 2.3, xend = 9.9, y = 0.6, yend = 0.6,
             linewidth = 1.6, arrow = arrow(length = unit(0.35, "cm"), type = "closed")) +
    annotate("segment", x = c(3.8, 7.8), xend = c(3.8, 7.8), y = 0.6, yend = 0.35, linewidth = 0.8) +
    annotate("text", x = c(3.8, 7.8), y = 0.1, label = c("T[1]", "T[2]"), parse = TRUE, size = 6) +
    annotate("text", x = 10.1, y = 0.6, label = "Time", hjust = 0, size = 5.6) +
    coord_fixed(xlim = c(0, 10.9), ylim = c(-0.1, 5.3)) +
    theme_void() + theme(legend.position = "none")
}

# (3) Stylized difference-in-differences logic
#     Negative treatment effect, consistent with the Spotify example:
#     the treated group ends up BELOW its counterfactual.
fig_did_stylized <- function() {
  x <- c(pre = 0, intervention = 1, post = 2)
  ctrl  <- c(0.8, 1.6, 2.4)
  treat <- c(2.6, 3.4, 3.0)
  cf    <- treat[2] + (ctrl[3] - ctrl[2])          # counterfactual at post (= 4.2)
  lines <- rbind(
    data.frame(x = x, y = ctrl,  series = "Control group"),
    data.frame(x = x, y = treat, series = "Treated group")
  )
  ggplot(lines, aes(x, y, color = series)) +
    annotate("rect", xmin = 1, xmax = 2.35, ymin = -Inf, ymax = Inf,
             fill = "grey95") +
    geom_vline(xintercept = 1, linetype = "dotted", color = "grey55") +
    annotate("segment", x = 1, xend = 2, y = treat[2], yend = cf,
             linetype = "dashed", color = wu_blue, linewidth = 1) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 3.5) +
    annotate("point", x = 2, y = cf, size = 3.5, shape = 21,
             fill = "white", color = wu_blue, stroke = 1.3) +
    annotate("segment", x = 2.1, xend = 2.1, y = cf, yend = treat[3],
             color = wu_red, linewidth = 1.1,
             arrow = arrow(ends = "both", length = unit(0.22, "cm"), type = "closed")) +
    annotate("text", x = 2.16, y = (cf + treat[3]) / 2, label = "Delta < 0",
             parse = TRUE, color = wu_red, size = 6, hjust = 0) +
    annotate("text", x = 1.5, y = (treat[2] + cf) / 2, label = "counterfactual",
             color = wu_blue, fontface = "italic", size = 4.6, angle = 16, vjust = -0.9) +
    annotate("text", x = -0.06, y = treat[1], label = "Y[t*','*pre]", parse = TRUE,
             hjust = 1, size = 5, color = wu_dark) +
    annotate("text", x = -0.06, y = ctrl[1], label = "Y[c*','*pre]", parse = TRUE,
             hjust = 1, size = 5, color = wu_green) +
    annotate("text", x = 1.94, y = treat[3] - 0.3, label = "Y[t*','*post]", parse = TRUE,
             hjust = 1, size = 5, color = wu_dark) +
    annotate("text", x = 2.06, y = ctrl[3] - 0.2, label = "Y[c*','*post]", parse = TRUE,
             hjust = 0, size = 5, color = wu_green) +
    scale_color_manual(values = c("Control group" = wu_green, "Treated group" = wu_dark)) +
    scale_x_continuous(breaks = x, labels = names(x), limits = c(-0.35, 2.55)) +
    scale_y_continuous(limits = c(0.4, 4.7)) +
    labs(x = NULL, y = "Outcome", color = NULL) +
    theme_course(base_size = 16) +
    theme(axis.text.y = element_blank(), panel.grid = element_blank(),
          axis.line = element_line(color = "grey40"))
}

# (4) The Spotify example as difference-in-differences
fig_spotify_did <- function() {
  d <- spotify_spend
  d$label <- eur(d$spend)
  d$label[d$period == "T2"] <- paste0(
    d$label[d$period == "T2"], "\n(",
    c("\u00b10%", "\u221224%"), ")"
  )
  ggplot(d, aes(group, spend, fill = group)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = label), vjust = -0.3, size = 5.4, lineheight = 0.9) +
    facet_wrap(~period, strip.position = "bottom",
               labeller = as_labeller(c(T1 = "T1 (before)", T2 = "T2 (after)"))) +
    scale_fill_manual(values = c(wu_grey, wu_blue)) +
    scale_y_continuous(limits = c(0, 12.8), expand = c(0, 0)) +
    labs(title = "Monthly spending on CDs & downloads",
         subtitle = "Same consumers, observed twice",
         x = NULL, y = NULL) +
    theme_course(base_size = 16) +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.line.x = element_line(linewidth = 1.2),
          strip.placement = "outside",
          strip.text = element_text(face = "bold", size = rel(1.05)),
          panel.spacing = unit(2.5, "lines"))
}

# --- Who turns analytics into decisions? (own drawing; concept after
#     Henke, Levine & McInerney, 2018, McKinsey Quarterly) ---------------
fig_analytics_roles <- function(base_size = 13) {
  circle <- function(x0, y0, r, id, n = 200) {
    t <- seq(0, 2 * pi, length.out = n)
    data.frame(x = x0 + r * cos(t), y = y0 + r * sin(t), id = id)
  }
  r <- 1.5
  circles <- rbind(circle(0, 0.9, r, "Analytics"),
                   circle(-0.85, -0.55, r, "IT & data"),
                   circle(0.85, -0.55, r, "Business"))
  areas <- data.frame(
    x = c(0, -1.75, 1.75, -0.95, 0.95, 0, 0),
    y = c(1.75, -1.0, -1.0, 0.45, 0.45, -1.15, -0.05),
    label = c("Analytics", "IT & data", "Business", "Data\nscientists",
              "Translators", "Data\nengineers", "Head of\nanalytics"),
    face = c("bold", "bold", "bold", "plain", "bold", "plain", "plain"),
    size = c(1.25, 1.25, 1.25, 1, 1.05, 1, 0.95) * base_size / 3.2
  )
  notes <- data.frame(
    x = c(-4.1, 3.05, 3.05), y = c(1.35, 1.35, -1.95),
    label = c("build and maintain the\nmodels and algorithms",
              "understand business needs,\nframe the right question, and\nturn results into decisions",
              "make sure the data\nneeded tomorrow exist\nand are reliable"),
    head = c("Data scientists", "Translators", "Data engineers"),
    hjust = c(0, 0, 0)
  )
  ggplot() +
    geom_polygon(data = circles, aes(x, y, group = id), fill = wu_blue, alpha = 0.22, color = "white", linewidth = 1) +
    geom_text(data = areas, aes(x, y, label = label, fontface = face, size = size),
              color = wu_dark, lineheight = 0.9) +
    scale_size_identity() +
    geom_text(data = notes, aes(x, y, label = head, hjust = hjust), size = base_size / 3.4,
              color = wu_dark, fontface = "bold", vjust = -0.3) +
    geom_text(data = notes, aes(x, y, label = label, hjust = hjust), size = base_size / 3.6,
              color = "grey20", lineheight = 0.95, vjust = 1.1) +
    annotate("segment", x = c(-2.15, 2.95, 2.95), xend = c(-1.35, 1.55, 0.35),
             y = c(1.2, 1.35, -1.95), yend = c(0.5, 0.6, -1.35), color = "grey60", linewidth = 0.4) +
    coord_fixed(xlim = c(-4.2, 6.2), ylim = c(-2.6, 2.6), clip = "off") +
    theme_void() +
    theme(legend.position = "none")
}


# --- Divergent delivery (own illustration after Braun & Schwartz, 2025) ---
fig_divergent_delivery <- function(base_size = 13) {
  set.seed(5)
  mk <- function(n, share_a, x0, row) {
    k <- ceiling(sqrt(n))
    g <- expand.grid(i = 0:(k - 1), j = 0:(k - 1))[1:n, ]
    type <- c(rep("Sports fans", round(n * share_a)), rep("Music fans", n - round(n * share_a)))
    data.frame(x = x0 + g$i * 0.32, y = row + g$j * 0.32 - (k - 1) * 0.16,
               type = sample(type))
  }
  pts <- rbind(
    cbind(mk(25, 0.5, 0, 2), panel = "a"), cbind(mk(25, 0.5, 0, 0), panel = "b"),
    cbind(mk(16, 0.85, 4.6, 2), panel = "c"), cbind(mk(16, 0.15, 4.6, 0), panel = "d")
  )
  ggplot(pts, aes(x, y)) +
    annotate("segment", x = 1.75, xend = 4.2, y = c(2, 0), yend = c(2, 0),
             arrow = arrow(length = unit(0.25, "cm"), type = "closed"), color = wu_grey) +
    annotate("text", x = 2.95, y = c(2.35, 0.35), label = "delivery\nalgorithm", size = base_size / 3.8,
             color = wu_grey, lineheight = 0.9) +
    geom_point(aes(color = type), size = 4) +
    scale_color_manual(values = c(`Sports fans` = wu_orange, `Music fans` = wu_blue), name = NULL) +
    annotate("text", x = -0.6, y = c(2, 0), label = c("Ad A", "Ad B"), hjust = 1,
             fontface = "bold", size = base_size / 3.2, color = wu_dark) +
    annotate("text", x = c(0.64, 5.08), y = 3.25, label = c("Randomized users\n(balanced)", "Who actually sees the ad\n(not balanced)"),
             fontface = "bold", size = base_size / 3.6, color = wu_dark, lineheight = 0.9) +
    coord_fixed(xlim = c(-1.6, 6.6), ylim = c(-0.9, 3.6), clip = "off") +
    theme_void(base_size = base_size) +
    theme(legend.position = "bottom")
}