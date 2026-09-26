# ======================================================================
# Marketing Analytics — Chapter 4: Learning from Experiments
# Companion code to Chapter 4 of the course book. The chapter explains
# every step in detail; this script collects the code in one place.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------------------

library(tidyverse)
options(scipen = 999)


# ======================================================================
# 4.1 Back to the A/B test
# ======================================================================

# The Smart Mix A/B test from Chapter 3 (simulated data):
# 400 users, randomly split; outcome: listening hours in four weeks
set.seed(31)
ab_test <- tibble(
  group = factor(rep(c("Control", "Smart Mix"), each = 200), levels = c("Smart Mix", "Control")),
  hours = c(rgamma(200, shape = 2, scale = 10), rgamma(200, shape = 2, scale = 11.5))
)

ab_test |>
  group_by(group) |>
  summarise(n = n(), mean = mean(hours), sd = sd(hours))

ggplot(ab_test, aes(x = group, y = hours)) +
  geom_boxplot(fill = "#dbe7f3") +
  stat_summary(fun = mean, geom = "point", color = "#0f5da8", size = 4) +
  labs(x = NULL, y = "Listening hours (four weeks)") +
  theme_minimal()


# ======================================================================
# 4.2 The difference between two means
# ======================================================================

# The standard error of the difference — computed once by hand
stats <- ab_test |> group_by(group) |> summarise(mean = mean(hours), sd = sd(hours), n = n())
difference <- stats$mean[1] - stats$mean[2]
se_diff    <- sqrt(stats$sd[1]^2 / stats$n[1] + stats$sd[2]^2 / stats$n[2])
c(difference = difference, se = se_diff, t = difference / se_diff)

# A world without any effect: 10,000 simulated A/B tests
set.seed(1)
null_diffs <- replicate(10000, mean(rgamma(200, 2, scale = 10.75)) - mean(rgamma(200, 2, scale = 10.75)))
mean(abs(null_diffs) >= abs(difference))   # share at least as extreme as observed


# ======================================================================
# 4.3 Testing a hypothesis
# ======================================================================

t.test(hours ~ group, data = ab_test)

# Only the confidence interval of the difference, or only the p-value
t.test(hours ~ group, data = ab_test)$conf.int
t.test(hours ~ group, data = ab_test)$p.value

# Your turn: the same test with only the first 50 users per group
t.test(hours ~ group, data = ab_test |> group_by(group) |> slice_head(n = 50))


# ======================================================================
# 4.4 How large is the effect?
# ======================================================================

# Relative effect (in % of the control group)
100 * difference / stats$mean[2]

# Cohen's d: difference divided by the pooled standard deviation
pooled_sd <- sqrt(mean(stats$sd^2))
difference / pooled_sd


# ======================================================================
# 4.6 Comparing shares
# ======================================================================

# Premium trial offer: 131 of 2,000 vs. 104 of 2,000 users upgrade
prop.test(x = c(131, 104), n = c(2000, 2000))

# The same as a chi-square test on the 2 x 2 table
upgrades <- matrix(c(131, 2000 - 131, 104, 2000 - 104), nrow = 2, byrow = TRUE,
                   dimnames = list(c("Trial offer", "Standard page"), c("upgraded", "not upgraded")))
upgrades
chisq.test(upgrades)

# Your turn: Jon (200 of 300) vs. Mark (100 of 300)
prop.test(x = c(200, 100), n = c(300, 300))


# ======================================================================
# 4.7 Errors, power, and sample size
# ======================================================================

# Power of the Smart Mix experiment (d = 0.22, 200 users per group)
power.t.test(n = 200, delta = 0.22, sd = 1)

# Users per group needed for 80% power
power.t.test(delta = 0.22, sd = 1, sig.level = 0.05, power = 0.80)

# Shares: from 5% to 6%
power.prop.test(p1 = 0.05, p2 = 0.06, sig.level = 0.05, power = 0.80)

# Online advertising: click-through rate from 0.50% to 0.55% (+10%)
power.prop.test(p1 = 0.005, p2 = 0.0055, power = 0.80)

# Your group project: smallest detectable effect with 60 per group
power.t.test(n = 60, sd = 1, power = 0.80)

# Your turn: newsletter click-through rate 2% -> 2.2% and 2% -> 2.5%
power.prop.test(p1 = 0.02, p2 = 0.022, power = 0.80)
power.prop.test(p1 = 0.02, p2 = 0.025, power = 0.80)


# ======================================================================
# 4.8 Other designs and tests
# ======================================================================

# --- More than two groups: ANOVA ------------------------------------------
set.seed(14)
mix_test <- tibble(
  version = factor(rep(c("No mix", "Weekly mix", "Daily mix"), each = 150),
                   levels = c("No mix", "Weekly mix", "Daily mix")),
  hours   = c(rgamma(150, 2, scale = 10), rgamma(150, 2, scale = 10.8), rgamma(150, 2, scale = 12))
)

mix_test |> group_by(version) |> summarise(n = n(), mean = mean(hours))

summary(aov(hours ~ version, data = mix_test))

# Post-hoc tests: all pairs, corrected for multiple comparisons
pairwise.t.test(mix_test$hours, mix_test$version, p.adjust.method = "bonferroni")

# Why correct? Chance of at least one false alarm with k tests
k <- c(1, 3, 10, 20)
1 - 0.95^k

# --- Same people in both conditions: paired t-test ---------------------
set.seed(4)
skill <- rnorm(60, 0, 8)
search_test <- tibble(
  old_design = round(40 + skill + rnorm(60, 0, 3), 1),
  new_design = round(38 + skill + rnorm(60, 0, 3), 1)
)

t.test(search_test$new_design, search_test$old_design, paired = TRUE)
t.test(search_test$new_design, search_test$old_design)   # wrong test for this design

# --- Non-parametric alternatives -----------------------------------------
wilcox.test(hours ~ group, data = ab_test)          # two groups
kruskal.test(hours ~ version, data = mix_test)      # three or more groups


# ======================================================================
# 4.9 Using p-values responsibly: the danger of peeking
# ======================================================================

# 200 A/B tests WITHOUT any effect, checked after every 20 users per group
set.seed(2024)
looks <- seq(20, 1000, by = 20)
ever_significant <- replicate(200, {
  a <- rnorm(1000); b <- rnorm(1000)
  p <- sapply(looks, function(n) t.test(a[1:n], b[1:n])$p.value)
  any(p < 0.05)
})
mean(ever_significant)   # far more than 5%
