# ======================================================================
# Marketing Analytics — Chapter 4: Learning from Experiments
# The tests of Chapter 4 in R. The chapter on the course website explains
# the logic, the simulations, and the interpretation; this script only
# contains the code you need to run the tests on your own data.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Setup and data
# ----------------------------------------------------------------------

library(tidyverse)
library(psych)        # cohen.d()
options(scipen = 5, digits = 4)

base_url <- "https://raw.githubusercontent.com/wu-rds/MA2026/main/data/"

# The Smart Mix A/B test: 400 users, randomly split; listening hours in four weeks
ab_test <- read_csv(paste0(base_url, "ab_test.csv")) |>
  mutate(group = factor(group, levels = c("Smart Mix", "Control")))

# Three versions of Smart Mix: 150 users each
mix_test <- read_csv(paste0(base_url, "mix_test.csv")) |>
  mutate(version = factor(version, levels = c("No mix", "Weekly mix", "Daily mix")))

# The search-design test: 60 users, each tried both designs (seconds per search)
search_test <- read_csv(paste0(base_url, "search_test.csv"))


# ======================================================================
# 4.1–4.3 Two groups: the t-test
# ======================================================================

# --- Step 1: look at the data ---------------------------------------------
# Boxplot with the individual users on top: how many, how spread out, outliers?
ggplot(ab_test, aes(x = group, y = hours)) +
  geom_boxplot(fill = "#dbe7f3", outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", color = "#b03a2e", size = 3) +    # red dot = mean
  labs(x = NULL, y = "Listening hours (four weeks)") +
  theme_minimal()

# --- Step 2: the group summaries you will report --------------------------
ab_summary <- ab_test |>
  group_by(group) |>
  summarise(n = n(), mean = mean(hours), sd = sd(hours), se = sd / sqrt(n)) |>
  mutate(lower = mean - qt(0.975, n - 1) * se,       # 95% confidence interval
         upper = mean + qt(0.975, n - 1) * se)
ab_summary

# --- Step 3: the chart that goes with the test ----------------------------
# Group means with their 95% confidence intervals
ggplot(ab_summary, aes(x = group, y = mean)) +
  geom_col(width = 0.55, fill = "#0f5da8") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  labs(x = NULL, y = "Mean listening hours (95% CI)") +
  theme_minimal()

# (The same in one pipe — handy for a quick look, but keep the summary
#  as a separate object when you need it for a table or a report.)
ab_test |>
  group_by(group) |>
  summarise(n = n(), mean = mean(hours), se = sd(hours) / sqrt(n)) |>
  mutate(lower = mean - qt(0.975, n - 1) * se, upper = mean + qt(0.975, n - 1) * se) |>
  ggplot(aes(x = group, y = mean)) +
  geom_col(width = 0.55, fill = "#0f5da8") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  theme_minimal()

# --- Step 4: the test -------------------------------------------------------
# Welch t-test (R's default: equal variances not assumed)
t.test(hours ~ group, data = ab_test)

# Pieces of the output you may want separately
t_result <- t.test(hours ~ group, data = ab_test)
t_result$estimate    # the two group means
t_result$conf.int    # 95% confidence interval of the difference
t_result$p.value

# One-sided test (only if decided before seeing the data)
t.test(hours ~ group, data = ab_test, alternative = "greater")


# ======================================================================
# 4.4 The same people in both conditions: the paired t-test
# ======================================================================

# Between-subjects design (each person in ONE condition)  -> Welch t-test, as above
# Within-subjects design  (each person in ALL conditions) -> paired t-test

# The search-design test: one row per user, one column per design
head(search_test)

# --- The data in "long" format: one row per user AND design -----------------
search_long <- search_test |>
  pivot_longer(c(old_design, new_design), names_to = "design", values_to = "seconds") |>
  mutate(design = factor(design, levels = c("old_design", "new_design")))
head(search_long)

# --- The chart that goes with the test: one line per user, old -> new ---------
ggplot(search_long, aes(x = design, y = seconds, group = user_id)) +
  geom_line(alpha = 0.4, color = "grey50") +
  geom_point(alpha = 0.6) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "#b03a2e", linewidth = 1.5) +
  labs(x = NULL, y = "Seconds to find a song") +
  theme_minimal()

# --- The usual report chart: the two condition means with 95% CIs --------------
# NOTE: these intervals reflect the differences BETWEEN users (the vertical
# spread above) and overlap a lot — they are not the uncertainty the paired
# test uses. Always add the paired test result to the caption.
search_summary <- search_long |>
  group_by(design) |>
  summarise(n = n(), mean = mean(seconds), se = sd(seconds) / sqrt(n)) |>
  mutate(lower = mean - qt(0.975, n - 1) * se, upper = mean + qt(0.975, n - 1) * se)
search_summary

ggplot(search_summary, aes(x = design, y = mean)) +
  geom_col(width = 0.55, fill = "#0f5da8") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  labs(x = NULL, y = "Mean seconds to find a song (95% CI)") +
  theme_minimal()

# --- The paired t-test: the mean of the within-user differences vs. zero -------
paired_result <- t.test(search_test$new_design, search_test$old_design, paired = TRUE)
paired_result

# The chart that matches the paired test: the mean difference with its 95% CI
difference_summary <- tibble(what = "New - old design",
                             estimate = paired_result$estimate,
                             lower = paired_result$conf.int[1], upper = paired_result$conf.int[2])
ggplot(difference_summary, aes(x = what, y = estimate)) +
  geom_col(width = 0.4, fill = "#0f5da8") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.12) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = NULL, y = "Mean within-user difference in seconds (95% CI)") +
  theme_minimal()

# The same data analyzed as if they were two independent groups (ignores the pairing):
# same difference, but a much larger p-value — comparing different people adds uncertainty
t.test(search_test$new_design, search_test$old_design)

# A paired t-test is nothing but a one-sample t-test of the within-user differences
# (compare the output with paired_result above: identical)
t.test(search_test$new_design - search_test$old_design)


# ======================================================================
# YOUR TURN: QuickBite, a food-delivery app (two tests on new data)
# ======================================================================
# The solutions are in 04-your-turn-solutions.R — try it yourself first.

# (1) BETWEEN-SUBJECTS: 300 new customers were randomly split; 150 received a
#     free-delivery voucher for their first four weeks, 150 did not.
#     Outcome: total order value (euros) in these four weeks.
delivery_voucher <- read_csv(paste0(base_url, "delivery_voucher.csv")) |>
  mutate(group = factor(group, levels = c("Voucher", "No voucher")))
head(delivery_voucher)

# Tasks: (a) look at the data (boxplot + jitter); (b) summarise the groups
#        (n, mean, SD, 95% CI) and draw the bar chart with intervals;
#        (c) run the appropriate t-test; (d) report the result in one sentence:
#        effect, uncertainty, test.

# your code here


# (2) WITHIN-SUBJECTS: 40 couriers delivered with the old routing app AND with
#     the new one (same type of shift, random order).
#     Outcome: average minutes per delivery with each app.
delivery_routes <- read_csv(paste0(base_url, "delivery_routes.csv"))
head(delivery_routes)

# Tasks: (a) draw the chart that shows the within-courier comparison;
#        (b) run the appropriate t-test and report the result;
#        (c) run the test you would use for a between-subjects design on the
#            same two columns — what changes, and why?

# your code here


# ======================================================================
# 4.5 How large is the effect?
# ======================================================================

# Relative effect in % of the control group
group_means <- ab_test |> group_by(group) |> summarise(mean = mean(hours))
100 * diff(rev(group_means$mean)) / group_means$mean[group_means$group == "Control"]

# Cohen's d with the psych package (the sign reflects the order of the groups)
cohen.d(ab_test$hours, ab_test$group)$cohen.d


# ======================================================================
# 4.7 Two shares: the test of proportions
# ======================================================================

# Premium trial offer: 131 of 2,000 vs. 104 of 2,000 users upgraded
upgrade_rates <- tibble(group = factor(c("Trial offer", "Standard page"), levels = c("Trial offer", "Standard page")),
                        upgrades = c(131, 104), n = c(2000, 2000)) |>
  mutate(rate = upgrades / n, se = sqrt(rate * (1 - rate) / n),
         lower = rate - 1.96 * se, upper = rate + 1.96 * se)
upgrade_rates

# The chart that goes with the test: shares with 95% confidence intervals
ggplot(upgrade_rates, aes(x = group, y = 100 * rate)) +
  geom_col(width = 0.55, fill = "#0f5da8") +
  geom_errorbar(aes(ymin = 100 * lower, ymax = 100 * upper), width = 0.15) +
  labs(x = NULL, y = "Upgrade rate (%) with 95% CI") +
  theme_minimal()

prop.test(x = c(131, 104), n = c(2000, 2000))

# With a data set instead of counts: successes and group sizes from the data
# prop.test(x = c(sum(upgraded[group == "A"]), sum(upgraded[group == "B"])),
#           n = c(sum(group == "A"), sum(group == "B")))

# The same as a chi-square test on the 2 x 2 table
upgrades <- matrix(c(131, 2000 - 131, 104, 2000 - 104), nrow = 2, byrow = TRUE,
                   dimnames = list(c("Trial offer", "Standard page"), c("upgraded", "not upgraded")))
chisq.test(upgrades)

# Your turn: Jon (200 of 300) vs. Mark (100 of 300)
prop.test(x = c(200, 100), n = c(300, 300))


# ======================================================================
# 4.8 Power and sample size — decide BEFORE the experiment
# ======================================================================

# Means: users per group for 80% power to detect an assumed effect of d = 0.22
power.t.test(delta = 0.22, sd = 1, sig.level = 0.05, power = 0.80)

# Means: power of a planned experiment with 200 users per group
power.t.test(n = 200, delta = 0.22, sd = 1, sig.level = 0.05)

# Shares: users per group to detect an increase from 5% to 6%
power.prop.test(p1 = 0.05, p2 = 0.06, sig.level = 0.05, power = 0.80)

# Shares: a click-through rate from 0.50% to 0.55% (+10%)
power.prop.test(p1 = 0.005, p2 = 0.0055, sig.level = 0.05, power = 0.80)

# Group project: smallest detectable effect with 60 respondents per group
power.t.test(n = 60, sd = 1, sig.level = 0.05, power = 0.80)

# Your turn: newsletter click-through rate 2% -> 2.2% and 2% -> 2.5%
power.prop.test(p1 = 0.02, p2 = 0.022, sig.level = 0.05, power = 0.80)
power.prop.test(p1 = 0.02, p2 = 0.025, sig.level = 0.05, power = 0.80)


# ======================================================================
# 4.9 Other designs
# ======================================================================

# --- Three or more groups: ANOVA ---------------------------------------
mix_summary <- mix_test |>
  group_by(version) |>
  summarise(n = n(), mean = mean(hours), sd = sd(hours), se = sd / sqrt(n)) |>
  mutate(lower = mean - qt(0.975, n - 1) * se, upper = mean + qt(0.975, n - 1) * se)
mix_summary

# The chart that goes with the test: the group means the ANOVA compares, with 95% CIs
ggplot(mix_summary, aes(x = version, y = mean)) +
  geom_col(width = 0.55, fill = "#0f5da8") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  labs(x = NULL, y = "Mean listening hours (95% CI)") +
  theme_minimal()

# Step 1: do the groups differ at all?
mix_aov <- aov(hours ~ version, data = mix_test)
summary(mix_aov)

# Welch ANOVA (does not assume equal variances)
oneway.test(hours ~ version, data = mix_test)

# Step 2: which groups differ? Corrected pairwise comparisons
pairwise.t.test(mix_test$hours, mix_test$version, p.adjust.method = "bonferroni")
pairwise.t.test(mix_test$hours, mix_test$version)   # default: Holm correction
TukeyHSD(mix_aov)

# --- Non-parametric alternatives ---------------------------------------
# Rank-based tests compare the typical value, not the mean: the chart that goes
# with them shows the distributions and their medians
ggplot(ab_test, aes(x = group, y = hours)) +
  geom_boxplot(fill = "#dbe7f3") +
  labs(x = NULL, y = "Listening hours (box: middle 50%, line: median)") +
  theme_minimal()

# Two independent groups: Wilcoxon rank-sum test (Mann-Whitney U test)
wilcox.test(hours ~ group, data = ab_test)

# Same people, two conditions: Wilcoxon signed-rank test
wilcox.test(search_test$new_design, search_test$old_design, paired = TRUE)

# Three or more independent groups: Kruskal-Wallis test + corrected pairs
ggplot(mix_test, aes(x = version, y = hours)) +
  geom_boxplot(fill = "#dbe7f3") +
  labs(x = NULL, y = "Listening hours (box: middle 50%, line: median)") +
  theme_minimal()

kruskal.test(hours ~ version, data = mix_test)
pairwise.wilcox.test(mix_test$hours, mix_test$version, p.adjust.method = "bonferroni")
