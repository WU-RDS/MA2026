# ======================================================================
# Marketing Analytics - Module 8: Survey design
# From the Qualtrics export to the results of a survey experiment.
# Example: "A seal for the coffee" - a two-condition experiment at the
# size of a group project: the product page of a coffee without vs. with
# a sustainability seal (Control vs. Seal), with a moderator
# (environmental concern, measured before the stimulus) and a mediator
# (anticipated warm glow, measured after it). The data are simulated; the
# file has the format of a real Qualtrics export. Use this script as the
# template for your own project.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------------------

library(tidyverse)
library(emmeans)      # install.packages("emmeans") if needed
library(mediation)    # install.packages("mediation") if needed
# The psych package is used for two functions below (psych::alpha, psych::cohen.d);
# install.packages("psych") if needed

# NOTE: the mediation package loads the package MASS, which has a select() function of
# its own. A plain select() then no longer calls the function from dplyr and stops with
# an error ("unused arguments"). This script therefore writes dplyr::select() everywhere -
# do the same in your project.

options(scipen = 5, digits = 4)
theme_set(theme_minimal(base_size = 13))

base_url <- "https://raw.githubusercontent.com/wu-rds/MA2026/main/data/"
file <- paste0(base_url, "coffee_seal_qualtrics.csv")
# For your own project: put the export into your project folder and use
# file <- "your_export.csv"


# ======================================================================
# Before the data collection: how many respondents do you need? (Chapter 4)
# ======================================================================
# Two numbers: the difference between the two conditions that you expect (here:
# 1 euro) and the standard deviation of the outcome (here: about 2 euros).

power.t.test(delta = 1, sd = 2, sig.level = 0.05, power = 0.80)    # n = respondents PER CONDITION

# The reverse question: which difference can 60 respondents per condition detect?
power.t.test(n = 60, sd = 2, sig.level = 0.05, power = 0.80)

# For a multi-item scale with seven points, sd is typically between 1 and 1.5 scale
# points. Do this calculation for your own project and bring it to Coaching I.


# ======================================================================
# 1. Import: a Qualtrics CSV has THREE header rows
# ======================================================================
# Row 1: variable names (your question labels), row 2: question texts,
# row 3: internal import IDs. The data start in row 4.

col_names <- names(read_csv(file, n_max = 0, show_col_types = FALSE))            # row 1: the names
raw <- read_csv(file, skip = 3, col_names = col_names, show_col_types = FALSE)   # from row 4: the data

# Row 2 makes a handy codebook: which question is behind which column?
codebook <- read_csv(file, n_max = 1, col_types = cols(.default = "c")) |>
  pivot_longer(everything(), names_to = "variable", values_to = "question")
print(codebook, n = 30)

glimpse(raw)

# (Alternative: the qualtRics package reads the file in one line and keeps
#  the question texts as labels: qualtRics::read_survey("your_export.csv"))


# ======================================================================
# 2. A first look: who is in the file?
# ======================================================================

nrow(raw)                                            # recorded responses
count(raw, Status, DistributionChannel)              # Status 0 = real response, 1 = survey preview
count(raw, Finished)                                 # 1 = reached the end, 0 = left before
count(raw, condition)                                # the embedded data field written by the randomizer

# Who left - and when? Unfinished responses with a condition left AFTER the randomizer
raw |> filter(Status == 0, Finished == 0) |> count(condition, Progress)


# ======================================================================
# 3. Exclusions: rules fixed BEFORE the data were collected
# ======================================================================
# (1) previews and tests, (2) unfinished responses, (3) failed attention
# check (asked BEFORE the stimulus), (4) speeders: faster than one third of
# the median completion time. Never: the manipulation check or the outcome.

d <- raw |>
  rename(duration = `Duration (in seconds)`) |>
  filter(Status == 0)                                # (1)
nrow(d)

d <- d |> filter(Finished == 1)                      # (2)
nrow(d)

median(d$duration) / 60                              # median completion time in minutes
d <- d |> filter(attn == 2)                          # (3) correct answer: 2 = "Tea"
nrow(d)

d <- d |> filter(duration >= median(duration) / 3)   # (4)
nrow(d)


# ======================================================================
# 4. Clean the variables
# ======================================================================

# Willingness to pay was a text field without validation: look at what people typed
d |> filter(!str_detect(wtp, "^[0-9]+(\\.[0-9]+)?$")) |> pull(wtp)

# Extract the number (with a point or a comma as the decimal sign) and convert it
d <- d |>
  mutate(wtp_text = wtp,
         wtp = wtp_text |> str_extract("[0-9]+([.,][0-9]+)?") |> str_replace(",", ".") |> as.numeric())

# Impossible values (Chapter 2): more than 30 euros for 250 g is not a serious answer
d |> filter(wtp > 30) |> dplyr::select(ResponseId, wtp_text, wtp)
d <- d |> mutate(wtp = if_else(wtp > 30, NA_real_, wtp))

# The analysis sample: respondents with a valid answer to the outcome question
d <- d |> filter(!is.na(wtp))
nrow(d)

# Labels for the categorical variables; Control is the reference category
d <- d |>
  mutate(condition = factor(condition, levels = c("Control", "Seal")),
         gender = factor(gender, levels = 1:4,
                         labels = c("female", "male", "non-binary / other", "prefer not to say")))


# ======================================================================
# 5. From items to scales
# ======================================================================

# Environmental concern and warm glow are REFLECTIVE multi-item scales: the three
# items of each scale express the same construct, so they must be correlated, and
# Cronbach's alpha measures the reliability of the scale. (For a FORMATIVE index -
# items that make up a construct, such as cups at home + at work + in cafes - alpha
# says nothing.)

# env_2 ("... has been greatly exaggerated") is worded in the opposite direction:
# reverse it (on a 1-7 scale: 8 - x)
d <- d |> mutate(env_2r = 8 - env_2)

# Do the items of a scale go together? Correlations and Cronbach's alpha
# (about 0.7 or more is acceptable, 0.8 or more is good)
d |> dplyr::select(env_1, env_2r, env_3) |> cor() |> round(2)
psych::alpha(dplyr::select(d, env_1, env_2r, env_3))$total$raw_alpha       # environmental concern
psych::alpha(dplyr::select(d, glow_1, glow_2, glow_3))$total$raw_alpha     # warm glow

# The complete output: alpha, the correlation of each item with the other items
# (r.drop), and the alpha of the scale if an item is dropped
psych::alpha(dplyr::select(d, glow_1, glow_2, glow_3))

# What if you forget to reverse env_2? Negative correlations, alpha collapses -
# and psych::alpha() prints a warning that names the item
# psych::alpha(dplyr::select(d, env_1, env_2, env_3))$total$raw_alpha

# The scale score is the mean of the items
d <- d |>
  mutate(concern   = rowMeans(pick(env_1, env_2r, env_3)),
         warm_glow = rowMeans(pick(glow_1, glow_2, glow_3)))


# ======================================================================
# 6. Checks before the results
# ======================================================================

# Did the randomizer work? Group sizes ...
count(d, condition)

# ... and comparable groups on everything measured BEFORE the stimulus
d |>
  group_by(condition) |>
  summarise(n = n(),
            concern = mean(concern),
            cups = mean(cups),
            age = mean(age),
            share_female = mean(gender == "female"))

# Manipulation check: did respondents notice the difference?
d |> group_by(condition) |> summarise(mean_check = mean(mc_seal), sd_check = sd(mc_seal))
t.test(mc_seal ~ condition, data = d)


# ======================================================================
# 7. The three analyses
# ======================================================================

# --- H1: does the seal increase willingness to pay? (Chapter 4) ---------
d |> group_by(condition) |> summarise(n = n(), mean_wtp = mean(wtp), sd_wtp = sd(wtp))
t.test(wtp ~ condition, data = d)                    # note: the difference is Control - Seal
psych::cohen.d(d$wtp, d$condition)$cohen.d

# The same as a regression: the coefficient of conditionSeal is the effect
model_h1 <- lm(wtp ~ condition, data = d)
round(coef(summary(model_h1)), 3)
confint(model_h1)

ggplot(d, aes(x = condition, y = wtp)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.12, height = 0, alpha = 0.4) +
  stat_summary(fun.data = mean_se, fun.args = list(mult = 1.96), geom = "pointrange", color = "#0f5da8",
               position = position_nudge(x = 0.35)) +
  labs(x = NULL, y = "Willingness to pay for 250 g (euros)",
       subtitle = "Boxplots with the individual answers; blue: mean with 95% CI")

# --- H2: for whom? Moderation by environmental concern (Chapter 6) ------
d <- d |> mutate(seal = as.numeric(condition == "Seal"))      # 0/1 version for emtrends() and mediate()
model_h2 <- lm(wtp ~ seal * concern, data = d)
round(coef(summary(model_h2)), 3)
confint(model_h2)["seal:concern", ]

# The effect of the seal at low, medium and high environmental concern
quantile(d$concern, c(0.1, 0.5, 0.9))
emtrends(model_h2, ~ concern, var = "seal", at = list(concern = c(3, 4.5, 6)))

effect_by_concern <- emtrends(model_h2, ~ concern, var = "seal",
                              at = list(concern = seq(1.5, 7, by = 0.25))) |>
  summary(infer = TRUE) |> as.data.frame()

ggplot(effect_by_concern, aes(x = concern, y = seal.trend)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick") +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), fill = "#dbe7f3", alpha = 0.7) +
  geom_line(color = "#0f5da8", linewidth = 1.2) +
  geom_rug(data = d, aes(x = concern), inherit.aes = FALSE, alpha = 0.3, sides = "b") +
  labs(x = "Environmental concern (1-7)", y = "Effect of the seal on WTP (euros, 95% CI)")

# --- H3: why? Mediation by anticipated warm glow (Chapter 6) ------------
# concern was measured before the stimulus and is a likely confounder of the
# mediator-outcome relationship: it is a control variable in both models.
# mediate() needs the treatment as a 0/1 variable: seal (created for H2)
model_m <- lm(warm_glow ~ seal + concern, data = d)                # path a
model_y <- lm(wtp ~ seal + warm_glow + concern, data = d)          # paths b and c'
round(coef(summary(model_m)), 3)
round(coef(summary(model_y)), 3)

# mediate() draws 1,000 bootstrap samples and estimates both models in each of them.
# In the output: ACME = indirect effect (a x b), ADE = direct effect (c'),
# Prop. Mediated = indirect effect as a share of the total effect
set.seed(1)
med <- mediate(model_m, model_y, treat = "seal", mediator = "warm_glow", boot = TRUE, sims = 1000)
summary(med)

# The result supports the mechanism that was MEASURED. It does not rule out
# mechanisms that were not measured (e.g., expected taste, trust in the brand).


# ======================================================================
# CODING BLOCK (25 min): work with the raw export
# ======================================================================
# Solutions: 08-your-turn-solutions.R (released after the session)

# (1) Attrition. How many respondents left the survey before the end? How
#     many of them left before, and how many after the randomizer? Did more
#     respondents drop out in one condition than in the other - and why
#     would that be a problem?

# (2) Precision. Add concern and cups as control variables to the H1
#     regression. What happens to the estimated effect of the seal and to
#     its standard error? Why are these variables allowed as controls, while
#     warm_glow is not?

# (3) What not to do. Re-run H1 after dropping the respondents who "failed"
#     the manipulation check (Seal: mc_seal of 4 or less; Control: 4 or more).
#     How does the result change? Why is this exclusion not allowed, although
#     the attention check is?

# (4) Report H1, H2 and H3 in one sentence each, in the reporting format of
#     Chapters 4 and 6 (estimate, 95% CI, test - and what was planned).

# (5) Bonus - measurement. Repeat the mediation analysis of H3 with the single
#     statement glow_1 instead of the scale warm_glow. What happens to
#     path b, to the indirect effect and to the share mediated - and why?

# your code here
