# ======================================================================
# Marketing Analytics — Chapter 6: Mechanisms & Heterogeneity
# Companion code to Chapter 6 of the course book. The chapter explains
# every step in detail; this script collects the code in one place.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Setup and data
# ----------------------------------------------------------------------

library(tidyverse)
library(emmeans)      # install.packages("emmeans") if needed
options(scipen = 5, digits = 4)

base_url <- "https://raw.githubusercontent.com/wu-rds/MA2026/main/data/"

# All data sets of this chapter:

# 6.1, 6.3, 6.7  Smart Mix, second wave: 1,200 users, randomized; covariates
#                and a measured mediator (simulated)
wave2 <- read_csv(paste0(base_url, "smartmix_wave2.csv")) |>
  mutate(device = factor(device, levels = c("iOS", "Android", "Desktop")))

# 6.3, 6.4  Promotion campaigns, second year: 600 campaigns (simulated)
promo <- read_csv(paste0(base_url, "promo_campaigns.csv")) |>
  mutate(genre = factor(genre, levels = c("Pop", "HipHop/Rap", "Rock", "Electro/Dance")))

# 6.5  A 2 x 3 survey experiment: 360 respondents rate the creativity of an ad
ads <- read_csv(paste0(base_url, "ad_designs.csv")) |>
  mutate(source = factor(source, levels = c("Human", "AI", "Human + AI")),
         disclosure = factor(disclosure, levels = c("Revealed", "Not revealed")))

# Coding block, case 1  Climate-neutral shipping label: 1,000 customers (randomized)
green_label <- read_csv(paste0(base_url, "green_label.csv")) |>
  mutate(segment = factor(segment, levels = c("New", "Returning")))

# Coding block, case 2  The story behind the coffee: 320 respondents (survey experiment)
coffee_story <- read_csv(paste0(base_url, "coffee_story.csv")) |>
  mutate(version = factor(version, levels = c("Facts", "Story")))


# ======================================================================
# 6.1 The second Smart Mix experiment
# ======================================================================

glimpse(wave2)

# The average effect, with covariates as controls (precision, not bias)
model_avg <- lm(hours ~ smart_mix + prior_hours + premium + tenure, data = wave2)
summary(model_avg)
confint(model_avg)["smart_mix", ]


# ======================================================================
# 6.2 Interactions and marginal effects
# ======================================================================

# In a formula, x * z means x + z + x:z. Never use x:z without the main terms.
# Marginal effect of x in y ~ x * z:  beta_x + beta_xz * z
# Coefficient tables in this chapter: round(coef(summary(model)), 3)
# (estimate, standard error, t value, p-value; p-values below 0.0005 show as 0.000).


# ======================================================================
# 6.3 Continuous x categorical
# ======================================================================

# --- Face 1: a treatment effect that differs between groups ---------------

# Look at the data first: the four means
wave2 |>
  group_by(premium, smart_mix) |>
  summarise(mean_hours = mean(hours), .groups = "drop")

model_plan <- lm(hours ~ smart_mix * premium + prior_hours + tenure, data = wave2)
round(coef(summary(model_plan)), 3)

# Marginal effect of Smart Mix = b_smart_mix + b_smart_mix:premium * premium
b <- coef(model_plan)
b["smart_mix"] + b["smart_mix:premium"] * c(free = 0, premium = 1)

# The same, with standard errors and confidence intervals (emmeans package)
emtrends(model_plan, ~ premium, var = "smart_mix")

# --- Face 2: a slope that differs between groups --------------------------
model_genre <- lm(log(streams) ~ log(spend) * genre + log(followers) + placements, data = promo)
round(coef(summary(model_genre)), 3)

# Spend elasticity by genre = b_log(spend) + b_log(spend):genre (0 for Pop, the reference)
b <- coef(model_genre)
elasticity <- b["log(spend)"] +
  c(0, b["log(spend):genreHipHop/Rap"], b["log(spend):genreRock"], b["log(spend):genreElectro/Dance"])
names(elasticity) <- levels(promo$genre)
elasticity

# With confidence intervals
emtrends(model_genre, ~ genre, var = "log(spend)")

# --- Moderation by a continuous variable ----------------------------------
model_prior <- lm(hours ~ smart_mix * prior_hours + premium + tenure, data = wave2)
round(coef(summary(model_prior)), 3)

# Marginal effect of Smart Mix = b_smart_mix + b_smart_mix:prior_hours * prior_hours
b <- coef(model_prior)
b["smart_mix"] + b["smart_mix:prior_hours"] * c(10, 20, 30)

# With confidence intervals, at chosen values of prior listening
emtrends(model_prior, ~ prior_hours, var = "smart_mix", at = list(prior_hours = c(10, 20, 30)))

# Where are the users? Report the effect at the 10th / 50th / 90th percentile
quantile(wave2$prior_hours, c(0.1, 0.5, 0.9))

# Your turn: does the effect differ by device?
model_device <- lm(hours ~ smart_mix * device + prior_hours + premium + tenure, data = wave2)
round(coef(summary(model_device)), 3)
emtrends(model_device, ~ device, var = "smart_mix")


# ======================================================================
# 6.4 Continuous x continuous
# ======================================================================

# Uncentered: the "main effects" are marginal effects at log(x) = 0, i.e., at
# one follower / one euro (log(1) = 0) — values nobody in the data is near
model_cc <- lm(log(streams) ~ log(spend) * log(followers) + placements + genre, data = promo)
round(coef(summary(model_cc)), 3)

# Centered at the means: main effects are now marginal effects at the mean
promo_c <- promo |>
  mutate(log_spend_c = log(spend) - mean(log(spend)),
         log_fol_c   = log(followers) - mean(log(followers)))
model_cc_centered <- lm(log(streams) ~ log_spend_c * log_fol_c + placements + genre, data = promo_c)
round(coef(summary(model_cc_centered)), 3)

# Interaction, R-squared and predictions are identical in both versions
c(uncentered = summary(model_cc)$r.squared, centered = summary(model_cc_centered)$r.squared)

# The spend elasticity = b_log_spend_c + b_log_spend_c:log_fol_c * (log(followers) - mean)
# at the 10th, 50th and 90th percentile of followers
b <- coef(model_cc_centered)
fol_q <- quantile(promo$followers, c(0.1, 0.5, 0.9))
fol_q
b["log_spend_c"] + b["log_spend_c:log_fol_c"] * (log(fol_q) - mean(log(promo$followers)))

# The same with confidence intervals (from the uncentered model; same numbers)
emtrends(model_cc, ~ followers, var = "log(spend)", at = list(followers = unname(fol_q)))


# ======================================================================
# 6.5 Categorical x categorical: a 2 x 3 experiment
# ======================================================================

# Cell means
ads |>
  group_by(source, disclosure) |>
  summarise(n = n(), mean_creativity = mean(creativity), .groups = "drop")

ggplot(ads, aes(source, creativity, color = disclosure, group = disclosure)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", size = 3) +
  theme_minimal()

# The regression: reference cell = Human, Revealed
model_ads <- lm(creativity ~ source * disclosure, data = ads)
round(coef(summary(model_ads)), 3)

# Rebuilding a cell mean from the coefficients: AI, not revealed
b <- coef(model_ads)
b["(Intercept)"] + b["sourceAI"] + b["disclosureNot revealed"] + b["sourceAI:disclosureNot revealed"]

# Step 1: is there an interaction at all?
anova(model_ads)
# car::Anova(model_ads, type = 2)   # order-independent tests for unbalanced designs

# Step 2: which cells differ? Estimated marginal means and adjusted pairwise comparisons
emm <- emmeans(model_ads, ~ source | disclosure)
emm
pairs(emm)                             # Tukey-adjusted (default)
pairs(emm, adjust = "bonferroni")      # Bonferroni, as in Chapter 4

# The other direction: disclosure effect within each source
pairs(emmeans(model_ads, ~ disclosure | source))


# ======================================================================
# 6.6 Pitfalls: subgroup fishing and power (reading)
# ======================================================================

# Probability of at least one "significant" subgroup with k subgroups and no heterogeneity
k <- c(1, 5, 10, 20)
1 - 0.95^k

# An interaction as large as a main effect needs about four times the sample:
# SE(difference of two effects) = 2 x SE(overall effect)


# ======================================================================
# 6.7 Mediation
# ======================================================================

# Path a: does Smart Mix affect the mediator?
model_m <- lm(new_artists ~ smart_mix + prior_hours + premium, data = wave2)
round(coef(summary(model_m)), 3)

# Paths b and c': does the mediator affect the outcome, and what is left of the direct effect?
model_y <- lm(hours ~ smart_mix + new_artists + prior_hours + premium + tenure, data = wave2)
round(coef(summary(model_y)), 3)

a        <- coef(model_m)["smart_mix"]
b        <- coef(model_y)["new_artists"]
direct   <- coef(model_y)["smart_mix"]
indirect <- a * b
c(indirect = unname(indirect), direct = unname(direct), total = unname(indirect + direct),
  share_mediated = unname(indirect / (indirect + direct)))

# Bootstrap confidence interval for the indirect effect
set.seed(1)
boot_indirect <- replicate(2000, {
  d_boot <- wave2[sample(nrow(wave2), replace = TRUE), ]
  coef(lm(new_artists ~ smart_mix + prior_hours + premium, data = d_boot))["smart_mix"] *
    coef(lm(hours ~ smart_mix + new_artists + prior_hours + premium + tenure, data = d_boot))["new_artists"]
})
quantile(boot_indirect, c(0.025, 0.975))
hist(boot_indirect, breaks = 40, main = "Bootstrap distribution of the indirect effect")

# The same in one call with the mediation package (install.packages("mediation"))
# ACME = indirect effect, ADE = direct effect, Prop. Mediated = share mediated
# library(mediation)
# set.seed(1)
# med <- mediate(model_m, model_y, treat = "smart_mix", mediator = "new_artists", boot = TRUE, sims = 1000)
# summary(med)


# ======================================================================
# CODING BLOCK: two cases, 40 min
# ======================================================================
# Solutions: 06-your-turn-solutions.R (released after the session)

# --- Case 1: the climate-neutral shipping label -------------------------
# 1,000 customers of an online shop were randomly shown a "climate-neutral
# shipping" label at checkout (label = 1) or not (label = 0). Outcome: order
# value in euros. Moderators: segment (new / returning customer) and
# eco_score (importance of sustainability, 1-7, from an earlier survey).

head(green_label)                      # loaded at the top

# Tasks:
# (1) Mean order value by label x segment, as a table and as a plot (lines or
#     points per segment): are the lines parallel?
# (2) Fit order_value ~ label * segment. Compute the label effect for each
#     segment from the coefficients and with emtrends(). Do the effects differ
#     (coefficient, p-value)? How would you report this?
# (3) Fit order_value ~ label * eco_score. Compute the label effect for a
#     score of 2, 4 and 6. What does the coefficient of label alone mean here,
#     and why should you not report it?

# your code here

# --- Case 2: the story behind the coffee ---------------------------------
# A survey experiment of the kind you run in your group project: 320
# respondents saw a factual ("Facts") or a storytelling ("Story") product
# description of a specialty coffee (randomized), then rated the brand's
# authenticity (1-7, the mediator, measured AFTER the manipulation) and stated
# their willingness to pay for a 250 g pack (wtp, euros). Coffee involvement
# (1-7) was measured BEFORE the manipulation.

head(coffee_story)                     # loaded at the top

# Tasks:
# (1) The total effect of the story on willingness to pay, with its 95% CI
#     (a t-test or a regression of wtp on version; add a chart).
# (2) The three paths: a (version -> authenticity), b (authenticity -> wtp,
#     holding version constant) and c' (the direct effect), with involvement
#     as a control variable in both regressions. Compute the indirect effect
#     and the share of the total effect that is mediated.
# (3) A bootstrap confidence interval for the indirect effect (or mediate()
#     from the mediation package).
# (4) Which assumption does the split into direct and indirect effect rely
#     on? Which variables would you add to the questionnaire of this
#     experiment, and when would you measure them?

# your code here
