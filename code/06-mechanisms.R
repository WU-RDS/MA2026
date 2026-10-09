# ======================================================================
# Marketing Analytics — Chapter 6: Mechanisms & Heterogeneity
# Companion code to Chapter 6 of the course book. The chapter explains
# every step in detail; this script collects the code in one place.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------------------

library(tidyverse)
library(emmeans)      # install.packages("emmeans") if needed
options(scipen = 5, digits = 4)

base_url <- "https://raw.githubusercontent.com/wu-rds/MA2026/main/data/"


# ======================================================================
# 6.1 The second Smart Mix experiment
# ======================================================================

# 1,200 users, randomized; covariates and a measured mediator (simulated)
wave2 <- read_csv(paste0(base_url, "smartmix_wave2.csv")) |>
  mutate(device = factor(device, levels = c("iOS", "Android", "Desktop")))

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


# ======================================================================
# 6.3 Continuous x categorical
# ======================================================================

# --- Face 1: a treatment effect that differs between groups ---------------
wave2 |>
  group_by(premium, smart_mix) |>
  summarise(mean_hours = mean(hours), .groups = "drop")

model_plan <- lm(hours ~ smart_mix * premium + prior_hours + tenure, data = wave2)
summary(model_plan)

# Effect of Smart Mix for free users (premium = 0) and Premium users (premium = 1)
b <- coef(model_plan)
c(free = unname(b["smart_mix"]), premium = unname(b["smart_mix"] + b["smart_mix:premium"]))

# The same, with standard errors and confidence intervals, via emmeans
emtrends(model_plan, ~ premium, var = "smart_mix")

# --- Face 2: a slope that differs between groups --------------------------
promo <- read_csv(paste0(base_url, "promo_campaigns.csv")) |>
  mutate(genre = factor(genre, levels = c("Pop", "HipHop/Rap", "Rock", "Electro/Dance")))

model_genre <- lm(log(streams) ~ log(spend) * genre + log(followers) + placements, data = promo)
summary(model_genre)

# Spend elasticity by genre
emtrends(model_genre, ~ genre, var = "log(spend)")

# --- Moderation by a continuous variable ----------------------------------
model_prior <- lm(hours ~ smart_mix * prior_hours + premium + tenure, data = wave2)
summary(model_prior)

# Marginal effect of Smart Mix at chosen values of prior listening
b <- coef(model_prior)
prior_values <- c(10, 20, 30)
b["smart_mix"] + b["smart_mix:prior_hours"] * prior_values

# With confidence intervals
emtrends(model_prior, ~ prior_hours, var = "smart_mix", at = list(prior_hours = prior_values))

# Your turn: does the effect differ by device?
model_device <- lm(hours ~ smart_mix * device + prior_hours + premium + tenure, data = wave2)
coef(summary(model_device))
emtrends(model_device, ~ device, var = "smart_mix")


# ======================================================================
# 6.4 Continuous x continuous
# ======================================================================

# Uncentered: the "main effects" are marginal effects at log(x) = 0 — meaningless here
model_cc <- lm(log(streams) ~ log(spend) * log(followers) + placements + genre, data = promo)
summary(model_cc)

# Centered at the means: main effects are now marginal effects at the mean
promo_c <- promo |>
  mutate(log_spend_c = log(spend) - mean(log(spend)),
         log_fol_c   = log(followers) - mean(log(followers)))
model_cc_centered <- lm(log(streams) ~ log_spend_c * log_fol_c + placements + genre, data = promo_c)
summary(model_cc_centered)

# The spend elasticity at the 10th, 50th and 90th percentile of followers
fol_q <- log(quantile(promo$followers, c(0.1, 0.5, 0.9))) - mean(log(promo$followers))
b <- coef(model_cc_centered)
b["log_spend_c"] + b["log_spend_c:log_fol_c"] * fol_q


# ======================================================================
# 6.5 Categorical x categorical: a 2 x 3 experiment
# ======================================================================

ads <- read_csv(paste0(base_url, "ad_designs.csv")) |>
  mutate(source = factor(source, levels = c("Human", "AI", "Human + AI")),
         disclosure = factor(disclosure, levels = c("Revealed", "Not revealed")))

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
summary(model_ads)

# Rebuilding a cell mean from the coefficients: AI, not revealed
b <- coef(model_ads)
b["(Intercept)"] + b["sourceAI"] + b["disclosureNot revealed"] + b["sourceAI:disclosureNot revealed"]

# Step 1: is there an interaction at all?
anova(model_ads)
# car::Anova(model_ads, type = 2)   # order-independent tests for unbalanced designs

# Step 2: which cells differ? Estimated marginal means and Tukey-adjusted pairs
emm <- emmeans(model_ads, ~ source | disclosure)
emm
pairs(emm)

# The other direction: disclosure effect within each source
pairs(emmeans(model_ads, ~ disclosure | source))


# ======================================================================
# 6.6 Pitfalls: subgroup fishing and power
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
summary(model_m)

# Paths b and c': does the mediator affect the outcome, and what is left of the direct effect?
model_y <- lm(hours ~ smart_mix + new_artists + prior_hours + premium + tenure, data = wave2)
summary(model_y)

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

# The same with the mediation package (optional; install.packages("mediation"))
# library(mediation)
# med <- mediate(model_m, model_y, treat = "smart_mix", mediator = "new_artists", boot = TRUE, sims = 1000)
# summary(med)


# ======================================================================
# 6.8 Good and bad controls (no code — see the chapter)
# ======================================================================
