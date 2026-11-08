# ======================================================================
# Marketing Analytics — Chapter 5: Learning from Observational Data
# The regressions of Chapter 5 in R. The chapter on the course website
# explains the logic, the simulations, and the interpretation; this
# script contains the code you need to run the models on your own data.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Setup and data
# ----------------------------------------------------------------------

library(tidyverse)
library(sandwich)     # robust standard errors (section 5.9)
library(lmtest)       # coeftest()
options(scipen = 5, digits = 4)

base_url <- "https://raw.githubusercontent.com/wu-rds/MA2026/main/data/"

# All data sets of this chapter:

# 5.1–5.6, 5.8  Smart Mix as an opt-in feature: 2,000 users (simulated; true effect +3 hours)
smartmix_obs <- read_csv(paste0(base_url, "smartmix_obs.csv")) |>
  mutate(device = factor(device, levels = c("iOS", "Android", "Desktop")))

# Coding blocks 1 and 3  Podcast promotion: 400 episodes
podcast_promo <- read_csv(paste0(base_url, "podcast_promo.csv"))

# Coding block 2  Loyalty program of an online shop: 1,500 customers (opt-in)
loyalty_program <- read_csv(paste0(base_url, "loyalty_program.csv")) |>
  mutate(device = factor(device, levels = c("Desktop", "Mobile")))

# 5.7, 5.9  Promotion campaigns for new releases: 500 campaigns (simulated)
campaigns <- read_csv(paste0(base_url, "campaigns.csv")) |>
  mutate(genre = factor(genre, levels = c("Pop", "HipHop/Rap", "Rock", "Electro/Dance")))

# Coding block 4  Win-back offers: 1,200 users who uninstalled the app
app_reinstall <- read_csv(paste0(base_url, "app_reinstall.csv"))

# 5.10  Playlist listings: 290 songs x 21 weeks (real data, WU-RDS research project)
playlist <- read_csv(paste0(base_url, "playlist_did.csv")) |>
  mutate(week = as.Date(week))


# ======================================================================
# 5.1 The naive comparison
# ======================================================================

smartmix_obs |>
  group_by(smart_mix) |>
  summarise(n = n(), hours = mean(hours), prior_hours = mean(prior_hours))

# Adopters already listened more BEFORE the launch -> confounding


# ======================================================================
# 5.2 From correlation to a line
# ======================================================================

# The correlation coefficient
cor(smartmix_obs$prior_hours, smartmix_obs$hours)

# Scatter plot with the regression line
ggplot(smartmix_obs, aes(prior_hours, hours)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Listening hours before the launch", y = "Listening hours after the launch") +
  theme_minimal()

# The regression line (OLS): intercept and slope
model_line <- lm(hours ~ prior_hours, data = smartmix_obs)
coef(model_line)


# ======================================================================
# 5.3 Reading a regression
# ======================================================================

model_naive <- lm(hours ~ smart_mix, data = smartmix_obs)
summary(model_naive)                       # the full output
confint(model_naive)                       # confidence intervals of the coefficients
round(coef(summary(model_naive)), 3)       # the coefficient table, rounded (for reports)
summary(model_naive)$r.squared             # R-squared

# With one 0/1 predictor, the regression is the comparison of two means
t.test(hours ~ smart_mix, data = smartmix_obs, var.equal = TRUE)


# ======================================================================
# 5.4 Adjusting for confounders
# ======================================================================

# Holding prior listening constant
model_prior <- lm(hours ~ smart_mix + prior_hours, data = smartmix_obs)
summary(model_prior)

# More variables, same logic
model_full <- lm(hours ~ smart_mix + prior_hours + tenure + premium + device, data = smartmix_obs)
summary(model_full)
confint(model_full)["smart_mix", ]

# Compare: naive 9.3 -> adjusted 4.5 -> experiment (Chapter 4) 3.2 -> truth 3.0
# The assumption behind adjustment: exogeneity, Cov(smart_mix, error) = 0 —
# it cannot be tested in the data; it has to be argued from theory and from
# how the data came to be (who adopted Smart Mix, and why)


# ======================================================================
# 5.5 Categorical predictors
# ======================================================================

# R creates one dummy per category, relative to the reference (iOS)
coef(model_full)[c("premium", "deviceAndroid", "deviceDesktop")]

# Does device matter at all? Compare the model with and without it
model_nodevice <- lm(hours ~ smart_mix + prior_hours + tenure + premium, data = smartmix_obs)
anova(model_nodevice, model_full)

# Changing the reference category
smartmix_obs |>
  mutate(device = relevel(device, ref = "Android")) |>
  lm(hours ~ smart_mix + prior_hours + tenure + premium + device, data = _) |>
  coef()


# ======================================================================
# 5.6 Prediction vs. explanation
# ======================================================================

# Train/test split: fit on 1,500 users, evaluate on the other 500
set.seed(42)
train_ids <- sample(nrow(smartmix_obs), 1500)
train <- smartmix_obs[train_ids, ]
test  <- smartmix_obs[-train_ids, ]

model_pred <- lm(hours ~ prior_hours + tenure + premium + device + smart_mix, data = train)
test$predicted <- predict(model_pred, newdata = test)

# In-sample vs. out-of-sample fit
summary(model_pred)$r.squared                  # in-sample R-squared (training data; flatters the model)
cor(test$predicted, test$hours)^2              # out-of-sample R-squared (test data; the honest number)

# The typical prediction error: RMSE = sqrt(mean of squared prediction errors), in hours
sqrt(mean((test$hours - test$predicted)^2))


# ======================================================================
# CODING BLOCK (session 5): two cases, 40 min
# ======================================================================
# Solutions: 05-your-turn-solutions.R (released after the session)

# --- Case 1: podcast promotion -----------------------------------------
# 400 episodes: promotion spend (euros), host followers, downloads in the
# release week. Solutions: 05-your-turn-solutions.R

head(podcast_promo)                        # loaded at the top

# Tasks:
# (1) Scatter plot of downloads (y) against promo_spend (x); compute the
#     correlation; describe the relationship in one sentence
# (2) Regression of downloads on promo_spend: summary(); interpret the slope
#     in words; read off R-squared
# (3) Is the slope a causal effect of promotion? What would you need to argue?

# your code here

# --- Case 2: a loyalty program -----------------------------------------
# 1,500 customers of an online shop; membership in the loyalty program is
# opt-in. Solutions: 05-your-turn-solutions.R

head(loyalty_program)                      # loaded at the top

# Tasks:
# (1) Naive comparison: regress spending on member; interpret the coefficient
#     and say why it is probably not the causal effect of the program
# (2) Adjustment: add spend_prev, tenure and device; how does the member
#     coefficient change, and what does it mean now? Report it with its 95% CI
# (3) Interpret the coefficient of device
# (4) Is membership exogenous after adjustment? Argue from the data-generating
#     process: who joins a loyalty program, and is everything that drives this
#     in the model?

# your code here


# ======================================================================
# 5.7 Functional form: when the straight line is wrong
# ======================================================================

# Promotion campaigns: streams vs. spend on the original scale ...
ggplot(campaigns, aes(spend, streams)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# ... and with both axes logged
ggplot(campaigns, aes(log(spend), log(streams))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# Linear model (level-level)
model_linear <- lm(streams ~ spend + followers + placements, data = campaigns)
summary(model_linear)

# Log-log model: coefficients of logged predictors are elasticities;
# coefficients of unlogged predictors are approximate percentage changes
model_loglog <- lm(log(streams) ~ log(spend) + log(followers) + placements + genre, data = campaigns)
summary(model_loglog)

# Exact percentage change for a log-level coefficient
100 * (exp(coef(model_loglog)["placements"]) - 1)

# What does doubling the spend do to streams, according to the model?
100 * (2^coef(model_loglog)["log(spend)"] - 1)


# ======================================================================
# 5.8 Yes/no outcomes: logistic regression
# ======================================================================

# Upgrades to Premium among free users: a full S-curve in listening hours
free_users <- filter(smartmix_obs, premium == 0)
mean(free_users$upgraded)                  # share of upgrades

# Look at the data: share of upgrades per 5-hour bin
free_users |>
  mutate(bin = cut(hours, breaks = seq(0, 70, 5))) |>
  group_by(bin) |>
  summarise(n = n(), share_upgraded = mean(upgraded)) |>
  filter(n >= 8)

# Logistic regression: glm() with family = binomial
model_upgrade <- glm(upgraded ~ hours, family = binomial, data = free_users)
summary(model_upgrade)

# Odds ratios: exp(coefficient); the odds of an upgrade per additional hour
exp(coef(model_upgrade))
exp(confint(model_upgrade))

# Predicted probabilities at chosen values of the predictor ...
predict(model_upgrade, newdata = data.frame(hours = c(10, 20, 30, 40)), type = "response")

# ... and one hour later: the same odds ratio, a different change in probability
predict(model_upgrade, newdata = data.frame(hours = c(11, 21, 31, 41)), type = "response")

# Several predictors: churn (cancelled within three months)
model_churn <- glm(churn ~ hours + premium + tenure + smart_mix, family = binomial, data = smartmix_obs)
summary(model_churn)

# Average effect of Premium status in percentage points
p_free    <- predict(model_churn, newdata = mutate(smartmix_obs, premium = 0), type = "response")
p_premium <- predict(model_churn, newdata = mutate(smartmix_obs, premium = 1), type = "response")
mean(p_premium - p_free)

# McFadden's pseudo R-squared
1 - model_upgrade$deviance / model_upgrade$null.deviance


# ======================================================================
# 5.9 The diagnostics that change decisions
# ======================================================================

# 1. Shape: residuals vs. fitted values (a curve means the wrong form)
plot(model_linear, which = 1)
plot(model_loglog, which = 1)

# 2. Influence: residuals vs. leverage, with Cook's distance
plot(model_loglog, which = 5)

# 3. Uncertainty: robust standard errors when the residuals form a funnel
coeftest(model_linear, vcov = vcovHC(model_linear, type = "HC1"))


# ======================================================================
# 5.10 A quasi-experiment: difference-in-differences
# ======================================================================

# Mean log streams by group and week
playlist |>
  group_by(treated, week) |>
  summarise(mean_log = mean(log(streams + 1)), .groups = "drop") |>
  ggplot(aes(week, mean_log, color = factor(treated))) +
  geom_line() + geom_point() +
  geom_vline(xintercept = as.Date("2018-03-19"), linetype = "dashed") +
  labs(color = "treated", y = "Mean log(streams + 1)") +
  theme_minimal()

# Drop the listing week; log the outcome
did_data <- playlist |>
  filter(week != as.Date("2018-03-19")) |>
  mutate(log_streams = log(streams + 1))

# The four means
did_data |>
  group_by(treated, post) |>
  summarise(mean_log_streams = mean(log_streams), .groups = "drop")

# The DiD regression: treated + post + treated:post
model_did <- lm(log_streams ~ treated * post, data = did_data)
summary(model_did)
100 * (exp(coef(model_did)["treated:post"]) - 1)   # effect in percent

# Placebo test: a fake listing in the pre-period should show no effect
placebo_data <- did_data |>
  filter(post == 0) |>
  mutate(fake_post = ifelse(week >= as.Date("2018-02-12"), 1, 0))
round(coef(summary(lm(log_streams ~ treated * fake_post, data = placebo_data)))["treated:fake_post", ], 3)

# Your turn: add a control variable
summary(lm(log_streams ~ treated * post + major_label, data = did_data))


# ======================================================================
# CODING BLOCK (session 6): two cases, 40 min
# ======================================================================
# Solutions: 05-your-turn-solutions.R (released after the session)

# --- Case 3: podcast promotion, revisited ------------------------------
# Back to podcast_promo. Solutions: 05-your-turn-solutions.R

# Tasks:
# (1) Scatter plot of downloads against promo_spend with both axes logged:
#     what has changed?
# (2) Log-log model of downloads on promo_spend and host_followers; interpret
#     both coefficients as elasticities: what does +10% spend do to downloads?
# (3) Compare R-squared with the linear model from block 1; which model would
#     you use to advise the network on its promotion budget?

# your code here

# --- Case 4: winning back users ----------------------------------------
# 1,200 users who uninstalled the app received a win-back offer with a
# randomly assigned discount (0-60%). Solutions: 05-your-turn-solutions.R

head(app_reinstall)                        # loaded at the top

# Tasks:
# (1) Share of reinstalls per discount level, plotted against the discount:
#     describe the shape
# (2) Logistic regression of reinstalled on discount and prior_use: interpret
#     the sign and the odds ratio of the discount
# (3) Predict the reinstall probability for a user with 10 prior sessions at a
#     discount of 10, 11, 40 and 41 percent; by how many percentage points does
#     one more point of discount change the probability in each case, and why
#     do the two answers differ?

# your code here

