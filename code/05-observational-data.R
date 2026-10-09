# ======================================================================
# Marketing Analytics — Chapter 5: Learning from Observational Data
# Companion code to Chapter 5 of the course book. The chapter explains
# every step in detail; this script collects the code in one place.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------------------

library(tidyverse)
options(scipen = 5, digits = 4)

base_url <- "https://raw.githubusercontent.com/wu-rds/MA2026/main/data/"


# ======================================================================
# 5.1 Smart Mix without randomization
# ======================================================================

# 2,000 users of the streaming service; Smart Mix was launched as an
# opt-in feature (simulated data; true effect: +3 hours)
smartmix_obs <- read_csv(paste0(base_url, "smartmix_obs.csv")) |>
  mutate(device = factor(device, levels = c("iOS", "Android", "Desktop")))

glimpse(smartmix_obs)

# The naive comparison: adopters vs. non-adopters
smartmix_obs |>
  group_by(smart_mix) |>
  summarise(n = n(), hours = mean(hours), prior_hours = mean(prior_hours))

# Adopters already listened more BEFORE the launch -> confounding


# ======================================================================
# 5.2 From correlation to a line
# ======================================================================

cor(smartmix_obs$prior_hours, smartmix_obs$hours)

ggplot(smartmix_obs, aes(prior_hours, hours)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Listening hours before the launch", y = "Listening hours after the launch") +
  theme_minimal()

# The regression line: intercept and slope
coef(lm(hours ~ prior_hours, data = smartmix_obs))


# ======================================================================
# 5.3 Reading a regression
# ======================================================================

model_naive <- lm(hours ~ smart_mix, data = smartmix_obs)
summary(model_naive)
confint(model_naive)

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

# Your turn: does age change anything? (Is age a confounder here?)
model_age <- lm(hours ~ smart_mix + prior_hours + tenure + premium + device + age, data = smartmix_obs)
coef(summary(model_age))["smart_mix", ]

# Multicollinearity check: variance inflation factors (values > 5 deserve attention)
car::vif(model_full)


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

sqrt(mean((test$hours - test$predicted)^2))   # typical prediction error (RMSE)

# In-sample comparison of model size: adjusted R-squared and AIC
summary(model_pred)$adj.r.squared
AIC(model_pred)


# ======================================================================
# 5.7 Functional form: when the straight line is wrong
# ======================================================================

# 500 promotion campaigns (simulated): spend, playlist placements,
# artist followers, and streams in the first four weeks
campaigns <- read_csv(paste0(base_url, "campaigns.csv")) |>
  mutate(genre = factor(genre, levels = c("Pop", "HipHop/Rap", "Rock", "Electro/Dance")))

ggplot(campaigns, aes(spend, streams)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

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

# Your turn: what does doubling the spend do to streams, according to the model?
100 * (2^coef(model_loglog)["log(spend)"] - 1)


# ======================================================================
# 5.8 Yes/no outcomes: logistic regression
# ======================================================================

mean(smartmix_obs$churn)   # share of users who cancelled within three months

model_churn <- glm(churn ~ hours + premium + tenure + smart_mix, family = binomial, data = smartmix_obs)
summary(model_churn)

# Predicted probabilities for chosen predictor values
typical_user <- data.frame(hours = c(10, 20, 30), premium = 0, tenure = 24, smart_mix = 0)
predict(model_churn, newdata = typical_user, type = "response")

# Average effect of Premium status in percentage points
p_free    <- predict(model_churn, newdata = mutate(smartmix_obs, premium = 0), type = "response")
p_premium <- predict(model_churn, newdata = mutate(smartmix_obs, premium = 1), type = "response")
mean(p_premium - p_free)

# Odds ratios (see the "Under the hood" box in the chapter)
exp(coef(model_churn))


# ======================================================================
# 5.9 The diagnostics that change decisions
# ======================================================================

# 1. Shape: residuals vs. fitted values (a curve means the wrong form)
plot(model_linear, which = 1)
plot(model_loglog, which = 1)

# 2. Influence: residuals vs. leverage, with Cook's distance
plot(model_loglog, which = 5)
head(sort(cooks.distance(model_loglog), decreasing = TRUE))

# 3. Uncertainty: robust standard errors when the residuals form a funnel
library(sandwich)
library(lmtest)
coeftest(model_linear, vcov = vcovHC(model_linear, type = "HC1"))
bptest(model_linear)   # Breusch-Pagan test for heteroskedasticity


# ======================================================================
# 5.10 A quasi-experiment: difference-in-differences
# ======================================================================

# Real data: weekly streams of 290 songs; 53 were added to a major
# playlist on March 19, 2018 (WU-RDS research project)
playlist <- read_csv(paste0(base_url, "playlist_did.csv")) |>
  mutate(week = as.Date(week))

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
coef(summary(lm(log_streams ~ treated * fake_post, data = placebo_data)))["treated:fake_post", ]

# Your turn: add a control variable
coef(summary(lm(log_streams ~ treated * post + major_label, data = did_data)))
