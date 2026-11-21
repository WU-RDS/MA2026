# ------------------------------------------------------------------
# ch08-make-data.R
# Simulates the raw Qualtrics export used in the survey-design chapter
# (Module 8): data/coffee_seal_qualtrics.csv
#
# The file imitates what Qualtrics delivers for the example study
# "A seal for the coffee" (the same product page without or with a
# sustainability seal) at the scale of a group project:
#   * three header rows (variable name, question text, ImportId)
#   * metadata columns (StartDate, Status, Progress, Finished, ...)
#   * one column per question, named with the question labels
#   * the embedded-data field "condition" written by the randomizer
#   * preview responses, unfinished responses, failed attention
#     checks, speeders and a few messy text entries
# All data are synthetic. Run from the project root:
#   Rscript R/ch08-make-data.R
# ------------------------------------------------------------------

# the file contains a euro sign: make sure R works in UTF-8 (the default on current systems)
if (!isTRUE(l10n_info()[["UTF-8"]])) try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE)

make_raw <- function(seed = 46) {
  set.seed(seed)

  n <- 141                                   # people who opened the anonymous link

  # --- helper: turn a latent score into a 1-7 rating -----------------------
  rate7 <- function(x) pmin(7, pmax(1, round(x)))

  # --- who they are ---------------------------------------------------------
  env_lat <- rnorm(n)                                        # environmental concern (standardized)
  # not asked in the survey: "green" purchases in a typical week (related to environmental concern)
  green   <- pmax(0, round(1.9 + 0.9 * env_lat + rnorm(n, 0, 1.0)))
  age     <- ifelse(runif(n) < 0.62, round(runif(n, 19, 31)), round(runif(n, 32, 68)))
  gender  <- sample(1:4, n, replace = TRUE, prob = c(0.52, 0.44, 0.02, 0.02))

  # moderator: three items of the revised NEP scale (Dunlap et al. 2000)
  # (env_2, "... has been greatly exaggerated", is worded in the opposite direction: reverse-scored)
  env_2 <- 8 - rate7(4.7 + 1.15 * env_lat + rnorm(n, 0, 0.95))
  env_1 <- rate7(4.5 + 1.10 * env_lat + rnorm(n, 0, 0.95))
  env_3 <- rate7(4.3 + 1.20 * env_lat + rnorm(n, 0, 0.95))

  # attention check (correct answer: 2 = "Tea")
  attn <- rep(2, n)

  # --- randomizer: "randomly present 1 element", "evenly present elements" --
  # (in the order of arrival, each pair of respondents gets both conditions in random order)
  seal <- as.vector(replicate(ceiling(n / 2), sample(0:1)))[1:n]

  # --- after the stimulus ---------------------------------------------------
  # mediator: anticipated warm glow (items adapted from Hartmann & Apaolaza-Ibanez 2012)
  glow_lat <- 0.90 * seal + 0.15 * env_lat + 0.40 * seal * env_lat + rnorm(n, 0, 0.90)
  glow_1 <- rate7(3.9 + glow_lat + rnorm(n, 0, 0.80))
  glow_2 <- rate7(3.6 + glow_lat + rnorm(n, 0, 0.85))
  glow_3 <- rate7(3.8 + glow_lat + rnorm(n, 0, 0.80))

  # dependent variable: willingness to pay for 250 g (euros)
  e_wtp    <- rnorm(n, 0, 1.55)                              # everything else that moves willingness to pay
  wtp_true <- 7.4 + 0.30 * seal + 0.85 * glow_lat + 0.45 * env_lat + 0.30 * seal * env_lat +
    0.10 * green + e_wtp
  wtp_num  <- pmax(2.5, round(wtp_true * 2) / 2)
  price_point <- runif(n) < 0.12 & wtp_num == round(wtp_num)       # some people think in price points
  wtp_num[price_point] <- wtp_num[price_point] - sample(c(0.01, 0.10), sum(price_point), replace = TRUE)

  # manipulation check: "The coffee on the product page carried a seal for sustainable farming" (1-7)
  # (respondents with high environmental concern are more likely to notice the seal)
  mc_seal <- rate7(2.6 + 3.1 * seal + 0.5 * seal * env_lat + rnorm(n, 0, 1.25))

  # --- response behavior ----------------------------------------------------
  duration <- round(exp(rnorm(n, log(330), 0.38)))                 # seconds, median about 5.5 minutes
  status   <- rep("ok", n)

  # unfinished responses: 4 leave before the randomizer, 8 after the stimulus (5 Seal, 3 Control)
  drop_pre  <- sample(n, 4)
  drop_post <- c(sample(setdiff(which(seal == 1), drop_pre), 5),
                 sample(setdiff(which(seal == 0), drop_pre), 3))
  status[drop_pre]  <- "drop_pre"
  status[drop_post] <- "drop_post"

  # failed attention checks (2 per condition) and speeders (2 per condition)
  ok <- which(status == "ok")
  fail_attn <- c(sample(intersect(ok, which(seal == 1)), 2), sample(intersect(ok, which(seal == 0)), 2))
  attn[fail_attn] <- sample(c(1, 3, 4), 4, replace = TRUE)
  speeders <- c(sample(setdiff(intersect(ok, which(seal == 1)), fail_attn), 2),
                sample(setdiff(intersect(ok, which(seal == 0)), fail_attn), 2))
  duration[speeders] <- round(runif(4, 48, 85))
  for (v in c("env_1", "env_2", "env_3", "glow_1", "glow_2", "glow_3", "mc_seal")) {
    x <- get(v); x[speeders] <- 4; assign(v, x)                   # straight-lining
  }
  wtp_num[speeders] <- c(5, 10, 5, 10)

  # --- willingness to pay as typed (no validation was set for this question) -
  wtp <- formatC(wtp_num, format = "f", digits = 2)
  wtp <- sub("\\.00$", "", wtp); wtp <- sub("(\\.[0-9])0$", "\\1", wtp)
  valid <- setdiff(ok, c(fail_attn, speeders))
  messy <- sample(valid, 9)
  wtp[messy[1:4]] <- sub("\\.", ",", formatC(wtp_num[messy[1:4]], format = "f", digits = 2))   # 7,50
  wtp[messy[5]]   <- paste0(wtp[messy[5]], " €")                                          # 9 €
  wtp[messy[6]]   <- paste0("€", wtp[messy[6]])                                           # €8.5
  wtp[messy[7]]   <- paste0(wtp[messy[7]], " Euro")
  wtp[messy[8]]   <- paste0("ca. ", wtp[messy[8]])
  wtp[messy[9]]   <- "85"                                                                      # a typo for 8.5?

  # --- dates: fielded December 1-12, 2026 -----------------------------------
  start <- sort(as.POSIXct("2026-12-01 09:00:00", tz = "UTC") + runif(n, 0, 11.5 * 24 * 3600))
  end   <- start + duration
  fmt   <- function(x) format(x, "%Y-%m-%d %H:%M:%S")
  rid   <- function(k) paste0("R_", replicate(k, paste(sample(c(0:9, letters, LETTERS), 15, replace = TRUE), collapse = "")))

  d <- data.frame(
    StartDate = fmt(start), EndDate = fmt(end), Status = 0, IPAddress = "",
    Progress = 100, `Duration (in seconds)` = duration, Finished = 1,
    RecordedDate = fmt(end + 2), ResponseId = rid(n),
    RecipientLastName = "", RecipientFirstName = "", RecipientEmail = "", ExternalReference = "",
    LocationLatitude = "", LocationLongitude = "", DistributionChannel = "anonymous", UserLanguage = "EN",
    cups = NA, env_1 = env_1, env_2 = env_2, env_3 = env_3, attn = attn,
    glow_1 = glow_1, glow_2 = glow_2, glow_3 = glow_3,
    wtp = wtp, mc_seal = mc_seal, age = age, gender = gender,
    condition = ifelse(seal == 1, "Seal", "Control"),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  # unfinished responses: later questions were never seen and stay empty
  pre_vars  <- c("attn", "glow_1", "glow_2", "glow_3", "wtp", "mc_seal", "age", "gender", "condition")
  post_vars <- c("wtp", "mc_seal", "age", "gender")
  d[drop_pre, pre_vars]   <- NA
  d[drop_post, post_vars] <- NA
  d$env_3[drop_pre[1:2]]  <- NA
  d$glow_3[drop_post[1:3]] <- NA; d$glow_2[drop_post[1:2]] <- NA; d$glow_1[drop_post[1]] <- NA
  d$Finished[c(drop_pre, drop_post)] <- 0
  d$Progress[drop_pre]  <- sample(c(18, 27), 4, replace = TRUE)
  d$Progress[drop_post] <- sample(c(55, 64, 73), 8, replace = TRUE)
  d$`Duration (in seconds)`[c(drop_pre, drop_post)] <- round(runif(12, 25, 160))
  # Qualtrics records unfinished responses when the session expires (default: one week)
  late <- as.POSIXct(d$StartDate[c(drop_pre, drop_post)], tz = "UTC") + 7 * 24 * 3600
  d$RecordedDate[c(drop_pre, drop_post)] <- fmt(late)

  # three preview responses by the group (after the launch, to check the published version)
  prev <- d[sample(valid, 3), ]
  prev$Status <- 1; prev$DistributionChannel <- "preview"; prev$ResponseId <- rid(3)
  prev$StartDate <- fmt(as.POSIXct("2026-12-01 08:00:00", tz = "UTC") + c(600, 1500, 2400))
  prev$EndDate <- fmt(as.POSIXct(prev$StartDate, tz = "UTC") + c(95, 140, 120)); prev$RecordedDate <- prev$EndDate
  prev$`Duration (in seconds)` <- c(95, 140, 120)
  prev$age <- c(24, 24, 25); prev$wtp <- c("1", "8", "99")

  # warm-up question and control variable: cups of coffee per day. Coffee drinkers pay more for good
  # coffee (the habit is related to e_wtp), but the habit is unrelated to environmental concern.
  habit  <- 0.55 * e_wtp / 1.55 + sqrt(1 - 0.55^2) * rnorm(n)
  d$cups <- pmax(0, round(2.1 + 1.05 * habit))
  prev$cups <- c(2, 2, 3)

  d <- rbind(prev, d)
  d
}

d <- make_raw()

# --- write the file with the three Qualtrics header rows -------------------
labels <- c(
  "Start Date", "End Date", "Response Type", "IP Address", "Progress", "Duration (in seconds)", "Finished",
  "Recorded Date", "Response ID", "Recipient Last Name", "Recipient First Name", "Recipient Email",
  "External Data Reference", "Location Latitude", "Location Longitude", "Distribution Channel", "User Language",
  "How many cups of coffee do you drink on a typical day?",
  "How much do you agree with the following statements? - Humans are severely abusing the environment.",
  "How much do you agree with the following statements? - The so-called \"ecological crisis\" facing humankind has been greatly exaggerated.",
  "How much do you agree with the following statements? - If things continue on their present course, we will soon experience a major ecological catastrophe.",
  "To show that you are reading the questions carefully, please select \"Tea\".",
  "Imagine you bought this coffee. How much do you agree? - With this coffee, I can feel good because I help to protect the environment.",
  "Imagine you bought this coffee. How much do you agree? - With this coffee, I can feel better because I do not harm the environment.",
  "Imagine you bought this coffee. How much do you agree? - With this coffee, I have the feeling of contributing to the well-being of humanity and nature.",
  "What is the most you would be willing to pay for a 250 g pack of this coffee? (in euros)",
  "The coffee on the product page carried a seal for sustainable farming.",
  "How old are you? (in years)", "What is your gender?", "condition")
import_ids <- c(
  '{"ImportId":"startDate","timeZone":"Europe/Vienna"}', '{"ImportId":"endDate","timeZone":"Europe/Vienna"}',
  '{"ImportId":"status"}', '{"ImportId":"ipAddress"}', '{"ImportId":"progress"}', '{"ImportId":"duration"}',
  '{"ImportId":"finished"}', '{"ImportId":"recordedDate","timeZone":"Europe/Vienna"}', '{"ImportId":"_recordId"}',
  '{"ImportId":"recipientLastName"}', '{"ImportId":"recipientFirstName"}', '{"ImportId":"recipientEmail"}',
  '{"ImportId":"externalDataReference"}', '{"ImportId":"locationLatitude"}', '{"ImportId":"locationLongitude"}',
  '{"ImportId":"distributionChannel"}', '{"ImportId":"userLanguage"}',
  '{"ImportId":"QID3_TEXT"}', '{"ImportId":"QID4_1"}', '{"ImportId":"QID4_2"}', '{"ImportId":"QID4_3"}',
  '{"ImportId":"QID5"}', '{"ImportId":"QID8_1"}', '{"ImportId":"QID8_2"}', '{"ImportId":"QID8_3"}',
  '{"ImportId":"QID9_TEXT"}', '{"ImportId":"QID10"}', '{"ImportId":"QID11_TEXT"}', '{"ImportId":"QID12"}',
  '{"ImportId":"condition"}')
stopifnot(length(labels) == ncol(d), length(import_ids) == ncol(d))

body <- vapply(d, function(x) ifelse(is.na(x), "", trimws(as.character(x))), character(nrow(d)))
out  <- rbind(names(d), labels, import_ids, body)
csv_line <- function(x) paste0('"', gsub('"', '""', x, fixed = TRUE), '"', collapse = ",")
con <- file(file.path("data", "coffee_seal_qualtrics.csv"), open = "wb")
writeLines(enc2utf8(apply(out, 1, csv_line)), con, useBytes = TRUE)
close(con)
cat("Wrote data/coffee_seal_qualtrics.csv:", nrow(d), "responses,", ncol(d), "columns\n")
