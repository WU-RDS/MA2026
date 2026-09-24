# ======================================================================
# Marketing Analytics — Chapter 2: Describing Data
# Companion code to Chapter 2 of the course book. The chapter explains
# every step in detail; this script collects the code in one place.
#
# Run it from your RStudio Project, section by section.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------------------

# Install missing packages (only needed once per computer)
required_packages <- c("tidyverse", "psych")
missing_packages  <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(tidyverse)   # dplyr, ggplot2, readr, forcats, tidyr, ...
library(psych)       # describe()

options(scipen = 999)  # print large numbers without scientific notation


# ======================================================================
# 2.2 Getting data into R
# ======================================================================

# The data are stored as a European-style CSV file: values separated by
# semicolons, decimal commas -> read_csv2()
music_url  <- "https://raw.githubusercontent.com/wu-rds/MA2026/main/data/music_data_fin.csv"
music_data <- read_csv2(music_url)

# What happens with the wrong function? No error — but only ONE column.
wrong_import <- read_csv(music_url)
dim(wrong_import)

# If in doubt, look at the raw file first
readLines(music_url, n = 3)

# A first look at the data
dim(music_data)       # rows and columns
glimpse(music_data)   # every column: name, type, first values
head(music_data)      # first rows
summary(select(music_data, streams, danceability, song_length))

# Other formats (examples; the files are not part of the course data):
# library(readxl); read_excel("data/survey.xlsx", sheet = "Sheet1")
# library(haven);  read_sav("data/survey.sav")
# write_csv(music_data, "data/music_clean.csv")   # writing data


# ======================================================================
# 2.3 Variables and levels of measurement
# ======================================================================

# Tell R the level of measurement by choosing the right type
music_data <- music_data |>
  mutate(
    genre     = as.factor(genre),                                   # nominal
    label     = as.factor(label),                                   # nominal
    explicit  = factor(explicit, levels = c(0, 1),
                       labels = c("not explicit", "explicit")),     # nominal (binary)
    top10     = factor(top10, levels = c(0, 1), labels = c("no", "yes")),
    artist_id = as.character(artist_id),                            # an ID is not a number
    expert_rating = factor(expert_rating,
                           levels = c("poor", "fair", "good", "excellent", "masterpiece"),
                           ordered = TRUE)                          # ordinal
  )

# Ordered factor: categories appear in their meaningful order
table(music_data$expert_rating)

# Median of an ordinal variable: the category of the middle observation
median_position <- ceiling(nrow(music_data) / 2)
sort(music_data$expert_rating)[median_position]


# ======================================================================
# 2.4 Data handling
# ======================================================================

# --- Choosing rows and columns ----------------------------------------
big_pop <- filter(music_data, genre == "Pop", streams > 10000000)
nrow(big_pop)

filter(music_data, label %in% c("Sony Music", "Warner Music")) |> nrow()

select(big_pop, artistName, trackName, streams) |> head(3)

# --- The pipe: chaining steps ("and then") ---------------------------
music_data |>
  filter(genre == "Pop") |>
  arrange(desc(streams)) |>
  select(artistName, trackName, streams) |>
  head(5)

# --- Creating and changing variables ----------------------------------
music_data <- music_data |>
  mutate(
    streams_mio  = streams / 1e6,                          # streams in millions
    release_year = as.integer(format(release_date, "%Y")), # year from the date
    log_streams  = log10(streams)                          # log scale
  ) |>
  rename(artist = artistName, track = trackName)

# Merging rare categories
music_data |>
  mutate(genre_top5 = fct_lump_n(genre, n = 5, other_level = "Remaining genres")) |>
  count(genre_top5)

# --- Summaries by group ------------------------------------------------
music_data |>
  group_by(label) |>
  summarise(
    n_tracks       = n(),
    median_streams = median(streams),
    share_top10    = mean(top10 == "yes")
  )

music_data |> count(genre, sort = TRUE) |> head(5)

# --- Combining tables ---------------------------------------------------
label_info <- tibble(
  label      = c("Universal Music", "Sony Music", "Warner Music", "Independent"),
  label_type = c("Major", "Major", "Major", "Independent")
)

music_data <- music_data |> left_join(label_info, by = "label")
music_data |> count(label_type)   # always check the row count after a join

# --- The same operations in base R (for reading other people's code) ---
# music_data[music_data$streams > 1e6, ]                   # filter
# music_data[, c("artist", "streams")]                      # select
# music_data[order(-music_data$streams), ]                  # arrange
# aggregate(streams ~ genre, data = music_data, FUN = median)  # group summary


# ======================================================================
# 2.5 Not trusting your data too early
# ======================================================================

# Check 2: missing values — and zeros that mean "unknown"
colSums(is.na(music_data))[colSums(is.na(music_data)) > 0]   # no NAs at all?
music_data |> summarise(share_zero_youtube = mean(youtube_views == 0))

# If a zero means "unknown", recode it as missing:
# music_data <- music_data |> mutate(youtube_views = na_if(youtube_views, 0))
# ... and remember na.rm = TRUE in functions such as mean()

# Check 3: impossible values
music_data |>
  summarise(
    first_release = min(release_date), last_release = max(release_date),
    min_length = min(song_length), max_length = max(song_length),
    min_tempo  = min(tempo)
  )

music_data |>
  filter(release_date > as.Date("2025-01-01")) |>
  select(artist, track, release_date)

# Check 4: duplicates — is the identifier really unique?
music_data |> summarise(n_rows = n(), n_distinct_isrc = n_distinct(isrc))

# Check 5: extreme values — errors or information?
music_data |> arrange(desc(streams)) |> select(artist, track, streams) |> head(5)

# Document corrections explicitly, e.g. with a flag instead of deleting:
music_data <- music_data |>
  mutate(valid_date = release_date <= as.Date("2021-12-31"))
count(music_data, valid_date)

# How did the data come to be? Our data contain only tracks that made the
# charts -> see section 2.5 of the chapter on selection into the data set.


# ======================================================================
# 2.6 Summarizing data
# ======================================================================

# --- Categorical variables: frequencies ---------------------------------
music_data |>
  count(genre, sort = TRUE) |>
  mutate(share = n / sum(n))

# Conditional frequencies: share of top-10 hits per genre
music_data |>
  group_by(genre) |>
  summarise(n = n(), share_top10 = mean(top10 == "yes")) |>
  arrange(desc(share_top10))

# The same with base R
table(music_data$genre, music_data$top10)
prop.table(table(music_data$genre, music_data$top10), margin = 1)

# --- Continuous variables: location, dispersion, shape ------------------
music_data |>
  summarise(
    mean   = mean(streams),
    median = median(streams),
    sd     = sd(streams),
    q25    = quantile(streams, 0.25),
    q75    = quantile(streams, 0.75),
    max    = max(streams)
  )

# Coefficient of variation (spread relative to the level)
sd(music_data$danceability) / mean(music_data$danceability)

# Many statistics at once
music_data |>
  select(streams, danceability, energy, valence, song_length) |>
  describe() |>
  select(n, mean, sd, median, min, max, skew)

# --- Summaries by group --------------------------------------------------
music_data |>
  group_by(label_type) |>
  summarise(n = n(), median_streams = median(streams),
            mean_danceability = mean(danceability))

# --- Standardized values (z-scores) ------------------------------------
music_data <- music_data |>
  mutate(danceability_z = (danceability - mean(danceability)) / sd(danceability))
summary(music_data$danceability_z)


# ======================================================================
# 2.7 Visualizing data
# ======================================================================

# --- Building a bar chart step by step ---------------------------------
genre_shares <- music_data |>
  count(genre) |>
  mutate(share = n / sum(n))

# Minimal version: data + mapping + geom
ggplot(genre_shares, aes(x = genre, y = share)) +
  geom_col()

# Finished version
ggplot(genre_shares, aes(x = fct_reorder(genre, share), y = share)) +
  geom_col(fill = "#0f5da8") +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            hjust = -0.15, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
  labs(x = NULL, y = "Share of tracks",
       title = "Pop and hip-hop dominate the charts",
       subtitle = "Share of tracks per genre") +
  theme_minimal()

# --- Distributions: histogram, linear vs. log scale --------------------
ggplot(music_data, aes(x = streams)) +
  geom_histogram(bins = 50, fill = "#0f5da8") +
  labs(x = "Streams", y = "Number of tracks") +
  theme_minimal()

ggplot(music_data, aes(x = streams)) +
  geom_histogram(bins = 50, fill = "#0f5da8") +
  scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "Streams (log scale)", y = "Number of tracks") +
  theme_minimal()

# --- Comparing groups: boxplots -----------------------------------------
music_data |>
  mutate(genre = fct_lump_n(genre, n = 6, other_level = "Remaining genres")) |>
  ggplot(aes(x = fct_reorder(genre, streams, .fun = median), y = streams)) +
  geom_boxplot(fill = "#dbe7f3", outlier.alpha = 0.15) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  coord_flip() +
  labs(x = NULL, y = "Streams (log scale)") +
  theme_minimal()

# --- Relationships: scatter plot (transparency against overplotting) ---
ggplot(music_data, aes(x = danceability, y = streams)) +
  geom_point(alpha = 0.05, color = "#0f5da8") +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "Danceability (0-100)", y = "Streams (log scale)") +
  theme_minimal()

# --- Development over time: line chart ----------------------------------
music_data |>
  count(release_year) |>
  ggplot(aes(x = release_year, y = n)) +
  geom_line(color = "#0f5da8", linewidth = 1) +
  geom_point(color = "#0f5da8", size = 1.5) +
  labs(x = "Release year", y = "Number of tracks") +
  theme_minimal()

# --- Saving the most recent plot ----------------------------------------
# ggsave("graphics/genre_shares.png", width = 8, height = 4.5, dpi = 300)


# ======================================================================
# 2.8 From description to insight
# ======================================================================

# "Do tracks with explicit lyrics perform worse?"
music_data |>
  group_by(explicit) |>
  summarise(n = n(), median_streams = median(streams))

# Look within genres before writing anything down
music_data |>
  mutate(genre = fct_lump_n(genre, n = 5, other_level = "Remaining genres")) |>
  group_by(genre) |>
  summarise(
    share_explicit      = mean(explicit == "explicit"),
    median_not_explicit = median(streams[explicit == "not explicit"]),
    median_explicit     = median(streams[explicit == "explicit"])
  )

# ... and by label type
music_data |>
  group_by(label_type) |>
  summarise(share_explicit = mean(explicit == "explicit"),
            median_streams = median(streams))

# Descriptive (OK):  "Among the tracks in our chart data, those with explicit
#                     lyrics have a lower median number of streams."
# Causal (NOT OK):    "Explicit lyrics reduce streams."
