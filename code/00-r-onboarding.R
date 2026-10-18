# ======================================================================
# Marketing Analytics — R onboarding
# Companion code to the appendix "R onboarding" of the course book.
#
# How to use this script:
#   1. Open your RStudio Project (see the onboarding appendix).
#   2. Save this file in the project's "code" folder.
#   3. Run it line by line: put the cursor in a line and press
#      Ctrl + Enter (Windows) or Cmd + Enter (Mac).
#
# Everything after a "#" is a comment and is ignored by R.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Install the packages used in this course (run once per computer)
# ----------------------------------------------------------------------

# This snippet checks which of the required packages are missing and
# installs only those. You can run it as often as you like.
required_packages <- c("tidyverse", "psych", "readxl", "haven", "rmarkdown")
missing_packages  <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) install.packages(missing_packages)


# ----------------------------------------------------------------------
# 1. Objects and assignment
# ----------------------------------------------------------------------

streams <- 163608            # assign a number to the object 'streams'
streams                      # typing the name prints the value
streams * 2                  # objects can be used in calculations

artist <- "Imagine Dragons"  # text must be put in quotation marks
artist

# R is case-sensitive: the next line produces an error on purpose,
# because 'Streams' (capital S) does not exist.
# Streams

# Comparisons return TRUE or FALSE
streams > 100000
streams == 163608            # note the DOUBLE equals sign for comparisons


# ----------------------------------------------------------------------
# 2. Functions and arguments
# ----------------------------------------------------------------------

seq(from = 1, to = 10, by = 3)   # arguments named explicitly
seq(1, 10, 3)                     # same result, arguments in default order
seq(by = 3, to = 10, from = 1)    # named arguments may come in any order

round(3.14159, digits = 2)

# Open the help page of a function (or press F1 on the function name):
?seq


# ----------------------------------------------------------------------
# 3. Vectors and data types
# ----------------------------------------------------------------------

# c() ("combine") creates a vector: a sequence of values of ONE type
top_streams  <- c(163608, 126687, 120480, 110022, 108630)          # numeric
top_genres   <- c("Dance", "Alternative", "Latino", "Dance", "Dance")  # character
top_explicit <- c(FALSE, FALSE, FALSE, TRUE, FALSE)                 # logical

mean(top_streams)       # many functions work on whole vectors
length(top_streams)     # number of elements
class(top_genres)       # the type of an object

# Categories with a fixed set of values are stored as factors
top_genres_factor <- as.factor(top_genres)
top_genres_factor
levels(top_genres_factor)

# Dates
release <- as.Date("2026-10-12")
release + 30            # date arithmetic works: 30 days later

# Converting between types
as.character(163608)    # number -> text
as.numeric("3.14")      # text -> number


# ----------------------------------------------------------------------
# 4. Data frames
# ----------------------------------------------------------------------

# A data frame is R's table: each column is a variable, each row an
# observation. All columns have the same length.
top_tracks <- data.frame(
  artist   = c("Ed Sheeran", "Imagine Dragons", "J. Balvin"),
  streams  = c(163608, 126687, 120480),
  explicit = c(FALSE, FALSE, FALSE)
)

top_tracks            # print the whole data frame
top_tracks$streams    # $ extracts one column as a vector
str(top_tracks)       # structure: rows, columns, type of each column
nrow(top_tracks)      # number of rows
ncol(top_tracks)      # number of columns

# View(top_tracks)    # opens a spreadsheet-like viewer (capital V!)


# ----------------------------------------------------------------------
# 5. Loading packages (in every session)
# ----------------------------------------------------------------------

# install.packages() installs a package once per computer (see section 0);
# library() loads it — put library() calls at the top of every script.
library(tidyverse)

# A function can also be called without loading its package:
# dplyr::filter(...)


# ----------------------------------------------------------------------
# 6. Loading the course data
# ----------------------------------------------------------------------

# The main data set of Chapter 2: streaming chart data for ~67,000 tracks.
# Why read_csv2() and not read_csv()? -> see Chapter 2, section "Getting data into R".
music_url  <- "https://raw.githubusercontent.com/wu-rds/MA2026/main/data/music_data_fin.csv"
music_data <- read_csv2(music_url)

dim(music_data)       # number of rows and columns
str(music_data)       # structure of the data


# ----------------------------------------------------------------------
# 7. Checkpoint: your first reproducible report
# ----------------------------------------------------------------------

# Create a new R Markdown file (File -> New File -> R Markdown... -> HTML),
# delete the template content below the header, and paste the following
# (without the leading "# "). Then click "Knit".
#
# ---
# title: "First report"
# author: "Your name"
# output: html_document
# ---
#
# ```{r}
# library(tidyverse)
# music_url <- "https://raw.githubusercontent.com/wu-rds/MA2026/main/data/music_data_fin.csv"
# music_data <- read_csv2(music_url)
# ```
#
# The data set contains `r nrow(music_data)` tracks.
#
# ```{r}
# str(music_data)
# ```
#
# If the HTML file opens and shows the structure of the data, your setup
# works and you are ready for the second session.
