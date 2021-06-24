library(dplyr)
library(stringr)
library(magrittr)
library(rlang)
library(ggplot2)

# Load data
ncaa_player <- readr::read_csv("~/Desktop/all_ncaa_stats.csv")
player_2021 <- readr::read_csv("~/Desktop/ncaa_stats_2021.csv")
ncaa_team <- readr::read_csv("~/Desktop/all_school_stats.csv")
team_2021 <- readr::read_csv("~/Desktop/school_stats_2021.csv")

ncaa_player <- ncaa_player %>%
  dplyr::filter(season != 2021) %>%
  dplyr::bind_rows(player_2021)
ncaa_team <- ncaa_team %>%
  dplyr::filter(season != 2021) %>%
  dplyr::bind_rows(team_2021)

# Custom colors for viz
darkblue <- "#191970"
lightblue <- "#1E90FF"
gold <- "#DAA520"
orange <- "#FF8C00"
green <- "#228B22"
purple <- "#4B0082"
red <- "#B22222"

# Get rid of extra columns
discard_cols <- c(
  "bpm-dum",
  "high_school",
  "number",
  "ws-dum",
  "sos"
)
ncaa_player %<>%
  dplyr::select(-discard_cols)

# Convert height to inches
adjusted <- ncaa_player %>%
  dplyr::mutate_at(
    "height", ~ as.numeric(sapply(strsplit(.x, "-"), "[", 1)) * 12 +
      as.numeric(sapply(strsplit(.x, "-"), "[", 2))
  )

# Change RSCI and Class to factor
adjusted %<>%
  dplyr::mutate(
    rsci = factor(
      dplyr::case_when(
        rsci == 0 | is.na(rsci) ~ "unranked",
        rsci >= 81 ~ "50-100",
        rsci >= 26 ~ "26-50",
        rsci >= 11 ~ "11-25",
        rsci >= 6 ~ "6-10",
        rsci >= 2 ~ "2-5",
        rsci == 1 ~ "1"
      ),
      levels = c("1", "2-5", "6-10", "11-25", "26-50", "50-100", "unranked")),
    class = factor(
      class,
      levels = c("FR", "Fr", "SO", "JR", "Jr", "SR", "Sr", "sr"),
      labels = c("FR", "FR", "SO", "JR", "JR", "SR", "SR", "SR")
    )
  )

# Make totals per 40 minutes
counting_stats <- c(
  "ast", "blk", "drb", "fg", "fg2", "fg2a", "fg3", "fg3a", "fga", "ft", "fta",
  "orb", "pf", "pts", "stl", "tov", "trb"
)

# Account for pace: calculate total stats as per 100 possessions
adjusted %<>%
  dplyr::mutate_at(counting_stats,
                   list("per_100" = ~ ifelse(mp == 0, 0, .x / (pace / 40 * mp) * 100),
                        "per_g" = ~ ifelse(g == 0, 0, .x / g))) %>%
  dplyr::mutate(mp_per_g = ifelse(g == 0, 0, mp / g))

adjusted %<>%
  dplyr::left_join(
    ncaa_team %>%
      dplyr::select(season, school, sos)
  ) %>%
  dplyr::mutate_at(c("ows", "dws"), list("per_40" = ~ ifelse(mp == 0, 0, .x / mp * 40)))

# Scraper set 0's for NA's for a few columns, will impute here
adjusted %<>%
  dplyr::mutate_at(c("drb", "orb"),
                   ~ ifelse(season <= 2000 & .x == 0, NA_real_, .x)) %>%
  dplyr::mutate_at(c("drb_pct", "orb_pct"),
                   ~ ifelse(season <= 2009 & .x == 0, NA_real_, .x)) %>%
  dplyr::mutate_at("pos", ~ ifelse(.x == "0", NA_character_, .x))

# Remove rows w/o height, weight, and class, as well as columns with more than half NULLs
remove_cols <- adjusted %>%
  dplyr::summarise_all(~ sum(ifelse(is.na(.x), 1, 0)) / dplyr::n()) %>%
  t() %>%
  data.table::as.data.table(keep.rownames=TRUE) %>%
  setNames(., c("stat", "pct_null")) %>%
  dplyr::filter(pct_null > 0.5) %>%
  dplyr::pull(stat)

adjusted <- adjusted %>%
  dplyr::select(-remove_cols) %>%
  dplyr::filter(!is.na(height) & !is.na(weight) & !is.na(class))

# For players with 0 MP set per 100 cols to 0; 0 GP set per game cols to 0
adjusted <- adjusted %>%
  dplyr::mutate_at(vars(tidyselect::contains("_100")), ~ ifelse(is.na(.x), 0, .x))

adjusted <- adjusted %>%
  dplyr::mutate_at(vars(tidyselect::contains("per_g")), ~ ifelse(is.na(.x), 0, .x))

# rows with pid == james-thompsoniv-1 have position = "D", recode to "F"
adjusted <- adjusted %>%
  dplyr::mutate_at("pos", ~ ifelse(pid == "james-thompsoniv-1", "F", .x))
