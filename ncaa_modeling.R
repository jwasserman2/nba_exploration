library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)

# Load data
ncaa_player <- readr::read_csv("~/Desktop/all_ncaa_stats.csv")
ncaa_team <- readr::read_csv("~/Desktop/all_school_stats.csv")

# Custom colors for viz
darkblue <- "#191970"
lightblue <- "#1E90FF"
gold <- "#DAA520"
orange <- "#FF8C00"
green <- "#228B22"
purple <- "#4B0082"
red <- "#B22222"

# Group records by final season
groups <- adjusted %>%
  dplyr::select(pid, class, season) %>%
  dplyr::group_by(pid) %>%
  dplyr::summarize(max_class = max(class),
                   age = 19 + max(season) - min(season)) %>%
  dplyr::ungroup()

