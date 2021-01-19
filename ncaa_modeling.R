library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)

# Load data
nba_player <- readr::read_csv("~/Desktop/player_stats.csv")
discard_cols <- c(
  "toss_col",
  "ws-dum",
  "bpm-dum"
)
nba_player %<>%
  dplyr::select(-discard_cols)

# Custom colors for viz
darkblue <- "#191970"
lightblue <- "#1E90FF"
gold <- "#DAA520"
orange <- "#FF8C00"
green <- "#228B22"
purple <- "#4B0082"
red <- "#B22222"

# Determine weights for college weighted averages
valid_mp_seasons_by_pid <- adjusted %>%
  dplyr::select(pid, class, season, mp) %>%
  dplyr::group_by(pid) %>%
  dplyr::summarize(yo_college = max(season) - min(season) + 1,
                   num_valid_seasons = sum(ifelse(mp > 20, 1, 0)),
                   trd_season = max(season[mp > 20]),
                   sec_season = ifelse(num_valid_seasons >= 2,
                                       max(season[mp > 20 & season != trd_season]),
                                       NA_integer_),
                   fst_season = ifelse(num_valid_seasons >= 3,
                                       max(season[mp > 20 & season != trd_season &
                                                    season != sec_season]),
                                       NA_integer_),
                   trd_season_wt = ifelse(!is.na(trd_season) & trd_season != -Inf,
                                          sqrt(mp[season == trd_season]) * .6,
                                          0),
                   sec_season_wt = ifelse(!is.na(sec_season) & sec_season != -Inf,
                                          sqrt(mp[season == sec_season]) * .3,
                                          0),
                   fst_season_wt = ifelse(!is.na(fst_season) & fst_season != -Inf,
                                          sqrt(mp[season == fst_season]) * .1,
                                          0),
                   total_weights = trd_season_wt + sec_season_wt + fst_season_wt) %>%
  dplyr::ungroup()

# Generate 3-year weighted averages of college stats
weighted_avgs <- adjusted %>%
  # Join the weighting df
  dplyr::inner_join(valid_mp_seasons_by_pid) %>%
  # Numericize position
  dplyr::mutate_at("pos",
                   ~ dplyr::case_when(
                      .x %in% c("0", "GF", "CF", "D") ~ NA_integer_,
                      .x == "G" ~ 1L,
                      .x == "F" ~ 2L,
                      .x == "C" ~ 3L)) %>%
  dplyr::group_by(pid, pos, yo_college, height, weight, rsci) %>%
  dplyr::summarize_at(
    # We need SOS-weighted stats for most things, but also need a few others
    # that we excluded from SOS weighting
    vars("mp_per_g", "mp", "usg_pct", "ft_pct", "fta_per_fga_pct", "fg3a_per_fga_pct",
         c(tidyselect::matches("sos_weighted"))),
    # We need to sum these stats for "career" stats, average them for straight
    # averages (specifically MPG, USG%), and our weighted averages for everything else
    list(sum = ~ sum(.x, na.rm = T),
         mean = ~ mean(.x, na.rm = T),
         weighted = ~ sum(dplyr::case_when(
           season == trd_season ~ .x * trd_season_wt / total_weights,
           season == sec_season ~ .x * sec_season_wt / total_weights,
           season == fst_season ~ .x * fst_season_wt / total_weights,
           TRUE ~ 0)))
  ) %>%
  dplyr::ungroup()

# Find rookie seasons for each NBA player
fst_seasons <- nba_player %>%
  dplyr::group_by(player) %>%
  dplyr::summarize(fst_year = min(year)) %>%
  dplyr::ungroup()

# Full join NBA rookie stats to college weighted averages on PID
# NCAA PID form: firstname-lastname-#, NBA PID form: firstname-lastname

# Mapping for specific players for whom this PID join needs manual alteration
# NBA <--> NCAA
pid_map <- c(
  "dewan-hernandez" = "dewan-huell",
  "garrison-mathews" = "garrison-matthews",
  "ja-morant" = "temetrius-morant",
  "zach-norvell" = "zach-norvelljr",
  "kz-okpala" = "kezie-okpala",
  "marvin-bagley" = "marvin-bagleyiii",
  "shake-milton" = "malik-milton",
  "ray-spalding" = "raymond-spalding",
  "bam-adebayo" = "edrice-adebayo",
  "naz-mitrou-long" = "nazareth-mitrou-long",
  "andrew-white" = "andrew-whiteiii",
  "kay-felder" = "kahlil-felder",
  "yogi-ferrell" = "kevin-ferrell",
  "fred-vanvleet" = "fred-van-vleet",
  "stephen-zimmerman" = "stephen-zimmermanjr",
  "bryce-dejean-jones" = "bryce-jones",
  "devyn-marble" = "roy-devyn-marble",
  "ish-smith" = "ishmael-smith",
  "jeff-ayres" = "jeff-pendergraph",
  "henry-walker" = "bill-walker",
  "lou-amundson" = "louis-amundson",
  "jeff-sheppard" = "jeffrey-sheppard"
)

rookies_joined <- weighted_avgs %>%
  # Only keep columns we want to model with
  dplyr::select(pid, height, weight, yo_college, pos, rsci, mp_sum, mp_mean,
                mp_per_g_mean, tidyselect::matches("_weighted_weighted")) %>%
  dplyr::mutate(
    # Translate NCAA PID form to NBA PID form
    nba_pid = ifelse(
      # These are players for whom our standard join results in duplicate records,
      # so we have them keep their original NCAA PID
      pid %in% c("tony-mitchell-3", "tony-mitchell-4", "john-lucas-2"),
      pid,
      str_replace(pid, "(-\\d+|--\\d+)", ""))
  ) %>%
  dplyr::full_join(
    nba_player %>%
      dplyr::left_join(fst_seasons) %>%
      dplyr::filter(year == fst_year) %>%
      dplyr::mutate(
        # Create NBA PID from first and last names
        pid = dplyr::case_when(
          # Manually change these three to the NCAA PID we specified above
          player == "Mitchell,Tony" & team_id == "MIL" ~ "tony-mitchell-3",
          player == "Mitchell,Tony" & team_id == "DET" ~ "tony-mitchell-4",
          player == "Lucas III,John" ~ "john-lucas-2",
          TRUE ~ as.character(str_glue(
          "{str_replace_all(tolower(sapply(strsplit(player, ','), '[', 2)), ' ', '-') %>%
            str_replace_all(., \"'\", '') %>%
            str_replace_all(., '\\\\.', '') %>%
            str_replace_all(., 'á', 'a')}\\
          -{str_replace_all(tolower(sapply(strsplit(player, ','), '[', 1)), ' ', '-') %>%
            str_replace_all(., \"'\", '') %>%
            str_replace_all(., '\\\\.', '') %>%
            str_replace_all(., 'á', 'a')}")))
      ) %>%
      dplyr::mutate_at("pid", ~ ifelse(.x %in% names(pid_map),
                                       pid_map[.x],
                                       .x)),
    by = c("nba_pid" = "pid"),
    suffix = c("_ncaa", "_nba")
  )

# Handling NA's for weight and height
# Use these cols for imputation models
bio_impute_cols <- c(
  "height", "pos_ncaa", "weight",
  "blk_per_100_sos_weighted_weighted",
  "drb_per_100_sos_weighted_weighted",
  "fg2a_per_100_sos_weighted_weighted",
  "fg3a_per_100_sos_weighted_weighted",
  "fta_per_100_sos_weighted_weighted",
  "orb_per_100_sos_weighted_weighted",
  "trb_per_100_sos_weighted_weighted",
  "blk_pct_sos_weighted_weighted",
  "orb_pct_sos_weighted_weighted",
  "trb_pct_sos_weighted_weighted"
)

# Function for imputing bio info
impute_bio_info <- function(player_df, impute_dv, impute_covs, impute_covs_to_dummify) {
  message(str_glue("Imputing {impute_dv}..."))
  # GLM model used for imputing
  mod <- glmnet::cv.glmnet(
    x = player_df %>%
      dplyr::select(!!!syms(impute_covs)) %>%
      dplyr::filter_all(all_vars(!is.na(.))) %>%
      dplyr::mutate_at(impute_covs_to_dummify, as.factor) %>%
      dplyr::select(-!!sym(impute_dv)) %>%
      model.matrix(~ ., .),
    y = player_df %>%
      dplyr::select(c(impute_covs, impute_dv)) %>%
      dplyr::filter_all(all_vars(!is.na(.))) %>%
      dplyr::pull(!!sym(impute_dv)) %>%
      as.matrix())

  # Join imputed values and add to full df
  preds <- predict(
    mod,
    player_df %>%
      dplyr::filter(is.na(!!sym(impute_dv))) %>%
      dplyr::select(!!!syms(dplyr::setdiff(impute_covs, impute_dv))) %>%
      dplyr::filter_all(all_vars(!is.na(.))) %>%
      dplyr::mutate_at(impute_covs_to_dummify, as.factor) %>%
      model.matrix(~ ., .))

  # Return df that has impute values and original values and columns indicating
  # the imputed values
  out_df <- player_df %>%
    dplyr::filter(is.na(!!sym(impute_dv))) %>%
    dplyr::select(-!!sym(impute_dv)) %>%
    dplyr::filter_at(dplyr::setdiff(impute_covs, impute_dv),
                     ~ !is.na(.x)) %>%
    dplyr::bind_cols(!!impute_dv := as.vector(preds)) %>%
    dplyr::mutate(!!str_glue("{impute_dv}_imputed") := 1) %>%
    dplyr::bind_rows(
      player_df %>%
        dplyr::filter(!is.na(!!sym(impute_dv))) %>%
        dplyr::mutate(!!str_glue("{impute_dv}_imputed") := 0)
    )

  return(out_df)
}

rookies_joined <- impute_bio_info(rookies_joined,
                                  "weight",
                                  bio_impute_cols,
                                  c("pos_ncaa")) %>%
  impute_bio_info(.,
                  "height",
                  bio_impute_cols,
                  c("pos_ncaa"))

# Everyone who's missing a position is missing games and MP--we'll just remove
# rather than do the bidirectional imputation that would probably use weight and
# height
rookies_joined %<>%
  dplyr::filter(!is.na(pos_ncaa))

# Filter out 15 records with missing minutes
rookies_joined %<>%
  dplyr::filter(!is.na(mp_per_g_mean))

## LET'S MODEL
dv <- "ts_pct"

# Stage 1: Inclusion Model
inc_mod_df <- rookies_joined %>%
  dplyr::mutate(inclusion_dv = ifelse(!is.na(dv), 1L, 0L),
                pos_ncaa = as.factor(pos_ncaa)) %>%
  dplyr::select(c(dplyr::intersect(colnames(weighted_avgs),
                                   colnames(rookies_joined)),
                  "pos_ncaa", "height_imputed", "weight_imputed",
                  "inclusion_dv"))
inc_mod <- randomForestSRC::rfsrc(inclusion_dv ~., inc_mod_df)
