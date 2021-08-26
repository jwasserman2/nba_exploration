library(dplyr)
library(stringr)
library(magrittr)
library(rlang)
library(randomForestSRC)
library(survival)
library(ggplot2)

# Load data
nba_player <- readr::read_csv("data/player_stats.csv")
discard_cols <- c(
  "toss_col",
  "DUMMY",
  "DUMMY_1"
)
nba_player %<>%
  dplyr::select(-discard_cols)

nba_combine <- readr::read_csv("data/combine.csv")
colnames(nba_combine) <- trimws(colnames(nba_combine),which = "both") %>%
  str_replace_all(., "/", "") %>%
  str_replace_all(., "%", "") %>%
  str_replace_all(., "\\([A-Z]+\\)", "") %>%
  trimws(., which = "both") %>%
  str_replace_all(., "\\s", "_") %>%
  tolower(.)

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
  dplyr::group_by(pid) %>%
  dplyr::summarize(
    yo_college = max(season) - min(season) + 1,
    num_valid_seasons = sum(ifelse(mp > 20, 1, 0)),
    last_season = max(season[mp > 20]),
    last_minus_1_season = ifelse(
      num_valid_seasons >= 2,
      max(season[mp > 20 & season != last_season]),
      NA_integer_),
    last_minus_2_season = ifelse(
      num_valid_seasons >= 3,
      max(season[mp > 20 & season != last_season & season != last_minus_1_season]),
      NA_integer_)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(num_valid_seasons > 0)

adjusted_with_weights <- adjusted %>%
  dplyr::left_join(valid_mp_seasons_by_pid, by = "pid") %>%
  dplyr::filter(!is.na(num_valid_seasons)) %>%
  dplyr::mutate(
    season_weight = dplyr::case_when(
      season == last_season ~ sqrt(mp) * .6,
      season == last_minus_1_season ~ sqrt(mp) * .3,
      season == last_minus_2_season ~ sqrt(mp) * .1
    ))

# Generate 3-year weighted averages of college stats
drop_cols <- c("class", "index", "pace", "school", "season", "num_valid_seasons",
               "last_season", "last_minus_1_season", "last_minus_2_season")
groupby_cols <- c("pid", "pos", "yo_college", "height", "weight", "rsci")

sum_cols <- c("ast", "blk", "drb", "dws", "fg", "fg2", "fg2a", "fg3", "fg3a",
              "fga", "ft", "fta", "g", "gs", "mp", "orb", "ows", "pf", "pprod",
              "pts", "stl", "tov", "trb", "ws")
career_sums <- adjusted_with_weights %>%
  dplyr::group_by(!!!syms(groupby_cols)) %>%
  dplyr::summarize_at(
    sum_cols,
    list(sum = ~ sum(.x, na.rm = T))
  ) %>%
  dplyr::ungroup()

tidyselect::poke_vars(colnames(adjusted_with_weights))
mean_cols <- c(sum_cols, "sos",
               colnames(adjusted_with_weights)[c(
                 tidyselect::contains("pct"), tidyselect::ends_with("per_g"),
                 tidyselect::ends_with("per_100"), tidyselect::ends_with("per_40"))])

career_weighted_avgs <- adjusted_with_weights %>%
  dplyr::group_by(!!!syms(groupby_cols)) %>%
  dplyr::summarize_at(
    vars(mean_cols),
    list(weighted = ~ sum(season_weight * .x, na.rm = T) /
           sum(season_weight, na.rm = T))
  ) %>%
  dplyr::ungroup()
tidyselect::poke_vars(NULL)

# Munge NBA Combine stats and join
munged_combine <- nba_combine %>%
  dplyr::mutate_if(is.character, ~ str_replace(.x, "%", "")) %>%
  dplyr::mutate_at("weight", ~ ifelse(.x == "0", NA_character_, .x)) %>%
  dplyr::mutate_if(is.character, ~ dplyr::case_when(trimws(.x) == "-" ~ "DNP",
                                                    is.na(.x) ~ "DNP",
                                                    TRUE ~ .x)) %>%
  dplyr::mutate_at(setdiff(colnames(nba_combine), c("player", "pos", "year")),
                   list("dnp" = ~ ifelse(.x == "DNP", 1, 0),
                        "dne" = ~ ifelse(.x == "DNE", 1, 0))) %>%
  dplyr::mutate_at(c("height_wo_shoes", "height_w_shoes", "standing_reach",
                     "wingspan"),
                   ~ ifelse(.x %in% c("DNP", "DNE"), NA_real_,
                          (str_replace_all(.x, "\\s", "") %>%
                            str_split(., "'") %>%
                            sapply(., "[", 1) %>% as.numeric() * 12) +
                            (str_replace_all(.x, "\\s", "") %>%
                               str_split(., "'") %>%
                               sapply(., "[", 2) %>% as.numeric()))) %>%
  dplyr::mutate_at(setdiff(colnames(nba_combine), c("player", "pos", "year")),
                   ~ ifelse(.x %in% c("DNP", "DNE"), 0, as.numeric(.x)))  %>%
  dplyr::mutate(pid = str_replace_all(player, "\\?", "") %>%
                  str_replace_all(., "\\.", "") %>%
                  str_replace_all(., "'", "") %>%
                  str_replace_all(., " ", "-") %>%
                  tolower(.) %>%
                  str_replace_all(., "-jr$", "jr") %>%
                  str_replace_all(., "-ii$", "ii") %>%
                  str_replace_all(., "-iii$", "iii") %>%
                  str_replace_all(., "-iv$", "iv")) %>%
  dplyr::select(-pos)

deduped_munged_combine <- munged_combine %>%
  dplyr::inner_join(
    munged_combine %>%
      dplyr::group_by(pid) %>%
      dplyr::summarize(last_combine = max(year)) %>%
      dplyr::ungroup(),
    by = c("pid", "year" = "last_combine"))

ncaa_modeling_df <- career_weighted_avgs %>%
  dplyr::left_join(valid_mp_seasons_by_pid[, c("pid", "last_season")], by = "pid") %>%
  dplyr::mutate(combine_pid = dplyr::case_when(
    str_replace(pid, "-[0-9]+", "") == "tim-hardaway-jr" ~ "tim-hardawayjr",
    TRUE ~ str_replace(str_replace(pid, "--[0-9]+", ""), "-[0-9]+", ""))) %>%
  dplyr::left_join(
    deduped_munged_combine,
    by = c("combine_pid" = "pid", "last_season" = "year"),
    suffix = c("_ncaa", "_combine")) %>%
  dplyr::mutate(
    weight_coalesce = ifelse(weight_combine == 0 | is.na(weight_combine),
                             weight_ncaa, weight_combine),
    height_coalesce = ifelse(height_wo_shoes == 0 | is.na(height_wo_shoes),
                             height, height_wo_shoes)) %>%
  dplyr::mutate_at(vars(tidyselect::ends_with("dne")),
                   ~ ifelse(is.na(.x), 0, .x)) %>%
  dplyr::mutate_at(vars(tidyselect::ends_with("dnp")),
                   ~ ifelse(is.na(.x), 1, .x)) %>%
  dplyr::mutate_at(setdiff(colnames(nba_combine),
                           c("weight", "player", "pos", "year")),
                   ~ ifelse(is.na(.x), 0, .x))

# Join NBA df to college weighted averages on PID
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
  "jeff-sheppard" = "jeffrey-sheppard",
  "michael-porter" = "michael-porterjr",
  "jj-barea" = "jose-barea"
)

# Create DF for inclusion model
inclusion_df <- ncaa_modeling_df %>%
  dplyr::mutate(
    # Translate NCAA PID form to NBA PID form
    nba_pid = ifelse(
      # These are players for whom our standard join results in duplicate records,
      # so we have them keep their original NCAA PID
      pid %in% c("tony-mitchell-3", "tony-mitchell-4", "john-lucas-2"),
      pid,
      str_replace(pid, "(-\\d+|--\\d+)", ""))
  ) %>%
  dplyr::left_join(
    nba_player %>%
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
            str_replace_all(., 'á', 'a') %>%
            str_replace_all(., 'ć', 'c') %>%
            str_replace_all(., 'è', 'e')}\\
          -{str_replace_all(tolower(sapply(strsplit(player, ','), '[', 1)), ' ', '-') %>%
            str_replace_all(., \"'\", '') %>%
            str_replace_all(., '\\\\.', '') %>%
            str_replace_all(., 'á', 'a') %>%
            str_replace_all(., 'ć', 'c') %>%
            str_replace_all(., 'è', 'e')}")))
      ) %>%
      dplyr::mutate_at("pid", ~ ifelse(.x %in% names(pid_map),
                                       pid_map[.x],
                                       .x)) %>%
      dplyr::distinct(pid) %>%
      dplyr::mutate(inclusion_dv = 1),
    by = c("nba_pid" = "pid")
  ) %>%
  dplyr::mutate_at("inclusion_dv", ~ ifelse(is.na(.x), 0, .x))

# Filter records without position
inclusion_df %<>%
  dplyr::filter(!is.na(pos))

# Stage 1: Inclusion Model
tidyselect::poke_vars(colnames(inclusion_df)[!str_detect(colnames(inclusion_df), "sos")])
standardize_df_vars <- function(df) {
  sds <- sapply(df, sd)
  sds_matrix <- matrix(sds) %>%
    rep(., nrow(df)) %>%
    matrix(ncol = nrow(df))

  means <- colMeans(df)
  means_matrix <- matrix(means) %>%
    rep(., nrow(df)) %>%
    matrix(ncol = nrow(df))

  return((df - t(means_matrix)) / t(sds_matrix))
}

pca_cols <- inclusion_df %>%
  dplyr::select(tidyselect::ends_with("weighted")) %>%
  colnames()

standardized_df <- inclusion_df %>%
  dplyr::select(pca_cols) %>%
  standardize_df_vars() %>%
  cbind(., inclusion_df %>% dplyr::select(pos))

g_pca <- standardized_df %>%
  dplyr::filter(pos == "G") %>%
  dplyr::select(-c("pos")) %>%
  princomp()

tidyselect::poke_vars(NULL)
anthro_cols <- c("lane_agility_time", "shuttle_run", "three_quarter_sprint",
                 "standing_vertical_leap", "max_vertical_leap", "body_fat",
                 "hand_length", "hand_width", "standing_reach", "wingspan")
form <- paste0(
  "inclusion_dv ~ ",
  paste(c(paste0("Comp.", 1:13),
          "yo_college", "rsci", "sos_weighted", "height_coalesce", "weight_coalesce",
          colnames(inclusion_df)[str_detect(colnames(inclusion_df), "(dnp|dne)")]),
        collapse = " + "),
  " + ",
  paste(paste0("s(", anthro_cols, ", k=2, by=", anthro_cols, "_dnp)"), collapse = " + "))

mod <- cbind(
  g_pca$scores[, 1:13],
  inclusion_df %>%
    dplyr::filter(pos == "G") %>%
    dplyr::select(tidyselect::ends_with("dnp"), tidyselect::ends_with("dne"),
                  tidyselect::ends_with("coalesce"),
                  setdiff(colnames(nba_combine),
                          c("pos", "weight", "player", "height_w_shoes", "year")),
                  c("last_season", "pos", "sos_weighted", "yo_college", "rsci",
                    "inclusion_dv"))) %>%
  dplyr::filter(last_season < 2020) %>%
  mgcv::gam(as.formula(form), family = binomial(), data = .)

pred_df <- cbind(
  g_pca$scores[, 1:13],
  inclusion_df %>%
    dplyr::filter(pos == "G") %>%
    dplyr::select(tidyselect::ends_with("dnp"), tidyselect::ends_with("dne"),
                  tidyselect::ends_with("coalesce"),
                  setdiff(colnames(nba_combine),
                          c("pos", "weight", "player", "height_w_shoes", "year")),
                  c("last_season", "pos", "sos_weighted", "yo_college", "rsci",
                    "inclusion_dv"))) %>%
  dplyr::filter(last_season > 2019)

guard_inc_preds <- data.frame(
  "pid" = inclusion_df %>% dplyr::filter(pos == "G", last_season > 2019) %>% dplyr::select(pid),
  "pred" = predict(mod, pred_df, type = "response"))

inc_pred_cors <- cbind(
  "pred" = guard_inc_preds$pred,
  cbind(standardized_df, "last_season" = inclusion_df$last_season) %>%
    dplyr::filter(last_season > 2019, pos == "G") %>%
    dplyr::select(-c("pos", "last_season"))) %>%
  cor() %>%
  as.data.frame() %>%
  dplyr::select(pred) %>%
  tibble::rownames_to_column("stat")

set.seed(2045)
N_DRAWS <- 20
inclusion_draws <- sapply(guard_inc_preds$pred, rbinom, size = 1, n = N_DRAWS) %>%
  t() %>%
  as.data.frame() %>%
  setNames(paste0("draw", as.character(seq(1, N_DRAWS)))) %>%
  dplyr::bind_cols(guard_inc_preds, .)

first_nba_seasons <- nba_player %>%
  dplyr::group_by(player) %>%
  dplyr::summarize(year = min(year)) %>%
  dplyr::ungroup()

nba_stats_df <- cbind(
  g_pca$scores[, 1:13],
  ncaa_modeling_df %>%
    dplyr::filter(pos == "G") %>%
    dplyr::select(tidyselect::ends_with("dnp"), tidyselect::ends_with("dne"),
                  tidyselect::ends_with("coalesce"),
                  setdiff(colnames(nba_combine),
                          c("pos", "weight", "player", "height_w_shoes", "year")),
                  c("pid", "last_season", "pos", "sos_weighted", "yo_college", "rsci"))) %>%
  dplyr::mutate(
    # Translate NCAA PID form to NBA PID form
    nba_pid = ifelse(
      # These are players for whom our standard join results in duplicate records,
      # so we have them keep their original NCAA PID
      pid %in% c("tony-mitchell-3", "tony-mitchell-4", "john-lucas-2"),
      pid,
      str_replace(pid, "(-\\d+|--\\d+)", ""))
  ) %>%
  dplyr::inner_join(
    nba_player %>%
      dplyr::inner_join(first_nba_seasons, by = c("player", "year")) %>%
      dplyr::filter(g >= 20) %>%
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
            str_replace_all(., 'á', 'a') %>%
            str_replace_all(., 'ć', 'c') %>%
            str_replace_all(., 'è', 'e')}\\
            -{str_replace_all(tolower(sapply(strsplit(player, ','), '[', 1)), ' ', '-') %>%
            str_replace_all(., \"'\", '') %>%
            str_replace_all(., '\\\\.', '') %>%
            str_replace_all(., 'á', 'a') %>%
            str_replace_all(., 'ć', 'c') %>%
            str_replace_all(., 'è', 'e')}")))
      ) %>%
      dplyr::mutate_at("pid", ~ ifelse(.x %in% names(pid_map),
                                       pid_map[.x],
                                       .x)) %>%
      dplyr::select(-c("pos", "player")),
    by = c("nba_pid" = "pid"),
    suffix = c("_ncaa", "_nba")) %>%
  dplyr::filter(dplyr::between(year - last_season, 0, 2))

pred_stat <- "ast_pct"
form <- paste0(
  pred_stat,
  " ~ ",
  paste(c(paste0("Comp.", 1:18),
          "yo_college", "rsci", "sos_weighted", "height_coalesce", "weight_coalesce",
          colnames(inclusion_df)[str_detect(colnames(inclusion_df), "(dnp|dne)")]),
        collapse = " + "),
  " + ",
  paste(paste0("s(", anthro_cols, ", k=2, by=", anthro_cols, "_dnp)"), collapse = " + "))

fg_pct_mod <- mgcv::gam(as.formula(form), family = gaussian(), data = nba_stats_df)

guard_fg_pct_preds <- data.frame(
  "pid" = inclusion_df %>% dplyr::filter(pos == "G", last_season > 2019) %>% dplyr::select(pid),
  "pred" = predict(fg_pct_mod, pred_df, type = "response")) %>%
  dplyr::mutate_at("pred", ~ ifelse(.x < 0, 0, .x))

set.seed(2050)
fg_pct_dists <- purrr::pmap(list(
  "n" = rowSums(inclusion_draws %>% dplyr::select(tidyselect::starts_with("draw"))),
  "sdlog" = guard_inc_preds$pred,
  "meanlog" = log(guard_fg_pct_preds$pred)),
  rlnorm)

final_dists <- purrr::map_dfr(
  fg_pct_dists,
  ~ c(.x, rep(0, N_DRAWS - length(.x)))) %>%
  t() %>%
  as.data.frame() %>%
  setNames(paste0("draw", as.character(seq(1, N_DRAWS)))) %>%
  cbind(guard_fg_pct_preds, .) %>%
  dplyr::left_join(guard_inc_preds, by = "pid", suffix = c("_stat", "_inc"))

final_dists <- cbind(
  final_dists,
  "avg" = final_dists %>%
    dplyr::select(tidyselect::starts_with("draw")) %>%
    rowMeans())

final_dists %>%
  dplyr::filter(pid %in% c("cameron-thomas", "james-bouknight", "jalen-suggs", "moses-moody")) %>%
  tidyr::pivot_longer(cols = -c("pid", "pred")) %>%
  ggplot() +
  geom_density(aes(x = value, fill = pid), alpha = 0.3)

PCT_ANTHRO_WEIGHTS <- 0.5
PCT_NCAA_WEIGHTS <- 0.4
PCT_COMBINE_WEIGHTS <- 0.1
N_PCA_COMPONENTS <- 13
MATCHING_WEIGHTS <- c(
  rep(PCT_ANTHRO_WEIGHTS / 2, 2),
  rep(PCT_COMBINE_WEIGHTS / length(anthro_cols), length(anthro_cols)),
  (g_pca$sdev[1:N_PCA_COMPONENTS])^2 / sum((g_pca$sdev[1:N_PCA_COMPONENTS])^2) * PCT_NCAA_WEIGHTS
) %>%
  setNames(c("height_coalesce", "weight_coalesce", anthro_cols, paste0("Comp.", 1:N_PCA_COMPONENTS)))

standardized_matching_df <- nba_stats_df %>%
  dplyr::select(names(MATCHING_WEIGHTS)) %>%
  dplyr::bind_rows(., pred_df %>% dplyr::select(names(MATCHING_WEIGHTS))) %>%
  standardize_df_vars() %>%
  cbind(., "pid" = c(nba_stats_df$pid, guard_inc_preds$pid))
standardized_matching_df$pid <- as.character(standardized_matching_df$pid)

get_matching_scores <- function(ncaa_vec, matching_df) {
  nba_df <- matching_df %>%
    dplyr::select(-pid) %>%
    dplyr::slice(1:nrow(nba_stats_df))
  n_nba_rows <- dim(nba_df)[1]
  stacked_row <- t(matrix(rep(as.vector(ncaa_vec), n_nba_rows), ncol = n_nba_rows))
  stacked_matching_weights <- t(matrix(rep(matrix(MATCHING_WEIGHTS), n_nba_rows), ncol = n_nba_rows))
  stacked_sum_matching_weights <- rep(sum(MATCHING_WEIGHTS), n_nba_rows)
  
  scores <- matrix((as.numeric(as.matrix(nba_df)) - as.numeric(stacked_row))^2 * stacked_matching_weights,
                   nrow = n_nba_rows) %>%
    t() %>%
    colSums() / stacked_sum_matching_weights
  weights <- 1 / scores / sum(1 / scores)
  
  return(data.frame("pid" = matching_df$pid[1:nrow(nba_stats_df)],
                    "weight" = weights,
                    stringsAsFactors = FALSE))
}

all_scores <- purrr::pmap(
  standardized_matching_df %>%
    dplyr::select(names(MATCHING_WEIGHTS)) %>%
    dplyr::slice(-(1:nrow(nba_stats_df))),
  list) %>%
  purrr::map(., get_matching_scores, standardized_matching_df) %>%
  purrr::reduce(dplyr::left_join, "pid") %>%
  setNames(c("nba_player", guard_inc_preds$pid))

player_pid <- "keon-johnson-2"
all_scores %>%
  dplyr::select(nba_player, !!sym(player_pid)) %>%
  dplyr::arrange(!!sym(player_pid))

all_scores %>%
  dplyr::select(nba_player, !!sym(player_pid)) %>%
  dplyr::arrange(!!sym(player_pid)) %>%
  ggplot() +
  geom_density(aes(x = !!sym(player_pid)))

ncaa_modeling_df %>%
  dplyr::filter(pid %in% c(player_pid, "nick-johnson-1")) %>%
  View()

# Aggregate predictions using similarity scores
similarity_df <- all_scores %>%
  dplyr::left_join(
    inclusion_df %>%
      dplyr::select(pid, nba_pid),
    by = c("nba_player" = "pid")) %>%
  dplyr::left_join(
    nba_player %>%
      dplyr::inner_join(first_nba_seasons, by = c("player", "year")) %>%
      dplyr::mutate(
        # Create NBA PID from first and last names
        nba_pid = dplyr::case_when(
          # Manually change these three to the NCAA PID we specified above
          player == "Mitchell,Tony" & team_id == "MIL" ~ "tony-mitchell-3",
          player == "Mitchell,Tony" & team_id == "DET" ~ "tony-mitchell-4",
          player == "Lucas III,John" ~ "john-lucas-2",
          TRUE ~ as.character(str_glue(
          "{str_replace_all(tolower(sapply(strsplit(player, ','), '[', 2)), ' ', '-') %>%
            str_replace_all(., \"'\", '') %>%
            str_replace_all(., '\\\\.', '') %>%
            str_replace_all(., 'á', 'a') %>%
            str_replace_all(., 'ć', 'c') %>%
            str_replace_all(., 'è', 'e')}\\
          -{str_replace_all(tolower(sapply(strsplit(player, ','), '[', 1)), ' ', '-') %>%
            str_replace_all(., \"'\", '') %>%
            str_replace_all(., '\\\\.', '') %>%
            str_replace_all(., 'á', 'a') %>%
            str_replace_all(., 'ć', 'c') %>%
            str_replace_all(., 'è', 'e')}")))
      ) %>%
      dplyr::mutate_at("nba_pid", ~ ifelse(.x %in% names(pid_map),
                                           pid_map[.x],
                                           .x)),
    by = "nba_pid"
  ) %>%
  dplyr::filter(g >= 20)

get_nba_aggregates <- function(player_pid, similarity_df, raw_nba_df) {
  similarity_df %>%
    dplyr::select(!!sym(player_pid), colnames(raw_nba_df)) %>%
    dplyr::mutate(weight_ntile = dplyr::ntile(!!sym(player_pid), 10)) %>%
    dplyr::filter(weight_ntile == 10) %>%
    dplyr::mutate(new_weight_total = sum(!!sym(player_pid))) %>%
    dplyr::mutate_at(player_pid, ~ .x / new_weight_total) %>%
    dplyr::summarize_at(c(vars(tidyselect::contains("per_poss")),
                          vars(tidyselect::contains("pct")),
                          vars(tidyselect::starts_with("ws")), "per"),
                        ~ sum(.x * !!sym(player_pid)))
}

player_aggs <- purrr::map_dfr(guard_inc_preds$pid, get_nba_aggregates, similarity_df, nba_player)
player_aggs <- cbind(guard_inc_preds, player_aggs) %>%
  dplyr::rename(inc_pred = pred)

set.seed(2050)
fg_pct_dists <- purrr::pmap(list(
  "n" = rowSums(inclusion_draws %>% dplyr::filter(pid == player_pid) %>% dplyr::select(tidyselect::starts_with("draw"))),
  "sdlog" = player_aggs$inc_pred,
  "meanlog" = log(player_aggs[[pred_stat]])),
  rlnorm)

final_dists <- purrr::map_dfr(
  fg_pct_dists,
  ~ c(.x, rep(0, N_DRAWS - length(.x)))) %>%
  t() %>%
  as.data.frame() %>%
  setNames(paste0("draw", as.character(seq(1, N_DRAWS)))) %>%
  cbind(guard_fg_pct_preds, .) %>%
  dplyr::left_join(guard_inc_preds, by = "pid", suffix = c("_stat", "_inc"))

final_dists <- cbind(
  final_dists,
  "avg" = final_dists %>%
    dplyr::select(tidyselect::starts_with("draw")) %>%
    rowMeans())


PCA_COLS_MATCHING_MULTIPLIER <- 2.5
COMBINE_MEASUREMENTS_MATCHING_MULTIPLIER <- 1
HEIGHT_WEIGHT_MATCHING_MULTIPLIER <- 0.75
sapply(g_pca$sdev, function(x) x / sum(g_pca$sdev))

sds_for_matching <- cbind(
  g_pca$scores[, 1:13] %>%
    as.data.frame() %>%
    dplyr::summarize_all(~ sd(.x) * PCA_COLS_MATCHING_MULTIPLIER),
  ncaa_modeling_df %>%
    dplyr::summarize_at(anthro_cols,
                        ~ sd(.x) * COMBINE_MEASUREMENTS_MATCHING_MULTIPLIER),
  ncaa_modeling_df %>%
    dplyr::summarize_at(c("height_coalesce", "weight_coalesce"),
                        ~ sd(.x) * HEIGHT_WEIGHT_MATCHING_MULTIPLIER))

bucketed_df_for_matching <- purrr::map_dfc(
  c(#colnames(nba_stats_df)[str_detect(colnames(nba_stats_df), "Comp")],
    paste0("Comp.", seq(1:7)),
    anthro_cols, "height_coalesce", "weight_coalesce"),
    ~ cut(nba_stats_df[[.x]],
          unique(c(seq(min(nba_stats_df[[.x]]) - 0.01,
                       max(nba_stats_df[[.x]]) + 0.01,
                       dplyr::pull(sds_for_matching, .x)),
                       max(nba_stats_df[[.x]]) + 0.01)),
          labels = FALSE
          )) %>%
  setNames(c(#colnames(nba_stats_df)[str_detect(colnames(nba_stats_df), "Comp")],
             paste0("Comp.", seq(1:7)),
             anthro_cols, "height_coalesce", "weight_coalesce")) %>%
  cbind("pid" = nba_stats_df$pid, .)

matched_stat_pred <- cbind(
  bucketed_df_for_matching, "stat" = nba_stats_df[[pred_stat]]) %>%
  dplyr::group_by(!!!syms(c(colnames(bucketed_df_for_matching)[str_detect(colnames(bucketed_df_for_matching), "Comp")],
                            "height_coalesce"))) %>%
  dplyr::summarize(n = dplyr::n(),
                   avg_stat = mean(stat)) %>%
  dplyr::ungroup()

purrr::map_dfc(
  c(paste0("Comp.", seq(1:7)),
    anthro_cols, "height_coalesce", "weight_coalesce"),
    ~ cut(pred_df[[.x]],
          unique(c(seq(min(pred_df[[.x]]) - 0.01,
                       max(pred_df[[.x]]) + 0.01,
                       dplyr::pull(sds_for_matching, .x)),
                       max(pred_df[[.x]]) + 0.01)),
          labels = FALSE
          )) %>%
  setNames(c(paste0("Comp.", seq(1:7)),
             anthro_cols, "height_coalesce", "weight_coalesce")) %>%
  cbind("pid" = guard_inc_preds$pid, .) %>%
  dplyr::left_join(matched_stat_pred)

bucketed_df_for_matching %>%
  dplyr::group_by(!!!syms(c(colnames(bucketed_df_for_matching)[str_detect(colnames(bucketed_df_for_matching), "Comp")],
                            "height_coalesce"))) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  View()
bucketed_df_for_matching %>%
  dplyr::group_by(!!!syms(c(colnames(bucketed_df_for_matching)[str_detect(colnames(bucketed_df_for_matching), "Comp")],
                            "height_coalesce"))) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  dplyr::filter(n == 1) %>% dplyr::summarize(s = sum(n))




# args
NFOLDS <- 10
upsampled_samp_prop <- .3 # underrepresented class will comprise {upsampled_samp_prop} * 100% of the sample
n_upsampled_records <- floor(sum(as.numeric(as.character(inc_mod_df$inclusion_dv))) * .8) # {n_upsampled_records} will be in the sample

# params based on args
# Want to maintain even sampling distribution of records, so institute max sample counts
n_downsampled_records <- floor((n_upsampled_records / upsampled_samp_prop) - n_upsampled_records)
max_sample_count_1 <- ceiling(
  NFOLDS * n_upsampled_records /
    nrow(inc_mod_df[inc_mod_df$inclusion_dv == 1,])
)
max_sample_count_0 <- ceiling(
  NFOLDS * n_downsampled_records /
  nrow(inc_mod_df[inc_mod_df$inclusion_dv == 0,])
)
inc_mod_df <- rookies_joined %>%
  dplyr::left_join(valid_mp_seasons_by_pid) %>%
  dplyr::filter(!(trd_season %in% c("2020", "2021")), weight > 0) %>%
  dplyr::mutate(inclusion_dv = ifelse(!is.na(!!sym(dv)), 1, 0),
                pos_ncaa = as.factor(pos_ncaa),
                rsci = factor(rsci, ordered = FALSE),
                index = dplyr::row_number(),
                sample_count = 0) %>%
  # remove columns that cause deficient rank
  dplyr::select(-c("fg2_per_100_sos_weighted_weighted",
                   "fg2a_per_100_sos_weighted_weighted"))

conf_matrices <- list()
for (x in 1:NFOLDS) {
  message(str_glue("Formulating GLM for fold {x}"))

  train_data_1 <- tryCatch(
    dplyr::sample_n(inc_mod_df[inc_mod_df$inclusion_dv == 1 &
                                 inc_mod_df$sample_count < max_sample_count_1, ],
                    size = n_upsampled_records),
    error = function(e) {
      inc_mod_df %>%
        dplyr::filter(inclusion_dv == 1, sample_count < max_sample_count_1)
    })

  train_data_0 <- tryCatch(
    dplyr::sample_n(inc_mod_df[inc_mod_df$inclusion_dv == 0 &
                                 inc_mod_df$sample_count < max_sample_count_0, ],
                    size = n_downsampled_records),
    error = function(e) {
      inc_mod_df %>%
        dplyr::filter(inclusion_dv == 0,sample_count < max_sample_count_0)
    })

  train_data <- dplyr::bind_rows(train_data_0, train_data_1)

  form <- inc_mod_df %>%
    dplyr::select(pos_ncaa, yo_college, height, weight, rsci, height_imputed,
                  weight_imputed, mp_sum, mp_mean,
                  tidyselect::ends_with("_weighted")) %>%
    colnames() %>%
    paste(collapse = " + ") %>%
    paste0("inclusion_dv ~ ", ., " + height:pos_ncaa + weight:pos_ncaa")

  mod <- glm(as.formula(form),
    train_data,
    family = binomial(link = "logit"))


  test_data <- inc_mod_df %>%
    dplyr::filter(!(index %in% train_data$index))

  conf_matrix <- dplyr::bind_cols(
    test_data, "pred" = predict.glm(mod, test_data, type = "response")) %>%
    dplyr::group_by(inclusion_dv) %>%
    dplyr::summarize(pred_0 = sum(ifelse(pred < .5, 1, 0)),
                     pred_1 = sum(ifelse(pred >= .5, 1, 0))) %>%
    dplyr::ungroup()

  conf_matrices[[x]] <- conf_matrix

  inc_mod_df <- inc_mod_df %>%
    dplyr::mutate_at("sample_count", ~ ifelse(index %in% train_data$index, .x + 1, .x))
}

purrr::map_dfr(conf_matrices, dplyr::bind_rows) %>%
  dplyr::mutate_at(c("pred_0", "pred_1"),
                   list("pct" = ~ .x / (pred_0 + pred_1))) %>%
  dplyr::group_by(inclusion_dv) %>%
  dplyr::summarize_all(mean)

rookies_joined %>%
  dplyr::mutate_at(c("pos_ncaa", "rsci"), factor, ordered = F) %>%
  predict(base_inc_model)

make_sampling_weights <- function(df, upweight_val, ntree) {
  df %>%
    dplyr::mutate(
      inc_weight = ifelse(inclusion_dv == 1,
                          upweight_val,
                          1)
    ) %>%
    dplyr::pull(inc_weight) %>%
    rep(ntree) %>%
    matrix(nrow = nrow(df), ncol = ntree)
}

btstrap_weights_500 <- make_sampling_weights(inc_mod_df, 20.59515, 500)
btstrap_weights_1k <- make_sampling_weights(inc_mod_df, 20.59515, 1000)

inc_mod <- randomForestSRC::rfsrc(inclusion_dv ~ .,
                                  as.data.frame(inc_mod_df),
                                  mtry = floor(.2 * (ncol(inc_mod_df) - 1)),
                                  splitrule = "random",
                                  samp = btstrap_weights_500,
                                  ntree = 500)
inc_mod_1k <- randomForestSRC::rfsrc(inclusion_dv ~ .,
                                  as.data.frame(inc_mod_df),
                                  mtry = floor(.2 * (ncol(inc_mod_df) - 1)),
                                  splitrule = "random",
                                  samp = btstrap_weights_1k,
                                  ntree = 1000)
inc_mod_default_btstrap <- randomForestSRC::rfsrc(inclusion_dv ~ .,
                                  as.data.frame(inc_mod_df),
                                  mtry = floor(.2 * (ncol(inc_mod_df) - 1)),
                                  splitrule = "random",
                                  ntree = 500)


rookies_joined %>%
  dplyr::left_join(valid_mp_seasons_by_pid) %>%
  dplyr::bind_cols("inclusion_prob" = inc_mod$predicted.oob[,2]) %>%
  #dplyr::filter(trd_season == 2021) %>%
  dplyr::select(pid, inclusion_prob, yo_college, nba_pid, pts_per_poss) %>%
  dplyr::arrange(desc(inclusion_prob)) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  View()

# Survival Models
new_bio_impute_cols <- c(bio_impute_cols[bio_impute_cols != "pos_ncaa"], "pos")
weighted_avgs %>%
  # First time period is college
  dplyr::select(pid, height, weight, yo_college, pos, rsci, mp_sum, mp_mean,
                mp_per_g_mean, tidyselect::matches("_weighted_weighted")) %>%
  impute_bio_info("weight",
                  new_bio_impute_cols,
                  c("pos")) %>%
  impute_bio_info( "height",
                  new_bio_impute_cols,
                  c("pos")) %>%
  dplyr::filter(!is.na(pos)) %>%
  dplyr::filter(!is.na(mp_per_g_mean)) %>%
  dplyr::mutate(time = 0, status = 1) %>%
  dplyr::union_all(
    rookies_joined %>%
      dplyr::filter(!is.na(ts_pct)) %>%
      dplyr::select(pid, height, weight, yo_college, pos_ncaa, rsci, mp_sum, mp_mean,
                    mp_per_g_mean, tidyselect::matches("_weighted_weighted"),
                    weight, weight_imputed, height, height_imputed) %>%
      dplyr::rename(pos = pos_ncaa) %>%
      dplyr::mutate(time = 1, status = 1)
  )

surv_df <- rookies_joined %>%
  dplyr::mutate(pos_ncaa = as.factor(pos_ncaa),
                year = 1,
                in_nba = ifelse(is.na(ts_pct), 1, 0)) %>%
  dplyr::select(height, weight, yo_college, pos_ncaa, rsci, mp_sum, mp_mean,
                mp_per_g_mean, tidyselect::matches("_weighted_weighted"),
                weight, weight_imputed, height, height_imputed,
                year, in_nba) %>%
  data.frame()

surv_mod <-  rfsrc(Surv(year, in_nba) ~ ., surv_df, splitrule = "random")

rookies_joined %>%
  dplyr::left_join(valid_mp_seasons_by_pid) %>%
  dplyr::bind_cols("p_not_in_nba" = surv_mod$predicted.oob) %>%
  #dplyr::filter(trd_season == 2021) %>%
  dplyr::select(pid, p_not_in_nba, yo_college, nba_pid, pts_per_poss) %>%
  dplyr::arrange(p_not_in_nba) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  View()

nba_fst_seasons <- nba_player %>%
  dplyr::left_join(fst_seasons) %>%
  dplyr::filter(year == fst_year)

stat <- "ast_per_poss"
group_cols <- c("pos")
wide <- nba_player %>%
  dplyr::select(year, player, !!!syms(c(stat, group_cols))) %>%
  dplyr::arrange(player, year)

if ("pos" %in% group_cols) {
  wide <- wide %>% dplyr::left_join(
    data.frame(
      "player" = wide %>%
        dplyr::pull(player) %>%
        unique(),
      "all_pos" = wide %>%
        dplyr::mutate_at("pos", ~ str_replace_all(.x, "-", ",")) %>%
        dplyr::group_by(player) %>%
        dplyr::summarize(all_pos = strsplit(paste(pos, collapse = ","), ",")) %>%
        dplyr::pull(all_pos) %>%
        lapply(function(x) {paste(sort(unique(x)), collapse = ",")}) %>%
        rlang::flatten_chr()
    ),
    by = "player")
  group_cols <- c(group_cols[group_cols != "pos"], "all_pos")
}

wide <- wide %>%
  dplyr::group_by(player) %>%
  dplyr::mutate(year_num = row_number()) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(id_cols = c("player", group_cols),
                     names_from = "year_num",
                     names_prefix = "year_",
                     values_from = stat)

year_cols <- tidyselect::vars_select(colnames(wide),
                                     tidyselect::contains("year"))
diffs <- wide %>%
  dplyr::select(player, !!!syms(group_cols))
for (col in year_cols) {
  col_num <- as.numeric(str_extract(col, "(?<=year_)[0-9]+"))
  prev_year_col <- paste0("year_", col_num - 1)
  if ((col_num == 1) |
    (col_num == max(as.numeric(str_extract(year_cols, "(?<=year_)[0-9]+"))))) {
    diffs[, col] <- wide[, col]
  } else {
    diffs[, col] <- wide[,col] - wide[,prev_year_col]
  }
}

diffs %>%
  dplyr::group_by(!!!syms(group_cols)) %>%
  dplyr::summarize_at(year_cols, mean, na.rm = TRUE) %>%
  dplyr::summarize_at(year_cols, list("mean" = ~ mean(.x, na.rm = TRUE),
                                      "count" = ~ sum(ifelse(!is.na(.x), 1, 0)))) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(cols = -group_cols, names_to = "stat", values_to = "value") %>%
  dplyr::mutate(year = str_extract(stat, "[\\d]+"))
  dplyr::mutate_at("stat", ~ as.numeric(str_replace(.x, "year_", ""))) %>%
  dplyr::rename(year="stat", diff="value") %>%
  ggplot(aes(x = year, y = diff, group = all_pos)) +
  geom_line(aes(color = all_pos))


nba_player %>%
  dplyr::group_by(player, age) %>%
  dplyr::tally() %>%
  dplyr::filter(n > 1)
  dplyr::select(player, age, year, fg_per_poss) %>%
  dplyr::group_by(player, age) %>%
  dplyr::mutate(year_num = min_rank(year)) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(player, year_num = factor(year_num), fg_per_poss) %>%
  tidyr::pivot_wider(names_from = "year_num", values_from = "fg_per_poss")
