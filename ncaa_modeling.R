library(dplyr)
library(stringr)
library(magrittr)
library(randomForestSRC)
library(survival)
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
    vars("sos", "mp_per_g", "mp", "usg_pct", "ft_pct", "fta_per_fga_pct", "fg3a_per_fga_pct",
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
  "jeff-sheppard" = "jeffrey-sheppard",
  "michael-porter" = "michael-porterjr"
)

rookies_joined <- weighted_avgs %>%
  # Only keep columns we want to model with
  dplyr::select(pid, height, weight, yo_college, pos, rsci, sos_mean, mp_sum, mp_mean,
                mp_per_g_mean, tidyselect::ends_with("_weighted")) %>%
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
            str_replace_all(., 'á', 'a') %>%
            str_replace_all(., 'è', 'e')}\\
          -{str_replace_all(tolower(sapply(strsplit(player, ','), '[', 1)), ' ', '-') %>%
            str_replace_all(., \"'\", '') %>%
            str_replace_all(., '\\\\.', '') %>%
            str_replace_all(., 'á', 'a') %>%
            str_replace_all(., 'è', 'e')}")))
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
  dplyr::mutate(inclusion_dv = as.factor(ifelse(!is.na(!!sym(dv)), 1, 0)),
                pos_ncaa = as.factor(pos_ncaa)) %>%
  dplyr::select(c(dplyr::intersect(
                    dplyr::setdiff(colnames(weighted_avgs), c("pid")),
                                   colnames(rookies_joined)),
                  "pos_ncaa", "height_imputed", "weight_imputed",
                  "inclusion_dv"))

# Base Logistic Regression
base_inc_model <- glmnet::cv.glmnet(
  x = inc_mod_df %>%
    dplyr::mutate(rsci = factor(rsci, ordered = FALSE)) %>%
    model.matrix(inclusion_dv ~ pos_ncaa + yo_college + height + weight +
                    rsci + height:pos_ncaa + weight:pos_ncaa +
                    height_imputed + weight_imputed, .),
  y = as.numeric(as.character(inc_mod_df$inclusion_dv)) %>% as.matrix())

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

k <- 20
n_obs <- nrow(nba_fst_seasons)
.init_centers <- runif(k)
centers <- rep(.init_centers, n_obs) %>%
  sort() %>%
  matrix(nrow = n_obs, ncol = k) %>%
  as_data_frame() %>%
  setNames(paste0("center_", seq(1, k)))
.cluster_df <- nba_fst_seasons %>%
  dplyr::select(pts_per_poss, fga) %>%
  dplyr::bind_cols(centers)
.cluster_df %>%
  dplyr::m