library(dplyr)
library(stringr)
library(magrittr)
library(rlang)
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
career_means <- adjusted_with_weights %>%
  dplyr::group_by(!!!syms(groupby_cols)) %>%
  dplyr::summarize_at(
    mean_cols,
    list(mean = ~ mean(.x, na.rm = T))
  ) %>%
  dplyr::ungroup()

career_weighted_avgs <- adjusted_with_weights %>%
  dplyr::group_by(!!!syms(groupby_cols)) %>%
  dplyr::summarize_at(
    vars(mean_cols),
    list(weighted = ~ sum(season_weight * .x, na.rm = T) /
           sum(season_weight, na.rm = T))
  ) %>%
  dplyr::ungroup()

ncaa_modeling_df <- career_sums %>%
  dplyr::inner_join(career_means, by = groupby_cols) %>%
  dplyr::inner_join(career_weighted_avgs, by = groupby_cols)

tidyselect::poke_vars(NULL)

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

  return((df - means_matrix) / sds_matrix)
}
g_pca <- inclusion_df %>%
  dplyr::filter(pos == "G") %>%
  dplyr::select(tidyselect::ends_with("sum"), tidyselect::ends_with("mean"),
                tidyselect::ends_with("weighted")) %>%
  standardize_df_vars() %>%
  princomp()
matrix(c(rep(1, 8), rep(0, 8)), ncol = 4) - matrix(rep(c(1, 1, 0, 0), 4), nrow = 4)
g_pca$scores[, 0:4]

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
                    height_imputed + weight_imputed - 1, .),
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


