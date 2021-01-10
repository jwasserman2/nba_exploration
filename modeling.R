library(dplyr)
library(glue)
library(stringr)
library(rlang)
library(glmnet)
library(ggplot2)

college <- readr::read_csv("~/Desktop/college_stats.csv") %>%
  dplyr::select(-c("X1"))

# Get scraped data
stats <- readr::read_csv("~/Desktop/player_stats.csv") %>%
  dplyr::select(-c("X1"))

# Get scrape MP
mps <- readr::read_csv("~/Desktop/scraped_missing_mps.csv") %>%
  dplyr::select(-c("X1"))

# Get school strength
school <- readr::read_csv("~/Desktop/school_strength.csv")

# Custom colors for viz
darkblue <- "#191970"
lightblue <- "#1E90FF"
gold <- "#DAA520"
orange <- "#FF8C00"
green <- "#228B22"
purple <- "#4B0082"
red <- "#B22222"

# Utils
make_factor_dummy_matrix <- function(df, factor_col) {
  # Pull the factor column
  new_df <- df %>% dplyr::select(!!sym(factor_col))

  # Make 0/1 columns for each individual level of the factor column
  for (lvl in forcats::lvls_union(new_df[factor_col])) {
    new_df <- new_df %>%
      dplyr::mutate(
        !!sym(glue("{factor_col}_is_{lvl}")) := as.factor(ifelse(
          !!sym(factor_col) == lvl,
          1,
          0
        ))
      )
  }

  new_df <- new_df %>% dplyr::select(-!!sym(factor_col))
  out <- cbind(df, new_df) %>%
    dplyr::select(-!!sym(factor_col))

  return(out)
}

get_missing_values <- function(df){
  df %>%
    dplyr::mutate_all(function(x){
      dplyr::case_when(
        is.na(x) | x == "" | x == " " ~ 1,
        TRUE ~ 0
      )}
    ) %>%
    summarize_all(sum)
}

group_get_missing_values <- function(df, stat) {
  df %>%
    dplyr::group_by(season, college_id) %>%
    dplyr::summarize(count = sum(dplyr::case_when(is.na(!!sym(stat)) ~ 1,
                                                  TRUE ~ 0)),
                     total = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(season)) %>%
    as.data.frame() %>%
    dplyr::filter(count > 0)
}

# Deal with NA's
na_count <- get_missing_values(college)
na_3pa <- group_get_missing_values(college, "fg3a")

some_have_3s <- na_3pa %>%
  dplyr::filter(count < total)

munged <- college %>%
  dplyr::left_join(some_have_3s) %>%
  dplyr::mutate_at(
    c("fg3", "fg3a"),
    ~ dplyr::case_when(is.na(.x) & count < total & !is.na(total) ~ 0,
                       is.na(.x) & as.numeric(substr(season, 1, 4)) > 1985 &
                         !(college_id %in% c("MOREHOUSE", "MIDLANDCC", "TAMPA")) ~ 0,
                       TRUE ~ .x)) %>%
  dplyr::select(!tidyselect::any_of(c("count", "total")))

na_count <- get_missing_values(munged)

na_mp <- group_get_missing_values(college, "mp")
na_mp_players <- college %>%
  dplyr::filter(is.na(mp)) %>%
  dplyr::distinct(player_id)
write.csv(na_mp_players, "~/Desktop/missing_mp_players.csv")

munged %<>%
  dplyr::left_join(mps, by = c("player_id", "season")) %>%
  dplyr::mutate(mp = dplyr::coalesce(mp.x, mp.y),
                mp_per_g = dplyr::coalesce(mp_per_g, mp / g)) %>%
  dplyr::select(-c("mp.x", "mp.y"))

# Add college type var
munged %<>%
  dplyr::left_join(school, by = c("college_id" = "School")) %>%
  dplyr::mutate_at(c("SRS", "SOS"), ~ ifelse(is.na(.x), min(.x), .x))

# Standardize + interact with college rating
standardized_ind_rows <- munged %>%
  dplyr::mutate_at(vars(-c("player_id", "season", "age", "college_id")),
                   ~ (.x - mean(.x, na.rm = T)) / sd(.x, na.rm = T)) %>%
  dplyr::mutate_at(vars(-c("player_id", "season", "age", "college_id")),
                   list("srs" = ~ SRS * .))

max_season <- munged %>%
  dplyr::group_by(player_id) %>%
  dplyr::mutate(season_num = dplyr::row_number()) %>%
  dplyr::filter(season_num == max(season_num)) %>%
  dplyr::ungroup()

ncaa_avgs <- munged %>%
  dplyr::select(-c("season", "age", "college_id")) %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarize_all(mean, na.rm = T) %>%
  dplyr::ungroup()

standardized_grouped <- ncaa_avgs %>%
  dplyr::left_join(max_season, by = "player_id",
                   suffix = c("_avg", "_fseason")) %>%
  dplyr::select(-c(paste0(colnames(school)[!(colnames(school) %in% c("SRS", "School"))],
                          "_avg"),
                   paste0(colnames(school)[!(colnames(school) %in% c("SRS", "School"))],
                          "_fseason"))) %>%
  dplyr::mutate_at(vars(-c("player_id", "season", "age", "college_id", "season_num")),
                   ~ (.x - mean(.x, na.rm = T)) / sd(.x, na.rm = T)) %>%
  dplyr::mutate_at(vars(-c("player_id", "season", "age", "college_id", "season_num")),
                   list("srs" = ~ SRS_avg * .))

# Cor
stat <- "fg3_per_g"
standardized_grouped %>%
  dplyr::left_join(stats %>%
                     dplyr::group_by(player) %>%
                     dplyr::mutate(season = dplyr::row_number()) %>%
                     dplyr::ungroup() %>%
                     dplyr::filter(season == 1) %>%
                     dplyr::select(player, !!sym(stat)),
                   by = c("player_id" = "player")) %>%
  dplyr::select_if(is.numeric) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  dplyr::select(!!sym(stat)) %>%
  tibble::rownames_to_column() %>%
  dplyr::arrange(!!sym(stat))

# GLM
ppg_covs <- standardized_grouped %>%
  dplyr::select(tidyselect::matches("(SRS_|pts|fg|ft|mp|age|season_num)")) %>%
  colnames()
fg3_covs <- standardized_grouped %>%
  dplyr::select(tidyselect::matches("(ast|stl|blk)")) %>%
  colnames() %>%
  c(., ppg_covs)

X <- standardized_grouped %>%
  dplyr::filter(!(player_id %in% c("alexaky01", "bolbo01", "cacokde01"))) %>%
  dplyr::select(-tidyselect::matches("(orb|tov|pf)")) %>%
  dplyr::select(-c("player_id", "season", "college_id")) %>%
  dplyr::select(fg3_covs)

y <- standardized_grouped %>%
  dplyr::filter(!(player_id %in% c("alexaky01", "bolbo01", "cacokde01"))) %>%
  dplyr::distinct(player_id) %>%
  dplyr::inner_join(stats %>%
                      dplyr::group_by(player) %>%
                      dplyr::mutate(season = dplyr::row_number()) %>%
                      dplyr::ungroup() %>%
                      dplyr::filter(season == 1),
                    by = c("player_id" = "player")) %>%
  dplyr::pull(stat)

final <- cbind(X, y) %>%
  dplyr::filter_all(all_vars(!is.na(.)))

folds <- standardized_grouped %>%
  dplyr::filter(!(player_id %in% c("alexaky01", "bolbo01", "cacokde01"))) %>%
  dplyr::select(-tidyselect::matches("(orb|tov|pf)")) %>%
  dplyr::filter_at(fg3_covs, all_vars(!is.na(.))) %>%
  dplyr::mutate(
    fold = as.numeric(factor(season,
                  levels = sort(unique(.$season)),
                  labels = 1:length(unique(.$season))))
  ) %>%
  dplyr::pull(fold)

glm <- glmnet::cv.glmnet(
  as.matrix(final %>% dplyr::select(-y)),
  final$y,
  type.measure = "mse",
  foldid = folds
)

preds <- predict(glm, as.matrix(final %>% dplyr::select(-y)),
                 lambda = glm$lambda.min)
glm_comb_preds <- standardized_grouped %>%
  dplyr::filter(!(player_id %in% c("alexaky01", "bolbo01", "cacokde01"))) %>%
  dplyr::distinct(player_id, .keep_all = TRUE) %>%
  dplyr::inner_join(stats %>%
                      dplyr::group_by(player) %>%
                      dplyr::mutate(season = dplyr::row_number()) %>%
                      dplyr::ungroup() %>%
                      dplyr::filter(season == 1) %>%
                      dplyr::select(player, y = !!sym(stat)),
                    by = c("player_id" = "player")) %>%
  filter_at(colnames(final), all_vars(!is.na(.))) %>%
  cbind(., preds, folds) %>%
  setNames(c(colnames(standardized_grouped),  "y", "pred", "fold")) %>%
  dplyr::arrange(desc(pred)) %>%
  dplyr::select(player_id, season, y, pred, fold)

ggplot(glm_comb_preds, aes(x = y, y = pred)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  annotate(
    "text",  x = 2, y = 5,
    label = round(cor(glm_comb_preds$y, glm_comb_preds$pred), 2))
# Make lagged variables
new_vars <- stats %>%
  dplyr::group_by(player) %>%
  dplyr::arrange(age) %>%
  dplyr::mutate_at(
    vars(tidyselect::matches("_per_g|_pct")),
    .funs = list( "minus_1" = ~ dplyr::lag(.),
                  "minus_2" = ~ dplyr::lag(., n = 2L),
                  "minus_3" = ~ dplyr::lag(., n = 3L),
                  "minus_4" = ~ dplyr::lag(., n = 4L))
  ) %>%
  dplyr::ungroup()

# See how many observations we'll be working with for each lagged stat
n_lagged_obs <- new_vars %>%
  dplyr::select(tidyselect::matches("^fg_pct")) %>%
  dplyr::mutate_all(~ dplyr::case_when(
    is.na(.x) ~ 0,
    TRUE ~ 1
  )) %>%
  dplyr::summarize_all(sum)

# Make season approximation by comparing age to min age (since data starts in
# 1980 and players weren't necessarily rookies then we set this to NULL for
# anyone over the age of 23 before 1993)
min_ages <- new_vars %>%
  dplyr::group_by(player) %>%
  dplyr::summarize(min_age = min(age)) %>%
  dplyr::ungroup()

new_vars <- new_vars %>%
  dplyr::left_join(min_ages, by = "player") %>%
  dplyr::mutate(
    season = dplyr::case_when(
      min_age <= 23 | year > 1993 ~ age - min_age + 1,
      TRUE ~ NA_real_
    )
  )

# Correlation Viz
make_cor_scatter <- function(df, stat) {
  use <- df %>%
    dplyr::select(tidyselect::matches(stat)) %>%
    tidyr::pivot_longer(cols = -c(stat),
                        names_to = "lag",
                        values_to = "value") %>%
    dplyr::filter(!!sym(stat) != 0, value != 0)

  if (str_detect(stat, "pct")) {
    use <- use %>%
      dplyr::filter(!!sym(stat) != 1, value != 1)
  }

  corr <- use %>%
    dplyr::group_by(lag) %>%
    dplyr::summarize(label = cor(!!sym(stat), value),
                     x = .1 * max(value, na.rm = T),
                     y = .95 * max(!!sym(stat), na.rm = T)) %>%
    dplyr::ungroup()

  ggplot(use, aes(x = value, y = !!sym(stat))) +
    geom_jitter() +
    geom_abline(aes(slope = 1, intercept = 0),
                linetype = "dashed", size = 1, color = red) +
    facet_wrap(. ~ lag) +
    geom_text(
      data = corr,
      aes(x = x, y = y, label = glue("Corr: {round(label * 100, 1)}%")),
      hjust = 0) +
    labs(subtitle = glue(.sep = "\n",
                         "Seasons with stat = 0 (or = 1 if a % stat) removed",
                         "Identity line dashed in red"))
}

make_cor_scatter(new_vars, "pts_per_g")
make_cor_scatter(new_vars, "fg_pct")
make_cor_scatter(new_vars, "tov_per_g")
make_cor_scatter(new_vars, "fg3_pct")
make_cor_scatter(new_vars, "mp_per_g")
make_cor_scatter(new_vars, "ft_pct")

draft_age_data <- new_vars %>%
  dplyr::group_by(season, min_age) %>%
  dplyr::summarize(mean_pts = median(pts_per_g),
                   n = n(),
                   sd = sd(pts_per_g)) %>%
  dplyr::ungroup()

draft_age_data %>%
  dplyr::filter(!is.na(season), season < 16, min_age < 25) %>%
  ggplot() +
  geom_line(aes(x = season, y = mean_pts, group = min_age,
                color = as.factor(min_age)), size = 2) +
  # geom_point(aes(x = season, y = mean_pts,
  #                color = factor(min_age, levels = seq(18, 25, 1)),
  #            size = n)) +
  scale_x_continuous(breaks = 1:20) +
  scale_color_brewer(palette = "Blues", direction = -1) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Ridge Model
run_ridge_regression <- function(df, stat, nfolds, age_as_factor = TRUE) {
  # Create age * lag interaction variables, as well as unique ID for each row
  # and a "sampled" column we'll use to ensure every row is left out exactly
  # once

  if (age_as_factor) {
    age_factors <- df %>%
      dplyr::select(tidyselect::matches("_is_")) %>%
      colnames()

    training_data <- df %>%
      dplyr::filter(g > 10) %>%
      dplyr::mutate_at(
        vars(tidyselect::starts_with(glue("{stat}_"))),
        .funs = purrr::map(
          age_factors,
          ~ as.formula(glue("~ dplyr::case_when(
                            {.x} == 1 ~ ., TRUE ~ 0)"))
        ) %>%
          setNames(age_factors)) %>%
      dplyr::filter_all(all_vars(!is.na(.))) %>%
      dplyr::mutate(
        sampled = 0,
        row_num = dplyr::row_number())

    select_clause <- glue("{stat}.|is_")
  } else {
    training_data <- df %>%
      dplyr::filter(g > 10) %>%
      dplyr::mutate_at(
        vars(tidyselect::starts_with(glue("{stat}."))),
        .funs = list("age" = ~ age * .)
        ) %>%
      dplyr::filter_all(all_vars(!is.na(.))) %>%
      dplyr::mutate(
        sampled = 0,
        row_num = dplyr::row_number())

    select_clause <- glue("{stat}.")
  }

  # Run NFold Cross-Validation where we output predicted values and coefficients
  FOLDS <- nfolds
  preds <- tibble::tibble()
  coeffs <- tibble::tibble()
  for (i in 1:FOLDS) {
    message(glue("Modeling fold {i}..."))
    nrows <- ifelse(
      nrow(training_data[training_data$sampled == 0,]) >
        nrow(training_data) / FOLDS,
      nrow(training_data) / FOLDS,
      nrow(training_data[training_data$sampled == 0,])
      )

    test_rows <- sample(
      training_data$row_num[training_data$sampled == 0],
      size = ceiling(nrows)
    )

    # Make training and testing data based off random sample
    X_train <- training_data %>%
      dplyr::filter(!(row_num %in% test_rows)) %>%
      dplyr::select(tidyselect::matches(select_clause))

    y <- training_data %>%
      dplyr::filter(!(row_num %in% test_rows)) %>%
      dplyr::pull(!!sym(stat))

    X_test <- training_data %>%
      dplyr::filter(row_num %in% test_rows) %>%
      dplyr::select(tidyselect::matches(select_clause))

    # Model on training data
    model <- cv.glmnet(
      x = data.matrix(X_train),
      y = as.matrix(y),
      type.measure = "mse"
    )

    # Make out of sample predictions and join to training data
    fold_preds <- predict(
      model,
      data.matrix(X_test)
    )

    attach <- training_data %>%
      dplyr::filter(row_num %in% test_rows) %>%
      cbind(., fold_preds) %>%
      setNames(c(colnames(training_data), "pred"))

    preds <- rbind(preds, attach)

    # Store coefficients
    fold_coeffs <- broom::tidy(coef(model)) %>%
      dplyr::transmute(
        variable = row,
        fold = i,
        coef = value
      )

    coeffs <- rbind(coeffs, fold_coeffs)


    # Set "sampled" == 1 for test rows in this fold
    training_data$sampled[training_data$row_num %in% test_rows] <- 1

  }

  return(list(
    preds = preds,
    coeffs = coeffs
  ))
}

# Original Regression
pts_ridge <- run_ridge_regression(new_vars, "pts_per_g", 5, FALSE)
preds <- pts_ridge$preds
mse <- mean((preds$pred - preds$pts_per_g)^2)
corr <- cor(preds$pred, preds$pts_per_g)

ggplot(preds) +
  geom_boxplot(aes(x = age, y = (pred - pts_per_g)^2, group = age))

# Age as factor regression
factor_age <- new_vars %>%
  dplyr::mutate_at("age", ~ as.factor(.x)) %>%
  make_factor_dummy_matrix("age")
pts_ridge_factor_age <- run_ridge_regression(factor_age, "pts_per_g", 5)
factor_age_preds <- pts_ridge_factor_age$preds
factor_age_mse <- mean((factor_age_preds$pred - factor_age_preds$pts_per_g)^2)
factor_age_mse_by_age <- factor_age_preds %>%
  dplyr::group_by(age_is_30) %>%
  dplyr::summarize(mse = mean((pred - pts_per_g) ^2),
                   avg = mean(pts_per_g)) %>%
  as.data.frame()

# Only one-lag regression
one_lag_only <- run_ridge_regression(
  new_vars %>%
    dplyr::select(-c("pts_per_g_minus_2",
                     "pts_per_g_minus_3")),
  "pts_per_g",
  5,
  TRUE)
one_lag_only_preds <- one_lag_only$preds
one_lag_only_mse <- mean((one_lag_only_preds$pred - one_lag_only_preds$pts_per_g)^2)
one_lag_only_mse_by_age <- one_lag_only_preds %>%
  dplyr::group_by(age) %>%
  dplyr::summarize(mse = mean((pred - pts_per_g) ^2),
                   avg = mean(pts_per_g)) %>%
  as.data.frame()

# Draft age as factor regression
draft_age_included <- factor_age %>%
  dplyr::mutate_at("min_age", ~ as.factor(.x)) %>%
  make_factor_dummy_matrix("min_age") %>%
  dplyr::select(-tidyselect::matches("minus_4"))
draft_age_reg <- run_ridge_regression(draft_age_included, "fg_pct", 5, TRUE)
draft_age_preds <- draft_age_reg$preds
draft_age_reg_mse <- mean((draft_age_preds$pred - draft_age_preds$fg_pct)^2)

# Spline Regression
reg <- new_vars %>%
  dplyr::mutate_at(c("season", "age", "min_age"), ~ as.factor(.x)) %>%
  mgcv::gam(fg_pct ~ age + season +
              fg_pct_minus_1 + fg_pct_minus_2 + fg_pct_minus_3 +
              fg_pct_minus_4 +
              s(fg_pct_minus_1, age, bs = "re") +
              s(fg_pct_minus_1, season, bs = "re") +
              s(fg_pct_minus_2, age, bs = "re") +
              s(fg_pct_minus_2, season, bs = "re"),
            data = .)
base_reg <- glm(fg_pct ~ age + season + fg_pct_minus_1 + fg_pct_minus_2 +
                  fg_pct_minus_3 + fg_pct_minus_4 + min_age,
                data = new_vars %>%
                  dplyr::mutate_at(c("season", "age", "min_age"),
                                   ~ as.factor(.x)))

# Stan

# Predicted values viz
actual_vs_pred <- draft_age_preds %>%
  dplyr::select(row_num, pred, fg_pct) %>%
  tidyr::pivot_longer(c("pred", "fg_pct")) %>%
  ggplot() +
  geom_density(aes(x = value, fill = name), alpha = .6)

errors <- draft_age_preds %>%
  dplyr::transmute(error = pred - fg_pct) %>%
  ggplot() +
  geom_density(aes(x = error), fill = green, color = green, alpha = .7)

mse_by_age <- stats %>%
  dplyr::inner_join(
    draft_age_preds %>%
      dplyr::select(player, year, pred),
    by = c("player", "year")) %>%
  dplyr::group_by(age) %>%
  dplyr::summarize(mse = mean((pred - fg_pct) ^2),
                   avg = mean(fg_pct)) %>%
  as.data.frame()

### COLLEGE WORK
mutated <- college %>%
  dplyr::group_by(player_id) %>%
  dplyr::arrange(age) %>%
  dplyr::mutate(
    season_num = dplyr::row_number(),
    max_season = max(season_num),
    final_season = ifelse(season_num == max_season, TRUE, FALSE)
  ) %>%
  dplyr::ungroup()

final_season_pcts <- stats %>%
  dplyr::left_join(
    mutated %>%
      dplyr::filter(final_season == TRUE),
    by = c("player" = "player_id"),
    suffix = c("_pro", "_ncaa")
  ) %>%
  dplyr::select(pos, fg3, fg3a, fg_pct_ncaa, fg, ft_pct_ncaa,
                fg3_pct_ncaa, fg3_pct_pro) %>%
  dplyr::filter(pos %in% c("PG", "SG", "SF")) %>%
  dplyr::filter_all(all_vars(!is.na(.))) %>%
  dplyr::select(-pos) %>%
  dplyr::mutate_all(~ (.x - mean(.x)) / sd(.x))

coefs <- purrr::map_dfr(
  colnames(final_season_pcts)[colnames(final_season_pcts) != "fg3_pct_pro"],
  function(x) {
    formula <- as.formula(glue("fg3_pct_pro ~ {x}"))
    reg <- lm(formula, data = final_season_pcts)
    broom::tidy(reg) %>%
      dplyr::transmute(
        var = x,
        term,
        estimate)
  }
)

final_season_pcts %>%
  tidyr::pivot_longer(
    cols = -c("fg3_pct_pro"),
    names_to = "stat",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_point(aes(x = value, y = fg3_pct_pro, color = stat)) +
  geom_abline(
    data = coefs %>%
      dplyr::filter(term == "(Intercept)") %>%
      dplyr::left_join(
        coefs %>%
          dplyr::filter(term != "(Intercept)"),
        by = "var"
      ) %>%
      dplyr::transmute(
        var,
        "Intercept" = estimate.x,
        "slope" = estimate.y
      ),
    aes(intercept = Intercept, slope = slope, color = var))

stats %>%
  dplyr::left_join(
    mutated %>%
      dplyr::filter(final_season == TRUE),
    by = c("player" = "player_id"),
    suffix = c("_pro", "_ncaa")
  ) %>%
  dplyr::arrange(desc(fg3_pct_pro)) %>%
  dplyr::select(player, year, age_ncaa, fg3_pct_ncaa, fg3_pct_pro)

glmnet_data <- stats %>%
  dplyr::left_join(
    mutated %>%
      dplyr::filter(final_season == TRUE),
    by = c("player" = "player_id"),
    suffix = c("_pro", "_ncaa")
  ) %>%
  dplyr::select(fg3, fg3a, fg_pct_ncaa, fg, ft_pct_ncaa,
                fg3_pct_ncaa, fg3_pct_pro) %>%
  dplyr::filter_all(all_vars(!is.na(.)))

model <- cv.glmnet(
  x = glmnet_data %>% dplyr::select(-fg3_pct_pro) %>% as.matrix(),
  y = glmnet_data %>% dplyr::pull(fg3_pct_pro))
coef(model, model$lambda.1se)
