library(dplyr)
library(glue)
library(stringr)
library(rlang)
library(glmnet)
library(ggplot2)

read <- read.csv("~/Downloads/summary_112.csv")

# Get scraped data
stats <- read.csv("~/Desktop/player_stats.csv")

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


