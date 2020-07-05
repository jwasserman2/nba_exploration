library(dplyr)
library(glue)
library(stringr)
library(rlang)
library(glmnet)
library(ggplot2)


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

# Ridge Model
run_ridge_regression <- function(df, stat, nfolds, age_as_factor = TRUE) {
  # Create age * lag interaction variables, as well as unique ID for each row
  # and a "sampled" column we'll use to ensure every row is left out exactly
  # once

  if (age_as_factor) {
    age_factors <- df %>%
      dplyr::select(tidyselect::matches("^is_")) %>%
      colnames()

    training_data <- df %>%
      dplyr::filter(g > 10) %>%
      dplyr::mutate_at(
        vars(tidyselect::matches(glue("{stat}."))),
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
        vars(tidyselect::matches(glue("{stat}."))),
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

pts_ridge <- run_ridge_regression(new_vars, "pts_per_g", 5, FALSE)
preds <- pts_ridge$preds
mse <- mean((preds$pred - preds$pts_per_g)^2)
corr <- cor(preds$pred, preds$pts_per_g)

ggplot(preds) +
  geom_boxplot(aes(x = age, y = (pred - pts_per_g)^2, group = age))

factor_age <- new_vars %>%
  dplyr::mutate_at("age", ~ as.factor(.x)) %>%
  make_factor_dummy_matrix("age")
pts_ridge_factor_age <- run_ridge_regression(factor_age, "pts_per_g", 5)
factor_age_preds <- pts_ridge_factor_age$preds
factor_age_mse <- mean((factor_age_preds$pred - factor_age_preds$pts_per_g)^2)
factor_age_mse_by_age <- factor_age_preds %>%
  dplyr::group_by(is_24) %>%
  dplyr::summarize(mse = mean((pred - pts_per_g) ^2),
                   avg = mean(pts_per_g)) %>%
  as.data.frame()

# Predicted values viz
actual_vs_pred <- preds %>%
  dplyr::select(row_num, pred, pts_per_g) %>%
  tidyr::pivot_longer(c("pred", "pts_per_g")) %>%
  ggplot() +
  geom_density(aes(x = value, fill = name), alpha = .6)

errors <- preds %>%
  dplyr::transmute(error = pred - pts_per_g) %>%
  ggplot() +
  geom_density(aes(x = error), fill = green, color = green, alpha = .7)

mse_by_age <- preds %>%
  dplyr::group_by(age) %>%
  dplyr::summarize(mse = mean((pred - pts_per_g) ^2),
                   avg = mean(pts_per_g)) %>%
  as.data.frame()

make_factor_dummy_matrix <- function(df, factor_col) {
  # Pull the factor column
  new_df <- df %>% dplyr::select(!!sym(factor_col))

  # Make 0/1 columns for each individual level of the factor column
  for (lvl in forcats::lvls_union(new_df[factor_col])) {
    new_df <- new_df %>%
      dplyr::mutate(
        !!sym(glue("is_{lvl}")) := as.factor(ifelse(
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
