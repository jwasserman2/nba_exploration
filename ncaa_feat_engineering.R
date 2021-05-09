library(dplyr)
library(stringr)
library(magrittr)
library(rlang)
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
                   list("per_100" = ~ .x / (pace / 40 * mp) * 100,
                        "per_g" = ~ .x / g))

# Account for SOS -- run regressions fon different stats to determine adjustments
sos_reg <- function(adjusted_df, stat) {
  # Determine dvs and covs based on stat used for regression
  if (stat %in% counting_stats) {
    dvs <- rlang::quos(
      log_y = ifelse(!!sym(stat) == 0, 0, log(!!sym(stat))),
      y = !!sym(stat),
      log_y_per_g = ifelse(!!sym(stat) == 0, 0,
                           log(!!sym(str_glue(stat, "_per_g")))),
      y_per_g = !!sym(stat) / g,
      y_per_100 = !!sym(str_glue(stat, "_per_g")),
      log_y_per_100 = ifelse(!!sym(stat) == 0, 0,
                             log(!!sym(str_glue(stat, "_per_100"))))
    )
    pred_dv <- "log_y_per_100"
    covs <- c("g", "mp", "rsci", "height", "weight", "sos", "class",
              "num_hs_ranked_players")
  } else {
    dvs <- rlang::quos(
      log_y = ifelse(!!sym(stat) == 0, 0, log(!!sym(stat))),
      y = !!sym(stat)
    )
    pred_dv <- "y"
    covs <- c("g", "mp", "fga", "fta", "rsci", "height", "weight", "sos",
              "class", "num_hs_ranked_players")
  }

  # Set up df for regression
  message("Preparing df for regression")
  reg_df <- adjusted_df %>%
    # Exclude records in the current season
    dplyr::filter(season != 2021) %>%
    # Join team SOS
    dplyr::left_join(
      ncaa_team %>%
        dplyr::select(season, school, sos)
    ) %>%
    dplyr::group_by(season, school) %>%
    # Engineer feature of # of ranked HS players on the team
    dplyr::mutate(num_hs_ranked_players = sum(ifelse(rsci != "unranked", 1, 0))) %>%
    dplyr::ungroup() %>%
    # Exclude players who didn't log a minute
    dplyr::filter(mp > 0, g > 0) %>%
    dplyr::mutate(!!!dvs) %>%
    dplyr::select(tidyselect::starts_with("y"),
                  tidyselect::starts_with("log_y"),
                  !!!syms(covs)) %>%
    # Exclude NA's
    dplyr::filter_all(all_vars(!is.na(.)))

  dv_cols <- reg_df %>%
    dplyr::select(c(tidyselect::starts_with("y"),
                    tidyselect::starts_with("log_y"))) %>%
    colnames()

  # Run models for each dv
  sos_mods <- purrr::map(
    dv_cols,
    function(x) {
      message(str_glue("Building model for {x}"))
      df <- reg_df %>%
        dplyr::transmute(
          rsci = as.numeric(rsci),
          class = as.numeric(class),
          !!!syms(covs[!(covs %in% c("rsci", "class"))])) %>%
        as.matrix()
      y <- as.matrix(reg_df %>% dplyr::pull(!!sym(x)))
      return(glmnet::cv.glmnet(df, y))
    }
  ) %>%
    setNames(dv_cols)

  # Make preds for log_{stat}_per_100 for counting stats and {stat} for pct stats
  sos_preds <- predict(
    sos_mods[[pred_dv]],
    reg_df %>%
      dplyr::transmute(
        rsci = as.numeric(rsci),
        class = as.numeric(class),
        !!!syms(covs[!(covs %in% c("rsci", "class"))])) %>%
      as.matrix())

  # Make plot of the residuals
  message("Making a plot of the residuals")
  resid_df <- data.frame(reg_df, "pred" = c(sos_preds))

  resid_plot <- resid_df %>%
    dplyr::mutate(resid = pred - !!sym(pred_dv)) %>%
    tidyr::pivot_longer(c(pred_dv, "pred", "resid")) %>%
    ggplot() +
    geom_density(aes(x = value, fill = name), alpha = .3)

  return(list(
    "resid_df" = resid_df,
    "resid_plot" = resid_plot,
    "model" = sos_mods[[pred_dv]]
  ))
}

# Run SOS regs for scoring stats
sos_pts_reg <- sos_reg(adjusted, "pts")
sos_fg3_pct_reg <- sos_reg(adjusted, "fg3_pct")
sos_trb_reg <- sos_reg(adjusted, "trb")
sos_fg_pct_reg <- sos_reg(adjusted, "fg_pct")
sos_to_reg <- sos_reg(adjusted, "tov")
sos_to_rate_reg <- sos_reg(adjusted, "tov_pct")
sos_ts_pct_reg <- sos_reg(adjusted, "ts_pct")
sos_efg_pct_reg <- sos_reg(adjusted, "efg_pct")

# For counting stats, we're going to use the coefficient from the "pts" reg and
# the "fg_pct" reg for pct stats because they seem the most reasonable
sos_coefs <- purrr::map_dbl(
  list(sos_pts_reg$model, sos_fg_pct_reg$model),
  ~ coef(.x, s = .x$lambda.min) %>%
    broom::tidy() %>%
    dplyr::filter(row == "sos") %>%
    dplyr::pull(value)
) %>%
  setNames(c("pts", "fg_pct"))

# Weight stats by 1 / exp(SOS * coef) (exp(SOS * coef) for turnover stats)
adjusted %<>%
  dplyr::left_join(
    ncaa_team %>%
      dplyr::select(season, school, sos)
  ) %>%
  dplyr::mutate(dws_per_40 = dws / mp * 40) %>%
  dplyr::mutate_at(
    c(paste0(counting_stats[counting_stats != "tov"], "_per_100"),
      "dws_per_40", "ws_per_40"),
    list("sos_weighted" = ~ .x * (1 / exp(sos * sos_coefs[['pts']])))
  ) %>%
  dplyr::mutate_at(
    vars(tidyselect::matches("pct")),
    list("sos_weighted" = ~ .x * (1 / exp(sos * sos_coefs[['fg_pct']])))
  ) %>%
  dplyr::select(-paste0(
    c("tov_pct", "usg_pct", "fta_per_fga_pct", "ft_pct", "fg3a_per_fga_pct"),
    "_sos_weighted")) %>%
  dplyr::mutate(
    tov_per_100_sos_weighted = tov_per_100 * exp(sos * sos_coefs[['pts']]),
    tov_pct_sos_weighted = tov_pct * exp(sos * sos_coefs[['fg_pct']]),
    mp_per_g = mp / g
  )


# Scraper set 0's for NA's for a few columns, will impute here
adjusted %<>%
  dplyr::mutate_at(c("drb", "orb"),
                   ~ ifelse(season <= 2000 & .x == 0, NA_real_, .x)) %>%
  dplyr::mutate_at(c("drb_pct", "orb_pct"),
                   ~ ifelse(season <= 2009 & .x == 0, NA_real_, .x))

reb_impute_cols <- c("pos", "height", "fg2a", "fg2a_per_100", "fg2_pct",
                     "fta_per_fga_pct", "trb_per_100", "rsci", "trb")

glm_impute_col <- function(player_df, impute_dv, impute_covs, impute_covs_to_dummify) {
  message(str_glue("Imputing {impute_dv}..."))
  # GLM model used for imputing
  form <- as.character(str_glue("{impute_dv} ~ ."))
  X_train <- player_df %>%
    dplyr::select(!!!syms(c(impute_covs, impute_dv))) %>%
    dplyr::filter_all(all_vars(!is.na(.))) %>%
    dplyr::filter_all(all_vars(. != Inf)) %>%
    dplyr::mutate_at(impute_covs_to_dummify, as.factor)

  stopifnot(nrow(X_train) > 2000)

  y <- player_df %>%
    dplyr::select(!!!syms(c(impute_covs, impute_dv))) %>%
    dplyr::filter_all(all_vars(!is.na(.))) %>%
    dplyr::filter_all(all_vars(. != Inf)) %>%
    dplyr::pull(!!sym(impute_dv))

  stopifnot(length(y) == nrow(X_train))

  message(str_glue("Training model with {dim(X_train)[1]} training records"))
  mod <- glmnet::cv.glmnet(model.matrix(as.formula(form), X_train), as.matrix(y))

  # Join imputed values and add to full df
  X_pred <- player_df %>%
    dplyr::filter(is.na(!!sym(impute_dv))) %>%
    dplyr::select(!!!syms(impute_covs)) %>%
    dplyr::filter_all(all_vars(!is.na(.))) %>%
    dplyr::filter_all(all_vars(. != Inf)) %>%
    dplyr::mutate_at(impute_covs_to_dummify, as.factor)

  stopifnot(nrow(X_pred) > 0)
  message(str_glue("Generating {dim(X_pred)[1]} imputed values for {impute_dv}"))
  preds <- predict(mod, model.matrix(~ ., X_pred))

  # Return df that has impute values and original values and columns indicating
  # the imputed values
  joined_preds <- player_df %>%
    dplyr::filter(is.na(!!sym(impute_dv))) %>%
    dplyr::select(-!!sym(impute_dv)) %>%
    dplyr::filter_at(dplyr::setdiff(impute_covs, impute_dv),
                     ~ !is.na(.x)) %>%
    dplyr::filter_at(dplyr::setdiff(impute_covs, impute_dv),
                     ~ .x != Inf) %>%
    dplyr::bind_cols(!!impute_dv := as.vector(preds)) %>%
    dplyr::mutate(!!str_glue("{impute_dv}_imputed") := 1)

  out_df <- player_df %>%
    dplyr::filter(!(pid %in% joined_preds$pid)) %>%
    dplyr::mutate(!!str_glue("{impute_dv}_imputed") := 0) %>%
    dplyr::bind_rows(joined_preds)
  message(nrow(out_df))

  # Bind rows that didn't have enough data for imputation--will remove later
  missing_imputations <- out_df %>%
    dplyr::filter(is.na(!!sym(impute_dv)))
  message(str_glue("Failed to impute {nrow(missing_imputations)} records due to \\
                   missing data"))

  stopifnot(nrow(out_df) == nrow(player_df))

  return(out_df)
}

adjusted <- glm_impute_col(adjusted, "orb", reb_impute_cols, c("rsci"))

adjusted %>%
  dplyr::filter(is.na(orb)) %>%
  dplyr::select(c(reb_impute_cols, "orb")) %>%
  dplyr::filter_at(reb_impute_cols, all_vars(!is.na(.))) %>%
  dplyr::filter_at(reb_impute_cols, all_vars(. != Inf))
