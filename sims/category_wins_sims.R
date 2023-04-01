players <- readr::read_csv("nba_exploration/data/player_stats.csv")

simulate_stats <- function(players, stat, n_sims = 1000) {
  message(paste0("Preparing simulations for ", stat))
  # Limit to last year players who were top 168 in the statistic (totals not per game)
  # if (stat == "tov") {
  #   limit_df <- players %>%
  #     dplyr::filter(year == 2021, g >= 50) %>%
  #     dplyr::arrange(desc(mp)) %>%
  #     dplyr::slice(1:168)
  # } else {
  #   limit_df <- players %>%
  #     dplyr::filter(year == 2021, g >= 50) %>%
  #     dplyr::arrange(desc(!!sym(stat))) %>%
  #     dplyr::slice(1:168)
  # }
  limit_df <- players %>%
    dplyr::filter(year >= 2019, g >= 50)
  
  # Determine weights for weighted average of past seasons
  valid_mp_seasons_by_player <- players %>%
    dplyr::filter(player %in% limit_df$player, year >= 2017) %>%
    dplyr::group_by(player) %>%
    dplyr::summarize(
      num_valid_seasons = sum(ifelse(mp > 20, 1, 0)),
      last_season = max(year[mp > 20]),
      last_minus_1_season = ifelse(
        num_valid_seasons >= 2,
        max(year[mp > 20 & year != last_season]),
        NA_integer_),
      last_minus_2_season = ifelse(
        num_valid_seasons >= 3,
        max(year[mp > 20 & year != last_season & year != last_minus_1_season]),
        NA_integer_)) %>%
    dplyr::ungroup()
  
  season_weights <- players %>%
    dplyr::filter(player %in% limit_df$player, year >= 2017) %>%
    dplyr::left_join(valid_mp_seasons_by_player, by = "player") %>%
    dplyr::mutate(
      season_weight = dplyr::case_when(
        year == last_season ~ sqrt(mp) * .6,
        year == last_minus_1_season ~ sqrt(mp) * .3,
        year == last_minus_2_season ~ sqrt(mp) * .1
      )) %>%
    dplyr::select(player, year, season_weight) %>%
    dplyr::filter(!is.na(season_weight))
  
  # Get means for individual player distributions--weighted average of last 3 seasons of stat
  mp_weighted_avg <- season_weights %>%
    dplyr::left_join(players, by = c("player", "year")) %>%
    dplyr::group_by(player) %>%
    dplyr::summarize(stat = sum(season_weight * !!sym(stat), na.rm = T) /
                       sum(season_weight, na.rm = T)) %>%
    dplyr::ungroup()
  
  # Regress stat on minutes played to estimate variance of the stat
  reg_formula <- paste0(stat, " ~ mp")
  mp_reg <- season_weights %>%
    dplyr::left_join(players, by = c("player", "year")) %>%
    lm(as.formula(reg_formula), data = .)
  
  var_term_2 <- var(min(mp_reg$model$mp):max(mp_reg$model$mp) * mp_reg$coefficients['mp'])
  var_term_1_reg_df <- mp_reg$model %>%
    dplyr::mutate(mp_bucket = cut(mp, breaks = 50)) %>%
    dplyr::group_by(mp_bucket) %>%
    dplyr::summarize(m = sum((!!sym(stat) - mean(!!sym(stat)))^2) / dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(int = dplyr::row_number())
  
  var_term_1_reg <- lm(log(m + 1) ~ int, data = var_term_1_reg_df)
  
  pred_var_term_1 <- season_weights %>%
    dplyr::left_join(players, by = c("player", "year")) %>%
    dplyr::mutate(mp_bucket = cut(mp, breaks = 50)) %>%
    dplyr::left_join(var_term_1_reg_df) %>%
    predict(var_term_1_reg, .)
  
  sd_stat_df <- season_weights %>%
    dplyr::left_join(players, by = c("player", "year")) %>%
    cbind(., "var_term_1" = exp(pred_var_term_1)) %>%
    dplyr::group_by(player) %>%
    dplyr::summarize(sd_stat = sqrt(mean(var_term_1) + var_term_2)) %>%
    dplyr::ungroup()

  # Find the distribution for the weekly average (20 weeks in the season)
  weekly_stat_dists <- mp_weighted_avg %>%
    dplyr::left_join(sd_stat_df) %>%
    dplyr::mutate(stat = stat / 20,
                  sd_stat = sqrt(sd_stat^2 / 400))
  
  message(paste0("Running sims for ", stat))
  sims <- cbind(
    weekly_stat_dists,
    purrr::map2_dfr(
      weekly_stat_dists$stat,
      weekly_stat_dists$sd_stat,
      function(x, y) {
        z <- rep(rnorm(n_sims, x, y))
        while (any(z <= 0)) {
          z_star <- rnorm(length(z[z<=0]), x, y)
          z <- c(z[z>0], z_star)
        }
        return(data.frame(matrix(z, nrow = 1)))
      }))

  # Generate an average performance for 13 other players
  mean_team_totals <- matrix(
    rnorm(n_sims * 13, mean(weekly_stat_dists$stat), mean(weekly_stat_dists$sd_stat)),
          ncol = n_sims) %>%
    colSums()

  sim_results <- cbind(weekly_stat_dists, purrr::map2_dfr(
    sims[paste0("X", 1:n_sims)],
    mean_team_totals,
    ~ .x + .y))

  return(sim_results %>%
    dplyr::mutate_at(vars(tidyselect::starts_with("X")), ~ ifelse(.x > mean(.x), 1, 0)) %>%
    dplyr::select(tidyselect::starts_with("X")) %>%
    rowSums() %>%
    cbind(weekly_stat_dists, "category" = stat, "win_pct" = . / 1000))
}

stats <- c("pts", "trb", "fg3", "fg_pct", "ft_pct", "ast", "stl", "blk", "tov")
all_sims <- purrr::map_dfr(stats, ~ simulate_stats(players, .x))
wide_all_sims <- all_sims %>%
  tidyr::pivot_wider(id_cols = c("player"), names_from = "category", values_from = "win_pct") %>%
  dplyr::mutate_at(stats, ~ tidyr::replace_na(.x, min(.x, na.rm = T)))

# Now just looking at shit here
wide_all_sims <- wide_all_sims %>%
  dplyr::mutate_at("tov", ~ 1 - .x) %>%
  dplyr::mutate_if(is.numeric, list("win_shares" = ~ .2 * (.x - median(.x)))) %>%
  dplyr::mutate(total_win_shares = pts_win_shares + trb_win_shares + fg3_win_shares +
                  fg_pct_win_shares + ft_pct_win_shares + ast_win_shares + stl_win_shares +
                  blk_win_shares + .1 * tov_win_shares)

csv_path <- "~/Documents/Data/auction_draft_projections_1.csv"
wide_all_sims %>%
  readr::write_csv(csv_path)

# We added prices from last year
total_shares_with_prices <- readr::read_csv(csv_path)
max(total_shares_with_prices$keeper_price, na.rm = T)
total_shares_with_prices %>%
  dplyr::filter(!is.na(keeper_price)) %>%
  lm(total_win_shares)



# # Take one draw from weekly_pts_dists to get a sample average of a week
# pts_draw <- cbind(weekly_pts_dists,
#   "pts_draw" = purrr::map2_dbl(
#   weekly_pts_dists$pts,
#   weekly_pts_dists$sd_pts,
#   function(x, y) {
#     z <- 0
#     while (z <= 0) {
#       z <- rep(rnorm(1, x, y))
#     }
#     return(z)
#   }))
# team_weekly_pts <- data.frame(pts_draw) %>%
#   dplyr::mutate(rand_num = runif(nrow(weekly_pts_dists))) %>%
#   dplyr::arrange(rand_num) %>%
#   dplyr::mutate(team = rep(1:12, 14)) %>%
#   dplyr::group_by(team) %>%
#   dplyr::summarize(weekly_pts = sum(pts_draw))
# mean(team_weekly_pts$weekly_pts)

# simulate_team_selection <- function() {
#   teams <- data.frame()
#   for (i in seq(1, 146, 24)) {
#     teams <- rbind(teams, weekly_pts_dists %>%
#                      dplyr::arrange(pts) %>%
#                      dplyr::slice(i:(i + 23)) %>%
#                      dplyr::mutate(rand_num = runif(24)) %>%
#                      dplyr::arrange(rand_num) %>%
#                      dplyr::transmute(player, pts, sd_pts, team = rep(1:12, 2)))
#   }
#   
#   return(teams %>%
#     dplyr::left_join(pts_draw) %>%
#     dplyr::group_by(team) %>%
#     dplyr::mutate(total_pts = sum(pts_draw)) %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by() %>%
#     dplyr::mutate(max_total = max(total_pts)) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(total_pts == max_total))
# }
# 
# pts_sims <- data.frame()
# for (i in 1:1000) {
#   pts_sims <- rbind(pts_sims, simulate_team_selection())
# }
# pts_sims %>%
#   dplyr::group_by(player) %>%
#   dplyr::summarize(wins = dplyr::n()) %>%
#   dplyr::arrange(desc(wins)) %>%
#   View()

