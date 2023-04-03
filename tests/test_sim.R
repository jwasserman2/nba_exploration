source("./sims/category_wins_sims_2022.R")

long_preds <- make_stat_preds(players = players, stats[1])
wide_preds <- tidyr::pivot_wider(long_preds,
                                 values_from = "pred",
                                 names_from = "stat")
wide_preds <- wide_preds %>%
  dplyr::left_join(mp_preds %>% dplyr::transmute(player, mp = pred))

pred_players <- players %>%
  dplyr::filter(year >= 2021) %>%
  dplyr::pull(player) %>%
  unique()

sim <- suppressMessages(
  simulate_stats(players, wide_preds, stats[1], pred_players, n_sims = 1000)
)
head(apply(sim[,-1], 1, mean))