library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(gam)
library(splines)

players <- readr::read_csv("./data/player_stats.csv")
players_2022 <- readr::read_csv("./data/player_stats_2022.csv")
players <- dplyr::bind_rows(players, players_2022) %>%
  dplyr::filter(player != "Caver,Ahmad")
players$player[players$player == "Claxton,Nic"] <- "Claxton,Nicolas"
players$pos[players$player == "Boucher,Chris" & players$year == 2022] <- "C"


# add "jr." to players with nba dads or younger guys with the same name
juniors <- players %>%
  dplyr::select(player, year) %>%
  dplyr::group_by(player) %>%
  dplyr::mutate(min_season = min(year),
                max_season = max(year)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(max_season - min_season >= 20) %>%
  dplyr::distinct(player) %>%
  dplyr::pull(player)

dupe_ages <- players %>%
  dplyr::arrange(player, age) %>%
  dplyr::group_by(player) %>%
  dplyr::mutate(age_rank = dplyr::min_rank(age)) %>%
  dplyr::select(year, player, age_rank) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(player, age_rank) %>%
  dplyr::tally() %>%
  dplyr::filter(n > 1) %>%
  dplyr::pull(player) %>%
  unique()

name_changes <- unique(c(dupe_ages,
                         juniors[!(juniors %in% c("Garnett,Kevin", "Carter,Vince", "Nowitzki,Dirk",
                                                  "Johnson,Joe"))]))
for (p in name_changes) {
  years <- players %>%
   dplyr::filter(player == p) %>%
   dplyr::pull(year)
  max_year_jump <- max(cummax(years[2:length(years)]) - years[1:(length(years) - 1)])
  final_sr_year <- years[
   which(cummax(years[2:length(years)]) - years[1:(length(years) - 1)] ==
          max_year_jump)]
  players$player[players$player == p & players$year > final_sr_year] <- paste0(
    "Jr. ", p)
}

# make position unique to player by taking position from their last season
players <- players %>%
  dplyr::mutate(pos = stringr::str_extract(pos, "^([A-Z]{2}|[A-Z]{1})")) %>%
  dplyr::left_join(
    players %>%
      dplyr::mutate(pos = stringr::str_extract(pos, "^([A-Z]{2}|[A-Z]{1})")) %>%
      dplyr::group_by(player, pos) %>%
      dplyr::summarize(n_years = dplyr::n(),
                       last_year = max(year)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(player) %>%
      dplyr::mutate(best_pos = max(n_years),
                    max_year = max(last_year)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(best_pos == n_years) %>%
      dplyr::group_by(player) %>%
      dplyr::mutate(nrows = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(nrows == 1 | (nrows >= 2 & max_year == last_year)) %>%
      dplyr::transmute(player, pos_unique = pos)
  )

# project minutes
mp_df <- players %>%
  dplyr::select(player, year, mp, pos_unique, ws_per_48) %>%
  dplyr::group_by(player) %>%
  dplyr::mutate(min_season = min(year),
                max_season = max(year)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(cols = c("mp", "ws_per_48")) %>%
  dplyr::mutate(season_num = year - min_season + 1) %>%
  dplyr::filter(!(player %in% c("Jr. Jones,Charles", "Jones,Charles",
                                "Jr. Smith,Michael", "Jr. Smith,Charles",
                                "Smith,Charles", "Johnson,Eddie",
                                "Jr. Williams,Marcus", "Williams,Marcus",
                                "Johnson,George", "Mitchell,Tony"))) %>% # dupes
  tidyr::pivot_wider(id_cols = c("player", "pos_unique", "min_season", "max_season"),
                     names_from = c("name", "season_num"),
                     values_from = "value",
                     names_sep = "_season_")

id_cols <- c("player", "pos_unique", "min_season", "max_season")
max_season_iter <- max(as.numeric(
  gsub("[a-z0-9_]+_season_", "",
       colnames(mp_df)[!colnames(mp_df) %in% id_cols])
))
mp_models <- list()
for (season in seq(2, max_season_iter)) {
  if (season <= 13) {
    rhs <- paste(sapply(seq_len(season - 1),
                        function(s) {
                          paste(paste0(c("ws_per_48_season_", "mp_season_"), s),
                                collapse = "+")
                        }),
                 collapse="+")
  } else {
    rhs <- paste(sapply(seq(season - 3, season - 1),
                        function(s) {
                          paste(paste0(c("ws_per_48_season_", "mp_season_"), s),
                                collapse = "+")
                        }),
                 collapse="+")
  }
  lhs <- paste0("mp_season_", season)
  form <- as.formula(paste0(lhs, " ~ ", rhs, " + pos_unique"))

  reg_data <- mp_df %>%
    dplyr::filter(min_season + season <= max_season) %>%
    dplyr::mutate_at(vars(tidyselect::contains("season")),
                     ~ ifelse(is.na(.x), 0, .x))
  if (length(unique(reg_data$player)) < 2) {
    mp_models[[season]] <- NULL
  } else {
    for (s in seq_len(season - 1)) {
      no_outlier_data <- reg_data
      ws_col <- paste0("ws_per_48_season_", s)
      mp_col <- paste0("mp_season_", s)
      quants <- quantile(reg_data[[ws_col]], c(0.05, 0.95))
      no_outlier_data <- no_outlier_data %>%
        dplyr::filter((!!sym(ws_col) > quants[1] & !!sym(ws_col) < quants[2]) |
                        !!sym(mp_col) > 100)
    }
    mp_reg <- glm(form, data = no_outlier_data, family = poisson())
    mp_models[[season]] <- mp_reg
  }
}
mp_preds <- purrr:::map2_dfr(
  mp_models[2:16],
  seq(2, 16),
  function(x, y) {
    cbind(pred = predict(x,
                         mp_df %>%
                           dplyr::filter(min_season == 2023 - y + 1) %>%
                           dplyr::mutate_at(vars(tidyselect::contains("season")),
                                            ~ ifelse(is.na(.x), 0, .x)),
                         "response"),
          mp_df %>%
            dplyr::filter(min_season == 2023 - y + 1))
  })

make_stat_preds <- function(players, stat) {
  message(paste0("Predicting ", stat))
  if (!grepl("pct", stat)) {
    stat_per_poss_col <- paste0(stat, "_per_poss")
    stat_df <- players %>%
      dplyr::mutate_at(stat_per_poss_col, ~ .x / 100) # it's actually per 100 poss col
    all_stat_cols <- c(stat_per_poss_col)
  } else {
    stat_per_poss_col <- stat
    stat_attempt_col <- gsub("_pct", "", stat)
    all_stat_cols <- c(stat_per_poss_col, stat_attempt_col)
    stat_df <- players
  }
  
  stat_df <- stat_df %>%
    dplyr::select(player, year, mp, pos_unique, !!!syms(all_stat_cols)) %>%
    dplyr::group_by(player) %>%
    dplyr::mutate(min_season = min(year),
                  max_season = max(year)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c("mp", all_stat_cols)) %>%
    dplyr::mutate(season_num = year - min_season + 1) %>%
    dplyr::filter(!(player %in% c("Jr. Jones,Charles", "Jones,Charles",
                                  "Jr. Smith,Michael", "Jr. Smith,Charles",
                                  "Smith,Charles", "Johnson,Eddie",
                                  "Jr. Williams,Marcus", "Williams,Marcus",
                                  "Johnson,George", "Mitchell,Tony"))) %>% # dupes
    tidyr::pivot_wider(id_cols = c("player", "pos_unique", "min_season", "max_season"),
                       names_from = c("name", "season_num"),
                       values_from = "value",
                       names_sep = "_season_")
  
  id_cols <- c("player", "pos_unique", "min_season", "max_season")
  max_season_iter <- max(as.numeric(
    gsub("[a-z0-9_]+_season_", "",
         colnames(stat_df)[!colnames(stat_df) %in% id_cols])
  ))
  stat_models <- list()
  for (season in seq(2, max_season_iter)) {
    if (season <= 13) {
      rhs <- paste(paste0("bs(", stat_per_poss_col, "_season_",
                          seq_len(season - 1), ", df=5)"),
                   collapse="+")
    } else {
      rhs <- paste(paste0("bs(", stat_per_poss_col, "_season_",
                          seq(season - 3, season - 1), ", df=5)"),
                   collapse="+")
    }
    lhs <- paste0(stat_per_poss_col, "_season_", season)
    form <- as.formula(paste0(lhs, " ~ ", rhs, "+pos_unique"))
    reg_data <- stat_df %>%
      dplyr::filter(min_season + season <= max_season) %>%
      dplyr::mutate_at(vars(tidyselect::contains("season")),
                       ~ ifelse(is.na(.x), 0, .x))
    if (length(unique(reg_data$player)) < 2) {
      stat_models[[season]] <- NULL
    } else {
      for (s in seq_len(season - 1)) {
        no_outlier_data <- reg_data
        season_stat_col <- paste0(stat_per_poss_col, "_season_", s)
        season_mp_col <- paste0("mp_season_", s)
        quants <- quantile(reg_data[[season_stat_col]], c(0.05, 0.95))
        no_outlier_data <- no_outlier_data %>%
          dplyr::filter((!!sym(season_stat_col) > quants[1] &
                           !!sym(season_stat_col) < quants[2]) |
                          !!sym(season_mp_col) > 100)
      }
      if (grepl("pct", stat)) {
        stat_reg <- gam::gam(
          form, data = no_outlier_data, family = quasibinomial(),
          weights = no_outlier_data[[paste0(stat_attempt_col, "_season_", season)]]
        )
        stat_models[[season]] <- stat_reg
      } else {
        stat_reg <- gam::gam(form, data = no_outlier_data)
        stat_models[[season]] <- stat_reg        
      }
    }
  }
  stat_preds <- purrr:::map2_dfr(
    stat_models[2:16],
    seq(2, 16),
    function(x, y) {
      cbind(pred = predict(x,
                           stat_df %>%
                             dplyr::filter(min_season == 2023 - y + 1) %>%
                             dplyr::mutate_at(vars(tidyselect::contains("season")),
                                              ~ ifelse(is.na(.x), 0, .x)),
                           "response"),
            stat_df %>%
              dplyr::filter(min_season == 2023 - y + 1) %>%
              dplyr::transmute(player, stat = stat))
    })
  
  return(stat_preds)
}

simulate_stats <- function(player_df, pred_df, stat, pred_players, n_sims = 1000) {
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
  
  # Determine weights for weighted average of past seasons
  # valid_mp_seasons_by_player <- players %>%
  #   dplyr::filter(player %in% limit_df$player, year >= 2020) %>%
  #   dplyr::group_by(player) %>%
  #   dplyr::summarize(
  #     num_valid_seasons = sum(ifelse(mp > 20, 1, 0)),
  #     last_season = max(year[mp > 20]),
  #     last_minus_1_season = ifelse(
  #       num_valid_seasons >= 2,
  #       max(year[mp > 20 & year != last_season]),
  #       NA_integer_),
  #     last_minus_2_season = ifelse(
  #       num_valid_seasons >= 3,
  #       max(year[mp > 20 & year != last_season & year != last_minus_1_season]),
  #       NA_integer_)) %>%
  #   dplyr::ungroup()
  
  # prep sims
  if (!grepl("pct", stat)) {
    stat_per_poss_col <- paste0(stat, "_per_poss")
    mins_stats <- player_df %>%
      dplyr::filter(player %in% pred_players) %>%
      dplyr::transmute(year = as.character(year), player, mp, pos_unique,
                       stat = !!sym(stat_per_poss_col) / 100) %>%
      dplyr::bind_rows(
        ., pred_df %>%
          dplyr::filter(player %in% pred_players) %>%
          dplyr::left_join(player_df %>% dplyr::distinct(player, pos_unique)) %>%
          dplyr::transmute(year = "pred", player, mp, pos_unique, stat = !!sym(stat)))
  } else {
    stat_per_poss_col <- stat
    mins_stats <- player_df %>%
      dplyr::filter(player %in% pred_players) %>%
      dplyr::transmute(year = as.character(year), player, mp, pos_unique,
                       stat = !!sym(stat_per_poss_col)) %>%
      dplyr::bind_rows(
        ., pred_df %>%
          dplyr::filter(player %in% pred_players) %>%
          dplyr::left_join(player_df %>% dplyr::distinct(player, pos_unique)) %>%
          dplyr::transmute(year = "pred", player, mp, pos_unique, stat = !!sym(stat)))
  }
  
  season_counts <- mins_stats %>%
    dplyr::group_by(player, pos_unique) %>%
    dplyr::tally() %>%
    dplyr::ungroup()

  mins_stats$year <- factor(
    mins_stats$year,
    levels = c(as.character(
      seq(as.numeric(min(mins_stats$year)),
          as.numeric(max(mins_stats$year[mins_stats$year != "pred"])) - 1)),
      "pred", "2022"))
  mins_stats <- mins_stats %>%
    dplyr::arrange(player) %>%
    dplyr::group_by(player) %>%
    dplyr::mutate(season_num = rank(year)) %>%
    dplyr::ungroup()
  
  # run sims
  AVG_POSS_PER_MIN <- 2.057
  N_PLAYERS_PER_TEAM <- 14
  N_TEAMS <- 12
  pct_players_taken <- N_TEAMS * N_PLAYERS_PER_TEAM / length(pred_players)
  all_sims <- season_counts %>%
    dplyr::select(player)
  for (sim_num in seq_len(n_sims)) {
    choices <- cbind(
      season_counts,
      mp_choice = season_counts %>%
        dplyr::pull(n) %>%
        sapply(., function(n) sample(n, 1,
                                     prob = cumsum(seq_len(n)/ sum(cumsum(seq_len(n)))))),
      stat_choice = season_counts %>%
        dplyr::pull(n) %>%
        sapply(., function(n) sample(n, 1,
                                     prob = cumsum(seq_len(n)/ sum(cumsum(seq_len(n))))))
    )

    sim <- mins_stats %>%
      dplyr::left_join(choices)
    
    if (!grepl("pct", stat)) {
      final_sim <- cbind(
        season_counts,
        mp = sim$mp[sim$season_num == sim$mp_choice],
        stat = sim$stat[sim$season_num == sim$stat_choice],
        season_sim = sapply(sim$stat[sim$season_num == sim$stat_choice] *
                              sim$mp[sim$season_num == sim$mp_choice] *
                              AVG_POSS_PER_MIN,
                            function(x) max(0, x))
      )
    } else {
      final_sim <- cbind(
        season_counts,
        mp = sim$mp[sim$season_num == sim$mp_choice],
        stat = sim$stat[sim$season_num == sim$stat_choice],
        season_sim = sim$stat[sim$season_num == sim$stat_choice]
      )
    }

    # get avg stat values for rest of the team
    # 7 random bench players + mobley + haliburton + 4 other pos that aren't the player's pos
    hali_mob <- final_sim[final_sim$player %in% c("Mobley,Evan",
                                                  "Haliburton,Tyrese"),]

    mp_quant <- quantile(final_sim$mp, 1 - pct_players_taken)
    keep_players <- final_sim[final_sim$mp > mp_quant,]
    other_pos_avgs <- keep_players %>%
      dplyr::filter(!(player %in% c("Mobley,Evan", "Haliburton,Tyrese"))) %>%
      dplyr::group_by(pos_unique) %>%
      dplyr::summarize(mean_season_sim = mean(season_sim)) %>%
      dplyr::ungroup()
    # other_pos_avgs_sum <- sum(other_pos_avgs$mean_season_sim)
    
    rest_of_team_vals <- setNames(c(
      sample(other_pos_avgs$mean_season_sim, 7, replace = TRUE),
      hali_mob$season_sim,
      other_pos_avgs$mean_season_sim),
      c(rep("bench", 7), "hali", "mob", other_pos_avgs$pos_unique))
    # rest_of_team_sum <- sum(rest_of_team_vals)
    
    # random opponent
    opponent_vals <- setNames(c(
      sample(keep_players$season_sim, 7, replace = TRUE),
      sample(keep_players$season_sim[grepl("F$", keep_players$pos_unique)], 1),
      sample(keep_players$season_sim[grepl("G$", keep_players$pos_unique)], 1),
      sample(keep_players$season_sim[keep_players$pos_unique == "C"], 1),
      sample(keep_players$season_sim[keep_players$pos_unique == "PF"], 1),
      sample(keep_players$season_sim[keep_players$pos_unique == "PG"], 1),
      sample(keep_players$season_sim[keep_players$pos_unique == "SF"], 1),
      sample(keep_players$season_sim[keep_players$pos_unique == "SG"], 1)),
      c(rep("bench", 7), "F", "G", other_pos_avgs$pos_unique)
    )
    # opponent_sum <- sum(opponent_vals)
    
    if (!grepl("pct", stat)) {
      rest_of_team_vals <- rest_of_team_vals / 19
      rest_of_team_sims <- matrix(rpois(14 * 19, rest_of_team_vals), ncol = 14, byrow = TRUE)
      colnames(rest_of_team_sims) <- names(rest_of_team_vals)
      
      opponent_vals <- opponent_vals / 19
      opponent_sims <- matrix(rpois(14 * 19, opponent_vals), ncol = 14, byrow = TRUE)
      opponent_weeks <- apply(opponent_sims, 1, sum)

      player_sims <- matrix(rpois(nrow(final_sim) * 19, final_sim$season_sim / 19),
                            ncol = nrow(final_sim), byrow = TRUE)
      colnames(player_sims) <- final_sim$player
      out <- sapply(final_sim$player,
                   function(player) {
                     player_specific_sims <- rest_of_team_sims
                     player_specific_sims[,final_sim$pos_unique[final_sim$player == player]] <-
                       player_sims[,player]
                     sum(apply(player_specific_sims, 1, sum) > opponent_weeks)
                   })
      all_sims <- cbind(all_sims, out)
    } else {
      rest_of_team_sims <- matrix(
        rbeta(14 * 19, 1.2, 1.2 / rest_of_team_vals - rest_of_team_vals),
        ncol = 14, byrow = TRUE)
      colnames(rest_of_team_sims) <- names(rest_of_team_vals)
      
      opponent_sims <- matrix(
        rbeta(14 * 19, 1.2, 1.2 / opponent_vals - opponent_vals),
        ncol = 14, byrow = TRUE)
      opponent_weeks <- apply(opponent_sims, 1, mean)
      
      player_sims <- matrix(
        rbeta(nrow(final_sim) * 19, 1.2, 1.2 / final_sim$season_sim - final_sim$season_sim),
        ncol = nrow(final_sim), byrow = TRUE)
      colnames(player_sims) <- final_sim$player
      out <- sapply(final_sim$player,
                    function(player) {
                      player_specific_sims <- rest_of_team_sims
                      player_specific_sims[,final_sim$pos_unique[final_sim$player == player]] <-
                        player_sims[,player]
                      sum(apply(player_specific_sims, 1, mean) > opponent_weeks)
                    })
      all_sims <- cbind(all_sims, out)
    }
    # final_sim <- final_sim %>%
    #   dplyr::left_join(other_pos_avgs %>%
    #                      rename(pos_mean_sim = mean_season_sim)) %>%
    #   dplyr::mutate(
    #     team_sim = rest_of_team_sum + season_sim + other_pos_avgs_sum -
    #       pos_mean_sim,
    #     opponent_sim = opponent_sum)
    
    # all_sims <- cbind(all_sims, final_sim$team_sim > final_sim$opponent_sim)
  }
  
  return(all_sims)
}

stats <- c("pts", "trb", "fg3", "fg_pct", "ft_pct", "ast", "stl", "blk", "tov")

if (sys.nframe() == 0) {
  long_preds <- purrr::map_dfr(stats, make_stat_preds, players = players)
  wide_preds <- tidyr::pivot_wider(long_preds,
                                   values_from = "pred",
                                   names_from = "stat")
  wide_preds <- wide_preds %>%
    dplyr::left_join(mp_preds %>% dplyr::transmute(player, mp = pred))
  
  pred_players <- players %>%
    dplyr::filter(year >= 2021) %>%
    dplyr::pull(player) %>%
    unique()
  
  all_sims <- Reduce(
    cbind,
    lapply(
      stats,
      function(stat) {
        message(paste0("Simming ", stat))
        sim <- suppressMessages(
          simulate_stats(players, wide_preds, stat, pred_players, n_sims = 1000)
        )
        apply(sim[,-1], 1, mean)
      }))
  colnames(all_sims) <- stats
  
  # Added prices from last year to a different csv
  prices_csv_path <- "~/Documents/Data/auction_draft_2022_projections_1.csv"
  prices_df <- readr::read_csv(prices_csv_path)
  sim_df <- as.data.frame(all_sims) %>%
    tibble::rownames_to_column("player") %>%
    dplyr::left_join(prices_df %>% dplyr::select(player, price_2022)) %>%
    dplyr::mutate_at("price_2022", ~ ifelse(is.na(.x), 0, .x)) %>%
    dplyr::mutate_at(colnames(all_sims),
                     list(stdized = ~ (.x - mean(.x)) / sd(.x),
                          rank = ~ dplyr::min_rank(.x))) %>%
    dplyr::mutate_at(vars(tidyselect::ends_with("rank")),
                     list(top_168 = ~ as.numeric(.x > 698 - 168),
                          top_100 = ~ as.numeric(.x > 698 - 100),
                          top_50 = ~ as.numeric(.x > 698 - 50),
                          top_30 = ~ as.numeric(.x > 698 - 30),
                          top_10 = ~ as.numeric(.x > 698 - 10))) %>%
    dplyr::mutate(n_top_168s = pts_rank_top_168 + trb_rank_top_168 +
                    ast_rank_top_168 + blk_rank_top_168 + stl_rank_top_168 +
                    ft_pct_rank_top_168 + fg_pct_rank_top_168 + fg3_rank_top_168 +
                    tov_rank_top_168,
                  n_top_100s = pts_rank_top_100 + trb_rank_top_100 +
                    ast_rank_top_100 + blk_rank_top_100 + stl_rank_top_100 +
                    ft_pct_rank_top_100 + fg_pct_rank_top_100 + fg3_rank_top_100 +
                    tov_rank_top_100,
                  n_top_50s = pts_rank_top_50 + trb_rank_top_50 +
                    ast_rank_top_50 + blk_rank_top_50 + stl_rank_top_50 +
                    ft_pct_rank_top_50 + fg_pct_rank_top_50 + fg3_rank_top_50 +
                    tov_rank_top_50,
                  n_top_30s = pts_rank_top_30 + trb_rank_top_30 +
                    ast_rank_top_30 + blk_rank_top_30 + stl_rank_top_30 +
                    ft_pct_rank_top_30 + fg_pct_rank_top_30 + fg3_rank_top_30 +
                    tov_rank_top_30,
                  n_top_10s = pts_rank_top_10 + trb_rank_top_10 +
                    ast_rank_top_10 + blk_rank_top_10 + stl_rank_top_10 +
                    ft_pct_rank_top_10 + fg_pct_rank_top_10 + fg3_rank_top_10 +
                    tov_rank_top_10
    )
  csv_path <- "~/Documents/Data/auction_draft_2022_projections_2.csv"
  sim_df %>%
    readr::write_csv(csv_path)
  
  
  total_shares_with_prices <- readr::read_csv(csv_path)
  max(total_shares_with_prices$price_2022, na.rm = T)
  total_shares_with_prices %>%
    lm(total_win_shares ~ price_2022, .) %>%
    summary()
}
