rm(list = ls(all.names = TRUE))
today = format(Sys.Date(),"%d-%b-%Y")

# Load libraries
library(baseballr)
library(tidyverse)
library(lubridate)
library(zoo)
library(fitdistrplus)

# Get all MLB games for 2025
schedule_2025 <- mlb_schedule(season = 2025)
#unique(schedule_2025$teams_home_team_name)

teamnames <- c("Colorado Rockies", "Athletics", "Boston Red Sox", "Cincinnati Reds", "Los Angeles Dodgers" ,"St. Louis Cardinals", "New York Mets","Miami Marlins")
win_totals <- c(60.5,70.5,84.5,78.5,103.5,77.5,100,80)
ex_win_colors <- c("purple2","green2","red2","red1","blue2","red4","orange2","pink3")

# All MLB
non_MLB_teams <- c("Hanshin Tigers", "Tokyo Yomiuri Giants", "Memphis Redbirds", "Sultanes de Monterrey", "National League All-Stars")

teamnames <- schedule_2025 %>%
  filter(!teams_home_team_name %in% non_MLB_teams) %>%
  pull(teams_home_team_name) %>%
  unique()

ex_win_colors <- c("blue2","navy","red2","blue1","blue3",
                   "orangered2","orange4","orange1","orange2","yellow4",
                   "navy","red3","blue","orange2","pink2",
                   "red1","blue3","purple2","red","gold2",
                   "red3","red4","yellow2","red","navy",
                   "red3","green2","orange","cyan3","grey25")

season_win_totals <- tibble(Team = teamnames,
                            #`Win Total Line` = win_totals,
                            Expected_Wins = NA,
                            E95_Wins = NA,
                            E05_Wins = NA)

for (i in  1:length(teamnames)){
  teamname = teamnames[i]
  #win_total = win_totals[i]
  print(paste("Computing", teamname, "Expected Win Total"))
  
  # Overall Record & Runs for and against
  team_record <- schedule_2025 %>%
    filter(
      teams_home_team_name == teamname | teams_away_team_name == teamname,
      game_type == "R", 
      status_coded_game_state == "F") %>%
    mutate(
      Game_Win = case_when(
        teams_home_team_name == teamname & teams_home_is_winner ~ 1,
        teams_away_team_name == teamname & !teams_home_is_winner ~ 1,
        TRUE ~ 0),
      Game_Loss = 1 - Game_Win,
      
      Runs_Scored = case_when(
        teams_home_team_name == teamname ~ teams_home_score,
        teams_away_team_name == teamname ~ teams_away_score),
      Runs_Allowed = case_when(
        teams_home_team_name == teamname ~ teams_away_score,
        teams_away_team_name == teamname ~ teams_home_score),
      
      Wins = cumsum(Game_Win),
      Losses = cumsum(Game_Loss),
      Runs_Scored_Total = cumsum(Runs_Scored),
      Runs_Allowed_Total = cumsum(Runs_Allowed))
  
  # Run Diff Total
  team_record <- team_record %>%
    mutate(Run_Differential_Total = Runs_Scored_Total - Runs_Allowed_Total) %>%
    mutate(DT = as.POSIXct(date))
  
  # Team Record DF
  team_record <- team_record %>%
    mutate(Game = 1) %>%
    mutate(GameNum = cumsum(Game))
  
  # Pythagorean Winning Percentage - using David Smyth's Pythagenpat formula
  # exponent = [(RS + RA)/G] ^ 0.287
  team_record <- team_record %>%
    mutate(pythagenpat = ((Runs_Scored_Total + Runs_Allowed_Total)/GameNum)^0.287) %>% 
    mutate(Pythag_P = (Runs_Scored_Total^pythagenpat)/((Runs_Scored_Total^pythagenpat) + (Runs_Allowed_Total^pythagenpat))) %>%
    mutate(Winning_p = (Wins)/(Wins + Losses))
  
  # Expected Wins
  team_record <- team_record %>%
    mutate(Ex_wins = Pythag_P * 162)
  
  # Simulate the rest of the season ############################################
  # Fit a Beta Distribution to Pythagorean Win %
  #hist(team_record$Pythag_P)
  pythagorean_beta <- fitdist(team_record$Pythag_P, "beta", method = "mle")
  #summary(pythagorean_beta)
  #plot(pythagorean_beta)
  
  alpha_hat <- pythagorean_beta$estimate["shape1"]
  beta_hat  <- pythagorean_beta$estimate["shape2"]
  
  # Simulate the rest of the season many times
  # include uncert in pythag %
  set.seed(12345)
  n_sims <- 1000
  outer_loops <- 1000
  remaining_games <- 162 - nrow(team_record)
  current_wins <- tail(team_record$Wins, 1)
  
  sim_list <- list()
  # Future Game Number
  future_game_nums <- (nrow(team_record) + 1):162
  
  for (j in 1:outer_loops){
    pythag_prob <- rbeta(1, alpha_hat, beta_hat)
    
    sim_games <- matrix(rbinom(n_sims * remaining_games, 1, pythag_prob),
                        nrow = n_sims, ncol = remaining_games)
    
    sim_cumsum <- t(apply(sim_games, 1, cumsum)) + current_wins  # [n_sims x remaining_games]
    
    # Summarize by game (columns)
    p05 <- apply(sim_cumsum, 2, quantile, probs = 0.05)
    p50 <- apply(sim_cumsum, 2, quantile, probs = 0.50)
    p95 <- apply(sim_cumsum, 2, quantile, probs = 0.95)
    ex_win <- colMeans(sim_cumsum)
    
    sim_list[[j]] <- data.frame(
      GameNum = future_game_nums,
      p05 = p05,
      p50 = p50,
      p95 = p95,
      Ex_win = ex_win,
      SimNum = j)
  }
  
  projected_df <- do.call(rbind, sim_list)
  
  win_projection <- projected_df %>%
    group_by(GameNum) %>%
    summarise(
      median_proj = median(p50),
      median_exp = median(Ex_win),
      p05_med = quantile(p50, 0.05),
      p95_med = quantile(p50, 0.95),
      p05_exp = quantile(Ex_win, 0.05),
      p95_exp = quantile(Ex_win, 0.95),
      .groups = "drop")
  
  # Plot projections
  ggplot() + 
    geom_hline(yintercept = 80,linetype = "dashed")+
    geom_line(data = team_record, aes(x = GameNum, y = Wins)) + 
    geom_line(data = team_record, aes(x = GameNum, y = Ex_wins), color = ex_win_colors[i]) + 
    geom_line(data = win_projection, aes(x = GameNum, y = median_exp), linetype = "dashed",color = "black") + 
    geom_ribbon(data = win_projection,aes(x = GameNum, ymin = p05_exp, ymax = p95_exp),fill = "grey50", alpha = 0.15) +
    scale_y_continuous(breaks = seq(0,150,10), minor_breaks = seq(0,150,2)) + 
    scale_x_continuous(breaks = seq(0,170,10),
                       minor_breaks = seq(0,170,5)) +
    coord_cartesian(ylim = c(0,130))+
    theme_bw() +
    annotate("text", x = max(team_record$GameNum) - 10,
             y = tail(team_record$Ex_wins, 1)+10, 
             label = "(Rolling-Pythagorean Win Projection)", 
             hjust = -0.1, color = ex_win_colors[i],size = 3) +
    labs(x = "Game Number", y = "Wins",
         title = paste0(teamname," Win Projection - ",today))
  
  dir.create(paste0("D:/zz.Fun/Baseball/Baseball-Stuff/ProjectionFigs/",teamname,"/"), showWarnings = FALSE, recursive = TRUE)
  ggsave(paste0("D:/zz.Fun/Baseball/Baseball-Stuff/ProjectionFigs/",teamname,"/2025_Win_Proj_Uncert - ", today, ".png"),height = 5, width = 7, dpi=300)

  # Save Team Projections
  season_win_totals$Expected_Wins[i] <- tail(win_projection$median_exp, n = 1)
  season_win_totals$E95_Wins[i] <- tail(win_projection$p95_exp, n = 1)
  season_win_totals$E05_Wins[i] <- tail(win_projection$p05_exp, n = 1)
}

# Export Projections
#wager <- c("Over","Over","Over","Over","Over","Under","Over",NA)
#season_win_totals <- season_win_totals %>% 
#  mutate(Over_Under = wager)

season_win_totals <- season_win_totals %>% 
  arrange(desc(Expected_Wins))

write.csv(season_win_totals,paste0("D:/zz.Fun/Baseball/Baseball-Stuff/Projections/Win_Projections_",today,".csv"),row.names = F)
