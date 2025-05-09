rm(list = ls(all.names = TRUE))
today = format(Sys.Date(),"%d-%b-%Y")

# Load libraries
library(baseballr)
library(tidyverse)
library(lubridate)
library(zoo)


# Get all MLB games for 2025
schedule_2025 <- mlb_schedule(season = 2025)

teamname <- "New York Mets"

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

# Run Diff Total Plot
# Automatic date spacing
max_DT <- team_record %>% 
  slice_max(DT, n=1) %>% pull(DT)

unqiue_months <- unique(month(team_record$DT))
unqiue_months <- unqiue_months + 1

season_start <- as.POSIXct("2025-03-27")
first_of_da_month <- as.POSIXct(paste0("2025-",unqiue_months,"-01"))
month_brks <- seq(first_of_da_month[1],first_of_da_month[length(unqiue_months)],"1 week")
dt_brks <- c(season_start,month_brks)

ggplot(team_record) + 
  geom_line(aes(x = DT, y = Run_Differential_Total)) + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-10,60,10), minor_breaks = seq(-10,60,2), limits = c(-10,60)) + 
  scale_x_datetime(breaks = dt_brks,
                   date_minor_breaks = "1 day",
                   date_labels = "%m-%d") +
  theme_bw() + 
  labs(x = "Date", y = "Run Differential", title = "2025 Mets Run Diff")

# Pythagorean Winning Percentage
team_record <- team_record %>%
  mutate(Pythag_P = (Runs_Scored_Total^2)/((Runs_Scored_Total^2) + (Runs_Allowed_Total^2))) %>%
  mutate(Winning_p = (Wins)/(Wins + Losses))

ggplot(team_record) + 
  geom_line(aes(x = DT, y = Winning_p)) + 
  geom_line(aes(x = DT, y = Pythag_P), color = "blue2") + 
  scale_y_continuous(breaks = seq(0,1,.1), minor_breaks = seq(0,1,0.05), limits = c(0,1)) + 
  scale_x_datetime(breaks = dt_brks,
                   date_minor_breaks = "1 day",
                   date_labels = "%m-%d") +
  theme_bw() + 
  labs(x = "Date", y = "%", title = "2025 Mets Win and Pythag %")

# Expected Wins
team_record <- team_record %>%
  mutate(Ex_wins = Pythag_P * 162)

ggplot(team_record) + 
  geom_line(aes(x = DT, y = Wins)) + 
  geom_line(aes(x = DT, y = Ex_wins), color = "blue2") + 
  scale_y_continuous(breaks = seq(0,150,10), minor_breaks = seq(0,150,5)) + 
  scale_x_datetime(breaks = dt_brks,
                   date_minor_breaks = "1 day",
                   date_labels = "%m-%d") +
  coord_cartesian(ylim = c(0,140))+
  geom_hline(yintercept = 120,linetype = "dashed")+
  theme_bw() + 
  labs(x = "Date", y = "Wins", title = "2025 Mets Wins & EWins")

team_record <- team_record %>%
  mutate(Game = 1) %>%
  mutate(GameNum = cumsum(Game))

ggplot(team_record) + 
  geom_line(aes(x = GameNum, y = Wins)) + 
  geom_line(aes(x = GameNum, y = Ex_wins), color = "blue2") + 
  scale_y_continuous(breaks = seq(0,150,10), minor_breaks = seq(0,150,5)) + 
  scale_x_continuous(breaks = seq(0,165,5),
                   minor_breaks = seq(0,165,1)) +
  coord_cartesian(ylim = c(0,140))+
  geom_hline(yintercept = 90,linetype = "dashed", color = "orange2")+
  geom_hline(yintercept = 100,linetype = "dashed", color = "orange3")+
  theme_bw() + 
  labs(x = "Game Number", y = "Wins", title = paste0("2025 Mets Wins & EWins - ",today))
ggsave(paste0("D:/zz.Fun/Baseball/Figs/Mets/Mets_2025_ExWins - ", today, ".png"),height = 5, width = 7, dpi=300)

team_record %>%
  mutate(Ex_wins = Pythag_P * 36) %>% pull(Ex_wins)

# Simulate the rest of the season ##############################################
# there could be something here to simulate based on completed games as well - come back later

# Number of games played
games_played <- nrow(team_record)
total_games <- 162
remaining_games <- total_games - games_played

# Use Current Pythag winning %
pythag_win_pct <- tail(team_record$Pythag_P, 1)

# 10-game trailing window of mean Pythag Win %
game_window <- 10
pythag_10_games <- rollmean(team_record$Pythag_P, k = game_window, align = "right", fill = NA)

# Most Recent
hist(team_record$Pythag_P)

alpha <- pythag_win_pct * game_window
beta <- (1 - pythag_win_pct) * game_window

# Simulate the rest of the season many times
# include uncert in pythag %
set.seed(12345)
n_sims <- 100000
remaining_games <- 162 - nrow(team_record)
current_wins <- tail(team_record$Wins, 1)

sim_list <- list()
outer_loops <- length(team_record$Pythag_P)

# loop last 10 pythags or all 
for (i in 1:outer_loops){
  # Grab each of the last ten for the sim
  #pythag_prob <- team_record$Pythag_P[nrow(team_record) - i]
  pythag_prob <- team_record$Pythag_P[i]
  
  sim_results <- replicate(n_sims, {
    # Simulate remaining games
    wins_future <- rbinom(remaining_games, 1, pythag_prob)
    cumsum(wins_future) + current_wins
  })
  
  future_game_nums <- (nrow(team_record) + 1):162
  quantiles <- apply(sim_results, 1, quantile, probs = c(0.05, 0.5, 0.95))
  ex_wins <- apply(sim_results, 1, mean)
  
  projectedwins_df <- data.frame(
    GameNum = future_game_nums,
    p05 = quantiles[1, ],
    p50 = quantiles[2, ],
    p95 = quantiles[3, ],
    Ex_win = ex_wins,
    sim_num = i)
  
  sim_list[[i]] <- projectedwins_df
}

proj_df <- bind_rows(sim_list)

proj_summary <- proj_df %>%
  group_by(GameNum) %>%
  summarise(
    p05 = mean(p05),
    p50 = mean(p50),
    p95 = mean(p95),
    Ex_wins = mean(Ex_win),
    .groups = "drop")

# Singular realization
# sim_results <- replicate(n_sims, {
#   # Sample win prob from beta distribution
#   win_prob_i <- rbeta(1, alpha, beta)
#   
#   # Simulate remaining games
#   wins_future <- rbinom(remaining_games, 1, win_prob_i)
#   cumsum(wins_future) + current_wins
# })
# 
# # Convert to a tidy data frame for plotting
# future_game_nums <- (nrow(team_record) + 1):162
# quantiles <- apply(sim_results, 1, quantile, probs = c(0.05, 0.5, 0.95))
# 
# projectedwins_df <- data.frame(
#   GameNum = future_game_nums,
#   p05 = quantiles[1, ],
#   p50 = quantiles[2, ],
#   p95 = quantiles[3, ]
# )

# Append Existing Games as NA to the top of this
dummy_rows <- data.frame(
  GameNum = 1:games_played,
  p05 = NA,
  p50 = NA,
  p95 = NA)

remaining_projections <- bind_rows(dummy_rows,projectedwins_df)

# Join with Team Record by Game Num - DUH this doesnt work
#team_record <- team_record %>% left_join(remaining_projections, by = "GameNum")

#projectedwins_df <- proj_summary %>% filter(row_number() %% 2 == 0)

# Plot projections
ggplot() + 
  geom_hline(yintercept = 90,linetype = "dashed")+
  geom_hline(yintercept = 100,linetype = "dashed")+
  geom_hline(yintercept = 110,linetype = "dashed")+
  geom_line(data = team_record, aes(x = GameNum, y = Wins)) + 
  geom_line(data = team_record, aes(x = GameNum, y = Ex_wins), color = "blue2") + 
  geom_line(data = proj_summary, aes(x = GameNum, y = Ex_wins), linetype = "dashed",color = "orange3") + 
  geom_ribbon(data = proj_summary,aes(x = GameNum, ymin = p05, ymax = p95),fill = "orange3", alpha = 0.15) +
  scale_y_continuous(breaks = seq(0,150,10), minor_breaks = seq(0,150,5)) + 
  scale_x_continuous(breaks = seq(0,170,10),
                     minor_breaks = seq(0,170,5)) +
  coord_cartesian(ylim = c(0,130))+
  theme_bw() +
  annotate("text", x = 100, y = 94,label = "90 Wins (-400)") +
  annotate("text", x = 100, y = 103,label = "100 Wins (+225)") +
  annotate("text", x = 100, y = 113,label = "100 Wins (+1700)") +
  annotate("text", x = max(team_record$GameNum) - 10, y = tail(team_record$Ex_wins, 1)+10, label = "(Rolling-Pythagorean Win Projection)", hjust = -0.1, color = "blue2",size = 3) +
  labs(x = "Game Number", y = "Wins", title = paste0("2025 Mets Win Projection - ",today))
ggsave(paste0("D:/zz.Fun/Baseball/Figs/Mets/Mets_2025_Win_Proj_Uncert - ", today, ".png"),height = 5, width = 7, dpi=300)

