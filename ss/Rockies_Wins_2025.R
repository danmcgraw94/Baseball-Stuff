# Load libraries
library(baseballr)
library(tidyverse)
library(lubridate)
library(zoo)

# Get all MLB games for 2025
today <- format(Sys.Date(),"%d-%b-%Y")
schedule_2025 <- mlb_schedule(season = 2025)

# Overall Record & Runs for and against
team_record <- schedule_2025 %>%
  filter(
    teams_home_team_name == "Colorado Rockies" | teams_away_team_name == "Colorado Rockies",
    game_type == "R", 
    status_coded_game_state == "F") %>%
  mutate(
    Game_Win = case_when(
      teams_home_team_name == "Colorado Rockies" & teams_home_is_winner ~ 1,
      teams_away_team_name == "Colorado Rockies" & !teams_home_is_winner ~ 1,
      TRUE ~ 0),
    Game_Loss = 1 - Game_Win,
    
    Runs_Scored = case_when(
      teams_home_team_name == "Colorado Rockies" ~ teams_home_score,
      teams_away_team_name == "Colorado Rockies" ~ teams_away_score),
    Runs_Allowed = case_when(
      teams_home_team_name == "Colorado Rockies" ~ teams_away_score,
      teams_away_team_name == "Colorado Rockies" ~ teams_home_score),
    
    Wins = cumsum(Game_Win),
    Losses = cumsum(Game_Loss),
    Runs_Scored_Total = cumsum(Runs_Scored),
    Runs_Allowed_Total = cumsum(Runs_Allowed))

# Filter for Rockies Home games
# rockies_home_games <- team_record %>%
#   filter(teams_home_team_name == "Colorado Rockies")

# Filter completed regular season games
# rockies_home_games <- rockies_home_games %>%
#   filter(game_type == "R", 
#          status_coded_game_state == "F") %>%
#   mutate(Game_Win = ifelse(teams_home_is_winner == T,1,0),
#          Game_Loss = ifelse(teams_home_is_winner == T,0,1))

# Replace these with the actual dates you attended
attended_dates <- as.Date(c("2025-04-05", "2025-04-09", "2025-04-19",
                            "2025-04-26", "2025-04-27", "2025-04-28",
                            "2025-04-29"))

# Add Column for Games I went to
team_record <- team_record %>%
  mutate(Attended = ifelse(date %in% attended_dates,1,0)) %>%
  mutate(Attended_Total = cumsum(Attended))

# Plot Runs differential with and without attendance
team_record %>%
  mutate(attendance = ifelse(Attended == 1, "Present", "Absent"),
    result = ifelse(Game_Win == 1, "Win", "Loss"))  %>%
  ggplot() + 
  geom_jitter(aes(x = Runs_Allowed,
                 y = Runs_Scored,
                 color = attendance,
                 fill = attendance),
              size = 2, width = 0.1, height = 0.1) +
  scale_x_continuous(breaks = seq(0, 30, 2),limits = c(0,18)) +
  scale_y_continuous(breaks = seq(0, 30, 2),limits = c(0,14)) +
  scale_fill_manual(name = "Attendance",
                    values = c("Absent" = "black", "Present" = "purple2")) +
  scale_color_manual(name = "Attendance",
                    values = c("Absent" = "black", "Present" = "purple2")) +
  labs(x = "Runs Allowed", y = "Runs Scored", title = "Rockies 2025 (so far)") +
  geom_abline(intercept = 0, slope = 1) +
  annotate("text", x = 1.75, y = 10, label = "Total Wins = 5") +
  annotate("text", x = 14, y = 8, label = "Record with Dan Attending = 0-7") +
  theme_bw()

# Misery Index Plot?
# Automatic date spacing
team_record <- team_record %>% mutate(DT = as.POSIXct(date))

max_DT <- team_record %>% 
  slice_max(DT, n=1) %>% pull(DT)

unqiue_months <- unique(month(team_record$DT))
unqiue_months <- unqiue_months + 1

season_start <- as.POSIXct("2025-03-27")
first_of_da_month <- as.POSIXct(paste0("2025-",unqiue_months,"-01"))
month_brks <- seq(first_of_da_month[1],first_of_da_month[length(unqiue_months)],"1 week")
dt_brks <- c(season_start,month_brks)

# Run Diff Total
team_record <- team_record %>%
  mutate(Run_Differential_Total = Runs_Scored_Total - Runs_Allowed_Total) %>%
  mutate(DT = as.POSIXct(date))

# Run Diff Total Plot
ggplot(team_record) + 
  geom_line(aes(x = DT, y = Run_Differential_Total)) + 
  geom_line(data = filter(team_record, Attended == 1),
            aes(x = DT, y = Run_Differential_Total), color = "purple2") + 
  geom_point(data = filter(team_record, Attended == 1),
            aes(x = DT, y = Run_Differential_Total), fill = "purple2",color = "purple2") + 
  scale_y_continuous(breaks = seq(-100,50,10), minor_breaks = seq(-100,50,2), limits = c(-100,10)) + 
  scale_x_datetime(breaks = dt_brks,
                   date_minor_breaks = "1 day",
                   date_labels = "%m-%d") +
  theme_bw() + 
  labs(x = "Date", y = "Run Differential", title = "2025 Rockies Misery Index")

# Pythagorean Winning Percentage
team_record <- team_record %>%
  mutate(Pythag_P = (Runs_Scored_Total^2)/((Runs_Scored_Total^2) + (Runs_Allowed_Total^2))) %>%
  mutate(Winning_p = (Wins)/(Wins + Losses))

ggplot(team_record) + 
  geom_line(aes(x = DT, y = Winning_p)) + 
  geom_line(aes(x = DT, y = Pythag_P), color = "purple2") + 
  scale_y_continuous(breaks = seq(0,1,.1), minor_breaks = seq(0,1,0.05), limits = c(0,1)) + 
  scale_x_datetime(breaks = dt_brks,
                   date_minor_breaks = "1 day",
                   date_labels = "%m-%d") +
  theme_bw() + 
  labs(x = "Date", y = "%", title = "2025 Rockies Winning & Pythag %")

# Expected Wins
team_record <- team_record %>%
  mutate(Ex_wins = Pythag_P * 162)

team_record <- team_record %>%
  mutate(Game = 1) %>%
  mutate(GameNum = cumsum(Game))


ggplot(team_record) + 
  geom_hline(yintercept = 20,linetype = "dashed",color = "red3") +
  geom_hline(yintercept = 60.5,linetype = "dashed",color = "green3") +
  geom_line(aes(x = GameNum, y = Wins)) + 
  geom_line(aes(x = GameNum, y = Ex_wins), color = "purple2") + 
  scale_y_continuous(breaks = seq(0,100,10), minor_breaks = seq(0,100,5)) + 
  scale_x_continuous(breaks = seq(0,165,5),
                     minor_breaks = seq(0,165,1)) +
  coord_cartesian(ylim = c(0,70))+
  theme_bw() +
  annotate("text", x = 10, y = 22,label = "1989 Cleveland Spiders: 20") +
  annotate("text", x = 10, y = 62.5,label = "DK Win Total: 60.5 (-110)") +
  labs(x = "Game Number", y = "Wins", title = paste0("2025 Rockies Wins & Rolling Win Projection - ",today))
ggsave(paste0("D:/zz.Fun/Baseball/Figs/Rockies/Rockies_2025_Rolling_Win_Proj - ", today, ".png"),height = 5, width = 7, dpi=300)


# Simulate the rest of the season ##############################################
# there could be something here to simulate based on completed games as well - come back later

# Number of games played
games_played <- nrow(team_record)
total_games <- 162
remaining_games <- total_games - games_played

# Use Current Pythag winning %
win_prob <- tail(team_record$Pythag_P, 1)

# 10-game trailing window of mean Pythag Win %
game_window <- 10
pythag_10_games <- rollmean(team_record$Pythag_P, k = game_window, align = "right", fill = NA)

# Most Recent
hist(team_record$Pythag_P)
pythag_win_pct <- tail(pythag_10_games, 1)

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

# loop last 10 pythags
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
  
  projectedwins_df <- data.frame(
    GameNum = future_game_nums,
    p05 = quantiles[1, ],
    p50 = quantiles[2, ],
    p95 = quantiles[3, ],
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
    .groups = "drop")

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

# Could make a legend easier
# team_lines <- rbind(
#   data.frame(GameNum = team_record$GameNum, Value = team_record$Wins, Label = "Wins"),
#   data.frame(GameNum = team_record$GameNum, Value = team_record$Ex_wins, Label = "Rolling Pythagorean Win Projection"),
#   data.frame(GameNum = proj_summary$GameNum, Value = proj_summary$p50, Label = "Median Projected Wins")
# )

ggplot() + 
  geom_hline(yintercept = 20,linetype = "dashed",color = "red3") +
  geom_hline(yintercept = 60.5,linetype = "dashed",color = "green3") +
  geom_line(data = team_record, aes(x = GameNum, y = Wins)) + 
  geom_line(data = team_record, aes(x = GameNum, y = Ex_wins), color = "purple2") + 
  geom_line(data = proj_summary, aes(x = GameNum, y = p50), linetype = "dashed") + 
  geom_ribbon(data = proj_summary,aes(x = GameNum, ymin = p05, ymax = p95),fill = "purple", alpha = 0.15) +
  #geom_line(data = projectedwins_df, aes(x = GameNum, y = p50), linetype = "dashed") + 
  scale_y_continuous(breaks = seq(0,100,10), minor_breaks = seq(0,100,5)) + 
  scale_x_continuous(breaks = seq(0,170,10),
                     minor_breaks = seq(0,170,5)) +
  coord_cartesian(ylim = c(0,70))+
  theme_bw() +
  annotate("text", x = 30, y = 22,label = "1989 Cleveland Spiders: 20") +
  annotate("text", x = 30, y = 62.5,label = "DK Win Total: 60.5 (-110)") +
  annotate("text", x = max(team_record$GameNum) - 10, y = tail(team_record$Ex_wins, 1)+10, label = "(Rolling Pythagorean Win Projection)", hjust = -0.1, color = "purple2",size = 3) +
  labs(x = "Game Number", y = "Wins", title = paste0("2025 Rockies Win Projection - ",today))
ggsave(paste0("D:/zz.Fun/Baseball/Figs/Rockies/Rockies_2025_Win_Proj_Uncert - ", today, ".png"),height = 5, width = 7, dpi=300)

