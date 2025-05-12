rm(list = ls(all.names = TRUE))
today = format(Sys.Date(),"%d-%b-%Y")

# Load libraries
library(baseballr)
library(tidyverse)
library(lubridate)

# Get all MLB games for 2025
schedule_2025 <- mlb_schedule(season = 2025)

teamname <- "Boston Red Sox"

# Overall Record & Runs for and against
redsox_record <- schedule_2025 %>%
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
redsox_record <- redsox_record %>%
  mutate(Run_Differential_Total = Runs_Scored_Total - Runs_Allowed_Total) %>%
  mutate(DT = as.POSIXct(date))

# Run Diff Total Plot
# Automatic date spacing
max_DT <- redsox_record %>% 
  slice_max(DT, n=1) %>% pull(DT)

unqiue_months <- unique(month(redsox_record$DT))
unqiue_months <- unqiue_months + 1

season_start <- as.POSIXct("2025-03-27")
first_of_da_month <- as.POSIXct(paste0("2025-",unqiue_months,"-01"))
month_brks <- seq(first_of_da_month[1],first_of_da_month[length(unqiue_months)],"1 week")
dt_brks <- c(season_start,month_brks)

ggplot(redsox_record) + 
  geom_line(aes(x = DT, y = Run_Differential_Total)) + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-50,50,10), minor_breaks = seq(-50,50,2), limits = c(-25,25)) + 
  scale_x_datetime(breaks = dt_brks,
                   date_minor_breaks = "1 day",
                   date_labels = "%m-%d") +
  theme_bw() + 
  labs(x = "Date", y = "Run Differential", title = "2025 Red Sox Run Diff")

# Pythagorean Winning Percentage
redsox_record <- redsox_record %>%
  mutate(Pythag_P = (Runs_Scored_Total^2)/((Runs_Scored_Total^2) + (Runs_Allowed_Total^2))) %>%
  mutate(Winning_p = (Wins)/(Wins + Losses))

ggplot(redsox_record) + 
  geom_line(aes(x = DT, y = Winning_p)) + 
  geom_line(aes(x = DT, y = Pythag_P), color = "red3") + 
  scale_y_continuous(breaks = seq(0,1,.1), minor_breaks = seq(0,1,0.05), limits = c(0,1)) + 
  scale_x_datetime(breaks = dt_brks,
                   date_minor_breaks = "1 day",
                   date_labels = "%m-%d") +
  theme_bw() + 
  labs(x = "Date", y = "%", title = "2025 Red Sox Win and Pythag %")

# Expected Wins
redsox_record <- redsox_record %>%
  mutate(Ex_wins = Pythag_P * 162)

ggplot(redsox_record) + 
  geom_line(aes(x = DT, y = Wins)) + 
  geom_line(aes(x = DT, y = Ex_wins), color = "red3") + 
  scale_y_continuous(breaks = seq(0,150,10), minor_breaks = seq(0,150,5)) + 
  scale_x_datetime(breaks = dt_brks,
                   date_minor_breaks = "1 day",
                   date_labels = "%m-%d") +
  coord_cartesian(ylim = c(0,110))+
  theme_bw() + 
  labs(x = "Date", y = "Wins", title = "2025 Red Sox Wins & Ex Wins")


