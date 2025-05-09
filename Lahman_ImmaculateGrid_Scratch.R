library(Lahman)
library(tidyverse)

# Phillies Pitcher with <3.00 ERA
# use Teams instead of Franchise
PHI_ID <- as.character(Lahman::TeamsFranchises$franchID[grep("*Phillies*",Lahman::TeamsFranchises$franchName)])

phils_low_era <- Lahman::People %>%
  select(playerID, nameFirst, nameLast, birthCountry) %>% 
  mutate(fullName = paste(nameFirst,nameLast)) %>% 
  inner_join(Lahman::Pitching, by = "playerID") %>%
  filter(teamID == PHI_ID) %>%
  filter(ERA < 3.0) %>%
  slice_max(ERA,n = 10)
plot(phils_low_era$G,phils_low_era$W)

# Nats and Dodgers Players
LA_id <- as.character(unique(Lahman::Teams$teamID[grep("Dodgers", Lahman::Teams$name)]))
Nats_id_all <- Lahman::Teams[grep("Nationals", Lahman::Teams$name),]
Nats_id <- as.character(unique(Nats_id_all$teamID[Nats_id_all$yearID >= 2005]))
expos_id <- as.character(unique(Lahman::Teams$teamID[grep("Montreal", Lahman::Teams$name)]))

# Sum Games played for each team
dodgers <- Lahman::Batting %>%
  filter(teamID == LA_id[1] | teamID == LA_id[2]) %>%
  group_by(playerID) %>%
  summarize(total_G_LA = sum(G, na.rm = TRUE), .groups = "drop")

nats <- Lahman::Batting %>%
  filter(teamID == Nats_id | teamID == expos_id) %>%
  group_by(playerID) %>%
  summarize(total_G_Nats = sum(G, na.rm = TRUE), .groups = "drop")

# Join them together
both_teams_games <- inner_join(dodgers,nats, by = "playerID",relationship = "many-to-many")

# Get their names and the top 10 total games played
both_teams <- Lahman::People %>% 
  select(playerID, nameFirst, nameLast, birthCountry) %>% 
  mutate(fullName = paste(nameFirst,nameLast)) %>%
  inner_join(both_teams_games, by = "playerID") %>% 
  mutate(Total_MLB_Games = total_G_Nats + total_G_LA)

both_teams %>% slice_max(Total_MLB_Games, n = 10)


# TB and SF, Pitt
TB_id <- as.character(unique(Lahman::Teams$teamID[grep("Rays", Lahman::Teams$name)]))
SF_id <- as.character(unique(Lahman::Teams$teamID[grep("Giants", Lahman::Teams$name)]))[3]
Pitt_id <- as.character(unique(Lahman::Teams$teamID[grep("Pirates", Lahman::Teams$name)]))[2]

tb <- Lahman::Batting %>%
  group_by(playerID) %>%
  filter(teamID == TB_id) %>%
  summarize(total_G_TB = sum(G, na.rm = TRUE), .groups = "drop")

sf <- Lahman::Batting %>%
  group_by(playerID) %>%
  filter(teamID == SF_id) %>%
  summarize(total_G_SF = sum(G, na.rm = TRUE), .groups = "drop")

pitt <- Lahman::Batting %>%
  group_by(playerID) %>%
  filter(teamID == Pitt_id) %>%
  summarize(total_G_pitt = sum(G, na.rm = TRUE), .groups = "drop")

people_subset <- Lahman::People %>% 
  select(playerID, nameFirst, nameLast, birthCountry) %>% 
  mutate(fullName = paste(nameFirst,nameLast))

tb_sf_pitt <- left_join(tb,sf, by = "playerID") %>%
  left_join(pitt, by = "playerID") %>%
  inner_join(people_subset, by = "playerID") %>%
  mutate(TB_SF_G = ifelse(!is.na(total_G_TB) & !is.na(total_G_SF),total_G_TB+total_G_SF,NA),
         TB_Pitt_G = ifelse(!is.na(total_G_TB) & !is.na(total_G_pitt),total_G_TB+total_G_pitt,NA))

tb_sf_pitt %>% slice_max(TB_SF_G, n = 5)
tb_sf_pitt %>% slice_max(TB_Pitt_G, n = 5)

# Cubs and Pitt
Cubs_id <- as.character(unique(Lahman::Teams$teamID[grep("Cubs", Lahman::Teams$name)]))
cubs <- Lahman::Batting %>%
  group_by(playerID) %>%
  filter(teamID == Cubs_id) %>%
  summarize(total_G_cubs = sum(G, na.rm = TRUE), .groups = "drop")

cubs_pitt <- inner_join(cubs,pitt, by = "playerID") %>%
  inner_join(people_subset, by = "playerID") %>% 
  mutate(Cubs_Pitt_G = total_G_cubs + total_G_pitt)
cubs_pitt %>% slice_max(Cubs_Pitt_G, n = 5)

# SF and Pitt 2nd baseman
sf_2b <- Lahman::Fielding %>%
  filter(teamID == SF_id) %>%
  filter(POS == "2B") %>% 
  arrange(desc(G)) %>% 
  filter(!is.na(G)) %>%
  slice_max(G,n = 10) %>%
  inner_join(people_subset, by = "playerID")

# Mets x SF, Cubs, Houston
mets_id <- as.character(unique(Lahman::Teams$teamID[grep("Mets", Lahman::Teams$name)]))

mets <- Lahman::Batting %>%
  filter(teamID == mets_id) %>%
  group_by(playerID) %>%
  summarize(total_G_mets = sum(G, na.rm = TRUE), .groups = "drop")

sf <- Lahman::Batting %>%
  filter(teamID == SF_id) %>%
  group_by(playerID) %>%
  summarize(total_G_SF = sum(G, na.rm = TRUE), .groups = "drop")

cubs <- Lahman::Batting %>%
  filter(teamID == Cubs_id) %>%
  group_by(playerID) %>%
  summarize(total_G_Cubs = sum(G, na.rm = TRUE), .groups = "drop")
  
houst <- Lahman::Batting %>%
  filter(teamID == as.character(unique(Lahman::Teams$teamID[grep("Houston", Lahman::Teams$name)]))) %>%
  group_by(playerID) %>%
  summarize(total_G_Hou = sum(G, na.rm = TRUE), .groups = "drop")

mets_colm <- left_join(mets,sf, by = "playerID") %>%
  left_join(cubs, by = "playerID") %>%
  left_join(houst, by = "playerID") %>%
  mutate(mets_SF_G = ifelse(!is.na(total_G_mets) & !is.na(total_G_SF),total_G_mets+total_G_SF,NA),
         mets_cubs_G  = ifelse(!is.na(total_G_mets) & !is.na(total_G_Cubs),total_G_mets+total_G_Cubs,NA),
         mets_hou_G  = ifelse(!is.na(total_G_mets) & !is.na(total_G_Hou),total_G_mets+total_G_Hou,NA)
         ) %>% 
  inner_join(people_subset, by = "playerID")

mets_colm %>% slice_max(mets_SF_G, n = 5)
mets_colm %>% slice_max(mets_cubs_G, n = 5)
mets_colm %>% slice_max(mets_hou_G, n = 5) %>% arrange(desc(mets_hou_G))

# Cinncy x SF, Cubs, Houston 
reds_id <- as.character(unique(Lahman::Teams$teamID[grep("Reds", Lahman::Teams$name)]))[4]

reds <- Lahman::Batting %>%
  filter(teamID == reds_id) %>%
  group_by(playerID) %>%
  summarize(total_G_reds = sum(G, na.rm = TRUE), .groups = "drop")

reds_colm <- left_join(reds,sf, by = "playerID") %>%
  left_join(cubs, by = "playerID") %>%
  left_join(houst, by = "playerID") %>%
  mutate(reds_SF_G = ifelse(!is.na(total_G_reds) & !is.na(total_G_SF),total_G_reds+total_G_SF,NA),
         reds_cubs_G  = ifelse(!is.na(total_G_reds) & !is.na(total_G_Cubs),total_G_reds+total_G_Cubs,NA),
         reds_hou_G  = ifelse(!is.na(total_G_reds) & !is.na(total_G_Hou),total_G_reds+total_G_Hou,NA)
  ) %>% 
  inner_join(people_subset, by = "playerID")

reds_colm %>% slice_max(reds_SF_G, n = 5) %>% arrange(desc(reds_SF_G))
reds_colm %>% slice_max(reds_cubs_G, n = 5) %>% arrange(desc(reds_SF_G))
reds_colm %>% slice_max(reds_hou_G, n = 5) %>% arrange(desc(reds_SF_G))

# 30 Stolen Bases x SF, Cubs, Houston 
stolen30 <- Lahman::Batting %>%
  filter(SB > 30)

stolen30_sf <- stolen30 %>% 
  filter(teamID == SF_id) %>% 
  inner_join(people_subset, by = "playerID") %>% 
  slice_max(SB, n = 10) %>%
  arrange(desc(SB))

stolen30_cubs <- stolen30 %>% 
  filter(teamID == Cubs_id) %>%
  inner_join(people_subset, by = "playerID") %>% 
  filter(yearID > 1990) %>% 
  slice_max(SB, n = 10) %>%
  arrange(desc(SB))

stolen30_houst <- stolen30 %>% 
  filter(teamID == as.character(unique(Lahman::Teams$teamID[grep("Houston", Lahman::Teams$name)]))) %>%
  inner_join(people_subset, by = "playerID")%>% 
  slice_max(SB, n = 10) %>%
  arrange(desc(SB))

# Baltimore players born outside us
balt <- Lahman::Batting %>% 
  filter(teamID == "BAL") %>%
  inner_join(people_subset, by = "playerID") %>% 
  filter(birthCountry != "USA") %>%
  slice_max(G, n = 10) %>%
  arrange(desc(G))

# Cinncy and balt, A's
balt <- Lahman::Batting %>% filter(teamID == "BAL") %>%
  group_by(playerID) %>%
  summarize(total_G_balt = sum(G, na.rm = TRUE), .groups = "drop")

oak <- Lahman::Batting %>% filter(teamID == "OAK") %>%
  group_by(playerID) %>%
  summarize(total_G_oak = sum(G, na.rm = TRUE), .groups = "drop")

reds_balt <- Lahman::Batting %>%
  filter(teamID == reds_id) %>% 
  group_by(playerID) %>%
  summarize(total_G_reds = sum(G, na.rm = TRUE), .groups = "drop") %>% 
  inner_join(balt, by = "playerID") %>% 
  mutate(Total_G = total_G_reds + total_G_balt) %>%
  inner_join(people_subset, by = "playerID")%>% 
  slice_max(Total_G, n = 10) %>% 
  arrange(desc(Total_G))

reds_oak <- Lahman::Batting %>%
  filter(teamID == reds_id) %>% 
  group_by(playerID) %>%
  summarize(total_G_reds = sum(G, na.rm = TRUE), .groups = "drop") %>% 
  inner_join(oak, by = "playerID") %>% 
  mutate(Total_G = total_G_reds + total_G_oak) %>%
  inner_join(people_subset, by = "playerID")%>% 
  slice_max(Total_G, n = 10) %>% 
  arrange(desc(Total_G))
  
# Braves and Atlanta
atl <- Lahman::Batting %>% filter(teamID == "ATL") %>%
  group_by(playerID) %>%
  summarize(total_G_atl = sum(G, na.rm = TRUE), .groups = "drop")

oak_atl <- Lahman::Batting %>% filter(teamID == "OAK") %>%
  group_by(playerID) %>%
  summarize(total_G_oak = sum(G, na.rm = TRUE), .groups = "drop") %>% 
  inner_join(atl, by = "playerID") %>% 
  mutate(Total_G = total_G_oak + total_G_atl) %>%
  inner_join(people_subset, by = "playerID")%>% 
  slice_max(Total_G, n = 10) %>% 
  arrange(desc(Total_G))

# Grid 706 ####################################################################
# Cols: rookie of the year, 3rd base, silver slugger
# Rows: played OF, 6+ WAR, 40+ HR
