library(dplyr)
library(ggplot2)
load("gls2017.RData")
View(gls2017)
load("pls2017.RData")
View(pls)

#Making data accessible.
players2017 <-  data.frame()
playing_positions <- data.frame()
teams <- data.frame()
leagues <- data.frame()

for( i in 1:length(pls)) {
  players2017 <- rbind(players2017, pls[[i]]$players)
  playing_positions <- rbind(playing_positions, pls[[i]]$playing_positions)
  teams <- rbind(teams, pls[[i]]$teams)
  leagues <- rbind(leagues, pls[[i]]$leagues)
}

teams <- unique(teams)
playing_positions <- unique(playing_positions)

games <- data.frame()
game_logs <- data.frame()

for( i in 1:length(gls2017)) {
  games <- rbind(games, gls2017[[i]]$games)
  game_logs <- rbind(game_logs, gls2017[[i]]$game_logs)
}

home_teams <- data.frame()
away_teams <- data.frame()
winning_teams <- data.frame()
seasons <- data.frame()

for( i in 1:length(gls2017)) {
  home_teams <- rbind(home_teams, gls2017[[i]]$home_teams)
  away_teams <- rbind(away_teams, gls2017[[i]]$away_teams)
  winning_teams <- rbind(winning_teams, gls2017[[i]]$winning_teams)
  seasons <- rbind(seasons, gls2017[[i]]$seasons)
}

#Making data frame easier to look at.
teams_new <- teams %>% dplyr::select(id, nickname)

games <- unique(games)
games_new <- games %>% dplyr::select(away_team_outcome, home_team_outcome, 
                              away_team_score, home_team_score,
                              winning_team_id, label, scoreline)

#Adding column to define the winning teams.
games_new <- merge(games_new, teams_new, by.x = "winning_team_id", 
                   by.y = "id", all.x = T)
colnames(games_new)[which(names(games_new) == "nickname")] <- "Winning_Team"

games_new <- unique(games_new)
team_heights <- data.frame(table(games_new$Winning_Team))

#Data frame for boxplot of all heights on team.
players_height <- players2017 %>% dplyr::select(height, team_id, position_abbreviation, id)
players_height <- merge(players_height, teams_new, 
                         by.x = "team_id", by.y = "id", all.x = T)
players_height <- filter(players_height, height != 0)

colnames(players_height)[which(names(players_height) == "nickname")] <- "Team_Name"

ggplot(data = players_height, aes(x = reorder(Team_Name, height, FUN = median), y = height)) + 
  geom_boxplot(color='red') + theme(axis.text.x = element_text(size = 8, angle = 45)) + 
  xlab("Teams") + ylab("Height (In.)") + ggtitle("Boxplot of Team Heights with Unnatural Outliers")

#Getting rid of unnatural outliers.
players_height <- filter(players_height, height != 7)
players_height <- filter(players_height, height != 158)

ggplot(data = players_height, aes(x = reorder(Team_Name, height, FUN = median), y = height)) + 
  geom_boxplot(color='blue') + theme(axis.text.x = element_text(size = 8, angle = 45)) + 
  xlab("Teams") + ylab("Height (In.)") + ggtitle("Boxplot of Team Heights") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red")


#Seeing if there is correlation between height and team wins.
players_height_new <- aggregate(height ~ Team_Name,data = players_height, mean, 
          na.action = na.omit)

team_heights <- cbind(team_heights, players_height_new$height)
colnames(team_heights)[which(names(team_heights) == "players_height_new$height")] <- "Team_Height"
team_heights <- team_heights[order(team_heights$Team_Height),]

ggplot(data = team_heights, aes(x = reorder(Var1, Team_Height, FUN = mean), y = Freq)) + 
  geom_bar(stat = "identity", aes(fill = Team_Height)) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  xlab("Teams") + ylab("Number of Wins") + ggtitle("Team Wins in Order of Mean Height")

mean(team_heights$Team_Height)

#Seeing if different positions tend to have different heights.
ggplot(data = players_height, aes(x = reorder(position_abbreviation, height, FUN = median), 
                                  y = height)) + 
  geom_boxplot(color='blue') + theme(axis.text.x = element_text(size = 8, angle = 45)) + 
  xlab("Position") + ylab("Height (In.)") + ggtitle("Boxplot of All Position Heights") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red")

#Checking if their is correlation between team wins and shortest position (2B).
SP <- filter(players_height, position_abbreviation == "SP")
SecondB <- filter(players_height, position_abbreviation == "2B")
quantile(SP$height, .8) 
quantile(SecondB$height, .2)
SP <- filter(SP, height >= 76)
SecondB <- filter(SecondB, height <= 70)

ggplot(data = SecondB, aes(x = reorder(Team_Name, height, FUN = median), 
                      y = height)) + 
  geom_boxplot(color='blue') + theme(axis.text.x = element_text(size = 8, angle = 45)) + 
  xlab("Team") + ylab("Height") + ggtitle("Boxplot of Shortest 20% 2B's Heights on Teams") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red")

SecondB_mean <- aggregate(height ~ Team_Name,data = SecondB, mean, 
                                na.action = na.omit)
SecondB_mean <- cbind(SecondB_mean, team_heights$Freq)
colnames(SecondB_mean)[which(names(SecondB_mean) == "team_heights$Freq")] <- "Wins"

ggplot(data = SecondB_mean, aes(x = reorder(Team_Name, height, FUN = mean), y = Wins)) + 
  geom_bar(stat = "identity", aes(fill = height)) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  xlab("Teams") + ylab("Number of Wins") + ggtitle("Team Wins in regards to Shortest 20% 2B")

#Checking if their is correlation between team wins and tallest position (SP).
ggplot(data = SP, aes(x = reorder(Team_Name, height, FUN = median), 
                                  y = height)) + 
  geom_boxplot(color='blue') + theme(axis.text.x = element_text(size = 8, angle = 45)) + 
  xlab("Team") + ylab("Height") + ggtitle("Boxplot of SP's Heights on Teams") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red")

SP_mean <- aggregate(height ~ Team_Name,data = SP, mean, 
                     na.action = na.omit)
SP_mean <- cbind(SP_mean, team_heights$Freq)
colnames(SP_mean)[which(names(SP_mean) == "team_heights$Freq")] <- "Wins"

ggplot(data = SP_mean, aes(x = reorder(Team_Name, height, FUN = mean), y = Wins)) + 
  geom_bar(stat = "identity", color='green') +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  xlab("Teams") + ylab("Number of Wins") + ggtitle("Team Wins in regards to Tallest 20% 2B")

#Creating Metrics.
game_logs_metrics <- game_logs %>% dplyr::select(outfield_assists, fielding_errors,
                                          wild_pitches, balls_thrown, pitcher_hit_by_pitch,
                                          pitches_thrown, strikes_thrown, innings_pitched,
                                          stolen_bases, caught_stealing, total_bases, 
                                          home_runs, hits, player_id, 
                                          game_played, pitcher_hits, balls_thrown, strike_percentage)

game_logs_played <- game_logs_metrics %>%
  group_by(player_id) %>%
  summarise(games_played = sum(game_played, na.rm = T),
            total_assists = sum(outfield_assists, na.rm = T),
            total_stolen = sum(stolen_bases, na.rm = T),
            total_bases_sum = sum(total_bases, na.rm = T),
            total_HR = sum(home_runs, na.rm = T),
            total_hits = sum(hits, na.rm = T))

game_logs_played <- merge(game_logs_metrics, game_logs_played, by.x = "player_id", 
                          by.y = "player_id", all.x = T)

game_logs_played <- filter(game_logs_played, games_played >= 40)


Metric <- merge(game_logs_played, players_height, by.x = "player_id", by.y = "id", all.x = T)
Metric$Fielder <- Metric$outfield_assists/Metric$total_assists
Metric$Bad_Throw_Percentage <- (Metric$wild_pitches + Metric$balls_thrown + 
                                  Metric$pitcher_hit_by_pitch)/Metric$pitches_thrown
Metric$Balls_Percentage <- (Metric$balls_thrown/Metric$pitches_thrown)
Metric$Steal_Effectiveness <- Metric$total_stolen/Metric$total_bases_sum
Metric$Prob_Home_Run_For_Hits <- Metric$home_runs/Metric$total_HR

#Proportion of Assists per Game
Fielding_Wrangle <- Metric %>% dplyr::select(team_id, height, position_abbreviation, 
                                       Team_Name, player_id, Fielder)
table(Fielding_Wrangle$position_abbreviation)

Fielding_Wrangle <- filter(Fielding_Wrangle, position_abbreviation != "RP")

Fielding_Wrangle <- na.omit(Fielding_Wrangle)
c1 <- cut(Fielding_Wrangle$height, breaks = 10, 
          labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
Fielding_Wrangle$bin1 <- c1
Fielding_Wrangle$Fielder_Log <- log(scale(Fielding_Wrangle$Fielder))
Fielding_Wrangle <- na.omit(Fielding_Wrangle)
hist(Fielding_Wrangle$Fielder)
hist(Fielding_Wrangle$Fielder_Log)

mu <- mean(Fielding_Wrangle$Fielder_Log)
df_sig <- data.frame(bin = character(),
                     pos =character(),
                     sig = numeric(),
                     stringsAsFactors = FALSE)
x <-1
for(i in unique(Fielding_Wrangle$bin1)) {
  for (j in unique(Fielding_Wrangle$position_abbreviation)) {
    a <- filter(Fielding_Wrangle, bin1 == i, position_abbreviation == j)
    #mu_0 <- mean(a$Fielder)
    if (nrow(a)<2){
      next
    }
    else {
      result <- t.test(a$Fielder, Fielding_Wrangle$Fielder_Log )
        if (result$p.value < .05 & result$statistic < 0) {
          b <- -1
          print(result$statistic)
        }
        else if (result$p.value < .05 & result$statistic > 0){
          b <- 1
          #print(b)
        }
        else {
          b <- 0
        }
      df_sig[x,] <- c(i,j,b) 
      x <- x+1
    }
  }
}

Fielding_Sig <- merge(Fielding_Wrangle, df_sig, by.x = c("position_abbreviation", "bin1"), 
                   by.y = c("pos", "bin"), all.x = T)
Fielding_Sig$sig <- as.numeric(Fielding_Sig$sig)
Fielding_Sig <- Fielding_Sig %>%
  group_by(Team_Name) %>%
  summarise(significance = sum(sig))

Fielding_Sig <- merge(Fielding_Sig, team_heights, 
                      by.x = "Team_Name", by.y = "Var1", all.x = T)
Fielding_Sig <- na.omit(Fielding_Sig)

ggplot(data = Fielding_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
  geom_bar(stat = "identity", aes(fill = significance)) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  xlab("Teams") + ylab("Number of Wins") + 
  ggtitle("Team Wins in Order of Significance Score of Assists Per Game Metric") 


#Steal Percentage
Steal_Effectiveness_Wrangle <- Metric %>% dplyr::select(team_id, height, position_abbreviation, 
                                       Team_Name, player_id, Steal_Effectiveness)
table(Steal_Effectiveness_Wrangle$position_abbreviation)

Steal_Effectiveness_Wrangle <- filter(Steal_Effectiveness_Wrangle, position_abbreviation != "RP")

Steal_Effectiveness_Wrangle <- na.omit(Steal_Effectiveness_Wrangle)
c2 <- cut(Steal_Effectiveness_Wrangle$height, breaks = 10, 
          labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
Steal_Effectiveness_Wrangle$bin1 <- c2
Steal_Effectiveness_Wrangle$Steal_Effectiveness_Log <- log(scale(Steal_Effectiveness_Wrangle$Steal_Effectiveness))
Steal_Effectiveness_Wrangle <- na.omit(Steal_Effectiveness_Wrangle)
hist(Steal_Effectiveness_Wrangle$Steal_Effectiveness)
hist(Steal_Effectiveness_Wrangle$Steal_Effectiveness_Log)

mu <- mean(Steal_Effectiveness_Wrangle$Steal_Effectiveness_Log)
df_sig_1 <- data.frame(bin = character(),
                     pos =character(),
                     sig = numeric(),
                     stringsAsFactors = FALSE)
x <-1
for(i in unique(Steal_Effectiveness_Wrangle$bin1)) {
  for (j in unique(Steal_Effectiveness_Wrangle$position_abbreviation)) {
    a <- filter(Steal_Effectiveness_Wrangle, bin1 == i, position_abbreviation == j)
    if (nrow(a)<2){
      next
    }
    else {
      result <- t.test(a$Steal_Effectiveness, Steal_Effectiveness_Wrangle$Steal_Effectiveness_Log )
      if (result$p.value < .05 & result$statistic < 0) {
        b <- -1
        print(result$statistic)
      }
      else if (result$p.value < .05 & result$statistic > 0){
        b <- 1
        #print(b)
      }
      else {
        b <- 0
      }
      df_sig_1[x,] <- c(i,j,b) 
      x <- x+1
    }
  }
}

Steal_Effectiveness_Sig <- merge(Steal_Effectiveness_Wrangle, df_sig_1, by.x = c("position_abbreviation", "bin1"), 
                       by.y = c("pos", "bin"), all.x = T)
Steal_Effectiveness_Sig$sig <- as.numeric(Steal_Effectiveness_Sig$sig)
Steal_Effectiveness_Sig <- Steal_Effectiveness_Sig %>%
  group_by(Team_Name) %>%
  summarise(significance = sum(sig))

Steal_Effectiveness_Sig <- merge(Steal_Effectiveness_Sig, team_heights, 
                       by.x = "Team_Name", by.y = "Var1", all.x = T)

ggplot(data = Steal_Effectiveness_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
  geom_bar(stat = "identity", aes(fill = significance)) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  xlab("Teams") + ylab("Number of Wins") + 
  ggtitle("Team Wins in Order of Significance Score of Steal Percentage Metric")


#Prob_Home_Run_For_Hits
Home_Run_Wrangle <- Metric%>%
  dplyr::select(team_id, height, position_abbreviation,
         Team_Name, player_id, Prob_Home_Run_For_Hits)

table(Home_Run_Wrangle$position_abbreviation)

Home_Run_Wrangle <- filter(Home_Run_Wrangle, position_abbreviation != "RP")

Home_Run_Wrangle <- na.omit(Home_Run_Wrangle)
c3 <- cut(Home_Run_Wrangle$height, breaks = 10, 
          labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
Home_Run_Wrangle$bin1 <- c3
Home_Run_Wrangle$Home_Run_Log <- log(scale(Home_Run_Wrangle$Prob_Home_Run_For_Hits))
Home_Run_Wrangle <- na.omit(Home_Run_Wrangle)
hist(Home_Run_Wrangle$Prob_Home_Run_For_Hits)
hist(Home_Run_Wrangle$Home_Run_Log)

mu <- mean(Home_Run_Wrangle$Home_Run_Log)
df_sig_3 <- data.frame(bin = character(),
                       pos =character(),
                       sig = numeric(),
                       stringsAsFactors = FALSE)
x <-1
for(i in unique(Home_Run_Wrangle$bin1)) {
  for (j in unique(Home_Run_Wrangle$position_abbreviation)) {
    a <- filter(Home_Run_Wrangle, bin1 == i, position_abbreviation == j)
    if (nrow(a)<2){
      next
    }
    else {
      result <- t.test(a$Home_Run, Home_Run_Wrangle$Home_Run_Log)
      if (result$p.value < .05 & result$statistic < 0) {
        b <- -1
        print(result$statistic)
      }
      else if (result$p.value < .05 & result$statistic > 0){
        b <- 1
        #print(b)
      }
      else {
        b <- 0
      }
      df_sig_3[x,] <- c(i,j,b) 
      x <- x+1
    }
  }
}

Home_Run_Sig <- merge(Home_Run_Wrangle, df_sig_3, by.x = c("position_abbreviation", "bin1"), 
                                 by.y = c("pos", "bin"), all.x = T)
Home_Run_Sig$sig <- as.numeric(Home_Run_Sig$sig)
Home_Run_Sig <- Home_Run_Sig %>%
  group_by(Team_Name) %>%
  summarise(significance = sum(sig))

Home_Run_Sig <- merge(Home_Run_Sig, team_heights, 
                                 by.x = "Team_Name", by.y = "Var1", all.x = T)

ggplot(data = Home_Run_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
  geom_bar(stat = "identity", aes(fill = significance)) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  xlab("Teams") + ylab("Number of Wins") + 
  ggtitle("Team Wins in Order of Significance Score of Probability of Home Run/Game")

#Bad Throw Percentage
Metric_Pitcher <- filter(Metric, position_abbreviation == c("P", "RP", "SP"))
Bad_Throw_Wrangle <- Metric_Pitcher %>% dplyr::select(team_id, height, position_abbreviation, 
                                       Team_Name, player_id, Bad_Throw_Percentage)
table(Bad_Throw_Wrangle$position_abbreviation)

Bad_Throw_Wrangle <- na.omit(Bad_Throw_Wrangle)
c2 <- cut(Bad_Throw_Wrangle$height, breaks = 10, 
          labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
Bad_Throw_Wrangle$bin1 <- c2
Bad_Throw_Wrangle$Bad_Throw_Log <- log(scale(Bad_Throw_Wrangle$Bad_Throw_Percentage))
Bad_Throw_Wrangle <- na.omit(Bad_Throw_Wrangle)
hist(Bad_Throw_Wrangle$Bad_Throw_Percentage)
hist(Bad_Throw_Wrangle$Bad_Throw_Log)

mu <- mean(Bad_Throw_Wrangle$Bad_Throw_Log)
df_sig_4 <- data.frame(bin = character(),
                     pos =character(),
                     sig = numeric(),
                     stringsAsFactors = FALSE)
x <- 1
for(i in unique(Bad_Throw_Wrangle$bin1)) {
  for (j in unique(Bad_Throw_Wrangle$position_abbreviation)) {
    a <- filter(Bad_Throw_Wrangle, bin1 == i, position_abbreviation == j)
    if (nrow(a)<2){
      next
    }
    else {
      result <- t.test(a$Bad_Throw_Percentage, Bad_Throw_Wrangle$Bad_Throw_Log )
      if (result$p.value < .05 & result$statistic < 0) {
        b <- -1
        print(result$statistic)
      }
      else if (result$p.value < .05 & result$statistic > 0){
        b <- 1
        #print(b)
      }
      else {
        b <- 0
      }
      df_sig_4[x,] <- c(i,j,b) 
      x <- x+1
    }
  }
}

Bad_Throw_Sig <- merge(Bad_Throw_Wrangle, df_sig_4, by.x = c("position_abbreviation", "bin1"), 
                       by.y = c("pos", "bin"), all.x = T)
Bad_Throw_Sig$sig <- as.numeric(Bad_Throw_Sig$sig)
Bad_Throw_Sig <- Bad_Throw_Sig %>%
  group_by(Team_Name) %>%
  summarise(significance = sum(sig))

Bad_Throw_Sig <- merge(Bad_Throw_Sig, team_heights, 
                       by.x = "Team_Name", by.y = "Var1", all.x = T)

ggplot(data = Bad_Throw_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
  geom_bar(stat = "identity", aes(fill = significance)) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  xlab("Teams") + ylab("Number of Wins") + 
  ggtitle("Team Wins in Order of Significance Score of Bad Throw Percentage")


#Balls Percentage
Metric_Pitcher <- filter(Metric, position_abbreviation == c("P", "RP", "SP"))
Balls_Percentage_Wrangle <- Metric_Pitcher %>% dplyr::select(team_id, height, position_abbreviation, 
                                                      Team_Name, player_id, Balls_Percentage)
table(Balls_Percentage_Wrangle$position_abbreviation)

Balls_Percentage_Wrangle <- na.omit(Balls_Percentage_Wrangle)
c2 <- cut(Balls_Percentage_Wrangle$height, breaks = 10, 
          labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
Balls_Percentage_Wrangle$bin1 <- c2
Balls_Percentage_Wrangle$Balls_Percentage_Log <- log(scale(Balls_Percentage_Wrangle$Balls_Percentage))
Balls_Percentage_Wrangle <- na.omit(Balls_Percentage_Wrangle)
hist(Balls_Percentage_Wrangle$Balls_Percentage)
hist(Balls_Percentage_Wrangle$Balls_Percentage_Log)

mu <- mean(Balls_Percentage_Wrangle$Balls_Percentage_Log)
df_sig_4 <- data.frame(bin = character(),
                       pos =character(),
                       sig = numeric(),
                       stringsAsFactors = FALSE)
x <- 1
for(i in unique(Balls_Percentage_Wrangle$bin1)) {
  for (j in unique(Balls_Percentage_Wrangle$position_abbreviation)) {
    a <- filter(Balls_Percentage_Wrangle, bin1 == i, position_abbreviation == j)
    if (nrow(a)<2){
      next
    }
    else {
      result <- t.test(a$Balls_Percentage, Balls_Percentage_Wrangle$Balls_Percentage)
      if (result$p.value < .05 & result$statistic < 0) {
        b <- -1
        print(result$statistic)
      }
      else if (result$p.value < .05 & result$statistic > 0){
        b <- 1
        #print(b)
      }
      else {
        b <- 0
      }
      df_sig_4[x,] <- c(i,j,b) 
      x <- x+1
    }
  }
}

Balls_Percentage_Sig <- merge(Balls_Percentage_Wrangle, df_sig_4, by.x = c("position_abbreviation", "bin1"), 
                       by.y = c("pos", "bin"), all.x = T)
Balls_Percentage_Sig$sig <- as.numeric(Balls_Percentage_Sig$sig)
Balls_Percentage_Sig <- Balls_Percentage_Sig %>%
  group_by(Team_Name) %>%
  summarise(significance = sum(sig))

Balls_Percentage_Sig <- merge(Balls_Percentage_Sig, team_heights, 
                       by.x = "Team_Name", by.y = "Var1", all.x = T)

ggplot(data = Balls_Percentage_Sig, aes(x = reorder(Team_Name, significance), y = Freq)) + 
  geom_bar(stat = "identity", aes(fill = significance)) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  xlab("Teams") + ylab("Number of Wins") + 
  ggtitle("Team Wins in Order of Significance Score of Percentage of Balls Thrown")

cor(Balls_Percentage_Sig$Freq, Balls_Percentage_Sig$significance)
cor(Bad_Throw_Sig$Freq, Bad_Throw_Sig$significance)
cor(Home_Run_Sig$Freq, Home_Run_Sig$significance)
cor(Steal_Effectiveness_Sig$Freq, Steal_Effectiveness_Sig$significance)
cor(Fielding_Sig$Freq, Fielding_Sig$significance)



Bad_Throw_Sig
Home_Run_Sig
Steal_Effectiveness_Sig
Fielding_Sig