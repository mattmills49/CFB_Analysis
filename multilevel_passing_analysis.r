source("readin.r")
library(ggplot2)
library(magrittr)
library(lme4)
library(dplyr)
options(dplyr.width = Inf)

passmodel <- function(data, dependent, player_info = players, team_info = teams){
  data$Y <- data[[dependent]]
  if(n_distinct(data$Y) > 2){
    mm <- lmer(Y ~ 1 + (1 | Passer.Player.Code) + (1 | Receiver.Player.Code) + (1 | Defense.Team.Code), data = data)
  }
  if(n_distinct(data$Y) == 2){
    mm <- glmer(Y ~ 1 + (1 | Passer.Player.Code) + (1 | Receiver.Player.Code) + (1 | Defense.Team.Code), data = data, family = "binomial")
  }
  coefs <- coef(mm)
  
  passers <- data_frame(Passer = as.numeric(row.names(coefs$Passer.Player.Code)), Values = coefs$Passer.Player.Code[,1])
  passers <- left_join(passers, select(player_info, Player.Code, Team.Code, Last.Name, First.Name), by = c("Passer" = "Player.Code"))
  rawpasserinfo <- data %>% group_by(Passer.Player.Code) %>% summarize(n = n(), Comp.Perc = mean(Completion), YPP = mean(Yards)) %>% mutate(Passer = as.numeric(as.character(Passer.Player.Code)))
  passers <- left_join(passers, rawpasserinfo, by = c("Passer"))

  receivers <- data_frame(Receiver = as.numeric(row.names(coefs$Receiver.Player.Code)), Values = coefs$Receiver.Player.Code[,1])
  receivers <- left_join(receivers, select(player_info, Player.Code, Team.Code, Last.Name, First.Name), by = c("Receiver" = "Player.Code"))
  rawreceiverinfo <- data %>% group_by(Receiver.Player.Code) %>% summarize(n = n(), Comp.Perc = mean(Completion), YPT = mean(Yards)) %>% mutate(Receiver = as.numeric(as.character(Receiver.Player.Code)))
  receivers <- left_join(receivers, rawreceiverinfo, by = c("Receiver"))

  defenses <- data_frame(Team = as.numeric(row.names(coefs$Defense.Team.Code)), Values = coefs$Defense.Team.Code[,1])
  defenses <- left_join(defenses, team_info, by = c("Team" = "Team.Code"))
  
  return(list(passers, receivers, defenses, model = mm))
}

pass <- readin("pass", 2014)
plays <- readin("play", 2014)
teams <- readin("team", 2014)
players <- readin("player",2014)
 
passinfo <- left_join(pass, select(plays, Game.Code, Play.Number, Offense.Team.Code, Defense.Team.Code, Down, Distance, Spot), by = c("Game.Code" = "Game.Code", "Play.Number" = "Play.Number", "Team.Code" = "Offense.Team.Code"))

compperc <- passmodel(passinfo, "Completion")
yardsperatt <- passmodel(passinfo, "Yards")

qb_info <- compperc[[1]] %>% rename(Comp_Perc_Value = Values) %>% left_join(select(yardsperatt[[1]], Passer, Values) %>% rename(YPP_Value = Values), by = c("Passer"))
wr_info <- compperc[[2]] %>% rename(Comp_Perc_Value = Values) %>% left_join(select(yardsperatt[[2]], Receiver, Values) %>% rename(YPP_Value = Values), by = c("Receiver"))
d_info <- compperc[[3]] %>% rename(Comp_Perc_Value = Values) %>% left_join(select(yardsperatt[[3]], Team, Values) %>% rename(YPP_Value = Values), by = c("Team"))


