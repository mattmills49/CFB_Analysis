library(readr) 
library(stringr)
library(ggplot2) 
library(dplyr)
library(shiny) # only needed for the shiny app
library(ggiraph) # only needed for interactive graphs
options(dplyr.width = Inf)
source("data/helper_functions.r")

play_df <- read_csv(file = "data/play.csv")
conf_df <- read_csv(file = "data/conference.csv")
team_df <- read_csv(file = "data/team.csv")

names(play_df) <- str_replace_all(names(play_df), " ", "_")
names(conf_df) <- str_replace_all(names(conf_df), " ", "_")
names(team_df) <- str_replace_all(names(team_df), " ", "_")
names(team_df)[2] <- "Team_Name"
names(conf_df)[2] <- "Conf_Name"

play_df <- AddTime(play_df)
play_df <- select(play_df, -scoreTextVector, -driveTextVector)

team_df$Team_Name <- team_df$Team_Name %>%
  str_replace_all(fixed("\xa0"), "") %>%
  str_replace_all(fixed("'"), "")

team_conf_df <- left_join(team_df, conf_df, by = c("Conference_Code"))

games <- select(play_df, Game_Code, Offense_Team_Code, Defense_Team_Code) %>%
  distinct %>%
  rename(Team = Offense_Team_Code, Opponent = Defense_Team_Code) %>%
  left_join(team_conf_df, by = c("Team" = "Team_Code")) %>%
  left_join(select(team_conf_df, Team_Code, Team_Name, Subdivision) %>% rename(Opponent_Name = Team_Name, Opp_Sub = Subdivision), by = c("Opponent" = "Team_Code"))

plot_df <- left_join(games, play_df, by = c("Game_Code")) %>%
  filter(Play_Type != "KICKOFF", Period_Number <= 4, Subdivision != "FCS", Opp_Sub != "FCS") %>%
  mutate(Point_Diff = ifelse(Team == Offense_Team_Code, Offense_Points - Defense_Points, Defense_Points - Offense_Points),
         Time_Remaining = New.Clock + (4 - Period_Number) * 900,
         Min_Elapsed = 60 - Time_Remaining / 60,
         tooltip = ifelse(Team == Offense_Team_Code, str_c(Opponent_Name, ": ", Offense_Points, " - ", Defense_Points), str_c(Opponent_Name, ": ", Defense_Points, " - ", Offense_Points)))

shinyServer(function(input, output) {
#   output$conf_plot <- renderPlot({
#     filter(plot_df, Conf_Name == input$conf) %>%
#       ggplot(aes(x = Min_Elapsed, y = Point_Diff, group = Opponent, color = Point_Diff)) +
#       geom_line() +
#       facet_wrap(~Team_Name, nrow = 3) +
#       geom_hline(yintercept = 0, linetype = "dashed") + 
#       theme(legend.position = "none") +
#       theme_538 +
#       scale_color_gradient2(low = "darkorange", high = "blue", mid = "grey85", midpoint = 0) +
#       ggtitle(str_c(input$conf, "Point Differential by Minute", sep = " ")) +
#       ylab("Point Differential") +
#       theme(strip.text = element_text(face = "bold", size = 12)) + 
#       scale_x_continuous(name = "Game Time", breaks = seq(0, 60, by = 15))
#   })
  
#   output$team_plot <- renderPlot({
#     filter(plot_df, Team_Name == input$team) %>%
#       ggplot(aes(x = Min_Elapsed, y = Point_Diff, group = Opponent, color = Point_Diff)) +
#       geom_line() +
#       geom_hline(yintercept = 0, linetype = "dashed") + 
#       theme(legend.position = "none") +
#       theme_538 +
#       scale_color_gradient2(low = "darkorange", high = "blue", mid = "grey85", midpoint = 0) +
#       ggtitle(str_c(input$team, "Point Differential by Minute", sep = " ")) +
#       ylab("Point Differential") +
#       scale_x_continuous(name = "Game Time", breaks = seq(0, 60, by = 15))
#   })
  
  output$conf_interactive <- renderggiraph({
    confp <- filter(plot_df, Conf_Name == input$conf) %>%
      mutate(data_id = 1:n()) %>%
      ggplot(aes(x = Min_Elapsed, y = Point_Diff, group = Opponent, color = Point_Diff, tooltip = tooltip, data_id = data_id)) +
      geom_path_interactive(aes(group = Opponent), size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_wrap(~Team_Name, nrow = 3) +
      theme(legend.position = "none") +
      theme_538 +
      scale_color_gradient2(low = "darkorange", high = "blue", mid = "grey85", midpoint = 0) +
      ggtitle(str_c(input$conf, "Point Differentials by Minute", sep = " ")) +
      ylab("Point Differential") +
      theme(strip.text = element_text(face = "bold", size = 12)) + 
      scale_x_continuous(name = "Game Time", breaks = seq(0, 60, by = 15))
    
    ggiraph(code = {print(confp)}, width = 10, height = 7)
    #, width = 8.5, height = 7, hover_css = "{stroke:black;stroke-width:3;}")
  })
  
  output$team_interactive <- renderggiraph({
    testp <- filter(plot_df, Team_Name == input$team) %>%
      mutate(data_id = 1:n()) %>%
      ggplot(aes(x = Min_Elapsed, y = Point_Diff, group = Opponent, color = Point_Diff, tooltip = tooltip, data_id = Opponent)) +
      geom_path_interactive(aes(group = Opponent), size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      theme(legend.position = "none") +
      theme_538 +
      scale_color_gradient2(low = "darkorange", high = "blue", mid = "grey85", midpoint = 0) +
      ggtitle(str_c(input$team, "Point Differentials by Minute", sep = " ")) +
      ylab("Point Differential") +
      scale_x_continuous(name = "Game Time", breaks = seq(0, 60, by = 15))
    
    ggiraph(code = {print(testp)}, width = 10, height = 7, hover_css = "{stroke:black;stroke-width:3;}")
  })
})