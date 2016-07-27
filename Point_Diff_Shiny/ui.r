library(dplyr)
library(readr)
library(stringr)
library(shiny)
library(ggiraph)
conf_df <- read_csv(file = "data/conference.csv")
team_df <- read_csv(file = "data/team.csv")

names(conf_df) <- str_replace_all(names(conf_df), " ", "_")
names(team_df) <- str_replace_all(names(team_df), " ", "_")
names(team_df)[2] <- "Team_Name"
names(conf_df)[2] <- "Conf_Name"

team_df$Team_Name <- team_df$Team_Name %>%
  str_replace_all(fixed("\xa0"), "") %>%
  str_replace_all(fixed("'"), "")

team_conf_df <- left_join(team_df, conf_df, by = c("Conference_Code"))

conferences <- sort(unique(conf_df$Conf_Name[conf_df$Subdivision == "FBS"]))
teams <- sort(unique(team_conf_df$Team_Name[team_conf_df$Subdivision == "FBS"]))

shinyUI(fluidPage(
  h1("Play by Play College Football Point Differentials"),
  p("by Matt Mills"),
  HTML('<a href = "https://twitter.com/millsGT49">@millsGT49</a>'),
  mainPanel(
    tabsetPanel(
      tabPanel("Team Plot", 
               selectInput("team", label = "Pick a Team", choices = teams, selected = "Georgia Tech"),
               # plotOutput("team_plot", width = 620),
               ggiraphOutput("team_interactive")),
      tabPanel("Conference Plot", 
               selectInput("conf", label = "Pick a Conference", choices = conferences)
               # plotOutput("conf_plot")
               ,ggiraphOutput("conf_interactive")
               ,em("The conference plot may be a little slow to load")
      ),
      tabPanel("Code", includeHTML("point_diff_plots.html"))
    ), width = 12
  )
))
