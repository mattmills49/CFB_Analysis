shinyUI(fluidPage(
  h1("Play by Play College Football Point Differentials"),
  p("by Matt Mills"),
  HTML('<a href = "https://twitter.com/millsGT49">@millsGT49</a>'),
  mainPanel(
    tabsetPanel(
      tabPanel("Conference Plot", 
                selectInput("conf", label = "Pick a Conference", choices = conferences),
                plotOutput("conf_plot")),
      tabPanel("Team Plot", 
               selectInput("team", label = "Pick a Team", choices = teams),
               plotOutput("team_plot", width = 8),
               ggiraphOutput("team_interactive")),
      tabPanel("Code", includeHTML("point_diff_plots.html"))
    ), width = 12
  )
))
