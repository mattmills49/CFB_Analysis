source("point_diff_plots.R")

shinyServer(function(input, output) {
  output$conf_plot <- renderPlot({
    filter(plot_df, Conf_Name == input$conf) %>%
      ggplot(aes(x = Min_Elapsed, y = Point_Diff, group = Opponent, color = Point_Diff)) +
      geom_line() +
      facet_wrap(~Team_Name, nrow = 3) +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      theme(legend.position = "none") +
      theme_538 +
      scale_color_gradient2(low = "darkorange", high = "blue", mid = "grey85", midpoint = 0) +
      ggtitle(str_c(input$conf, "Point Differential by Minute", sep = " ")) +
      ylab("Point Differential") +
      theme(strip.text = element_text(face = "bold", size = 12)) + 
      scale_x_continuous(name = "Game Time", breaks = seq(0, 60, by = 15))
  })
  
  output$team_plot <- renderPlot({
    filter(plot_df, Team_Name == input$team) %>%
      ggplot(aes(x = Min_Elapsed, y = Point_Diff, group = Opponent, color = Point_Diff)) +
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      theme(legend.position = "none") +
      theme_538 +
      scale_color_gradient2(low = "darkorange", high = "blue", mid = "grey85", midpoint = 0) +
      ggtitle(str_c(input$team, "Point Differential by Minute", sep = " ")) +
      ylab("Point Differential") +
      scale_x_continuous(name = "Game Time", breaks = seq(0, 60, by = 15))
  })
  
  output$team_interactive <- renderggiraph({
    testp <- filter(plot_df, Team_Name == input$team) %>%
      mutate(data_id = 1:n()) %>%
      ggplot(aes(x = Min_Elapsed, y = Point_Diff, group = Opponent, color = Point_Diff, tooltip = Opponent_Name, data_id = data_id)) +
      geom_path_interactive(aes(group = Opponent), size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      theme(legend.position = "none") +
      theme_538 +
      scale_color_gradient2(low = "darkorange", high = "blue", mid = "grey85", midpoint = 0) +
      ggtitle(str_c(input$team, "Point Differential by Minute", sep = " ")) +
      ylab("Point Differential") +
      scale_x_continuous(name = "Game Time", breaks = seq(0, 60, by = 15))
    
    ggiraph(code = {print(testp)}, width = 8, height = 7, hover_css = "{stroke:black;stroke-width:3;}")
  })
})