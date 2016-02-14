AddTime <- function(plays){
  clock <- plays$Clock # creates a vector of all the clock values
  previousclock <- 9000 # initializes the previous clock so it won't catch as NA the first time
  newclock <- rep(0,length(clock))
  currentclock <- clock[1]
  newclock[1] <- 900
  clockcount <- 1
  for(i in 2:length(clock)){ # starts at two because we have already handled play 1
    if(is.na(clock[i])){ # if there is no clock info we add a counter to record the play
      clockcount <- clockcount + 1
    }
    if(!is.na(clock[i])){ # if there is clock info we have three options:
      #1.) it is a new quarter
      #2.) it is a repeat clock like attempt and kickoff, same time but a new play
      #3.) there was a new time listed and we have to roll up time to the plays inbetween
      if(clock[i] == 900){
        clockvalues <- seq(currentclock,10,length.out = clockcount) # creates the sequence of times from last recorded clock to 10 seconds
        for(time in 1:clockcount){ # this will take the times just created and assign them to the previous plays so that it fills all the "missing" times
          newclock[i - time] <- clockvalues[clockcount - time + 1]
        }
        clockcount <- 1
        currentclock <- 900
        newclock[i] <- 900
      }
      if(clock[i] != 900){
        if(clock[i] == currentclock){ # this is a special case for scores and kickoffs
          newclock[i] <- currentclock
          clockcount <- 1
        }
        if(clock[i] != currentclock){
          clockcount <- clockcount + 1
          previousclock <- currentclock
          currentclock <- clock[i]
          clockvalues <- seq(previousclock, currentclock, length.out = clockcount)
          for(time in 1:clockcount){
            newclock[i - time + 1] <- clockvalues[clockcount - time + 1]
          }
          clockcount <- 1
        }
      }
    }
  }
  plays$New.Clock <- newclock
  return(plays)
}

theme_538 <- theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(axis.ticks=element_blank()) +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=20)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=10,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=10,colour="#535353",face="bold",vjust=-.5)) +
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm"))

test_data <- data.frame(step = 1:20, yval = 1:20 + rnorm(20, sd = 2))
graph_plot <- ggplot(aes(x = step, y = yval, data_id = step), data = test_data) +
  geom_path_interactive()
ggiraph(code = {print(graph_plot)}, hover_css = "{stroke:red;stroke-width:3;")
