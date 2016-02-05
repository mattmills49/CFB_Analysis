#' # QBR Visualization
#' 
#+
library(rvest)
library(ggplot2)
library(stringr)
library(magrittr)
library(purrr)
library(dplyr)
options(dplyr.width = Inf)

qbr_url <- c("http://espn.go.com/ncf/qbr/_/year/", "/seasontype/2/type/player-week/page/", "/week/")
year <- 2015
num_weeks <- 15
qbr_data <- data.frame()
for(w in seq_len(num_weeks)){
  page_html <- str_c(qbr_url[1], year, qbr_url[2], 1, qbr_url[3], w) %>%
    read_html
  if(w %in% c(14, 15)) num_results <- NULL
  if(!(w %in% c(14, 15))) num_results <- page_html %>% html_node(".totalResults")
  if(is.null(num_results)) num_pages <- 1
  if(!is.null(num_results)){
    num_pages <- num_results %>%
      html_text %>%
      str_extract("[0-9]+") %>%
      as.numeric %>% 
      divide_by(50) %>%
      ceiling
  }
  for(p in seq_len(num_pages)){
    page_table <- str_c(qbr_url[1], year, qbr_url[2], p, qbr_url[3], w) %>%
      read_html %>%
      html_node(".tablehead") %>%
      html_table(header = T)
    names(page_table) <- str_replace_all(names(page_table), " ", "_")
    page_table <- filter(page_table, PLAYER != "PLAYER") %>% mutate(Week = w)
    # Convert to numeric
    numeric_columns <- map_lgl(page_table, is.character)
    numeric_columns[2:3] <- F
    page_table[] <- map_if(page_table, numeric_columns, as.numeric)
    qbr_data <- bind_rows(qbr_data, page_table)
  }
  print(str_c(c(w, num_pages), collapse = ", "))
}


# Get Team and Opponent
qbr_data$Team <- str_extract(qbr_data$PLAYER, "[A-Z]{2,}")
qbr_data$Opponent <- str_extract(qbr_data$RESULT, "[A-Z]{2,}")
# Get Player Name
qbr_data$QB <- map_chr(qbr_data$PLAYER, ~ str_split_fixed(.x, ",", n = 2)[, 1])
qbr_data$QBshort <- qbr_data$QB %>% # dirty for now but good enough
  str_replace_all(fixed("."), "") %>%
  str_replace_all(" Jr", "") %>%
  str_split(" ") %>% 
  map_chr(~ str_c(tail(.x, 1), str_sub(.x[1], 1, 1), sep = ", "))
# Get Offense Points
# To do that we need to know who won
qbr_data$Win <- str_sub(qbr_data$RESULT, 1, 1)
winning_points <- str_extract_all(qbr_data$RESULT, "[0-9]+") %>% map_chr(~ .x[1]) %>% as.numeric
losing_points <- str_extract_all(qbr_data$RESULT, "[0-9]+") %>% map_chr(~ .x[2]) %>% as.numeric
qbr_data$Points_For <- ifelse(qbr_data$Win == "W", winning_points, losing_points) # There is probably a better way to do this but I couldn't think of one immediately

# save(qbr_data, file = "~/Documents/qbr_data.rdata")
plot_qbr <- function(qbname = "Deshaun Watson", qbr_data, color1 = "darkorange", color2 = "purple4"){
  qb_perf <- qbr_data %>% 
    filter(QB == qbname) %>%
    mutate(Opponent = factor(Opponent, levels = unique(Opponent[order(Week)])))
  
  qb_def <- qbr_data %>% 
    group_by(Opponent) %>%
    filter(any(QB == qbname)) %>%
    ungroup %>%
    mutate(Opponent = factor(Opponent, levels = qb_perf$Opponent))
  
  qb_plot <- ggplot() +
    geom_point(aes(x = Opponent, y = RAW_QBR, size = ACT_PLAYS, alpha = ACT_PLAYS), data = qb_def) + 
    geom_point(aes(x = Opponent, y = RAW_QBR), size = 6, color = color1, data = qb_perf) +
    geom_point(aes(x = Opponent, y = RAW_QBR), size = 4, color = color2, data = qb_perf) +
    xlab("") +
    ylab("Raw QBR") +
    ggtitle(str_c(qbname, "'s QBR Performance by Week")) +
    coord_flip() +
    theme(axis.text.y = element_text(face = "bold", size = 16), legend.position = "none")
    
  return(qb_plot)
}