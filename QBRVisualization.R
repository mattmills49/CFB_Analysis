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
    html
  num_results <- page_html %>%
    html_node(".totalResults")
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
      html %>%
      html_node(".tablehead") %>%
      html_table(header = T)
    names(page_table) <- str_replace_all(names(page_table), " ", "_")
    page_table <- filter(page_table, PLAYER != "PLAYER") %>% mutate(Week = w)
    qbr_data <- bind_rows(qbr_data, page_table)
  }
}

