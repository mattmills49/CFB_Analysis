# Helper Functions #

library_s <- function(...){
  suppressPackageStartupMessages(library(...))
}

library_s(pROC)
library_s(ggplot2)
library_s(scales)
library_s(tidyr)
library_s(grid)
#library_s(cowplot)
source("~/Documents/multiplot.R")

jitter_binary <- function(x, amount = .1) {
  ## randomizes data for 0/1 values for use in plots. 
  jit <- runif(length(x), 0, amount) ## random offsets, but always positive
  ifelse(x == 0L,x + jit, x - jit) ## for 0 values add offset, for 1's subtract it. 
}

view <- function(x, n = 6) {
  if("data.frame" %in% class(x)){
    if(n > nrow(x)){return("WARNING: n must be larger than object")}
    x %>% sample_n(n) %>% tbl_df %>% print
  }
  else sample(x, size = n)
}

in_ <- function(x, table){
  vapply(x, function(v) any(table == v), logical(1))
}

liklihood <- function(x){paste0(round(x, 0), "X")}

difference_quotient <- function(f, x, h){
  return((f(x + h) - f(x)) / h)
}

tableNA <- function(...){
  table(..., useNA = c("ifany"))
}

deciles <- function(x, na_log = F){
  v <- c(min(x, na.rm = na_log), quantile(x, probs = seq(.1, .9, .1), na.rm = na_log), max(x, na.rm = na_log))
  names(v)[c(1,11)] <- c("min", "max")
  return(v)
}

how_many_nas <- function(x){
  if("data.frame" %in% class(x)){
    lapply(x, function(y) sum(is.na(y))) %>% unlist
  } else if(is.vector(x)){
    sum(is.na(x))
  } else stop("x is not a data.frame or vector")
}

col_classes <- function(df){
  stopifnot("data.frame" %in% class(df))
  unlist(lapply(df, class))
}

safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))

phist <- function(yval, binwidth = NULL,...){
  df <- data_frame(yval)
  ggplot(df, aes(x = yval)) + geom_bar(aes(y = (..count..) / sum(..count..)), binwidth = binwidth,...) + ylab("Percent of Observations") + scale_y_continuous(labels = function(x) paste0(x*100, "%"))
}
  
venn <- function(vec1, vec2){
  both <- intersect(vec1, vec2)
  not_in_2 <- vec1[!(vec1 %in% vec2)]
  not_in_1 <- vec2[!(vec2 %in% vec1)]
  return(list("Not in 2" = not_in_2, 
              "Not in 1" = not_in_1, 
              "In Both" = both, 
              "Num_Summary" = c("Num Items 1" = length(vec1), 
                                "Num Items 2" = length(vec2),
                                "Num Only in 1" = length(not_in_2), 
                                "Num Only in 2" = length(not_in_1),
                                "Num in Both" = length(both))))
}

theme_538 <- theme_bw() +
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
  theme(axis.ticks=element_blank()) +
  # Set title and axis labels, and format these and tick marks
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=18)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=10,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=14,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=14,colour="#535353",face="bold",vjust=-.5)) +
  # Big bold line at y=0
  # Plot margins and finally line annotations
  theme(plot.margin = unit(c(1, 1, .5, .7), "cm"))

rand.int <- function(n, min = 1, max = 10) sample(seq(min, max, by = 1), size = n, replace = T)

ypec <- scale_y_continuous(labels = function(x) paste0(x * 100, "%"))
xpec <- scale_x_continuous(labels = function(x) paste0(x * 100, "%"))
yperc <- ypec
xperc <- xpec
