# Helper Functions #

library_s <- function(...){
  suppressPackageStartupMessages(library(...))
}

library_s(pROC)
library_s(ggplot2)
library_s(scales)
source("~/Documents/multiplot.R")

jitter_binary <- function(x, amount = .1) {
  ## randomizes data for 0/1 values for use in plots. 
  jit <- runif(length(x), 0, amount) ## random offsets, but always positive
  ifelse(x == 0L,x + jit, x - jit) ## for 0 values add offset, for 1's subtract it. 
}

view <- function(x, n = 6) {
  if(n > nrow(x)){return("WARNING: n must be larger than object")}
  nums <- sample(nrow(x), n)
  v <- x[nums, ]
  row.names(v) <- as.character(nums)
  print.data.frame(v)
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

model_test <- function(preds, Y){
  vals <- data_frame(Predictions = preds, Actual = Y)
  
  vals$Groups <- cut(x = vals$Predictions, breaks = seq(0, 1, .05))
  cal_data <- vals %>% group_by(Groups) %>% summarize(Mean_Prediction = mean(Predictions), Mean_Actual = mean(Actual))
  cal_plot <- ggplot(aes(x = Mean_Prediction, y = Mean_Actual), data = cal_data) + geom_line(size = 2, color = "blue") + geom_abline(a = 1, b = 0, linetype = "dashed", color = "grey") + ggtitle("Calibration Plot") + xlab("Predicted Probability") + ylab("Actual Probability") + scale_y_continuous(labels = percent) + scale_x_continuous(labels = percent)
  
  roc_info <- roc(response = Y, predictor = preds)
  roc_data <- data_frame(Sensitivity = roc_info$sensitivities, Specificity = roc_info$specificities)
  auc_plot <- ggplot(aes(x = 1 - Sensitivity, y = Specificity), data = roc_data) + geom_line(size = 2, color = "blue") + geom_abline(a = 1, b = 0, linetype = "dashed", color = "grey") + annotate("text", x = .75, y = .25, label = paste0("AUC = ", round(roc_info$auc[1], 2))) + ggtitle("AUC Plot")
  
  values <- preds %>% round(3) %>% table %>% rev %>% names %>% as.numeric
  pct_pos <- sapply(values,function(x) sum(Y[preds >= x] == 1)/(sum(Y == 1)))
  pct_neg <- sapply(values,function(x) sum(Y[preds >= x] == 0)/(sum(Y == 0)))
  nums <- preds %>% round(3) %>% table %>% rev %>% cumsum
  info <- data.frame(Values = values,
                     Number_of_Values = nums/max(nums),
                     Pct_of_Yes = pct_pos,
                     Pct_of_No = pct_neg)
  k_s <- max(info$Pct_of_Yes - info$Pct_of_No)*100
  where <- which.max(info$Pct_of_Yes - info$Pct_of_No)
  locsdata <- with(info, data_frame(a = Number_of_Values[where], b = Pct_of_Yes[where], c = Pct_of_No[where]))
  
  gain_plot <- ggplot() + geom_line(aes(x = Number_of_Values, y = Pct_of_Yes), data = info, color = "blue", size = 2) + geom_line(aes(x = Number_of_Values, y = Pct_of_No), data = info, color = "red", size = 2) + geom_segment(aes(x = a, xend = a, y = b, yend = c), linetype = "dashed", data = locsdata) + xlab("Percentage of Population") + ylab("Percentage of Yes") + ggtitle("Gain Plot") + scale_x_continuous(labels = percent, breaks = seq(0, 1, .1)) + scale_y_continuous(labels = percent) + geom_text(aes(x = a, y = b/2 + c/2), data = locsdata, label = paste0("K-S = ", round(k_s,2)))
  
  dist_plot <- ggplot(aes(x = Predictions), data = vals) + geom_density(aes(fill = as.factor(Actual)), alpha = .8) + xlab("Predictied No Show Probability") + ggtitle("Distribution of Scores") + theme(legend.position = "top") + scale_fill_discrete(name = "No Show")
  
  scores <- round(c(roc_info$auc, sqrt(mean((vals$Predictions - vals$Actual)^2)), k_s),3)
  names(scores) <- c("AUC", "RMSE", "K-S")
  print(scores)
  return(list(auc_plot, cal_plot, gain_plot, dist_plot))
}
