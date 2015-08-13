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


WOE_ <- function(x, y, bins = 10, adj = .5, incl_NA = F){
  na_log <- is.na(x)
  if(class(x) == "numeric" | class(x) == "integer"){
    if(!incl_NA){
      x_bins <- cut(x[!na_log], breaks = unique(quantile(x, probs = seq(0, 1, length.out = bins), na.rm = T)), include.lowest = T)
      y <- y[!na_log]
    }
    else {
      x_bins <- rep("NA", length(x))
      x_bins[!na_log] <- cut(x[!na_log], breaks = unique(quantile(x, probs = seq(0, 1, length.out = bins), na.rm = T)), include.lowest = T)
    }
  }
  if(class(x) == "character" | class(x) == "factor"){
    if(!incl_NA){
      x_bins <- as.character(x[!na_log])
      y <- y[!na_log]
    }
    else {
      x_bins <- as.character(x)
      x_bins[na_log] <- "NA"
    }
  }
  bins <- length(unique(x_bins))
  x_tab <- prop.table(table(x_bins, y == 1) + adj, 2)
  x_woe <- log(x_tab[, 2] / x_tab[, 1])
  iv <- sum((x_tab[, 2] - x_tab[, 1]) * x_woe)
  return(iv)
}

EDA <- function(df, yvar, print_plots = T){
  if(any(is.na(df))) stop("No Missing Values Allowed")
  num_vars <- names(df)[lapply(df, class) %>% unlist %in% c("numeric", "integer") & names(df) != yvar]
  yvar_log <- names(df) == yvar
  if(n_distinct(df[[yvar]]) == 2){
    cont_vars <- gather(df[, yvar_log | names(df) %in% num_vars], Variables, Value, -eval(parse(text = yvar)))
    hist_list <- list()
    smooth_list <- list()
    multi <- list()
    for(i in seq_along(num_vars)){
      plot_df <- data_frame(Y = df[[yvar]], X = df[[num_vars[i]]])
      r <- max(plot_df$X) - min(plot_df$X)
      hist_list[[i]] <- ggplot(plot_df, aes(x = X)) + geom_histogram(aes(y = ..density..), binwidth = r/20) + xlab(num_vars[i])
      smooth_list[[i]] <- ggplot(plot_df, aes(x = X, y = Y)) + geom_point(alpha = .6) + geom_smooth(method = "gam", formula = y ~ s(x), family = "binomial") + xlab(num_vars[i]) + ylab(yvar)
      multi[[i]] <- plot_grid(hist_list[[i]], smooth_list[[i]], labels = c("Histogram", "Variable Exploration"))
    }
  }
  pdf(paste0("eda_", yvar, ".pdf"))
  for(i in seq(1, 13, 3)){
    arrangeGrob()
  }
  bquiet = lapply(multi, print)
  dev.off()
  ggsave("arrange2x2.pdf", do.call(marrangeGrob, c(smooth_list, list(nrow=2, ncol=2))))
}

