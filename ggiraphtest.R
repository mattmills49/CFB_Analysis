mtcars$facet_var <- ifelse(mtcars$am == 1, "This Facet Label Is Way Too Long", "none")
good_plot <- ggplot(aes(x = hp, y = mpg), data = mtcars) +
  geom_point() +
  facet_wrap(~facet_var, ncol = 2) +
  ggtitle("Another unnecessarily Long Title to use") +
  theme(strip.text = element_text(face = "bold", size = 14))

ggiraph(code = print({good_plot}), width = 6, height = 4)
ggiraph(code = print({good_plot}), width = 3, height = 4)

dataset <- mtcars
dataset$tooltip <- row.names(dataset)

good_tooltip <- ggplot(dataset, aes(x = disp, y = qsec, color = wt, tooltip = tooltip)) + 
  geom_point_interactive(size=3)
ggiraph(code = {print(good_tooltip)}, width = 7, height = 6)

raw_data <- data.frame(Team = c(rep("A", 20), rep("B", 20)), Xval = rep(seq(1, 10, length.out = 20), 2), data_id = 1:40)
raw_data$Yval <- ifelse(raw_data$Team == "A", 2, 5) + ifelse(raw_data$Team == "A", 4, 2) * raw_data$Xval + rnorm(n = 40, mean = 0, sd = 4)
test_plot <- ggplot(aes(x = Xval, y = Yval, group = Team, data_id = Team, tooltip = Team), data = raw_data) + geom_path_interactive(aes(group = Team, color = Team)) + theme(legend.position = "none")
ggiraph(code = print({test_plot}), hover_css = "{stroke:black;stroke-width:3;}")

x <- rnorm(85)
y <- 2*x +.5*rnorm(85)
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){
  cbind(1,x)%*%fit$coef
}
sq.err <- function(y,yhat) { (y-yhat)^2}
results <- bootpred(x,y,20,theta.fit,theta.predict,
                    err.meas=sq.err)
