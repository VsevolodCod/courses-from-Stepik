#3.2

fill_na <- function(x) {
  x_no <- subset(x, !is.na(y))
  fit <- lm(y ~ x_1 + x_2, data = x_no)
  x$y_full <- ifelse(is.na(x$y), predict(fit, newdata = x), x$y)
  return(x)
}mod




model <- mpg+disp+hp



summary(lm(rating ~ complaints * critical, data = attitude))


mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
gs1 <- lm(mpg ~ wt * am, data = mtcars)
summary(gs1)


mtcars$am <- factor(mtcars$am)
my_plot <- ggplot(mtcars, aes(x = wt , y = mpg, col = am))+
  geom_smooth(method = "lm")