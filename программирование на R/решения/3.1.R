#3.1

corr.calc <- function(x){
  c_teste <- cor.test(~ x[[1]] + x[[2]], x)
  return(c(c_teste$estimate,c_teste$p.value))
}


filtered.cor <- function(x){    
  n_v <- sapply(x, function(x) is.numeric(x))    
  c_m <- cor(x[, n_v])    
  diag(c_m) <- 0    
  return(c_m[which.max(abs(c_m))])
}


dfff <- read.table("dataset_11508_12.txt", header = T)
lm_dffff <- lm(dfff[,1]~dfff[,2], data = dfff)
intercept <- coef(lm_dffff)[1]
slope <- coef(lm_dffff)[2]
cat(intercept, slope, sep = " ")


smart_cor <- function(data) {
  var1 <- data[[1]]
  var2 <- data[[2]]
  s_test_var1 <- shapiro.test(var1)
  s_test_var2 <- shapiro.test(var2)
  if (s_test_var1$p.value < 0.05 || s_test_var2$p.value < 0.05) {
    return(cor(var1, var2, method = "spearman"))
  } else {
    return(cor(var1, var2, method = "pearson"))
  }
}







dataa <-diamonds[diamonds$cut == "Ideal" & diamonds$carat == 0.46, ]
lm_dataaa <- lm(price ~ depth, data =dataa )
fit_coef <- lm_dataaa$coefficients

regr.calc <- function(data) {
  
  c_test <- cor.test(data[, 1], data[, 2], method = "pearson")
  if (c_test$p.value < 0.05) {
    model <- lm(data[, 1] ~ data[, 2], data = data)
    data$fit <- predict(model)
    return(data)
  } else {
    return("There is no sense in prediction")
  }
}


my_plot <- ggplot(iris, aes (x = Sepal.Width, y = Petal.Width, color = Species ))+
  geom_point()+
  geom_smooth(method = "lm", ) 
