#3.6
log_coef <- coef(glm(am ~ disp +vs+ mpg, data= mtcars, family = binomial))



library("ggplot2")
obj <- ggplot(data = ToothGrowth, aes(x = supp, y=len, fill = factor(dose)))+
  geom_boxplot(col = "black")


df  <- read.csv("https://stepic.org/media/attachments/lesson/11478/data.csv")
df_known <- subset(df, admit != "NA")
df_NA <- subset(df, is.na(admit))
fit  <- glm(admit ~ rank * gpa, df_known, family = "binomial")
head(predict(object = fit, type = "response"))
df_known$prob <- predict(object = fit, type = "response")
df_NA$prob_predict <- predict(fit, newdata = df_NA, type = "response")
df_NA$admit <- ifelse(df_NA$prob_predict > 0.4, 1, 0)
sum(df_NA$admit)