#2.3

DV ~ IV1 * IV2
DV ~ (IV1*IV2)^2
DV ~ (IV1 + IV2)^2

dff <- npk
f1 <- aov(yield ~ N*P, data = dff)
summary(f1)

dff <- npk
f2 <- aov(yield ~ N+P+K, data = dff)
summary(f2)

data(iris)
anova_result <- aov(Sepal.Width ~ Species, data = iris)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

mynewdata <- read.csv("Pillulkin.csv")
str(mynewdata)
mynewdata$patient <- factor(mynewdata$patient)
fig <- aov(temperature ~ doctor *pill + Error(patient /(pill * doctor)), data = mynewdata)
summary(fig)



library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))

