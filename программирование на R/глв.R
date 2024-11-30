my_data <- read.csv("shops.csv")
str(my_data)


boxplot(price ~origin, data =my_data)
ggplot(my_data, aes(x = origin, y= price))+
  geom_boxplot()


fit <- aov(price~origin, data = my_data)
summary(fit)


fit1 <- aov(price~origin + store, data = my_data)
summary(fit1)


model.tables(fit1,"means")

pd = position_dodge(0.1)
ggplot(my_data, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()

fit3 <- aov(price ~ origin + store + origin:store, data=my_data)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=my_data)
summary(fit4)

dff <- npk
f1 <- aov(yield ~ N*P, data = dff)
summary(f1)

ggplot(my_data, aes(x = food, y = price)) +
  geom_boxplot()

fit5 <- aov(price ~ food, data = my_data)
summary(fit5)

TukeyHSD(fit5)

gada <- iris
fffi <- aov(Sepal.Width~Species, data = gada)
summary(fffi)


TukeyHSD(fffi)




df2 <- iris

boxplot(Sepal.Width ~ Species, data=df2)

fit8 <- aov(Sepal.Width ~ Species, data=df2)
summary(fit8)

TukeyHSD(fit8)

ggplot(df2, aes(Species, Sepal.Width))+
  stat_summary(fun.data = mean_cl_normal, geom = 'pointrange', 
               size = 1)

