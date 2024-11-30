#1.6
df <- airquality
ggplot(df, aes(x = factor(Month) , y = Ozone))+
  geom_boxplot()

plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, color = hp))+
  geom_point()


ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, size = Petal.Length)) +
  geom_point()