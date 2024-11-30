#1.3
data(mtcars)
mtcars$even_gear <- ifelse(mtcars$gear %% 2 == 0, 1, 0)
head(mtcars)

mpg_4 <- mtcars$mpg[mtcars$cyl == 4]
mpg_4

mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)), ]
mini_mtcars