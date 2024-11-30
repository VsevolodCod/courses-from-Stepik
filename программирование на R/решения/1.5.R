#1.5
result<-mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg >20])

df <- mtcars
descriptions_stat <- aggregate(df[,c(4,3)], by = list(df$am), FUN = sd)

air <- subset(airquality, Month %in% c(7,8,9))
result <- aggregate(air$Ozone ~ Month, air, length)

describeBy(airquality, group = airquality$Wind, mat = T, digits = 1, 
           fast = T)
res <- describeBy(x = airquality[,c(1,2,3,4)], group = df$Month, mat = T, digits = 1)


describe(x=iris)


mean_value <- mean(my_vector, na.rm = TRUE)
fixed_vector <- replace(my_vector, is.na(my_vector), mean_value)