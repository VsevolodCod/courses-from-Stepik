

?mtcars

df  <- mtcars

str(df)

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))


median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean_disp  <- mean(df$disp)

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])



?aggregate
mean_hp_vs <- aggregate(x= df$hp, by = list(df$vs), FUN = mean)

colnames(mean_hp_vs) <- c("vs","Mean H")

aggregate(hp ~ vs, df, mean )

aggregate(hp ~ vs + am, df, mean)
aggregate(x=df$hp, by= list(df$vs, df$am), FUN = mean)
aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median )

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd )

aggregate(cbind(mpg , disp) ~ am + vs, df, sd)

cbind(df$mpg, df$disp)

descriptions_stat <- aggregate(df[,c(4,3)], by = list(df$am), FUN = sd)

library(psych)

describe(x = df)

describe2 <- describeBy(x = df[, -c(8,9)], group =  df$vs, mat = T, digits =  1)

describe3 <- describeBy(x = df[, -c(8,9)], group =  df$vs, mat = T, digits =  1, fast = T)

describeBy(df$qsec, group = list(df$vs,df$am ), mat = T, digits = 1, 
           fast = T)

sum(is.na (df$mpg))



df$mpg[1:10] <- NA


mean(df$mpg, na.rm =T)
aggregate(mpg ~ am, df,sd)

describe()


air <- subset(airquality, Month %in% c(7,8,9))
result <- aggregate(air$Ozone ~ Month, air, length)



df <- airquality
new_airquality <- subset(df, Month == 7 | Month == 8 | Month == 9)
new_airquality <- subset(df,  Month%in%c(7,8,9))
result <- aggregate(x = new_airquality$Ozone, by = list(new_airquality$Month), FUN = length, simplify = TRUE)
result <- aggregate(Ozone ~ Month, new_airquality, length)

res <- describeBy(x = df[,c(1,2,3,4)], group = df$Month, mat = T, digits = 1)
f <- iris
df15 <- subset(iris, Species != 'setosa')
describe(x=iris) 


df <- mtcars
df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$vs, labels = c("Auto", "Manual")
                
                hist(df$mpg, breaks = 20)


