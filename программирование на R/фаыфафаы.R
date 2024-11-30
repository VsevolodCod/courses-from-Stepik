dfd <- ToothGrowth
o_d <- dfd[dfd$supp == "OJ" & dfd$dose == 0.5, ]
v_d <- dfd[dfd$supp == "VC" & dfd$dose == 2,]
t_statt <- t.test(o_d$len, v_d$len)
t_stat <- t_statt$statistic

data <- read.csv("lekarstva.csv")
t.test(data$Pressure_before, data$Pressure_after, paired = T)

data(iris)
anova_result <- aov(Sepal.Width ~ Species, data = iris)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

mydata <- read.csv("therapy_data (1).csv")
str(mydata)

mydata$subject <- as.factor(mydata$subject)
fit1 <- aov(well_being ~ therapy, data = mydata)
summary(fit1)
fit1b <- aov(well_being ~ therapy + Error(subject/therapy), data = mydata)
summary(fit1b)

fit2 <- aov(well_being ~ therapy*price, data = mydata)
summary(fit2)

ggplot(mydata, aes(x = price, y = well_being)) + 
  geom_boxplot()


fit2b <- aov(well_being ~ therapy*price + Error(subject/(therapy *price)), data = mydata)
summary(fit2b)

ggplot(mydata, aes(x =price, y = well_being))+
  geom_boxplot()+
  facet_grid(~subject)


fit3<- aov(well_being ~therapy*price*sex, data= mydata)
summary(fit3)
fit3b <- aov(well_being ~therapy*price*sex + Error(subject/(therapy*price)),data = mydata)
summary(fit3b)



mynewdata <- read.csv("Pillulkin.csv")
str(mynewdata)
mynewdata$patient <- factor(mynewdata$patient)
fig <- aov(temperature ~ doctor *pill + Error(patient /(pill * doctor)), data = mynewdata)
summary(fig)


df <- read.csv("Pillulkin.csv")
df$patient <- factor(df$patient)
fit2 <- aov(df$temperature ~ doctor*pill + Error(df$patient/(pill*doctor)), df)
summary(fit2)


df <- read.csv('dataset_11504_15.txt')
df
t1 <- read.table("dataset_11504_15.txt")
t1
#t2 <- as.data.frame(t1)
bartlett.test(V1 ~ V2, t1) # гомогенність дисперсії
t.test(V1  ~ V2, t1, var.equal = T)
wilcox.test(Petal.Length ~ Species, t1)

s <- 100


my_calc  <- function(x,y) {
  s <- x +y
  d <- x -y
  return(c(s,d))
}
result <- my_calc(x = 10, y = 15)

my_calc2  <- function(x,y,z = 10) {
  s <- x +y +z
  d <- x -y - z
  return(c(s,d))
}
my_calc2(1,2,3)

my_calc2(1,2)

dister1 <- rnorm(1000)
dister1[1:30] <- NA

dister1[is.na(dister1)] <- mean(dister1, na.rm =  T)

my_na_rm <- function(x){
  if (is.numeric(x)){
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
  }else{
    print("x is not numeric")
  }
 
}

dister1 <- my_na_rm(x = dister1)

hist(dister1)

my_na_rm(x = c("2", "3", NA))

my_na_rm <- function(x){
  if (is.numeric(x)){
    stat_test <- shapiro.test(x)
    if (stat_test$p.value >0.05){
      x[is.na(x)] <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    }else {
      x[is.na(x)] <- median(x, na.rm = T)
      print("NA values were replaced with median")
      }
    return(x)
  }else{
    print("x is not numeric")
  }
  
}


dister1 <- my_na_rm(dister1)
d1 <- rnorm(2000)
d2 <- runif(2000)
d1[1:10]<- NA
d2[1:10]<- NA

d1 <- my_na_rm(d1)
head(d1)

d2 <- my_na_rm(d2)
head(d2)


#source("my_na_rm")

dir(pattern = "*.csv")


grants <- data.frame()

for (i in dir(pattern = "*.csv")){
  temp_df <- read.csv(i)
  grants <- rbind(temp_df, grants)
}


read_data <- function(){
  df <- data.frame()
  number <<- 0
  for (i in dir(pattern = "*.csv")){
    temp_df <- read.csv(i)
    df <- rbind(temp_df, grants)
    number <<-number +1
  }
  print(paste(as.character(number), "files were combined"))
  return(df)
}


grants2 <- read_data()


filtered.sum <- function(x){
  p_values <- x[x > 0]
  return (sum (p_values , is.na = T))
}






