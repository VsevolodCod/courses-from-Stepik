df <- iris
str(df)

df1 <- subset(df, Species != "setosa" )
str(df1)

hist(df1$Sepal.Length)

ggplot(df1 , aes (x= Sepal.Length))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~.)

ggplot(df1, aes(x=Sepal.Length, fill = Species))+
  geom_density(alpha = 0.5)
ggplot(df1, aes (x = Species, y =Sepal.Length ))+
  geom_boxplot()


shapiro.test(df1$Sepal.Length)
shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

bartlett.test(Sepal.Length ~ Species, df1)

test1 <- t.test(Sepal.Length ~ Species, df1)
str(test1)
test1$p.value

t.test(Sepal.Length ~ Species, df1, equal = T)

t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)






dfd <- ToothGrowth
o_d <- dfd[dfd$supp == "OJ" & dfd$dose == 0.5, ]
v_d <- dfd[dfd$supp == "VC" & dfd$dose == 2,]
t_statt <- t.test(o_d$len, v_d$len)
t_stat <- t_statt$statistic

data <- read.csv("lekarstva.csv")
t.test(data$Pressure_before, data$Pressure_after, paired = T)

ggplot(df1, aes(x= Species, y = Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               width =0.1)+
  stat_summary( fun.y = mean, geom = "point", size= 4)

ggplot(df1, aes(x= Species, y = Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange",
               size = 2)
?wilcox.test
test2 <- wilcox.test(Petal.Length ~Species, df1)
pv <- test2$p.value

ggplot(df1 , aes(Species,Petal.Length ))+
  geom_boxplot()

wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)


paires_wtest <-wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)



dat <- read.table("dataset_11504_15 (3).txt", header= F)
f <- bartlett.test(V1 ~ V2, dat)
if (f$p.value >= 0.05){
  print(round(t.test(V1~V2,f, paired = TRUE)$p.value,digits = 4)
}else{    print(round(wilcox.test(V1~V2,dat)$p.value, digits = 4)
}


df <- read.table("dataset_11504_16 (1).txt")
t.test(df$V1, df$V2, var.equal = F)
mean_cl_normal(df$V1)
mean_cl_normal(df$V2)
mean(df$V1)
mean(df$V2)

data <- read.table("dataset_11504_15 (3).txt", header = TRUE)
bartlett_result <- bartlett.test(value ~ group, data = data)
p_value_bartlett <- bartlett_result$p.value
if (p_value_bartlett > 0.05) {
  t_test_result <- t.test(value ~ group, data = data, var.equal = TRUE)
  final_p_value <- t_test_result$p.value
} else {
  wilcox_test_result <- wilcox.test(value ~ group, data = data)
  final_p_value <- wilcox_test_result$p.value
}
round(final_p_value, 4)



t.test(V1 ~ V2, dat, var.equal = T)
wilcox.test(Petal.Lenght ~ Species, dat)






