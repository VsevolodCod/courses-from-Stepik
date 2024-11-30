
#df$vs <- factor(df$vs, labels = c("V", "S"))
#df$am <- factor(df$vs, labels = c("Auto", "Manual"))
          
#hist(df$mpg, breaks = 20, xlab ="MPG")

#boxplot(mpg ~ am, df, ylab = "MPG" )


#plot(df$mpg, df$hp)



#ggplot(df, aes(x = mpg, fill = am ))+
#  geom_dotplot()

#ggplot(df, aes(x = mpg))+
#  geom_density(fill = "red")

#ggplot(df, aes(x = mpg, fill =am))+
#  geom_density(alpha = 0.5)


#ggplot(df, aes( x = am , y = hp, col = vs))+
#  geom_boxplot()



#my_plot <- ggplot( df, aes(x = mpg , y  = hp, col = vs, size = qsec))+
#  geom_point()

df  <- mtcars

mean_mpg  <- mean(df$mpg)

descr_df  <- describe(df[,-c(8,9)])

my_boxplot  <- ggplot(df, aes(x = factor(am), y = disp))+
  geom_boxplot()+
  xlab("Transmission")+
  ylab("Displacement")+
  ggtitle("My boxplot")

write.csv(df, "df.csv")
write.csv(descr_df, "descr_df.csv")

mu_mean <- mean(10^6:10^9)

df <- as.data.frame(HairEyeColor[,,"Female"])

library("ggplot2")

odj <- ggplot(data = df, aes(x = Hair, y = Freq, col = Eye, fill = Eye)) +
  geom_bar(stat="identity", position = position_dodge() ) +
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

binom.test(x = 5, n = 20, p  = 0.5)
binom.test(t1)

cgi <- chisq.test(t1)

cgi$exp
cgi$obs


t2
chisq.test(t2)

fisher.test(t2)



df <- as.data.frame(HairEyeColor[,,"Female"])
brown_hair_girls <- df[df$Hair == "Brown", ]
eye_color <- brown_hair_girls$Freq
chisq_test <- chisq.test(eye_color)

contingency_table <- table(diamonds$cut, diamonds$color)
chisq_test <- chisq.test(contingency_table)
main_stat <- chisq_test$statistic
main_stat <- as.vector(main_stat)


df <- mtcars
sad_table <- table(df$am , df$vs)
ch_test <- fisher.test(sad_table)

fisher_test<-ch_test$p.value


mean_p <- mean(diamonds$price)
mean_c <- mean(diamonds$carat)
diamonds$factor_price <- ifelse(diamonds$price >= mean_p , 1 , 0)
diamonds$factor_carat <- ifelse(diamonds$carat >= mean_c, 1,0)
dad_table <- table(diamonds$factor_carat , diamonds$factor_price)
square_test <- chisq.test(contingency_table)
main_stat <- square_test$statistic


