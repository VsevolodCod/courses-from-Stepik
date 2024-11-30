f_d <- prop.table(HairEyeColor[,,"Male"],2)
red_men <- f_d['Red', 'Blue']



df <- as.data.frame(HairEyeColor[,,"Female"])
library("ggplot2")
obj <- ggplot(data = df, aes(x = Hair, y = Freq, col = Eye, fill = Eye)) +
  geom_bar(stat="identity", position = position_dodge() ) +
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

df <- as.data.frame(HairEyeColor[,,"Female"])
brown_hair_girls <- df[df$Hair == "Brown", ]
eye_color <- brown_hair_girls$Freq
chisq_test <- chisq.test(eye_color)

contingency_table <- table(diamonds$cut, diamonds$color)
chisq_test <- chisq.test(contingency_table)
main_stat <- chisq_test$statistic
main_stat <- as.vector(main_stat)


mean_p <- mean(diamonds$price)
mean_c <- mean(diamonds$carat)
diamonds$factor_price <- ifelse(diamonds$price >= mean_p , 1 , 0)
diamonds$factor_carat <- ifelse(diamonds$carat >= mean_c, 1,0)
dad_table <- table(diamonds$factor_carat , diamonds$factor_price)
square_test <- chisq.test(dad_table)
main_stat <- square_test$statistic



df <- mtcars
sad_table <- table(df$am , df$vs)
ch_test <- fisher.test(sad_table)
fisher_test<-ch_test$p.value