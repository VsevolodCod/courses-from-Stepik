df <- mtcars

cor.test(x = df$mpg, y = df$hp)
fit <- cor.test(x = df$mpg, y = df$hp)
str(fit)

cor.test(~mpg + hp, df)

fit$p.value


plot(x = df$mpg, y = df$hp)

ggplot(df, aes( x = mpg , y = hp, col = factor(cyl)))+
  geom_point(size = 5)

df_numeric <- df[,c(1,2:7)]

pairs(df_numeric)

cor(df_numeric)
library(psych)

fit <- corr.test(df_numeric)
fit$r
fit$p



df <- mtcars
df_numeric <- df[,c(1,2:7)]

fit <- lm(mpg ~hp, df)
summary(fit)


ggplot(df, aes(hp, mpg)) +
  geom_smooth(method = "lm", se = F) + 
  facet_grid(.~cyl)

fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)


new_hp <- data.frame(hp = c(100, 150, 129 , 300))

new_hp$mpg <-predict(fit, new_hp)

predict(fit, new_hp)

my_df <- mtcars
my_df$cyl <- factor(my_df$cyl, labels =  c("four", "six", "eight"))

fit <- lm(mpg ~ cyl, my_df)
summary(fit)

ggplot(my_df, aes(cyl , mpg))+
  geom_point()+
  theme(axis.text =  element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))

aggregate(mpg ~ cyl , my_df ,mean)

dfff <- read.table("dataset_11508_12.txt", header = T)
lm_dffff <- lm(dfff[,1]~dfff[,2], data = dfff)
intercept <- coef(lm_dffff)[1]
slope <- coef(lm_dffff)[2]
cat(intercept, slope, sep = " ")


dataa <-diamonds[diamonds$cut == "Ideal" & diamonds$carat == 0.46, ]
lm_dataaa <- lm(price ~ depth, data =dataa )
fit_coef <- lm_dataaa$coefficients


?swiss
swiss <- data.frame(swiss)

str(swiss)


fit_ful <- lm (Fertility ~. ,data = swiss)
summary(fit_ful)

fir_red1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic +Education , data = swiss)
summary(fir_red1)


anova(fit_ful, fir_red1)

fiy_R2 <- lm(Fertility ~ Infant.Mortality + Agriculture + Catholic +Education , data = swiss)
  summary(fiy)
anova(fit_ful, fiy_R2)



 hist(swiss$Fertility , col = "red")

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)


fit2 <- lm(Fertility ~ Examination * Catholic, data = swiss)
summary(fit2)

confint(fit2)

hist(swiss$Catholic , col = "red")
swiss$religios <- ifelse(swiss$Catholic > 60 , "Lots", "Few")
swiss$religios <- as.factor(swiss$religios)

fit3 <- lm(Fertility ~ Examination + religios, data = swiss)
summary(fit3)


fit4 <- lm(Fertility ~ Examination * religios, data = swiss)
summary(fit4)

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')



ggplot(swiss, aes(x= Examination, y = Fertility, col = religios))+
  geom_point()+
  geom_smooth(method = "lm")



fit5 <- lm(Fertility ~  religios *Infant.Mortality* Examination , data = swiss)
summary(fit5)




fill_na <- function(x) {
  x_no <- subset(x, !is.na(y))
  fit <- lm(y ~ x_1 + x_2, data = x_no)
  x$y_full <- ifelse(is.na(x$y), predict(fit, newdata = x), x$y)
  return(x)
}


optimal_fit <- step(fit_ful, direction = "backward")
summary(optimal_fit)

ideal_model <- step(model_full, scope = list(lower = model_null,
 upper = model_full),
direction = "both")



#3.4 конец близок ...
data(swiss)
str(swiss)


pairs(swiss)


ggplot(swiss, aes (x = Examination, y = Education))+
  geom_point()+
  theme(axis.text  = element_text(size = 25),
        axis.title = element_text(size = 25, face = "bold"))+
  geom_smooth(method = "lm")

ggplot(swiss, aes (x = Examination))+
  geom_histogram()
  
ggplot(swiss, aes (x = log(Education)))+
  geom_histogram()





ggplot(swiss, aes (x = Examination, y = Education))+
  geom_point()+
  geom_smooth()
  
  
lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)



swiss$Examination_squared <- (swiss$Examination)^2


lm2 <- lm(Education ~ Examination+Examination_squared, swiss)
summary(lm2)


swiss$lm1_fitted <- lm1$fitted
anova(lm2, lm1)

swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)


ggplot(swiss, aes(x = Examination, y = Education))+
  geom_point(size = 3)+
  geom_line(aes(x = Examination , y = lm1_fitted), col = "red", lwd = 1)+
  geom_line(aes(x = Examination , y = lm2_fitted), col = "blue", lwd = 1)
  
ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid))+
  geom_point(size = 3)+ 
  geom_hline(yintercept = 0,  col = "red" , lwd = 1)



ggplot(swiss, aes(x = lm2_fitted , y = lm1_resid))+
  geom_point(size = 3) + geom_hline(yintercept = 0,  col = "red" , lwd = 1)


ggplot(swiss, aes(x = lm2_fitted , y = lm2_resid))+
  geom_point(size = 3) + geom_hline(yintercept = 0,  col = "red" , lwd = 1)




ggplot(swiss, aes (x=obs_number, y = lm1_resid ))+
  geom_point(size = 3) + geom_smooth()
ggplot(swiss, aes (x=obs_number, y = lm2_resid ))+
  geom_point(size = 3) + geom_smooth()


ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid))+
  geom_point(size = 3)


ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid))+
  geom_point(size = 3)

library(gvlma)

my_df <- read.csv("https://stepic.org/media/attachments/lesson/12088/homosc.csv")
x <- gvlma(DV ~ IV, data = my_df)
summary(x)

install.packages("gvlma")
library(gvlma)
y <- read.csv("homosc.csv")
x <- lm(DV ~ IV, data = y)
a <- gvlma(x)
summary(a)


qqnorm(lm1$residuals)
qqline(lm1$residuals)







library(ggplot2)

my_df <- read.csv("train.csv", sep = ';',header = T, stringsAsFactors = T)
str(my_df)

ggplot(my_df, aes(read, math, col = gender))+
  geom_point(size = 5)+
  facet_grid(.~hon)+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))

library(nnet)
fit <- glm(hon ~ read + math + gender, data = my_df)
summary(fit)



exp(fit$coefficients)

head(predict(object = fit))

head(predict(object = fit, type = "response"))

my_df$prob  <- predict(object = fit, type = "response")




library(ROCR)
?ROCR
pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc")
str(auc)



perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)

legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v= 0.225, lwd = 2)


my_df$pred_resp  <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))

my_df$correct  <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)


ggplot(my_df, aes(prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))

mean(my_df$correct)


test_df  <- read.csv("test.csv", sep = ";")
test_df$hon  <- NA

test_df$hon  <- predict(fit, newdata = test_df, type = "response")
View(test_df)










