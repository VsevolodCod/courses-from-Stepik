#3.3


ideal_model <- step(model_full ,scope = list(lower = model_null, upper = model_full),direction = "both")




model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
ideal_model <- step(model_full ,scope = list(lower = model_null, upper = model_full),direction = "both")
anova(model_full,ideal_model)





model <- lm(sr ~ (.)^2, data = LifeCycleSavings)