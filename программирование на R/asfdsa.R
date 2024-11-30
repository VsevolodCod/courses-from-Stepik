df <- read.csv("grants.csv")

str(df)


df$status <- as.factor(df$status)
levels(df$status) <- c ("Not found", "Founded")

df$status <- factor(df$status, labels = c("Not founded", "Founded"))

t1 <- table(df$status)

dim(t1)

t2 <- table(df$status, df$field)
t2 <- table(status = df$status, field = df$field)

dim(t2)

prop.table(t2)
prop.table(t2, 2)

t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)

dim(t3)



t5 <- HairEyeColor[ , 'Green','Female']
sum(t5)

barplot(t2)

barplot(t2, legend.text = T , args.legend = list(x = "topright"))
barplot(t2, legend.text = T , args.legend = list(x = "topright"), beside = T )

mosaicplot(t2)

binom.test(t1)
