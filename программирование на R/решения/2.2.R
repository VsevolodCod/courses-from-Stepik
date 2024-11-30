#2.2

dfd <- ToothGrowth
o_d <- dfd[dfd$supp == "OJ" & dfd$dose == 0.5, ]
v_d <- dfd[dfd$supp == "VC" & dfd$dose == 2,]
t_statt <- t.test(o_d$len, v_d$len)
t_stat <- t_statt$statistic



ttg <- read.table("dataset_11504_15 (5).txt")
bartlett.test(V1 ~ V2, ttg) 
t.test(V1  ~ V2, ttg, var.equal = T)
df <- read.table("dataset_11504_15 (5).txt", header=FALSE)
b_test <- bartlett.test(V1 ~ V2, data = df)
if (b_test$p.value >= 0.05) {
  p_value <- t.test(V1 ~ V2, data = df, var.equal = TRUE)$p.value
} else {
  p_value <- wilcox.test(V1 ~ V2, data = df)$p.value
}
print(round(p_value, digits = 4))


daaataa <- read.table("dataset_11504_16 (3).txt")
t_te <- t.test(daaataa$V1 , daaataa$V2, var.equal = F)$p.value
if (t_te <0.05){
  print(c(mean(daaataa$V1), mean(daaataa$V2), t_te))
}else{
  print("The difference is not significant")
}

