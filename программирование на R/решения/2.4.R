#2.4

NA.position <- function(x) {
  return(which(is.na(x)))
}


NA.counter <- function(x){
  return(sum(is.na(x)))
}


filtered.sum <- function(x){
  p_values <- x[x > 0]
  return (sum (p_values , na.rm = T))
}




outliers.rm <- function(x){
  x <- x[!is.na(x)]
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x_cleaned <- x[x >= lower_bound & x <= upper_bound]
  
  return(x_cleaned)
}