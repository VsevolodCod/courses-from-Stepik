#3.5

library(gvlma)
my_df <- read.csv("https://stepic.org/media/attachments/lesson/12088/homosc.csv")
x <- gvlma(DV ~ IV, data = my_df)
summary(x)

resid.norm <- function(fit) {    
  resid.norm.pv <- shapiro.test(fit$residuals)$p.value    
  plt <- ggplot(data.frame(fit$model), aes(x = fit$residuals)) +    
    geom_histogram(fill = ifelse(resid.norm.pv < 0.05, 'red', 'green'))    
  return(plt)}

high.corr <- function(x) {
  cr <- cor(x)
  diag(cr) <- 0
  m_c_value <- max(abs(cr))
  m_c_ind <- which(abs(cr) == m_c_value, arr.ind = TRUE)
  v_name <- unique(rownames(cr)[m_c_ind])
  return(v_name)
}