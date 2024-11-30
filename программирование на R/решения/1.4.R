#1.4
mtcars$new_var <- ifelse((mtcars$carb >= 4 )|( mtcars$cyl>6) , 1 ,0) 

mean_value <- mean(my_vector)
if (mean_value>20) {
  result <- "My mean is great"
} else {
  result <-"My mean is not so great"
} 

good_months <- c()
for (i in 2:length(AirPassengers)) {
  if (AirPassengers[i] > AirPassengers[i - 1]) {
    good_months <- c(good_months, AirPassengers[i])
  }
}

window_size <- 10
moving_average <- numeric(length(AirPassengers) - window_size + 1)
for (i in 1:(length(AirPassengers) - window_size + 1)) {
  moving_average[i] <- mean(AirPassengers[i:(i + window_size - 1)])
}