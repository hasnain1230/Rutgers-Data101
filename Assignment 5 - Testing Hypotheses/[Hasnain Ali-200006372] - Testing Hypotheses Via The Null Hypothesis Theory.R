library(ggplot2)
citi_bike <- read.csv("~/Downloads/citibike.May20.50K.csv")

citi_bike_customer_vs_subscriber <- function() { # Total amount of time subscribers use vs. customers.
  citi_bike_frame = data.frame(trip_time = citi_bike$tripduration, type = citi_bike$usertype) # Convert data to dataframe. 
  acb = data.frame(aggregate(citi_bike_frame$trip_time ~ citi_bike_frame$type, citi_bike_frame,  # Get average for trip time for each customer type.
                             FUN = mean))
  
  n = 2.0
  
  acb$citi_bike_frame.trip_time = acb$citi_bike_frame.trip_time / 60.0 # Seconds to minutes. 
  x_bar = acb$citi_bike_frame.trip_time[1]
  mu = acb$citi_bike_frame.trip_time[2]
  sigma = sd(acb$citi_bike_frame.trip_time)
  
  print(sigma)
  
  
  z = ((x_bar - mu)) / ((sigma / (sqrt(n)))) # Calculate z
  
  P = 1 - pnorm(z) # We can reject the NH because we're cewl. You're also cewl. Thanks me! :D You are cool too. Thanks you. Thanks me! :D You are cool too. Thanks you. Thanks me! :D You are cool too. Thanks you. Thanks me! :D You are cool too. Thanks you. Thanks me! :D You are cool too. Thanks you. (Stackoverflow Error, out of memory.)
  
  print(z)
  print(P * 100)
}

citi_bike_ridetime_vs_gender <- function() {
  citi_bike_frame = data.frame(trip_time = citi_bike$tripduration, gender = citi_bike$gender) # Make data frame
  gender_data_set=subset(citi_bike_frame,gender!=0)#subset the data
  View(gender_data_set)
  print(gender_data_set)
  acg = data.frame(aggregate((gender_data_set$trip_time / 60.0) ~ gender_data_set$gender, gender_data_set, FUN = mean)) # Aggregate gender values based on time. Convert to minutes based on process. Find the mean of the total time for each gender.
  n = 2.0
  colnames(acg) = c("Gender", "Time")
  x_bar = acg$Time[2]
  mu = acg$Time[1]
  sigma = sd(acg$Time)
  z = ((x_bar - mu)) / ((sigma / (sqrt(n)))) # Calculate z.
  P = 1 - pnorm(z) # Calculate P. 
  print(z)
  print(P * 100)
  print(sigma)
}

citi_bike_customer_vs_subscriber()
citi_bike_ridetime_vs_gender()