train_data = read.csv("~/Desktop/Programming Related Stuff/CS Assignments/Data101/Assignment 12 - ML Making Predictions (Part 2)/citybike.train.csv")
predict_data = read.csv("~/Desktop/Programming Related Stuff/CS Assignments/Data101/Assignment 12 - ML Making Predictions (Part 2)/citybike.test20000.csv")
# test_data = read.csv("~/Desktop/Programming Related Stuff/CS Assignments/Data101/Assignment 12 - ML Making Predictions (Part 2)/citybike.test10000Awithduration.csv")

create_model <- function() {
  if (file.exists("~/Desktop/Hasnain_Predictions.csv")) {
    file.remove("~/Desktop/Hasnain_Predictions.csv") 
  }
  
  train_data$Day <- as.numeric(substr(train_data$starttime,9,10))
  train_data$Hour <- as.numeric(substr(train_data$starttime,12,13))
  train_data$Minutes <- as.numeric(substr(train_data$starttime,15,16))
  train_data$Second <- as.numeric(substr(train_data$starttime,18, 19))
  x2 = train_data$start.station.latitude
  x1 = train_data$end.station.latitude
  y2 = train_data$end.station.longitude
  y1 = train_data$start.station.longitude
  
  distance = sqrt((x2 - x1)^2 + (y2 - y1)^2)
  train_data$Distance = distance
  
  my_weight = lm(tripduration~start.station.id + end.station.id + I(log(bikeid)) + Day + Hour + Minutes + Second + 
                  start.station.latitude + end.station.latitude + start.station.longitude + end.station.longitude +
                   Distance + usertype, data=train_data)
  
  
  predict_data$Day <- as.numeric(substr(predict_data$starttime,9,10))
  predict_data$Hour <- as.numeric(substr(predict_data$starttime,12,13))
  predict_data$Minutes <- as.numeric(substr(predict_data$starttime,15,16))
  predict_data$Second <- as.numeric(substr(predict_data$starttime,18, 19))
  
  x2 = predict_data$start.station.latitude
  x1 = predict_data$end.station.latitude
  y2 = predict_data$end.station.longitude
  y1 = predict_data$start.station.longitude
  
  distance = sqrt((x2 - x1)^2 + (y2 - y1)^2)
  predict_data$Distance = distance
  
  prediction = predict(my_weight, predict_data)
  summary(my_weight)
  write.csv(prediction, file="~/test.csv")
  data = read.csv("~/test.csv")
  file.remove("~/test.csv")
  colnames(data) = c("Id", "Predicted")
  write.csv(data, file="~/Desktop/Hasnain_Predictions.csv", row.names = FALSE)
}

create_model()


