trainingData <- read.csv("/Users/hasnain1230/Desktop/Programming\ Related\ Stuff/CS\ Assignments/Data101/Assignment 11\ -\ ML\ Making\ Predictions/citybike.train.csv")
testDataA <- read.csv("/Users/hasnain1230/Desktop/Programming\ Related\ Stuff/CS\ Assignments/Data101/Assignment\ 11\ -\ ML\ Making\ Predictions/citybike.test10000A.csv")
testDataB <- read.csv("/Users/hasnain1230/Desktop/Programming\ Related\ Stuff/CS\ Assignments/Data101/Assignment\ 11\ -\ ML\ Making\ Predictions/citybike.test10000B.csv")
source("/Users/hasnain1230/Desktop/Programming\ Related\ Stuff/CS\ Assignments/Data101/Assignment\ 11\ -\ ML\ Making\ Predictions/Errors.R")

fun_10000A_model <- function() {
  weight = lm(tripduration~I(start.station.latitude^2), data=trainingData)
  predicted = predict(weight, testData)
  testData$predicted = predicted
  summary(weight)
  regr.error(predicted, trainingData$tripduration) # Because R is a pain and just awful and I don't want to deal with this right now, I just manually pasted the errors into the file.
  
  lapply(predicted, write, "/Users/hasnain1230/Desktop/Programming\ Related\ Stuff/CS\ Assignments/Data101/Assignment\ 11\ -\ ML\ Making\ Predictions/errors.txt", append=TRUE)
}

fun_10000B_model <- function() {
  trainingData$Day <- as.numeric(substr(trainingData$starttime,9,10))
  my_weight = lm(tripduration~start.station.latitude + I(bikeid^3) + Day + start.station.id, data=trainingData)
  testDataB$Day <- as.numeric(substr(testDataB$starttime,9,10))
  prediction = predict(my_weight, testDataB)
  write.csv(prediction, file="~/test.csv")
  data = read.csv("~/test.csv")
  file.remove("~/test.csv")
  colnames(data) = c("Id", "Predicted")
  write.csv(data, file="~/Desktop/Hasnain_Predictions.csv", row.names = FALSE)
  
  summary(my_weight)
}

fun_10000A_model()