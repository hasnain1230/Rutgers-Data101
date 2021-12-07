

## Code used in the How to get started on Kaggle ppt
citybike.train <- read.csv("~/Dropbox/Teaching/Data101/Assignments20/CityBikePrediction/citybike.train.csv")
View(citybike.train)
plot(citybike.train$bikeid,citybike.train$tripduration)
summary(citybike.train$tripduration)
summary(citybike.train$bikeid)

#LM model on bikeid
bikeid.lm.model <- lm(tripduration~bikeid, data=citybike.train)
summary(bikeid.lm.model)
citybike.test100 <- read.csv("~/Dropbox/Teaching/Data101/Assignments20/CityBikePrediction/citybike.test100.csv")
View(citybike.test100)
bikeid.lm.predictions <- predict(bikeid.lm.model,citybike.test100)
library(DMwR)
citybike.test100withduration <- read.csv("~/Dropbox/Teaching/Data101/Assignments20/CityBikePrediction/citybike.test100withduration.csv")

DMwR::regr.eval(bikeid.lm.predictions,citybike.test100withduration$tripduration)
### Or with Errors.R
regr.error(bikeid.lm.predictions,citybike.test100withduration$tripduration)

#LM model on Day
citybike.train$Day <- as.numeric(substr(citybike.train$starttime,9,10))
day.lm.model <- lm(tripduration~Day, data=citybike.train)
citybike.test100$Day <- as.numeric(substr(citybike.test100$starttime,9,10))
day.lm.predictions <- predict(day.lm.model,citybike.test100)
DMwR::regr.eval(day.lm.predictions,citybike.test100withduration$tripduration)
### Or with Errors.R
regr.error(day.lm.predictions,citybike.test100withduration$tripduration)


#Simple model - all 600 predictions
all600.predictions <- data.frame(Predicted=rep(600,10000))
View(all600.predictions)
write.csv(all600.predictions,file="all600.predictions.csv")

## Prof Marian's submission
my.lm <- lm(tripduration~Day+bikeid, data=citybike.train)
citybike.test10000B$Day <- as.numeric(substr(citybike.test10000B$starttime,9,10))
my.predictions.10000B <- predict(my.lm,citybike.test10000B)
write.csv(my.predictions.10000B,file="ProfMarian-lecture.csv")
ProfMarian.lecture <- read.csv("~/F21Data101/LectureDecisionTrees/ProfMarian-lecture.csv")
View(ProfMarian.lecture)
colnames(ProfMarian.lecture) <- c("Id","Predicted")
write.csv(ProfMarian.lecture,file="ProfMarian-lecture.csv",row.names=FALSE)



