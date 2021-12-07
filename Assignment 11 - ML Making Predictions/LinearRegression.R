


##EYE TEST EXAMPLE

EyeTestPerformance <- read.csv("~/Dropbox/Teaching/Data101/EyeTestPerformance.csv")
EyeTrain <- EyeTestPerformance[1:435,]
EyeTest <- EyeTestPerformance[436:544,]
plot(EyeTrain$HEIGHT,EyeTrain$WEIGHT)
#plot the linear regression of weight based on height
abline(lm(WEIGHT~HEIGHT,data=EyeTrain), col="red")
#see the linear regression of weight based on height
lm(WEIGHT~HEIGHT,data=EyeTrain)
summary(lm(WEIGHT~HEIGHT,data=EyeTrain))

## Using the weight-based Linear Regression to predict Weight of the EyeTestdata
weight.lm <- lm(WEIGHT~HEIGHT,data=EyeTrain)
weight.pred <- predict(weight.lm,EyeTest)
EyeTest$Predicted <- weight.pred
View(EyeTest)
#MSE
mean((EyeTest$Predicted-EyeTest$WEIGHT)^2)
#RMSE
sqrt(mean((EyeTest$Predicted-EyeTest$WEIGHT)^2))

#A package to quiclkly compute errors
install.packages('DMwR') 
library(DMwR)
DMwR::regr.eval(EyeTest$Predicted,EyeTest$WEIGHT)
### Using the Errors.R error code if DMwR does not work
source(Errors.R)
regr.error(EyeTest$Predicted,EyeTest$WEIGHT)

#Using more than one predictor
weight.lm2 <- lm(WEIGHT~HEIGHT+AGE+PERFORMANCE,data=EyeTrain)
summary(weight.lm2)
weight.pred2 <- predict(weight.lm2,EyeTest)
EyeTest$Predicted2 <- weight.pred2
abline(weight.lm2,col="blue")
DMwR::regr.eval(EyeTest$Predicted2,EyeTest$WEIGHT)
#comparing with the first model
DMwR::regr.eval(EyeTest$Predicted,EyeTest$WEIGHT)
### Using the Errors.R error code if DMwR does not work
regr.error(EyeTest$Predicted2,EyeTest$WEIGHT)
regr.error(EyeTest$Predicted,EyeTest$WEIGHT)

## Data101 Views Predictions
Data101ZoomParticipantsF21 <- read.csv("~/Dropbox/Teaching/Data101/Data101ZoomParticipantsF21.csv")
View(Data101ZoomParticipantsF21)
Data101ZoomParticipantsF21$TotalViews <- Data101ZoomParticipantsF21$Participants+Data101ZoomParticipantsF21$ViewsAsoff1115
View(Data101ZoomParticipantsF21)
plot(Data101ZoomParticipantsF21$LectureNumber,Data101ZoomParticipantsF21$Participants, xlim=c(0,28),ylim=c(0,300))
d101.part.lm <- lm(Participants~LectureNumber,data=Data101ZoomParticipantsF21)
summary(d101.part.lm)
abline(d101.part.lm,col="blue")
predict(d101.part.lm,newdata=data.frame(LectureNumber=21))
predict(d101.part.lm,newdata=data.frame(LectureNumber=27))

### Beyond linear regression
# Quadratic
plot(EyeTrain$HEIGHT,EyeTrain$WEIGHT)
weight.quad <- lm(WEIGHT~I(HEIGHT^2),data=EyeTrain)
predicted.quad <- predict(weight.quad,data.frame(HEIGHT=c(1:180)))
lines(predicted.quad,col="blue",lwd=2)
regr.error(predict(weight.quad,EyeTest),EyeTest$WEIGHT)
summary(weight.quad)


weight.quad2 <- lm(WEIGHT~I(HEIGHT^2)+HEIGHT,data=EyeTrain)
predicted.quad2 <- predict(weight.quad2,data.frame(HEIGHT=c(1:180)))
lines(predicted.quad2,col="red",lwd=2)
summary(weight.quad2)
regr.error(predict(weight.quad2,EyeTest),EyeTest$WEIGHT)

#All predictors
weight.all <- lm(WEIGHT~I(HEIGHT^2)+HEIGHT+I(AGE^2)+AGE+I(PERFORMANCE^2)+PERFORMANCE,data=EyeTrain)
summary(weight.all)
regr.error(predict(weight.all,EyeTest),EyeTest$WEIGHT)

#Exponential function
plot(EyeTrain$HEIGHT,EyeTrain$WEIGHT)
weight.exp <- lm(I(log(WEIGHT))~HEIGHT,data=EyeTrain)
predict.weight.exp <- predict(weight.exp,data.frame(HEIGHT=c(1:180))) 
lines(exp(predict.weight.exp),col="purple",lwd=2)
regr.error(exp(predict(weight.exp,EyeTest)),EyeTest$WEIGHT)
summary(weight.exp)
#with more than one predictor
weight.exp.ha = lm(I(log(WEIGHT))~HEIGHT,data=EyeTrain)
summary(weight.exp.ha)
predict.weight.exp.ha <- predict(weight.exp.ha, data.frame(HEIGHT=c(1:180),AGE=c(1,80)))
lines(exp(predict.weight.exp.ha),col="orange",lwd=2)
regr.error(exp(predict(weight.exp.ha,EyeTest)),EyeTest$WEIGHT)



#Log Function
plot(EyeTrain$WEIGHT,EyeTrain$HEIGHT)
weight.log=lm(HEIGHT~I(log(WEIGHT)), data=EyeTrain)
predict.weight <- predict(weight.log,data.frame(WEIGHT=c(1:70)))
lines(predict.weight,col="red",lwd=2)
regr.error(exp(predict(weight.log,EyeTest)),EyeTest$HEIGHT)
summary(weight.log)

## Predicting the weight
predict(weight.lm,data.frame(HEIGHT=173))
#53.66524 
predict(weight.quad2,data.frame(HEIGHT=173))
# 60.14353 
exp(predict(weight.exp,data.frame(HEIGHT=173)))
# 63.74461 
exp(predict(weight.exp.ha,data.frame(HEIGHT=173,AGE=45)))
#63.52399
predict(weight.all,data.frame(HEIGHT=173,AGE=45,PERFORMANCE=1))
#60.44381

## Data101 Views Predictions
plot(Data101ZoomParticipantsF21$LectureNumber,Data101ZoomParticipantsF21$Participants, xlim=c(0,28),ylim=c(0,300))
d101.part.lm <- lm(Participants~LectureNumber,data=Data101ZoomParticipantsF21)
summary(d101.part.lm)
abline(d101.part.lm,col="green")
predict(d101.part.lm,newdata=data.frame(LectureNumber=22))
predict(d101.part.lm,newdata=data.frame(LectureNumber=27))

#Log prediction
part.log.lm=lm(Participants~I(log(LectureNumber)),data=Data101ZoomParticipantsF21)
summary(part.log.lm)
plot(Data101ZoomParticipantsF21$LectureNumber,Data101ZoomParticipantsF21$Participants, xlim=c(0,28),ylim=c(0,300))
abline(d101.part.lm,col="green")
lines(predicted.part.log,col="red",lwd=2)
predict(part.log.lm, newdata = data.frame(LectureNumber=22))
predict(part.log.lm, newdata = data.frame(LectureNumber=27))

#Loglog prediction
part.loglog.lm=lm(I(log(Participants))~I(log(LectureNumber)),data=Data101ZoomParticipantsF21)
summary(part.loglog.lm)
lines(exp(predicted.part.loglog),col="purple",lwd=2)
exp(predict(part.loglog.lm, newdata = data.frame(LectureNumber=22)))
exp(predict(part.loglog.lm, newdata = data.frame(LectureNumber=27)))

#Caret for cross-validation
library(caret)
set.seed(1234)#for reproducability
#set the cross validation
caret.control <- trainControl(method="repeatedcv", number=10, repeats=3) #10 fold validation  (repeats is just an additional layer of redundancy)
weight.cv <- train(WEIGHT~HEIGHT,data=EyeTrain, method="lm", trControl=caret.control)
weight.cv
regr.error(predict(weight.cv,EyeTest),EyeTest$WEIGHT)
summary(weight.cv)


#cross validating the quadratic model
weight.cv.all <- train(WEIGHT~I(HEIGHT^2)+HEIGHT+I(AGE^2)+AGE+I(PERFORMANCE^2)+PERFORMANCE,data=EyeTrain, method="lm", trControl=caret.control)
summary(weight.cv.all)
weight.cv.all
