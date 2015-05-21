

# q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
set.seed(33833)

modRF=train(y~.,data=vowel.train,method="rf",prox=TRUE)
predRf=predict(modRF,newdata=vowel.test)
confusionMatrix(vowel.test$y,predRf)


modgbm=train(y~.,data=vowel.train,method="gbm",verbose=FALSE)
predGBM=predict(modgbm,newdata=vowel.test)
confusionMatrix(vowel.test$y,predGBM)


pred <- data.frame(predRf, predGBM, y=vowel.test$y, agree=predRf == predGBM)

accuracy <- sum(predRf[pred$agree] == pred$y[pred$agree]) / sum(pred$agree)
accuracy



#q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)


