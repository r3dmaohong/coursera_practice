library(caret)
data("faithful")
set.seed(1234)

head(faithful)

train_indx = createDataPartition(y=faithful$eruptions, p=0.5, list=FALSE)
train = faithful[train_indx, ]
test = faithful[-train_indx,]

plot(train$waiting, train$eruptions, col='blue', 
     pch=19,
     xlab='waiting', ylab='duration')

lm1 = lm(eruptions ~ waiting, data=train)
summary(lm1)

plot(train$waiting, train$eruptions, col='blue', 
     pch=19,
     xlab='waiting', ylab='duration')
lines(train$waiting, lm1$fitted.values, lwd=3)

predict(lm1, data.frame(waiting=80))

##
##
par(mfrow=c(1,2))
plot(train$waiting, train$eruptions, col='blue',
     pch=19, xlab='waiting', ylab='duration')
lines(train$waiting, predict(lm1), lwd=3)
title('training set')
plot(test$waiting, test$eruptions, col='red',
     pch=19, xlab='waiting', ylab='duration')
lines(test$waiting, predict(lm1, data.frame(waiting=test$waiting)), lwd=3)
title('test set')

RMSE(pred=lm1$fitted.values, obs = train$eruptions)
sqrt(mean((lm1$fitted.values-train$eruptions)^2))
RMSE(pred=predict(lm1, data.frame(waiting=test$waiting)), obs = test$eruptions)


## prediction interval
pred1 = predict(lm1, newdata=test, interval='prediction')
head(pred1)
par(mfrow=c(1,1))
plot(test$waiting, test$eruptions, pch=19, col='blue')
matlines(test$waiting, pred1, col=c(1,2,2), lwd=3, lty=1)

lm_model = train(eruptions~waiting, data=train, method='lm')
summary(lm_model$finalModel)

lm_model$results$RMSE
