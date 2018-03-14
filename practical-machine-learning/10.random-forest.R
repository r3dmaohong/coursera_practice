data(iris)
library(caret)
library(ggplot2)

train_indx = createDataPartition(y=iris$Species,
                                 p=0.7,
                                 list=F)
train = iris[train_indx,]
test = iris[-train_indx,]
dim(train)
dim(test)

rf_model = train(Species~., method='rf',
                 data=train, prox=T)
rf_model

library(randomForest)
getTree(rf_model$finalModel, k=500)
getTree(rf_model$finalModel, k=2)

irisP = classCenter(train[,3:4], train$Species, rf_model$finalModel$proximity)
irisP = as.data.frame(irisP)
irisP$Species = rownames(irisP)

p = qplot(Petal.Width, Petal.Length, col=Species,
          data=train)
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species),
               size=5, shape=4, data=irisP)

pred = predict(rf_model, test)
test$predRight = (pred==test$Species)
table(pred, test$Species)

qplot(Petal.Width, Petal.Length, color=predRight, data=test, main='Predictions')
#rfcv
