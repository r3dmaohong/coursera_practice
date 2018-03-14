library(ggplot2)
library(caret)
data(iris)

table(iris$Species)

dim(iris)

train_indx = createDataPartition(y=iris$Species, p=0.7, list=FALSE)
train = iris[train_indx,]
test = iris[-train_indx,]

dim(train)
dim(test)

names(iris)
qplot(Petal.Width, Sepal.Width, color=Species, data=train)

rpart_model = train(Species~., method='rpart', data=train)
rpart_model
rpart_model$finalModel

plot(rpart_model$finalModel, uniform=T, main="Classification Tree")
text(rpart_model$finalModel, use.n = T, all=T, cex=0.8)

#https://zhiyzuo.github.io/installation-rattle/
library(rattle)
fancyRpartPlot(rpart_model$finalModel)


predict(rpart_model, test)
