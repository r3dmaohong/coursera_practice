data("iris")
library(ggplot2)
library(caret)

train_indx = createDataPartition(y=iris$Species,
                                 p=0.7,
                                 list=F)

train = iris[train_indx,]
test = iris[-train_indx,]

kmeans1 = kmeans(subset(train, select = -Species), centers = 3)
train$cluster = as.factor(kmeans1$cluster)
qplot(Petal.Width, Petal.Length, color=cluster, 
      data=train)

table(train$Species, train$cluster)

rpart_model = train(cluster~., method='rpart',
                    data=subset(train, select=-Species))
table(predict(rpart_model, train), train$Species)
