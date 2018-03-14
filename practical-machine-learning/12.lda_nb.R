data(iris)
library(ggplot)
names(iris)

train_indx = createDataPartition(y=iris$Species,
                           p=0.7,
                           list=F)
train = iris[train_indx,]
test = iris[-train_indx,]

lda_model = train(Species~., method='lda',
                  data=train)
nb_model = train(Species~., method='nb',
                 data=train)

pred_lda = predict(lda_model, test)#linear discriminant analysis
pred_nb = predict(nb_model, test)#naive bayes

table(pred_lda, pred_nb)

equal_pred = (pred_lda==pred_nb)
qplot(Petal.Width, Sepal.Width, 
      color = equal_pred, data=test)
