library(ISLR)
data(Wage)
library(ggplot2)
library(caret)

wage = subset(Wage, select=-logwage)
train_indx = createDataPartition(y=wage$wage, 
                                 p=0.7,
                                 list=F)
train = wage[train_indx,]
test = wage[-train_indx,]

#boosting with trees
( gbm_model = train(wage~., method='gbm',
                  data=train, verbose=F) )

qplot(predict(gbm_model, test), wage, data=test) +
  geom_abline(intercept = 0, slope=1)
