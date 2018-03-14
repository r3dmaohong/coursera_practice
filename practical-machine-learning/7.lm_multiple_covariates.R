library(ISLR)
library(ggplot2)
library(caret)
data(Wage)

wage = subset(Wage, select=-logwage)
summary(wage)

train_indx = createDataPartition(y=wage$wage,
                                 p=0.7,
                                 list=FALSE)
train = wage[train_indx,]
test = wage[-train_indx,]
dim(train)
dim(test)

featurePlot(x=train[,c('age', 'education', 'jobclass'),],
            y=train$wage, plot='pairs')
qplot(x=age, y=wage, data=train)
qplot(x=age, y=wage, color=jobclass, data=train)
qplot(age, wage, color=education, data=train)

##
sapply(train, class)
lm_model = train(wage~age+education+jobclass, 
                 method='lm',
                 data=train)
lm_model

plot(lm_model$finalModel, 1, pch=19,
     cex=0.5)
qplot(lm_model$finalModel$fitted.values, 
      lm_model$finalModel$residuals,
      color=race,
      data=train)

# index <=> row
plot(lm_model$finalModel$residuals, pch=19)

pred = predict(lm_model, test)
qplot(wage, pred, color=year, data=test) +
  geom_abline(intercept = 0, slope = 1, lwd=2)

full_lm_model = train(form=wage~., method='lm', data=train)
pred = predict(full_lm_model, test)
qplot(wage, pred, data=test) +
  geom_abline(intercept = 0, slope = 1, lwd=1.5, color='red')

lm_model$results$RMSE
full_lm_model$results$RMSE
