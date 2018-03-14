# stacking models
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)

Wage = subset(Wage, select=-logwage)
build_indx = createDataPartition(y=Wage$wage, 
                                 p=0.7,
                                 list=F)

build = Wage[build_indx,]
validation = Wage[-build_indx,]

train_indx = createDataPartition(y=build$wage,
                                 p=0.7,
                                 list=F)
train = build[train_indx,]
test = build[-train_indx,]
dim(train)
dim(test)
dim(validation)

mdl1 = train(wage~., method='glm', data=train)
mdl2 = train(wage~., method='rf', data=train,
             trControl=trainControl(method='cv'), number=3)

pred1 = predict(mdl1, test)
pred2 = predict(mdl2, test)

qplot(pred1, pred2, color=wage, data=test)+
  geom_abline(intercept = 0, slope = 1)

pred_df = data.frame(pred1, pred2, wage=test$wage)
comb_mdl_fit = train(wage~., method='gam', data=pred_df)
comb_pred = predict(comb_mdl_fit, pred_df)

RMSE(pred1, test$wage)
RMSE(pred2, test$wage)
RMSE(comb_pred, test$wage)

valid_pred1 = predict(mdl1,validation)
valid_pred2 = predict(mdl2,validation)
valid_pred_df = data.frame(pred1=valid_pred1, pred2=valid_pred2, wage=validation$wage)
comb_valid_pred = predict(comb_mdl_fit, valid_pred_df)

RMSE(valid_pred1, validation$wage)
RMSE(valid_pred2, validation$wage)
RMSE(comb_valid_pred, validation$wage)
